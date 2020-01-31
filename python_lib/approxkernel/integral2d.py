from typing import List, Callable
import tensorflow as tf

from approxkernel.data_preparation import (
    create_kernels_1d,
    kernel_1d_to_2d_projection_matrix,
    KernelFn,
)
from approxkernel.kernels import get_coulomb_2d_kernel_fn


def get_kernel_integral_radial_fn(
    kernel_size: int,
    num_scales: int,
    use_fft: bool = False,
    kernel_fn: KernelFn = get_coulomb_2d_kernel_fn(),
) -> Callable[[tf.Tensor], tf.Tensor]:
    kernels = get_trainable_radial_kernels(kernel_size, num_scales, kernel_fn)
    integral_fn = get_kernel_integral_fn(kernels, use_fft=use_fft)
    return integral_fn


def get_kernel_integral_fn(
    kernels: List[tf.Tensor], use_fft: bool = False
) -> Callable[[tf.Tensor], tf.Tensor]:

    assert (
        len(kernels) > 0
    ), f"Kernels list must have at least one element, got {kernels}"
    for kernel in kernels:
        ks = kernel.shape.as_list()
        assert ks[0] == ks[1], f"Kernel must be square, got {ks}"
        assert (
            ks[2] == ks[3]
        ), f"Kernel shape must be of form [size, size, 1, 1], got {ks}"
        assert (
            kernel.shape.as_list() == kernels[0].shape.as_list()
        ), "All kernels must be of the same size"

    def _integrate_fn(x_input: tf.Tensor):
        """x_input = [batch size, h, w, nc]"""
        _, x_height, x_width, nc = x_input.shape.as_list()

        assert nc == 1, "Input number of channels must be one"

        with tf.variable_scope("integral"):
            num_scales = len(kernels)
            final_h = 0.0
            sources = x_input

            for s in range(num_scales):
                with tf.variable_scope(f"scale_{s}"):
                    h = sources
                    # print(f"[{s+1}] scale input: {sources.shape.as_list()}")

                    if not use_fft:
                        h = tf.nn.conv2d(
                            h, kernels[s], strides=(1, 1, 1, 1), padding="SAME"
                        )
                    else:
                        h = conv2d_fft(h, kernels[s])

                    height, width = h.shape.as_list()[1:3]
                    if height != x_height or width != x_width:
                        h = tf.image.resize_bilinear(
                            h, size=(x_height, x_width), align_corners=True
                        )

                    final_h = final_h + h
                    sources = tf.layers.average_pooling2d(
                        sources, pool_size=2, strides=2, padding="SAME"
                    )

        return final_h

    return tf.make_template("integral_2d", _integrate_fn)


def get_trainable_radial_kernels(
    kernel_size: int, num_scales: int, kernel_fn: KernelFn
) -> List[tf.Tensor]:

    radius = kernel_size // 2 + 1
    kernels_1d = create_kernels_1d(
        kernel_size=radius, num_scales=num_scales, symmetrical=False
    )
    kernels_1d = [tf.reshape(k, [radius, 1]) for k in kernels_1d]

    projection_mat = kernel_1d_to_2d_projection_matrix(
        kernel_size=kernel_size, kernel_fn=kernel_fn
    )

    kernels = [
        tf.reshape(projection_mat @ k, [kernel_size, kernel_size, 1, 1])
        for k in kernels_1d
    ]
    return kernels


def conv2d_fft(x: tf.Tensor, kernel: tf.Tensor) -> tf.Tensor:
    """Compute convolution using FFT"""
    x_shape = x.shape.as_list()
    kernel_shape = kernel.shape.as_list()

    x_size = x_shape[1]
    k_size = kernel_shape[0]

    assert x_shape[-1] == 1
    assert x_shape[1] == x_shape[2]
    assert x_shape[1] is not None
    assert kernel_shape[0] == kernel_shape[1]
    assert kernel_shape[2] == kernel_shape[3]
    assert kernel_shape[3] == 1
    assert kernel_shape[0] is not None

    assert k_size % 2 == 1, "Kernel must be of even size"

    x = tf.squeeze(x, -1)[0]
    x = tf.pad(x, [[0, k_size], [0, k_size]])

    fft_lenght1 = x.shape.as_list()[0]
    fft_lenght2 = x.shape.as_list()[1]

    x_fft = tf.spectral.rfft2d(x, fft_length=[fft_lenght1, fft_lenght2])
    kernel_fft = tf.spectral.rfft2d(
        kernel[:, :, 0, 0], fft_length=[fft_lenght1, fft_lenght2]
    )
    x_conv_fft = tf.spectral.irfft2d(x_fft * kernel_fft, [fft_lenght1, fft_lenght2])
    start = k_size // 2
    end = k_size // 2 + 1

    x_conv_fft = x_conv_fft[start:-end, start:-end]
    return tf.reshape(x_conv_fft, [-1, x_size, x_size, 1])
