from typing import Tuple

import tensorflow as tf
import numpy as np
from logging import getLogger
from tqdm import tqdm

from approxkernel.data_preparation import create_kernels_1d

_LOGGER = getLogger(f"fit_kernel.{__name__}")


def define_coulomb_integral_1d(kernels):
    def _template_fn(x_input):
        """x_input = [batch size, nx]"""
        x_input = tf.expand_dims(x_input, -1)

        _, x_width, nc = x_input.shape.as_list()
        with tf.variable_scope("coulomb_integral_1d"):
            num_scales = len(kernels)
            final_h = []
            sources = x_input

            for s in range(num_scales):
                with tf.variable_scope(f"scale_{s}"):
                    h = sources
                    sf = s ** 2
                    tf.add_to_collection("SOURCES_SCALES", sources)
                    _LOGGER.info(f"Creating input scale: {sf} shape: {sources.shape}")
                    h = tf.nn.conv1d(h, kernels[s], stride=1, padding="SAME")
                    width = h.shape.as_list()[1]
                    if width != x_width:
                        h = tf.expand_dims(h, -1)
                        h = tf.image.resize_bilinear(
                            h, size=(x_width, 1), align_corners=True
                        )
                        h = tf.squeeze(h, -1)

                    tf.add_to_collection("POTENTIAL_SCALES", h)
                    final_h.append(h)

                    resize_kernel = tf.constant([0.25, 0.5, 0.25])
                    resize_kernel = tf.reshape(resize_kernel, [3, 1, 1])
                    sources = tf.nn.conv1d(
                        sources, resize_kernel, stride=1, padding="SAME"
                    )
                    sources = tf.layers.average_pooling1d(
                        sources, pool_size=2, strides=2, padding="SAME"
                    )

        final_h = tf.add_n(final_h)
        return tf.squeeze(final_h, -1)

    return tf.make_template("coulomb_integral_1d", _template_fn)


def define_model(num_scales: int, kernel_size: int, grid_size: int):

    kernels_1d = create_kernels_1d(num_scales, kernel_size)
    c_integral = define_coulomb_integral_1d(kernels=kernels_1d)

    source_ph = tf.placeholder(tf.float32, shape=[None, grid_size])
    target_ph = tf.placeholder(tf.float32, shape=[None, grid_size])

    c_integral_output = c_integral(source_ph)

    loss = tf.abs(target_ph - c_integral_output)
    loss = tf.reduce_sum(loss, -1)
    loss = tf.reduce_mean(loss)

    return {
        "source_ph": source_ph,
        "target_ph": target_ph,
        "kernels_1d": kernels_1d,
        "output": c_integral_output,
        "loss": loss,
    }


def train_model(
    sess: tf.Session,
    model_def: dict,
    data: Tuple[np.ndarray, np.ndarray],
    initial_lr: float = 0.001,
    num_lr_steps: int = 4,
    lr_decay: float = 0.5,
    steps_per_lr: int = 500,
):

    x_sources, y_targets = data
    lr_ph = tf.placeholder(tf.float32, [])
    optimizer = tf.train.MomentumOptimizer(lr_ph, 0.9)
    train_op = optimizer.minimize(model_def["loss"])
    sess.run(tf.global_variables_initializer())
    _LOGGER.info("Running kernel training (this may take a while)...")
    loss_history = []

    for lr in [initial_lr * lr_decay ** s for s in range(num_lr_steps)]:
        _LOGGER.info(f"Training with lr={lr}")
        for _ in tqdm(range(steps_per_lr)):
            loss_np, _ = sess.run(
                [model_def["loss"], train_op],
                feed_dict={
                    model_def["source_ph"]: x_sources,
                    model_def["target_ph"]: y_targets,
                    lr_ph: lr,
                },
            )
            loss_history.append(loss_np)
        _LOGGER.info(f"Current loss: {loss_history[-1]}")

    return loss_history[steps_per_lr:]
