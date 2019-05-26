from typing import Callable, Tuple

import numpy as np
from logging import getLogger
from tqdm import tqdm
import math


KernelFn = Callable[[float], float]
Kernel2DFn = Callable[[float, float], float]

_LOGGER = getLogger(__name__)


def get_source_and_target_2d(
        kernel_fn: KernelFn,
        source_pos: Tuple[int, int],
        grid_size: int
) -> Tuple[np.ndarray, np.ndarray]:

    target = np.zeros([grid_size, grid_size])
    source = np.zeros([grid_size, grid_size])
    source[source_pos] = 1

    ri = np.array(source_pos)
    for i in range(grid_size):
        for j in range(grid_size):
            rj = np.array([i, j])
            r = np.linalg.norm(ri - rj)
            target[i, j] = kernel_fn(r)

    return source, target


def get_source_and_target_1d(
        kernel1d_fn,
        source_pos: int,
        num_points: int
) -> Tuple[np.ndarray, np.ndarray]:

    source = np.zeros([num_points])
    source[source_pos] = 1
    target = np.array([kernel1d_fn(i, source_pos) for i in range(num_points)])

    return source, target


def create_cross_section_kernel_fn(
        kernel_fn: KernelFn
) -> Kernel2DFn:

    def _kernel1d_fn(i: float, j: float) -> float:
        return kernel_fn(abs(i - j))

    return _kernel1d_fn


def create_training_data(
        grid_size: int,
        kernel_fn: KernelFn
) -> Tuple[np.ndarray, np.ndarray]:

    _LOGGER.info(f"Creating training data for grid_size={grid_size}")

    x_sources = []
    y_targets = []

    kernel1d_fn = create_cross_section_kernel_fn(kernel_fn)

    for i in tqdm(range(grid_size)):
        source, target = get_source_and_target_1d(kernel1d_fn, i, grid_size)
        x_sources.append(source)
        y_targets.append(target)

    x_sources = np.array(x_sources)
    y_targets = np.array(y_targets)

    return x_sources, y_targets


def convert_1d_kernel_to_2d(kernels_1d: list, kernel_size: int, kernel_fn: KernelFn):
    # symmetrize and convert scale kernels to work with 2D
    sym_kernels_np = []
    for scale, kernel in enumerate(kernels_1d):
        forward_kernel = kernel[:kernel_size // 2 + 1][::-1]
        reversed_kernel = kernel[kernel_size // 2:]
        sym_kernel = (forward_kernel + reversed_kernel) / 2 * 2 ** scale
        sym_kernels_np.append(sym_kernel)

    radius = kernel_size // 2 + 1
    hk = kernel_size // 2

    projection = np.zeros([kernel_size, kernel_size, radius])

    for i in range(kernel_size):
        for j in range(kernel_size):
            ix = i - hk
            iy = j - hk
            r = math.sqrt(ix ** 2 + iy ** 2) + 1e-7
            cr = int(math.ceil(r))
            fr = int(math.floor(r))
            alpha = (kernel_fn(r) - kernel_fn(fr)) / (
                     kernel_fn(cr) - kernel_fn(fr))
            if cr < radius:
                projection[i, j, cr] = alpha
            if fr < radius:
                projection[i, j, fr] = 1 - alpha

    projection = projection / (projection.sum(axis=-1, keepdims=True) + 1e-7)
    flat_projection = projection.reshape([-1, radius])

    kernels = [
        np.reshape(flat_projection @ np.expand_dims(k, -1),
                   [kernel_size, kernel_size]) for k in sym_kernels_np
    ]

    return kernels