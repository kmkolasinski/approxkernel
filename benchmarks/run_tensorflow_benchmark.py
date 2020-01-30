#!/usr/bin/env python
import argparse
from datetime import datetime
from pathlib import Path

import numpy as np
import tensorflow as tf

from approxkernel.integral2d import get_kernel_integral_radial_fn

NUM_GRID_SIZES = 4
NUM_SCALES = 7
NUM_WARMUP_STEPS = 10
NUM_BENCHMARK_STEPS = 20


def run_benchmark(num_scales: int, kernel_size: int, grid_size: int) -> float:

    with tf.Graph().as_default() as graph:
        with tf.Session(graph=graph) as sess:
            integral_fn = get_kernel_integral_radial_fn(
                kernel_size=kernel_size, num_scales=num_scales
            )
            input_ph = tf.placeholder(tf.float32, [1, grid_size, grid_size, 1])
            output = integral_fn(input_ph)
            sess.run(tf.global_variables_initializer())

            for _ in range(NUM_WARMUP_STEPS):
                fake_input = np.random.rand(1, grid_size, grid_size, 1)
                sess.run(output, {input_ph: fake_input})

            start = datetime.now()
            for _ in range(NUM_BENCHMARK_STEPS):
                fake_input = np.random.rand(1, grid_size, grid_size, 1)
                sess.run(output, {input_ph: fake_input})
            end = datetime.now()
            delta = (end - start).total_seconds() / NUM_BENCHMARK_STEPS
    return delta


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Benchmark integration")
    parser.add_argument("--name", default="default", help="Benchmark file name suffix")
    parser.add_argument(
        "--tag",
        default="tensorflow",
        help="Additional benchmark tag to distinguish between runs",
    )

    args = parser.parse_args()

    name = args.name
    tag = args.tag
    save_path = f"results/tensorflow-{name}.txt"

    if not Path(save_path).exists():
        row = "timestamp,tag,grid_size,num_scales,kernel_size,delta\n"
        with open(save_path, "w") as file:
            file.write(row)
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    for i in range(1, NUM_GRID_SIZES + 1):
        for num_scales in range(1, NUM_SCALES + 1):

            grid_size = 32 * 2 ** i
            kernel_size = int(4 * grid_size / 2 ** num_scales + 1)
            print(
                f"Running N={grid_size:6} Ns={num_scales:4} K={kernel_size:5}"
            )

            try:
                delta = run_benchmark(
                    num_scales=num_scales, kernel_size=kernel_size, grid_size=grid_size
                )
                row = f"{timestamp},{tag},{grid_size},{num_scales},{kernel_size},{delta}\n"
                with open(save_path, "a") as file:
                    file.write(row)
            except Exception as error:
                print(f"ERROR: Could not execute test:", error)