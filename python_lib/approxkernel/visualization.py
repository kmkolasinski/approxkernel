from pathlib import Path
import tensorflow as tf
import matplotlib.pyplot as plt
import numpy as np


def plot_sample_cross_section(
    source: np.ndarray, target: np.ndarray, save_path: str, figsize=(15, 4)
) -> None:

    plt.figure(figsize=figsize)
    plt.plot(source, "k-", label="source")
    plt.plot(target, "r-", label="K(r)")
    plt.xlabel("x")
    plt.legend()
    plt.savefig(save_path)
    plt.close()


def plot_sample_target2d(target: np.ndarray, save_path: str, figsize=(20, 5)) -> None:

    grid_size = target.shape[0]
    plt.figure(figsize=figsize)
    plt.subplot(121)
    plt.imshow(target, cmap="gray")
    plt.colorbar()
    plt.xlabel("x")
    plt.ylabel("y")

    plt.subplot(122)
    plt.plot(target[grid_size // 2, :], label="K(x, y=n/2)")
    plt.plot(target[grid_size // 2 - 1, :], label="K(x, y=n/2 - 1)")
    plt.plot(target[grid_size // 2 - 2, :], label="K(x, y=n/2 - 2)")
    plt.xlabel("x")
    plt.legend()
    plt.savefig(save_path)
    plt.close()


def plot_loss_history(loss_history: list, save_path: str, figsize=(10, 5)):
    plt.figure(figsize=figsize)
    plt.plot(loss_history)
    plt.xlabel("Iteration")
    plt.savefig(save_path)
    plt.close()


def plot_kernels1d(kernels_1d: list, save_dir: Path, figsize=(10, 5)):

    for s, kernel in enumerate(kernels_1d):
        plt.figure(figsize=figsize)
        plt.title(f"Kernel for scale {s}")
        plt.plot(kernel, "-", label=f"k={s}")
        plt.legend()
        plt.savefig(str(save_dir / f"trained_kernel_scale={s}.png"))
        plt.close()

    plt.figure(figsize=figsize)
    plt.title("Normalized kernels")
    for s, kernel in enumerate(kernels_1d):
        plt.plot(kernel / kernels_1d[0].max(), "o-", label=f"k={s}")
    plt.legend()
    plt.savefig(str(save_dir / f"trained_kernels_normalized.png"))
    plt.close()


def plot_train_target_comparison(
    sess,
    model_def: dict,
    source: np.ndarray,
    target: np.ndarray,
    save_path: str,
    figsize=(10, 5),
):
    results = sess.run(
        [model_def["output"]] + tf.get_collection("POTENTIAL_SCALES"),
        feed_dict={model_def["source_ph"]: np.expand_dims(source, 0),},
    )

    target_predicted = results[0][0, :]
    prediction_scales = results[1:]
    error = 50 * (target_predicted - target)
    plt.figure(figsize=figsize)
    plt.plot(target, "k-", label="target")
    plt.plot(target_predicted, "b--", label="predicted")
    plt.plot(error, "r-", label="50x(error)")
    # for scale in prediction_scales:
    #     plt.plot(scale[0, :, 0], "--")
    plt.xlabel("x")
    plt.legend()
    plt.savefig(save_path)
    plt.close()


def plot_kernels2d(kernels_2d: list, save_dir: Path, figsize=(5, 5)):
    for s, kernel in enumerate(kernels_2d):
        plt.figure(figsize=figsize)
        plt.title(f"Kernel-2D for scale {s}")
        plt.imshow(kernel)
        plt.colorbar()
        plt.savefig(save_dir / f"trained_kernel_2d_scale={s}.png")
        plt.close()
