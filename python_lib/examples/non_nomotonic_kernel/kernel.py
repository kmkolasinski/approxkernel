import math


def kernel_fn(r: float) -> float:
    """Some more complicated kernel function

    Args:
        r: distance from center, expressed in terms of grid position

    Returns:
        value of U(r)
    """

    dx = 0.1
    r = dx * r
    amplitude = 0.5 * (4 * r ** 2 + 1) * math.exp(- r ** 2)
    return amplitude
