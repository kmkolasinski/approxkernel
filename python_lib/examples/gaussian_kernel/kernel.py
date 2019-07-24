import math

_epsilon = 1e-6


def kernel_fn(r: float) -> float:
    """Example implementation of the gaussian 2D kernel

            U(r) = exp( - 0.5 * r**2 )

    Args:
        r: distance from center, expressed in terms of grid position

    Returns:
        value of U(r)
    """

    dx = 0.2
    distance = dx * r
    amplitude = math.exp(- 0.5 * distance ** 2)
    return amplitude
