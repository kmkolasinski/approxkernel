_epsilon = 1e-6


def kernel_fn(r: float) -> float:
    """Example implementation of the coulomb 2D kernel

            U(r) = alpha / |r*dx + epsilon|

    Args:
        r: distance from center, expressed in terms of grid position

    Returns:
        value of U(r)
    """
    alpha = 0.05
    dx = 1 / 32
    distance = dx * (r + _epsilon)
    amplitude = 1 / distance
    amplitude = min(amplitude, 2 / dx)
    return alpha * amplitude
