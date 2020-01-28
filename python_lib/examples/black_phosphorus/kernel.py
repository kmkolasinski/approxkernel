from scipy import special

_epsilon = 1e-6


def kernel_fn(r: float) -> float:
    """

    Args:
        r: distance from center, expressed in terms of grid position

    Returns:
        value of U(r)
    """
    alpha = 1.0
    r0 = 10
    # TODO Fix this value to r -> 0
    if r == 0:
        return -2 * alpha / r0

    H0 = special.yn(0, r / r0)
    Y0 = special.struve(0, r / r0)
    return alpha / r0 * (H0 - Y0)
