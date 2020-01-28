import math

_epsilon = 1e-6


def get_coulomb_2d_kernel_fn(alpha: float = 0.05, dx: float = 1 / 32):
    def kernel_fn(r: float) -> float:
        """Example implementation of the coulomb 2D kernel

                U(r) = alpha / |r*dx + epsilon|

        Args:
            r: distance from center, expressed in terms of grid position

        Returns:
            value of U(r)
        """
        distance = dx * (r + _epsilon)
        amplitude = 1 / distance
        amplitude = min(amplitude, 2 / dx)
        return alpha * amplitude

    return kernel_fn


def get_gaussian_2d_kernel_fn(dx: float = 0.2):
    def kernel_fn(r: float) -> float:
        """Example implementation of the gaussian 2D kernel

                U(r) = exp( - 0.5 * r**2 )

        Args:
            r: distance from center, expressed in terms of grid position

        Returns:
            value of U(r)
        """
        distance = dx * r
        amplitude = math.exp(-0.5 * distance ** 2)
        return amplitude

    return kernel_fn
