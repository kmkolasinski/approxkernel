import math
_epsilon = 1e-6


def kernel2d_fn(r: float) -> float:
    """Example implementation of the coulomb 2D kernel

            U(r) = Q / |r*dx + epsilon|
	For r = 0 we use exact formula, for square grid of size dx:
			U(0) = - 2* log(3 - 2 * sqrt(2)) 
    Args:
        r: distance from center, expressed in terms of grid position

    Returns:
        value of U(r)
    """
    charge = 1.0
    dx = 1.0
    singularity = - 2.0 * math.log(3.0 - 2.0 * math.sqrt(2.0))
    distance = r + _epsilon
	
    amplitude = 1 / distance
    amplitude = min(amplitude, singularity)
    return charge * amplitude / dx
