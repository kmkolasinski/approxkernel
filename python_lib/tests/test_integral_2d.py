import tensorflow as tf
import numpy as np
import approxkernel.kernels as kernels_ops
import approxkernel.integral2d as integral2d_ops


class Integral2DTest(tf.test.TestCase):
    def test_create_kernel_integral(self):
        kernel_fn = kernels_ops.get_coulomb_2d_kernel_fn()
        kernels = integral2d_ops.get_trainable_radial_kernels(33, 3, kernel_fn)

        self.assertEqual(len(tf.trainable_variables()), 3)

        for var in tf.trainable_variables():
            self.assertEqual([17, 1], var.shape.as_list())

        for kernel in kernels:
            self.assertEqual([33, 33, 1, 1], kernel.shape.as_list())

        integral_fn = integral2d_ops.get_kernel_integral_fn(kernels)

        input_ph = tf.placeholder(tf.float32, [None, 256, 256, 1])
        output_t = integral_fn(input_ph)

        self.assertEqual(output_t.shape.as_list(), [None, 256, 256, 1])

        with self.test_session() as sess:
            sess.run(tf.global_variables_initializer())
            output_np = sess.run(output_t, {input_ph: np.random.rand(1, 256, 256, 1)})

            self.assertEqual(output_np.shape, (1, 256, 256, 1))
            self.assertEqual(output_np.dtype, np.float32)
