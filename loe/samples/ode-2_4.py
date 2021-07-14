import numpy as np

import tensorflow as tf
from tfdiffeq import odeint

def ode_sys(t, XY):
	x=XY[0]
	y=XY[1]
	dx_dt= - x + y
	dy_dt= 4. * x - y
	return tf.stack([dx_dt, dy_dt])

t_begin=0.
t_end=5.
t_nsamples=100
t_space = np.linspace(t_begin, t_end, t_nsamples)
x_init = tf.constant([2.])
y_init = tf.constant([0.])

num_sol = odeint(
    ode_sys, 
    tf.convert_to_tensor([x_init, y_init], dtype=tf.float64), 
    tf.constant(t_space)).numpy()
