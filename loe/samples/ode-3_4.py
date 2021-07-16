import numpy as np

import tensorflow as tf
from tfdiffeq import odeint

def ode_sys(t, X):
	x=X[0]
	dx_dt=X[1]
	d2x_dt2=-dx_dt - 2*x
	return tf.stack([dx_dt, d2x_dt2])

t_begin=0.
t_end=12.
t_nsamples=100
t_space = np.linspace(t_begin, t_end, t_nsamples)
x_init = tf.constant([1.])
dxdt_init = tf.constant([0.])

num_sol = odeint(
	ode_sys, 
	tf.convert_to_tensor([x_init, dxdt_init], dtype=tf.float64), 
	tf.constant(t_space)).numpy()
