import numpy as np

import tensorflow as tf
import tensorflow_probability as tfp

def ode_sys(t, X):
	x=X[0]
	dx_dt=X[1]
	d2x_dt2=-dx_dt - 2*x
	return [dx_dt, d2x_dt2]

t_begin=0.
t_end=12.
t_nsamples=100
t_space = np.linspace(t_begin, t_end, t_nsamples)
t_init = tf.constant(t_begin)
x_init = tf.constant(1.)
dxdt_init = tf.constant(0.)

num_sol = tfp.math.ode.BDF().solve(ode_sys, t_init, [x_init, dxdt_init],
	solution_times=tfp.math.ode.ChosenBySolver(tf.constant(t_end)) )
