import numpy as np

import tensorflow as tf
import tensorflow_probability as tfp

def ode_sys(t, XY):
	x=XY[0]
	y=XY[1]
	dx_dt= - x + y
	dy_dt= 4. * x - y
	return [dx_dt, dy_dt]

t_begin=0.
t_end=5.
t_nsamples=100
t_space = np.linspace(t_begin, t_end, t_nsamples)
t_init = tf.constant(t_begin)
x_init = tf.constant(2.)
y_init = tf.constant(0.)

num_sol = tfp.math.ode.BDF().solve(ode_sys, t_init, [x_init, y_init],
	solution_times=tfp.math.ode.ChosenBySolver(tf.constant(t_end)) )
