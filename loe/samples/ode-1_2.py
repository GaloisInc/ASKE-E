import numpy as np

import tensorflow as tf
import tensorflow_probability as tfp

ode_fn = lambda t, x: tf.math.sin(t) + tf.constant(3.) * tf.math.cos(tf.constant(2.) * t) - x

t_begin=0.
t_end=10.
t_nsamples=100
t_space = np.linspace(t_begin, t_end, t_nsamples)
t_init = tf.constant(t_begin)
x_init = tf.constant(0.)

num_sol = tfp.math.ode.BDF().solve(ode_fn, t_init, x_init,
	solution_times=tfp.math.ode.ChosenBySolver(tf.constant(t_end)) )
