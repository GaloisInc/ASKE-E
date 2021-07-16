import numpy as np

import tensorflow as tf
from tfdiffeq import odeint

ode_fn = lambda t, x: tf.math.sin(t) + 3. * tf.math.cos(2. * t) - x

t_begin=0.
t_end=10.
t_nsamples=100
t_space = np.linspace(t_begin, t_end, t_nsamples)
x_init = tf.constant([0.])

x_num_sol = odeint(ode_fn, x_init, tf.constant(t_space))
