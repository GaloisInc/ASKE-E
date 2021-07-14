import numpy as np
import torch

from neurodiffeq import diff
from neurodiffeq.ode import solve_system
from neurodiffeq.ode import IVP
from neurodiffeq.ode import Monitor
import neurodiffeq.networks as ndenw

ode_sys = lambda x, y, t: [diff(x, t, order=1) + x - y, diff(y, t, order=1) - 4. * x + y ]

t_begin=0.
t_end=2.
t_nsamples=100
t_space = np.linspace(t_begin, t_end, t_nsamples)
x_init = IVP(t_0=t_begin, x_0=2.0)
y_init = IVP(t_0=t_begin, x_0=0.0)

batch_size=200

net = ndenw.FCNN(
	n_input_units=1,
        n_output_units=2,
	n_hidden_layers=3, 
	n_hidden_units=50, 
	actv=ndenw.SinActv)

optimizer = torch.optim.Adam(net.parameters(), lr=0.003)
 
num_sol, history = solve_system(
	ode_system=ode_sys,
	conditions=[x_init, y_init], 
	t_min=t_begin, 
	t_max=t_end,
	batch_size=batch_size,
	max_epochs=1200,
	return_best=True,
	single_net = net,
	optimizer=optimizer,
	monitor=Monitor(t_min=t_begin, t_max=t_end, check_every=10))
num_sol = num_sol(t_space, as_type='np')
