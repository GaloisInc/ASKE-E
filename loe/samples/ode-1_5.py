import numpy as np
import torch

from neurodiffeq import diff
from neurodiffeq.ode import solve
from neurodiffeq.ode import IVP
from neurodiffeq.ode import Monitor
import neurodiffeq.networks as ndenw

ode_fn = lambda x, t: diff(x, t, order=1) + x - torch.sin(t) - 3. * torch.cos(2. * t)

t_begin=0.
t_end=2.
t_nsamples=100
t_space = np.linspace(t_begin, t_end, t_nsamples)
x_init = IVP(t_0=t_begin, x_0=0.0)

net = ndenw.FCNN(n_hidden_layers=6, n_hidden_units=50, actv=torch.nn.Tanh)
optimizer = torch.optim.SGD(net.parameters(), lr=0.001)
num_sol, loss_sol = solve(ode_fn, x_init, t_min=t_begin, t_max=t_end,
	batch_size=30,
	max_epochs=1000,
	return_best=True,
	net=net,
	optimizer=optimizer,
	monitor=Monitor(t_min=t_begin, t_max=t_end, check_every=10))
x_num_sol = num_sol(t_space, as_type='np')
