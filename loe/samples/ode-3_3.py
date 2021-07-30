import numpy as np

import torch
from torchdiffeq import odeint

def ode_sys(t, X):
	x=torch.Tensor([X[0]])
	dx_dt=torch.Tensor([X[1]])
	d2x_dt2=torch.Tensor([-dx_dt - 2*x])
	return torch.cat([dx_dt, d2x_dt2])

t_begin=0.
t_end=12.
t_nsamples=100
t_space = np.linspace(t_begin, t_end, t_nsamples)
x_init = torch.Tensor([1.])
dxdt_init = torch.Tensor([0.])

num_sol = odeint(ode_sys, torch.cat([x_init, dxdt_init]), torch.Tensor(t_space)).numpy()
