import numpy as np

import torch
from torchdiffeq import odeint

def ode_sys(t, XY):
	x=XY[0]
	y=XY[1]
	dx_dt= torch.Tensor([- x + y])
	dy_dt= torch.Tensor([4. * x - y])
	return torch.cat([dx_dt, dy_dt])

t_begin=0.
t_end=5.
t_nsamples=100
t_space = np.linspace(t_begin, t_end, t_nsamples)
x_init = torch.Tensor([2.])
y_init = torch.Tensor([0.])

num_sol = odeint(ode_sys, torch.cat([x_init, y_init]), torch.Tensor(t_space)).numpy()

