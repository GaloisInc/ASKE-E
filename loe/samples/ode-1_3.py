import numpy as np

import torch
from torchdiffeq import odeint

ode_fn = lambda t, x: torch.sin(t) + 3. * torch.cos(2. * t) - x

t_begin=0.
t_end=10.
t_nsamples=100
t_space = np.linspace(t_begin, t_end, t_nsamples)
x_init = torch.tensor([0.])

x_num_sol = odeint(ode_fn, x_init, torch.tensor(t_space))
