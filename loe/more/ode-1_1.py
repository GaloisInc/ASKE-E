import numpy as np

from scipy.integrate import solve_ivp

ode_fn = lambda t, x: np.sin(t) + 3. * np.cos(2. * t) - x

t_begin=0.
t_end=10.
t_nsamples=100
t_space = np.linspace(t_begin, t_end, t_nsamples)
x_init = 0.

method = 'RK45' #available methods: 'RK45', 'RK23', 'DOP853', 'Radau', 'BDF', 'LSODA'
num_sol = solve_ivp(ode_fn, [t_begin, t_end], [x_init], method=method, dense_output=True)
x_num_sol = num_sol.sol(t_space).T
