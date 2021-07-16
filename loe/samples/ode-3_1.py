import numpy as np

from scipy.integrate import solve_ivp

def ode_sys(t, X):
	x=X[0]
	dx_dt=X[1]
	d2x_dt2=-dx_dt - 2*x
	return [dx_dt, d2x_dt2]

t_begin=0.
t_end=12.
t_nsamples=100
t_space = np.linspace(t_begin, t_end, t_nsamples)
x_init = 1.
dxdt_init = 0.

method = 'RK45'
num_sol = solve_ivp(ode_sys, [t_begin, t_end], [x_init, dxdt_init], method=method, dense_output=True)
X_num_sol = num_sol.sol(t_space)
x_num_sol = X_num_sol[0].T
