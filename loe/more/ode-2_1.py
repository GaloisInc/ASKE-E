import numpy as np

from scipy.integrate import solve_ivp

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
x_init = 2.
y_init = 0.

method = 'RK45' #available methods: 'RK45', 'RK23', 'DOP853', 'Radau', 'BDF', 'LSODA'
num_sol = solve_ivp(ode_sys, [t_begin, t_end], [x_init, y_init], method=method, dense_output=True)
XY_num_sol = num_sol.sol(t_space)
x_num_sol = XY_num_sol[0].T
y_num_sol = XY_num_sol[1].T
