using DifferentialEquations

ode_fn(x,p,t) = sin(t) + 3.0 * cos(2.0 * t) - x

t_begin=0.0
t_end=10.0
tspan = (t_begin,t_end)
x_init=0.0

prob = ODEProblem(ode_fn, x_init, tspan)
num_sol = solve(prob, Tsit5(), reltol=1e-8, abstol=1e-8)
