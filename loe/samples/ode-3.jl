using DifferentialEquations

function ode_fn(dx,x,p,t)
    -dx -2.0 * x
end

t_begin=0.0
t_end=12.0
tspan = (t_begin,t_end)
x_init=1.0
dxdt_init=0.0

prob = SecondOrderODEProblem(ode_fn, dxdt_init, x_init, tspan)
num_sol = solve(prob, Tsit5(), reltol=1e-8, abstol=1e-8)
x_num_sol = [u[2] for u in num_sol.u]
