using StaticArrays
using DifferentialEquations

A = @SMatrix [-1.0 1.0
              4.0 -1.0]

function ode_fn(du,u,p,t)
    du[[true, true]] = A * u
end

t_begin=0.0
t_end=5
tspan = (t_begin,t_end)
x_init=2.0
y_init=0.0

prob = ODEProblem(ode_fn, [x_init, y_init], tspan)
num_sol = solve(prob, Tsit5(), reltol=1e-8, abstol=1e-8)
x_num_sol = [u[1] for u in num_sol.u]
y_num_sol = [u[2] for u in num_sol.u]
