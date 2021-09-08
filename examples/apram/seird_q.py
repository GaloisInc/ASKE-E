#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


import aPRAM_utils as utils
from aPRAM_expressions import WN

from aPRAM_settings import pop,sim

pop.size = 100000
delta = 0.001

pop.reset()
sim.reset()

pop.make_column('health',np.zeros(pop.size))
S = 1
E = 2
I = 3
R = 4
D = 5
pop.make_column('quarantined', np.zeros(pop.size))
Q = 1
NQ = 2

pop.make_param('beta', 0.3)

# def p_transmission (beta):
#     # Infectious agents -- those in cohort F -- can transmit infections
#     # The probability of being an infectious agent is:
#     p_infectious = pop.F.size / pop.size

#     # Similarly, the probability of being a Susceptible agent is:
#     p_susceptible = pop.S.size / pop.size

#     # The probability that one meeting between two agents has one
#     # infectious and one Susceptible is:
#     p_one_potential_transmission = p_infectious * p_susceptible

#     # Potential transmissions become actual transmissions with
#     # probability beta:
#     p_one_transmission = p_one_potential_transmission * beta

#     # return both probability of transmission and its complement,
#     # as Mods will need both

#     return [p_one_transmission, 1 - p_one_transmission]

pop.make_cohort('P', lambda: np.ones(pop.size).astype(bool))
pop.make_cohort('S', lambda: pop.health.eq(S))
pop.make_cohort('E', lambda: pop.health.eq(E))
pop.make_cohort('I', lambda: pop.health.eq(I))
pop.make_cohort('R', lambda: pop.health.eq(R))
pop.make_cohort('D', lambda: pop.health.eq(D))
pop.make_cohort('Q', lambda: pop.quarantined.eq(Q))
pop.make_cohort(
    'F',
    lambda: np.logical_and(
        pop.quarantined.eq(NQ),
        np.logical_or(
            pop.health.eq(E), 
            pop.health.eq(I)
        )
    )
)

sim.make_mod(
    name= 'initialize_attributes',
    cohort = pop.P,
    mods = [ 
        [lambda **kwargs: pop.health.assign(S, **kwargs), lambda **kwargs: pop.quarantined.assign(NQ, **kwargs)] 
    ],
    prob_spec=lambda: [
        1
    ],
    sim_phase='setup'
)

sim.make_mod(
    name = 'seed_infections',
    cohort = pop.P,
    mods = [
        [lambda **kwargs: pop.health.assign(E, **kwargs)],
        [lambda **kwargs: pop.health.assign(S, **kwargs)],
    ],
    prob_spec = [
        0.001,
        0.999
    ],
    sim_phase= 'setup'
    )

sim.make_mod(
    name= 'susceptible_to_exposed',
    cohort = pop.S,
    mods = [
        [lambda **kwargs: pop.health.assign(E, **kwargs)],
        []
    ],
    prob_spec = lambda: [
        (pop.F.size / pop.size) * (pop.S.size / pop.size) * pop.beta.val,
        1 - (pop.F.size / pop.size) * (pop.S.size / pop.size) * pop.beta.val,
    ],
    sim_phase = 'loop'
)

sim.make_mod(
    name = 'exposed_to_next',
    cohort = pop.E,
    mods = [ 
        [lambda **kwargs: pop.health.assign(I, **kwargs)],
        [lambda **kwargs: pop.health.assign(R, **kwargs)],
        [],
    ],
    prob_spec = [
        0.2,
        0.2,
        0.6,
    ],
    sim_phase = 'loop'
    )

sim.make_mod(
    name = 'infected_to_next',
    cohort = pop.I,
    mods = [ 
        [lambda **kwargs: pop.health.assign(R, **kwargs)],
        [lambda **kwargs: pop.health.assign(D, **kwargs)],
        [], 
    ],
    prob_spec = [
        0.1,
        0.01,
        0.89,
    ],
    sim_phase = 'loop'
    )

sim.make_mod(
    name = 'quarantine',
    cohort = pop.I,
    mods = [ 
        [lambda **kwargs: pop.quarantined.assign(Q, **kwargs), lambda **kwargs: pop.health.assign(D, **kwargs)],
        []
    ],
    prob_spec = [0.1, 0.9],
    sim_phase = 'loop'
    )


# ASKEE_STOP

def probe_fn (day):
    record = [day, sim.susceptible_to_exposed.mod_selector.probs[0]]
    record.extend([c.size for c in [pop.S,pop.E,pop.I,pop.R,pop.D,pop.F,pop.Q]])
    return record

probe_labels = ['day','p','S','E','I','R','D','F','Q']


sim.num_iterations  = 500
sim.probe_labels    = probe_labels
sim.probe_fn        = probe_fn

sim.run_simulation()


df1 = pd.DataFrame(sim.records,columns=probe_labels)

print(df1)
df1.S.plot(label='S')
df1.E.plot(label='E')
df1.I.plot(label='I')
df1.R.plot(label='R')
df1.D.plot(label='D')
df1.Q.plot(label='Q')
plt.legend()
plt.show()
