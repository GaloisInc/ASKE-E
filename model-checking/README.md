# Model Checking COVID-19 Spread 

We consider working on a problem of model-checking COVID-19 widespread using compartmental epidemiology models such as SIR (Susceptible, Infectious, Recovery) and SIS (Susceptible, Infectious, Susceptible). These models are Generalized Stochastic Petri-nets (GSPNs) in nature and the probable candidate probabilistic model checking tools available are [PRISM](https://www.prismmodelchecker.org) and [STORM](https://www.stormchecker.org/index.html). The former, although one of the widely used model checking tools available today, does not have direct support for GSPNs. However, one can work around it by posing GSPNs as Continuous Time Markov Chains (CTMC) and verify some quantitative properties about them. The latter, a modern probabilistic model checker, however, does seem to have support for GSPNs in addition to

* Discrete Time Markov Chains (DTMC)
* Continuous Time Markov Chains (CTMC)
* Markov Decision Processes (MDP)
* Markov Automata (MA)

 Moreover, it has support for accepting input models specified in other tool-specific languages, such as PRISM, JANI, GSPNs, and so forth.


## Running Storm

One can verify properties, specified in sir.prism, of a model, sir.props, using the command:

```shell
storm --prism sir.prism -prop sir.props --prismcompat --constants N=100 --timemem 
```
We can also obtain precise probabilities using the keyword `--exact` keyword
