import math

def exponentialCDF(x, rate):
    return (1.0 - math.exp(-rate * x))

def rates2prob(rates, deltaT):
    selfloop = 0.0
    rateSum = 0.0
    prob = []
    for rate in rates:
        rateSum += rate
    prob.append(1.0 - exponentialCDF(deltaT, rateSum))
    for rate in rates:
        conditional = rate / rateSum
        probThis = exponentialCDF(deltaT, rateSum)
        prob.append(probThis*conditional)
    return prob
