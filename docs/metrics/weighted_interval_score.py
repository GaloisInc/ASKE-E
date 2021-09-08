import numpy as np


def IS_alpha(u, l, alpha, y):
    """
    Closer to 0 score indicates a better model. Note that if l <= y <= u, the two terms = 0 due to the indicator
    function. The last two terms are penalty terms which penalizes the interval score if the observed value, y,
    falls outside of the predicted interval, [l, u]

    If alpha = 0.02, the predictive interval is [0.01, 1-0.01] = [0.01, 0.99]

    Refer to the slides for more information

    Parameters
    ----------
    u : float
        The upper bound for the predictive interval: alpha/2 quantile of F
    l : float
        The lower bound for the predictive interval: 1 - alpha/2 quantile of F
    alpha : [0, 1]
        Uncertainty level. Predictive intervals are based on alphas: (1 - alpha)
    y : float
        The observation (ground truth)

    Returns
    -------
    interval_score : float
        A positive score that evaluates the accuracy of the prediction based on the data
    """
    if y < l:
        # if the observed data, y, is to the left (outside) of the lower bound
        I_left = 1
    else:
        I_left = 0

    if y > u:
        # if the observed data, y, is to the right (outside) of the upper bound
        I_right = 1
    else:
        I_right = 0

    interval_score = (u-l) + (2/alpha)*(l-y)*I_left + (2/alpha)*(y-u)*I_right

    return interval_score


def WIS_alpha(m, y, alphas, F):
    """
    Weighted Interval Score (WIS) is an aggregation of multiple predictive intervals. Each predictive interval is
    based on a particular alpha level

    Refer to the slides for more information

    Parameters
    ----------
    m : float
        The predictive median (median: 50% of the predicted values are above or below it)
    y : float
        The observed value (ground truth) used to evaluate the model (a particular skill, such as number hospitalized or
        number of deaths). Depending on the observation, it may be an integer instead.
    alphas : list
        List containing all of the uncertainty levels (alpha), where alpha = [0, 1]. Predictive intervals are based
        on alphas: (1 - alpha). The predictive intervals are represented by quantiles.
    F : Predictive distribution
        The predictive distribution from a model

    Returns
    -------
    weighted_interval_score : float
        A positive real number. Closer to 0 the better the model

    """
    # K = number of interval scores, intervals are based on alpha
    # len(alpha) = K
    # alpha = uncertainty level (alpha = 0.02, 0.05, 0.1, 0.2, ..., 0.9)
    # m = predictive median
    w_0 = 1/2
    K = len(alphas)

    aggregated_interval_score = 0
    for alpha in alphas:
        w_k = alpha/2

        l = quantile(alpha/2, F)
        u = quantile(1 - alpha/2, F)

        interval_score = IS_alpha(u, l, alpha, y)

        aggregated_interval_score += w_k*interval_score

    weighted_interval_score = 1/(K+(1/2)) * (w_0 * abs(y - m) + aggregated_interval_score)

    return weighted_interval_score


def quantile(q, F):
    """
    Pr[X <= x] = q

    Parameters
    ----------
    q : float
        Fraction, quantile specified [0,1]
    F : Predictive distribution
        The predictive distribution from a model

    Returns
    -------
    x : float
        Value of the F that results in the F values < x for p fraction
    """

    # either use samples from a distribution or use the raw distribution
    samples = sample(F)

    return np.quantile(samples, q)
