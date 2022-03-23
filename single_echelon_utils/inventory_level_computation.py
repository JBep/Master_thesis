import numpy as np
from scipy import stats 
from demand_models import *

def probability_IL_compound_poisson(R: int,Q: int,demand_probability_array: np.array,j: int) -> float:
    """Calculates the probability of IL equals j under compound poisson demand.

    Assumes compound poisson stationary lead time demand and an (R,Q)-policy

    Reference: Axsäter (2006) Inventory control 2nd edition, equation 5.36
    
    params:
        R: Re-order point.
        Q: Order quantity.
        demand_probability_array: np.array of probabilities of demand 
            equal to d, (d = index).
    """

    demand_prob_sum = 0
    k = max(R+1,j)
    for k in range(max(R+1,j),R+Q):
        demand_prob_sum = demand_prob_sum + demand_probability_array[k-j]

    return 1/Q*demand_prob_sum

def IL_prob_array_discrete_positive():
    pass

def IL_prob_array_discrete_negative():
    pass

# Re-work this into the above ones.
def IL_probability_array_compound_poisson(R: int, Q: int, L: int, E_z: float, 
    V_z: float, compounding_dist: str, positive = 1, threshold = 1e-4) -> np.array:
    """Computes an array of IL probabilities.
    
    Params:
        R: Reorder point
        Q: order quantity
        L: Lead time.
        E_z: mean demand during a time unit.
        V_z: variance of demand during a time unit.
        positive: If 1, returns probabilities of positive IL=j (j=index), 
            f -1, returns probabilities of negative IL = j (j = -index).
        threshold: when probability goes below the threshold, all IL above is 
            assumed to have probability zero.

    Returns:
        Array of IL probabilities.
    """
    if positive != 1 or positive != -1:
        raise ValueError("Positive needs to be 1 or -1.")

    IL_probabilies_array = []
    p = 1
    j = 0
    while p > threshold:
        demand_probability_array = demand_probability_array_compound_poisson(L, mu, sigma2)
        IL_probabilies_array.append(probability_IL_compound_poisson(R,Q,demand_probability_array,j))


def IL_distribution_normal(R: int, Q: int, mean_normal: int, std_dev_normal: int, x: int) -> float: 
    """Computes an array of IL probabilities.
    
    Params:
        R: Reorder point
        Q: order quantity
        mean_normal: mean of normal lead time demand
        std_dev_normal: standard deviation of normal lead time demand
        x: inventory level

    Returns:
        Probability of IL<=x
    """
    
    x1 = (R-x-mean_normal)/std_dev_normal
    x2 = (R+Q-x-mean_normal)/std_dev_normal

    IL_dist = (std_dev_normal/Q)*(loss_function(x1)-loss_function(x2))

    return IL_dist

def loss_function(x: int):
    """Computes loss function G(x)
    
    Params:
        x: variable

    Returns:
        Loss function
    """
    function = stats.norm.pdf(x)-x*(1-stats.norm.cdf(x))

    return function

def main():
    fill_rate = IL_distribution_normal(14, 6, 7.84, 5.3, 0)
    print(fill_rate)
    pass

if __name__ == "__main__":
    main()


