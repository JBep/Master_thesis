import numpy as np
from scipy import stats 
from demand_models import *

def IL_prob_array_discrete_positive(R: int, Q: int,demand_probability_array: np.ndarray) -> float:
    """Calculates the inventory level distribution for all positive values.
    
    Assumes discrete demand distribution.
    Reference: Axsäter (2006) Inventory control, eq 5.36

    Params: 
        R: Re-order point.
        Q: Order quantity.
        demand_probability_array: np.ndarray of probabilities of demand 
            equal to d, (d = index).

    Returns:
        np.ndarray of probabilities of IL equal to j, (j = index), 0 <= j <= R+Q.
    """
    # Assert that there won't be any indexes out of bound:
    diff = R+Q+1-len(demand_probability_array)
    if diff > 0:
        demand_probability_array = np.pad(demand_probability_array, pad_width = (0, diff))


    IL_prob_array = np.zeros(R+Q+1)
    for j in range(R+Q+1):
        demand_prob_sum = 0
        k = max(R+1,j)
        for k in range(max(R+1,j),R+Q+1):
            demand_prob_sum = demand_prob_sum + demand_probability_array[k-j]

        IL_prob_array[j] = 1/Q*demand_prob_sum
    
    return IL_prob_array

def IL_prob_array_discrete_negative():
    # Implement if needed, probably not.
    pass


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

# Går Oi göra generell??
def prob_undershoot_compound_possion(u: int, Q: int, demand_probability_array: np.array) -> float:
    """Computes an array of IL probabilities.
    
    Params:
        u: undershoot at retailer
        Q: order quantity at retailer 
        demand_probability_array: demand probability array 

    Returns:
        
    """
    #Create variable for undershoot prob
    undershoot_prob = 0

    #Calculate probability of an undershoot of size u using eq. 19 in BM (2014)
    for k in range(u+1, u+Q):
        undershoot_prob = undershoot_prob + (1/Q)*demand_probability_array(k)

    return undershoot_prob 

#TO-DO
def prob_undershoot_normal():
    pass

#TO-DO
def prob_undershoot_compound_poisson():
    pass


def main():
    pass

if __name__ == "__main__":
    main()


