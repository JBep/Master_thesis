import pandas as pd
import numpy as np

def fill_rate_compound_poisson_demand(demand_size_probability_array: np.array, pos_IL_probability_array: np.array) -> float:
    """Calculates the item fill rate under compound poisson demand.
    
    reference: Axsäter (2006) Inventory control 2nd edition, equation 5.51
    params:
        demand_size_probabilities: np.array of probabilities of demand size 
            equals k, (k = index).
        pos_IL_probability_array: np.array of probabilities of inventory level 
            equal to j, (j = index).

    return:
        Item fill rate: float decimal between 0 and 1.

    """

    numerator, denominator = 0,0
    for f_k,k in enumerate(demand_size_probability_array[1:]):
        for p_IL_equals_j,j in enumerate(pos_IL_probability_array[1:]):
            numerator = numerator + np.min(j,k)*f_k*p_IL_equals_j
            denominator = denominator + k*f_k

    item_fill_rate = numerator/denominator
    return item_fill_rate

def fill_rate_normal_demand() -> float:
    pass

def fill_rate_poisson_demand(pos_IL_probability_array: np.array) -> float:
    """Calculates the item fill rate for poisson demand
    
    For poisson demand the item fill rate degenerates to the expression of
    ready rate.

    Reference: Axsäter (2006) Inventory control 2nd edition, equation 5.51 
        (ready rate equation 5.50)

    params:
        pos_IL_probability_array: np.array of probabilities of inventory level 
            equal to j, (j = index).
    """
    return ready_rate_discrete_demand(pos_IL_probability_array)

def ready_rate_discrete_demand(pos_IL_probability_array: np.array) -> float:
    """Calculates the ready rate for discrete demand.
    
    By definition probability of inventory level zero or above, equals sum of 
        all probabilities greater than zero.

    Reference: Axsäter (2006), Inventory control 2nd edition, equation 5.50

    params:
        pos_IL_probability_array: np.array of probabilities of inventory level 
            equal to j, (j = index).
    """

    return np.sum(pos_IL_probability_array)


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
