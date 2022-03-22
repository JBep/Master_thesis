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

# To-do
def fill_rate_normal_demand() -> float:
    return ready_rate_continuous_demand()

#To-do
def ready_rate_continuous_demand() -> float:
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

# Maybe:

def cycle_service_compound_demand():
    pass

def cycle_service_normal_demand():
    pass