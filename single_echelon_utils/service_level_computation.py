#import pandas as pd
import numpy as np
import math

import os, sys
currentdir = os.path.dirname(os.path.realpath(__file__))
sys.path.append(currentdir)
#from my_log import *
from inventory_level_computation import *


##@log("default_log")
def fill_rate_compound_poisson_demand(demand_size_probability_array: np.array, pos_IL_probability_array: np.array) -> float:
    """Calculates the item fill rate under compound poisson demand.
    
    reference: Axsäter (2006) Inventory control 2nd edition, equation 5.51
    params:
        demand_size_probabilities: np.array of probabilities of demand size 
            equals k, (k = index+1).
        pos_IL_probability_array: np.array of probabilities of inventory level 
            equal to j, (j = index).

    return:
        Item fill rate: float decimal between 0 and 1.

    """

    numerator, denominator = 0,0
    for idx_k,f_k in enumerate(demand_size_probability_array):
        k = idx_k + 1
        denominator += k*f_k
        
        for idx_j,p_IL_equals_j in enumerate(pos_IL_probability_array[1:]):
            j = idx_j +1
            numerator += min(j,k)*f_k*p_IL_equals_j
            

    item_fill_rate = numerator/denominator
    return item_fill_rate

#@log("default_log")
def fill_rate_normal_demand(R: int, Q: int, mean_normal: float, std_dev_normal: float):
    """Calculates the item fill rate under normal demand.
    reference: Axsäter (2006) Inventory control 2nd edition, equation 5.52
    params:
        R: Reorder point
        Q: order quantity
        mean_normal: mean of normal lead time demand
        std_dev_normal: standard deviation of normal lead time demand
    return:
       Item fill rate: float decimal between 0 and 1.
    """

    fill_rate_continuous_normal = 1 - IL_distribution_normal(R, Q, mean_normal, std_dev_normal, x=0)

    return fill_rate_continuous_normal

#This service level is the same as fill rate for continous normal lead time demand.
def ready_rate_continuous_demand(R: int, Q: int, mean_normal: float, std_dev_normal: float) -> float:
    return fill_rate_normal_demand(R, Q, mean_normal, std_dev_normal)

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

#@log("default_log")
def ready_rate_discrete_demand(pos_IL_probability_array: np.array) -> float:
    """Calculates the ready rate for discrete demand.
    
    By definition probability of inventory levels of one and above, equals sum of 
        all probabilities greater than zero.

    Reference: Axsäter (2006), Inventory control 2nd edition, equation 5.50

    params:
        pos_IL_probability_array: np.array of probabilities of inventory level 
            equal to j, (j = index).
    """

    return np.sum(pos_IL_probability_array[1:])

#@log("default_log")
def undershoot_adjustment_normal(R: int, Q: int, mean_normal: int, std_dev_normal: int, demand_prob_array: np.array) -> float:
    """Calculates fill rate with undershoot adjustment U1 in BM (2014)
    
    params:
     R: Reorder point
     Q: order quantity
     mean_normal: mean of normal lead time demand
     std_dev_normal: standard deviation of normal lead time demand
     u: undershoot at retailer
     demand_probability_array: demand probability array 

    return: fill rate with undershoot adjustment
    """
    
    fill_rate_adj_normal = 0

    # calculates fill rate using eq. 21 in BM (2014).
    for u in range(0, Q):
        fill_rate_adj_normal = fill_rate_adj_normal + fill_rate_normal_demand(R-u, Q, mean_normal, std_dev_normal) * prob_undershoot_normal(u, Q, demand_prob_array)
        print(f"fill rate: {fill_rate_normal_demand(R-u, Q, mean_normal, std_dev_normal)}")
        print(f"prob undershoot: {prob_undershoot_normal(u, Q, demand_prob_array)}")
        print(f"adjusted fill rate: {fill_rate_adj_normal}")
    
    return fill_rate_adj_normal

# Maybe:

def cycle_service_compound_demand():
    pass

def cycle_service_normal_demand():
    pass

def main():
    fill_rate = fill_rate_compound_poisson_demand(14, 6, 7.84, 5.3)
    print(fill_rate)
    pass

if __name__ == "__main__":
    main()