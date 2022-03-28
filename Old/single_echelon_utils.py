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

def demand_probability_array_compound_poisson(L: int, E_z: float, V_z: float, compounding_dist) -> np.array:
    pass

def demand_prob_arr_comp_poisson_logarithmic(L: int, E_z: float, V_z: float, threshold = 1e-4):
    """Computes the array of demand probabilities under logarithmic compound poisson.
    
    This function actually uses the fact that logarithmic compound poisson is the 
    negative binomial distribution.

    reference: Axsäter 2006, Inventory control, p. 81, eq 5.15-5.17

    Params:
        L: Lead time
        E_z: Mean demand during one time unit.
        V_z: Variance of demand during one time unit.
        threshold: Demand with probability less than this is deemed to be zero.
    
    """

    # First find params r and p.
    p = 1 - (E_z/V_z)
    r = E_z*(1-p)/p
  
    demand_prob_arr = []

    # First do k = 0
    demand_prob_arr.append((1-p)**r)
    
    # Then do for k = 1,2,...
    p = 1
    k = 1
    while p > threshold:
        P_D_k = 0
        temp = 0
        while temp < k:
            P_D_k *= (r + temp)
            temp += 1

        P_D_k = (P_D_k / np.math.factorial(k))*((1-p)**r)*(p**k)

        p = P_D_k
        demand_prob_arr.append(P_D_k)
        k += 1

    return np.array(demand_prob_arr)


    