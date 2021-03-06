import numpy as np
import math
import os, sys

currentdir = os.path.dirname(os.path.realpath(__file__))
sys.path.append(currentdir)
from my_log import *

@log("default_log")
def norm_sigma(my: float, sigma: float, l: float) -> float:
    """Normalization of sigma. 
    
    Reference: For conversion formula see Table 3.
    
    my = expected demand per time unit
    sigma = standard deviation of demand per time unit
    l = transportation time

    return:
        Item fill rate: float decimal between 0 and 1.

    """

    sigma_norm = 100 * sigma/(my*np.sqrt(l))
    return sigma_norm

def g_factor(h: float, Q: int, p: float, my: float, l: float):
    """Calculate g factor with input normalized Q and p (is necessary for testing against tabulated numbers) 
    
    Reference: For normalized conversion formula see Table 3.
               For equation see (51). 
    
    Q = order quantity
    p = shortage cost

    return:
        g factor

    """

    #normalize Q
    Q_norm = Q*100*my/l

    #normalize p
    p_norm = p/h
    
    #calculate ga, gb and G
    g_a = min(0.015*p_norm, max(0.65/np.sqrt(p_norm), 0.05))
    g_b = max(-1.2, -2*p_norm**(-0.25))
    G = min(0.015, 0.005*p_norm**0.2)

    #calculate g factor
    g_factor = min(g_a*Q_norm**g_b, G)

    return g_factor
   
#Calculate k factor with input normalized Q and p (is necessary for testing against tabulated numbers)
#See eq. (52)
def k_factor(h: float, Q: int, p: float, my: float, l: float):
    """Calculate k factor with input normalized Q and p (is necessary for testing against tabulated numbers) 
    
    Reference: For normalized conversion formula see Table 3.
               For equation see (52). 
    
    Q = order quantity
    p = shortage cost
    my = standard deviation of customer demand

    return:
        k factor

    """
    #normalize Q
    Q_norm = Q*100*my/l

    #normalize p
    p_norm = p/h

    #calculate ka, kb, K
    k_a = max(0.7, min(0.9, 0.6*p_norm**0.075))
    k_b = min(0.2, 0.4*p_norm**(-0.35))
    K = max(1.3, min(2, 2.5*p_norm**(-0.15)))

    #calculate k factor
    k_factor = max(1, min(k_a*Q_norm**k_b, K))

    return k_factor

@log("default_log")
def induced_backorder_cost_opt(h: float, Q: int, p: int, l: int, my: float, sigma: float) -> float:
    """Calculates optimal induced backorder cost faced by RDC from one dealer/retailer.
    
    Reference: For equation see (49)
    
    params:
        h = holding cost at the dealer.
        Q = order quantity of the dealer.
        p = shortage cost per unit at the dealer.
        l = transportation time for an order from the RDC to the dealer.
        my = expected demand per time unit at the dealer.
        sigma = standard deviation of demand per time unit at the dealer.
  

    return:
        Beta: induced backorder cost from a dealer at the warehouse.

    """
    #calculate g and k factor
    g_Q_p = g_factor(h, Q, p, my, l)
    k_Q_p = k_factor(h, Q, p, my, l)

    beta_opt = h * g_Q_p * math.pow(norm_sigma(my, sigma, l),k_Q_p)

    return beta_opt

@log("default_log")
def weighting_backorder_cost(mu_i_array: np.ndarray, my_0: float, beta_opt_array: np.ndarray) -> float:
    """Weighting optimal induced backorder for non-identical retailers
    
    Reference: For equation see (53)
    
     my_i = mean demand at retailer i-1
     my_0 = mean demand at supplying warehouse
     beta_opt_array = optimal beta at retailer i-1

    return:
        Weighted optimal induced backorder cost

    """
    weighted_beta_opt = 0

    for i,mu_i in enumerate(mu_i_array):
        weighted_beta_opt += (mu_i/my_0) * beta_opt_array[i]

    return weighted_beta_opt
    
