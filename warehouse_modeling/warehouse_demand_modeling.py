from scipy import stats
import math
import numpy as np
import pandas as pd

import os, sys
currentdir = os.path.dirname(os.path.realpath(__file__))
parentdir = os.path.dirname(currentdir)
sys.path.append(parentdir)


from single_echelon_utils.demand_models import *

IMPLEMENTED_DEMAND_TYPES = ["Normal","NBD","Poisson"]
THRESHOLD = 1e-6


def delta_func_Normal_demand(Q_dealer: int, L_warehouse: float, mu: float, sigma: float, n: int) -> float:
    """Computes the delta-function value of n for a specific dealer.
    
    The delta-value is interpreted as the probability of a dealer ordering at most
    n orders during the warehouse lead time. Assumes demand faced by dealer to be
    normally distributed.

    reference: Berling and Marklund (2014) eq. 7.
    
    params:
        Q_dealer: Batch quantity at dealer in units.
        L_warehouse: lead time to warehouse, assumed constant.
        mu: mean demand per one time unit at the dealer.
        sigma: standard deviance of demand per one time unit at the dealer.

    """
    a = Q_dealer * stats.norm.cdf( ((n+1)*Q_dealer - mu*L_warehouse) / (sigma*math.sqrt(L_warehouse)) )
    
    b = sigma*math.sqrt(L_warehouse) * ( stats.norm.pdf( ((n+1)*Q_dealer-mu*L_warehouse) / (sigma*math.sqrt(L_warehouse)) ) 
        - stats.norm.pdf( (n*Q_dealer-mu*L_warehouse) / (sigma*math.sqrt(L_warehouse)) ) )

    c = (n*Q_dealer-mu*L_warehouse) * ( stats.norm.cdf( ((n+1)*Q_dealer - mu*L_warehouse) / (sigma*math.sqrt(L_warehouse)) )
        - stats.norm.cdf( (n*Q_dealer-mu*L_warehouse) / (sigma*math.sqrt(L_warehouse)) ) )

    delta = 1/Q_dealer * (a + b + c)
    return delta

def delta_func_NBD_demand(Q_dealer: int, L_warehouse: float, mu: float, sigma: float, n: int):
    """Computes the delta-function value of n for a specific dealer with NBD demand.
    
    The delta-value is interpreted as the probability of a dealer ordering at most
    n orders during the warehouse lead time. Assumes demand faced by dealer to be
    normally distributed.

    reference: Berling and Marklund (2014) eq. 7.
    
    params:
        Q_dealer: Batch quantity at dealer in units.
        L_warehouse: lead time to warehouse, assumed constant.
        mu: mean demand per one time unit at the dealer.
        sigma: standard deviance of demand per one time unit at the dealer.

    """
    demand_probability_arr = demand_prob_arr_negative_binomial(L_warehouse,mu,math.pow(sigma,2))
    max_demand = len(demand_probability_arr)

    # Somewhere here is prbably a good place to find bugs
    prob_sum = 0
    for x in range(1,Q_dealer+1):
        d = n*Q_dealer+x-1
        if d >= max_demand:
            prob_sum += np.sum(demand_probability_arr)
        else:
            prob_sum += np.sum(demand_probability_arr[:d])

    delta = 1/Q_dealer * prob_sum
    return delta

def delta_func_Poisson_demand(Q_dealer: int, L_warehouse: float, mu: float, n: int, sigma: float, ):
    """Computes the delta-function value of n for a specific dealer of Poisson demand.
    
    The delta-value is interpreted as the probability of a dealer ordering at most
    n orders during the warehouse lead time. Assumes demand faced by dealer to be
    normally distributed.

    reference: Berling and Marklund (2014) eq. 7.
    
    params:
        Q_dealer: Batch quantity at dealer in units.
        L_warehouse: lead time to warehouse, assumed constant.
        mu: mean demand per one time unit at the dealer.
        sigma: standard deviance of demand per one time unit at the dealer.

    """
    demand_probability_arr = demand_prob_arr_poisson(L_warehouse,mu)
    max_demand = len(demand_probability_arr)

    # Somewhere here is prbably a good place to find bugs
    prob_sum = 0
    for x in range(1,Q_dealer+1):
        d = n*Q_dealer+x-1
        if d >= max_demand:
            prob_sum += np.sum(demand_probability_arr)
        else:
            prob_sum += np.sum(demand_probability_arr[:d])

    delta = 1/Q_dealer * prob_sum
    return delta

def pmf_func_warehouse_subbatch_demand(Q_dealer: int, L_warehouse: float, mu: float, sigma: float, demand_type: str) -> np.ndarray:
    """Computes the pmf array for all possible subbatches u (u = n*q_i) demanded 
    at the warehouse by a dealer (retailer).

    The function is the probability of a dealer (retailer) ordering exactly u 
    sub-batches.

    f(0) = delta(0)
    f(u)=f(n*q) = delta(n) - delta(n-1)
    
    u is in number of units, n is batches.
    q is Q_dealer in units of a common divisor (sub-batch) Q_divisor:
        Q_dealer = q*Q_divisor.

    reference: Berling and Marklund (2014) eq. 8. or Berling and Marklund (2013)
        eq. 10. Notation from B&M 2014.

    When cumulative probability reaches above 1-THRESHOLD, THRESHOLD is 1e-4, the iteration stops.
        All u's above are assumed to have probability 0.

    params:
        Q_dealer: Batch quantity at dealer in units.
        L_warehouse: lead time to warehouse, assumed constant.
        mu: mean demand per one time unit.
        sigma: standard deviance of demand per one time unit.
        demand_type: Check "IMPLEMENTED_DEMAND_TYPES" for available inputs.

    returns:
        numpy.ndarray of probabilities of a dealer demanding n*q subbatches. index = n.
    """
    delta_func_str = f"delta_func_{demand_type}_demand"
    cumulative_prob = 0
    probability_array = []

    # Doing u = 0, (n = 0), separately.
    f_0 = globals()[delta_func_str](Q_dealer = Q_dealer,L_warehouse = L_warehouse, mu = mu, n = 0, sigma = sigma)
    probability_array.append(f_0)
    cumulative_prob += f_0

    # u = 1*q_i,2*q_i,3*q_i ...
    n = 1
    d_n_1 = f_0
    while cumulative_prob < 1 - 1e-6:
        d_n =  globals()[delta_func_str](Q_dealer = Q_dealer,L_warehouse = L_warehouse, mu = mu, n = n, sigma = sigma)
        f_u = d_n-d_n_1 
        probability_array.append(f_u)
        cumulative_prob += f_u
        
        d_n_1 = d_n
        n += 1
        

    return np.array(probability_array)

def warehouse_demand_variance_term(Q_dealer:int,Q_subbatch:int,L_warehouse:float,mu:float,sigma:float,demand_type:str):
    """Calculates the subbatch demand variance term from a dealer.
    
    reference: Berling and Marklund (2014) p. 3336

    When cumulative probability reaches above 1-THRESHOLD, THRESHOLD is 1e-4, the iteration stops.
        All u's above are assumed to have probability 0.
    
    params:
        Q_dealer: Batch quantity at dealer in units.
        L_warehouse: lead time to warehouse, assumed constant.
        mu: mean demand per one time unit.
        sigma: standard deviance of demand per one time unit.
        demand_type: Check "IMPLEMENTED_DEMAND_TYPES" for available inputs.

    returns:
        sigma2: Variance contribution of a single dealer to the total variance faced
            by the warehouse.
    
    """
    
    if demand_type not in IMPLEMENTED_DEMAND_TYPES:
        raise ValueError("That is not an available demand type.")
    mu_0_dealer = mu*L_warehouse/Q_subbatch
    q = Q_dealer/Q_subbatch
    if not q.is_integer():
        raise ValueError("Q_subbatch should be a divisor of Q_dealer.")

    f_nq = pmf_func_warehouse_subbatch_demand(Q_dealer,L_warehouse,mu,sigma,demand_type)
    mu_nq_square = []
    for n in range(len(f_nq)):
        mu_nq_square.append(math.pow(mu_0_dealer-n*q,2)) 

    return f_nq.dot(np.array(mu_nq_square))

def warehouse_demand_mean_approximation(dealer_mean_demand: np.ndarray, L_warehouse: float, Q_subbatch:int):
    """Calculates warehouse mean.
    
    params:
        dealer_mean_demand: Array containing all dealer mean demand estimations.
        L_warehouse: Constant lead time from outside supplier to warehouse.
        Q_subbatch: Size of the subbatch (smallest common divisor among batch-sizes).

    returns:
        Mean estimate of warehouse subbatch demand.
        """

    return np.sum(dealer_mean_demand)*L_warehouse/Q_subbatch

def warehouse_demand_variance_approximation(Q_dealer_array: np.ndarray, mu_dealer_array: np.ndarray, sigma_dealer_array: np.ndarray, demand_type_array: np.ndarray, L_warehouse: float, Q_subbatch: int) -> float:
    """Computes the warehouse demand variance estimate.
    
    reference: Berling and Marklund 2014 p. 3336
    
    params:
        Q_dealer_array: Dealers batch-quantities in units. 
        mu_dealer_array: Dealers mean demand.
        sigma_dealer_array: Dealers demand variances.

    returns: 
        warehouse demand variance. 
    """

    warehouse_demand_variance = 0

    for Q,mu,sigma,demand_type in zip(Q_dealer_array,mu_dealer_array,sigma_dealer_array,demand_type_array):
        warehouse_demand_variance += warehouse_demand_variance_term(Q,Q_subbatch,L_warehouse,mu,sigma,demand_type)
    
    return warehouse_demand_variance


def warehouse_subbatch_demand_probability_array(Q_dealer_array: np.ndarray, mu_dealer_array: np.ndarray, sigma_dealer_array: np.ndarray, demand_type_array: np.ndarray, L_warehouse: float, Q_subbatch: int):
    """Computes the warehouse subbatch demand probability array estimates.
    
    reference: Berling and Marklund 2014 p. 3336

    OBSERVE! Subbatch demand is not equal to unit demand, e.g. mu_unit = mu_subbatch*subbatch_size.
    
    params:
        Q_dealer_array: Dealers batch-quantities in units. 
        mu_dealer_array: Dealers mean demand.
        sigma_dealer_array: Dealers demand variances.
        L_warehouse: Warehouse leadtime.
        Q_subbatch: Subbatch size (smallest common divisor of Q's)

    returns: 
        Warehouse subbatch demand probability array with probabilities of u = 0,1,2,...
        and warehouse demand type, warehouse mean subbatch demand, 
        warehouse subbarch demand variance.
    """
    mu_L = warehouse_demand_mean_approximation(mu_dealer_array,L_warehouse,Q_subbatch)
    sigma2_L = warehouse_demand_variance_approximation(Q_dealer_array,mu_dealer_array,
        sigma_dealer_array,demand_type_array,L_warehouse,Q_subbatch)
    
    #sigma2_L = 2.0364792**2

    if sigma2_L/mu_L > 1:
        #NBD-dist
<<<<<<< HEAD
        probability_arr = demand_prob_arr_negative_binomial(1,mu_L,sigma2_L)
=======
        probability_arr = demand_prob_arr_negative_binomial(1,mu_L,sigma2_L) # Function requires mean demand during time unit, workaround by putting L = 1.
>>>>>>> ec190a0a39fdaab56c8f96e64111366c640bd336
        warehouse_demand_type = "NBD"

    else:
        if math.sqrt(sigma2_L)/mu_L < 0.25:
            # Normal approx
            F = lambda x: stats.norm.cdf(x, loc = mu_L, scale = math.sqrt(sigma2_L)) 
            warehouse_demand_type = "Normal"
        else: 
            # Gamma approx
            rate = mu_L/sigma2_L
            alpha = mu_L*rate
            F = lambda x: stats.gamma.cdf(x, a = alpha, scale = 1/rate)
            warehouse_demand_type = "Gamma"

        # Computing pmf-array for normal or gamma.
        probability_list = []
        # Separately handling f_u (u = 0)
        #neg_prob = F(-0.5)
        #f_0_marklund = (F(0.5)-F(-0.5))/(1-neg_prob) #CHANGE HERE
        f_0 = F(0.5)  
        probability_list.append(f_0) #CHANGE HERE
        cumulative_prob = f_0 # CHANGE HERE
        
        u = 1
        f_u_1 = f_0 # Keeping last computed value.
        while cumulative_prob < 1-THRESHOLD:
            #Discrete approximation
            f_u_2 = F(u+0.5)
            f_u = (f_u_2-f_u_1)
            #f_u_marklund = (F(u+0.5)-F(u-0.5))/(1-neg_prob) #CHANGE HERE

            probability_list.append(f_u) #HANGE HERE
            cumulative_prob += f_u #HANGE HERE
            u += 1
            f_u_1 = f_u_2

        probability_arr = np.array(probability_list)

    return probability_arr, warehouse_demand_type, mu_L, sigma2_L
   
    

def main():
    print("\n -------------------------------- \n")
    print("Testing delta_func_normal_demand:")
    for n in range(5):
        print(f"For n = {n}, delta = {delta_func_Normal_demand(Q_dealer = 2,L_warehouse = 5,mu = 1,sigma = 2, n=n)}")

    print("\n")
    print("Testing pmf-func w. normal.:")
    print(f"Probability arr: {pmf_func_warehouse_subbatch_demand(Q_dealer = 2, L_warehouse = 5, mu = 1, sigma = 2,demand_type = 'Normal')}")
    print(f"Sum of array is: {np.sum(pmf_func_warehouse_subbatch_demand(Q_dealer = 2, L_warehouse = 5, mu = 1, sigma = 2,demand_type= 'Normal'))}")

    print("\n")
    print("Testing warehouse demand subbatch variance, Normal demand.")
    print(f"Variance is: {warehouse_demand_variance_term(Q_dealer = 2, Q_subbatch=1, L_warehouse = 5, mu = 1, sigma = 2, demand_type = 'Normal')}")

    print("\n -------------------------------- \n")
    print("Testing delta_func_NBD_demand:")
    for n in range(5):
        print(f"For n = {n}, delta = {delta_func_NBD_demand(Q_dealer = 2,L_warehouse = 5,mu = 1,sigma = 2, n=n)}")

    print("\n")
    print("Testing pmf-func w. NBD.:")
    print(f"Probability arr: {pmf_func_warehouse_subbatch_demand(Q_dealer = 2, L_warehouse = 5, mu = 1, sigma = 2,demand_type = 'NBD')}")
    print(f"Sum of array is: {np.sum(pmf_func_warehouse_subbatch_demand(Q_dealer = 2, L_warehouse = 5, mu = 1, sigma = 2,demand_type= 'NBD'))}")

    print("\n")
    print("Testing warehouse demand subbatch variance, NBD demand.")
    print(f"Variance is: {warehouse_demand_variance_term(Q_dealer = 2, Q_subbatch=1, L_warehouse = 5, mu = 1, sigma = 2, demand_type = 'NBD')}")

if __name__ == "__main__":
    main()

