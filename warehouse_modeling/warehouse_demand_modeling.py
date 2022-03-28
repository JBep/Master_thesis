from scipy import stats
import math
import numpy as np

def delta_func_normal_demand(Q_dealer: int, L_warehouse: float, mu: float, sigma: float, n: int) -> float:
    """Computes the delta-function value of n for a specific dealer.
    
    The delta-value is interpreted as the probability of a dealer ordering at most
    n orders during the warehouse lead time. Assumes demand faced by dealer to be
    normally distributed.

    reference: Berling and Marklund (2014) eq. 7.
    
    params:
        Q_dealer: Batch quantity at dealer in units.
        L_warehouse: lead time to warehouse, assumed constant.
        mu: mean demand per one time unit.
        sigma: standard deviance of demand per one time unit.

    """
    a = Q_dealer * stats.norm.cdf( ((n+1)*Q_dealer - mu*L_warehouse) / (sigma*math.sqrt(L_warehouse)) )
    
    b = sigma*math.sqrt(L_warehouse) * ( stats.norm.pdf( ((n+1)*Q_dealer-mu*L_warehouse) / (sigma*math.sqrt(L_warehouse)) ) 
        - stats.norm.pdf( (n*Q_dealer-mu*L_warehouse) / (sigma*math.sqrt(L_warehouse)) ) )

    c = (n*Q_dealer-mu*L_warehouse) * ( stats.norm.cdf( ((n+1)*Q_dealer - mu*L_warehouse) / (sigma*math.sqrt(L_warehouse)) )
        - stats.norm.cdf( (n*Q_dealer-mu*L_warehouse) / (sigma*math.sqrt(L_warehouse)) ) )

    delta = 1/Q_dealer * (a + b + c)
    return delta

def f_func_normal_demand(Q_dealer: int, L_warehouse: float, mu: float, sigma: float, threshold = 1e-6) -> np.ndarray:
    cumulative_prob = 0
    u = 0
    probability_array = []

    while cumulative_prob < 1 - threshold:
        probability_array    

    pass

def warehouse_demand_variance_term_normal_demand():
    pass

def delta_func_comp_poisson_demand():
    pass

def g_func_comp_poisson_demand():
    pass

def warehouse_demand_variance_term_comp_poisson_demand():
    pass

def warehouse_demand_approximation(mu_L:float, sigma2_L:float):
    pass

def main():
    for n in range(15):
        print(f"For n = {n}, delta = {delta_func_normal_demand(Q_dealer = 2,L_warehouse = 5,mu = 1,sigma = 2, n=n)}")

if __name__ == "__main__":
    main()