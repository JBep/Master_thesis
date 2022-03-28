import scipy.stats

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
    a = 

    delta = a + b + c + d
    pass

def f_func_normal_demand():
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