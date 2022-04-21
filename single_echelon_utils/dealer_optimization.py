import os, sys
currentdir = os.path.dirname(os.path.realpath(__file__))
sys.path.append(currentdir)

from scipy.stats import norm

from service_level_computation import *
from inventory_level_computation import *
from demand_models import *
from my_log import *

@log("default_log")
@log("sparse_log")
def dealer_R_optimization(Q: int, L_est: float, fill_rate_target: float, demand_type: str, 
    demand_mean: float, demand_variance: float = -1, compounding_dist_arr: np.ndarray = -1*np.ones(10), 
    name: str = "No name passed.") -> tuple[int,float,float]:
    """Function for optimizing R at dealers.

    Note! If demand_type = "Poisson", the demand variance, by definition, equals 
    demand mean and the "demand_variance" parameter is not used.
    
    params:
        Q: int, given order quantity.
        L_est: float, Lead time estimate in time units.
        fill_rate_target: float.
        
        demand_type: "Poisson","NBD","Normal","Normal_Adjusted", "Compound_Poisson_empiric".
        demand_mean: float, mean demand during a time unit.
        demand_variance: float, variance of demand during a time unit. Not
            required if demand_type is "Poisson".

        compounding_dist_arr: Array of demand size probabilities used for 
            demand_type = "Compound_Poisson_empiric" and "Normal_Adjusted".
    
    returns:
        R_opt, realized_fill_rate, expected_stock_on_hand.
    """
    func_name = f"dealer_R_optimization_{demand_type}"
    
    return globals()[func_name](Q = Q, L_est = L_est, fill_rate_target = fill_rate_target, 
        demand_mean = demand_mean, demand_variance = demand_variance, compounding_dist_arr = compounding_dist_arr,
        name = name)




def dealer_R_optimization_Poisson(**kwargs) -> tuple[int,float,float]:
    """Function finds minimum R that fulfils target service level.
    
    Params:
        Q: Given order quantity.
        L_est: Lead time estimate
        fill_rate_target
        E_z: Mean demand during a time unit.
        
    Returns:
        R, realised_fill_rate, E_IL
        """
    demand_arr = demand_prob_arr_poisson(L = kwargs["L_est"], E_z = kwargs["demand_mean"])

    R = -kwargs["Q"]
    fill_rate = 0
    while fill_rate < kwargs["fill_rate_target"]:
        R += 1
        IL_prob_arr = IL_prob_array_discrete_positive(R,kwargs["Q"],demand_arr)
        fill_rate = fill_rate_poisson_demand(IL_prob_arr)
    
    #Computing E_IL = IL
    exp_stock_on_hand = 0
    for i,p_IL in enumerate(IL_prob_arr):
        exp_stock_on_hand += i*p_IL

    return (R, fill_rate, exp_stock_on_hand)

def dealer_R_optimization_NBD(**kwargs) -> tuple[int,float,float]:
    """Function finds minimum R that fulfils target service level.
    
    Params:
        Q: Given order quantity.
        L_est: Lead time estimate
        fill_rate_target
        E_z: Mean demand during a time unit.
        V_z: Variance of demand during a time unit.
        
    Returns:
        R, Q, fill_rate, E_IL
        """

    assert kwargs["demand_variance"] != -1, "Variance is -1 (default value), has the variance been entered correctly?"
    demand_arr = demand_prob_arr_negative_binomial(L = kwargs["L_est"],E_z = kwargs["demand_mean"] ,V_z = kwargs["demand_variance"])
    demand_size_prob_arr = demand_size_arr_logarithmic(E_z = kwargs["demand_mean"],V_z = kwargs["demand_variance"]) # demand mean and sigma2 can be of any time unit.

    R = -kwargs["Q"]
    fill_rate = 0
    while fill_rate < kwargs["fill_rate_target"]:
        R += 1
        IL_prob_arr = IL_prob_array_discrete_positive(R,kwargs["Q"],demand_arr)
        fill_rate = fill_rate_compound_poisson_demand(demand_size_prob_arr,IL_prob_arr)
    
    #Computing E_IL = IL
    exp_stock_on_hand = 0
    for i,p_IL in enumerate(IL_prob_arr):
        exp_stock_on_hand += i*p_IL

    return (R, fill_rate, exp_stock_on_hand)

def dealer_R_optimization_Normal(**kwargs) -> tuple[int,float,float]:
    """Function finds minimum R that fulfils target service level.
    
    Params:
        Q: Given order quantity.
        L_est: Lead time estimate
        fill_rate_target
        E_z: Mean demand during a time unit.
        V_z: Variance of demand during a time unit.
        
    Returns:
        R, Q, fill_rate, E_IL
        """

    assert kwargs["demand_variance"] != -1, "Variance is -1 (default value), has the variance been entered correctly?"
    lt_demand_mean = lead_time_demand_mean(E_z = kwargs["demand_mean"],L = kwargs["L_est"])
    lt_demand_variance = lead_time_demand_variance_M1(V_z = kwargs['demand_variance'],L = kwargs["L_est"])

    prob_neg_demand = norm.cdf(0,loc = lt_demand_mean, scale = math.sqrt(lt_demand_variance))
    if prob_neg_demand > 0.0001:
        print(f"""Dealer {kwargs["name"]} model has a large probability for negative demand, 
        F(0) = {prob_neg_demand}, consider using another demand model than the Normal approximation.""")

    R = -kwargs["Q"]
    fill_rate = 0
    while fill_rate < kwargs["fill_rate_target"]:
        R += 1
        fill_rate = fill_rate_normal_demand(R,kwargs["Q"],lt_demand_mean,np.sqrt(lt_demand_variance))
    
    # Computing E_IL = IL
    # Not implemented, (this formula is the fucker).
    exp_stock_on_hand = 0

    return (R, fill_rate, exp_stock_on_hand)

def dealer_R_optimization_Normal_adjusted(**kwargs):
    pass

@log("default_log")
def dealer_R_optimization_Empiric_Compound_Poisson(**kwargs):
    """Function finds minimum R that fulfils target service level for empiric compound poisson.
    
    Params:
        Q: Given order quantity.
        L_est: Lead time estimate
        fill_rate_target
        demand_mean: Mean demand during a time unit.
        demand_variance: Variance of demand during a time unit.
        compounding_dist_arr: Array of probabilities of order sizes.
        
    Returns:
        R, Q, fill_rate, E_IL
        """

    assert kwargs["demand_variance"] != -1, "Variance is -1 (default value), has the variance been entered correctly?"
    demand_arr = demand_probability_arr_Empiric_Compound_Poisson(L = kwargs["L_est"],
        E_z = kwargs["demand_mean"], V_z = kwargs["demand_variance"],
        compounding_dist_arr = kwargs["compounding_dist_arr"])
    
    demand_size_prob_arr = kwargs["compounding_dist_arr"]
    assert demand_size_prob_arr[0] != -1

    R = -1
    IL_prob_arr = IL_prob_array_discrete_positive(R,kwargs["Q"],demand_arr)
    fill_rate = fill_rate_compound_poisson_demand(demand_size_prob_arr,IL_prob_arr)
    while fill_rate < kwargs["fill_rate_target"]:
        R += 1
        IL_prob_arr = IL_prob_array_discrete_positive(R,kwargs["Q"],demand_arr)
        fill_rate = fill_rate_compound_poisson_demand(demand_size_prob_arr,IL_prob_arr)
    
    #Computing E_IL = IL
    exp_stock_on_hand = 0
    for i,p_IL in enumerate(IL_prob_arr):
        exp_stock_on_hand += i*p_IL


    return (R, fill_rate, exp_stock_on_hand)


def main():
    R,fill_rate,E_IL = dealer_R_optimization(Q=10,L_est = 5,fill_rate_target = 0.95, 
        demand_type = 'Poisson', demand_mean = 5)
    print(f"Poisson values: R = {R}, IFR = {fill_rate}, E_IL = {E_IL}")

    R,fill_rate,E_IL = dealer_R_optimization(Q=5,L_est = 4,fill_rate_target = 0.25, 
        demand_type = 'NBD', demand_mean = 5, demand_variance = 10)
    print(f"NBD values: R = {R}, IFR = {fill_rate}, E_IL = {E_IL}")

    R,fill_rate,E_IL = dealer_R_optimization(Q=10,L_est = 5,fill_rate_target = 0.95, 
        demand_type = 'Normal', demand_mean = 5, demand_variance = 10)
    print(f"Normal values: R = {R}, IFR = {fill_rate}, E_IL = {E_IL}")



if __name__ == "__main__":
    main()