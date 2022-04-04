import os, sys
currentdir = os.path.dirname(os.path.realpath(__file__))
sys.path.append(currentdir)

from service_level_computation import *
from inventory_level_computation import *
from demand_models import *

def dealer_R_optimization(Q, L_est, fill_rate_target, demand_type, demand_mean, **kwargs) -> tuple[int,float,float,float]:
    """Function for optimizing R at dealers.
    
    params:
        Q: int, given order quantity.
        L_est: float, Lead time estimate in time units.
        fill_rate_target: float.
        
        demand_type: "Poisson","NBD","Normal","Normal_adjusted", "Compound_Poisson_empiric".
        demand_mean: float, mean demand during a time unit.
        demand_variance: float, variance of demand during a time unit. Not
            required if demand_type is "Poisson".
        demand_size_probabilities: Array of demand size probabilities used for 
            demand_type = "Compound_Poisson_empiric".
    
    returns:
        R, Q, realised_fill_rate, expected_stock_on_hand.
    """
    func_name = f"dealer_R_optimization_{demand_type}"
    
    return globals()[func_name](Q, L_est, fill_rate_target, demand_mean, **kwargs)




def dealer_R_optimization_Poisson(Q: int, L_est: float, fill_rate_target: float, demand_mean: float, **kwargs) -> tuple[int,int,float,float]:
    """Function finds minimum R that fulfils target service level.
    
    Params:
        Q: Given order quantity.
        L_est: Lead time estimate
        fill_rate_target
        E_z: Mean demand during a time unit.
        
    Returns:
        R, realised_fill_rate, E_IL
        """
    demand_arr = demand_prob_arr_poisson(L = L_est, E_z = demand_mean)

    R = -Q
    fill_rate = 0
    while fill_rate < fill_rate_target:
        R += 1
        IL_prob_arr = IL_prob_array_discrete_positive(R,Q,demand_arr)
        fill_rate = fill_rate_poisson_demand(IL_prob_arr)
    
    #Computing E_IL = IL
    exp_stock_on_hand = 0
    for i,p_IL in enumerate(IL_prob_arr):
        exp_stock_on_hand += i*p_IL

    return (R, Q, fill_rate, exp_stock_on_hand)

def dealer_R_optimization_NBD(Q: int, L_est: float, fill_rate_target: float, demand_mean: float, **kwargs) -> tuple[int,int,float,float]:
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
    demand_arr = demand_prob_arr_negative_binomial(L = L_est,E_z = demand_mean ,V_z = kwargs['demand_variance'])
    demand_size_prob_arr = demand_size_arr_logarithmic(demand_mean,kwargs['demand_variance']) # demand mean and sigma2 can be of any time unit.

    R = -Q
    fill_rate = 0
    while fill_rate < fill_rate_target:
        R += 1
        IL_prob_arr = IL_prob_array_discrete_positive(R,Q,demand_arr)
        fill_rate = fill_rate_compound_poisson_demand(demand_size_prob_arr,IL_prob_arr)
    
    #Computing E_IL = IL
    exp_stock_on_hand = 0
    for i,p_IL in enumerate(IL_prob_arr):
        exp_stock_on_hand += i*p_IL

    return (R, Q, fill_rate, exp_stock_on_hand)

def dealer_R_optimization_Normal(Q: int, L_est: float, fill_rate_target: float, demand_mean: float, **kwargs) -> tuple[int,int,float,float]:
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

    lt_demand_mean = lead_time_demand_mean(demand_mean,L_est)
    lt_demand_variance = lead_time_demand_variance_M1(kwargs['demand_variance'],L_est)

    R = -Q
    fill_rate = 0
    while fill_rate < fill_rate_target:
        R += 1
        fill_rate = fill_rate_normal_demand(R,Q,lt_demand_mean,np.sqrt(lt_demand_variance))
    
    # Computing E_IL = IL
    # Not implemented, (this formula is the fucker).
    exp_stock_on_hand = 0

    return (R, Q, fill_rate, exp_stock_on_hand)

def dealer_R_optimization_Normal_adjusted(Q, L_est, fill_rate_target, demand_mean, **kwargs):
    pass

def dealer_R_optimization_Compound_Poisson_empiric(Q, L_est, fill_rate_target, demand_mean, **kwargs):
    pass


def main():
    R,Q,fill_rate,E_IL = dealer_R_optimization(Q=10,L_est = 5,fill_rate_target = 0.95, 
        demand_type = 'Poisson', demand_mean = 5)
    print(f"Poisson values: R = {R}, Q = {Q}, IFR = {fill_rate}, E_IL = {E_IL}")

    R,Q,fill_rate,E_IL = dealer_R_optimization(Q=5,L_est = 4,fill_rate_target = 0.25, 
        demand_type = 'NBD', demand_mean = 5, demand_variance = 10)
    print(f"NBD values: R = {R}, Q = {Q}, IFR = {fill_rate}, E_IL = {E_IL}")

    R,Q,fill_rate,E_IL = dealer_R_optimization(Q=10,L_est = 5,fill_rate_target = 0.95, 
        demand_type = 'Normal', demand_mean = 5, demand_variance = 10)
    print(f"Normal values: R = {R}, Q = {Q}, IFR = {fill_rate}, E_IL = {E_IL}")



if __name__ == "__main__":
    main()