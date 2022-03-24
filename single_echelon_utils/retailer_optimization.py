from service_level_computation import *
from inventory_level_computation import *
from demand_models import *


def retailer_R_optimization_poisson(Q: int, L_estimate: float, fill_rate_target: float, mean_demand: float, demand_var: float) -> tuple[int,int,float,float]:
    """Function finds minimum R that fulfils target service level.
    
    Params:
        Q: Given order quantity.
        L_estimate: Lead time estimate
        fill_rate_target
        E_z: Mean demand during a time unit.
        
    Returns:
        R, fill_rate, E_IL
        """
    demand_arr = demand_prob_arr_poisson(L = L_estimate, E_z = mean_demand)

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

def retailer_R_optimization_NBD(Q: int, L_estimate: float, fill_rate_target: float, mean_demand: float, demand_var: float) -> tuple[int,int,float,float]:
    """Function finds minimum R that fulfils target service level.
    
    Params:
        Q: Given order quantity.
        L_estimate: Lead time estimate
        fill_rate_target
        E_z: Mean demand during a time unit.
        V_z: Variance of demand during a time unit.
        
    Returns:
        R, Q, fill_rate, E_IL
        """
    demand_arr = demand_prob_arr_negative_binomial(L = L_estimate,E_z = mean_demand ,V_z = demand_var)
    demand_size_prob_arr = demand_size_arr_logarithmic()

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

def retailer_R_optimization_Normal(Q: int, L_estimate: float, fill_rate_target: float, mean_demand: float, demand_var: float) -> tuple[int,int,float,float]:
    """Function finds minimum R that fulfils target service level.
    
    Params:
        Q: Given order quantity.
        L_estimate: Lead time estimate
        fill_rate_target
        E_z: Mean demand during a time unit.
        V_z: Variance of demand during a time unit.
        
    Returns:
        R, Q, fill_rate, E_IL
        """

    lt_demand_mean = lead_time_demand_mean(mean_demand,L_estimate)
    lt_demand_var = lead_time_demand_variance_M1(demand_var,L_estimate)

    R = -Q
    fill_rate = 0
    while fill_rate < fill_rate_target:
        R += 1
        fill_rate = fill_rate_normal_demand(R,Q,lt_demand_mean,np.sqrt(lt_demand_var))
    
    #Computing E_IL = IL
    # Not implemented, (this formula is the fucker).
    exp_stock_on_hand = 1000000

    return (R, Q, fill_rate, exp_stock_on_hand)
def main():
    R,Q,fill_rate,E_IL = retailer_R_optimization_poisson(Q=10,L_estimate = 5,fill_rate_target = 0.95, E_z = 5)
    print(f"R = {R}, Q = {Q}, IFR = {fill_rate}, E_IL = {E_IL}")

if __name__ == "__main__":
    main()