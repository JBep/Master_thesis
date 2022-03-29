import numpy as np

from warehouse_demand_modeling import *



def test_warehouse_mean_approximation():
    print("Testing warehouse_mean_approximation.")
    dealer_mu_arr = np.array([2,2,2,2,2,5,5,5,5,5])
    L_warehouse = 3
    Q_subbatch = 1

    subbatch_demand_mu = warehouse_demand_mean_approximation(dealer_mu_arr,L_warehouse,Q_subbatch)

    print(f"Mean is: {subbatch_demand_mu}")
    assert subbatch_demand_mu == 105, f"mean demand should be 105, it is {subbatch_demand_mu}"

def test_warehouse_variance_approximation():
    print("Testing warehouse_variance_approximation.")
    Q_dealer_arr = [10,8,6,4,12,10,2,4]
    dealer_mu_arr = np.array([2,2,2,2,1,1,1,1])
    dealer_sigma_arr = np.array([1,2,4,0.2,4,3,2,3])
    demand_type_arr = np.array(["Normal","Normal","Normal","Normal","NBD","NBD","NBD","NBD"])
    L_warehouse = 3
    Q_subbatch = 2

    variance = warehouse_demand_variance_approximation(Q_dealer_arr,dealer_mu_arr,dealer_sigma_arr,demand_type_arr,L_warehouse,Q_subbatch)
    print(f"Variance is: {variance}")

def test_warehouse_subbatch_demand_probability_array():
    print("Testing test_warehouse_subbatch_demand_probability_array.")
    Q_dealer_arr = [10,8,6,4,12,10,2,4]
    dealer_mu_arr = np.array([2,2,2,2,1,1,1,1])
    dealer_sigma_arr = np.array([1,2,4,0.2,4,3,2,3])
    demand_type_arr = np.array(["Normal","Normal","Normal","Normal","NBD","NBD","NBD","NBD"])
    L_warehouse = 3
    Q_subbatch = 2

    prob_arr = warehouse_subbatch_demand_probability_array(Q_dealer_arr,dealer_mu_arr,dealer_sigma_arr,demand_type_arr,L_warehouse,Q_subbatch)
    print(f"Demand prob is: {prob_arr}")

    cumulative_prob = np.sum(prob_arr)
    assert cumulative_prob > 1-1e-4 and cumulative_prob <= 1, f"Cumulative probability is: {cumulative_prob}."

def main():
    print("Starting testing session!")
    test_warehouse_mean_approximation()
    print("\n------------------------------ \n")
    test_warehouse_variance_approximation()

    print("\n------------------------------ \n")
    test_warehouse_subbatch_demand_probability_array()
    
    print("All tests finished!")


if __name__ == "__main__":
    main()