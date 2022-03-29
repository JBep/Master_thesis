import numpy as np

from warehouse_optimization import *
from warehouse_demand_modeling import *

def test_warehouse_optimization():
    #Create demand prob array
    Q_dealer_arr = [10,8,6,4,12,10,2,4]
    dealer_mu_arr = np.array([2,2,2,2,1,1,1,1])
    dealer_sigma_arr = np.array([1,2,4,0.2,4,3,2,3])
    demand_type_arr = np.array(["Normal","Normal","Normal","Normal","NBD","NBD","NBD","NBD"])
    L_warehouse = 3
    Q_subbatch = 2

    prob_arr = warehouse_subbatch_demand_probability_array(Q_dealer_arr,dealer_mu_arr,dealer_sigma_arr,demand_type_arr,L_warehouse,Q_subbatch)
   
    R_opt = warehouse_optimization(100, 3, prob_arr, 2, 100)
    print(f"Optimal R: {R_opt}")

def test_negative_inventory():
     #Create demand prob array
    Q_dealer_arr = [10,8,6,4,12,10,2,4]
    dealer_mu_arr = np.array([2,2,2,2,1,1,1,1])
    dealer_sigma_arr = np.array([1,2,4,0.2,4,3,2,3])
    demand_type_arr = np.array(["Normal","Normal","Normal","Normal","NBD","NBD","NBD","NBD"])
    L_warehouse = 3
    Q_subbatch = 2

    prob_arr = warehouse_subbatch_demand_probability_array(Q_dealer_arr,dealer_mu_arr,dealer_sigma_arr,demand_type_arr,L_warehouse,Q_subbatch)
    print(f"Demand prob is: {prob_arr}")

    print(f" negative inventory: {negative_inventory(2, 4, 1, prob_arr)}")

def test_positive_inventory():
     #Create demand prob array
    Q_dealer_arr = [10,8,6,4,12,10,2,4]
    dealer_mu_arr = np.array([2,2,2,2,1,1,1,1])
    dealer_sigma_arr = np.array([1,2,4,0.2,4,3,2,3])
    demand_type_arr = np.array(["Normal","Normal","Normal","Normal","NBD","NBD","NBD","NBD"])
    L_warehouse = 3
    Q_subbatch = 2

    prob_arr = warehouse_subbatch_demand_probability_array(Q_dealer_arr,dealer_mu_arr,dealer_sigma_arr,demand_type_arr,L_warehouse,Q_subbatch)
    #print(f"Demand prob is: {prob_arr}")

    print(f" positive inventory: {positive_inventory(1000, 4, 2, prob_arr)}")

def test_total_cost():
    #Create demand prob array
    Q_dealer_arr = [10,8,6,4,12,10,2,4]
    dealer_mu_arr = np.array([2,2,2,2,1,1,1,1])
    dealer_sigma_arr = np.array([1,2,4,0.2,4,3,2,3])
    demand_type_arr = np.array(["Normal","Normal","Normal","Normal","NBD","NBD","NBD","NBD"])
    L_warehouse = 3
    Q_subbatch = 2

    prob_arr = warehouse_subbatch_demand_probability_array(Q_dealer_arr,dealer_mu_arr,dealer_sigma_arr,demand_type_arr,L_warehouse,Q_subbatch)
    print(total_cost(2, 4, 1, prob_arr, 5, 3))

def main():
    test_warehouse_optimization()
    #test_negative_inventory()
    #test_positive_inventory()
    #test_total_cost()

if __name__ == "__main__":
    main()
