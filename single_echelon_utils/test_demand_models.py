from demand_models import *
import numpy as np
import random
from timeit import default_timer as timer
# Testing negative binomial

def test_NBD():
    L,E_z,V_z,threshold = 5,10,20,1e-4
    prob_array_1 = demand_prob_arr_negative_binomial(L,E_z,V_z,threshold)

    mu = E_z*L
    sigma2 = V_z*L
    p = 1 - (mu/sigma2)
    r = mu*(1-p)/p

    # Test return-type
    assert type(prob_array_1) is np.ndarray, f"Return-type should be np.array, it is: {type(prob_array_1)}"

    # Test that no probabilities is above threshold
    assert any(prob_array_1 <= threshold), "There's some probabilities less than threshold value."   

    # Test that probabilities is close to 1.
    assert np.sum(prob_array_1) >= 1-threshold and np.sum(prob_array_1) <= 1, f"The probabilities doesn't add to one, they add to: {np.sum(prob_array_1)}."
    print(f"The cumulative probability is: {np.sum(prob_array_1)}")

    # Simulation test
    sim_size = 100000
    simulation_run = np.random.negative_binomial(n = r, p = p, size = sim_size)
    simulation_count = [0]*len(prob_array_1)
    simulation_prob = [0]*len(prob_array_1)
    for i in range(len(prob_array_1)):
        print(f"simulating demand value: {i}")
        for value in simulation_run:
            if value == i:
                simulation_count[i] += 1
            simulation_prob[i] = simulation_count[i]/sim_size

    diff = simulation_prob - prob_array_1

    diff_threshold = 1e-3
    assert any(abs(diff) > diff_threshold), "Differences is larger than: {diff_threshold} for some value."
    #print(diff)        

# Testing poisson.

def test_Poisson():
    L,E_z,threshold = 5,10,1e-4
    prob_array_1 = demand_prob_arr_poisson(L,E_z,threshold)
    

    mu = E_z*L
    
    # Test return-type
    print("Testing type.")
    assert type(prob_array_1) is np.ndarray, f"Return-type should be np.array, it is: {type(prob_array_1)}"


    # Test that no probabilities is above threshold
    print("Testing threshold.")
    assert any(prob_array_1 <= threshold), "There's some probabilities less than threshold value."   

    # Test that probabilities is close to 1.
    print("Testing cumulative probability.")
    assert np.sum(prob_array_1) >= 1-threshold and np.sum(prob_array_1) <= 1, f"The probabilities doesn't add to one, they add to: {np.sum(prob_array_1)}."
    print(f"The cumulative probability is: {np.sum(prob_array_1)}")

    # Simulation test
    sim_size = 10000
    simulation_run = np.random.poisson(lam = mu, size = sim_size)
    simulation_count = [0]*len(prob_array_1)
    simulation_prob = [0]*len(prob_array_1)
    for i in range(len(prob_array_1)):
        print(f"simulating demand value: {i}")
        for value in simulation_run:
            if value == i:
                simulation_count[i] += 1
            simulation_prob[i] = simulation_count[i]/sim_size

    diff = simulation_prob - prob_array_1

    diff_threshold = 1e-3
    assert any(abs(diff) > diff_threshold), "Differences is larger than: {diff_threshold} for some value."
    #print(diff)      


def test_compound_poisson(lam = 0.4, comp_dist_arr = [0.5,0.5], L = 3, threshold = 1e-4):
    # Computing E_z and V_z according to Axsäter eq 5.4 and 5.6
    j_arr = np.arange(start=1,stop=len(comp_dist_arr)+1)   
    E_z = lam*j_arr.dot(comp_dist_arr)

    j2_arr = np.power(j_arr,2*np.ones(shape = len(j_arr))) 
    V_z = lam*j2_arr.dot(comp_dist_arr)

    prob_array = demand_probability_array_empiric_compound_poisson(L,E_z,V_z,comp_dist_arr,customer_threshold = threshold)
    
    print(f"Probability array is: {prob_array}")
    # Test return-type
    print("Testing type.")
    assert type(prob_array) is np.ndarray, f"Return-type should be np.array, it is: {type(prob_array)}"

    # Test that probabilities is close to 1.
    print("Testing cumulative probability.")
    assert np.sum(prob_array) >= 1-threshold and np.sum(prob_array) <= 1, f"The probabilities doesn't add to one, they add to: {np.sum(prob_array)}."
    print(f"The cumulative probability is: {np.sum(prob_array)}")

    # Simulation test
    print("Doing simulation test.")
    # First, create two arrays to handle order sizes and their probabilities.
    possible_order_sizes_arr = []
    order_size_cumulative_probabilities_arr = []

    cumulative_prob = 0
    for i,p in enumerate(comp_dist_arr):
        if p > 0:
            cumulative_prob += p
            possible_order_sizes_arr.append(i+1)
            order_size_cumulative_probabilities_arr.append(cumulative_prob)


    # Generate no of customers for sim_size lead times (time units).
    sim_size = 100000
    simulation_run_customers = np.random.poisson(lam = lam*L, size = sim_size)
    simulation_run = np.zeros_like(simulation_run_customers)
    
    for index,k in enumerate(simulation_run_customers):
        for _ in range(k):
            #print(k)
            rand_number = np.random.uniform()
            #print(rand_number)
            for i,cum_prob in enumerate(order_size_cumulative_probabilities_arr):
                if rand_number <= cum_prob:
                    demand_size = possible_order_sizes_arr[i]
                    break
            simulation_run[index] += demand_size

    simulation_count = [0]*len(prob_array)
    simulation_prob = [0]*len(prob_array)
    for i in range(len(prob_array)):
        print(f"simulating demand value: {i}")
        for value in simulation_run:
            if value == i:
                simulation_count[i] += 1
            simulation_prob[i] = simulation_count[i]/sim_size

    diff = simulation_prob - prob_array

    diff_threshold = 1e-2
    assert all(abs(diff) < diff_threshold), f"Differences is larger than: {diff_threshold} for some value. \n {diff}"
    print(f"""Simulation test done, \n probabilities analytical: {prob_array} 
    probabilities simulated {simulation_prob} \n count simulated {simulation_count}, 
    Difference: {diff}""")  
    print("All tests done!")

def time_test_compound_poisson(lam = 0.4, comp_dist_arr = [0.5,0.5], L = 3, threshold = 1e-4, n = 100):
    # Computing E_z and V_z according to Axsäter eq 5.4 and 5.6
    j_arr = np.arange(start=1,stop=len(comp_dist_arr)+1)   
    E_z = lam*j_arr.dot(comp_dist_arr)

    j2_arr = np.power(j_arr,2*np.ones(shape = len(j_arr))) 
    V_z = lam*j2_arr.dot(comp_dist_arr)
    start = timer()
    for i in range(n):
        p_arr = demand_probability_array_empiric_compound_poisson(L,E_z,V_z,
        comp_dist_arr,customer_threshold = threshold)
    end = timer()

    return (end-start)/n

def test_time_comp_poisson_sparse_vs_dense(lam = 0.4, comp_dist_arr = [0.5,0.5], L = 3, threshold = 1e-4, n = 100):
    for i in range(1,101,10):
        comp_dist_arr = np.random.uniform(size = i)
        comp_dist_arr = comp_dist_arr/np.sum(comp_dist_arr)
        print(f"Order size:{i} Dense: {time_test_compound_poisson(lam,comp_dist_arr,L,threshold,n)} seconds.")

        comp_dist_arr = np.zeros_like(comp_dist_arr)
        for k in range(0,len(comp_dist_arr),10):
            comp_dist_arr[k] = 5*np.random.uniform()
        comp_dist_arr = comp_dist_arr/np.sum(comp_dist_arr)
        print(f"Order size:{i} Sparse: {time_test_compound_poisson(lam,comp_dist_arr,L,threshold,n)} seconds.")

def main():
    lam = 0.4 
    comp_dist_arr = np.random.uniform(size = 100)
    comp_dist_arr = comp_dist_arr/np.sum(comp_dist_arr)
    print(f"Sum is: {np.sum(comp_dist_arr)}")
    L = 3 
    threshold = 1e-4
    test_compound_poisson(lam,comp_dist_arr,L,threshold)

        

if __name__ == "__main__":
    main()