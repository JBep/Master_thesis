from demand_models import *
import numpy as np
import random

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


def test_compound_poisson(lam = 1, comp_dist_arr = [0.5,0.5], L = 5, threshold = 1e-4):
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
    sim_size = 10000
    simulation_run_customers = np.random.poisson(lam = lam, size = sim_size)
    simulation_run = np.zeors_like(simulation_run_customers)
    
    for k in simulation_run_customers:
        for customer in range(k):
            rand_number = np.random.uniform()
            demand_size_temp = 1
            p = 0
            for prob in comp_dist_arr:
                p += prob
                if prob > 0 and rand_number <= p:
                    demand_size = demand_size_temp
                demand_size_temp += 1

            simulation_run[k] += demand_size

    simulation_count = [0]*len(prob_array)
    simulation_prob = [0]*len(prob_array)
    for i in range(len(prob_array)):
        print(f"simulating demand value: {i}")
        for value in simulation_run:
            if value == i:
                simulation_count[i] += 1
            simulation_prob[i] = simulation_count[i]/sim_size

    diff = simulation_prob - prob_array

    diff_threshold = 1e-3
    assert any(abs(diff) > diff_threshold), "Differences is larger than: {diff_threshold} for some value."
    #print(diff)  


def main():
    test_compound_poisson()

if __name__ == "__main__":
    main()