import numpy as np
from service_level_computation import *

#Testing service level for normal demand manually

def test_fill_rate_normal(R, Q, mean_normal, std_dev_normal):
    fill_rate = fill_rate_normal_demand(R, Q, mean_normal, std_dev_normal)
    print(fill_rate)

#Testing service level for adjusted normal demand manually

def test_fill_rate_adj_normal(R, Q, mean_normal, std_dev_normal, demand_prob_array) -> float:
    normal = undershoot_adjustment_normal(R, Q, mean_normal, std_dev_normal, demand_prob_array)
    print(normal)

# Testing service level compound poisson demand
def test_fill_rate_compound_poisson():
    pass

# Testing service level poisson demand
def test_fill_rate_poisson():
    pass

# Testing service level compound poisson demand
def test_ready_rate_discrete():
    pass

def main():
    test_fill_rate_adj_normal(14, 6, 7.84, 5.3, #insert array)
    test_fill_rate_normal(14, 6, 7.84, 5.3)

if __name__ == "__main__":
    main()