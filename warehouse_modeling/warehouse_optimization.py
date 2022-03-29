import numpy as np
from warehouse_demand_modeling import warehouse_subbatch_demand_probability_array

def positive_inventory(Q: int, Q_0: int, R_0: int, f_u: np.array):
    
    pos_inv = 0

    for k, i in enumerate(range(R_0+1, Q_0)): #index och sen värde
        for c in range(0, i+1):
            pos_inv = pos_inv + (i-c)*f_u[c]
            print(pos_inv)

    return (Q/Q_0)*pos_inv


def negative_inventory(Q: int, Q_0: int, R_0: int, f_u: np.array):
    neg_inv = 0

    for k, i in enumerate(range(R_0+1, Q_0)): #index och sen värde
        for c in range(i, len(f_u)):
            neg_inv = neg_inv + (c-i)*f_u[c]
            print(neg_inv)

    return (Q/Q_0)*neg_inv

def total_cost(Q: int, Q_0: int, R_0: int, f_u: np.array, h: float, b: float) -> float:
    total_cost  = h * positive_inventory(Q, Q_0, R_0, f_u) + b * negative_inventory(Q, Q_0, R_0, f_u)
    return total_cost

def warehouse_optimization(Q: int, Q_0: int, f_u: np.array, h: float, b: float):

    R_0 = 0
    total_cost_first = total_cost(Q, Q_0, R_0, f_u, h, b)
    total_cost_second = total_cost(Q, Q_0, R_0+1, f_u, h, b)

    if total_cost_first < total_cost_second:
        while total_cost_first < total_cost_second:
            R_0 = R_0 - 1
            total_cost_first = total_cost(Q, Q_0, R_0, f_u, h, b)
            total_cost_second = total_cost(Q, Q_0, R_0+1, f_u, h, b)
        return R_0+1
    else: 
        while total_cost_first > total_cost_second:
            R_0 = R_0 + 1
            total_cost_first = total_cost(Q, Q_0, R_0, f_u, h, b)
            total_cost_second = total_cost(Q, Q_0, R_0 + 1, f_u, h, b)
        return R_0
