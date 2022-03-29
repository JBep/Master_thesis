import numpy as np
from warehouse_demand_modeling import warehouse_subbatch_demand_probability_array

def positive_inventory(Q: int, Q_0: int, R_0: int, f_u: np.array):
    
    pos_inv = 0

    for k, i in enumerate(range(R_0+1, R_0+1)): #index och sen värde
        for c in range(0, i+1):
            pos_inv = pos_inv + (i-c)*f_u(c)

    return (Q/Q_0)*pos_inv

    
def negative_inventory(Q: int, Q_0: int, R_0: int, f_u: np.array):
    neg_inv = 0

    for k, i in enumerate(range(R_0+1, R_0+1)): #index och sen värde
        for c in range(i, len(f_u)+1):
            neg_inv = neg_inv + (c-i)*f_u(c)

    return (Q/Q_0)*neg_inv

def total_cost(E_IL_pos: float, E_IL_neg: float, )

def warehouse_optimzation():
    for c in range():
        
