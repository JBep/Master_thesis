from email.errors import InvalidMultipartContentTransferEncodingDefect
import numpy as np
import os, sys

currentdir = os.path.dirname(os.path.realpath(__file__))
sys.path.append(currentdir)
from my_log import *

if __name__ == "__main__":
    from warehouse_demand_modeling import warehouse_subbatch_demand_probability_array

@log("default_log")
def positive_inventory(Q: int, Q_0: int, R_0: int, f_u_arr: np.ndarray):
    """Calculates the expected positive inventory-level at the RDC.
    
    reference BM 2014 eq. 10
    
    params:
        Q: Subbatch size.
        Q_0: Order quantity in units of subbatches.
        R_0: Reorder point at the RDC.
        f_u_arr: Subbatch demand probability at the RDC.
        
    returns:
        Expected positive inventory level (stock-on-hand)."""
    
    pos_inv = 0

    for y in range(R_0+1, R_0+Q_0+1): #The iteration finishes after R_0+Q_0
        for u in range(y+1):
            # It tries to get the values from the array, if it's "out" then probability is 0.
            try:
                p = f_u_arr[u]
            except IndexError:
                p = 0

            pos_inv += (y-u)*p

    return (Q/Q_0)*pos_inv

@log("default_log")
def negative_inventory(Q: int, Q_0: int, R_0: int, f_u_arr: np.ndarray, method = 1):
    """Calculates the expected negative inventory-level at the RDC.

    reference: BM 2014 eq. 11
    
    params:
        Q: Subbatch size.
        Q_0: Order quantity in units of subbatches.
        R_0: Reorder point at the RDC.
        f_u_arr: Subbatch demand probability at the RDC.
        
    returns:
        Expected negative inventory level (backorders)."""

    neg_inv = 0

    for y in range(R_0+1, R_0+Q_0+1): #index och sen värde
        u = max(y,0)
        len_f_u_arr = len(f_u_arr)
        while u < len_f_u_arr:
            neg_inv += (u-y)*f_u_arr[u]
            u += 1

    return (Q/Q_0)*neg_inv

@log("default_log")
def total_cost(Q: int, Q_0: int, R_0: int, f_u: np.array, h: float, b: float) -> float:
    total_cost  = h * positive_inventory(Q, Q_0, R_0, f_u) + b * negative_inventory(Q, Q_0, R_0, f_u)
    return total_cost

@log("default_log")
@log("sparse_log")
def warehouse_optimization(Q: int, Q_0: int, f_u: np.array, h: float, b: float):
    """Calcutes the optimal reorder point for the RDC/warehouse.
    
    params:
        Q: Subbatch size
        Q_0: RDC/Warehouse order quantity in units of Q.
        f_u: Warehouse subbatch demand probability array.
        h: Warehouse/RDC holding cost.
        b: Warehouse/RDC shortage cost (estimated by induced backorder cost).
        
    returns:
        R_0: Optimal reorder point at the RDC.
    """
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
