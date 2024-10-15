

def waiting_time(exp_backorders: float, L_warehouse: float, 
    mean_lead_time_subbatch_demand_warehouse: float, Q_subbatch: int):
    """Calculates the expected waiting time due to delays at the warehouse.
    
    Reference: For equation see (64)
    
    params:
        exp_backorders: Expected amount of backorders at the warehouse.
        L_warehouse: Warehouse lead time.
        mean_lead_time_subbatch_demand_warhouse: Mean lead time demand at the warehouse.
        Q_subbatch: Size of the subbatch.
    
    return: waiting time in time units

    """

    return L_warehouse * exp_backorders / (mean_lead_time_subbatch_demand_warehouse*Q_subbatch)
