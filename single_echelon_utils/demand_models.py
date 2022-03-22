import numpy as np

def demand_probability_array_empiric_compound_poisson(L: int, E_z: float, V_z: float) -> np.array:
    #TO-DO, not prio 1.
    pass

def demand_probability_array_poisson():
    #TO-DO
    pass

def demand_probability_array_normal():
    #TO-DO
    pass

def demand_prob_arr_negative_binomial(L: int, E_z: float, V_z: float, threshold = 1e-4) -> np.array:
    """Computes the array of demand probabilities under negative binomial dist 
    (logarithmic compound poisson).
    
    This function actually uses the fact that logarithmic compound poisson is the 
    negative binomial distribution.

    reference: Axsäter 2006, Inventory control, p. 81, eq 5.15-5.17

    Params:
        L: Lead time
        E_z: Mean demand during one time unit.
        V_z: Variance of demand during one time unit.
        threshold: Demand with probability less than this is deemed to be zero.
    
    """

    # First find params r and p.
    p = 1 - (E_z/V_z)
    r = E_z*(1-p)/p
  
    demand_prob_arr = []

    # First do k = 0
    demand_prob_arr.append((1-p)**r)
    
    # Then do for k = 1,2,...
    p = 1
    k = 1
    while p > threshold: #Potential bug: What if prob of low demands is super low?
        P_D_k = 0
        temp = 0
        while temp < k:
            P_D_k *= (r + temp)
            temp += 1

        P_D_k = (P_D_k / np.math.factorial(k))*((1-p)**r)*(p**k)

        p = P_D_k
        demand_prob_arr.append(P_D_k)
        k += 1

    return np.array(demand_prob_arr)


def main():
    print(demand_prob_arr_negative_binomial(5,1,20))

if __name__ == "__main__":
    main()