from decimal import DivisionByZero
import numpy as np
import math

## Test change


def lead_time_mean(E_z, L) -> float:
    """Calculates the lead time demand mean.
    
    Multiplies L with E_z.
    """
    mu = L*E_z
    return mu

def lead_time_variance_M1(V_z, L) -> float:
    """Calculates lead time demand variance by using assumption of constant L."""
    
    sigma2 = L*V_z
    return sigma2

def lead_time_variance_M2(V_z, E_z, V_L, E_L) -> float:
    """Calculates lead time demand variance under stochastic lead times."""
    if V_L == None:
        raise ValueError("Supply lead time variance if using method M2.")
    sigma2 = V_z*E_L + E_z**2 * V_L

    return sigma2


def demand_probability_array_empiric_compound_poisson(L: int, E_z: float, V_z: float, 
    compounding_dist_arr:np.ndarray, customer_threshold = 1e-4, lead_time_demand_method = "M1", lead_time_variance = None) -> np.ndarray:
    """"Compound poisson distribution with empiric compounding.

    Params:
        L: Lead-time
        E_z: mean demand during a time-unit.
        V_z: demand variance during a time-unit.
        compounding_dist_arr: numpy.ndarray with probabilities that one customer 
            orders index+1 goods. Observe that index 0 regards orderinge 1 item.
        customer_threshold: Computations stop when cumulative probability of
            customers reaches 1-threshold.
        lead_time_demand_method: "M1" or "M2" methods of computing lead time demand variance.
        lead_time_variance: Variance of lead time.

    Returns: 
        numpy.ndarray of demand probabilities.
    """

    # First estimate lead time demand mean and variance.
    mu = lead_time_mean(E_z= E_z,L = L)
    if lead_time_demand_method == "M1":
        sigma2 = lead_time_variance_M1(V_z = V_z,L = L)
    elif lead_time_demand_method == "M2":
        sigma2 = lead_time_variance_M2(V_z = V_z,E_z = E_z, V_L = lead_time_variance,E_L=L)
    else:
        raise ValueError("lead_time_demand_method needs to be 'M1' or 'M2'")

    # compute lambda from axsäter 5.4
    j_arr = np.arange(start=1,stop=len(compounding_dist_arr)+1)
    lam = mu/j_arr.dot(compounding_dist_arr)
    print(f"j-values: {j_arr} \n compound values: {compounding_dist_arr} \n mu: {mu} \n sum: {j_arr.dot(compounding_dist_arr)} \n lambda: {lam})")

    # Compute p(D=d) from axsäter 5.2 and 5.3
    # First compute vector of probability of k customers which is poisson distributed.
    customer_prob_arr = []
    cumulative_prob = 0
    k = 0

    while cumulative_prob < 1-customer_threshold:
        p_k = (math.pow(lam,k)/math.factorial(k))*np.exp(-lam)
        #print(f"np.exp is {np.exp(-lam)}")
        print(f"Cumulative prob after {k} values is: {cumulative_prob}, p_k is: {p_k}, k is {k}")
        cumulative_prob += p_k
        customer_prob_arr.append(p_k)
        k += 1

    # Convert to array
    customer_prob_arr = np.array(customer_prob_arr)

    # Create f_k_j-matrix
    k_max = len(customer_prob_arr) #k goes to max customers+1 as 0 customers is possible.
    j_max = k_max*len(compounding_dist_arr)
    print(k_max)
    print(j_max)
    f_k_j = np.zeros((k_max,j_max))
    
    # Insert case of 1 customer.
    f_k_j[1,0:len(compounding_dist_arr)] = compounding_dist_arr

    # From k = 2 to k_max-1 customers.
    for k in range(2,k_max):
        # k customers buying j wares.
        for j in range(1,j_max):
            f_k_j_temp = 0
            for i in range(k-1,j-1):
                f_k_j_temp += f_k_j[k-1,i]*f_k_j[1,j-i]
            f_k_j[k,j]=f_k_j_temp


    demand_prob_arr = customer_prob_arr.dot(f_k_j)

    return demand_prob_arr





    pass

def demand_prob_arr_poisson(L: int, E_z: float, threshold = 1e-4) -> np.ndarray:
    """Returns probability array for poisson demand.
    Axsäter 5.1
    
    params:
        L: Lead time.
        E_z: Mean demand during a time unit.
        threshold: Computations stop when cumulative demand reaches 1-threshold.
        """
    mu = lead_time_mean(E_z,L)

    demand_prob_arr = []
    cumulative_prob = 0

    #while probability > threshold: #Potential bug: What if prob of low demands is super low?
    k = 0
    while cumulative_prob < 1-threshold:
        p_k = (math.pow(mu,k)/math.factorial(k))*np.exp(-mu)
        print(f"np.exp is {np.exp(-mu)}")
        print(f"Cumulative prob after {k} values is: {cumulative_prob}, p_k is: {p_k}, k is {k}")
        cumulative_prob += p_k
        demand_prob_arr.append(p_k)
        k += 1

    return np.array(demand_prob_arr)

    


    #TO-DO
    pass


def demand_prob_arr_negative_binomial(L: int, E_z: float, V_z: float, threshold = 1e-4, 
    lead_time_demand_method = "M1", lead_time_variance = None) -> np.ndarray:
    """Computes the array of demand probabilities under negative binomial dist 
    (logarithmic compound poisson).
    
    This function actually uses the fact that logarithmic compound poisson is the 
    negative binomial distribution.

    reference: Axsäter 2006, Inventory control, p. 81, eq 5.15-5.17

    Params:
        L: Lead time
        E_z: Mean demand during one time unit.
        V_z: Variance of demand during one time unit.
        threshold: Computations stop when cumulative demand reaches 1-threshold.
        lead_time_demand_method: "M1" or "M2" methods of computing lead time demand variance.
        lead_time_variance: Variance of lead time.

    Returns:
        np.array of probabilities of demand index. Demands of length(np.array) and 
        larger is approximated to zero.
    
    """

    # First find params r and p.
    # Lead time demand mean and variance.
    mu = lead_time_mean(E_z= E_z,L = L)
    if lead_time_demand_method == "M1":
        sigma2 = lead_time_variance_M1(V_z = V_z,L = L)
    elif lead_time_demand_method == "M2":
        sigma2 = lead_time_variance_M2(V_z = V_z,E_z = E_z, V_L = lead_time_variance,E_L=L)
    else:
        raise ValueError("lead_time_demand_method needs to be 'M1' or 'M2'")

    try:
        p = 1 - (mu/sigma2)
    
    except DivisionByZero:
        raise DivisionByZero("Variance needs to be larger than mean in the NBD-distribution.")

    if p < 0:
        raise ValueError("Variance needs to be smaller than mean in the NBD-distribution.")

    r = mu*(1-p)/p
    demand_prob_arr = []

    # First do k = 0
    P_D_0 = (1-p)**r
    demand_prob_arr.append(P_D_0)
    
    # Then do for k = 1,2,...
    cumulative_prob = P_D_0 
    k = 1
    #while probability > threshold: #Potential bug: What if prob of low demands is super low?
    while cumulative_prob < 1-threshold:
        P_D_k = 1
        temp = 0
        while temp < k:
            P_D_k *= (r + temp)
            temp += 1

        P_D_k = (P_D_k / np.math.factorial(k))*((1-p)**r)*(p**k)

        cumulative_prob += P_D_k
        demand_prob_arr.append(P_D_k)
        k += 1

    return np.array(demand_prob_arr)


def main():
    #print(demand_prob_arr_negative_binomial(5,1,20))
    #print(demand_prob_arr_poisson(L=5,E_z=10))
    compounding_dist= np.array([0,0,0.5,0,0,0,0.25,0.1,0.1,0.05])
    arr = demand_probability_array_empiric_compound_poisson(L = 5, E_z = 10, V_z = 10, compounding_dist_arr=compounding_dist)
    print(arr)
    print(f"sum is: {np.sum(arr)}")



if __name__ == "__main__":
    main()