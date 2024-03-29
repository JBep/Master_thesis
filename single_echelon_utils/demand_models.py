from decimal import DivisionByZero
import numpy as np
import pandas as pd
import math
import os, sys

currentdir = os.path.dirname(os.path.realpath(__file__))
sys.path.append(currentdir)
#from my_log import *

## Test change

THRESHOLD = 1e-4


def lead_time_demand_mean(E_z, L) -> float:
    """Calculates the lead time demand mean.
    
    Multiplies L with E_z.

    Reference: For equation see (27)
    """

    mu = L*E_z
    return mu


def lead_time_demand_variance_M1(V_z, L) -> float:
    """Calculates lead time demand variance by using assumption of constant L."""
    
    sigma2 = L*V_z
    return sigma2

def lead_time_demand_variance_M2(V_z, E_z, V_L, E_L) -> float:
    """Calculates lead time demand variance under stochastic lead times.
    
    Reference: For equation see (28)
    """

    if V_L == None:
        raise ValueError("Supply lead time variance if using method M2.")
    sigma2 = V_z*E_L + E_z**2 * V_L

    return sigma2

##@log("default_log")
def demand_probability_arr_Empiric_Compound_Poisson(L: int, E_z: float, V_z: float, 
    compounding_dist_arr: np.ndarray, customer_threshold = THRESHOLD, lead_time_demand_method = "M1", lead_time_variance = None) -> np.ndarray:
    """Compound poisson distribution with empiric compounding.

    Reference: See section 4.5.4 

    Params:
        L: Lead-time
        E_z: mean demand during a time-unit.
        V_z: demand variance during a time-unit.
        compounding_dist_arr: numpy.ndarray with probabilities that one customer 
            orders index+1 goods. Observe that index 0 regards ordering 1 item.
        customer_threshold: Computations stop when cumulative probability of
            customers reaches 1-threshold.
        lead_time_demand_method: "M1" or "M2" methods of computing lead time demand variance.
        lead_time_variance: Variance of lead time.

    Returns: 
        numpy.ndarray of demand probabilities.
    """

    # First estimate lead time demand mean and variance.
    mu = lead_time_demand_mean(E_z= E_z,L = L)
    if lead_time_demand_method == "M1":
        sigma2 = lead_time_demand_variance_M1(V_z = V_z,L = L)
    elif lead_time_demand_method == "M2":
        sigma2 = lead_time_demand_variance_M2(V_z = V_z,E_z = E_z, V_L = lead_time_variance,E_L=L)
    else:
        raise ValueError("lead_time_demand_method needs to be 'M1' or 'M2'")

    # Then scrap zeros from the end of compounding_dist_arr
    compounding_dist_arr = np.trim_zeros(compounding_dist_arr, 'b')

    # compute lambda from axsäter 5.4
    j_arr = np.arange(start=1,stop=len(compounding_dist_arr)+1)
    lam = mu/j_arr.dot(compounding_dist_arr)
    
    # Compute p(D=d) from axsäter 5.2 and 5.3
    # First compute vector of probability of k customers which is poisson distributed.
    customer_prob_arr = []
    cumulative_prob = 0
    k = 0

    while cumulative_prob < 1-customer_threshold:
        k_ln_lam = k*math.log(lam)
        ln_k_factorial = math.log(math.factorial(k))
        p_k = np.exp(k_ln_lam-ln_k_factorial-lam)
        cumulative_prob += p_k
        customer_prob_arr.append(p_k)
        k += 1

    # Convert to array
    customer_prob_arr = np.array(customer_prob_arr)

    # Create f_k_j-matrix
    j_max_per_customer = len(compounding_dist_arr)
    k_max = len(customer_prob_arr)-1 #k goes to max customers+1 as 0 customers is possible.
    j_max = k_max*j_max_per_customer

    
    f_k_j = np.zeros((j_max,k_max)) 
    
    #Generating matrix of probabilities of k customers (columns) buying a total of j units (rows)
    #Index j = 0 is 1 unit. index k = 0 is 1 customer. case of demand = 0 handled separately.
    # # Insert case of 1 customer.
    f_k_j[0:len(compounding_dist_arr),0] = compounding_dist_arr

    # From k = 2 to k_max-1 customers.
    for k in range(1,k_max):
        # k customers buying j wares.
        for j in range(k,j_max_per_customer*(k+1)):
            f_k_j_temp = 0
            for i in range(k,j+1):
                fa = f_k_j[i-1,k-1]
                fb = f_k_j[j-i,0]
                f_k_j_temp += fa*fb
             
            f_k_j[j,k]=f_k_j_temp

        #print(k, iters)
    #df = pd.DataFrame(f_k_j)
    #df.to_excel("test_excel_file1.xlsx")
    #print(iters)

    demand_prob_arr = np.array(f_k_j).dot(customer_prob_arr[1:]) #Convoluting probability of customer with the probability of different order sizes.
    demand_prob_arr = np.concatenate((customer_prob_arr[0:1],demand_prob_arr)) # Adding probability of 0 demand (=0 customers)

    assert np.sum(demand_prob_arr) > 1-customer_threshold-1e-4 and np.sum(demand_prob_arr) <= 1, f"Demand array is not close enough to 1, it sums to: {np.sum(demand_prob_arr)}"

    return demand_prob_arr


##@log("default_log")
def demand_prob_arr_poisson(L: int, E_z: float, threshold = THRESHOLD) -> np.ndarray:
    """Returns probability array for poisson demand.
    
    Reference: For equation see (8)
    
    params:
        L: Lead time.
        E_z: Mean demand during a time unit.
        threshold: Computations stop when cumulative demand reaches 1-threshold.
        """
    mu = lead_time_demand_mean(E_z,L)

    demand_prob_arr = []
    cumulative_prob = 0

    #while probability > threshold: #Potential bug: What if prob of low demands is super low?
    k = 0
    while cumulative_prob < 1-threshold:
        p_k = (math.pow(mu,k)/math.factorial(k))*np.exp(-mu)
        cumulative_prob += p_k
        demand_prob_arr.append(p_k)
        k += 1

    return np.array(demand_prob_arr)

    


    #TO-DO
    pass

##@log("default_log")
def demand_prob_arr_negative_binomial(L: int, E_z: float, V_z: float, threshold = THRESHOLD, 
    lead_time_demand_method = "M1", lead_time_variance = None) -> np.ndarray:
    """Computes the array of demand probabilities under negative binomial dist 
    (logarithmic compound poisson).
    
    This function actually uses the fact that logarithmic compound poisson is the 
    negative binomial distribution.

    Reference: see section 4.5.2 

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
    mu = lead_time_demand_mean(E_z,L )
    if lead_time_demand_method == "M1":
        sigma2 = lead_time_demand_variance_M1(V_z = V_z,L = L)
    elif lead_time_demand_method == "M2":
        sigma2 = lead_time_demand_variance_M2(V_z = V_z,E_z = E_z, V_L = lead_time_variance,E_L=L)
    else:
        raise ValueError("lead_time_demand_method needs to be 'M1' or 'M2'")

    p = logarithmic_alpha(mu,sigma2)
    r = mu*(1-p)/p
    demand_prob_arr = []

    # First do k = 0
    P_D_0 = (1-p)**r
    demand_prob_arr.append(P_D_0)
    
    # Then do for k = 1,2,...
    cumulative_prob = P_D_0 
    k = 1
    logarithm_list = np.array([math.log(i) for i in range(1,1000)]) # idx i holds value log(i+1)
   
    while cumulative_prob < 1-threshold:
        # This algorithm might seem complicated, however, as the NBD has factorial terms
        # which gets out of hand quickly, some trickery is needed as to avoid 
        # return values of inf or nan. This algorithm should hold for demand of
        # up the approximately mean 150 or so, which should be plenty for the task at hand!
        
        denominator_log = np.sum(logarithm_list[:k]) # denominator_log = log(k!)
        d = math.exp(denominator_log/k) # d = e^(log(k!)/k) = (k!)^(1/k)

        P_D_k = 1
        temp = 0
        while temp < k:
            P_D_k *= (r + temp)/d
            temp += 1

        P_D_k *= math.pow(1-p,r)*math.pow(p,k)
        
        cumulative_prob += P_D_k
        demand_prob_arr.append(P_D_k)
        k += 1

    return np.array(demand_prob_arr)

##@log("default_log")
def demand_size_arr_logarithmic(E_z: float, V_z:float, threshold = THRESHOLD) -> np.ndarray:
    """Calculates the logarithmic compounding distribution array.'

    Reference: see section 4.5.2  
    
    params:
        E_z: demand mean during a time unit (or the lead time).
        V_z: demand variance during a time unit (or the lead time).
        
    returns: 
        numpy.ndarray of probabilities, index 0 is probability for demand = 0.
    """

    alpha = logarithmic_alpha(E_z,V_z)
    f_j_arr = [0] # Probability of demand = 0 is 0.
    cumulative_prob = 0
    j = 1

    while cumulative_prob < 1-threshold:
        p = -math.pow(alpha,j)/(j*math.log(1-alpha))
        f_j_arr.append(p)
        cumulative_prob += p
        j += 1

    return np.array(f_j_arr) 

##@log("default_log")
def logarithmic_alpha(E_z: float, V_z:float) -> float:
    """Calculates the alpha-value for the logarithmic distribution.
    
    Same as p-value for the NBD-dist.

    Reference: for equation see (14)
    
    Params:
        E_z: demand mean during a time unit (or the lead time).
        V_z: demand variance during a time unit (or the lead time).
        
    returns: 
        alpha for logarithmic dist. (or p-value for NBD)
  
    """
    try:
        alpha = 1 - (E_z/V_z) 
    except DivisionByZero:
        raise DivisionByZero("Variance needs to be larger than mean in the NBD-distribution.")

    if alpha < 0:
        raise ValueError("Variance needs to be larger than mean in the NBD-distribution.")

    return alpha

##@log("default_log")
def logarithmic_compound_params(E_z: float, V_z:float) -> tuple[float,float]:
    """Calculates the alpha-value for the logarithmic distribution.
    
    Same as p-value for the NBD-dist.

    Reference: For equations see (14) and (15)
    
    Params:
        E_z: demand mean during one time unit.
        V_z: demand variance during one time unit.
        
    returns: 
        lam, alpha - For compound poisson dist with logarithmic compounding. 
    """

    alpha = logarithmic_alpha(E_z,V_z)
    
    lam = -E_z * ((1-alpha)*math.log(1-alpha))/alpha

    return lam, alpha

##@log("default_log")
def logarithmic_compound_mean_variance(lam: float, alpha: float) -> tuple[float,float]:
    """Converts lambda and alpha of compound poisson logarithmic distribution to 
    mean and variance.

    Reference: For equations see (14) and (15)
    
    params:
        lam: Lambda of compound poisson distribution (regarding one time unit).
        alpha: alpha of logarithmic compounding distribution.

    return:
        mean and variance for compound distribution with logarithmic compounding 
            during one time unit.

    """

    mu_prime = -lam * alpha / ((1-alpha)*math.log(1-alpha))
    sigma2_prime = -lam * alpha / (math.pow(1-alpha,2)*math.log(1-alpha))

    return mu_prime, sigma2_prime



def main():
    #print(demand_prob_arr_negative_binomial(5,1,20))
    #print(demand_prob_arr_poisson(L=5,E_z=10))
    compounding_dist= np.array([0,0,0.5,0,0,0,0.25,0.1,0.1,0.05])
    arr = demand_probability_array_empiric_compound_poisson(L = 5, E_z = 10, V_z = 10, compounding_dist_arr=compounding_dist)
    print(arr)
    print(f"sum is: {np.sum(arr)}")

    compounding_dist= np.array([0,0,0.5,0,0,0,0.25,0.1,0.1,0.05,0,0,0,0,0,0,0])
    arr2 = demand_probability_array_empiric_compound_poisson(L = 5, E_z = 10, V_z = 10, compounding_dist_arr=compounding_dist)
    print(arr2)
    print(f"sum is: {np.sum(arr2)}")

    for i in range(len(arr)):
        assert arr[i] == arr2[i]
    assert len(arr) == len(arr2)
    print("tests passed")



if __name__ == "__main__":
    main()