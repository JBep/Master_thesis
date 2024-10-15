import time
from demand_models import *

def demand_probability_arr_Empiric_Compound_Poisson_test(L: int, E_z: float, V_z: float, 
    compounding_dist_arr: np.ndarray, customer_threshold = THRESHOLD, lead_time_demand_method = "M1", lead_time_variance = None) -> np.ndarray:
    """Compound poisson distribution with empiric compounding.

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

    
    f_k_j = np.zeros((j_max,k_max+1)) #Making it one bigger to do rekursion, will remove this later.
    f_k_j_bools = np.zeros((j_max,k_max+1))
    
    #Generating matrix of probabilities of k customers (columns) buying a total of j units (rows)
    #Index j = 0 is 1 unit. index k = 0 is 1 customer. case of demand = 0 handled separately.
    # # Insert case of 1 customer.
    f_k_j[0:len(compounding_dist_arr),0] = compounding_dist_arr
    f_k_j_bools[0:len(compounding_dist_arr),0] = 1


    def rekursion(j,k,f_k_j: np.ndarray,f_k_j_bools: np.ndarray):
        #print(f"j: {j}, k: {k}")
        #base case1
        if j < k or j >= j_max_per_customer*(k+1):
            f_k_j[j,k] = 0

        #base case2
        if(k == 0):
            return
        
        f_k_j_temp = 0
        for i in range(k,j+1):
            if not f_k_j_bools[i-1,k-1]:
                fa = rekursion(i-1,k-1,f_k_j,f_k_j_bools)
            
            if not f_k_j_bools[j-i,0]:
                fb = rekursion(j-i,0,f_k_j,f_k_j_bools)
            
            fa = f_k_j[i-1,k-1]
            fb = f_k_j[j-i,0]
            f_k_j_temp += fa*fb
        
        f_k_j[j,k] = f_k_j_temp
        f_k_j_bools[j,k] = 1
    
    rekursion(j_max-1,k_max,f_k_j,f_k_j_bools)
    f_k_j = f_k_j[:,:-1] # Removing extra column.

    demand_prob_arr = np.array(f_k_j).dot(customer_prob_arr[1:]) #Convoluting probability of customer with the probability of different order sizes.
    demand_prob_arr = np.concatenate((customer_prob_arr[0:1],demand_prob_arr)) # Adding probability of 0 demand (=0 customers)

    df = pd.DataFrame(f_k_j)
    df.to_excel("test_excel_file2.xlsx")
    #assert np.sum(demand_prob_arr) > 1-customer_threshold-1e-4 and np.sum(demand_prob_arr) <= 1, f"Demand array is not close enough to 1, it sums to: {np.sum(demand_prob_arr)}"

    return demand_prob_arr

if __name__ == "__main__":
    L_test = 10
    E_z_test = 3.54
    V_z_test = 30.3
    compounding_dist_arr_test = [0.119075145,0.707514451,0.004624277,0.052023121,
    0.002312139,0.013872832,0.003468208,0.008092486,0.002312139,0.043930636,
    0.001156069,0.011560694,0,0.001156069,0.001156069,0.004624277,0,0.002312139,
    0.001156069,0.010404624,0,0,0,0.005780347,0,0.001156069,0,0,0,0.002312139]
    start1 = time.time()
    cust_demand_arr_real = demand_probability_arr_Empiric_Compound_Poisson(L_test,
        E_z_test,V_z_test,compounding_dist_arr_test)
    end1 = time.time()

    start2 = time.time()
    cust_demand_arr_test = demand_probability_arr_Empiric_Compound_Poisson_test(L_test,
        E_z_test,V_z_test,compounding_dist_arr_test)
    end2 = time.time()

    print(f"Real array took: {end1-start1}s \nTest array took: {end2-start2}s")