from demand_models import *
from service_level_computation import *
import pandas as pd

def fill_rate_test_NBD(test_cases: pd.DataFrame, test_case_no: int):
    demand_arr = demand_prob_arr_negative_binomial(L = test_cases.at[test_case_no-1,"L"],E_z = test_cases.at[test_case_no-1,"demand_mean"] ,V_z = test_cases.at[test_case_no-1,'demand_variance'])
    demand_size_prob_arr = demand_size_arr_logarithmic(test_cases.at[test_case_no-1,"demand_mean"],test_cases.at[test_case_no-1,'demand_variance']) # demand mean and sigma2 can be of any time unit.

    IL_prob_arr = IL_prob_array_discrete_positive(test_cases.at[test_case_no-1,"R"],test_cases.at[test_case_no-1,"Q"],demand_arr)
    fill_rate = fill_rate_compound_poisson_demand(demand_size_prob_arr,IL_prob_arr)
    
    #Computing E_IL = IL
    exp_stock_on_hand = 0
    for i,p_IL in enumerate(IL_prob_arr):
        exp_stock_on_hand += i*p_IL

    test_cases.at[test_case_no-1,"fill_rate"] = fill_rate
    test_cases.at[test_case_no-1,"exp_stock_on_hand"] = exp_stock_on_hand

def create_test_cases(demand_mean,demand_variance):
    """Generates a dataframe of 50 test cases.
    
    params:
        demand_mean: mean demand during one time unit.
        demand_variance: variance of demand during one time unit.
        
    """

    df = pd.DataFrame(columns=["test_case","R","Q","L","demand_mean","demand_variance","lambda","alpha","MTBA"])
    case = 0
    for R in [0,5,10,15,20]:
        for Q in [1,5,10,20]:
            for L in [2,4,8]:
                if (Q == 10 or Q == 20) and L == 4:
                    continue
                lam, alpha = logarithmic_compound_params(demand_mean,demand_variance)
                MTBA = 1/lam
                df.at[case] = [case+1,R,Q,L,demand_mean,demand_variance,lam,alpha,MTBA]

                case += 1

    return df


def main():
    mu_prime = 1
    sigma2_prime = 5 
    my_test_cases = create_test_cases(mu_prime,sigma2_prime)

    print(my_test_cases)
    zeros = [0]*len(my_test_cases["test_case"])
    my_test_cases["fill_rate"] = zeros
    my_test_cases["exp_stock_on_hand"] = zeros

    for test_case in my_test_cases["test_case"].view():
        fill_rate_test_NBD(my_test_cases,test_case)

    print(my_test_cases)






if __name__ == "__main__":
    main()