import numpy as np

from warehouse_demand_modeling import warehouse_demand_mean_approximation


def test_warehouse_mean_estimate():
    dealer_mu_arr = np.ndarray([2,2,2,2,2,5,5,5,5,5])
    L_warehouse = 3
    Q_subbatch = 1

    subbatch_demand_mu = warehouse_demand_mean_approximation(dealer_mu_arr,L_warehouse,Q_subbatch)

    assert subbatch_demand_mu == 105, f"mean demand should be 105, it is {subbatch_demand_mu}"



def main():
    print("Starting testing session!")
    test_warehouse_mean_estimate
    print("All tests finished!")


if __name__ == "__main__":
    main()