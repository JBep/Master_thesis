from induced_backorder_cost import *

# Testing for optimal induced back order costs to compare with tabulated numbers in BM 2006
def test_induced_backorder_cost(h, Q, p, l, my, sigma):
    induced_backorder_opt = induced_backorder_cost_opt(h, Q, p, l, my, sigma)
    print(f"induced backorder: {induced_backorder_opt}")


#Testing k factor 
def test_k_factor(h, Q, p, my, l):
    k = k_factor(h, Q, p, my, l)
    print(f"k-factor: {k}")

#Testing g factor
def test_g_factor(h, Q, p, my, l):
    g = g_factor(h, Q, p, my, l)
    print(f"g factor: {g}")

#Testing weighting optimal induced backorder costs for non-identical retailers
def test_weighting_backorder_cost():
    retailer_demands_arr = [1,2,3,4]
    retailer_backorder_costs_array = []
    retailer_backorder_costs_array.append(induced_backorder_cost_opt(1, 1, 1, 1, 100, 1))
    retailer_backorder_costs_array.append(induced_backorder_cost_opt(1, 1, 1, 1, 100, 1))
    retailer_backorder_costs_array.append(induced_backorder_cost_opt(1, 1, 1, 1, 100, 1))
    retailer_backorder_costs_array.append(induced_backorder_cost_opt(1, 1, 1, 1, 100, 1))
    warehouse_demand = 5
    weighted_backorder_opt = weighting_backorder_cost(retailer_demands_arr, warehouse_demand, retailer_backorder_costs_array)
    print(weighted_backorder_opt)


def main():
    #test_k_factor(1, 1, 1, 100, 1) (Seems like formula is approximation when compared to tabulated numbers in BM 2006)
    #test_g_factor(1, 1, 1, 100, 1) (Seems like formula is approximation when compared to tabulated numbers in BM 2006)
    #test_induced_backorder_cost(1, 100, 10, 1, 100, 10)
    test_weighting_backorder_cost()


if __name__ == "__main__":
    main()

