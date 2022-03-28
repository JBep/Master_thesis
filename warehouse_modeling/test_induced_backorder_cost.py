from induced_backorder_cost import *

# Testing for optimal induced back order costs to compare with tabulated numbers in BM 2006
def test_induced_backorder_cost(h, Q, p, l, my, sigma):
    induced_backorder_opt = induced_backorder_cost_opt(h, Q, p, l, my, sigma)
    print(induced_backorder_opt)


#Testing k factor 
def test_k_factor(h, Q, p, my, l):
    k = k_factor(h, Q, p, my, l)
    print(k)

#Testing g factor
def test_g_factor(h, Q, p, my, l):
    g = g_factor(h, Q, p, my, l)
    print(g)

#Start here on Monday by testing k and g factor against table in BM 2006
def main():
    test_k_factor(1, 10, 1, 100, 1)
    test_g_factor(1, 10, 1, 100, 1)
    #test_induced_backorder_cost(1, 100, 10, 1, 100, 10)

if __name__ == "__main__":
    main()

