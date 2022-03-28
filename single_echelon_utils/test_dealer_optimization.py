


from demand_models import logarithmic_compound_params


def main():
    L = 8
    E_z = 5
    V_z = 10

    lam, alpha = logarithmic_compound_params(L*E_z,L*V_z)
    MTBA = 1/lam*L
    print(lam,alpha,MTBA)


if __name__ == "__main__":
    main()