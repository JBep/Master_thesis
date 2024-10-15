from src.reorder_point_optimization_bm import reorder_point_optimization 
from src.reorder_point_optimization_naive import reorder_point_optimization_single_echelon

def main():
    df_bm = reorder_point_optimization(
        indata_path = "test_data/test_case_indata.csv",
        indata_demand_size_dist_path= "test_data/test_case_indata_demand_sizes.csv",
        outdata_path="test_data/test_case_outdata_bm.csv"
    )

    df_naive = reorder_point_optimization_single_echelon(
        indata_path = "test_data/test_case_indata.csv",
        indata_demand_size_dist_path= "test_data/test_case_indata_demand_sizes.csv",
        outdata_path="test_data/test_case_outdata_naive.csv"

    )
    
    return df_bm, df_naive
