import pandas as pd
import numpy as np

from warehouse_modeling.induced_backorder_cost import *
from warehouse_modeling.lead_time_approximation import *
from warehouse_modeling.warehouse_optimization import *
from warehouse_modeling.warehouse_demand_modeling import *

from single_echelon_utils.inventory_level_computation import *
from single_echelon_utils.service_level_computation import *
from single_echelon_utils.dealer_optimization import *

from utils import *

@log("default_log")
def reorder_point_optimization(indata_path: str, indata_sheet: str = None, indata_demand_size_dist_path: str = None, 
    outdata_path: str = None, print_outdata: bool = False):
    """Optimizing reorder points for a multi-echelon system according to the BM-model.

    params:
    =======
        indata_excel_path: Path to indata csv or excel-file.
        indata_sheet: Indata sheet assumed to contain columns: 
            "Installation id", "Type", "Name", "Transport time", "Q", 
            "Unit cost", "Target item fill rate", "Demand distribution",	
            "Demand mean per time unit", "Demand stdev per time unit"
        indata_demand_size_dist_path: Path or Sheetname to excel (using same path
            as indata) or csv-file containing empiric demand size distributions.
            Column A is order sizes, Column B an onwards is retailer ids/names.
        outdata_excel_path: Path to outdata excel-file. Beware that the program 
            overwrites this file if it already exists! If None this step is skipped.
        print_outdata: bool to decide whether or not to print results to the terminal.
    """
    ## Handling Inputs
    # --------------------------------------------------------------------------       
    # Ensuring indata path is not the same as outdata (outdata will erase current file.)
    if indata_path == outdata_path:
        raise ValueError('Indata path and outdata path should not be the same.')

    if indata_path[-4:] == "xlsx":
        indataDF = pd.read_excel(indata_path,indata_sheet)
    else:
        indataDF = pd.read_csv(indata_path)
    outdataDF = indataDF.copy()

    # Ensure correct columns are present:
    required_columns = {"Installation id", "Type", "Name", "Transport time", "Q", 
        "Unit cost", "Target item fill rate", "Demand distribution",	"Demand mean per time unit", "Demand stdev per time unit"}
    
    if not required_columns.issubset(set(indataDF.columns.to_list())):
        raise ValueError("Indata doesn't contain all required fields, see documentation.")

    # Initiating capital cost value 
    capital_cost = 0.15/365

    # Retrieving the data about dealers.
    Q_dealer_arr = indataDF.get(indataDF["Type"] == "Dealer").get("Q").to_numpy().astype("int32")
    mu_dealer_arr = indataDF.get(indataDF["Type"] == "Dealer").get("Demand mean per time unit").to_numpy().astype("float64")
    demand_type_arr = indataDF.get(indataDF["Type"] == "Dealer").get("Demand distribution").to_numpy()
    h_dealer_arr = capital_cost * indataDF.get(indataDF["Type"] == "Dealer").get("Unit cost").to_numpy().astype("float64")
    fill_rate_target_arr = indataDF.get(indataDF["Type"] == "Dealer").get("Target item fill rate").to_numpy().astype("float64")
    l_dealer_arr = indataDF.get(indataDF["Type"] == "Dealer").get("Transport time").to_numpy().astype("float64")

    # If demand distribution is poisson, retrieve 
    sigma_dealer_list = []
    for id in indataDF.get(indataDF["Type"] == "Dealer").get("Installation id"):
        if str(indataDF.get(indataDF["Installation id"] == id).get("Demand distribution")) == "Poisson":
            sigma_dealer_list.append(math.sqrt(
                float(indataDF.get(outdataDF["Installation id"]== id).get("Demand mean per time unit"))))
        else:
            sigma_dealer_list.append(float(indataDF.get(indataDF["Installation id"] == id).get("Demand stdev per time unit")))        
    sigma_dealer_arr = np.array(sigma_dealer_list)

    #Find the smallest common divisor of Q.
    Q_subbatch_size = find_smallest_divisor(Q_dealer_arr)
    
    # Input compounding distribution arrays here!
    # Supposed to have one row per dealer.
    if indata_demand_size_dist_path[-3:] == "csv":
        compounding_dist_df = pd.read_csv(indata_demand_size_dist_path)
    else:
        compounding_dist_df = pd.read_excel(indata_path,indata_demand_size_dist_path)

    compounding_dist_matrix = compounding_dist_df.to_numpy().T[1:] # Each array is a column in excel, transposing and removing first row holding item amounts.
    compounding_dist_matrix = np.nan_to_num(compounding_dist_matrix,copy = True)
    


    #Read warehouse values.
    L_wh = float(indataDF.get(indataDF["Type"]=="RDC").get("Transport time"))
    h_rdc = capital_cost*float(indataDF.get(indataDF["Type"] == "RDC").get("Unit cost"))*capital_cost
    Q_0 = int(int(indataDF.get(indataDF["Type"] == "RDC").get("Q"))/Q_subbatch_size) # Observe, Q_0 is in subbatches.

    # Central warehouse demand
    # --------------------------------------------------------------------------
    # Computing subbatch demand probability array, distribution, lead time demand mean, and 
    # lead time demand variance at central warehouse.
    # Observe that these values are returned in "subbatches".
    rdc_f_u_probability_array, wh_dist, mu_L, sigma2_L = warehouse_subbatch_demand_probability_array(
        Q_dealer_arr, mu_dealer_arr, sigma_dealer_arr, demand_type_arr, L_wh, Q_subbatch_size, 
        compounding_dist_matrix)

    outdataDF.loc[outdataDF["Type"] == "RDC","Demand distribution"] = wh_dist
    outdataDF.loc[outdataDF["Type"] == "RDC","Lead time demand mean"] = mu_L * Q_subbatch_size
    outdataDF.loc[outdataDF["Type"] == "RDC","Lead time demand stdev"] = math.sqrt(sigma2_L) * Q_subbatch_size
    outdataDF.loc[outdataDF["Type"] == "RDC","Demand mean per time unit"] = mu_L * Q_subbatch_size/L_wh
    outdataDF.loc[outdataDF["Type"] == "RDC","Demand stdev per time unit"] = math.sqrt(sigma2_L * Q_subbatch_size/L_wh)
    
    # Calculating induced backorder cost.
    # --------------------------------------------------------------------------
    # Computing shortage costs.
    p_dealer_arr = fill_rate_target_arr*h_dealer_arr/(np.ones_like(fill_rate_target_arr)-fill_rate_target_arr)
    
    # Computing induced backorder cost for each retailer.
    beta_list = []
    for h,Q,p,l,my,sigma in zip(h_dealer_arr,Q_dealer_arr,p_dealer_arr,l_dealer_arr,
    mu_dealer_arr,sigma_dealer_arr):
        beta_list.append(induced_backorder_cost_opt(h,Q,p,l,my,sigma))
    beta_arr = np.array(beta_list)

    # Computing weighted backorder cost at central warehouse.
    mu_wh = mu_L/L_wh * Q_subbatch_size
    beta_rdc = weighting_backorder_cost(mu_dealer_arr,mu_wh,beta_arr)
    
    outdataDF.loc[outdataDF["Type"] == "RDC", "Holding cost"] = h_rdc
    outdataDF.loc[outdataDF["Type"] == "Dealer", "Holding cost"] = h_dealer_arr
    outdataDF.loc[outdataDF["Type"] == "Dealer", "Estimated shortage cost"] = p_dealer_arr
    outdataDF.loc[outdataDF["Type"] == "RDC", "Beta"] = beta_rdc
    outdataDF.loc[outdataDF["Type"] == "Dealer", "Beta"] = beta_arr

    # Optimizing reorder point at central warehouse
    # --------------------------------------------------------------------------
    # Computing optimal reorder points as well as corresponding expected stock on hand 
    # and backorders.
    R_0 = warehouse_optimization(Q_subbatch_size,Q_0,rdc_f_u_probability_array,h_rdc,beta_rdc)
    
    outdataDF.loc[outdataDF["Type"] == "RDC", "R optimal"] = R_0*Q_subbatch_size
    stock_on_hand_wh = positive_inventory(Q_subbatch_size,Q_0,R_0,rdc_f_u_probability_array)
    outdataDF.loc[outdataDF["Type"] == "RDC","Expected stock on hand"] = stock_on_hand_wh
    
    backorders_wh = negative_inventory(Q_subbatch_size,Q_0,R_0,rdc_f_u_probability_array)
    outdataDF.loc[outdataDF["Type"] == "RDC","Expected backorders"] = backorders_wh

    # Computing expected delay and lead time
    # --------------------------------------------------------------------------
    W = waiting_time(negative_inventory(Q_subbatch_size,Q_0,R_0,rdc_f_u_probability_array),L_wh,mu_L,Q_subbatch_size)
    outdataDF.loc[outdataDF["Type"]== "Dealer", "Expected delay"] = W
    lead_time_dealer_arr = outdataDF.get(outdataDF["Type"]== "Dealer").get("Transport time").to_numpy() + W
    outdataDF.loc[outdataDF["Type"] == "Dealer", "Lead time"] = lead_time_dealer_arr

    # Entering dealer lead time demand and standard deviation.
    mu_L_dealer_array = outdataDF.get(outdataDF["Type"] == "Dealer").get("Lead time").to_numpy()*outdataDF.get(
            outdataDF["Type"] == "Dealer").get("Demand mean per time unit").to_numpy()
    outdataDF.loc[outdataDF["Type"] == "Dealer", "Lead time demand mean"] = mu_L_dealer_array
    
    sqrt_dealer_lead_time_arr = np.sqrt(lead_time_dealer_arr)
    outdataDF.loc[outdataDF["Type"] == "Dealer", "Lead time demand stdev"] = sqrt_dealer_lead_time_arr*outdataDF.get(outdataDF["Type"]== "Dealer").get("Demand stdev per time unit").to_numpy()
    
    # Entering central warehouse lead time as transport time for completeness.
    outdataDF.loc[outdataDF["Type"] == "RDC", "Lead time"] = indataDF.get(indataDF["Type"] == "RDC").get("Transport time")

    # Computing MTBA (mean time between arrivals) for use in simulation.
    # reference: Axsäter, 2006, Inventory control, eq. 5.4
    MTBA_arr = np.zeros_like(mu_L_dealer_array)
    for i,mu in enumerate(mu_L_dealer_array):
        compounding_dist_arr = compounding_dist_matrix[i]
        j_arr = np.arange(start=1,stop=len(compounding_dist_arr)+1)
        lam = mu/j_arr.dot(compounding_dist_arr)
        MTBA_arr[i] = 1/lam*lead_time_dealer_arr[i]
    outdataDF.loc[outdataDF["Type"]== "Dealer", "MTBA"] = MTBA_arr

    # Optimizing reorder points at dealer.
    # --------------------------------------------------------------------------
    # Computing optimal reorder point, expected realised fill rate, and expected 
    # stock on hand level.
    opt_dealer_list = []
    for Q,L_est,fill_rate_target,demand_type,mu,sigma,compounding_dist_arr in zip(Q_dealer_arr,
        lead_time_dealer_arr,fill_rate_target_arr,demand_type_arr, mu_dealer_arr,sigma_dealer_arr, compounding_dist_matrix):
        opt_dealer_list.append(dealer_R_optimization(Q,L_est,fill_rate_target,demand_type,
            mu,demand_variance = math.pow(sigma,2),compounding_dist_arr=compounding_dist_arr))

    R_opt_dealer_list,fill_rate_dealer_list,exp_stock_on_hand_list = [],[],[]
    for tup in opt_dealer_list:
        R_opt_dealer_list.append(tup[0])
        fill_rate_dealer_list.append(tup[1])
        exp_stock_on_hand_list.append(tup[2])
    R_opt_dealer_arr = np.array(R_opt_dealer_list)
    fill_rate_dealer_arr = np.array(fill_rate_dealer_list)
    exp_stock_on_hand_arr = np.array(exp_stock_on_hand_list)
    

    outdataDF.loc[outdataDF["Type"] == "Dealer", "R optimal"] = R_opt_dealer_arr
    outdataDF.loc[outdataDF["Type"] == "Dealer", "Realized item fill rate"] = fill_rate_dealer_arr
    outdataDF.loc[outdataDF["Type"] == "Dealer", "Expected stock on hand"] = exp_stock_on_hand_arr

    # Adding expected backorders at retailers.
    exp_backorders_dealer_arr = [ 
        expected_backorders_discrete(R,Q,lt_mu,exp_stock_on_hand) for 
        R,Q,lt_mu,exp_stock_on_hand in zip(R_opt_dealer_list,Q_dealer_arr,
        outdataDF.get(outdataDF["Type"] == "Dealer").get("Lead time demand mean").to_numpy(),
        exp_stock_on_hand_list) ]
    outdataDF.loc[outdataDF["Type"] == "Dealer", "Expected backorders"] = exp_backorders_dealer_arr

    # Computing cost expressions
    # --------------------------------------------------------------------------
    total_holding_cost_dealers_arr = h_dealer_arr*exp_stock_on_hand_arr
    total_backorder_cost_dealers_arr = exp_backorders_dealer_arr*p_dealer_arr
    outdataDF.loc[outdataDF["Type"] == "Dealer", "Expected holding costs per time unit"] = total_holding_cost_dealers_arr
    outdataDF.loc[outdataDF["Type"] == "Dealer", "Expected shortage costs per time unit"] = total_backorder_cost_dealers_arr
    outdataDF.loc[outdataDF["Type"] == "Dealer", "Total expected costs"] = total_holding_cost_dealers_arr + total_backorder_cost_dealers_arr
    
    outdataDF.loc[outdataDF["Type"] == "RDC", "Expected holding costs per time unit"] = h_rdc*stock_on_hand_wh
    outdataDF.loc[outdataDF["Type"] == "RDC", "Total expected costs"] = h_rdc*stock_on_hand_wh

    # Printing results.
    # --------------------------------------------------------------------------
    
    if outdata_path is not None:
        if outdata_path[-4:] == "xlsx":
            outdataDF.to_excel(outdata_path,sheet_name = "Outdata_latest_run")
        else:
            outdataDF.to_csv(outdata_path)

    if print_outdata:
        print(outdataDF)

def main():
    indata_path = "/Users/AlexanderLarsson/documents/VSCode/test_simulation_runs.xlsx"
    indata_sheet = "test_case_2"
    indata_demand_size_sheet = "test_case_1_cd"

    outdata_path = "/Users/AlexanderLarsson/documents/VSCode/outdata.xlsx"

    reorder_point_optimization(indata_path,indata_sheet,indata_demand_size_sheet,outdata_path,True)

if __name__ == "__main__":
    main()