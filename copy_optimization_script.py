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

excel_path_indata = "/Volumes/GoogleDrive/.shortcut-targets-by-id/10oYqI9u7nCLK0q7xF2CvGGIQVokusjaI/Exjobb/9. Analytical modeling/test_indata.xlsx"
indata_sheet = "test_case_1"
indataDF = pd.read_excel(excel_path_indata,indata_sheet)
outdataDF = indataDF.copy()
indataDF

Q_dealer_arr = indataDF.get(indataDF["Type"] == "Dealer").get("Q").to_numpy()
mu_dealer_arr = indataDF.get(indataDF["Type"] == "Dealer").get("Demand mean").to_numpy()
sigma_dealer_arr = indataDF.get(indataDF["Type"] == "Dealer").get("Demand stdev").to_numpy()
demand_type_arr = indataDF.get(indataDF["Type"] == "Dealer").get("Demand type").to_numpy()
Q_subbatch_size = find_smallest_divisor(Q_dealer_arr)
L_wh = float(indataDF.get(indataDF["Type"]=="RDC").get("Transport time"))

rdc_f_u_probability_array, wh_dist, mu_L, sigma2_L = warehouse_subbatch_demand_probability_array(Q_dealer_arr, mu_dealer_arr, 
    sigma_dealer_arr, demand_type_arr, L_wh, Q_subbatch_size)

outdataDF.loc[outdataDF["Type"] == "Dealer","Q, subbatches"] = Q_dealer_arr/Q_subbatch_size

outdataDF.loc[outdataDF["Type"] == "RDC","Demand type"] = wh_dist
outdataDF.loc[outdataDF["Type"] == "RDC","Lead time demand mean"] = mu_L * Q_subbatch_size
outdataDF.loc[outdataDF["Type"] == "RDC","Lead time demand stdev"] = math.sqrt(sigma2_L) * Q_subbatch_size
outdataDF.loc[outdataDF["Type"] == "RDC","Demand mean"] = mu_L * Q_subbatch_size/L_wh
outdataDF.loc[outdataDF["Type"] == "RDC","Demand stdev"] = math.sqrt(sigma2_L) * Q_subbatch_size/L_wh
outdataDF.loc[outdataDF["Type"] == "RDC", "Demand variance"] = sigma2_L * Q_subbatch_size/L_wh

outdataDF

h_dealer_arr = indataDF.get(indataDF["Type"] == "Dealer").get("Holding cost").to_numpy()
fill_rate_target_arr = indataDF.get(indataDF["Type"] == "Dealer").get("Target item fill rate").to_numpy()
p_dealer_arr = fill_rate_target_arr*h_dealer_arr/(np.ones_like(fill_rate_target_arr)-fill_rate_target_arr)
l_dealer_arr = indataDF.get(indataDF["Type"] == "Dealer").get("Transport time").to_numpy()
mu_wh = mu_L/L_wh * Q_subbatch_size

beta_list = []
for h,Q,p,l,my,sigma in zip(h_dealer_arr,Q_dealer_arr,p_dealer_arr,l_dealer_arr,mu_dealer_arr,sigma_dealer_arr):
    beta_list.append(induced_backorder_cost_opt(h,Q,p,l,my,sigma))

beta_arr = np.array(beta_list)

beta_rdc = weighting_backorder_cost(mu_dealer_arr,mu_wh,beta_arr)
outdataDF.loc[outdataDF["Type"] == "Dealer", "Shortage cost"] = p_dealer_arr
outdataDF.loc[outdataDF["Type"] == "RDC", "Beta"] = beta_rdc
outdataDF.loc[outdataDF["Type"] == "Dealer", "Beta"] = beta_arr
print(f"Optimal weighted induced backorder cost at the warehouse is: {beta_rdc}, betas are: {beta_arr}")

h_rdc = float(indataDF.get(indataDF["Type"] == "RDC").get("Holding cost"))
Q_0 = int(int(indataDF.get(indataDF["Type"] == "RDC").get("Q"))/Q_subbatch_size)

R_0 = warehouse_optimization(Q_subbatch_size,Q_0,rdc_f_u_probability_array,h_rdc,beta_rdc)

outdataDF.loc[outdataDF["Type"] == "RDC", "Q, subbatches"] = Q_0
outdataDF.loc[outdataDF["Type"] == "RDC", "R, subbatches"] = R_0
outdataDF

W = waiting_time(negative_inventory(Q_subbatch_size,Q_0,R_0,rdc_f_u_probability_array),L_wh,mu_L,Q_subbatch_size)
outdataDF.loc[outdataDF["Type"]== "Dealer", "Waiting time"] = W
outdataDF.loc[outdataDF["Type"] == "Dealer", "Lead time"] = outdataDF.get(outdataDF["Type"]== "Dealer").get("Transport time").to_numpy() + W
outdataDF.loc[outdataDF["Type"] == "Dealer", "Lead time demand mean"] = outdataDF.get(outdataDF["Type"]== "Dealer").get("Lead time").to_numpy()*outdataDF.get(outdataDF["Type"]== "Dealer").get("Demand mean").to_numpy()
outdataDF.loc[outdataDF["Type"] == "Dealer", "Lead time demand stdev"] =outdataDF.get(outdataDF["Type"]== "Dealer").get("Lead time").to_numpy()*outdataDF.get(outdataDF["Type"]== "Dealer").get("Demand stdev").to_numpy()
outdataDF.loc[outdataDF["Type"] == "RDC", "Lead time"] = indataDF.get(indataDF["Type"] == "RDC").get("Transport time")

opt_dealer_list = []
L_dealer_arr = outdataDF.get(outdataDF["Type"] == "Dealer").get("Lead time")
for Q,L_est,fill_rate_target,demand_type,mu,sigma in zip(Q_dealer_arr,L_dealer_arr,fill_rate_target_arr,demand_type_arr, mu_dealer_arr,sigma_dealer_arr):
    print(demand_type, mu, sigma)
    opt_dealer_list.append(dealer_R_optimization(Q,L_est,fill_rate_target,demand_type,mu,demand_variance = math.pow(sigma,2)))

R_opt_dealer_list,fill_rate_dealer_list,exp_stock_on_hand_list = [],[],[]
for tup in opt_dealer_list:
    R_opt_dealer_list.append(tup[0])
    fill_rate_dealer_list.append(tup[2])
    exp_stock_on_hand_list.append(tup[3])

outdataDF.loc[outdataDF["Type"] == "Dealer", "R optimal"] = R_opt_dealer_list
outdataDF.loc[outdataDF["Type"] == "Dealer", "Realized item fill rate"] = fill_rate_dealer_list
outdataDF.loc[outdataDF["Type"] == "Dealer", "Expected stock on hand"] = exp_stock_on_hand_list

outdataDF.loc[outdataDF["Type"] == "RDC","R optimal"] = R_0*Q_subbatch_size # In units.
outdataDF.loc[outdataDF["Type"] == "RDC","Expected stock on hand"] = positive_inventory(Q_subbatch_size,Q_0,R_0,rdc_f_u_probability_array)
outdataDF.loc[outdataDF["Type"] == "RDC","Expected backorders"] = negative_inventory(Q_subbatch_size,Q_0,R_0,rdc_f_u_probability_array)

# List comprehension for exp backorders.
outdataDF.loc[outdataDF["Type"] == "Dealer", "Expected backorders"] = [ 
    expected_backorders_discrete(R,Q,lt_mu,exp_stock_on_hand) for 
    R,Q,lt_mu,exp_stock_on_hand in zip(R_opt_dealer_list,Q_dealer_arr,
    outdataDF.get(outdataDF["Type"] == "Dealer").get("Lead time demand mean").to_numpy(),
    exp_stock_on_hand_list) ]

    # Adding costs
outdataDF.loc[outdataDF["Type"] == "Dealer", "Expected holding costs per time unit"] = h_dealer_arr*np.array(exp_stock_on_hand_list)
outdataDF.loc[outdataDF["Type"] == "Dealer", "Expected shortage costs per time unit"] = outdataDF.get(
    outdataDF["Type"] == "Dealer").get("Expected backorders").to_numpy()*p_dealer_arr
outdataDF.loc[outdataDF["Type"] == "Dealer", "Total expected costs"] = outdataDF.get(outdataDF["Type"] == "Dealer").get("Expected holding costs per time unit").to_numpy() + outdataDF.get(outdataDF["Type"] == "Dealer").get("Expected shortage costs per time unit").to_numpy()

outdataDF.loc[outdataDF["Type"] == "RDC", "Expected holding costs per time unit"] = h_rdc*outdataDF.get(outdataDF["Type"] == "RDC").get("Expected stock on hand").to_numpy()
outdataDF.loc[outdataDF["Type"] == "RDC", "Total expected costs"] = outdataDF.get(outdataDF["Type"] == "RDC").get("Expected holding costs per time unit").to_numpy()

excel_path_outdata = "/Volumes/GoogleDrive/.shortcut-targets-by-id/10oYqI9u7nCLK0q7xF2CvGGIQVokusjaI/Exjobb/9. Analytical modeling/test_outdata.xlsx"
outdataDF.to_excel(excel_path_outdata,sheet_name = "Outdata_latest_testrun")

outdataDF