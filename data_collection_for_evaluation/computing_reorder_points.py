
from ast import Continue
import pandas as pd
import os
import os, sys

currentdir = os.path.dirname(os.path.realpath(__file__))
parentdir = os.path.dirname(currentdir)
sys.path.append(parentdir)
from main_program import reorder_point_optimization
from my_log import *


# Initiating excel-writer
excel_path = "/Volumes/GoogleDrive/.shortcut-targets-by-id/10oYqI9u7nCLK0q7xF2CvGGIQVokusjaI/Exjobb/7. Data collection/item_outputs_test.xlsx"
writer = pd.ExcelWriter(excel_path)

investigated_items_path = "/Volumes/GoogleDrive/.shortcut-targets-by-id/10oYqI9u7nCLK0q7xF2CvGGIQVokusjaI/Exjobb/7. Data collection/investigated_items.csv"
items = pd.read_csv(investigated_items_path)

slow_items = {"11033998","11033999","11110683","11172907","15126069",
    "11110532", "11110533","11448509"}
do_items =  {"11448509"} #{"1030-61460", "11110022", "11110023", "11110175", "11110283"}
for i,item_code in enumerate(items["item code"]):
    logger = logging.getLogger("default_log")
    logger.debug(f"Item: {item_code}")
    logger = logging.getLogger("sparse_log")
    logger.debug(f"Item: {item_code}")
    
    #if(item_code in slow_items):
    #    continue
    if(item_code not in do_items):
        continue

    print(f"Now running item no {i} with id: {item_code}")
    data_dir = "/Volumes/GoogleDrive/.shortcut-targets-by-id/10oYqI9u7nCLK0q7xF2CvGGIQVokusjaI/Exjobb/7. Data collection"
    input_dir = "/item_input_csv_files"
    demand_size_dist_dir = "/item_demand_size_distributions_csv_files"

    input_path = f"{data_dir}{input_dir}/item_{item_code}_input.csv"
    demand_size_dist_path = f"{data_dir}{demand_size_dist_dir}/item_{item_code}_d_size_dist.csv"

    output_dir = "/Volumes/GoogleDrive/.shortcut-targets-by-id/10oYqI9u7nCLK0q7xF2CvGGIQVokusjaI/Exjobb/7. Data collection/outdata_csv_files"
    output_path = f"{output_dir}/item_{item_code}_output.csv"
    print("Starting optimization...")
    # optimization
    reorder_point_optimization(input_path, indata_demand_size_dist_path=demand_size_dist_path, outdata_path = output_path)

    print("Saving outputs...")
    # Printing to excel
    df = pd.read_csv(output_path)
    df.to_excel(writer,f"Output item {item_code}")
    
print("Finalizing...")
writer.save()

print("Done!")
