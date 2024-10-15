# Master thesis in multi echelon supply chain optimization
Welcome to this repo!

# Overview
This program was developed by me (Jakob Bengtsson) and Alexander Larsson as a part of our master thesis "Comparing single- and multiecehlon methods for inventory control of spare parts at Volvo". 

The thesis was conducted at LTH, Lund University in collaboration with Volvo Group - SML Advanced Analytics. Contacts at Volvo were Johan Lidvall and Christian Beckers.

The main purpose of the program in this repo is to find optimal reorder points for an (R,Q) - policy for a 2 - echelon OWMR network, i.e. One Warehouse serving Multiple Retailers. A network pattern that is often found in the vast distribution network at Volvo Group - SML. The program uses a multiechelon model based on pulishings by Johan Marklund and Peter Berling (2013,2014). 

An (R,Q) - policy is a replenishment policy used at stock keeping instances in the supply chain, e.g. shops, distribution centers, warehouses. The policy is to place an order of Q units when the current stock are at R units. The Q is often chosen to minimize delivery costs, e.g., going with a Q that ensures full pallets. The R is chosen with consideration to customer service and cost. Simplified, one could say that a higher R means that we're keeping a larger stock volume, which means better service (availability) but also higher costs.

With a single-location warehouse, the optimization problem is trivial, but the complexity grows with multiple levels (echelons) in the supply chain where the policy settings in the different stock locations affect the service to other installations. The method in this repository uses a modeling architecture allowing the optimization problem for the supply chain to be separated into multiple smaller optimization problems - which allows for scaling to larger networks.

To fully comprehend the details of the model and functions in the program, we refer to the written report of the thesis. In the code, relevant references to the report are denoted.
We hope that you a good time learning about this multiechelon model!

Over and out,
Jakob Bengtsson and Alexander Larsson

# 2024 thoughts
We wrote this program in 2022, here's some changes I would make if I would redo the project today.
- I would try to modularize more, for one, the main scripts are quite long and does a lot of things. There's also some files that holds a lot of functions, sometimes only semi-related. More separation would lead to better readability.
- Variable names. We used a lot of names related to the mathematical equations. Like "V", "V_z", "E" and so on. Coming back to this two years later without having the equations infront of me makes me wish that we would've spelled it out better.
- Added a src, run and try_me to make the package more approachable.


# Installation / Dependencies
- scipy
- numpy
- pandas
- simpy (if accessing the simulation)

# Directories
- src/single_echelon_utils: functions related to modeling inventory control on the single-node basis.
- src/warehouse_modeling: functions related to modeling the warehouse in the multiechelon setting can be found.
- src/simulation: functions related to simulating demand patterns to test the model (not actually used in the run script)
- src/reorder_point_optimization_bm: Main scripts for the reorder point optimization with the BM model
- src/reorder_point_optimization_naive: Main scripts for the reorder point optimization with the naive model
- Main scripts for the reorder point optimization

- try_me.ipynb: runs the main script with the test data and displays the results

# Learnings (from a coding perspective)
