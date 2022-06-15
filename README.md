# examensarbete
Welcome to this repo!

This program was developed by Jakob Bengtsson and Alexander Larsson as a part of 
their master thesis "Comparing single- and multiecehlon methods for inventory 
control of spare parts at Volvo". 

The thesis was conducted at LTH, Lund University in collaboration with 
Volvo Group - SML Advanced Analytics. Contacts at Volvo were Johan Lidvall and Christian Beckers.

The main purpose of the program in this repo is to find optimal reorder points for
an (R,Q) - policy for a 2 - echelon OWMR network. A network pattern that is often 
found in the vast distribution network at Volvo Group - SML. The program uses a
multiechelon model developed by Johan Marklund and Peter Berling (2013,2014). 

In the directory "single_echelon_utils" you can find functions related to inventory
control on the single-node basis. In the directory "warehouse_modeling" functions 
related to the multiechelon model can be found.

The directories "data_collection_for_evaluation" and "graph_generation_for_report"
is related to the numerical study conducted as a part of the thesis (sections 5 and 6 
in the report). These directories does not hold functions necessary to run the model.

The program is executed by the function "reorder_point_optimization" found in "main_program.py". Provided is a test case in the file "test_case_notebook.ipynb" where the program in
"main_program" is divided into steps. The test case uses data provided in the folder 
"test_case_data", however, other test case data is easily added. In the test case, 
every step in the modeling process can be executed sequentially in order to gain 
a better understanding of the code.

To fully comprehend the details of the model and functions in the program, we refer to 
the written report of the thesis. In the code, relevant references to the report are
denoted.

We hope that you a good time learning about this multiechelon model!

Over and out,
Jakob Bengtsson and Alexander Larsson