

Sub Run_Click()
    
    'MsgBox ("Testing")
    'Global variables
    pi = 3.141592654
    pdfmax = 1000
    Nmax = 10
    int_steg = 50
    epsilon = 1E-08

    
    Dim interface As Excel.Worksheet
    Dim output As Excel.Worksheet
    Dim Analysis As Excel.Worksheet
    Dim test As Worksheet
    
    Set interface = Worksheets("interface")
    Set test = Worksheets("testutskrifter")
    Set output = Worksheets("Output")
    Set Analysis = Worksheets("Analysis")
    
    Dim k, i, j, nr_loops, CW_demand_choice As Long
    Dim temp, R0, R0_old, R0u, R0l, R0_opt, C_tot_R0l, C_tot_R0u, C_tot_R0, C_tot_opt As Double
    Dim Answer As Variant
    Dim mean_temp, Var_temp, StdDev_temp As Double
    
    
    Dim L_n(), dCdL_n(), beta_n(), beta_u_n(), beta_l_n(), beta_opt_n(), summa_LOK_C, summa_dCdL As Double
    Dim Ci_opt_n(), Ci_R0u_n(), Ci_R0l_n(), Ci_R0_n() As Double
    Dim C0_opt, C0_R0u, C0_R0l, C0_R0, my0 As Double
    Dim R_n(), Rl_n(), Ru_n(), Ll_n(), Lu_n(), R_opt_n(), L_opt_n, delay_n() As Double
    Dim cdfDemand(), pdfDemand() As Double
    Dim search As Boolean
    Dim Beta_BM_unit, Beta_BM_batch As Double
    Dim CV_temp, pdf_compounding_dist_i(), Fillrate_opt_n(), Ready_rate_opt_n(), probability_for_k As Double
    Dim sum As Double
    Dim Medel_L, sigma_L, comp_prob As Double
    
    
'***********Dimensionering av variabler fˆr local search funktionen*******************************
    Dim R_opt_LS_n(), Ci_opt_LS_n(), Fillrate_opt_LS__n(), Ready_rate_opt_LS_n(), L_LS_n() As Double
    Dim R0_opt_LS, C0_opt_LS, C_tot_opt_LS As Double
    Dim Local_search_stop, Local_search_pass_zero As Boolean
    
'*************************************************************************************************
    
    interface.Cells(15, 1) = "START"

'***************  Initialisering (ist‰llet fˆr knapp i kalkylblad) ***********************************************
    Call Fi_init
    
    
'***************  Input data read from excel sheet ***********************************************
    
    Call Input_data(Leadtime_choice, Ret_demand_choice, VR_demand, CW_demand_choice, choice, Compounding_choice, _
                     N, Q_system, Q0, L0, h0, trp_n, h_n, p_n, Q_n, m_n, sigma_n)
    h0 = h0 * Q_system   'The evaluation of the CW is carried out in units of Q_system
    
    ReDim L_n(1 To N)
    ReDim Lu_n(1 To N)
    ReDim Ll_n(1 To N)
    ReDim L_opt_n(1 To N)
    ReDim dCdL_n(1 To N)
    ReDim beta_n(1 To N)
    ReDim beta_u_n(1 To N)
    ReDim beta_l_n(1 To N)
    ReDim beta_opt_n(1 To N)
    ReDim R_n(1 To N)
    ReDim Ru_n(1 To N)
    ReDim Rl_n(1 To N)
    ReDim R_opt_n(1 To N)
    ReDim delay_n(1 To N)
    ReDim Ci_opt_n(1 To N)
    ReDim Ci_R0u_n(1 To N)
    ReDim Ci_R0l_n(1 To N)
    ReDim Ci_R0_n(1 To N)
    
    ReDim Fillrate_opt_n(1 To N)
    ReDim Ready_rate_opt_n(1 To N)
    
    ReDim R_opt_LS_n(1 To N)
    ReDim Ci_opt_LS_n(1 To N)
    ReDim Fillrate_opt_LS_n(1 To N)
    ReDim Ready_rate_opt_LS_n(1 To N)
    ReDim L_LS_n(1 To N)
    ReDim mean_leadtime_demand_n(1 To N)
    ReDim sigma_leadtime_demand_n(1 To N)
    
    

 '**************************************************************************************************
 '*****identify a virtual retailer if any**********************
 
    vr_index = 0
    For i = 1 To N
        If trp_n(i) = 0 Then
            vr_index = i
        End If
    Next
 
    
 '*******Specification of empirical compounding distribution ***************************************
    
    If Ret_demand_choice = 4 Then
      Call Empirical_Compounding(pdf_compounding_dist_n, pdf_compounding_quant_n)
    End If  'Ret_demand_choice =4
    
    If Ret_demand_choice = 0 And Choice_undershoot <> 0 Then                                               'KOLLA HUR VI G÷R MED EMPIRICAL
       If Compounding_choice = 0 Then
         Call Empirical_Compounding(pdf_compounding_dist_n, pdf_compounding_quant_n)
       ElseIf Compounding_choice = 1 Then
         Call Geometric_Compounding_leadtime_demand(pdf_compounding_dist_n, pdf_compounding_quant_n)
       ElseIf Compounding_choice = 2 Then
         Call Geometric_Compounding_beta(pdf_compounding_dist_n, pdf_compounding_quant_n)
       ElseIf Compounding_choice = 3 Then
         Call Logarithmic_Compounding_leadtime_demand(pdf_compounding_dist_n, pdf_compounding_quant_n)
       ElseIf Compounding_choice = 4 Then
         Call Logarithmic_Compounding_alfa(pdf_compounding_dist_n, pdf_compounding_quant_n)
       End If
    End If 'Ret_demand_choice = 0
    
'**************************************************************************************************
            
'********************  Computation of CW demand distribution  *************************************
    
    If CW_demand_choice = 0 Then
        If Ret_demand_choice = 0 Then
            Call D_warehouse(N, pdfN, pmax)
        Else
            Stop 'D_warehouse requires Normal demand
        End If
    
    ElseIf CW_demand_choice = 1 Then
        Call D_warehouse_new(N, pdfN, pmax)
    ElseIf CW_demand_choice >= 2 Then
        If choice <= 2 Then
            Stop             'This choice of demand approx and computational method are not compatible
        Else
            Call D_warehouse_N_approx(CW_demand_choice, N, pdfN, pmax)
        End If
    End If
    
    mean_temp = 0
    Var_temp = 0
    For k = 0 To pmax
      mean_temp = mean_temp + k * pdfN(k)
    Next
    For k = 0 To pmax
      Var_temp = Var_temp + ((k - mean_temp) ^ 2) * pdfN(k)
    Next
    pdfN_kmax = pmax
    StdDev_temp = Sqr(Var_temp)
    'MsgBox "Expected CW demand from computed distribution=" & mean_temp
    'MsgBox "StdDev of CW demand from computed distribution=" & StdDev_temp
    
 '***********************************************************************************************************************************************
    
 '************  The evaluation method DMEIC II, i.e. using iterative procedure in AAM or AM    **************************************************
        
    If (choice = 1) Or (choice = 2) Then
      
        'search from L_i=trp_i
        summa_LOK_C = 0
        summa_dCdL = 0
        For i = 1 To N
            delay_n(i) = 0
            L_n(i) = delay_n(i) + trp_n(i)
            R_n(i) = Calc_R_opt(h_n(i), p_n(i), sigma_n(i) * Sqr(L_n(i)), m_n(i) * L_n(i), Q_system * Q_n(i))
            dCdL_n(i) = dCdL(h_n(i), p_n(i), sigma_n(i), m_n(i), Q_system * Q_n(i), L_n(i))
            beta_n(i) = (dCdL_n(i) / m_n(i)) * Q_system
            temp = LOK_C(h_n(i), p_n(i), sigma_n(i) * Sqr(L_n(i)), m_n(i) * L_n(i), R_n(i), Q_system * Q_n(i))
            summa_LOK_C = summa_LOK_C + temp
            summa_dCdL = summa_dCdL + dCdL_n(i)
        Next i
        
        Call lageropt0_res(m_n, delay_n, beta_n, Q_n, pdfN, h0, Q0, N, R0)
    
        search = True
        Call R0_Ri_search(search, R0, R_n, L_n, delay_n, dCdL_n, beta_n, C_tot_R0u, Ci_R0u_n, C0_R0u)
    
        R0u = R0
        For i = 1 To N
            Rl_n(i) = R_n(i)
            Ll_n(i) = L_n(i)
            beta_l_n(i) = beta_n(i)
        Next
        
        If R0u < -1 Then
            Answer = MsgBox("Warning R0<-Q the teoretical derivation is no longer valid! Continue? ", vbYesNo)
            If Answer <> vbYes Then
                Exit Sub
            End If
        End If
    
        'search from L_i=trp_i+L0
        summa_LOK_C = 0
        summa_dCdL = 0
        For i = 1 To N
            delay_n(i) = L0
            L_n(i) = delay_n(i) + trp_n(i)
            R_n(i) = Calc_R_opt(h_n(i), p_n(i), sigma_n(i) * Sqr(L_n(i)), m_n(i) * L_n(i), Q_system * Q_n(i))
            dCdL_n(i) = dCdL(h_n(i), p_n(i), sigma_n(i), m_n(i), Q_system * Q_n(i), L_n(i))
            beta_n(i) = (dCdL_n(i) / m_n(i)) * Q_system
            temp = LOK_C(h_n(i), p_n(i), sigma_n(i) * Sqr(L_n(i)), m_n(i) * L_n(i), R_n(i), Q_system * Q_n(i))
            summa_LOK_C = summa_LOK_C + temp
            summa_dCdL = summa_dCdL + dCdL_n(i)
        Next i
    
        Call lageropt0_res(m_n, delay_n, beta_n, Q_n, pdfN, h0, Q0, N, R0)
        
        search = True
        Call R0_Ri_search(search, R0, R_n, L_n, delay_n, dCdL_n, beta_n, C_tot_R0l, Ci_R0l_n, C0_R0l)
    
        R0l = R0
        For i = 1 To N
            Ru_n(i) = R_n(i)
            Lu_n(i) = L_n(i)
            beta_u_n(i) = beta_n(i)
        Next
        
        If R0u = R0l Then
            C_tot_opt = C_tot_R0l
            C0_opt = C0_R0l
            R0_opt = R0l
            For i = 1 To N
                R_opt_n(i) = Ru_n(i)
                L_opt_n(i) = Lu_n(i)
                Ci_opt_n(i) = Ci_R0l_n(i)
                beta_opt_n(i) = beta_u_n(i)
            Next
        ElseIf R0u - R0l < 0 Then
            Stop    'This should not be possible
        ElseIf R0u - R0l > 0 Then
            'Search all values in the interval [R0l, R0u]
            If C_tot_R0l < C_tot_R0u Then
                C_tot_opt = C_tot_R0l
                C0_opt = C0_R0l
                R0_opt = R0l
                For i = 1 To N
                    R_opt_n(i) = Ru_n(i)
                    L_opt_n(i) = Lu_n(i)
                    Ci_opt_n(i) = Ci_R0l_n(i)
                    beta_opt_n(i) = beta_u_n(i)
                Next
            Else
                C_tot_opt = C_tot_R0u
                C0_opt = C0_R0u
                R0_opt = R0u
                For i = 1 To N
                    R_opt_n(i) = Rl_n(i)
                    L_opt_n(i) = Ll_n(i)
                    beta_opt_n(i) = beta_l_n(i)
                    Ci_opt_n(i) = Ci_R0u_n(i)
                Next
            End If
            
            For R0 = R0l + 1 To R0u - 1
                search = False
                Call R0_Ri_search(search, R0, R_n, L_n, delay_n, dCdL_n, beta_n, C_tot_R0, Ci_R0_n, C0_R0)
                
                If C_tot_R0 < C_tot_opt Then
                    C_tot_opt = C_tot_R0
                    R0_opt = R0
                    C0_opt = C0_R0
                    For i = 1 To N
                        R_opt_n(i) = R_n(i)
                        L_opt_n(i) = L_n(i)
                        beta_opt_n(i) = beta_n(i)
                        Ci_opt_n(i) = Ci_R0_n(i)
                    Next
                End If
            Next
        End If
        
 '***********************************************************************************************************************************************
    
 '************  The new suggested approach based on beta:s estimated using BM    ****************************************************************

    ElseIf (choice = 3) Or (choice = 4) Then
        
         'Determine Beta_i
        my0 = 0
        vr_index = 0
        For i = 1 To N
            If trp_n(i) = 0 Then
                beta_n(i) = Determine_beta_direct_demand(i, p_n(i))
                vr_index = i
            ElseIf choice = 3 Then
                beta_n(i) = compute_b_approx(Q_n(i) * Q_system, h_n(i), p_n(i), m_n(i), sigma_n(i), trp_n(i))
            ElseIf choice = 4 Then
                beta_n(i) = Beta_BM_tabulated(Q_n(i) * Q_system, h_n(i), p_n(i), m_n(i), sigma_n(i), trp_n(i))
            End If
            my0 = my0 + m_n(i)
            beta_opt_n(i) = beta_n(i) * Q_system
        Next
        
        If 1 = 0 Then 'testutskrift
            test.Cells(6, 4) = beta_opt_n(1)
        End If
        
        'Determine a single induced backorder cost Beta at CW
        Beta_BM_unit = 0
        For i = 1 To N
           Beta_BM_unit = Beta_BM_unit + beta_n(i) * (m_n(i) / my0)
        Next
        Beta_BM_batch = Beta_BM_unit * Q_system
        
        'Optimization of R0 given Beta
        
        R0_opt = Int((my0 / Q_system) * L0)  'Startvalue for R0 optimization
        If (Leadtime_choice <= 1) Or (Leadtime_choice >= 3) Then 'Partial deliveries
            Call lageropt0_new(Q0, N, h0, Beta_BM_batch, pdfN, R0_opt)
            'Call lageropt0(Q0, n, h0, Beta_BM_batch, pdfN, R0_opt)
        ElseIf Leadtime_choice = 2 Then 'complete deliveries
            Stop    'This alternative may be time consuming and is not fully tested
            Call lageropt0_new_res(Q0, N, h0, Beta_BM_batch, pdfN, R0_opt)
        End If
        
        If vr_index > 0 Then
            'Stop
            If choice_VR_Beta = 1 Then 'Modify Beta_VR=p by iteative updating according to Andersson, Axs‰ter Marklund
                Call VR_beta_R0_adjust(vr_index, p_n, R0_opt, L_n, delay_n, dCdL_n, beta_n, Beta_BM_batch, Beta_BM_unit)
                For i = 1 To N
                    beta_opt_n(i) = beta_n(i) * Q_system
                Next
            Else
                'Keep the first estimate Beta_VR=p
            End If
        End If
        
      Local_search_stop = False
      Local_search_pass_zero = True
      
      'Stop
      'R0_opt = -Q0
      Do While Local_search_stop = False
        Call delay_arr(N, R0_opt, Q0, delay_n)
        summa_LOK_C = 0
        
        'Determine E[L_n(i)}]
        For i = 1 To N
            L_n(i) = delay_n(i) + trp_n(i)
        Next
        
        
        'Determine R(i)
        If Ret_demand_choice = 1 Then 'Neg binomial
            If Leadtime_choice <= 2 Or Leadtime_choice >= 5 Then
                Call NegBin_prob(cdfDemand_n, pdfDemand_n, cdfDemandmax_n, L_n)
            ElseIf Leadtime_choice >= 3 Then
                Call NegBin_prob_Li_demand(CW_demand_choice, R0_opt, Q0, cdfDemand_n, pdfDemand_n, cdfDemandmax_n, delay_n)
            End If
        
        ElseIf Ret_demand_choice = 2 Or Ret_demand_choice = 3 Then 'Poisson or Geometric Compound Poisson
            If Leadtime_choice <= 2 Or Leadtime_choice >= 5 Then
                Call Compound_Poisson_geometric_prob(cdfDemand_n, pdfDemand_n, cdfDemandmax_n, L_n)
            ElseIf Leadtime_choice >= 3 Then
                Call Compound_Poisson_geometric_prob_Li_demand(CW_demand_choice, R0_opt, Q0, cdfDemand_n, pdfDemand_n, cdfDemandmax_n, delay_n)
            End If
        
        ElseIf Ret_demand_choice >= 3 Then 'Poisson and Compound Poisson
            If Leadtime_choice <= 2 Or Leadtime_choice >= 5 Then
                Call Compound_Poisson_Empirical(pdf_compounding_dist_n, pdf_compounding_quant_n, max_compounding_dist_n, cdfDemand_n, pdfDemand_n, cdfDemandmax_n, L_n)
            ElseIf Leadtime_choice >= 3 Then
                 Stop
                'Not implemented yet
            End If
           
            
        End If
        
        For i = 1 To N
            If Ret_demand_choice = 0 Then
            
              ReDim pdf_compounding_dist_i(1 To max_compounding_dist_n(i))
              ReDim pdf_compounding_quant_i(1 To max_compounding_dist_n(i))
              For j = 1 To max_compounding_dist_n(i)
                    pdf_compounding_dist_i(j) = pdf_compounding_dist_n(i, j)
                    pdf_compounding_quant_i(j) = pdf_compounding_quant_n(i, j)
              Next
              
              If Cost_Fillrate_opt = 1 Then
                If (i = vr_index) And (VR_undershoot_comp > 0) Then
                  If VR_undershoot_comp = 1 Then
                    Call Calc_R_opt_Fillrate_constraint_Normal_VR_CW(i, R0_opt, Q0, trp_n(i), h_n(i), p_n(i), Target_Fillrate_n(i), max_compounding_dist_n(i), pdf_compounding_dist_i, pdf_compounding_quant_i, cdfDemand_n, pdfDemand_n, cdfDemandmax_n(i), m_n(i), sigma_n(i), Q_system * Q_n(i), L_n(i), R_opt_n(i), Fillrate_opt_n(i), Ready_rate_opt_n(i))
                    Ci_opt_n(i) = LOK_C(h_n(i), p_n(i), sigma_n(i) * Sqr(L_n(i)), m_n(i) * L_n(i), R_opt_n(i), Q_system * Q_n(i))
                  ElseIf VR_undershoot_comp = 2 Then
                    Call Calc_R_opt_Fillrate_constraint_Normal_VR_CW_U2(i, R0_opt, Q0, trp_n(i), h_n(i), p_n(i), Target_Fillrate_n(i), max_compounding_dist_n(i), pdf_compounding_dist_i, pdf_compounding_quant_i, cdfDemand_n, pdfDemand_n, cdfDemandmax_n(i), m_n(i), sigma_n(i), Q_system * Q_n(i), L_n(i), R_opt_n(i), Fillrate_opt_n(i), Ready_rate_opt_n(i))
                    Ci_opt_n(i) = LOK_C(h_n(i), p_n(i), sigma_n(i) * Sqr(L_n(i)), m_n(i) * L_n(i), R_opt_n(i), Q_system * Q_n(i))
                  ElseIf VR_undershoot_comp = 3 Then
                    Call Calc_R_opt_Fillrate_constraint_Normal_VR_CW_U3(i, R0_opt, Q0, trp_n(i), h_n(i), p_n(i), Target_Fillrate_n(i), max_compounding_dist_n(i), pdf_compounding_dist_i, pdf_compounding_quant_i, cdfDemand_n, pdfDemand_n, cdfDemandmax_n(i), m_n(i), sigma_n(i), Q_system * Q_n(i), L_n(i), R_opt_n(i), Fillrate_opt_n(i), Ready_rate_opt_n(i))
                    Ci_opt_n(i) = LOK_C(h_n(i), p_n(i), sigma_n(i) * Sqr(L_n(i)), m_n(i) * L_n(i), R_opt_n(i), Q_system * Q_n(i))
                  Else
                    Stop
                  End If
                Else
                  If Choice_undershoot = 1 Then
                    Call undershootComplex(i, max_compounding_dist_n(i), Q_n(i) * Q_system, m_n(i) * L_n(i), sigma_n(i) * Sqr(L_n(i)), R_opt_n(i), Ci_opt_n(i), interface.Cells(i + 18, 9))
                  Else
                   If Choice_undershoot = 2 Then
                     Medel_L = m_n(i) * L_n(i) + undershootsimple1(i, max_compounding_dist_n(i))
                     sigma_L = sigma_n(i) * Sqr(L_n(i))
                   ElseIf Choice_undershoot = 3 Then
                     Medel_L = m_n(i) * L_n(i) + undershootsimple2(i, max_compounding_dist_n(i), Q_n(i) * Q_system)
                     sigma_L = sigma_n(i) * Sqr(L_n(i))
                   ElseIf Choice_undershoot = 4 Then
                     Medel_L = m_n(i) * L_n(i) + undershootsimple1(i, max_compounding_dist_n(i))
                     sigma_L = Sqr(sigma_n(i) * sigma_n(i) * L_n(i) + undershootsimple3(i, max_compounding_dist_n(i)))
                   ElseIf Choice_undershoot = 5 Then
                     Medel_L = m_n(i) * L_n(i) + undershootsimple2(i, max_compounding_dist_n(i), Q_n(i) * Q_system)
                     sigma_L = Sqr(sigma_n(i) * sigma_n(i) * L_n(i) + undershootsimple4(i, max_compounding_dist_n(i), Q_n(i) * Q_system))
                   Else
                     MsgBox "No adjustment will be made"
                     Medel_L = m_n(i) * L_n(i)
                     sigma_L = sigma_n(i) * Sqr(L_n(i))
                   End If
                   R_opt_n(i) = Calc_R_opt(h_n(i), p_n(i), sigma_L, Medel_L, Q_system * Q_n(i))
                   Ci_opt_n(i) = LOK_C(h_n(i), p_n(i), sigma_n(i) * Sqr(L_n(i)), m_n(i) * L_n(i), R_opt_n(i), Q_system * Q_n(i))
                  End If
                  Call Calc_Fillrate_R_Normal_Demand(R_opt_n(i), sigma_n(i), m_n(i), Q_system * Q_n(i), L_n(i), Fillrate_opt_n(i))
                End If
                
                summa_LOK_C = summa_LOK_C + Ci_opt_n(i)
                
              Else 'Cost optimization
                Stop 'Not implemented for Normal demand in this program
              
              End If
                
            ElseIf Ret_demand_choice > 0 Then
                If Leadtime_choice <= 2 Or Leadtime_choice >= 5 Then
                    CV_temp = (sigma_n(i) ^ 2) / m_n(i)
                ElseIf Leadtime_choice >= 3 Then
                    CV_temp = sigma_leadtime_demand_n(i) ^ 2 / mean_leadtime_demand_n(i)
                End If
                
                If ((CV_temp - 1) < -1 * Exp(-10)) Or (vr_index = i And VR_demand = 0) Then 'Normal demand
                 If Leadtime_choice >= 3 Then
                        Stop
                        'Not implemented yet
                 End If
                 
                 If (i = vr_index) And (VR_undershoot_comp > 0) Then
                    ReDim pdf_compounding_dist_i(1 To max_compounding_dist_n(i))
                    ReDim pdf_compounding_quant_i(1 To max_compounding_dist_n(i))
                    For j = 1 To max_compounding_dist_n(i)
                        pdf_compounding_dist_i(j) = pdf_compounding_dist_n(i, j)
                        pdf_compounding_quant_i(j) = pdf_compounding_quant_n(i, j)
                    Next
                    If VR_undershoot_comp = 1 Then
                        Call Calc_R_opt_Fillrate_constraint_Normal_VR_CW(i, R0_opt, Q0, trp_n(i), h_n(i), p_n(i), Target_Fillrate_n(i), max_compounding_dist_n(i), pdf_compounding_dist_i, pdf_compounding_quant_i, cdfDemand_n, pdfDemand_n, cdfDemandmax_n(i), m_n(i), sigma_n(i), Q_system * Q_n(i), L_n(i), R_opt_n(i), Fillrate_opt_n(i), Ready_rate_opt_n(i))
                        Ci_opt_n(i) = LOK_C(h_n(i), p_n(i), sigma_n(i) * Sqr(L_n(i)), m_n(i) * L_n(i), R_opt_n(i), Q_system * Q_n(i))
                    ElseIf VR_undershoot_comp = 2 Then
                        Call Calc_R_opt_Fillrate_constraint_Normal_VR_CW_U2(i, R0_opt, Q0, trp_n(i), h_n(i), p_n(i), Target_Fillrate_n(i), max_compounding_dist_n(i), pdf_compounding_dist_i, pdf_compounding_quant_i, cdfDemand_n, pdfDemand_n, cdfDemandmax_n(i), m_n(i), sigma_n(i), Q_system * Q_n(i), L_n(i), R_opt_n(i), Fillrate_opt_n(i), Ready_rate_opt_n(i))
                        Ci_opt_n(i) = LOK_C(h_n(i), p_n(i), sigma_n(i) * Sqr(L_n(i)), m_n(i) * L_n(i), R_opt_n(i), Q_system * Q_n(i))
                    ElseIf VR_undershoot_comp = 3 Then
                        Call Calc_R_opt_Fillrate_constraint_Normal_VR_CW_U3(i, R0_opt, Q0, trp_n(i), h_n(i), p_n(i), Target_Fillrate_n(i), max_compounding_dist_n(i), pdf_compounding_dist_i, pdf_compounding_quant_i, cdfDemand_n, pdfDemand_n, cdfDemandmax_n(i), m_n(i), sigma_n(i), Q_system * Q_n(i), L_n(i), R_opt_n(i), Fillrate_opt_n(i), Ready_rate_opt_n(i))
                        Ci_opt_n(i) = LOK_C(h_n(i), p_n(i), sigma_n(i) * Sqr(L_n(i)), m_n(i) * L_n(i), R_opt_n(i), Q_system * Q_n(i))
                    ElseIf VR_undershoot_comp = 4 Then
                        Call Calc_R_opt_Fillrate_constraint_Normal_VR_CW_U4(i, R0_opt, Q0, trp_n(i), h_n(i), p_n(i), Target_Fillrate_n(i), max_compounding_dist_n(i), pdf_compounding_dist_i, pdf_compounding_quant_i, cdfDemand_n, pdfDemand_n, cdfDemandmax_n(i), m_n(i), sigma_n(i), Q_system * Q_n(i), L_n(i), R_opt_n(i), Fillrate_opt_n(i), Ready_rate_opt_n(i))
                        Ci_opt_n(i) = LOK_C(h_n(i), p_n(i), sigma_n(i) * Sqr(L_n(i)), m_n(i) * L_n(i), R_opt_n(i), Q_system * Q_n(i))
                    Else
                        Stop
                    End If
                 Else
                  If Cost_Fillrate_opt > 0 Then
                    If Choice_undershoot = 1 Then
                      Call undershootComplex(i, max_compounding_dist_n(i), Q_n(i) * Q_system, m_n(i) * L_n(i), sigma_n(i) * Sqr(L_n(i)), R_opt_n(i), Ci_opt_n(i), interface.Cells(i + 18, 9))
                    Else
                      If Choice_undershoot = 2 Then
                        Medel_L = m_n(i) * L_n(i) + undershootsimple1(i, max_compounding_dist_n(i))
                        sigma_L = sigma_n(i) * Sqr(L_n(i))
                      ElseIf Choice_undershoot = 3 Then
                        Medel_L = m_n(i) * L_n(i) + undershootsimple2(i, max_compounding_dist_n(i), Q_n(i) * Q_system)
                        sigma_L = sigma_n(i) * Sqr(L_n(i))
                      ElseIf Choice_undershoot = 4 Then
                        Medel_L = m_n(i) * L_n(i) + undershootsimple1(i, max_compounding_dist_n(i))
                        sigma_L = Sqr(sigma_n(i) * sigma_n(i) * L_n(i) + undershootsimple3(i, max_compounding_dist_n(i)))
                      ElseIf Choice_undershoot = 5 Then
                        Medel_L = m_n(i) * L_n(i) + undershootsimple2(i, max_compounding_dist_n(i), Q_n(i) * Q_system)
                        sigma_L = Sqr(sigma_n(i) * sigma_n(i) * L_n(i) + undershootsimple4(i, max_compounding_dist_n(i), Q_n(i) * Q_system))
                      Else
                        MsgBox "No adjustment will be made"
                        Medel_L = m_n(i) * L_n(i)
                        sigma_L = sigma_n(i) * Sqr(L_n(i))
                      End If
                      R_opt_n(i) = Calc_R_opt(h_n(i), p_n(i), sigma_L, Medel_L, Q_system * Q_n(i))
                      Ci_opt_n(i) = LOK_C(h_n(i), p_n(i), sigma_n(i) * Sqr(L_n(i)), m_n(i) * L_n(i), R_opt_n(i), Q_system * Q_n(i))
                    End If
                    Call Calc_Fillrate_R_Normal_Demand(R_opt_n(i), sigma_n(i), m_n(i), Q_system * Q_n(i), L_n(i), Fillrate_opt_n(i))
                  Else
                    Stop 'Costoptimization not implemented for normal demand in this program
                  End If
                 End If
                 summa_LOK_C = summa_LOK_C + Ci_opt_n(i)
                
                Else    'Compound Poisson or Pure Poisson Demand
                    ReDim cdfDemand_i(0 To cdfDemandmax_n(i))
                    ReDim pdfDemand_i(0 To cdfDemandmax_n(i))
                
                    For j = 0 To cdfDemandmax_n(i)
                        cdfDemand_i(j) = cdfDemand_n(i, j)
                        pdfDemand_i(j) = pdfDemand_n(i, j)
                    Next
                    
                    ReDim pdf_compounding_dist_i(1 To max_compounding_dist_n(i))
                    ReDim pdf_compounding_quant_i(1 To max_compounding_dist_n(i))
                     For j = 1 To max_compounding_dist_n(i)
                        pdf_compounding_dist_i(j) = pdf_compounding_dist_n(i, j)
                        pdf_compounding_quant_i(j) = pdf_compounding_quant_n(i, j)
                    Next
                    
                  If Cost_Fillrate_opt = 0 Then 'Cost optimization
                    If trp_n(i) = 0 Then 'Approximation of combined fillrate fˆr the Virtual Retalier and the CW when there is direct customer demand at the CW
                      'Note: the modified lead-time demand at the virtual retailer is determined inside the following subroutine!
                      Call Calc_R_opt_backorder_cost_model_compound_Poisson_VR_CW(i, R0_opt, Q0, trp_n(i), h_n(i), p_n(i), Target_Fillrate_n(i), max_compounding_dist_n(i), pdf_compounding_dist_i, pdf_compounding_quant_i, cdfDemand_i, pdfDemand_i, cdfDemandmax_n(i), m_n(i), sigma_n(i), Q_system * Q_n(i), L_n(i), R_opt_n(i), Fillrate_opt_n(i), Ready_rate_opt_n(i), Ci_opt_n(i))
                      'Call Calc_R_opt_Fillrate_constraint_compound_Poisson_VR_CW(i, R0_opt, Q0, trp_n(i), h_n(i), p_n(i), Target_Fillrate_n(i), max_compounding_dist_n(i), pdf_compounding_dist_i, pdf_compounding_quant_i, cdfDemand_i, pdfDemand_i, cdfDemandmax_n(i), m_n(i), sigma_n(i), Q_system * Q_n(i), L_n(i), R_opt_n(i), Fillrate_opt_n(i), Ready_rate_opt_n(i), Ci_opt_n(i))
                    Else
                      Call Calc_R_opt_and_Lok_C_compound_Poisson(h_n(i), p_n(i), max_compounding_dist_n(i), pdf_compounding_dist_i, pdf_compounding_quant_i, cdfDemand_i, pdfDemand_i, cdfDemandmax_n(i), m_n(i), sigma_n(i), Q_system * Q_n(i), L_n(i), R_opt_n(i), Ci_opt_n(i), Fillrate_opt_n(i), Ready_rate_opt_n(i))
                    End If
                    summa_LOK_C = summa_LOK_C + Ci_opt_n(i)
                  
                  Else  'Optimization under fillrate constraint
                    'Stop
                    If trp_n(i) = 0 Then 'Approximation of combined fillrate fˆr the Virtual Retalier and the CW when there is direct customer demand at the CW
                      'Note: the modified lead-time demand at the virtual retailer is determined inside the following subroutine!
                      Call Calc_R_opt_Fillrate_constraint_compound_Poisson_VR_CW(i, R0_opt, Q0, trp_n(i), h_n(i), p_n(i), Target_Fillrate_n(i), max_compounding_dist_n(i), pdf_compounding_dist_i, pdf_compounding_quant_i, cdfDemand_i, pdfDemand_i, cdfDemandmax_n(i), m_n(i), sigma_n(i), Q_system * Q_n(i), L_n(i), R_opt_n(i), Fillrate_opt_n(i), Ready_rate_opt_n(i), Ci_opt_n(i))
                    Else
                      Call Calc_R_opt_Fillrate_constraint_compound_Poisson(h_n(i), p_n(i), Target_Fillrate_n(i), max_compounding_dist_n(i), pdf_compounding_dist_i, pdf_compounding_quant_i, cdfDemand_i, pdfDemand_i, cdfDemandmax_n(i), m_n(i), sigma_n(i), Q_system * Q_n(i), L_n(i), R_opt_n(i), Fillrate_opt_n(i), Ready_rate_opt_n(i), Ci_opt_n(i))
                    End If
                    summa_LOK_C = summa_LOK_C + Ci_opt_n(i)
                    
                  End If
               End If
            End If
            

        Next i
        
        If Q0 > 3000 Then
            'Fˆr att spara ber‰kningstid ber‰knas inte kostnaderna i detta fall
            'Stop
            C0_opt = 0
            C_tot_opt = 0
        Else
            If (Leadtime_choice <= 1) Or (Leadtime_choice >= 3) Then
                If vr_index > 0 Then
                    'Stop
                    'C0_opt = cost0(R0_opt, Q0, h0, pdfN)
                    C0_opt = 0
                Else
                    C0_opt = cost0(R0_opt, Q0, h0, pdfN)
                End If
            ElseIf Leadtime_choice = 2 Then 'Complete deliveries
                Stop
                C0_opt = cost0_res(N, R0_opt, Q0, h0, m_n, delay_n, pdfN)
            End If
            C_tot_opt = C0_opt + summa_LOK_C
            
        End If
        
        If Local_search_pass_zero = True Then
            Local_search_pass_zero = False
            C_tot_opt_LS = 2 * C_tot_opt + 1
        End If
        
        If C_tot_opt <= C_tot_opt_LS Then
            R0_opt_LS = R0_opt
            C0_opt_LS = C0_opt
            C_tot_opt_LS = C_tot_opt
            For i = 1 To N
                Ci_opt_LS_n(i) = Ci_opt_n(i)
                R_opt_LS_n(i) = R_opt_n(i)
                Fillrate_opt_LS_n(i) = Fillrate_opt_n(i)
                Ready_rate_opt_LS_n(i) = Ready_rate_opt_n(i)
                L_LS_n(i) = L_n(i)
            Next
            R0_opt = R0_opt + 1
            Local_search_stop = False
        Else
            Local_search_stop = True
        End If
        
        If Local_search_choice = 0 Or C_tot_opt = 0 Then
            Local_search_stop = True
        End If
        
      Loop    'Local search
            
       
        
    End If 'choice
    
    'Utskrift
    If choice > 0 Then
        interface.Cells(9, 2) = choice
        interface.Cells(9, 11) = C_tot_opt_LS
        interface.Cells(17, 12) = Beta_BM_unit
        interface.Cells(17, 11) = C0_opt_LS
        interface.Cells(17, 13) = R0_opt_LS
        interface.Cells(17, 16) = L0
        For i = 1 To N
            interface.Cells(18 + i, 11) = Ci_opt_LS_n(i)
            interface.Cells(18 + i, 13) = R_opt_LS_n(i)
            interface.Cells(18 + i, 12) = beta_opt_n(i) / Q_system
            interface.Cells(18 + i, 14) = Fillrate_opt_LS_n(i)
            interface.Cells(18 + i, 15) = Ready_rate_opt_LS_n(i)
            interface.Cells(18 + i, 16) = L_LS_n(i)
        Next
    End If
    
    interface.Cells(15, 1) = "STOP"
    
End Sub