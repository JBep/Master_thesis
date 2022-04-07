Option Explicit
Sub Fillrate_calc(R, max_compounding, pdf_compounding_dist, pdf_compounding_quant, min_IL_r0, max_IL_r0, Prob_dist_IL_r0, Fillrate_Ri)
'Calculates fillrate for compound Poisson demand according to Axs‰ter 2006 eq. 5.51

Dim k, IL, min_IL, max_IL, min_IL_k As Long
Dim temp1, temp2, temp3, sum, Mean_compound As Double

    'Stop
    Mean_compound = 0
    For k = 1 To max_compounding
        Mean_compound = Mean_compound + pdf_compounding_quant(k) * pdf_compounding_dist(k)
    Next
    
    min_IL = min_IL_r0 + R
    If min_IL < 1 Then
        min_IL = 1
    End If
    max_IL = max_IL_r0 + R
    
    sum = 0
    For IL = min_IL To max_IL
        For k = 1 To max_compounding
            If pdf_compounding_quant(k) < IL Then
                min_IL_k = pdf_compounding_quant(k)
            Else
                min_IL_k = IL
            End If
            sum = sum + min_IL_k * pdf_compounding_dist(k) * Prob_dist_IL_r0(IL - R)
        Next
    Next
    
    Fillrate_Ri = sum / Mean_compound
    
End Sub
Sub IL_VR_CW_prob_dist(R, Q, R0, Q0, pdf_IL0, prob_IL0_positive, Prob_dist_IL_r0, min_IL_r0, min_IL, max_IL, cdf_IL, Prob_dist_IL)
'Calculates the combined VR_CW inventory level distribution

Dim j, k, IL, min_IL_k, S_vr As Long
Dim temp1, temp2, temp3, sum As Double

    S_vr = R + Q
    max_IL = S_vr + R0 + Q0
    min_IL = min_IL_r0 + R

    ReDim Prob_dist_IL(min_IL To max_IL)
    ReDim cdf_IL(min_IL To max_IL)
    sum = 0
    For j = min_IL To max_IL
        If j <= S_vr Then
            Prob_dist_IL(j) = (1 - prob_IL0_positive) * Prob_dist_IL_r0(j - R)
            
        Else 'S_vr <j<S_vr+R0+Q0
            Prob_dist_IL(j) = pdf_IL0(j - S_vr)

        End If
        sum = sum + Prob_dist_IL(j)
        cdf_IL(j) = sum
    Next
    
    If Abs(1 - sum) > epsilon Then
        Stop
    End If


End Sub
Sub IL_VR_CW_prob_dist_Norm(R, Q, R0, Q0, pdf_IL0, prob_IL0_positive, Prob_dist_IL_r0, min_IL_r0, max_IL_r0, min_IL, max_IL, cdf_IL, Prob_dist_IL)
'Calculates the combined VR_CW inventory level distribution

Dim j, k, IL, min_IL_k, S_vr As Long
Dim temp1, temp2, temp3, sum As Double
Dim delta As Double


    S_vr = R + Q
    
    delta = (max_IL_r0 + R) - S_vr
    If delta <= 0 Then
        max_IL = S_vr + R0 + Q0
    ElseIf delta <= (R0 + Q0) Then
        max_IL = S_vr + R0 + Q0
    Else
        Stop
        max_IL = S_vr + delta
    End If
        
    min_IL = min_IL_r0 + R

    ReDim Prob_dist_IL(min_IL To max_IL)
    ReDim cdf_IL(min_IL To max_IL)
    sum = 0
    For j = min_IL To max_IL
        If j <= S_vr Then
            Prob_dist_IL(j) = (1 - prob_IL0_positive) * Prob_dist_IL_r0(j - R)
        Else 'S_vr <j<max_IL
            
            If max_IL <= S_vr + R0 + Q0 Then
                If j <= (max_IL_r0 + R) Then
                    Prob_dist_IL(j) = pdf_IL0(j - S_vr) + (1 - prob_IL0_positive) * Prob_dist_IL_r0(j - R)
                Else    'j>max_IL_r0 + R)
                    Prob_dist_IL(j) = pdf_IL0(j - S_vr)
                End If
            Else 'max_IL > S_vr + R0 + Q0
                If j <= S_vr + R0 + Q0 Then
                    Prob_dist_IL(j) = pdf_IL0(j - S_vr) + (1 - prob_IL0_positive) * Prob_dist_IL_r0(j - R)
                Else    'j>max_IL_r0 + R)
                    Prob_dist_IL(j) = (1 - prob_IL0_positive) * Prob_dist_IL_r0(j - R)
                End If
            End If

        End If
        sum = sum + Prob_dist_IL(j)
        cdf_IL(j) = sum
    Next
    
    If Abs(1 - sum) > 1E-06 Then
        Stop
    End If


End Sub


Sub Fillrate_calc_VR_CW(max_compounding, pdf_compounding_dist, pdf_compounding_quant, min_IL, max_IL, Prob_dist_IL, Fillrate_Ri)
'Calculates fillrate for upstream demand by combining the stock at the virtual retailer and the CW

Dim k, IL, min_IL_k, min_IL_positive As Long
Dim temp1, temp2, temp3, sum, Mean_compound As Double

    'Stop
    Mean_compound = 0
    For k = 1 To max_compounding
        Mean_compound = Mean_compound + pdf_compounding_quant(k) * pdf_compounding_dist(k)
    Next

    If min_IL < 1 Then
        min_IL_positive = 1
    Else
        min_IL_positive = min_IL
    End If

    sum = 0
    For IL = min_IL_positive To max_IL
        For k = 1 To max_compounding
            If pdf_compounding_quant(k) < IL Then
                min_IL_k = pdf_compounding_quant(k)
            Else
                min_IL_k = IL
            End If
            sum = sum + min_IL_k * pdf_compounding_dist(k) * Prob_dist_IL(IL)
        Next
    Next
    
    Fillrate_Ri = sum / Mean_compound
    
End Sub
Sub Calc_R_opt_Fillrate_constraint_compound_Poisson_VR_CW(vr_index, R0, Q0, trp, h, p, Target_Fillrate_i, max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, cdfDemand_L_i, pdfDemand_L_i, cdfDemandmax_L_i, my, sigma, Q, L, R_opt, Fillrate_R, Ready_rate_R, C_opt)

Dim R, R_plus_one, i, j, k, IL, k_min, R_comb, max_IL0, min_IL0 As Long
Dim slask, B0_temp, sum_prob, sumprob, sum_check, Prob_dist_IL_r0(), Prob_dist_IL(), cdf_IL_r0(), min_IL_r0, max_IL_r0 As Double
Dim Ratio_h_p, Expected_IL_plus, Expected_IL_minus, temp1, temp2 As Double
Dim pdf_IL0(), pdf_IL_CW_VR(), prob_IL0_positive, Prob_IL0_zero_or_less As Double
Dim L_vr, cdfDemand(), pdfDemand(), cdfDemand_vr(), pdfDemand_vr(), cdf_IL_vr_CW(), Prob_dist_IL_vr_CW(), CW_demand_rate As Double
Dim L_min, L_max As Double
Dim cdfDemandmax, cdfDemandmax_vr, min_IL_vr_CW, max_IL_VR_CW As Long
Dim Fillrate_CW_VRdemand, ReadyRate_CW_VRdemand As Double

'Dim k_max As Long

Dim test As Worksheet
Set test = Worksheets("testutskrifter")

'Step 1: Calculating the pdf of the inventory level distribution at the CW P(IL0=j) for j>=0
max_IL0 = R0 + Q0
min_IL0 = R0 + 1 - pdfN_kmax
ReDim pdf_IL0(min_IL0 To max_IL0)

    prob_IL0_positive = 0
    For j = 1 To max_IL0
        sumprob = 0
        If j < R0 + 1 Then
            k_min = R0 + 1
        Else
            k_min = j
        End If
        
        For k = k_min To max_IL0   'max_IL0=(R0 + Q0)
            If (k - j) <= pdfN_kmax Then
                sumprob = sumprob + pdfN(k - j) / Q0
            Else 'IL0=j cannot be reached from IP0=k as (k-j)> max leadtime demand pdfN_kmax
                'Stop
            End If
        Next
        pdf_IL0(j) = sumprob
        prob_IL0_positive = prob_IL0_positive + sumprob
    Next
    Prob_IL0_zero_or_less = 1 - prob_IL0_positive

If 1 = 1 Then
'The probabilities for backorders at the CW P(IL0=j) not really needed here but computed for verification reasons
    slask = 0
    For j = min_IL0 To 0
        sumprob = 0
        If j < R0 + 1 Then
            k_min = R0 + 1
        Else
            k_min = j
        End If
    
        For k = k_min To max_IL0   'max_IL0=(R0 + Q0)
            If (k - j) <= pdfN_kmax Then
                sumprob = sumprob + pdfN(k - j) / Q0
            Else 'IL0=j cannot be reached from IP0=k as (k-j)> max leadtime demand pdfN_kmax
            End If
        Next
        pdf_IL0(j) = sumprob
        slask = slask + sumprob
    Next
    sum_check = 0
    B0_temp = 0
    For j = min_IL0 To -1
        sum_check = sum_check + (-j * pdf_IL0(j))
        B0_temp = B0_temp + (-j) * (pdf_IL0(j) / Prob_IL0_zero_or_less)
    Next
    
    If Abs(slask - Prob_IL0_zero_or_less) > (epsilon * 1000) Then
        Stop
    End If
   '
    If Abs(slask + prob_IL0_positive - 1) > (epsilon * 1000) Then
        Stop
    End If
End If

'Step 2: Estimation of the expected CW delay for the Virtual retailer when the CW is out of stock, i.e., IL0<=0
    'choice_L_vr = 1
    If choice_L_vr = 0 Then 'Standard Metric approx i.e., LM
        L_vr = L
        cdfDemandmax = cdfDemandmax_L_i
        ReDim pdfDemand(0 To cdfDemandmax)
        ReDim cdfDemand(0 To cdfDemandmax)
        For j = 0 To cdfDemandmax_L_i
            cdfDemand(j) = cdfDemand_L_i(j)
            pdfDemand(j) = pdfDemand_L_i(j)
        Next
    ElseIf choice_L_vr = 1 Then 'estimating L_vr as a two point distribution with mean according to Metric i.e., L
        L_min = trp_n(vr_index)
        If R0 >= 0 Then
            L_max = L0
        Else
            Stop    'not thoroughly verified yet
            For i = 1 To N
                CW_demand_rate = CW_demand_rate + m_n(i)
            Next
            L_max = L0 - (R0 / CW_demand_rate)
        End If
        Call Comp_Poisson_Empirical_Li_2_point_dist(vr_index, pdf_compounding_dist, pdf_compounding_quant, max_compounding_dist, cdfDemand, pdfDemand, cdfDemandmax, L, L_min, L_max)
    ElseIf choice_L_vr = 2 Or choice_L_vr = 3 Then 'adjusted mean as the average delay for units that are delayed
      If choice_L_vr = 2 Then
        Call Fillrate_calc_VR_CW(max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, 1, max_IL0, pdf_IL0, Fillrate_CW_VRdemand)
        'Logic: L=0*Fillrate_CW_VRdemand+L_vr*(1-Fillrate_CW_VRdemand)  =>  L_vr=L/(1-Fillrate_CW_VRdemand
        'Fillrate_CW_VRdemand = proportion of VR demand that can be satisfied directly from CW stock ,i.e., with leadtime=0
        '1-Fillrate_CW_VRdemand = proportion of VR demand experiencing a leadtime >0 with average L_vr
        L_vr = L / (1 - Fillrate_CW_VRdemand)
      
      Else 'choice_L_vr=3
        'Logic: L=0*ReadtRate_CW_VRdemand+L_vr*(1-Readyrate_CW_VRdemand)  =>  L_vr=L/(1-ReadyRate_CW_VRdemand
        'Readyrate_CW_VRdemand = proportion of timw that VR demand that can be satisfied directly from CW stock ,i.e., with leadtime=0
        '1-ReadyRate_CW_VRdemand = proportion of time VR demand experiencing a leadtime >0 with average L_vr
        ReadyRate_CW_VRdemand = prob_IL0_positive
        L_vr = L / (1 - ReadyRate_CW_VRdemand)
      End If
        If VR_demand = 4 Then
            Call Compound_Poisson_Empirical_VR(vr_index, pdf_compounding_dist, pdf_compounding_quant, max_compounding_dist, cdfDemand, pdfDemand, cdfDemandmax, L_vr)
        ElseIf VR_demand = 1 Then
            Call NegBin_prob_VR(vr_index, cdfDemand, pdfDemand, cdfDemandmax, L_vr)
        ElseIf VR_demand = 2 Then 'geometric compounding distribution
            Call Compound_Poisson_geometric_prob_VR(vr_index, cdfDemand, pdfDemand, cdfDemandmax, L_vr)
        Else
            Stop 'otillÂtet val
        End If
        
    End If
    
If 1 = 0 Then 'testutskrift
    For j = 0 To cdfDemandmax
        test.Cells(3 + j, 25) = j
        test.Cells(3 + j, 26) = pdfDemand(j)
    Next
    For j = 0 To cdfDemandmax_L_i
         test.Cells(3 + j, 27) = j
         test.Cells(3 + j, 28) = pdfDemand_L_i(j)
    Next
End If
       
If 1 = 0 Then 'with L_vr=L we can use the previously calculated lead-time demand distributions cdfDemand, pdfDemand

'Step 3: Calculation of the demand for the virtual retailer conditioned on L_vr, i.e. the CW being out of stock
    If Leadtime_choice > 0 Then
        Stop    'Only Metric approx implemented so far
    End If
    
    'If Ret_demand_choice = 1 Then 'Neg binomial
    '       If Leadtime_choice <= 2 Then
    '            Call NegBin_prob(cdfDemand_n, pdfDemand_n, cdfDemandmax_n, L_n)
    '        ElseIf Leadtime_choice >= 3 Then
    '            Call NegBin_prob_Li_demand(CW_demand_choice, R0_opt, Q0, cdfDemand_n, pdfDemand_n, cdfDemandmax_n, delay_n)
    '        End If
        
    '    ElseIf Ret_demand_choice = 2 Or Ret_demand_choice = 3 Then 'Poisson or Geometric Compound Poisson
    '        If Leadtime_choice <= 2 Then
    '            Call Compound_Poisson_geometric_prob(cdfDemand_n, pdfDemand_n, cdfDemandmax_n, L_n)
    '        ElseIf Leadtime_choice >= 3 Then
    '            Call Compound_Poisson_geometric_prob_Li_demand(CW_demand_choice, R0_opt, Q0, cdfDemand_n, pdfDemand_n, cdfDemandmax_n, delay_n)
    '        End If
        
    '    ElseIf Ret_demand_choice >= 3 Then 'Poisson and Compound Poisson
    '        If Leadtime_choice <= 2 Then
    '           call Compound_Poisson_Empirical_VR(j_vr, pdf_compounding_dist_n, pdf_compounding_quant_n, max_compounding_dist_n, cdfDemand, pdfDemand, cdfDemandmax, L_vr)
    Call Compound_Poisson_Empirical_VR(vr_index, pdf_compounding_dist, pdf_compounding_quant, max_compounding_dist, cdfDemand_vr, pdfDemand_vr, cdfDemandmax_vr, L_vr)
    '        ElseIf Leadtime_choice >= 3 Then
    '             Stop
                'Not implemented yet
    '        End If
End If
   
'Step 4: Calculation of the inventory level distribution for the virtual retailer in isolation P(IL_vr=j)for R_vr=0, in the vector Prob_dist_IL_r0, assuming the leadtime L_vr

Dim halt_search As Boolean

    R = 0
    min_IL_r0 = R + 1 - cdfDemandmax
    max_IL_r0 = R + Q
    
    ReDim Prob_dist_IL_r0(min_IL_r0 To max_IL_r0)
    ReDim cdf_IL_r0(min_IL_r0 To max_IL_r0)
    
'*****************Calculation of pdf and cdf of IL given R=0*********************************
    sum_check = 0
    For IL = min_IL_r0 To max_IL_r0
        
        If IL < R + 1 Then
            k_min = R + 1
        Else
            k_min = IL
        End If
        
        sum_prob = 0
        For k = k_min To (R + Q)
            If (k - IL) <= cdfDemandmax Then
                sum_prob = sum_prob + pdfDemand((k - IL)) / Q
            Else
                sum_prob = sum_prob + 0
            End If
        Next
        
        If k_min > IL Then
            If (R + Q - IL) <= cdfDemandmax Then
                slask = (cdfDemand((R + Q - IL)) - cdfDemand((k_min - 1 - IL))) / Q
            Else
                slask = (1 - cdfDemand((k_min - 1 - IL))) / Q
            End If
        ElseIf k_min = IL Then
            If (R + Q - IL) <= cdfDemandmax Then
                slask = cdfDemand((R + Q - IL)) / Q
            Else
                slask = 1 / Q
            End If
        End If
        
        If (Abs(slask - sum_prob) > 1E-10) Then
            Stop
        End If
        
        Prob_dist_IL_r0(IL) = sum_prob
        sum_check = sum_check + sum_prob
        cdf_IL_r0(IL) = sum_check
    Next
    
    'Stop
    If 1 = 0 Then 'Testutskrift
        k = 2
        For IL = min_IL_r0 To max_IL_r0
            k = k + 1
            test.Cells(k, 8) = IL
            test.Cells(k, 9) = Prob_dist_IL_r0(IL)
        Next
    End If
    
   
    
    If sum_check < 0.9999999999 Then
        Stop    'Investigate
    End If

'Step 5: Estimation of the combined CW_VR inventory level distribution for R_vr




'************Finding optimal R***********************************************************
    halt_search = False
    
    'Stop
    
    R = -Q - 1
    Do While halt_search = False
        R = R + 1
        
        'Stop
       
        Call IL_VR_CW_prob_dist(R, Q, R0, Q0, pdf_IL0, prob_IL0_positive, Prob_dist_IL_r0, min_IL_r0, min_IL_vr_CW, max_IL_VR_CW, cdf_IL_vr_CW, Prob_dist_IL_vr_CW)
        Call Fillrate_calc_VR_CW(max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, min_IL_vr_CW, max_IL_VR_CW, Prob_dist_IL_vr_CW, Fillrate_R)
        
        If Fillrate_R >= Target_Fillrate_i Then
            halt_search = True
        End If

        Ready_rate_R = (1 - cdf_IL_vr_CW(0))
        
    Loop
    
    R_opt = R
    
    If ((Fillrate_R - Ready_rate_R) > 1E-11) Then
        Stop    'This should not be possible - INVESTIGATE
    End If
    
'************Calculation of C_opt**************************
    
If 1 = 0 Then 'Val av s‰tt att ber‰kna kostnaderna Om 1=1 ber‰knas som separate stock annars ber‰knas genom kombinerade lagernivÂn CW_VR

    R = 0
    min_IL_r0 = R + 1 - cdfDemandmax_L_i
    max_IL_r0 = R + Q
    ReDim Prob_dist_IL_r0(min_IL_r0 To max_IL_r0)
    ReDim cdf_IL_r0(min_IL_r0 To max_IL_r0)
'*****************Calculation of pdf and cdf of IL given R=0 and leadtime L*********************************
    sum_check = 0
    For IL = min_IL_r0 To max_IL_r0
        
        If IL < R + 1 Then
            k_min = R + 1
        Else
            k_min = IL
        End If
        
        sum_prob = 0
        For k = k_min To (R + Q)
            If (k - IL) <= cdfDemandmax_L_i Then
                sum_prob = sum_prob + pdfDemand_L_i((k - IL)) / Q
            Else
                sum_prob = sum_prob + 0
            End If
        Next
        
        If k_min > IL Then
            If (R + Q - IL) <= cdfDemandmax_L_i Then
                slask = (cdfDemand_L_i((R + Q - IL)) - cdfDemand_L_i((k_min - 1 - IL))) / Q
            Else
                slask = (1 - cdfDemand_L_i((k_min - 1 - IL))) / Q
            End If
        ElseIf k_min = IL Then
            If (R + Q - IL) <= cdfDemandmax_L_i Then
                slask = cdfDemand_L_i((R + Q - IL)) / Q
            Else
                slask = 1 / Q
            End If
        End If
        
        If (Abs(slask - sum_prob) > 1E-10) Then
            Stop
        End If
        
        Prob_dist_IL_r0(IL) = sum_prob
        sum_check = sum_check + sum_prob
        cdf_IL_r0(IL) = sum_check
    Next
    
    'Stop
    If 1 = 0 Then 'Testutskrift
        k = 2
        For IL = min_IL_r0 To max_IL_r0
            k = k + 1
            test.Cells(k, 8) = IL
            test.Cells(k, 9) = Prob_dist_IL_r0(IL)
        Next
    End If
    
    If sum_check < 0.9999999999 Then
        Stop    'Investigate
    End If
    
    Expected_IL_plus = 0
    For IL = 1 To (R_opt + Q)
        Expected_IL_plus = Expected_IL_plus + IL * Prob_dist_IL_r0((IL - R_opt))
    Next
    
    temp1 = -1 * p * (R_opt + ((Q + 1) / 2) - my * L)
    temp2 = (h + p) * Expected_IL_plus
    
    C_opt = temp1 + temp2
        
    C_opt = C_opt + cost0(R0, Q0, h0, pdfN)
    
Else

    Expected_IL_plus = 0
    Expected_IL_minus = 0
    For IL = 1 To max_IL_VR_CW
        Expected_IL_plus = Expected_IL_plus + IL * Prob_dist_IL_vr_CW((IL))
    Next
    For IL = min_IL_vr_CW To -1
        Expected_IL_minus = Expected_IL_minus + (-1 * IL) * Prob_dist_IL_vr_CW((IL))
    Next
    
    C_opt = h * Expected_IL_plus + p * Expected_IL_minus
    
    'OBS!!! C_opt includes the holding cost of both the reserved VR stock and the unreserved CW stock
    'h=h0=h_vr
    
End If
    
    

End Sub
Sub Calc_R_opt_backorder_cost_model_compound_Poisson_VR_CW(vr_index, R0, Q0, trp, h, p, Target_Fillrate_i, max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, cdfDemand_L_i, pdfDemand_L_i, cdfDemandmax_L_i, my, sigma, Q, L, R_opt, Fillrate_R, Ready_rate_R, C_opt)

Dim R, R_plus_one, i, j, k, IL, k_min, R_comb, max_IL0, min_IL0 As Long
Dim slask, B0_temp, sum_prob, sumprob, sum_check, Prob_dist_IL_r0(), Prob_dist_IL(), cdf_IL_r0(), min_IL_r0, max_IL_r0 As Double
Dim Ratio_h_p, Expected_IL_plus, Expected_IL_minus, temp1, temp2 As Double
Dim pdf_IL0(), pdf_IL_CW_VR(), prob_IL0_positive, Prob_IL0_zero_or_less As Double
Dim L_vr, cdfDemand(), pdfDemand(), cdfDemand_vr(), pdfDemand_vr(), cdf_IL_vr_CW(), Prob_dist_IL_vr_CW(), CW_demand_rate As Double
Dim cdf_IL_vr_CW_temp(), Prob_dist_IL_vr_CW_temp()
Dim L_min, L_max As Double
Dim cdfDemandmax, cdfDemandmax_vr, min_IL_vr_CW, max_IL_VR_CW, min_IL_vr_CW_temp, max_IL_VR_CW_temp As Long
Dim Fillrate_CW_VRdemand, ready_rate_R_plus_one As Double

'Dim k_max As Long

Dim test As Worksheet
Set test = Worksheets("testutskrifter")

'Step 1: Calculating the pdf of the inventory level distribution at the CW P(IL0=j) for j>=0
max_IL0 = R0 + Q0
min_IL0 = R0 + 1 - pdfN_kmax
ReDim pdf_IL0(min_IL0 To max_IL0)

    prob_IL0_positive = 0
    For j = 1 To max_IL0
        sumprob = 0
        If j < R0 + 1 Then
            k_min = R0 + 1
        Else
            k_min = j
        End If
        
        For k = k_min To max_IL0   'max_IL0=(R0 + Q0)
            If (k - j) <= pdfN_kmax Then
                sumprob = sumprob + pdfN(k - j) / Q0
            Else 'IL0=j cannot be reached from IP0=k as (k-j)> max leadtime demand pdfN_kmax
                'Stop
            End If
        Next
        pdf_IL0(j) = sumprob
        prob_IL0_positive = prob_IL0_positive + sumprob
    Next
    Prob_IL0_zero_or_less = 1 - prob_IL0_positive

If 1 = 1 Then
'The probabilities for backorders at the CW P(IL0=j) not really needed here but computed for verification reasons
    slask = 0
    For j = min_IL0 To 0
        sumprob = 0
        If j < R0 + 1 Then
            k_min = R0 + 1
        Else
            k_min = j
        End If
    
        For k = k_min To max_IL0   'max_IL0=(R0 + Q0)
            If (k - j) <= pdfN_kmax Then
                sumprob = sumprob + pdfN(k - j) / Q0
            Else 'IL0=j cannot be reached from IP0=k as (k-j)> max leadtime demand pdfN_kmax
            End If
        Next
        pdf_IL0(j) = sumprob
        slask = slask + sumprob
    Next
    sum_check = 0
    B0_temp = 0
    For j = min_IL0 To -1
        sum_check = sum_check + (-j * pdf_IL0(j))
        B0_temp = B0_temp + (-j) * (pdf_IL0(j) / Prob_IL0_zero_or_less)
    Next
    
    If Abs(slask - Prob_IL0_zero_or_less) > (epsilon * 1000) Then
        Stop
    End If
   '
    If Abs(slask + prob_IL0_positive - 1) > (epsilon * 1000) Then
        Stop
    End If
End If

'Step 2: Estimation of the expected CW delay for the Virtual retailer when the CW is out of stock, i.e., IL0<=0
    'choice_L_vr = 1
    If choice_L_vr = 0 Then 'Standard Metric approx i.e., LM
        L_vr = L
        cdfDemandmax = cdfDemandmax_L_i
        ReDim pdfDemand(0 To cdfDemandmax)
        ReDim cdfDemand(0 To cdfDemandmax)
        For j = 0 To cdfDemandmax_L_i
            cdfDemand(j) = cdfDemand_L_i(j)
            pdfDemand(j) = pdfDemand_L_i(j)
        Next
    ElseIf choice_L_vr = 1 Then 'estimating L_vr as a two point distribution with mean according to Metric i.e., L
        L_min = trp_n(vr_index)
        If R0 >= 0 Then
            L_max = L0
        Else
            Stop    'not thoroughly verified yet
            For i = 1 To N
                CW_demand_rate = CW_demand_rate + m_n(i)
            Next
            L_max = L0 - (R0 / CW_demand_rate)
        End If
        Call Comp_Poisson_Empirical_Li_2_point_dist(vr_index, pdf_compounding_dist, pdf_compounding_quant, max_compounding_dist, cdfDemand, pdfDemand, cdfDemandmax, L, L_min, L_max)
    ElseIf choice_L_vr = 2 Then  'adjusted mean as the average delay for units that are delayed
        Call Fillrate_calc_VR_CW(max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, 1, max_IL0, pdf_IL0, Fillrate_CW_VRdemand)
        'Logic: L=0*Fillrate_CW_VRdemand+L_vr*(1-Fillrate_CW_VRdemand)  =>  L_vr=L/(1-Fillrate_CW_VRdemand
        'Fillrate_CW_VRdemand = proportion of VR demand that can be satisfied directly from CW stock ,i.e., with leadtime=0
        '1-Fillrate_CW_VRdemand = proportion of VR demand experiencing a leadtime >0 with average L_vr
        L_vr = L / (1 - Fillrate_CW_VRdemand)
        Call Compound_Poisson_Empirical_VR(vr_index, pdf_compounding_dist, pdf_compounding_quant, max_compounding_dist, cdfDemand, pdfDemand, cdfDemandmax, L_vr)
    End If
    
If 1 = 0 Then 'testutskrift
    For j = 0 To cdfDemandmax
        test.Cells(3 + j, 25) = j
        test.Cells(3 + j, 26) = pdfDemand(j)
    Next
    For j = 0 To cdfDemandmax_L_i
         test.Cells(3 + j, 27) = j
         test.Cells(3 + j, 28) = pdfDemand_L_i(j)
    Next
End If
       
If 1 = 0 Then 'with L_vr=L we can use the previously calculated lead-time demand distributions cdfDemand, pdfDemand

'Step 3: Calculation of the demand for the virtual retailer conditioned on L_vr, i.e. the CW being out of stock
    If Leadtime_choice > 0 Then
        Stop    'Only Metric approx implemented so far
    End If
    
    'If Ret_demand_choice = 1 Then 'Neg binomial
    '       If Leadtime_choice <= 2 Then
    '            Call NegBin_prob(cdfDemand_n, pdfDemand_n, cdfDemandmax_n, L_n)
    '        ElseIf Leadtime_choice >= 3 Then
    '            Call NegBin_prob_Li_demand(CW_demand_choice, R0_opt, Q0, cdfDemand_n, pdfDemand_n, cdfDemandmax_n, delay_n)
    '        End If
        
    '    ElseIf Ret_demand_choice = 2 Or Ret_demand_choice = 3 Then 'Poisson or Geometric Compound Poisson
    '        If Leadtime_choice <= 2 Then
    '            Call Compound_Poisson_geometric_prob(cdfDemand_n, pdfDemand_n, cdfDemandmax_n, L_n)
    '        ElseIf Leadtime_choice >= 3 Then
    '            Call Compound_Poisson_geometric_prob_Li_demand(CW_demand_choice, R0_opt, Q0, cdfDemand_n, pdfDemand_n, cdfDemandmax_n, delay_n)
    '        End If
        
    '    ElseIf Ret_demand_choice >= 3 Then 'Poisson and Compound Poisson
    '        If Leadtime_choice <= 2 Then
    '           call Compound_Poisson_Empirical_VR(j_vr, pdf_compounding_dist_n, pdf_compounding_quant_n, max_compounding_dist_n, cdfDemand, pdfDemand, cdfDemandmax, L_vr)
    Call Compound_Poisson_Empirical_VR(vr_index, pdf_compounding_dist, pdf_compounding_quant, max_compounding_dist, cdfDemand_vr, pdfDemand_vr, cdfDemandmax_vr, L_vr)
    '        ElseIf Leadtime_choice >= 3 Then
    '             Stop
                'Not implemented yet
    '        End If
End If
   
'Step 4: Calculation of the inventory level distribution for the virtual retailer in isolation P(IL_vr=j)for R_vr=0, in the vector Prob_dist_IL_r0, assuming the leadtime L_vr

Dim halt_search As Boolean

    R = 0
    min_IL_r0 = R + 1 - cdfDemandmax
    max_IL_r0 = R + Q
    
    ReDim Prob_dist_IL_r0(min_IL_r0 To max_IL_r0)
    ReDim cdf_IL_r0(min_IL_r0 To max_IL_r0)
    
'*****************Calculation of pdf and cdf of IL given R=0*********************************
    sum_check = 0
    For IL = min_IL_r0 To max_IL_r0
        
        If IL < R + 1 Then
            k_min = R + 1
        Else
            k_min = IL
        End If
        
        sum_prob = 0
        For k = k_min To (R + Q)
            If (k - IL) <= cdfDemandmax Then
                sum_prob = sum_prob + pdfDemand((k - IL)) / Q
            Else
                sum_prob = sum_prob + 0
            End If
        Next
        
        If k_min > IL Then
            If (R + Q - IL) <= cdfDemandmax Then
                slask = (cdfDemand((R + Q - IL)) - cdfDemand((k_min - 1 - IL))) / Q
            Else
                slask = (1 - cdfDemand((k_min - 1 - IL))) / Q
            End If
        ElseIf k_min = IL Then
            If (R + Q - IL) <= cdfDemandmax Then
                slask = cdfDemand((R + Q - IL)) / Q
            Else
                slask = 1 / Q
            End If
        End If
        
        If (Abs(slask - sum_prob) > 1E-10) Then
            Stop
        End If
        
        Prob_dist_IL_r0(IL) = sum_prob
        sum_check = sum_check + sum_prob
        cdf_IL_r0(IL) = sum_check
    Next
    
    'Stop
    If 1 = 0 Then 'Testutskrift
        k = 2
        For IL = min_IL_r0 To max_IL_r0
            k = k + 1
            test.Cells(k, 8) = IL
            test.Cells(k, 9) = Prob_dist_IL_r0(IL)
        Next
    End If
    
   
    
    If sum_check < 0.9999999999 Then
        Stop    'Investigate
    End If

'Step 5: Estimation of the combined CW_VR inventory level distribution for R_vr




'************Finding optimal R***********************************************************
    halt_search = False
    
    'Stop
    
    R = -Q - 1
    Do While halt_search = False
        R = R + 1
        
        'Stop
        
        If R = -Q Then
          Call IL_VR_CW_prob_dist(R, Q, R0, Q0, pdf_IL0, prob_IL0_positive, Prob_dist_IL_r0, min_IL_r0, min_IL_vr_CW, max_IL_VR_CW, cdf_IL_vr_CW, Prob_dist_IL_vr_CW)
        Else
          min_IL_vr_CW = min_IL_vr_CW_temp
          max_IL_VR_CW = max_IL_VR_CW_temp
          
          ReDim Prob_dist_IL_vr_CW(min_IL_vr_CW To max_IL_VR_CW)
          ReDim cdf_IL_vr_CW(min_IL_vr_CW To max_IL_VR_CW)
          
          For j = min_IL_vr_CW To max_IL_VR_CW
            Prob_dist_IL_vr_CW(j) = Prob_dist_IL_vr_CW_temp(j)
            cdf_IL_vr_CW(j) = cdf_IL_vr_CW_temp(j)
          Next
        End If
        
        
        'Call Fillrate_calc_VR_CW(max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, min_IL_vr_CW, max_IL_VR_CW, Prob_dist_IL_vr_CW, Fillrate_R)
        
        'If Fillrate_R >= Target_Fillrate_i Then
        '    halt_search = True
        'End If
        
        If (R = -Q) Then
            Ready_rate_R = 0    'Need this for numerical reasons when p=0
        Else
            Ready_rate_R = (1 - cdf_IL_vr_CW(0))
        End If
        
        R_plus_one = R + 1
        Call IL_VR_CW_prob_dist(R_plus_one, Q, R0, Q0, pdf_IL0, prob_IL0_positive, Prob_dist_IL_r0, min_IL_r0, min_IL_vr_CW_temp, max_IL_VR_CW_temp, cdf_IL_vr_CW_temp, Prob_dist_IL_vr_CW_temp)
        
        ready_rate_R_plus_one = (1 - cdf_IL_vr_CW_temp(0))
        
        Ratio_h_p = (p / (h + p))
        
        If (Ready_rate_R <= Ratio_h_p) And (ready_rate_R_plus_one > Ratio_h_p) Then
            halt_search = True
        End If
        
    Loop
    
    R_opt = R
    
    Call Fillrate_calc_VR_CW(max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, min_IL_vr_CW, max_IL_VR_CW, Prob_dist_IL_vr_CW, Fillrate_R)
    If ((Fillrate_R - Ready_rate_R) > 1E-11) Then
        Stop    'This should not be possible - INVESTIGATE
    End If
    
'************Calculation of C_opt**************************
'    R = 0
'    min_IL_r0 = R + 1 - cdfDemandmax_L_i
'    max_IL_r0 = R + Q
'    ReDim Prob_dist_IL_r0(min_IL_r0 To max_IL_r0)
'    ReDim cdf_IL_r0(min_IL_r0 To max_IL_r0)
'*****************Calculation of pdf and cdf of IL given R=0 and leadtime L*********************************
'    sum_check = 0
'    For IL = min_IL_r0 To max_IL_r0
'
'        If IL < R + 1 Then
'            k_min = R + 1
'        Else
'            k_min = IL
'        End If
'
'        sum_prob = 0
'        For k = k_min To (R + Q)
'            If (k - IL) <= cdfDemandmax_L_i Then
'                sum_prob = sum_prob + pdfDemand_L_i((k - IL)) / Q
'            Else
'                sum_prob = sum_prob + 0
'            End If
'        Next
'
'        If k_min > IL Then
'            If (R + Q - IL) <= cdfDemandmax_L_i Then
'                slask = (cdfDemand_L_i((R + Q - IL)) - cdfDemand_L_i((k_min - 1 - IL))) / Q
'            Else
'                slask = (1 - cdfDemand_L_i((k_min - 1 - IL))) / Q
'            End If
'        ElseIf k_min = IL Then
'            If (R + Q - IL) <= cdfDemandmax_L_i Then
'                slask = cdfDemand_L_i((R + Q - IL)) / Q
'            Else
'                slask = 1 / Q
'            End If
'        End If
'
'        If (Abs(slask - sum_prob) > 0.0000000001) Then
'            Stop
'        End If
'
'        Prob_dist_IL_r0(IL) = sum_prob
'        sum_check = sum_check + sum_prob
'        cdf_IL_r0(IL) = sum_check
'    Next
'
'    'Stop
'    If 1 = 0 Then 'Testutskrift
'        k = 2
'        For IL = min_IL_r0 To max_IL_r0
'            k = k + 1
'            test.Cells(k, 8) = IL
'            test.Cells(k, 9) = Prob_dist_IL_r0(IL)
'        Next
'    End If
'
'    If sum_check < 0.9999999999 Then
'        Stop    'Investigate
'    End If
    
    Expected_IL_plus = 0
    Expected_IL_minus = 0
    For IL = 1 To max_IL_VR_CW
        Expected_IL_plus = Expected_IL_plus + IL * Prob_dist_IL_vr_CW((IL))
    Next
    For IL = min_IL_vr_CW To -1
        Expected_IL_minus = Expected_IL_minus + (-1 * IL) * Prob_dist_IL_vr_CW((IL))
    Next
    
    C_opt = h * Expected_IL_plus + p * Expected_IL_minus
    
    'OBS!!! C_opt includes the holding cost of both the reserved VR stock and the unreserved CW stock
    'h=h0=h_vr
    

End Sub


Sub Calc_R_opt_Fillrate_constraint_Normal_VR_CW(vr_index, R0, Q0, trp, h, p, Target_Fillrate_i, max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, cdfDemand_L_i, pdfDemand_L_i, cdfDemandmax_L_i, my, sigma, Q, L, R_opt, Fillrate_R, Ready_rate_R)

Dim R, R_plus_one, i, j, k, IL, k_min, R_comb, max_IL0, min_IL0 As Long
Dim slask, B0_temp, sum_prob, sumprob, sum_check, Prob_dist_IL() As Double
Dim Ratio_h_p, Expected_IL_plus, temp1, temp2 As Double
Dim pdf_IL0(), pdf_IL_CW_VR(), prob_IL0_positive, Prob_IL0_zero_or_less As Double
Dim L_vr, cdfDemand(), pdfDemand(), cdfDemand_vr(), pdfDemand_vr(), cdf_IL_vr_CW(), Prob_dist_IL_vr_CW(), CW_demand_rate As Double
Dim L_min, L_max, pdf_undershoot(), my_Lvr, sigma_Lvr, arg1, arg2, cdf_IL_vr_zero_u As Double
Dim cdfDemandmax, cdfDemandmax_vr, min_IL_vr_CW, max_IL_VR_CW, max_undershoot, u As Long
Dim Fillrate_CW_VRdemand, ReadyRate_CW_VRdemand As Double

'Dim k_max As Long

Dim test As Worksheet
Set test = Worksheets("testutskrifter")

'Step 1: Calculating the pdf of the inventory level distribution at the CW P(IL0=j) for j>=0
max_IL0 = R0 + Q0
min_IL0 = R0 + Q0 - pdfN_kmax
ReDim pdf_IL0(min_IL0 To max_IL0)

    prob_IL0_positive = 0
    For j = 1 To max_IL0
        sumprob = 0
        If j < R0 + 1 Then
            k_min = R0 + 1
        Else
            k_min = j
        End If
        
        For k = k_min To max_IL0   'max_IL0=(R0 + Q0)
            sumprob = sumprob + pdfN(k - j) / Q0
        Next
        pdf_IL0(j) = sumprob
        prob_IL0_positive = prob_IL0_positive + sumprob
    Next
    Prob_IL0_zero_or_less = 1 - prob_IL0_positive

If 1 = 0 Then
'The probabilities for backorders at the CW P(IL0=j) not really needed here but computed for verification reasons
    'Stop
    slask = 0
    For j = min_IL0 To 0
        sumprob = 0
        If j < R0 + 1 Then
            k_min = R0 + 1
        Else
            k_min = j
        End If
    
        For k = k_min To max_IL0   'max_IL0=(R0 + Q0)
            sumprob = sumprob + pdfN((k - j)) / Q0
        Next
        pdf_IL0(j) = sumprob
        slask = slask + sumprob
    Next
    sum_check = 0
    B0_temp = 0
    For j = min_IL0 To -1
        sum_check = sum_check + (-j * pdf_IL0(j))
        B0_temp = B0_temp + (-j) * (pdf_IL0(j) / Prob_IL0_zero_or_less)
    Next
    
    If Abs(slask - Prob_IL0_zero_or_less) > (epsilon * 1000) Then
        Stop
    End If
   
    If Abs(slask + prob_IL0_positive - 1) > (epsilon * 1000) Then
        Stop
    End If
End If
    
'Step 2: Estimation of the expected CW delay for the Virtual retailer when the CW is out of stock, i.e., IL0<=0
    
    If choice_L_vr = 2 Or choice_L_vr = 3 Then 'adjusted mean as the average delay for units that are delayed
        'This is the preferred choice with superior performance
      If choice_L_vr = 2 Then
        Call Fillrate_calc_VR_CW(max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, 1, max_IL0, pdf_IL0, Fillrate_CW_VRdemand)
        'Logic: L=0*Fillrate_CW_VRdemand+L_vr*(1-Fillrate_CW_VRdemand)  =>  L_vr=L/(1-Fillrate_CW_VRdemand
        'Fillrate_CW_VRdemand = proportion of VR demand that can be satisfied directly from CW stock ,i.e., with leadtime=0
        '1-Fillrate_CW_VRdemand = proportion of VR demand experiencing a leadtime >0 with average L_vr
        L_vr = L / (1 - Fillrate_CW_VRdemand)
      Else 'choice_L_vr=3
        'Logic: L=0*ReadyRate_CW_VRdemand+L_vr*(1-Readyrate_CW_VRdemand)  =>  L_vr=L/(1-ReadyRate_CW_VRdemand
        'Readyrate_CW_VRdemand = proportion of timw that VR demand that can be satisfied directly from CW stock ,i.e., with leadtime=0
        '1-ReadyRate_CW_VRdemand = proportion of time VR demand experiencing a leadtime >0 with average L_vr
        ReadyRate_CW_VRdemand = prob_IL0_positive
        L_vr = L / (1 - ReadyRate_CW_VRdemand)
      End If
    Else
        Stop 'Other alternatives not implemented for normal demand
    End If
       

   
'Step 3: Calculation of R_opt=S-1 for which the fillrate target for the VR is satisfied

    Dim halt_search As Boolean
    
    my_Lvr = my * L_vr
    sigma_Lvr = sigma * Sqr(L_vr)
    
    Call undershoot_dist_empirical_VR(vr_index, max_compounding_dist_n, R0, Q0, pdf_IL0, pdf_undershoot, max_undershoot)
    
    halt_search = False
    R = -Q - 1
    If Target_Fillrate_i > 0 Then
        R = -Q  'Q=1 as an (S-1,S)policy is used at VR
    Else
        R = -Q - 1
    End If
    
    Do While halt_search = False
        R = R + 1
        
        sumprob = 0
        For u = 0 To max_undershoot
            arg1 = (R - u - my_Lvr) / sigma_Lvr
            arg2 = (R + 1 - u - my_Lvr) / sigma_Lvr
            cdf_IL_vr_zero_u = sigma_Lvr * (g(arg1) - g(arg2))
            sumprob = sumprob + pdf_undershoot(u) * cdf_IL_vr_zero_u
        Next
        
        Fillrate_R = 1 - (1 - prob_IL0_positive) * sumprob
    
        If Fillrate_R >= Target_Fillrate_i Then
            halt_search = True
            R_opt = R
        End If

        Ready_rate_R = Fillrate_R
    Loop
    

End Sub
Sub Calc_R_opt_Fillrate_constraint_Normal_VR_CW_U2(vr_index, R0, Q0, trp, h, p, Target_Fillrate_i, max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, cdfDemand_L_i, pdfDemand_L_i, cdfDemandmax_L_i, my, sigma, Q, L, R_opt, Fillrate_R, Ready_rate_R)

Dim R, R_plus_one, i, j, k, IL, k_min, R_comb, max_IL0, min_IL0 As Long
Dim slask, B0_temp, sum_prob, sumprob, sum_check, Prob_dist_IL() As Double
Dim Ratio_h_p, Expected_IL_plus, temp1, temp2 As Double
Dim pdf_IL0(), pdf_IL_CW_VR(), prob_IL0_positive, Prob_IL0_zero_or_less As Double
Dim L_vr, cdfDemand(), pdfDemand(), cdfDemand_vr(), pdfDemand_vr(), cdf_IL_vr_CW(), Prob_dist_IL_vr_CW(), CW_demand_rate As Double
Dim L_min, L_max, pdf_undershoot(), my_Lvr, sigma_Lvr, arg1, arg2, cdf_IL_vr_zero_u As Double
Dim cdfDemandmax, cdfDemandmax_vr, min_IL_vr_CW, max_IL_VR_CW, max_undershoot, u As Long
Dim Fillrate_CW_VRdemand, mean_u, sigma_u, var_u As Double

'Dim k_max As Long

Dim test As Worksheet
Set test = Worksheets("testutskrifter")

'Step 1: Calculating the pdf of the inventory level distribution at the CW P(IL0=j) for j>=0
max_IL0 = R0 + Q0
min_IL0 = R0 + Q0 - pdfN_kmax
ReDim pdf_IL0(min_IL0 To max_IL0)

    prob_IL0_positive = 0
    For j = 1 To max_IL0
        sumprob = 0
        If j < R0 + 1 Then
            k_min = R0 + 1
        Else
            k_min = j
        End If
        
        For k = k_min To max_IL0   'max_IL0=(R0 + Q0)
            sumprob = sumprob + pdfN(k - j) / Q0
        Next
        pdf_IL0(j) = sumprob
        prob_IL0_positive = prob_IL0_positive + sumprob
    Next
    Prob_IL0_zero_or_less = 1 - prob_IL0_positive

If 1 = 0 Then
'The probabilities for backorders at the CW P(IL0=j) not really needed here but computed for verification reasons
    'Stop
    slask = 0
    For j = min_IL0 To 0
        sumprob = 0
        If j < R0 + 1 Then
            k_min = R0 + 1
        Else
            k_min = j
        End If
    
        For k = k_min To max_IL0   'max_IL0=(R0 + Q0)
            sumprob = sumprob + pdfN((k - j)) / Q0
        Next
        pdf_IL0(j) = sumprob
        slask = slask + sumprob
    Next
    sum_check = 0
    B0_temp = 0
    For j = min_IL0 To -1
        sum_check = sum_check + (-j * pdf_IL0(j))
        B0_temp = B0_temp + (-j) * (pdf_IL0(j) / Prob_IL0_zero_or_less)
    Next
    
    If Abs(slask - Prob_IL0_zero_or_less) > (epsilon * 1000) Then
        Stop
    End If
   
    If Abs(slask + prob_IL0_positive - 1) > (epsilon * 1000) Then
        Stop
    End If
End If
    
'Step 2: Estimation of the expected CW delay for the Virtual retailer when the CW is out of stock, i.e., IL0<=0
    
    If choice_L_vr = 2 Then  'adjusted mean as the average delay for units that are delayed
        'This is the preferred choice with superior performance
        Call Fillrate_calc_VR_CW(max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, 1, max_IL0, pdf_IL0, Fillrate_CW_VRdemand)
        'Logic: L=0*Fillrate_CW_VRdemand+L_vr*(1-Fillrate_CW_VRdemand)  =>  L_vr=L/(1-Fillrate_CW_VRdemand
        'Fillrate_CW_VRdemand = proportion of VR demand that can be satisfied directly from CW stock ,i.e., with leadtime=0
        '1-Fillrate_CW_VRdemand = proportion of VR demand experiencing a leadtime >0 with average L_vr
        L_vr = L / (1 - Fillrate_CW_VRdemand)
    Else
        Stop 'Other alternatives not implemented for normal demand
    End If
       

   
'Step 3: Calculation of R_opt=S-1 for which the fillrate target for the VR is satisfied

    Dim halt_search As Boolean
    
    my_Lvr = my * L_vr
    sigma_Lvr = sigma * Sqr(L_vr)
    
    Call undershoot_dist_empirical_VR(vr_index, max_compounding_dist_n, R0, Q0, pdf_IL0, pdf_undershoot, max_undershoot)
    
    mean_u = 0
    For u = 1 To max_undershoot
        mean_u = mean_u + u * pdf_undershoot(u)
    Next
    var_u = 0
    For u = 0 To max_undershoot
        var_u = var_u + ((mean_u - u) ^ 2) * pdf_undershoot(u)
    Next
    sigma_u = Sqr(var_u)
    
    'Update mean and variance of the leadtime demand with the mean and variance of the undershoot
If 1 = 1 Then
    my_Lvr = my_Lvr + mean_u
    sigma_Lvr = Sqr((sigma_Lvr ^ 2) + var_u)
End If
    
    halt_search = False
    R = -Q - 1
    If Target_Fillrate_i > 0 Then
        R = -Q  'Q=1 as an (S-1,S)policy is used at VR
    Else
        R = -Q - 1
    End If
    
    Do While halt_search = False
        R = R + 1
        
        arg1 = (R - my_Lvr) / sigma_Lvr
        arg2 = (R + 1 - my_Lvr) / sigma_Lvr
        cdf_IL_vr_zero_u = sigma_Lvr * (g(arg1) - g(arg2))
        
        Fillrate_R = 1 - (1 - prob_IL0_positive) * cdf_IL_vr_zero_u
    
        If Fillrate_R >= Target_Fillrate_i Then
            halt_search = True
            R_opt = R
        End If

        Ready_rate_R = Fillrate_R
    Loop
    

End Sub
Sub Calc_R_opt_Fillrate_constraint_Normal_VR_CW_U3(vr_index, R0, Q0, trp, h, p, Target_Fillrate_i, max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, cdfDemand_L_i, pdfDemand_L_i, cdfDemandmax_L_i, my, sigma, Q, L, R_opt, Fillrate_R, Ready_rate_R)

Dim R, R_plus_one, i, j, k, IL, k_min, R_comb, max_IL0, min_IL0 As Long
Dim slask, B0_temp, sum_prob, sumprob, sum_check, Prob_dist_IL() As Double
Dim Ratio_h_p, Expected_IL_plus, temp1, temp2 As Double
Dim pdf_IL0(), pdf_IL_CW_VR(), prob_IL0_positive, Prob_IL0_zero_or_less As Double
Dim L_vr, cdfDemand(), pdfDemand(), cdfDemand_vr(), pdfDemand_vr(), cdf_IL_vr_CW(), Prob_dist_IL_vr_CW(), CW_demand_rate As Double
Dim L_min, L_max, pdf_undershoot(), my_Lvr, sigma_Lvr, arg1, arg2, cdf_IL_vr_zero_u As Double
Dim cdfDemandmax, cdfDemandmax_vr, min_IL_vr_CW, max_IL_VR_CW, max_undershoot, u As Long
Dim Fillrate_CW_VRdemand, mean_u, sigma_u, var_u As Double
Dim min_IL_r0, max_IL_r0 As Long

'Dim k_max As Long

Dim test As Worksheet
Set test = Worksheets("testutskrifter")

'Step 1: Calculating the pdf of the inventory level distribution at the CW P(IL0=j) for j>=0
max_IL0 = R0 + Q0
min_IL0 = R0 + Q0 - pdfN_kmax
ReDim pdf_IL0(min_IL0 To max_IL0)

    prob_IL0_positive = 0
    For j = 1 To max_IL0
        sumprob = 0
        If j < R0 + 1 Then
            k_min = R0 + 1
        Else
            k_min = j
        End If
        
        For k = k_min To max_IL0   'max_IL0=(R0 + Q0)
            sumprob = sumprob + pdfN(k - j) / Q0
        Next
        pdf_IL0(j) = sumprob
        prob_IL0_positive = prob_IL0_positive + sumprob
    Next
    Prob_IL0_zero_or_less = 1 - prob_IL0_positive

If 1 = 1 Then
'The probabilities for backorders at the CW P(IL0=j) not really needed here but computed for verification reasons
    'Stop
    slask = 0
    For j = min_IL0 To 0
        sumprob = 0
        If j < R0 + 1 Then
            k_min = R0 + 1
        Else
            k_min = j
        End If
    
        For k = k_min To max_IL0   'max_IL0=(R0 + Q0)
            sumprob = sumprob + pdfN((k - j)) / Q0
        Next
        pdf_IL0(j) = sumprob
        slask = slask + sumprob
    Next
    sum_check = 0
    B0_temp = 0
    For j = min_IL0 To -1
        sum_check = sum_check + (-j * pdf_IL0(j))
        B0_temp = B0_temp + (-j) * (pdf_IL0(j) / Prob_IL0_zero_or_less)
    Next
    
    If Abs(slask - Prob_IL0_zero_or_less) > (epsilon * 100) Then
        Stop
    End If
   
    If Abs(slask + prob_IL0_positive - 1) > (epsilon * 100) Then
        Stop
    End If
End If
    
'Step 2: Estimation of the expected CW delay for the Virtual retailer when the CW is out of stock, i.e., IL0<=0
    
    If choice_L_vr = 2 Then  'adjusted mean as the average delay for units that are delayed
        'This is the preferred choice with superior performance
        Call Fillrate_calc_VR_CW(max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, 1, max_IL0, pdf_IL0, Fillrate_CW_VRdemand)
        'Logic: L=0*Fillrate_CW_VRdemand+L_vr*(1-Fillrate_CW_VRdemand)  =>  L_vr=L/(1-Fillrate_CW_VRdemand
        'Fillrate_CW_VRdemand = proportion of VR demand that can be satisfied directly from CW stock ,i.e., with leadtime=0
        '1-Fillrate_CW_VRdemand = proportion of VR demand experiencing a leadtime >0 with average L_vr
        L_vr = L / (1 - Fillrate_CW_VRdemand)
    Else
        Stop 'Other alternatives not implemented for normal demand
    End If
       
    my_Lvr = my * L_vr
    sigma_Lvr = sigma * Sqr(L_vr)
       
If 1 = 0 Then 'Tested but leads to overseitmation of R so this option is NOT used
'Step 3: Calculation of the undershoot adjusted lead time demand D(L_vr)
    
    Call undershoot_dist_empirical_VR(vr_index, max_compounding_dist_n, R0, Q0, pdf_IL0, pdf_undershoot, max_undershoot)
    
    mean_u = 0
    For u = 1 To max_undershoot
        mean_u = mean_u + u * pdf_undershoot(u)
    Next
    var_u = 0
    For u = 0 To max_undershoot
        var_u = var_u + ((mean_u - u) ^ 2) * pdf_undershoot(u)
    Next
    sigma_u = Sqr(var_u)
    
    'Update mean and variance of the leadtime demand with the mean and variance of the undershoot

    my_Lvr = my_Lvr + mean_u
    sigma_Lvr = Sqr((sigma_Lvr ^ 2) + var_u)
End If
    
'Step 4: Calculation of the inventory level distribution for the virtual retailer in isolation P(IL_vr=j)for R_vr=0, in the vector Prob_dist_IL_r0, assuming the leadtime L_vr
    
    cdfDemandmax = Int(my_Lvr + 6 * sigma_Lvr)
    If (1 - Sfi(((cdfDemandmax - my_Lvr) / sigma_Lvr))) > 1E-06 Then
        Stop
    End If
    
    R = 0
    min_IL_r0 = R + 1 - cdfDemandmax
    max_IL_r0 = R + Q
    
    ReDim Prob_dist_IL_r0(min_IL_r0 To max_IL_r0)
    ReDim cdf_IL_r0(min_IL_r0 To max_IL_r0)
    
'*****************Calculation of pdf and cdf of IL given R=0*********************************
    sum_check = 0
    arg1 = ((R - (min_IL_r0 - 0.5)) - my_Lvr) / sigma_Lvr
    arg2 = ((R + Q - (min_IL_r0 - 0.5)) - my_Lvr) / sigma_Lvr
    temp1 = sigma_Lvr * (g(arg1) - g(arg2)) / Q
    
    For IL = min_IL_r0 To max_IL_r0
    
        If IL <= max_IL_r0 Then
            arg1 = ((R - (IL + 0.5)) - my_Lvr) / sigma_Lvr
            arg2 = ((R + Q - (IL + 0.5)) - my_Lvr) / sigma_Lvr
            temp2 = sigma_Lvr * (g(arg1) - g(arg2)) / Q
            
        Else 'IL=max_IL_r0
            arg1 = ((R - IL) - my_Lvr) / sigma_Lvr
            arg2 = ((R + Q - IL) - my_Lvr) / sigma_Lvr
            temp2 = sigma_Lvr * (g(arg1) - g(arg2)) / Q
        End If
        
        sum_prob = temp2 - temp1
        Prob_dist_IL_r0(IL) = sum_prob
        sum_check = sum_check + sum_prob
        cdf_IL_r0(IL) = sum_check
        
        'If IL = max_IL_r0 Then
        '    Stop
        'End If
        
        temp1 = temp2
    Next
    
If 1 = 0 Then
'**Normalizing Prob_dist_IL_r0 with slask=P(IL_vr<=R+Q)so that it sums to 1 even when probability for negative demand is high******
'***An option to be tested at least.....****************************
    slask = sum_check 'probability that IL<=R+Q may not equal 1 due to possibility for negative realization in Normal dist
    If 1 = 1 Then 'All probability associated with negative demand placed at P(IL_VR=max_IL_r0=R+Q
        Prob_dist_IL_r0(max_IL_r0) = Prob_dist_IL_r0(max_IL_r0) + (1 - slask)
        sum_check = sum_check + (1 - slask)
    Else 'probability associated with negative demand spread evenly accross all realizations IL<=max_IL_r0
        sum_check = 0
        For IL = min_IL_r0 To max_IL_r0
            Prob_dist_IL_r0(IL) = Prob_dist_IL_r0(IL) / slask
            sum_check = sum_check + Prob_dist_IL_r0(IL)
            cdf_IL_r0(IL) = sum_check
        Next
    End If
Else 'allowing negative demand realizations whith the consequence of max_IL_r0>R+Q
    
    IL = max_IL_r0
    Do While sum_check < 0.99999999
        IL = IL + 1
        
        arg1 = ((R - (IL + 0.5)) - my_Lvr) / sigma_Lvr
        arg2 = ((R + Q - (IL + 0.5)) - my_Lvr) / sigma_Lvr
        temp2 = sigma_Lvr * (g(arg1) - g(arg2)) / Q
        
        ReDim Preserve Prob_dist_IL_r0(min_IL_r0 To IL)
        ReDim cdf_IL_r0(min_IL_r0 To IL)
        
        sum_prob = temp2 - temp1
        Prob_dist_IL_r0(IL) = sum_prob
        sum_check = sum_check + sum_prob
        cdf_IL_r0(IL) = sum_check
    
        temp1 = temp2
    Loop
    
    max_IL_r0 = IL
    
End If
'*************************************************************************
    If sum_check < 0.99999999 Then
        Stop    'Investigate
    End If

    'Stop
    If 1 = 0 Then 'Testutskrift
        k = 2
        For IL = max_IL_r0 To min_IL_r0 Step -1
            k = k + 1
            test.Cells(k, 8) = IL
            test.Cells(k, 9) = Prob_dist_IL_r0(IL)
        Next
    End If
    
   
    
'************Finding optimal R***********************************************************
    Dim halt_search As Boolean
    halt_search = False
    
    'Stop
    
    R = -Q - 1
    Do While halt_search = False
        R = R + 1
        
        'Stop
       
        Call IL_VR_CW_prob_dist_Norm(R, Q, R0, Q0, pdf_IL0, prob_IL0_positive, Prob_dist_IL_r0, min_IL_r0, max_IL_r0, min_IL_vr_CW, max_IL_VR_CW, cdf_IL_vr_CW, Prob_dist_IL_vr_CW)
        Call Fillrate_calc_VR_CW(max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, min_IL_vr_CW, max_IL_VR_CW, Prob_dist_IL_vr_CW, Fillrate_R)
        
        If Fillrate_R >= Target_Fillrate_i Then
            halt_search = True
        End If

        Ready_rate_R = (1 - cdf_IL_vr_CW(0))
        
        If 1 = 0 Then 'Testutskrift
            k = 2
            For IL = max_IL_VR_CW To min_IL_vr_CW Step -1
                k = k + 1
                test.Cells(k, 11) = IL
                test.Cells(k, 12) = Prob_dist_IL_vr_CW(IL)
            Next
            k = 2
            For IL = max_IL0 To 1 Step -1
                k = k + 1
                test.Cells(k, 13) = IL
                test.Cells(k, 14) = pdf_IL0(IL)
            Next
            
        End If
        
    Loop
    
    R_opt = R
    
    If ((Fillrate_R - Ready_rate_R) > 1E-11) Then
        Stop    'This should not be possible - INVESTIGATE
    End If
    
    
    

End Sub
Function Prob_ILvr_S_u(my_Lvr, sigma_Lvr, R, x) As Double
Dim arg1, arg2, temp1, temp2 As Double

    
    arg1 = ((R - (x - 0.5)) - my_Lvr) / sigma_Lvr
    arg2 = ((R + 1 - (x - 0.5)) - my_Lvr) / sigma_Lvr
    temp1 = sigma_Lvr * (g(arg1) - g(arg2))
    
    arg1 = ((R - (x + 0.5)) - my_Lvr) / sigma_Lvr
    arg2 = ((R + 1 - (x + 0.5)) - my_Lvr) / sigma_Lvr
    temp2 = sigma_Lvr * (g(arg1) - g(arg2))
            
    Prob_ILvr_S_u = temp2 - temp1

End Function
Sub Calc_R_opt_Fillrate_constraint_Normal_VR_CW_U4(vr_index, R0, Q0, trp, h, p, Target_Fillrate_i, max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, cdfDemand_L_i, pdfDemand_L_i, cdfDemandmax_L_i, my, sigma, Q, L, R_opt, Fillrate_R, Ready_rate_R)

Dim R, R_plus_one, i, j, k, IL, k_min, R_comb, max_IL0, min_IL0 As Long
Dim slask, B0_temp, sum_prob, sumprob, sum_check, Prob_dist_IL_vr_S() As Double
Dim Ratio_h_p, Expected_IL_plus, temp1, temp2 As Double
Dim pdf_IL0(), pdf_IL_CW_VR(), prob_IL0_positive, Prob_IL0_zero_or_less As Double
Dim L_vr, cdfDemand(), pdfDemand(), cdfDemand_vr(), pdfDemand_vr(), cdf_IL_vr_CW(), Prob_dist_IL_vr_CW(), CW_demand_rate As Double
Dim L_min, L_max, pdf_undershoot(), my_Lvr, sigma_Lvr, arg1, arg2, cdf_IL_vr_zero_u As Double
Dim cdfDemandmax, cdfDemandmax_vr, min_IL_vr_CW, max_IL_VR_CW, max_undershoot, u As Long
Dim Fillrate_CW_VRdemand, mean_u, sigma_u, var_u As Double
Dim min_IL_r0, max_IL_r0, min_IL_S_u, max_IL_S_u As Long

'Dim k_max As Long

Dim test As Worksheet
Set test = Worksheets("testutskrifter")

'Step 1: Calculating the pdf of the inventory level distribution at the CW P(IL0=j) for j>=0
max_IL0 = R0 + Q0
min_IL0 = R0 + Q0 - pdfN_kmax
ReDim pdf_IL0(min_IL0 To max_IL0)

    prob_IL0_positive = 0
    For j = 1 To max_IL0
        sumprob = 0
        If j < R0 + 1 Then
            k_min = R0 + 1
        Else
            k_min = j
        End If
        
        For k = k_min To max_IL0   'max_IL0=(R0 + Q0)
            sumprob = sumprob + pdfN(k - j) / Q0
        Next
        pdf_IL0(j) = sumprob
        prob_IL0_positive = prob_IL0_positive + sumprob
    Next
    Prob_IL0_zero_or_less = 1 - prob_IL0_positive

If 1 = 1 Then
'The probabilities for backorders at the CW P(IL0=j) not really needed here but computed for verification reasons
    'Stop
    slask = 0
    For j = min_IL0 To 0
        sumprob = 0
        If j < R0 + 1 Then
            k_min = R0 + 1
        Else
            k_min = j
        End If
    
        For k = k_min To max_IL0   'max_IL0=(R0 + Q0)
            sumprob = sumprob + pdfN((k - j)) / Q0
        Next
        pdf_IL0(j) = sumprob
        slask = slask + sumprob
    Next
    sum_check = 0
    B0_temp = 0
    For j = min_IL0 To -1
        sum_check = sum_check + (-j * pdf_IL0(j))
        B0_temp = B0_temp + (-j) * (pdf_IL0(j) / Prob_IL0_zero_or_less)
    Next
    
    If Abs(slask - Prob_IL0_zero_or_less) > (epsilon * 1000) Then
        Stop
    End If
   
    If Abs(slask + prob_IL0_positive - 1) > (epsilon * 1000) Then
        Stop
    End If
End If
    
'Step 2: Estimation of the expected CW delay for the Virtual retailer when the CW is out of stock, i.e., IL0<=0
    
    If choice_L_vr = 2 Then  'adjusted mean as the average delay for units that are delayed
        'This is the preferred choice with superior performance
        Call Fillrate_calc_VR_CW(max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, 1, max_IL0, pdf_IL0, Fillrate_CW_VRdemand)
        'Logic: L=0*Fillrate_CW_VRdemand+L_vr*(1-Fillrate_CW_VRdemand)  =>  L_vr=L/(1-Fillrate_CW_VRdemand
        'Fillrate_CW_VRdemand = proportion of VR demand that can be satisfied directly from CW stock ,i.e., with leadtime=0
        '1-Fillrate_CW_VRdemand = proportion of VR demand experiencing a leadtime >0 with average L_vr
        L_vr = L / (1 - Fillrate_CW_VRdemand)
    Else
        Stop 'Other alternatives not implemented for normal demand
    End If
        
'Step 3: Calculation of the undershoot adjusted lead time demand D(L_vr)
    
    my_Lvr = my * L_vr
    sigma_Lvr = sigma * Sqr(L_vr)
    
    Call undershoot_dist_empirical_VR(vr_index, max_compounding_dist_n, R0, Q0, pdf_IL0, pdf_undershoot, max_undershoot)
    
    'mean_u = 0
    'For u = 1 To max_undershoot
    '    mean_u = mean_u + u * pdf_undershoot(u)
    'Next
    'var_u = 0
    'For u = 0 To max_undershoot
    '    var_u = var_u + ((mean_u - u) ^ 2) * pdf_undershoot(u)
    'Next
    'sigma_u = Sqr(var_u)
    
    
'Step 4: Calculation of the inventory level distribution for the virtual retailer in isolation P(IL_vr=j)for R_vr=0, in the vector Prob_dist_IL_r0, assuming the leadtime L_vr
    
    cdfDemandmax = Int(my_Lvr + 6 * sigma_Lvr)
    If (1 - Sfi(((cdfDemandmax - my_Lvr) / sigma_Lvr))) > 1E-06 Then
        Stop
    End If
    
    R = 0
    min_IL_r0 = R + 1 - max_undershoot - cdfDemandmax
    max_IL_r0 = R + Q
    
    ReDim Prob_dist_IL_r0(min_IL_r0 To max_IL_r0)
    ReDim cdf_IL_r0(min_IL_r0 To max_IL_r0)
    
'*****************Calculation of pdf and cdf of IL given R=0*********************************
    sum_check = 0
    For IL = max_IL_r0 To min_IL_r0 Step -1
        sum_prob = 0
        For u = 0 To max_undershoot
            If (R + 1 - u) >= IL Then
                sum_prob = sum_prob + pdf_undershoot(u) * Prob_ILvr_S_u(my_Lvr, sigma_Lvr, (R - u), IL)
            Else 'IP=S-u must be >= the inventory level IL
            End If
        Next
       
        Prob_dist_IL_r0(IL) = sum_prob
        sum_check = sum_check + sum_prob
        cdf_IL_r0(IL) = sum_check
        
    Next
If 1 = 1 Then
'**Normalizing Prob_dist_IL_r0 with slask=P(IL_vr<=R+Q)so that it sums to 1 even when probability for negative demand is high******
'***An option to be tested at least.....****************************
    slask = sum_check 'probability that IL<=R+Q may not equal 1 dus to possibility for negative realization in Normal dist
    sum_check = 0
    For IL = min_IL_r0 To max_IL_r0
        Prob_dist_IL_r0(IL) = Prob_dist_IL_r0(IL) / slask
        sum_check = sum_check + Prob_dist_IL_r0(IL)
        cdf_IL_r0(IL) = sum_check
    Next
'*************************************************************************
    If sum_check < 0.9999999999 Then
        Stop    'Investigate
    End If
End If
    
    
    'Stop
    If 1 = 0 Then 'Testutskrift
        k = 2
        For IL = min_IL_r0 To max_IL_r0
            k = k + 1
            test.Cells(k, 8) = IL
            test.Cells(k, 9) = Prob_dist_IL_r0(IL)
        Next
    End If
    
   
    
'************Finding optimal R***********************************************************
    Dim halt_search As Boolean
    halt_search = False
    
    'Stop
    
    R = -Q - 1
    Do While halt_search = False
        R = R + 1
        
        'Stop
       
        Call IL_VR_CW_prob_dist(R, Q, R0, Q0, pdf_IL0, prob_IL0_positive, Prob_dist_IL_r0, min_IL_r0, min_IL_vr_CW, max_IL_VR_CW, cdf_IL_vr_CW, Prob_dist_IL_vr_CW)
        Call Fillrate_calc_VR_CW(max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, min_IL_vr_CW, max_IL_VR_CW, Prob_dist_IL_vr_CW, Fillrate_R)
        
        If Fillrate_R >= Target_Fillrate_i Then
            halt_search = True
        End If

        Ready_rate_R = (1 - cdf_IL_vr_CW(0))
        
    Loop
    
    R_opt = R
    
    If ((Fillrate_R - Ready_rate_R) > 1E-11) Then
        Stop    'This should not be possible - INVESTIGATE
    End If
    
    
    

End Sub

Sub Calc_R_opt_Fillrate_constraint_compound_Poisson(h, p, Target_Fillrate_i, max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, cdfDemand, pdfDemand, cdfDemandmax, my, sigma, Q, L, R_opt, Fillrate_R, Ready_rate_R, C_opt)

Dim R, R_plus_one, i, j, k, IL, k_min As Long
Dim slask, sum_prob, sum_check, Prob_dist_IL_r0(), Prob_dist_IL(), cdf_IL_r0(), min_IL_r0, max_IL_r0 As Double
Dim Ratio_h_p, Expected_IL_plus, temp1, temp2 As Double

'Dim k_max As Long

Dim halt_search As Boolean

    R = 0
    min_IL_r0 = R + 1 - cdfDemandmax
    max_IL_r0 = R + Q
    
    ReDim Prob_dist_IL_r0(min_IL_r0 To max_IL_r0)
    ReDim cdf_IL_r0(min_IL_r0 To max_IL_r0)
    
'*****************Calculation of pdf and cdf of IL given R=0*********************************
    sum_check = 0
    For IL = min_IL_r0 To max_IL_r0
        
        If IL < R + 1 Then
            k_min = R + 1
        Else
            k_min = IL
        End If
        
        sum_prob = 0
        For k = k_min To (R + Q)
            If (k - IL) <= cdfDemandmax Then
                sum_prob = sum_prob + pdfDemand((k - IL)) / Q
            Else
                sum_prob = sum_prob + 0
            End If
        Next
        
        If k_min > IL Then
            If (R + Q - IL) <= cdfDemandmax Then
                slask = (cdfDemand((R + Q - IL)) - cdfDemand((k_min - 1 - IL))) / Q
            Else
                slask = (1 - cdfDemand((k_min - 1 - IL))) / Q
            End If
        ElseIf k_min = IL Then
            If (R + Q - IL) <= cdfDemandmax Then
                slask = cdfDemand((R + Q - IL)) / Q
            Else
                slask = 1 / Q
            End If
        End If
        
        If (Abs(slask - sum_prob) > 1E-10) Then
            Stop
        End If
        
        Prob_dist_IL_r0(IL) = sum_prob
        sum_check = sum_check + sum_prob
        cdf_IL_r0(IL) = sum_check
    Next
    
    'Stop
    If 1 = 0 Then 'Testutskrift
        k = 2
        Dim test As Worksheet
        Set test = Worksheets("testutskrifter")
        For IL = min_IL_r0 To max_IL_r0
            k = k + 1
            test.Cells(k, 8) = IL
            test.Cells(k, 9) = Prob_dist_IL_r0(IL)
        Next
    End If
    
    
    
    If sum_check < 0.9999999999 Then
        Stop    'Investigate
    End If
   
'************Finding optimal R***********************************************************
    halt_search = False
    
    'Stop
    
    R = -Q - 1
    Do While halt_search = False
        R = R + 1
        
        'Stop
        Call Fillrate_calc(R, max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, min_IL_r0, max_IL_r0, Prob_dist_IL_r0, Fillrate_R)
        
        
        If Fillrate_R >= Target_Fillrate_i Then
            halt_search = True
        End If

        Ready_rate_R = (1 - cdf_IL_r0(-R))
    Loop
    
    R_opt = R
    
    If (Fillrate_R - Ready_rate_R > 1E-11) Then
        Stop    'This should not be possible - INVESTIGATE
    End If
    
'************Calculation of C_opt**************************
    
    Expected_IL_plus = 0
    For IL = 1 To (R_opt + Q)
        Expected_IL_plus = Expected_IL_plus + IL * Prob_dist_IL_r0((IL - R_opt))
    Next
    temp1 = -1 * p * (R_opt + ((Q + 1) / 2) - my * L)
    temp2 = (h + p) * Expected_IL_plus
    
    C_opt = temp1 + temp2
    
    

End Sub
Sub Calc_R_opt_and_Lok_C_compound_Poisson(h, p, max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, cdfDemand, pdfDemand, cdfDemandmax, my, sigma, Q, L, R_opt, C_opt, Fillrate_R, Ready_rate_R)

    
Dim R, R_plus_one, i, j, k, IL, k_min As Long
Dim slask, sum_prob, sum_check, Prob_dist_IL_r0(), cdf_IL_r0(), min_IL_r0, max_IL_r0 As Double
Dim ready_rate_R_plus_one, Ratio_h_p, Expected_IL_plus, temp1, temp2 As Double

Dim halt_search As Boolean

    R = 0
    min_IL_r0 = R + 1 - cdfDemandmax
    max_IL_r0 = R + Q
    
    ReDim Prob_dist_IL_r0(min_IL_r0 To max_IL_r0)
    ReDim cdf_IL_r0(min_IL_r0 To max_IL_r0)
    
'*****************Calculation of pdf and cdf of IL given R=0*********************************
    sum_check = 0
    For IL = min_IL_r0 To max_IL_r0
        
        If IL < R + 1 Then
            k_min = R + 1
        Else
            k_min = IL
        End If
        
        sum_prob = 0
        For k = k_min To (R + Q)
            If (k - IL) <= cdfDemandmax Then
                sum_prob = sum_prob + pdfDemand((k - IL)) / Q
            Else
                sum_prob = sum_prob + 0
            End If
        Next
        
        If k_min > IL Then
            If (R + Q - IL) <= cdfDemandmax Then
                slask = (cdfDemand((R + Q - IL)) - cdfDemand((k_min - 1 - IL))) / Q
            Else
                slask = (1 - cdfDemand((k_min - 1 - IL))) / Q
            End If
        ElseIf k_min = IL Then
            If (R + Q - IL) <= cdfDemandmax Then
                slask = cdfDemand((R + Q - IL)) / Q
            Else
                slask = 1 / Q
            End If
        End If
        
        If (Abs(slask - sum_prob) > 1E-10) Then
            Stop
        End If
        
        Prob_dist_IL_r0(IL) = sum_prob
        sum_check = sum_check + sum_prob
        cdf_IL_r0(IL) = sum_check
    Next
    
    'Stop
    If 1 = 0 Then 'Testutskrift
        k = 2
        Dim test As Worksheet
        Set test = Worksheets("testutskrifter")
        For IL = min_IL_r0 To max_IL_r0
            k = k + 1
            test.Cells(k, 8) = IL
            test.Cells(k, 9) = Prob_dist_IL_r0(IL)
        Next
    End If
    
    
    
    If sum_check < 0.9999999999 Then
        Stop    'Investigate
    End If
   
'************Finding optimal R***********************************************************
    halt_search = False
    
    R = -Q - 1
    Do While halt_search = False
        R = R + 1
        If (R = -Q) Then
            Ready_rate_R = 0    'Need this for numerical reasons when p=0
        Else
            Ready_rate_R = (1 - cdf_IL_r0(-R))
        End If
        
        R_plus_one = R + 1
        ready_rate_R_plus_one = (1 - cdf_IL_r0(-R_plus_one))
        
        Ratio_h_p = (p / (h + p))
        
        If (Ready_rate_R <= Ratio_h_p) And (ready_rate_R_plus_one > Ratio_h_p) Then
            halt_search = True
        End If
    Loop
    
    R_opt = R
    
'************Calculation of C_opt**************************
    
    Expected_IL_plus = 0
    For IL = 1 To (R_opt + Q)
        Expected_IL_plus = Expected_IL_plus + IL * Prob_dist_IL_r0((IL - R_opt))
    Next
    temp1 = -1 * p * (R_opt + ((Q + 1) / 2) - my * L)
    temp2 = (h + p) * Expected_IL_plus
    
    C_opt = temp1 + temp2
    
    Call Fillrate_calc(R_opt, max_compounding_dist, pdf_compounding_dist, pdf_compounding_quant, min_IL_r0, max_IL_r0, Prob_dist_IL_r0, Fillrate_R)
    Ready_rate_R = (1 - cdf_IL_r0(-R_opt))
    

End Sub


Function C_derivata1(R, h, p, m, sigma, Q) As Double
Dim test1, test2, test3, test4 As Double
 
  C_derivata1 = h - (h + p) * (sigma / Q) * _
                    (g((R - m) / (sigma)) - _
                     g((R - m + Q) / (sigma)))

  

End Function

Sub Calc_Fillrate_R_Normal_Demand(R, sigma, m, Q, L, Fillrate_i)

Dim temp, m_L, sigma_L As Double

    m_L = m * L
    sigma_L = sigma * Sqr(L)
    temp = (sigma_L / Q) * (g((R - m_L) / (sigma_L)) - g((R - m_L + Q) / (sigma_L)))
    Fillrate_i = 1 - temp

End Sub
Function Calc_R_opt(h, p, sigma, m, Q) As Double

Dim R, temp, m_L, sigma_L           As Double
Dim cost_R, cost_R_plus_1           As Double
    
  If p = 0 Then
    Calc_R_opt = -Q
  Else
    R = Int(m - 0.5 * Q)
    Do While C_derivata1(R, h, p, m, sigma, Q) < 0
      R = R + 10
    Loop
    Do While C_derivata1(R, h, p, m, sigma, Q) > 0
      R = R - 1
    Loop
    
    'cost_R = H_func(0.01)
    
    'cost_R = h * (R + Q / 2 - m) + (h + p) * sigma * (H_func((R + Q - m) / sigma) - H_func((R - m) / sigma)) / Q
    'cost_R_plus_1 = h * (R + Q / 2 - m) + (h + p) * sigma * (-H_func((R + Q + 1 - m) / sigma) + H_func((R + 1 - m) / sigma)) / Q
    
    'If cost_R_plus_1 < cost_R Then
    '  Calc_R_opt = R + 1
    'Else
    '  Calc_R_opt = R
    'End If

    Do While C_derivata1(R, h, p, m, sigma, Q) < 0
      R = R + 0.1
    Loop
    R = R - 0.1
    Calc_R_opt = R + 0.1 * (C_derivata1(R + 0.1, h, p, m, sigma, Q) / _
           (C_derivata1(R + 0.1, h, p, m, sigma, Q) - C_derivata1(R, h, p, m, sigma, Q)))
  
  End If

End Function

Function dCdL(h, p, sigma, m, Q, L) As Double

Dim R_star As Double

    R_star = Calc_R_opt(h, p, sigma * Sqr(L), m * L, Q)
    dCdL = (h + p) * (sigma * sigma / (2 * Q)) * (Sfi((R_star - m * L + Q) / (sigma * Sqr(L))) - Sfi((R_star - m * L) / (sigma * Sqr(L))))

End Function

Function gamma(x) As Single

 gamma = (1 + x * x) * (1 - Sfi(x)) - x * fi(x)
 
End Function


Function LOK_C(h, p, sigma, m, R, Q) As Single

  LOK_C = h * (0.5 * Q + R - m) + ((h + p) * (sigma * sigma / (2 * Q))) * _
         (gamma((R - m) / (sigma)) - gamma((R - m + Q) / (sigma)))

End Function

Function g(x) As Double

  g = fi(x) - x * (1 - Sfi(x))

End Function 'G(x)


Function H_func(x) As Double

  H_func = ((x ^ 2 + 1) * (1 - Sfi(x)) - x * fi(x)) / 2

End Function