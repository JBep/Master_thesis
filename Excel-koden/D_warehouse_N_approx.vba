Option Explicit

Sub D_warehouse_N_approx(CW_demand_choice, N, pdfN, pdfmax)
 
Dim k, i, j, pdfmax_n(), k_max As Long
Dim slask, temp, cum_prob_i, tolerans, pdf_n(), mean_CW_demand_n(), Var_CW_demand_n(), L_temp() As Double
Dim mean_temp, Var_temp, k_mod, temp1, temp2, neg_prob, mean_CW_demand_total, Var_CW_demand_total As Double
Dim sumprob, StdDev_CW_demand_total, gamma_alfa, gamma_beta, CV_temp As Double
Dim min_batch_demand As Long
Dim Loop_stop, NegBin_stop As Boolean
Dim cdf_CW_Demand(), pdf_CW_Demand() As Double
Dim cdf_CW_Demandmax As Long

Dim test As Worksheet
Set test = Worksheets("testutskrifter")
Dim interface As Worksheet
Set interface = Worksheets("interface")


ReDim max_demand(1 To N)
ReDim pdfmax_n(1 To N)
ReDim mean_CW_demand_n(1 To N)
ReDim Var_CW_demand_n(1 To N)
ReDim L_temp(1 To N)


If Ret_demand_choice = 0 Then
    
    ReDim cdfDemandmax_n(1 To N) As Long
    For j = 1 To N
        cdfDemandmax_n(j) = 0
    Next

ElseIf Ret_demand_choice = 1 Then
    
    For j = 1 To N
        L_temp(j) = L0
    Next
    
    Call NegBin_prob(cdfDemand_n, pdfDemand_n, cdfDemandmax_n, L_temp)
    
    'slask = cdfDemandmax_n(1)
    'slask = cdfDemand_n(1, 1136)
    
ElseIf Ret_demand_choice = 2 Or Ret_demand_choice = 3 Then
    
    For j = 1 To N
        L_temp(j) = L0
    Next
    
    Call Compound_Poisson_geometric_prob(cdfDemand_n, pdfDemand_n, cdfDemandmax_n, L_temp)

ElseIf Ret_demand_choice >= 4 Then

    For j = 1 To N
        L_temp(j) = L0
    Next
    vr_index = vr_index
    Call Compound_Poisson_Empirical(pdf_compounding_dist_n, pdf_compounding_quant_n, max_compounding_dist_n, cdfDemand_n, pdfDemand_n, cdfDemandmax_n, L_temp)
    
    
End If

'Determining the maximum warehouse demand (number of batches) of interest
pdfmax = 0
For i = 1 To N
    CV_temp = (sigma_n(i) ^ 2) / m_n(i)
    cum_prob_i = 0
    j = 0
    Loop_stop = False
    Do While Loop_stop = False
        j = j + 1
        cum_prob_i = cum_p_new(i, m_n(i), sigma_n(i), Q_n(i) * Q_system, j, L0)
        
        If cum_prob_i > 0.999999999 Then
            Loop_stop = True
        End If
        If ((j * Q_n(i) * Q_system) > cdfDemandmax_n(i)) And (Ret_demand_choice > 0) And (CV_temp - 1) > -1 * Exp(-10) Then
            Loop_stop = True
        End If
        
        'test.Cells(k + 1, 11) = j
        'test.Cells(k + 1, 12) = cum_prob_i
    Loop
    pdfmax_n(i) = j * Q_n(i)
    If (j * Q_n(i)) > k_max Then
        k_max = j * Q_n(i)
    End If
    pdfmax = pdfmax + pdfmax_n(i)
   
Next
    
    ReDim Preserve pdf_n(1 To N, -k_max To k_max)
    
'Determining the probability distribution of the number of subbatches ordered by retailer i= 1,2,...N during L0
For i = 1 To N
    CV_temp = (sigma_n(i) ^ 2) / m_n(i)
    sumprob = 0
    If Ret_demand_choice = 0 Then
        min_batch_demand = -1 * pdfmax_n(i)
    ElseIf Ret_demand_choice > 0 Then
        If ((CV_temp - 1) < -1 * Exp(-10)) Or (vr_index = i And VR_demand = 0) Then 'Normal demand
            min_batch_demand = -1 * pdfmax_n(i)
        Else    'Compound Poisson or Pure Poisson demand
            min_batch_demand = 0
        End If
    End If
    For k = (min_batch_demand) To pdfmax_n(i)
        temp = D0i_new_N_approx(i, k, L0)
        If temp > 0 Then
            pdf_n(i, k) = temp
        Else
            temp = 0
            pdf_n(i, k) = temp
        End If
        sumprob = sumprob + temp
    Next
    If sumprob < 1 - 1E-07 Then
        MsgBox "VARNING slh! summa pdf_n[" & i & ", " & k & "] = " & sumprob
        Stop
    End If
Next

'Determining the mean and stddev of batch demands from retailer i= 1,2,...N and the total seen by the CW
mean_CW_demand_total = 0
Var_CW_demand_total = 0
For i = 1 To N
    mean_temp = 0
    Var_temp = 0
    CV_temp = (sigma_n(i) ^ 2) / m_n(i)
    If Ret_demand_choice = 0 Then
        min_batch_demand = -1 * pdfmax_n(i)
    ElseIf Ret_demand_choice > 0 Then
        If ((CV_temp - 1) < -1 * Exp(-10)) Or (vr_index = i And VR_demand = 0) Then 'Normal demand
            min_batch_demand = -1 * pdfmax_n(i)
        Else    'Compound Poisson or Pure Poisson demand
            min_batch_demand = 0
        End If
    End If
    For k = min_batch_demand To pdfmax_n(i)
        mean_temp = mean_temp + k * pdf_n(i, k)
    Next
    For k = min_batch_demand To pdfmax_n(i)
        Var_temp = Var_temp + ((k - mean_temp) ^ 2) * pdf_n(i, k)
        interface.Cells(70 + k, i * 2 - 1) = (k - mean_temp) ^ 2
        interface.Cells(70 + k, i * 2) = pdf_n(i, k)
    Next
    
    mean_CW_demand_n(i) = mean_temp
    Var_CW_demand_n(i) = Var_temp
    mean_CW_demand_total = mean_CW_demand_total + mean_temp
    Var_CW_demand_total = Var_CW_demand_total + Var_temp
Next
          
    slask = 0
    interface.Cells(4, 4) = pdfmax
    ReDim pdfN(0 To pdfmax)
    k = -1
    'tolerans = 0.00001
    tolerans = interface.Cells(5, 7)
    StdDev_CW_demand_total = Sqr(Var_CW_demand_total)
    interface.Cells(17, 7) = mean_CW_demand_total
    interface.Cells(17, 8) = StdDev_CW_demand_total
    
    'testutskrift
    If 1 = 0 Then
        MsgBox "mean_CW_demand_total=" & mean_CW_demand_total
        MsgBox "StdDev_CW_demand_total=" & StdDev_CW_demand_total
    End If
    
    If CW_demand_choice = 2 Then
        neg_prob = WorksheetFunction.NormDist(-0.5, mean_CW_demand_total, StdDev_CW_demand_total, True)
        MsgBox "Probability of negative demand in Normal approximation = " & neg_prob
        If neg_prob > 0.025 Then
            MsgBox "Warning! Probability of negative demand in Normal approximation exceeds 2.5%!"
        End If
    ElseIf CW_demand_choice = 3 Then
        gamma_alfa = (mean_CW_demand_total ^ 2) / Var_CW_demand_total
        gamma_beta = Var_CW_demand_total / mean_CW_demand_total
        stepsize_num_int_gamma = interface.Cells(5, 9)
    ElseIf CW_demand_choice = 4 Then
        'Stop
        Call NegBin_prob_CW_demand(cdf_CW_Demand, pdf_CW_Demand, cdf_CW_Demandmax, mean_CW_demand_total, Var_CW_demand_total)
        
    Else
        Stop
    End If
    
    
    NegBin_stop = False
    Do While (slask < (1 - tolerans)) And NegBin_stop = False
        k = k + 1
        If CW_demand_choice = 2 Then
            k_mod = (k + 0.5)
            temp1 = WorksheetFunction.NormDist(k_mod, mean_CW_demand_total, StdDev_CW_demand_total, True)
            temp2 = WorksheetFunction.NormDist(k_mod - 1, mean_CW_demand_total, StdDev_CW_demand_total, True)
            temp = (temp1 - temp2) / (1 - neg_prob)
        ElseIf CW_demand_choice = 3 Then
            k_mod = k + 0.5
            temp1 = GammaDistribution(k_mod, gamma_alfa, gamma_beta, True)
            If k_mod - 1 < 0 Then
                temp2 = GammaDistribution(0, gamma_alfa, gamma_beta, True)
            Else
                temp2 = GammaDistribution(k_mod - 1, gamma_alfa, gamma_beta, True)
            End If
            temp = (temp1 - temp2)
        ElseIf CW_demand_choice = 4 Then
            temp = pdf_CW_Demand(k)
            If k = cdf_CW_Demandmax Then
                NegBin_stop = True
            End If
        End If
        
        If (temp < 0) And (Abs(temp) < 1 * 10 ^ -8) Then
            temp = 0
        ElseIf temp < 0 Then
            MsgBox "VARNING negativ slh! pdfN[" & k & "] = " & temp
            Stop
            temp = 0
        End If
        
        ReDim Preserve pdfN(0 To k)
        pdfN(k) = temp
        slask = slask + temp
        'test.Cells(k + 3, 6) = k
        'test.Cells(k + 3, 7) = pdfN(k)
        interface.Cells(4, 5) = k
        interface.Cells(4, 6) = pdfN(k)
        interface.Cells(4, 7) = slask
    'interface.Cells(2, 6) = k
    'interface.Cells(3, 6) = pdfN(k)
    'printing the sums
        interface.Cells(49 + k, 7) = k
        interface.Cells(49 + k, 8) = pdfN(k)
    Loop
    
    'pdfN(pdfmax) = 0
    pdfmax = k
    
     If slask > (1 + tolerans) Then
        MsgBox "VARNING summan av pdfN[pdfmax]= " & slask & " ‰r stˆrre ‰n 1 !!"
    End If
     If slask < (1 - tolerans) Then
        MsgBox "VARNING summan av pdfN[i] ‰r mindre ‰n 1 !!" & slask
    End If
    
    
     interface.Cells(49, 6) = mean_CW_demand_total
     interface.Cells(50, 6) = StdDev_CW_demand_total
     interface.Cells(51, 6) = neg_prob
     
 
End Sub

Function D0i_new_N_approx(Retailer, d, t) As Double

  Dim j, Q As Long
  Dim my, sigma, CV_temp As Double
  
  j = Int(d / Q_n(Retailer))
  my = m_n(Retailer)
  sigma = sigma_n(Retailer)
  Q = Q_n(Retailer) * Q_system
  
  If (d Mod Q_n(Retailer)) > 0 Then
    D0i_new_N_approx = 0
  ElseIf (d Mod Q_n(Retailer)) = 0 Then
    If Ret_demand_choice = 0 Then
        D0i_new_N_approx = (cum_p_new(Retailer, my, sigma, Q, j, t) - cum_p_new(Retailer, my, sigma, Q, j - 1, t))
    ElseIf Ret_demand_choice > 0 Then
        CV_temp = (sigma ^ 2) / my
        If ((CV_temp - 1) < -1 * Exp(-10)) Or (vr_index = Retailer And VR_demand = 0) Then 'Normal demand
            D0i_new_N_approx = (cum_p_new(Retailer, my, sigma, Q, j, t) - cum_p_new(Retailer, my, sigma, Q, j - 1, t))
        Else
            If j = 0 Then
                D0i_new_N_approx = (cum_p_new(Retailer, my, sigma, Q, j, t))
            ElseIf j > 0 Then
                D0i_new_N_approx = (cum_p_new(Retailer, my, sigma, Q, j, t) - cum_p_new(Retailer, my, sigma, Q, j - 1, t))
            End If
        End If
    End If
  End If


End Function
Sub NegBin_prob_CW_demand(cdf_CW_Demand, pdf_CW_Demand, cdf_CW_Demandmax, mean_CW_demand_total, Var_CW_demand_total)

    Dim i, j, k, k_max, y As Long
    Dim temp1, temp2, temp3, faktorial, sum, r_j, prob_j As Double
    Dim t, sum_prob, lambda_j, Poisson_prob, CV_temp As Double
    
    Dim Poisson_cdf(), Poisson_pdf() As Double
    Dim max_compounding, i_max As Long
    'Dim Log_constant, prob_j_power_i, logarithmic_prob, sum_prob_compounding As Double

    ReDim Poisson_pdf(0 To 1)
    ReDim Poisson_cdf(0 To 1)
   
    max_compounding = 1

    k_max = 0
    temp1 = Var_CW_demand_total - mean_CW_demand_total
        
    CV_temp = Var_CW_demand_total / mean_CW_demand_total
    If Abs(CV_temp - 1) <= Exp(-10) Then
        temp1 = 0   'Poisson demand!!!
        CV_temp = 1
    End If
    If CV_temp < 1 Then
        Stop    'Conditions for compound Poisson demand are not satisfied. Change to Gamma or Normal demand
    End If
        
        
    If temp1 = 0 Then   'Pure Poisson demand, i.e., CV_temp=1
            'Stop  'Not tested yet!
            
        lambda_j = mean_CW_demand_total
        k = 0
        Poisson_prob = Exp(-1 * lambda_j)
        Poisson_pdf(0) = Poisson_prob
        sum_prob = Poisson_prob
        Poisson_cdf(k) = sum_prob
        Do While sum_prob < 0.99999999999
            k = k + 1
            Poisson_prob = Poisson_prob * ((lambda_j / k))
            ReDim Preserve Poisson_pdf(0 To k)
            ReDim Preserve Poisson_cdf(0 To k)
            Poisson_pdf(k) = Poisson_prob
            sum_prob = sum_prob + Poisson_prob
            Poisson_cdf(k) = sum_prob
        Loop
        If Poisson_prob > 1E-09 Then
            Stop 'investigate
        End If
        
        cdf_CW_Demandmax = k
            
        If k > k_max Then
            k_max = k
        End If
        
        ReDim Preserve cdf_CW_Demand(0 To k_max)
        ReDim Preserve pdf_CW_Demand(0 To k_max)
        
        For k = 0 To cdf_CW_Demandmax
            cdf_CW_Demand(k) = Poisson_cdf(k)
            pdf_CW_Demand(k) = Poisson_pdf(k)
        Next
                        
            If cdf_CW_Demand(cdf_CW_Demandmax) > 1 Then
                Stop
            End If
            If cdf_CW_Demand(cdf_CW_Demandmax) < 0.9999999999 Then
                Stop
            End If
 
    ElseIf CV_temp > 1 Then             'NegBIN demand

  
 '************NegBIN calculations begin********************************************
        prob_j = 1 - (mean_CW_demand_total / Var_CW_demand_total)
        r_j = ((mean_CW_demand_total) ^ 2) / (Var_CW_demand_total - mean_CW_demand_total)
        sum = 0
        k = -1
        Do While sum < 0.99999999
            k = k + 1
            ReDim Preserve cdf_CW_Demand(0 To k)
            ReDim Preserve pdf_CW_Demand(0 To k)
            If k = 0 Then
                faktorial = 1
                temp1 = (1 - prob_j) ^ r_j
                temp3 = faktorial * temp1
                sum = sum + temp3
            ElseIf k > 0 Then
                faktorial = faktorial * ((r_j + k - 1) / k)
                temp2 = prob_j ^ k
                temp3 = faktorial * temp1 * temp2
            
                sum = sum + temp3
                  
            End If
            cdf_CW_Demand(k) = sum
            pdf_CW_Demand(k) = temp3
        Loop
        cdf_CW_Demandmax = k
        If k > k_max Then
            k_max = k
        End If
        
    
        If sum > 1 Then
            Stop
        End If
        If sum < 0.99999999 Then
            Stop
        End If
    
    End If 'NegBIN condition satisfied - temp1>0
    

End Sub

Sub NegBin_prob(cdfDemand_n, pdfDemand_n, cdfDemandmax_n, L_temp)

    Dim i, j, k, k_max, y As Long
    Dim temp1, temp2, temp3, faktorial, sum, r_j, prob_j As Double
    Dim t, sum_prob, lambda_j, Poisson_prob, CV_temp As Double
    
    Dim cdfDemand(), pdfDemand(), Poisson_cdf(), Poisson_pdf() As Double
    Dim max_compounding, i_max As Long
    Dim Log_constant, prob_j_power_i, logarithmic_prob, sum_prob_compounding As Double
    
    
    ReDim cdfDemandmax_n(1 To N)
    ReDim cdfDemand(0 To 1)
    ReDim pdfDemand(0 To 1)
    ReDim Poisson_pdf(0 To 1)
    ReDim Poisson_cdf(0 To 1)
    ReDim max_compounding_dist_n(1 To N)
    ReDim pdf_compounding_dist_n(1 To N, 1 To 1)
    ReDim pdf_compounding_quant_n(1 To N, 1 To 1)
    
    max_compounding = 1
    
    k_max = 0
    For j = 1 To N
        
        temp1 = sigma_n(j) ^ 2 - m_n(j)
        
        CV_temp = sigma_n(j) ^ 2 / m_n(j)
        If Abs(CV_temp - 1) <= Exp(-10) Then
            temp1 = 0   'Poisson demand!!!
            CV_temp = 1
        End If
        If CV_temp < 1 Or (vr_index = j And VR_demand = 0) Then
            'Stop    'Conditions for compound Poisson demand are not satisfied. Change to Normal demand
            cdfDemandmax_n(j) = 0
            max_compounding_dist_n(j) = 0
        End If
        
        
      If temp1 = 0 Then   'Pure Poisson demand, i.e., CV_temp=1
            'Stop  'Not tested yet!
            max_compounding_dist_n(j) = 1
            pdf_compounding_dist_n(j, 1) = 1
            pdf_compounding_quant_n(j, 1) = 1
            
            
            lambda_j = m_n(j)
            t = L_temp(j)
            k = 0
            Poisson_prob = Exp(-1 * lambda_j * t)
            Poisson_pdf(0) = Poisson_prob
            sum_prob = Poisson_prob
            Poisson_cdf(k) = sum_prob
            Do While sum_prob < 0.99999999999
                k = k + 1
                Poisson_prob = Poisson_prob * (((lambda_j * t) / k))
                ReDim Preserve Poisson_pdf(0 To k)
                ReDim Preserve Poisson_cdf(0 To k)
                Poisson_pdf(k) = Poisson_prob
                sum_prob = sum_prob + Poisson_prob
                Poisson_cdf(k) = sum_prob
            Loop
            If Poisson_prob > 1E-09 Then
                Stop 'investigate
            End If
        
            cdfDemandmax_n(j) = k
            If k > k_max Then
                k_max = k
            End If
        
            ReDim Preserve cdfDemand_n(1 To N, 0 To k_max)
            ReDim Preserve pdfDemand_n(1 To N, 0 To k_max)
        
            For k = 0 To cdfDemandmax_n(j)
                cdfDemand_n(j, k) = Poisson_cdf(k)
                pdfDemand_n(j, k) = Poisson_pdf(k)
            Next
                        
            If cdfDemand_n(j, cdfDemandmax_n(j)) > 1 Then
                Stop
            End If
            If cdfDemand_n(j, cdfDemandmax_n(j)) < 0.9999999999 Then
                Stop
            End If
 
      ElseIf CV_temp > 1 Then             'NegBIN demand
        

  
 '************NegBIN calculations begin********************************************
        prob_j = 1 - (m_n(j) / (sigma_n(j) * sigma_n(j)))
        r_j = L_temp(j) * (m_n(j) * m_n(j)) / ((sigma_n(j) * sigma_n(j)) - m_n(j))
        sum = 0
        k = -1
        Do While sum < 0.9999999999
            k = k + 1
            ReDim Preserve cdfDemand(0 To k)
            ReDim Preserve pdfDemand(0 To k)
            If k = 0 Then
                faktorial = 1
                temp1 = (1 - prob_j) ^ r_j
                temp3 = faktorial * temp1
                sum = sum + temp3
            ElseIf k > 0 Then
                faktorial = faktorial * ((r_j + k - 1) / k)
                temp2 = prob_j ^ k
                temp3 = faktorial * temp1 * temp2
            
                sum = sum + temp3
                  
            End If
            cdfDemand(k) = sum
            pdfDemand(k) = temp3
        Loop
        cdfDemandmax_n(j) = k
        If k > k_max Then
            k_max = k
        End If
        
        ReDim Preserve cdfDemand_n(1 To N, 0 To k_max)
        ReDim Preserve pdfDemand_n(1 To N, 0 To k_max)
        
        For k = 0 To cdfDemandmax_n(j)
            cdfDemand_n(j, k) = cdfDemand(k)
            pdfDemand_n(j, k) = pdfDemand(k)
        Next
        
        
        If sum > 1 Then
            Stop
        End If
        If sum < 0.9999999999 Then
            Stop
        End If
        
'************Compounding distribution calculations********************************
 
        i_max = 1
        Log_constant = Log((1 - prob_j))
        logarithmic_prob = -1 * prob_j / Log_constant
        prob_j_power_i = prob_j
        sum_prob = logarithmic_prob
        pdf_compounding_dist_n(j, i_max) = logarithmic_prob
        pdf_compounding_quant_n(j, i_max) = i_max
        
        Do While sum_prob < 0.9999999999
            i_max = i_max + 1
            prob_j_power_i = prob_j_power_i * prob_j
            logarithmic_prob = -1 * prob_j_power_i / (Log_constant * i_max)
            sum_prob = sum_prob + logarithmic_prob
            If i_max > max_compounding Then
                max_compounding = i_max
                ReDim Preserve pdf_compounding_dist_n(1 To N, 1 To i_max)
                ReDim Preserve pdf_compounding_quant_n(1 To N, 1 To i_max)
            End If
            pdf_compounding_dist_n(j, i_max) = logarithmic_prob
            pdf_compounding_quant_n(j, i_max) = i_max
        Loop
        max_compounding_dist_n(j) = i_max
        
        If (prob_j > 0) And (logarithmic_prob > 1E-09) Then
            Stop 'investigate
        End If
        sum_prob_compounding = 0
        For i = 1 To i_max
            sum_prob_compounding = sum_prob_compounding + pdf_compounding_dist_n(j, i)
        Next
        If sum_prob_compounding > 1 Or sum_prob_compounding < 0.9999999999 Then
            Stop    'Investigate
        End If
        
      End If 'NegBIN condition satisfied - temp1>0
    Next

End Sub
Sub NegBin_prob_VR(j_vr, cdfDemand, pdfDemand, cdfDemandmax, L_temp)

    Dim i, j, k, k_max, y As Long
    Dim temp1, temp2, temp3, faktorial, sum, r_j, prob_j As Double
    Dim t, sum_prob, lambda_j, Poisson_prob, CV_temp As Double
    
    Dim Poisson_cdf(), Poisson_pdf() As Double
    Dim max_compounding, i_max As Long
    Dim Log_constant, prob_j_power_i, logarithmic_prob, sum_prob_compounding As Double
    
    
'    ReDim cdfDemandmax_n(1 To N)
'    ReDim cdfDemand(0 To 1)
'    ReDim pdfDemand(0 To 1)
    ReDim Poisson_pdf(0 To 1)
    ReDim Poisson_cdf(0 To 1)
'    ReDim max_compounding_dist_n(1 To N)               Already calculated earlier in the program and available as opublic arrays
'    ReDim pdf_compounding_dist_n(1 To N, 1 To 1)       Already calculated earlier in the program and available as opublic arrays
'    ReDim pdf_compounding_quant_n(1 To N, 1 To 1)      Already calculated earlier in the program and available as opublic arrays
    
    max_compounding = 1
    
    k_max = 0
    For j = j_vr To j_vr
        
        temp1 = sigma_n(j) ^ 2 - m_n(j)
        
        CV_temp = sigma_n(j) ^ 2 / m_n(j)
        If Abs(CV_temp - 1) <= Exp(-10) Then
            temp1 = 0   'Poisson demand!!!
            CV_temp = 1
        End If
        If CV_temp < 1 Or (vr_index = j And VR_demand = 0) Then
            Stop    'Conditions for compound Poisson demand are not satisfied. Change to Normal demand
            cdfDemandmax_n(j) = 0
            max_compounding_dist_n(j) = 0
        End If
        
        
      If temp1 = 0 Then   'Pure Poisson demand, i.e., CV_temp=1
            'Stop  'Not tested yet!
            max_compounding_dist_n(j) = 1
            pdf_compounding_dist_n(j, 1) = 1
            pdf_compounding_quant_n(j, 1) = 1
            
            
            lambda_j = m_n(j)
            t = L_temp(j)
            k = 0
            Poisson_prob = Exp(-1 * lambda_j * t)
            Poisson_pdf(0) = Poisson_prob
            sum_prob = Poisson_prob
            Poisson_cdf(k) = sum_prob
            Do While sum_prob < 0.99999999999
                k = k + 1
                Poisson_prob = Poisson_prob * (((lambda_j * t) / k))
                ReDim Preserve Poisson_pdf(0 To k)
                ReDim Preserve Poisson_cdf(0 To k)
                Poisson_pdf(k) = Poisson_prob
                sum_prob = sum_prob + Poisson_prob
                Poisson_cdf(k) = sum_prob
            Loop
            If Poisson_prob > 1E-09 Then
                Stop 'investigate
            End If
        
'            cdfDemandmax_n(j) = k
            cdfDemandmax = k
            If k > k_max Then
                k_max = k
            End If
        
            ReDim Preserve cdfDemand(1 To N, 0 To cdfDemandmax)
            ReDim Preserve pdfDemand(1 To N, 0 To cdfDemandmax)
        
            For k = 0 To cdfDemandmax
                cdfDemand(k) = Poisson_cdf(k)
                pdfDemand(k) = Poisson_pdf(k)
            Next

            If cdfDemand(cdfDemandmax) > 1 Then
                Stop
            End If
            If cdfDemand(cdfDemandmax) < 0.9999999999 Then
                Stop
            End If
 
      ElseIf CV_temp > 1 Then             'NegBIN demand
        

  
 '************NegBIN calculations begin********************************************
        prob_j = 1 - (m_n(j) / (sigma_n(j) * sigma_n(j)))
        r_j = L_temp * (m_n(j) * m_n(j)) / ((sigma_n(j) * sigma_n(j)) - m_n(j))
        sum = 0
        k = -1
        Do While sum < 0.9999999999
            k = k + 1
            ReDim Preserve cdfDemand(0 To k)
            ReDim Preserve pdfDemand(0 To k)
            If k = 0 Then
                faktorial = 1
                temp1 = (1 - prob_j) ^ r_j
                temp3 = faktorial * temp1
                sum = sum + temp3
            ElseIf k > 0 Then
                faktorial = faktorial * ((r_j + k - 1) / k)
                temp2 = prob_j ^ k
                temp3 = faktorial * temp1 * temp2
            
                sum = sum + temp3
                  
            End If
            cdfDemand(k) = sum
            pdfDemand(k) = temp3
        Loop
'        cdfDemandmax_n(j) = k
        cdfDemandmax = k
        If k > k_max Then
            k_max = k
        End If
        
'        ReDim Preserve cdfDemand_n(1 To N, 0 To k_max)
'        ReDim Preserve pdfDemand_n(1 To N, 0 To k_max)
'
'        For k = 0 To cdfDemandmax_n(j)
'            cdfDemand_n(j, k) = cdfDemand(k)
'            pdfDemand_n(j, k) = pdfDemand(k)
'        Next
        
        
        If sum > 1 Then
            Stop
        End If
        If sum < 0.9999999999 Then
            Stop
        End If
       
If 1 = 0 Then 'These calculations are already done earlier in the program and results are avalialble in the public arrays pdf_compounding_dist_n and pdf_compounding_quant_n
'************Compounding distribution calculations********************************
 
        i_max = 1
        Log_constant = Log((1 - prob_j))
        logarithmic_prob = -1 * prob_j / Log_constant
        prob_j_power_i = prob_j
        sum_prob = logarithmic_prob
        pdf_compounding_dist_n(j, i_max) = logarithmic_prob
        pdf_compounding_quant_n(j, i_max) = i_max
        
        Do While sum_prob < 0.9999999999
            i_max = i_max + 1
            prob_j_power_i = prob_j_power_i * prob_j
            logarithmic_prob = -1 * prob_j_power_i / (Log_constant * i_max)
            sum_prob = sum_prob + logarithmic_prob
            If i_max > max_compounding Then
                max_compounding = i_max
                ReDim Preserve pdf_compounding_dist_n(1 To N, 1 To i_max)
                ReDim Preserve pdf_compounding_quant_n(1 To N, 1 To i_max)
            End If
            pdf_compounding_dist_n(j, i_max) = logarithmic_prob
            pdf_compounding_quant_n(j, i_max) = i_max
        Loop
        max_compounding_dist_n(j) = i_max
        
        If (prob_j > 0) And (logarithmic_prob > 1E-09) Then
            Stop 'investigate
        End If
        sum_prob_compounding = 0
        For i = 1 To i_max
            sum_prob_compounding = sum_prob_compounding + pdf_compounding_dist_n(j, i)
        Next
        If sum_prob_compounding > 1 Or sum_prob_compounding < 0.9999999999 Then
            Stop    'Investigate
        End If
        
End If ' Componding calc
        
      End If 'NegBIN condition satisfied - temp1>0
    
    
    Next

End Sub

Sub NegBin_prob_Li_demand(CW_demand_choice, R0, Q0, cdfDemand_n, pdfDemand_n, cdfDemandmax_n, delay_temp)

    Dim i, j, k, k_max, y As Long
    Dim temp1, temp2, temp3, faktorial, sum, r_j, prob_j As Double
    Dim sum_prob, lambda_j, Poisson_prob, CV_temp As Double
    
    Dim cdfDemand(), pdfDemand(), Poisson_cdf(), Poisson_pdf() As Double
    Dim max_compounding, i_max As Long
    Dim Log_constant, prob_j_power_i, logarithmic_prob, sum_prob_compounding As Double
    Dim mean_L_n(), sigma_L_n() As Double
    
    ReDim cdfDemandmax_n(1 To N)
    ReDim cdfDemand(0 To 1)
    ReDim pdfDemand(0 To 1)
    ReDim Poisson_pdf(0 To 1)
    ReDim Poisson_cdf(0 To 1)
    ReDim max_compounding_dist_n(1 To N)
    ReDim pdf_compounding_dist_n(1 To N, 1 To 1)
    ReDim pdf_compounding_quant_n(1 To N, 1 To 1)
    ReDim mean_L_n(1 To N)
    ReDim sigma_L_n(1 To N)
    
    max_compounding = 1
    
    If Leadtime_choice = 4 Then
        'Stop
        Call Leadtime_mean_and_variance_approx(CW_demand_choice, pdfN, pmax, R0, Q0, trp_n, mean_L_n, sigma_L_n)
    End If
    
    k_max = 0
    For j = 1 To N
        
        If Leadtime_choice = 3 Then  '(Assumption: delivery delay is exponentially distributed)
            'sigma_L_n(j) = 0
            mean_L_n(j) = trp_n(j) + delay_temp(j)
            sigma_L_n(j) = delay_temp(j)
            
            mean_leadtime_demand_n(j) = mean_L_n(j) * m_n(j)
            sigma_leadtime_demand_n(j) = Sqr((sigma_n(j) ^ 2) * mean_L_n(j) + (m_n(j) ^ 2) * (sigma_L_n(j) ^ 2))
        
        ElseIf Leadtime_choice = 4 Then
            'Stop
            'Call Leadtime_mean_and_variance_approx(CW_demand_choice, pdfN, pmax, R0, Q0, trp_n, mean_L_n, sigma_L_n)
            
            mean_leadtime_demand_n(j) = mean_L_n(j) * m_n(j)
            sigma_leadtime_demand_n(j) = Sqr((sigma_n(j) ^ 2) * mean_L_n(j) + (m_n(j) ^ 2) * (sigma_L_n(j) ^ 2))
        Else
            mean_L_n(j) = trp_n(j) + delay_temp(j)
            mean_leadtime_demand_n(j) = mean_L_n(j) * m_n(j)
            sigma_leadtime_demand_n(j) = Sqr(mean_L_n(j)) * sigma_n(j)
        End If
        
        
        temp1 = sigma_leadtime_demand_n(j) ^ 2 - mean_leadtime_demand_n(j)
        'temp1 = sigma_n(j) ^ 2 - m_n(j)
        
        CV_temp = sigma_leadtime_demand_n(j) ^ 2 / mean_leadtime_demand_n(j)
        If Abs(CV_temp - 1) <= Exp(-10) Then
            temp1 = 0   'Poisson demand!!!
            CV_temp = 1
        End If
        If CV_temp < 1 Then
            'Stop    'Conditions for compound Poisson demand are not satisfied. Change to Normal demand
            cdfDemandmax_n(j) = 0
            max_compounding_dist_n(j) = 0
        End If
        
        
      If temp1 = 0 Then   'Pure Poisson demand, i.e., CV_temp=1
            'Stop  'Not tested yet!
            max_compounding_dist_n(j) = 1
            pdf_compounding_dist_n(j, 1) = 1
            pdf_compounding_quant_n(j, 1) = 1
            
            
            lambda_j = mean_leadtime_demand_n(j)
            't = L_temp(j)
            k = 0
            Poisson_prob = Exp(-1 * lambda_j)
            Poisson_pdf(0) = Poisson_prob
            sum_prob = Poisson_prob
            Poisson_cdf(k) = sum_prob
            Do While sum_prob < 0.99999999999
                k = k + 1
                Poisson_prob = Poisson_prob * (((lambda_j) / k))
                ReDim Preserve Poisson_pdf(0 To k)
                ReDim Preserve Poisson_cdf(0 To k)
                Poisson_pdf(k) = Poisson_prob
                sum_prob = sum_prob + Poisson_prob
                Poisson_cdf(k) = sum_prob
            Loop
            If Poisson_prob > 1E-09 Then
                Stop 'investigate
            End If
        
            cdfDemandmax_n(j) = k
            If k > k_max Then
                k_max = k
            End If
        
            ReDim Preserve cdfDemand_n(1 To N, 0 To k_max)
            ReDim Preserve pdfDemand_n(1 To N, 0 To k_max)
        
            For k = 0 To cdfDemandmax_n(j)
                cdfDemand_n(j, k) = Poisson_cdf(k)
                pdfDemand_n(j, k) = Poisson_pdf(k)
            Next
                        
            If cdfDemand_n(j, cdfDemandmax_n(j)) > 1 Then
                Stop
            End If
            If cdfDemand_n(j, cdfDemandmax_n(j)) < 0.9999999999 Then
                Stop
            End If
 
      ElseIf CV_temp > 1 Then             'NegBIN demand
        

  
 '************NegBIN calculations begin********************************************
        prob_j = 1 - (mean_leadtime_demand_n(j) / (sigma_leadtime_demand_n(j) ^ 2))
        r_j = (mean_leadtime_demand_n(j) ^ 2) / ((sigma_leadtime_demand_n(j) ^ 2) - mean_leadtime_demand_n(j))
        'r_j = (mean_leadtime_demand_n(j) * (1 - prob_j) / prob_j)
        'r_j = L_temp(j) * (m_n(j) * m_n(j)) / ((sigma_n(j) * sigma_n(j)) - m_n(j))
        sum = 0
        k = -1
        Do While sum < 0.9999999999
            k = k + 1
            ReDim Preserve cdfDemand(0 To k)
            ReDim Preserve pdfDemand(0 To k)
            If k = 0 Then
                faktorial = 1
                temp1 = (1 - prob_j) ^ r_j
                temp3 = faktorial * temp1
                sum = sum + temp3
            ElseIf k > 0 Then
                faktorial = faktorial * ((r_j + k - 1) / k)
                temp2 = prob_j ^ k
                temp3 = faktorial * temp1 * temp2
            
                sum = sum + temp3
                  
            End If
            cdfDemand(k) = sum
            pdfDemand(k) = temp3
        Loop
        cdfDemandmax_n(j) = k
        If k > k_max Then
            k_max = k
        End If
        
        ReDim Preserve cdfDemand_n(1 To N, 0 To k_max)
        ReDim Preserve pdfDemand_n(1 To N, 0 To k_max)
        
        For k = 0 To cdfDemandmax_n(j)
            cdfDemand_n(j, k) = cdfDemand(k)
            pdfDemand_n(j, k) = pdfDemand(k)
        Next
        
        
        If sum > 1 Then
            Stop
        End If
        If sum < 0.9999999999 Then
            Stop
        End If
        
'************Compounding distribution calculations********************************
 
        i_max = 1
        Log_constant = Log((1 - prob_j))
        logarithmic_prob = -1 * prob_j / Log_constant
        prob_j_power_i = prob_j
        sum_prob = logarithmic_prob
        pdf_compounding_dist_n(j, i_max) = logarithmic_prob
        pdf_compounding_quant_n(j, i_max) = i_max
        
        Do While sum_prob < 0.9999999999
            i_max = i_max + 1
            prob_j_power_i = prob_j_power_i * prob_j
            logarithmic_prob = -1 * prob_j_power_i / (Log_constant * i_max)
            sum_prob = sum_prob + logarithmic_prob
            If i_max > max_compounding Then
                max_compounding = i_max
                ReDim Preserve pdf_compounding_dist_n(1 To N, 1 To i_max)
                ReDim Preserve pdf_compounding_quant_n(1 To N, 1 To i_max)
            End If
            pdf_compounding_dist_n(j, i_max) = logarithmic_prob
            pdf_compounding_quant_n(j, i_max) = i_max
        Loop
        max_compounding_dist_n(j) = i_max
        
        If (prob_j > 0) And (logarithmic_prob > 1E-09) Then
            Stop 'investigate
        End If
        sum_prob_compounding = 0
        For i = 1 To i_max
            sum_prob_compounding = sum_prob_compounding + pdf_compounding_dist_n(j, i)
        Next
        If sum_prob_compounding > 1 Or sum_prob_compounding < 0.9999999999 Then
            Stop    'Investigate
        End If
        
      End If 'NegBIN condition satisfied - temp1>0
    Next

End Sub
Sub Compound_Poisson_Empirical_VR(j_vr, pdf_compounding_dist, pdf_compounding_quant, max_compounding_dist, cdfDemand, pdfDemand, cdfDemandmax, L_vr)
    Dim i, j, k, k_max, d, d_max, Max_d_max, i_max, max_temp, y As Long
    Dim temp1, temp2, temp3, faktorial, cum_prob, lambda_j, prob_j, sum_prob, sum_prob_d As Double
    Dim Poisson_prob, Geometric_prob, t, CV_temp As Double
    Dim Poisson_pdf(), Poisson_cdf(), f_j_k_vector(), f_k_prob_vector(), L_temp() As Double
    Dim f_k_quant_vector(), f_k_max_obs_vector(), f_k_start_search() As Long
    'Dim cdfDemand(), pdfDemand()
    Dim max_comp, max_comp_tot, Retailer, obs_nr As Long
    Dim sum_prob_compounding, Expected_demand_quantity, prob_demand_d, prob_d_k  As Double
    Dim output As Excel.Worksheet
    
    
    Set output = Worksheets("Output")
    
    
    
    ReDim cdfDemand(0 To 1)
    ReDim pdfDemand(0 To 1)
    ReDim Poisson_pdf(0 To 1)
    ReDim Poisson_cdf(0 To 1)
    ReDim L_temp(1 To N)
    
    Max_d_max = 1
    For j = j_vr To j_vr
        L_temp(j) = L_vr
        CV_temp = (sigma_n(j) ^ 2) / m_n(j)
        t = L_temp(j)
    
        If Abs(CV_temp - 1) <= Exp(-10) Then
            temp1 = 0   'Poisson demand!!!
            CV_temp = 1
        End If
        If Ret_demand_choice = 3 And (CV_temp > 1) Then
            Stop    'Condition for Poisson demand is not satisfied.
                    'Empirical compound Poisson is used instead
            Ret_demand_choice = 4
            interface.Cells(8, 2) = Ret_demand_choice
        End If
        If CV_temp < 1 Then
            'Stop    'Conditions for compound Poisson demand are not satisfied. Change to Normal demand
            cdfDemandmax = 0
        End If
        
  If CV_temp >= 1 Then
        
        Expected_demand_quantity = 0
        For i = 1 To max_compounding_dist
            Expected_demand_quantity = Expected_demand_quantity + pdf_compounding_quant(i) * pdf_compounding_dist(i)
        Next
       
        lambda_j = m_n(j) / Expected_demand_quantity
       
        
    '***Determining max number of customers k and maximum demand d_max*************
        
        k_max = 0
        Poisson_prob = Exp(-1 * lambda_j * t)
        Poisson_pdf(0) = Poisson_prob
        sum_prob = Poisson_prob
        Poisson_cdf(k_max) = sum_prob
        Do While sum_prob < 0.99999999999
            k_max = k_max + 1
            Poisson_prob = Poisson_prob * (((lambda_j * t) / k_max))
            ReDim Preserve Poisson_pdf(0 To k_max)
            ReDim Preserve Poisson_cdf(0 To k_max)
            Poisson_pdf(k_max) = Poisson_prob
            sum_prob = sum_prob + Poisson_prob
            Poisson_cdf(k_max) = sum_prob
        Loop
        If Poisson_prob > 1E-09 Then
            Stop 'investigate
        End If
   Else
        'max_compounding_dist_n(j) = 0
   End If
  
  If CV_temp = 1 Then  'Pure Poisson demand
        
        max_compounding_dist = 1
        pdf_compounding_dist(1) = 1
        
        d_max = k_max
        If d_max > Max_d_max Then
            Max_d_max = d_max
        End If
        
        ReDim Preserve cdfDemand(0 To Max_d_max)
        ReDim Preserve pdfDemand(0 To Max_d_max)
        
        For d = 0 To d_max
            pdfDemand(d) = Poisson_pdf(d)
            cdfDemand(d) = Poisson_cdf(d)
        Next
        
        cdfDemandmax = d_max
        
        If cdfDemand(d_max) > 1 Then
            Stop
        End If
        If cdfDemand(d_max) < 0.9999999999 Then
            Stop
        End If
        
  ElseIf CV_temp > 1 Then    'Compound Poisson demand with empirical compounding distribution
        
        max_comp = max_compounding_dist
        
'        ReDim pdf_compounding_dist(1 To max_comp)
'        ReDim pdf_compounding_quant(1 To max_comp)
        
'        sum_prob_compounding = 0
'        For i = 1 To max_comp
'            sum_prob_compounding = sum_prob_compounding + pdf_compounding_dist_n(j, i)
'            pdf_compounding_dist(i) = pdf_compounding_dist_n(j, i)
'            pdf_compounding_quant(i) = pdf_compounding_quant_n(j, i)
'        Next
'        If sum_prob_compounding > 1.000000001 Or sum_prob_compounding < 0.999999999 Then
'            Stop    'Investigate
'        End If
        
        i_max = pdf_compounding_quant(max_comp)
        d_max = k_max * i_max
        
        
    '*********convolution to find f_j_k for j=1 to d_max************
        'max_comp_tot = max_comp ^ k_max
        ReDim f_k_prob_vector(0 To k_max, 1 To max_comp)
        ReDim f_k_quant_vector(0 To k_max, 1 To max_comp)
        ReDim f_k_max_obs_vector(0 To k_max)
        
        Retailer = j
        Call Prob_dist_f_k_general(Retailer, k_max, i_max, d_max, pdf_compounding_dist, pdf_compounding_quant, max_comp, f_k_prob_vector, f_k_quant_vector, f_k_max_obs_vector)
        
        'ReDim f_j_k_vector(0 To d_max, 0 To k_max)
        'Call Prob_dist_f_j_k_general(j, k_max, i_max, d_max, pdf_compounding_dist, f_j_k_vector)
        
    '*****Determining the pdf and cdf of the demand during time interval t******************
        
        If d_max > Max_d_max Then
            Max_d_max = d_max
        End If
        
        ReDim Preserve cdfDemand(0 To Max_d_max)
        ReDim Preserve pdfDemand(0 To Max_d_max)
        ReDim f_k_start_search(0 To k_max)
        
        For i = 0 To k_max
            f_k_start_search(i) = 1
        Next
        
        'test utskrift
        'Stop
        If 1 = 0 Then
            For k = 1 To k_max
                For obs_nr = 1 To f_k_max_obs_vector(k)
                    output.Cells(9 + obs_nr, (9 + (k - 1) * 2) + k) = f_k_quant_vector(k, obs_nr)
                    output.Cells(9 + obs_nr, (9 + (k - 1) * 2) + k + 1) = f_k_prob_vector(k, obs_nr)
                Next
            Next
        End If
        
        cum_prob = 0
        d = -1
        'Stop
        Do While d < d_max      'cum_prob < 0.9999999999 And
            d = d + 1
            prob_demand_d = 0
            sum_prob_d = 0
            If d = 0 Then
                prob_d_k = f_k_prob_vector(0, 1)
                sum_prob_d = sum_prob_d + Poisson_pdf(0) * prob_d_k
            Else
                For k = 1 To k_max
                    prob_d_k = 0
                    y = f_k_start_search(k)
                    If f_k_quant_vector(k, y) = d Then
                        prob_d_k = f_k_prob_vector(k, y)
                        prob_demand_d = prob_demand_d + prob_d_k
                        If y < f_k_max_obs_vector(k) Then
                            f_k_start_search(k) = f_k_start_search(k) + 1
                        End If
                    ElseIf (f_k_quant_vector(k, y) < d) And (y < f_k_max_obs_vector(k)) Then
                        Do While f_k_quant_vector(k, y) <= d And y < f_k_max_obs_vector(k)
                            y = y + 1
                        Loop
                        y = y - 1
                        If f_k_quant_vector(k, y) = d Then
                            prob_d_k = f_k_prob_vector(k, y)
                            prob_demand_d = prob_demand_d + prob_d_k
                            If y < f_k_max_obs_vector(k) Then
                                f_k_start_search(k) = y + 1
                            End If
                        End If
                    ElseIf f_k_quant_vector(k, y) > d Then
                        'slh att k kunder efterfrÂgar d enheter =0
                    End If
                
                    sum_prob_d = sum_prob_d + Poisson_pdf(k) * prob_d_k        'f_j_k_vector(d, k)
                Next
            End If
            
            pdfDemand(d) = sum_prob_d
            cum_prob = cum_prob + sum_prob_d
            cdfDemand(d) = cum_prob
        Loop
        cdfDemandmax = d
        
        If cum_prob > 1 Then
            Stop
        End If
        If cum_prob < 0.999999999 Then
            Stop
        End If
  End If 'Prob_j
   
    Next
    
    'max_temp = 0
    'For j = 1 To N
    '    If max_temp < cdfDemandmax_n(j) Then
    '        max_temp = cdfDemandmax_n(j)
    '    End If
    'Next
    'Max_d_max = max_temp
    
    'test utskrift
    'Stop
    If 1 = 0 Then
        For j = 1 To 1
            For d = 0 To cdfDemandmax
                output.Cells(10 + d, 5) = d
                output.Cells(10 + d, 6) = cdfDemand(d)
                output.Cells(10 + d, 7) = pdfDemand(d)
            Next
        Next
    End If
    
    'ReDim Preserve cdfDemand(0 To Max_d_max)
    'ReDim Preserve pdfDemand(0 To Max_d_max)
End Sub
Sub Comp_Poisson_Empirical_Li_2_point_dist(vr_index, pdf_compounding_dist, pdf_compounding_quant, max_compounding_dist, cdfDemand, pdfDemand, cdfDemandmax, Li, Li_min, Li_max)
'Computes the leadtime demand when the leadtime follows a two point distribution with mean Li end points min_Li and max_Li

Dim i, j, k As Long
Dim cdfDemand_Li_min(), pdfDemand_Li_min(), cdfDemandmax_Li_min As Double
Dim cdfDemand_Li_max(), pdfDemand_Li_max(), cdfDemandmax_Li_max As Double
Dim prob_Li_max, prob_Li_min, cdf_sum As Double

    
prob_Li_max = (Li - Li_min) / Li_max - Li_min
prob_Li_min = 1 - prob_Li_max
cdf_sum = 0
If Li_min > 0 Then
    Call Compound_Poisson_Empirical_VR(vr_index, pdf_compounding_dist, pdf_compounding_quant, max_compounding_dist, cdfDemand_Li_min, pdfDemand_Li_min, cdfDemandmax_Li_min, Li_min)
    Call Compound_Poisson_Empirical_VR(vr_index, pdf_compounding_dist, pdf_compounding_quant, max_compounding_dist, cdfDemand_Li_max, pdfDemand_Li_max, cdfDemandmax_Li_max, Li_max)
    If cdfDemandmax_Li_max >= cdfDemandmax_Li_min Then
        cdfDemandmax = cdfDemandmax_Li_max
        For k = 0 To cdfDemandmax
            If k <= cdfDemandmax_Li_min Then
                pdfDemand(k) = prob_Li_max * pdfDemand_Li_max(k) + prob_Li_min * pdfDemand_Li_min(k)
            Else
                pdfDemand(k) = prob_Li_max * pdfDemand_Li_max(k)
            End If
            cdf_sum = cdf_sum + pdfDemand(k)
            cdfDemand(k) = cdf_sum
        Next
    Else
        Stop    'bˆr inte intr‰ffa kortare ledtid borde inneb‰ra l‰gre cdfDemandmax...
        cdfDemandmax = cdfDemandmax_Li_min
        For k = 0 To cdfDemandmax
            If k <= cdfDemandmax_Li_max Then
                pdfDemand(k) = prob_Li_max * pdfDemand_Li_max(k) + prob_Li_min * pdfDemand_Li_min(k)
            Else
                pdfDemand(k) = prob_Li_min * pdfDemand_Li_min(k)
            End If
            cdf_sum = cdf_sum + pdfDemand(k)
            cdfDemand(k) = cdf_sum
        Next
    End If
ElseIf Li_min = 0 Then
    Call Compound_Poisson_Empirical_VR(vr_index, pdf_compounding_dist, pdf_compounding_quant, max_compounding_dist, cdfDemand_Li_max, pdfDemand_Li_max, cdfDemandmax_Li_max, Li_max)
    cdfDemandmax = cdfDemandmax_Li_max
    ReDim Preserve pdfDemand(0 To cdfDemandmax)
    ReDim Preserve cdfDemand(0 To cdfDemandmax)
    For k = 0 To cdfDemandmax
            If k = 0 Then
                pdfDemand(k) = prob_Li_max * pdfDemand_Li_max(k) + prob_Li_min * 1
            Else
                pdfDemand(k) = prob_Li_max * pdfDemand_Li_max(k)
            End If
            cdf_sum = cdf_sum + pdfDemand(k)
            cdfDemand(k) = cdf_sum
    Next
End If

If Abs(cdf_sum - 1) > epsilon Then
    Stop
End If
    
End Sub

Sub Compound_Poisson_Empirical(pdf_compounding_dist_n, pdf_compounding_quant_n, max_compounding_dist_n, cdfDemand_n, pdfDemand_n, cdfDemandmax_n, L_temp)
    Dim i, j, k, k_max, d, d_max, Max_d_max, i_max, max_temp, y As Long
    Dim temp1, temp2, temp3, faktorial, cum_prob, lambda_j, prob_j, sum_prob, sum_prob_d As Double
    Dim Poisson_prob, Geometric_prob, t, CV_temp As Double
    Dim cdfDemand(), pdfDemand(), Poisspn_pdf(), Poisson_cdf(), pdf_compounding_dist(), f_j_k_vector(), f_k_prob_vector() As Double
    Dim pdf_compounding_quant(), max_compounding_dist(), f_k_quant_vector(), f_k_max_obs_vector(), f_k_start_search() As Long
    Dim max_comp, max_comp_tot, Retailer, obs_nr As Long
    Dim sum_prob_compounding, Expected_demand_quantity, prob_demand_d, prob_d_k  As Double
    Dim output As Excel.Worksheet
    
    
    Set output = Worksheets("Output")
    
    
    ReDim cdfDemandmax_n(1 To N)
    ReDim cdfDemand(0 To 1)
    ReDim pdfDemand(0 To 1)
    ReDim Poisson_pdf(0 To 1)
    ReDim Poisson_cdf(0 To 1)
    
    Max_d_max = 1
    For j = 1 To N
        
        CV_temp = (sigma_n(j) ^ 2) / m_n(j)
        t = L_temp(j)
    
        If Abs(CV_temp - 1) <= Exp(-10) Then
            temp1 = 0   'Poisson demand!!!
            CV_temp = 1
        End If
        If Ret_demand_choice = 3 And (CV_temp > 1) Then
            'Stop    'Condition for Poisson demand is not satisfied.
                    'Empirical compound Poisson is used instead
            Ret_demand_choice = 4
            interface.Cells(8, 2) = Ret_demand_choice
        End If
        If CV_temp < 1 Or (vr_index = j And VR_demand = 0) Then
            'Stop    'Conditions for compound Poisson demand are not satisfied. Change to Normal demand
            cdfDemandmax_n(j) = 0
        End If
        
    If CV_temp >= 1 Then
        
        Expected_demand_quantity = 0
        For i = 1 To max_compounding_dist_n(j)
            Expected_demand_quantity = Expected_demand_quantity + pdf_compounding_quant_n(j, i) * pdf_compounding_dist_n(j, i)
        Next
       
        lambda_j = m_n(j) / Expected_demand_quantity
       
        
    '***Determining max number of customers k and maximum demand d_max*************
        
        k_max = 0
        Poisson_prob = Exp(-1 * lambda_j * t)
        Poisson_pdf(0) = Poisson_prob
        sum_prob = Poisson_prob
        Poisson_cdf(k_max) = sum_prob
        Do While sum_prob < 0.99999999999
            k_max = k_max + 1
            Poisson_prob = Poisson_prob * (((lambda_j * t) / k_max))
            ReDim Preserve Poisson_pdf(0 To k_max)
            ReDim Preserve Poisson_cdf(0 To k_max)
            Poisson_pdf(k_max) = Poisson_prob
            sum_prob = sum_prob + Poisson_prob
            Poisson_cdf(k_max) = sum_prob
        Loop
        If Poisson_prob > 1E-09 Then
            'Stop 'investigate
        End If
   Else
        'max_compounding_dist_n(j) = 0
   End If
  
  If CV_temp = 1 Then  'Pure Poisson demand
        
        max_compounding_dist_n(j) = 1
        pdf_compounding_dist_n(j, 1) = 1
        
        d_max = k_max
        If d_max > Max_d_max Then
            Max_d_max = d_max
        End If
        
        ReDim Preserve cdfDemand_n(1 To N, 0 To Max_d_max)
        ReDim Preserve pdfDemand_n(1 To N, 0 To Max_d_max)
        
        For d = 0 To d_max
            pdfDemand_n(j, d) = Poisson_pdf(d)
            cdfDemand_n(j, d) = Poisson_cdf(d)
        Next
        
        cdfDemandmax_n(j) = d_max
        
        If cdfDemand_n(j, d_max) > 1 Then
            Stop
        End If
        If cdfDemand_n(j, d_max) < 0.9999999999 Then
            Stop
        End If
        
  ElseIf CV_temp > 1 Then    'Compound Poisson demand with empirical compounding distribution
        
        max_comp = max_compounding_dist_n(j)
        
        ReDim pdf_compounding_dist(1 To max_comp)
        ReDim pdf_compounding_quant(1 To max_comp)
        
        sum_prob_compounding = 0
        For i = 1 To max_comp
            sum_prob_compounding = sum_prob_compounding + pdf_compounding_dist_n(j, i)
            pdf_compounding_dist(i) = pdf_compounding_dist_n(j, i)
            pdf_compounding_quant(i) = pdf_compounding_quant_n(j, i)
        Next
        If sum_prob_compounding > 1.000000001 Or sum_prob_compounding < 0.999999999 Then
            Stop    'Investigate
        End If
        
        i_max = pdf_compounding_quant_n(j, max_comp)
        d_max = k_max * i_max
        
        
    '*********convolution to find f_j_k for j=1 to d_max************
        'max_comp_tot = max_comp ^ k_max
        ReDim f_k_prob_vector(0 To k_max, 1 To max_comp)
        ReDim f_k_quant_vector(0 To k_max, 1 To max_comp)
        ReDim f_k_max_obs_vector(0 To k_max)
        
        Retailer = j
        Call Prob_dist_f_k_general(Retailer, k_max, i_max, d_max, pdf_compounding_dist, pdf_compounding_quant, max_comp, f_k_prob_vector, f_k_quant_vector, f_k_max_obs_vector)
        
        'ReDim f_j_k_vector(0 To d_max, 0 To k_max)
        'Call Prob_dist_f_j_k_general(j, k_max, i_max, d_max, pdf_compounding_dist, f_j_k_vector)
        
    '*****Determining the pdf and cdf of the demand during time interval t******************
        
        If d_max > Max_d_max Then
            Max_d_max = d_max
        End If
        
        ReDim Preserve cdfDemand_n(1 To N, 0 To Max_d_max)
        ReDim Preserve pdfDemand_n(1 To N, 0 To Max_d_max)
        ReDim f_k_start_search(0 To k_max)
        
        For i = 0 To k_max
            f_k_start_search(i) = 1
        Next
        
        'test utskrift
        'Stop
        If 1 = 0 Then
            For k = 1 To k_max
                For obs_nr = 1 To f_k_max_obs_vector(k)
                    output.Cells(9 + obs_nr, (9 + (k - 1) * 2) + k) = f_k_quant_vector(k, obs_nr)
                    output.Cells(9 + obs_nr, (9 + (k - 1) * 2) + k + 1) = f_k_prob_vector(k, obs_nr)
                Next
            Next
        End If
        
        cum_prob = 0
        d = -1
        'Stop
        Do While d < d_max      'cum_prob < 0.9999999999 And
            d = d + 1
            prob_demand_d = 0
            sum_prob_d = 0
            If d = 0 Then
                prob_d_k = f_k_prob_vector(0, 1)
                sum_prob_d = sum_prob_d + Poisson_pdf(0) * prob_d_k
            Else
                For k = 1 To k_max
                    prob_d_k = 0
                    y = f_k_start_search(k)
                    If f_k_quant_vector(k, y) = d Then
                        prob_d_k = f_k_prob_vector(k, y)
                        prob_demand_d = prob_demand_d + prob_d_k
                        If y < f_k_max_obs_vector(k) Then
                            f_k_start_search(k) = f_k_start_search(k) + 1
                        End If
                    ElseIf (f_k_quant_vector(k, y) < d) And (y < f_k_max_obs_vector(k)) Then
                        Do While f_k_quant_vector(k, y) <= d And y < f_k_max_obs_vector(k)
                            y = y + 1
                        Loop
                        y = y - 1
                        If f_k_quant_vector(k, y) = d Then
                            prob_d_k = f_k_prob_vector(k, y)
                            prob_demand_d = prob_demand_d + prob_d_k
                            If y < f_k_max_obs_vector(k) Then
                                f_k_start_search(k) = y + 1
                            End If
                        End If
                    ElseIf f_k_quant_vector(k, y) > d Then
                        'slh att k kunder efterfrÂgar d enheter =0
                    End If
                
                    sum_prob_d = sum_prob_d + Poisson_pdf(k) * prob_d_k        'f_j_k_vector(d, k)
                Next
            End If
            
            pdfDemand_n(j, d) = sum_prob_d
            cum_prob = cum_prob + sum_prob_d
            cdfDemand_n(j, d) = cum_prob
        Loop
        cdfDemandmax_n(j) = d
        
        If cum_prob > 1 Then
            Stop
        End If
        If cum_prob < 0.999999999 Then
            Stop
        End If
  End If 'Prob_j
   
    Next
    
    max_temp = 0
    For j = 1 To N
        If max_temp < cdfDemandmax_n(j) Then
            max_temp = cdfDemandmax_n(j)
        End If
    Next
    Max_d_max = max_temp
    
    'test utskrift
    'Stop
    If 1 = 0 Then
        For j = 1 To 1
            For d = 0 To cdfDemandmax_n(j)
                output.Cells(10 + d, 5) = d
                output.Cells(10 + d, 6) = cdfDemand_n(j, d)
                output.Cells(10 + d, 7) = pdfDemand_n(j, d)
            Next
        Next
    End If
    
    ReDim Preserve cdfDemand_n(1 To N, 0 To Max_d_max)
    ReDim Preserve pdfDemand_n(1 To N, 0 To Max_d_max)
End Sub

Private Sub Prob_dist_f_k_general(j, k_max, i_max, d_max, pdf_compounding_dist, pdf_compounding_quant, max_comp, f_k_prob_vector, f_k_quant_vector, f_k_max_obs_vector)
    
    Dim i, d, k, obs_nr, max_obs_temp, max_obs_sum As Long
    Dim temp1, temp2, f_j_k, sum_prob, sum_i As Double
    Dim f_k_prob_vector_temp() As Double
    Dim f_k_quant_vector_temp(), d_min, i_min As Long
    Dim Sort_stop As Boolean
    
    
    'Note: d denotes the total amount of demanded units which in
    'Axs‰ter 2006 is referred to as j
    
    
    'Initiering
    'For k=0....
     f_k_prob_vector(0, 1) = 1
     f_k_quant_vector(0, 1) = 0
     f_k_max_obs_vector(0) = 1
    
    'For k=1.....
    For obs_nr = 1 To max_comp
        f_k_prob_vector(1, obs_nr) = pdf_compounding_dist(obs_nr)
        f_k_quant_vector(1, obs_nr) = pdf_compounding_quant(obs_nr)
    Next
     f_k_max_obs_vector(1) = max_comp
     

    For k = 2 To k_max
        max_obs_temp = f_k_max_obs_vector(k - 1) * max_comp
        ReDim f_k_prob_vector_temp(1 To max_obs_temp)
        ReDim f_k_quant_vector_temp(1 To max_obs_temp)

        obs_nr = 0
        For i = 1 To f_k_max_obs_vector(k - 1)
            For j = 1 To max_comp
                obs_nr = obs_nr + 1
                f_k_prob_vector_temp(obs_nr) = f_k_prob_vector(k - 1, i) * f_k_prob_vector(1, j)
                f_k_quant_vector_temp(obs_nr) = f_k_quant_vector(k - 1, i) + f_k_quant_vector(1, j)
            Next
        Next
        
        'Sorting and aggregating observed demand quantities and their corresponding probabilities
        For i = 1 To max_obs_temp
            d = f_k_quant_vector_temp(i)
            If d > 0 Then
                For j = i + 1 To max_obs_temp
                    If f_k_quant_vector_temp(j) = d Then
                        f_k_quant_vector_temp(j) = 0
                        f_k_prob_vector_temp(i) = f_k_prob_vector_temp(i) + f_k_prob_vector_temp(j)
                    End If
                Next
            End If
        Next
        
        ReDim Preserve f_k_prob_vector(0 To k_max, 1 To max_obs_temp)
        ReDim Preserve f_k_quant_vector(0 To k_max, 1 To max_obs_temp)
        
        Sort_stop = False
        obs_nr = 0
        i_min = 1
        Do While Sort_stop = False
            Sort_stop = True
            d_min = d_max + 10
            For i = 1 To max_obs_temp
                d = f_k_quant_vector_temp(i)
                If d > 0 And d < d_min Then
                    d_min = d
                    i_min = i
                End If
                If d > 0 Then
                    Sort_stop = False
                End If
            Next
            If Sort_stop = False Then
                obs_nr = obs_nr + 1
                f_k_quant_vector(k, obs_nr) = f_k_quant_vector_temp(i_min)
                f_k_prob_vector(k, obs_nr) = f_k_prob_vector_temp(i_min)
                f_k_max_obs_vector(k) = obs_nr
                f_k_quant_vector_temp(i_min) = 0
            End If
        Loop
        max_obs_temp = f_k_max_obs_vector(k)
        ReDim Preserve f_k_prob_vector(0 To k_max, 1 To max_obs_temp)
        ReDim Preserve f_k_prob_vector(0 To k_max, 1 To max_obs_temp)
        
    Next
    
            
    '****** test*****
    For k = 1 To k_max
        sum_prob = 0
        For obs_nr = 1 To f_k_max_obs_vector(k)
            sum_prob = sum_prob + f_k_prob_vector(k, obs_nr)
        Next
        If sum_prob < 0.999999 Then
            Stop 'investigate
        End If
    Next
    
    
End Sub

Private Sub Prob_dist_f_j_k_general(j, k_max, i_max, d_max, pdf_compounding_dist, f_j_k_vector)

    Dim i, d, k As Long
    Dim temp1, temp2, f_j_k, sum_prob, sum_i As Double
    
    'Note: d denotes the total amount of demanded units which in
    'Axs‰ter 2006 is referred to as j
    
    For d = 0 To i_max
        
        If d = 0 Then
             f_j_k_vector(d, 0) = 1
             f_j_k_vector(d, 1) = 0
        ElseIf d >= 1 Then
             f_j_k_vector(d, 1) = pdf_compounding_dist(d)
             f_j_k_vector(d, 0) = 0
        End If
    Next
    
    For d = 1 To d_max
        For k = 1 To min(d, k_max)
            sum_i = 0
            For i = (k - 1) To (d - 1)
                sum_i = sum_i + f_j_k_vector(i, k - 1) * f_j_k_vector(d - i, 1)
            Next
            f_j_k_vector(d, k) = sum_i
        Next
    Next
            
    '****** test*****
    For k = 1 To k_max
        sum_prob = 0
        For d = 1 To d_max
            sum_prob = sum_prob + f_j_k_vector(d, k)
        Next
        If sum_prob < 0.999999999 Then
            Stop 'invetsigate
        End If
    Next
    
    
End Sub

Sub Compound_Poisson_geometric_prob(cdfDemand_n, pdfDemand_n, cdfDemandmax_n, L_temp)

    Dim i, j, k, k_max, d, d_max, Max_d_max, i_max, max_temp, y As Long
    Dim temp1, temp2, temp3, faktorial, cum_prob, lambda_j, prob_j, sum_prob, sum_prob_d As Double
    Dim Poisson_prob, Geometric_prob, t, CV_temp As Double
    Dim cdfDemand(), pdfDemand(), Poisspn_pdf(), Poisson_cdf(), f_j_k_vector() As Double
    Dim max_compounding As Long
    Dim sum_prob_compounding As Double
    
    
    ReDim cdfDemandmax_n(1 To N)
    ReDim cdfDemand(0 To 1)
    ReDim pdfDemand(0 To 1)
    ReDim Poisson_pdf(0 To 1)
    ReDim Poisson_cdf(0 To 1)
    ReDim max_compounding_dist_n(1 To N)
    ReDim pdf_compounding_dist_n(1 To N, 1 To 1)
    ReDim pdf_compounding_quant_n(1 To N, 1 To 1)
    
    max_compounding = 1
    Max_d_max = 1
    For j = 1 To N
        
        t = L_temp(j)
    
    '**Determining the parameters lambda and p from data on mean and stdDev of demand per time unit*******
        temp1 = sigma_n(j) ^ 2 - m_n(j)
        CV_temp = (sigma_n(j) ^ 2) / m_n(j)
        If Abs(CV_temp - 1) <= Exp(-10) Then
            temp1 = 0   'Poisson demand!!!
            CV_temp = 1
        End If
        If Ret_demand_choice = 3 And (CV_temp > 1) Then
            Stop    'Condition for Poisson demand is not satisfied.
                    'Geometric compound Poisson is used instead
            Ret_demand_choice = 2
            interface.Cells(8, 2) = Ret_demand_choice
        End If
        If CV_temp < 1 Then
            'Stop    'Conditions for compound Poisson demand are not satisfied. Change to Normal demand
            cdfDemandmax_n(j) = 0
        End If
        
  If CV_temp >= 1 Then
        temp2 = m_n(j) + sigma_n(j) ^ 2
        prob_j = temp1 / temp2      'prob_j=0 mean Poisson demand!!
        If prob_j < 0 Or prob_j > 1 Then
            Stop    'This should not be possible, demand does not fit the distribution
        End If
  
        lambda_j = m_n(j) * (1 - prob_j)
        temp1 = (2 * (m_n(j)) ^ 2) / (sigma_n(j) ^ 2 + m_n(j))
        If Abs(lambda_j - temp1) > 1E-11 Then
            Stop    'investigate
        End If
        
    '***Determining max number of customers k and maximum demand d_max*************
        
        k_max = 0
        Poisson_prob = Exp(-1 * lambda_j * t)
        Poisson_pdf(0) = Poisson_prob
        sum_prob = Poisson_prob
        Poisson_cdf(k_max) = sum_prob
        Do While sum_prob < 0.99999999999
            k_max = k_max + 1
            Poisson_prob = Poisson_prob * (((lambda_j * t) / k_max))
            ReDim Preserve Poisson_pdf(0 To k_max)
            ReDim Preserve Poisson_cdf(0 To k_max)
            Poisson_pdf(k_max) = Poisson_prob
            sum_prob = sum_prob + Poisson_prob
            Poisson_cdf(k_max) = sum_prob
        Loop
        If Poisson_prob > 1E-09 Then
            Stop 'investigate
        End If
   Else
        max_compounding_dist_n(j) = 0
   End If
  
  If CV_temp = 1 Then  'Pure Poisson demand
        
        max_compounding_dist_n(j) = 1
        pdf_compounding_dist_n(j, 1) = 1
        pdf_compounding_quant_n(j, 1) = 1
        
        d_max = k_max
        If d_max > Max_d_max Then
            Max_d_max = d_max
        End If
        
        ReDim Preserve cdfDemand_n(1 To N, 0 To Max_d_max)
        ReDim Preserve pdfDemand_n(1 To N, 0 To Max_d_max)
        
        For d = 0 To d_max
            pdfDemand_n(j, d) = Poisson_pdf(d)
            cdfDemand_n(j, d) = Poisson_cdf(d)
        Next
        
        cdfDemandmax_n(j) = d_max
        
        If cdfDemand_n(j, d_max) > 1 Then
            Stop
        End If
        If cdfDemand_n(j, d_max) < 0.9999999999 Then
            Stop
        End If
        
  ElseIf CV_temp > 1 Then    'Compound Poisson demand with geometric compounding distribution
        
        i_max = 1
        Geometric_prob = (1 - prob_j)
        sum_prob = Geometric_prob
        pdf_compounding_dist_n(j, i_max) = Geometric_prob
        pdf_compounding_quant_n(j, i_max) = i_max
        
        Do While sum_prob < 0.99999999999
            i_max = i_max + 1
            Geometric_prob = Geometric_prob * (prob_j)
            sum_prob = sum_prob + Geometric_prob
            If i_max > max_compounding Then
                max_compounding = i_max
                ReDim Preserve pdf_compounding_dist_n(1 To N, 1 To i_max)
                ReDim Preserve pdf_compounding_quant_n(1 To N, 1 To i_max)
            End If
            pdf_compounding_dist_n(j, i_max) = Geometric_prob
            pdf_compounding_quant_n(j, i_max) = i_max
        Loop
        max_compounding_dist_n(j) = i_max
        
        ReDim pdf_compounding_dist(1 To i_max)
        ReDim pdf_compounding_quant(1 To i_max)
        
        If (prob_j > 0) And (Geometric_prob > 1E-09) Then
            Stop 'investigate
        End If
        sum_prob_compounding = 0
        For i = 1 To i_max
            sum_prob_compounding = sum_prob_compounding + pdf_compounding_dist_n(j, i)
            pdf_compounding_dist(i) = pdf_compounding_dist_n(j, i)
            pdf_compounding_quant(i) = pdf_compounding_quant_n(j, i)
        Next
        If sum_prob_compounding > 1 Or sum_prob_compounding < 0.99999999999 Then
            Stop    'Investigate
        End If
        
        d_max = k_max * i_max
        
        
    '*********convolution to find f_j_k for j=1 to d_max************
        ReDim f_j_k_vector(0 To d_max, 0 To k_max)
        Call Prob_dist_f_j_k(j, k_max, i_max, d_max, prob_j, f_j_k_vector)
        'Call Prob_dist_f_j_k_general(j, k_max, i_max, d_max, pdf_compounding_dist, f_j_k_vector)
    '*****Determining the pdf and cdf of the demand during time interval t******************
        
        If d_max > Max_d_max Then
            Max_d_max = d_max
        End If
        
        ReDim Preserve cdfDemand_n(1 To N, 0 To Max_d_max)
        ReDim Preserve pdfDemand_n(1 To N, 0 To Max_d_max)
        
        cum_prob = 0
        d = -1
        'Stop
        Do While cum_prob < 0.9999999999 And d < d_max
            d = d + 1
            sum_prob_d = 0
            For k = 0 To k_max
                sum_prob_d = sum_prob_d + Poisson_pdf(k) * f_j_k_vector(d, k)
            Next
            pdfDemand_n(j, d) = sum_prob_d
            cum_prob = cum_prob + sum_prob_d
            cdfDemand_n(j, d) = cum_prob
        Loop
        cdfDemandmax_n(j) = d
        
        If cum_prob > 1 Then
            Stop
        End If
        If cum_prob < 0.9999999999 Then
            Stop
        End If
  End If 'Prob_j
   
    Next
    
    max_temp = 0
    For j = 1 To N
        If max_temp < cdfDemandmax_n(j) Then
            max_temp = cdfDemandmax_n(j)
        End If
    Next
    Max_d_max = max_temp
    
    ReDim Preserve cdfDemand_n(1 To N, 0 To Max_d_max)
    ReDim Preserve pdfDemand_n(1 To N, 0 To Max_d_max)
    
 
End Sub

Sub Compound_Poisson_geometric_prob_Li_demand(CW_demand_choice, R0, Q0, cdfDemand_n, pdfDemand_n, cdfDemandmax_n, delay_temp)

    Dim i, j, k, k_max, d, d_max, Max_d_max, i_max, max_temp, y As Long
    Dim temp1, temp2, temp3, faktorial, cum_prob, lambda_j, prob_j, sum_prob, sum_prob_d As Double
    Dim Poisson_prob, Geometric_prob, CV_temp As Double
    Dim cdfDemand(), pdfDemand(), Poisspn_pdf(), Poisson_cdf(), f_j_k_vector() As Double
    Dim max_compounding As Long
    Dim sum_prob_compounding As Double
    Dim mean_L_n(), sigma_L_n() As Double
    
    
    ReDim cdfDemandmax_n(1 To N)
    ReDim cdfDemand(0 To 1)
    ReDim pdfDemand(0 To 1)
    ReDim Poisson_pdf(0 To 1)
    ReDim Poisson_cdf(0 To 1)
    ReDim max_compounding_dist_n(1 To N)
    ReDim pdf_compounding_dist_n(1 To N, 1 To 1)
    ReDim pdf_compounding_quant_n(1 To N, 1 To 1)
    
    ReDim mean_L_n(1 To N)
    ReDim sigma_L_n(1 To N)
    
    ReDim Var_leadtime_demand(1 To N)
    
    max_compounding = 1
    Max_d_max = 1
    
    If Leadtime_choice = 4 Then
        'Stop
        Call Leadtime_mean_and_variance_approx(CW_demand_choice, pdfN, pmax, R0, Q0, trp_n, mean_L_n, sigma_L_n)
    End If
    
    For j = 1 To N
        
        If Leadtime_choice = 3 Then  '(Assumption: delivery delay is exponentially distributed)
        
            'sigma_L_n(j) = 0
            mean_L_n(j) = trp_n(j) + delay_temp(j)
            sigma_L_n(j) = delay_temp(j)
            
            mean_leadtime_demand_n(j) = mean_L_n(j) * m_n(j)
            sigma_leadtime_demand_n(j) = Sqr((sigma_n(j) ^ 2) * mean_L_n(j) + (m_n(j) ^ 2) * (sigma_L_n(j) ^ 2))
        
        ElseIf Leadtime_choice = 4 Then
            'Stop
            'Call Leadtime_mean_and_variance_approx(CW_demand_choice, pdfN, pmax, R0, Q0, trp_n, mean_L_n, sigma_L_n)
            
            mean_leadtime_demand_n(j) = mean_L_n(j) * m_n(j)
            sigma_leadtime_demand_n(j) = Sqr((sigma_n(j) ^ 2) * mean_L_n(j) + (m_n(j) ^ 2) * (sigma_L_n(j) ^ 2))
        Else
            mean_L_n(j) = trp_n(j) + delay_temp(j)
            mean_leadtime_demand_n(j) = mean_L_n(j) * m_n(j)
            sigma_leadtime_demand_n(j) = Sqr(mean_L_n(j)) * sigma_n(j)
        End If
        
        't = mean_L_n(j)
    
    '**Determining the parameters lambda and p from data on mean and stdDev of demand per time unit*******
   
        temp1 = sigma_leadtime_demand_n(j) ^ 2 - mean_leadtime_demand_n(j)
        CV_temp = sigma_leadtime_demand_n(j) ^ 2 / mean_leadtime_demand_n(j)
    
        'temp1 = sigma_n(j) ^ 2 - m_n(j)
        'CV_temp = (sigma_n(j) ^ 2) / m_n(j)
    
        If Abs(CV_temp - 1) <= Exp(-10) Then
            temp1 = 0   'Poisson demand!!!
            CV_temp = 1
        End If
        If Ret_demand_choice = 3 And (CV_temp > 1) Then
            Stop    'Condition for Poisson demand is not satisfied.
                    'Geometric compound Poisson is used instead
            Ret_demand_choice = 2
            interface.Cells(8, 2) = Ret_demand_choice
        End If
        If CV_temp < 1 Then
            'Stop    'Conditions for compound Poisson demand are not satisfied. Change to Normal demand
            cdfDemandmax_n(j) = 0
        End If
        
  If CV_temp >= 1 Then
    
        temp2 = sigma_leadtime_demand_n(j) ^ 2 + mean_leadtime_demand_n(j)
        'temp2 = m_n(j) + sigma_n(j) ^ 2
       
        
        prob_j = temp1 / temp2      'prob_j=0 mean Poisson demand!!
        If prob_j < 0 Or prob_j > 1 Then
            Stop    'This should not be possible, demand does not fit the distribution
        End If
  
        lambda_j = mean_leadtime_demand_n(j) * (1 - prob_j)
        'lambda_j = m_n(j) * (1 - prob_j)
        
        temp1 = (2 * (mean_leadtime_demand_n(j)) ^ 2) / (sigma_leadtime_demand_n(j) ^ 2 + mean_leadtime_demand_n(j))
        If Abs(lambda_j - temp1) > 1E-11 Then
            Stop    'investigate
        End If
        
    '***Determining max number of customers k and maximum demand d_max*************
        
        k_max = 0
        Poisson_prob = Exp(-1 * lambda_j)
        Poisson_pdf(0) = Poisson_prob
        sum_prob = Poisson_prob
        Poisson_cdf(k_max) = sum_prob
        Do While sum_prob < 0.99999999999
            k_max = k_max + 1
            Poisson_prob = Poisson_prob * (((lambda_j) / k_max))
            ReDim Preserve Poisson_pdf(0 To k_max)
            ReDim Preserve Poisson_cdf(0 To k_max)
            Poisson_pdf(k_max) = Poisson_prob
            sum_prob = sum_prob + Poisson_prob
            Poisson_cdf(k_max) = sum_prob
        Loop
        If Poisson_prob > 1E-09 Then
            Stop 'investigate
        End If
   Else
        max_compounding_dist_n(j) = 0
   End If
  
  If CV_temp = 1 Then  'Pure Poisson demand
        
        max_compounding_dist_n(j) = 1
        pdf_compounding_dist_n(j, 1) = 1
        pdf_compounding_quant_n(j, 1) = 1
        
        d_max = k_max
        If d_max > Max_d_max Then
            Max_d_max = d_max
        End If
        
        ReDim Preserve cdfDemand_n(1 To N, 0 To Max_d_max)
        ReDim Preserve pdfDemand_n(1 To N, 0 To Max_d_max)
        
        For d = 0 To d_max
            pdfDemand_n(j, d) = Poisson_pdf(d)
            cdfDemand_n(j, d) = Poisson_cdf(d)
        Next
        
        cdfDemandmax_n(j) = d_max
        
        If cdfDemand_n(j, d_max) > 1 Then
            Stop
        End If
        If cdfDemand_n(j, d_max) < 0.9999999999 Then
            Stop
        End If
        
  ElseIf CV_temp > 1 Then    'Compound Poisson demand with geometric compounding distribution
        
        i_max = 1
        Geometric_prob = (1 - prob_j)
        sum_prob = Geometric_prob
        pdf_compounding_dist_n(j, i_max) = Geometric_prob
        pdf_compounding_quant_n(j, i_max) = i_max
        
        Do While sum_prob < 0.99999999999
            i_max = i_max + 1
            Geometric_prob = Geometric_prob * (prob_j)
            sum_prob = sum_prob + Geometric_prob
            If i_max > max_compounding Then
                max_compounding = i_max
                ReDim Preserve pdf_compounding_dist_n(1 To N, 1 To i_max)
                ReDim Preserve pdf_compounding_quant_n(1 To N, 1 To i_max)
            End If
            'ReDim Preserve pdf_compounding_dist_n(1 To N, 1 To i_max)
            'ReDim Preserve pdf_compounding_quant_n(1 To N, 1 To i_max)
            
            pdf_compounding_dist_n(j, i_max) = Geometric_prob
            pdf_compounding_quant_n(j, i_max) = i_max
        Loop
        max_compounding_dist_n(j) = i_max
        
        ReDim pdf_compounding_dist(1 To i_max)
        ReDim pdf_compounding_quant(1 To i_max)
        
        If (prob_j > 0) And (Geometric_prob > 1E-09) Then
            Stop 'investigate
        End If
        sum_prob_compounding = 0
        For i = 1 To i_max
            sum_prob_compounding = sum_prob_compounding + pdf_compounding_dist_n(j, i)
            pdf_compounding_dist(i) = pdf_compounding_dist_n(j, i)
            pdf_compounding_quant(i) = pdf_compounding_quant_n(j, i)
        Next
        If sum_prob_compounding > 1 Or sum_prob_compounding < 0.99999999999 Then
            Stop    'Investigate
        End If
        
        d_max = k_max * i_max
        
        
    '*********convolution to find f_j_k for j=1 to d_max************
        ReDim f_j_k_vector(0 To d_max, 0 To k_max)
        Call Prob_dist_f_j_k(j, k_max, i_max, d_max, prob_j, f_j_k_vector)
        'Call Prob_dist_f_j_k_general(j, k_max, i_max, d_max, pdf_compounding_dist, f_j_k_vector)
    '*****Determining the pdf and cdf of the demand during the leadtime Li******************
        
        If d_max > Max_d_max Then
            Max_d_max = d_max
        End If
        
        ReDim Preserve cdfDemand_n(1 To N, 0 To Max_d_max)
        ReDim Preserve pdfDemand_n(1 To N, 0 To Max_d_max)
        
        cum_prob = 0
        d = -1
        'Stop
        Do While cum_prob < 0.9999999999 And d < d_max
            d = d + 1
            sum_prob_d = 0
            For k = 0 To k_max
                sum_prob_d = sum_prob_d + Poisson_pdf(k) * f_j_k_vector(d, k)
            Next
            pdfDemand_n(j, d) = sum_prob_d
            cum_prob = cum_prob + sum_prob_d
            cdfDemand_n(j, d) = cum_prob
        Loop
        cdfDemandmax_n(j) = d
        
        If cum_prob > 1 Then
            Stop
        End If
        If cum_prob < 0.9999999999 Then
            Stop
        End If
  End If 'Prob_j
   
    Next
    
    max_temp = 0
    For j = 1 To N
        If max_temp < cdfDemandmax_n(j) Then
            max_temp = cdfDemandmax_n(j)
        End If
    Next
    Max_d_max = max_temp
    
    ReDim Preserve cdfDemand_n(1 To N, 0 To Max_d_max)
    ReDim Preserve pdfDemand_n(1 To N, 0 To Max_d_max)
    
 
End Sub
Sub Compound_Poisson_geometric_prob_VR(j_vr, cdfDemand, pdfDemand, cdfDemandmax, L_vr)

    Dim i, j, k, k_max, d, d_max, Max_d_max, i_max, max_temp, y As Long
    Dim temp1, temp2, temp3, faktorial, cum_prob, lambda_j, prob_j, sum_prob, sum_prob_d As Double
    Dim Poisson_prob, Geometric_prob, t, CV_temp As Double
    Dim Poisson_pdf(), Poisson_cdf(), f_j_k_vector() As Double
    Dim max_compounding As Long
    Dim sum_prob_compounding As Double
    
    
    'ReDim cdfDemandmax_n(1 To N)
    'ReDim cdfDemand(0 To 1)
    'ReDim pdfDemand(0 To 1)
    ReDim Poisson_pdf(0 To 1)
    ReDim Poisson_cdf(0 To 1)
    'ReDim max_compounding_dist_n(1 To N)
    'ReDim pdf_compounding_dist_n(1 To N, 1 To 1)
    'ReDim pdf_compounding_quant_n(1 To N, 1 To 1)
    
    max_compounding = 1
    Max_d_max = 1
    For j = j_vr To j_vr
        
        t = L_vr
    
    '**Determining the parameters lambda and p from data on mean and stdDev of demand per time unit*******
        temp1 = sigma_n(j) ^ 2 - m_n(j)
        CV_temp = (sigma_n(j) ^ 2) / m_n(j)
        If Abs(CV_temp - 1) <= Exp(-10) Then
            temp1 = 0   'Poisson demand!!!
            CV_temp = 1
        End If
        If Ret_demand_choice = 3 And (CV_temp > 1) Then
            Stop    'Condition for Poisson demand is not satisfied.
                    'Geometric compound Poisson is used instead
            Ret_demand_choice = 2
            interface.Cells(8, 2) = Ret_demand_choice
        End If
       
        
        If CV_temp < 1 Or (vr_index = j And VR_demand = 0) Then
            Stop    'Conditions for compound Poisson demand are not satisfied. Change to Normal demand
            cdfDemandmax_n(j) = 0
            max_compounding_dist_n(j) = 0
        End If

        
        
  If CV_temp >= 1 Then
        temp2 = m_n(j) + sigma_n(j) ^ 2
        prob_j = temp1 / temp2      'prob_j=0 mean Poisson demand!!
        If prob_j < 0 Or prob_j > 1 Then
            Stop    'This should not be possible, demand does not fit the distribution
        End If
  
        lambda_j = m_n(j) * (1 - prob_j)
        temp1 = (2 * (m_n(j)) ^ 2) / (sigma_n(j) ^ 2 + m_n(j))
        If Abs(lambda_j - temp1) > 1E-11 Then
            Stop    'investigate
        End If
        
    '***Determining max number of customers k and maximum demand d_max*************
        
        k_max = 0
        Poisson_prob = Exp(-1 * lambda_j * t)
        Poisson_pdf(0) = Poisson_prob
        sum_prob = Poisson_prob
        Poisson_cdf(k_max) = sum_prob
        Do While sum_prob < 0.99999999999
            k_max = k_max + 1
            Poisson_prob = Poisson_prob * (((lambda_j * t) / k_max))
            ReDim Preserve Poisson_pdf(0 To k_max)
            ReDim Preserve Poisson_cdf(0 To k_max)
            Poisson_pdf(k_max) = Poisson_prob
            sum_prob = sum_prob + Poisson_prob
            Poisson_cdf(k_max) = sum_prob
        Loop
        If Poisson_prob > 1E-09 Then
            Stop 'investigate
        End If
   Else
        max_compounding_dist_n(j) = 0
   End If
  
  If CV_temp = 1 Then  'Pure Poisson demand
        
        'max_compounding_dist_n(j) = 1
        'pdf_compounding_dist_n(j, 1) = 1
        'pdf_compounding_quant_n(j, 1) = 1
        
        d_max = k_max
        If d_max > Max_d_max Then
            Max_d_max = d_max
        End If
        
        'ReDim Preserve cdfDemand_n(1 To N, 0 To Max_d_max)
        'ReDim Preserve pdfDemand_n(1 To N, 0 To Max_d_max)
        
        For d = 0 To d_max
            pdfDemand(d) = Poisson_pdf(d)
            cdfDemand(d) = Poisson_cdf(d)
        Next
        
        'cdfDemandmax_n(j) = d_max
        cdfDemandmax = d_max
        
        If cdfDemand(d_max) > 1 Then
            Stop
        End If
        If cdfDemand(d_max) < 0.9999999999 Then
            Stop
        End If
        
  ElseIf CV_temp > 1 Then    'Compound Poisson demand with geometric compounding distribution
        
        
       If 1 = 0 Then    'The componding distributions are alreadt calculated earlier in the program and are available in the arrays
                        ' pdf_compounding_dist_n,pdf_compounding_quant_n and the max demannd size in max_compounding_dist_n
        i_max = 1
        Geometric_prob = (1 - prob_j)
        sum_prob = Geometric_prob
        pdf_compounding_dist_n(j, i_max) = Geometric_prob
        pdf_compounding_quant_n(j, i_max) = i_max
        
        Do While sum_prob < 0.99999999999
            i_max = i_max + 1
            Geometric_prob = Geometric_prob * (prob_j)
            sum_prob = sum_prob + Geometric_prob
            If i_max > max_compounding Then
                max_compounding = i_max
                ReDim Preserve pdf_compounding_dist_n(1 To N, 1 To i_max)
                ReDim Preserve pdf_compounding_quant_n(1 To N, 1 To i_max)
            End If
            pdf_compounding_dist_n(j, i_max) = Geometric_prob
            pdf_compounding_quant_n(j, i_max) = i_max
        Loop
        max_compounding_dist_n(j) = i_max
        
        ReDim pdf_compounding_dist(1 To i_max)
        ReDim pdf_compounding_quant(1 To i_max)
        
        If (prob_j > 0) And (Geometric_prob > 1E-09) Then
            Stop 'investigate
        End If
        sum_prob_compounding = 0
        For i = 1 To i_max
            sum_prob_compounding = sum_prob_compounding + pdf_compounding_dist_n(j, i)
            pdf_compounding_dist(i) = pdf_compounding_dist_n(j, i)
            pdf_compounding_quant(i) = pdf_compounding_quant_n(j, i)
        Next
        If sum_prob_compounding > 1 Or sum_prob_compounding < 0.99999999999 Then
            Stop    'Investigate
        End If
        
      Else
        i_max = max_compounding_dist_n(j)
      End If
      
       d_max = k_max * i_max
        
    '*********convolution to find f_j_k for j=1 to d_max************
        ReDim f_j_k_vector(0 To d_max, 0 To k_max)
        Call Prob_dist_f_j_k(j, k_max, i_max, d_max, prob_j, f_j_k_vector)
        'Call Prob_dist_f_j_k_general(j, k_max, i_max, d_max, pdf_compounding_dist, f_j_k_vector)
    '*****Determining the pdf and cdf of the demand during time interval t******************
        
        'If d_max > Max_d_max Then
        '    Max_d_max = d_max
        'End If
        
        'ReDim Preserve cdfDemand_n(1 To N, 0 To Max_d_max)
        'ReDim Preserve pdfDemand_n(1 To N, 0 To Max_d_max)
        
        ReDim Preserve cdfDemand(0 To d_max)
        ReDim Preserve pdfDemand(0 To d_max)
        
        cum_prob = 0
        d = -1
        'Stop
        Do While cum_prob < 0.9999999999 And d < d_max
            d = d + 1
            sum_prob_d = 0
            For k = 0 To k_max
                sum_prob_d = sum_prob_d + Poisson_pdf(k) * f_j_k_vector(d, k)
            Next
            pdfDemand(d) = sum_prob_d
            cum_prob = cum_prob + sum_prob_d
            cdfDemand(d) = cum_prob
        Loop
        'cdfDemandmax_n(j) = d
        cdfDemandmax = d
        
        If cum_prob > 1 Then
            Stop
        End If
        If cum_prob < 0.9999999999 Then
            Stop
        End If
  End If 'Prob_j
   
    Next
    
    'max_temp = 0
    'For j = 1 To N
    '    If max_temp < cdfDemandmax_n(j) Then
    '        max_temp = cdfDemandmax_n(j)
    '    End If
    'Next
    'Max_d_max = max_temp
    
    'ReDim Preserve cdfDemand_n(1 To N, 0 To Max_d_max)
    'ReDim Preserve pdfDemand_n(1 To N, 0 To Max_d_max)
    
 
End Sub

Private Sub Prob_dist_f_j_k(j, k_max, i_max, d_max, prob_j, f_j_k_vector)

    Dim i, d, k As Long
    Dim temp1, temp2, Geometric_prob, f_j_k, sum_prob, sum_i As Double
    
    'Note: d denotes the total amount of demanded units which in
    'Axs‰ter 2006 is referred to as j
    
    Geometric_prob = (1 - prob_j)
    For d = 0 To d_max
        
        If d = 0 Then
             f_j_k_vector(d, 0) = 1
             f_j_k_vector(d, 1) = 0
        ElseIf d = 1 Then
             f_j_k_vector(d, 1) = Geometric_prob
             f_j_k_vector(d, 0) = 0
        ElseIf d > 1 Then
            Geometric_prob = Geometric_prob * prob_j
            f_j_k_vector(d, 1) = Geometric_prob
            f_j_k_vector(d, 0) = 0
        End If
    Next
    
    For d = 1 To d_max
        For k = 1 To min(d, k_max)
            sum_i = 0
            For i = (k - 1) To (d - 1)
                sum_i = sum_i + f_j_k_vector(i, k - 1) * f_j_k_vector(d - i, 1)
            Next
            f_j_k_vector(d, k) = sum_i
        Next
    Next
            
    '****** test*****
    For k = 1 To k_max
        sum_prob = 0
        For d = 1 To d_max
            sum_prob = sum_prob + f_j_k_vector(d, k)
        Next
        If sum_prob < 0.999999999 Then
            Stop 'invetsigate
        End If
    Next
    
    
End Sub


Function cum_p_new(Retailer, my, sigma, Q, ant_order, t) As Double

    Dim j, x, k As Long
    Dim temp1, temp2, temp3, temp4, sum As Double
    Dim CV_temp As Double
    
    j = ant_order
    
    If Ret_demand_choice = 0 Then
        temp1 = ((j + 1) * Q - my * t) * Sfi(((j + 1) * Q - my * t) / (sigma * Sqr(t)))
        temp2 = (j * Q - my * t) * Sfi((j * Q - my * t) / (sigma * Sqr(t)))
        temp3 = sigma * Sqr(t) * fi(((j + 1) * Q - my * t) / (sigma * Sqr(t)))
        temp4 = sigma * Sqr(t) * fi((j * Q - my * t) / (sigma * Sqr(t)))
    
        cum_p_new = (1 / Q) * (temp1 - temp2 + temp3 - temp4)
    
    ElseIf Ret_demand_choice > 0 Then
        
        CV_temp = (sigma ^ 2) / my
        
        If ((CV_temp - 1) < (-1 * Exp(-10))) Or (vr_index = Retailer And VR_demand = 0) Then    'CV<1 use Normal demand
            'Stop
            temp1 = ((j + 1) * Q - my * t) * Sfi(((j + 1) * Q - my * t) / (sigma * Sqr(t)))
            temp2 = (j * Q - my * t) * Sfi((j * Q - my * t) / (sigma * Sqr(t)))
            temp3 = sigma * Sqr(t) * fi(((j + 1) * Q - my * t) / (sigma * Sqr(t)))
            temp4 = sigma * Sqr(t) * fi((j * Q - my * t) / (sigma * Sqr(t)))
    
            cum_p_new = (1 / Q) * (temp1 - temp2 + temp3 - temp4)
            
        Else    'Compound Poisson or Pure Poisson demand CV>=1
            sum = 0
            For x = 1 To Q
                k = j * Q + x - 1
                If k <= cdfDemandmax_n(Retailer) Then
                    temp1 = cdfDemand_n(Retailer, k)
                Else
                    temp1 = 1
                End If
                
                sum = sum + temp1
            Next
        
            cum_p_new = (1 / Q) * sum
        End If  'slask
    End If
            
    
End Function

Function GammaDistribution(x, gamma_alfa, gamma_beta, CDF_choice)

Dim my, sigma, x_max, x_min, temp1, temp2, temp3, fx_mod, z, CDFmod_x_max, CDFmod As Double
Dim gamma_function_alfa, stepsize As Double
Dim i, k As Long

    If gamma_alfa >= 0.5 Then
        If CDF_choice = True Then
            GammaDistribution = WorksheetFunction.GammaDist(x, gamma_alfa, gamma_beta, True)
        Else
            If x = 0 Then
                GammaDistribution = 0
            Else
                GammaDistribution = WorksheetFunction.GammaDist(x, gamma_alfa, gamma_beta, False)
            End If
        End If
    ElseIf gamma_alfa = 0 Then
        If CDF_choice = True Then
            GammaDistribution = 0
        Else
            GammaDistribution = 0
        End If
    Else 'modifiering fˆr att undvika att XL kraschar
        my = gamma_alfa * gamma_beta
        sigma = Sqr(gamma_alfa) * gamma_beta
    
        'stepsize = 0.01
        stepsize = stepsize_num_int_gamma
    
        'Determining the gamma_function using XL
        z = my / 2
        temp1 = (gamma_beta ^ (-1 * gamma_alfa)) * (z ^ (gamma_alfa - 1))
        temp2 = Exp(-1 * z / gamma_beta)
        temp3 = WorksheetFunction.GammaDist(z, gamma_alfa, gamma_beta, False)
        gamma_function_alfa = (temp1 * temp2) / temp3
    
        If CDF_choice = True Then
            If x < my / 2 Then
                GammaDistribution = WorksheetFunction.GammaDist(x, gamma_alfa, gamma_beta, True)
            
            ElseIf x >= my / 2 Then
                temp1 = WorksheetFunction.GammaDist((my / 2), gamma_alfa, gamma_beta, True)
                x_min = my / 2
                Call Num_integration_fx_mod(x, gamma_alfa, gamma_beta, x_min, stepsize, CDFmod)
                temp2 = CDFmod / gamma_function_alfa
        
                GammaDistribution = (temp1 + temp2)
                
                If (GammaDistribution - 1) > 0 Then
                    If (GammaDistribution - 1) > 0.0001 Then
                        Stop
                    End If
                    GammaDistribution = 1
                End If
                
            End If
        Else
        'Determine the gamma pdf in x, f(x)
            If x = 0 Then
                GammaDistribution = 0
            ElseIf x > 0 Then
                temp1 = (gamma_beta ^ (-1 * gamma_alfa)) * (x ^ (gamma_alfa - 1))
                temp2 = Exp(-1 * x / gamma_beta)
                GammaDistribution = temp1 * temp2 / gamma_function_alfa
            Else
                Stop
            End If
        End If
    End If

End Function

Private Sub Num_integration_fx_mod(x_max, alfa, beta, x_min, stepsize, CDFmod_x_max)
'Integrera nmeriskt funktionen fx_mod (densiteten fˆr gammafˆrdelningen med gammafunktionen exkluderad

    Dim v, v_min, temp, temp1, temp2, int_sum, interval, x, slask1, slask2 As Double
    Dim nr_interval, Alt, k As Long

    'If alfa >= 1 Then
    '    Stop    'Do not use this function it is beter to use the predefined worksheet function in XL
    'ElseIf alfa < 0 Then
    '    Stop    'Impossible
    'End If
    
    v = x_min
    interval = x_max - v
    'stepsize = 0.001
           
    If interval > stepsize Then
        nr_interval = interval / stepsize
    Else
        nr_interval = 2
        stepsize = interval / nr_interval
    End If
        
    slask1 = nr_interval / 2
    slask2 = slask1 - Int(slask1)
    Do While slask2 > 0
        nr_interval = Int(nr_interval) + 1
        slask1 = nr_interval / 2
        slask2 = slask1 - Int(slask1)
        stepsize = interval / nr_interval
    Loop
    
'Fˆrsta termen
    If v > 0 Then
        temp1 = (v ^ (alfa - 1))
        temp2 = Exp(-1 * v / beta)
        temp = temp1 * temp2
    ElseIf v = 0 Then
        temp = 0
    Else
        Stop
    End If
    
    int_sum = temp

    
    'Term 1 till nr_interval-1
    Alt = 1
    For k = 1 To (nr_interval - 1)
        v = v + stepsize
        temp1 = (v ^ (alfa - 1))
        temp2 = Exp(-1 * v / beta)
        temp = temp1 * temp2
    
        If Alt = 1 Then
            int_sum = int_sum + 4 * temp
            Alt = 0
        Else
            int_sum = int_sum + 2 * temp
            Alt = 1
        End If
    Next
    
    'Sista termen
    v = v + stepsize
    temp1 = (v ^ (alfa - 1))
    temp2 = Exp(-1 * v / beta)
    temp = temp1 * temp2
    
    int_sum = int_sum + temp
    
    'Viktning med stegl‰ngd avslutning
    CDFmod_x_max = (stepsize / 3) * int_sum * (beta ^ (-1 * alfa))
    
End Sub