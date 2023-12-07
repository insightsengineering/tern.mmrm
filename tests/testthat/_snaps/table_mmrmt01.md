# LS means table is produced correctly

    Code
      result
    Output
                                                PBO            TRT     
                                              (N=420)        (N=380)   
      —————————————————————————————————————————————————————————————————
      VIS1                                                             
        n                                        68             66     
        Adjusted Mean (SE)                   32.6 (0.8)     37.3 (0.8) 
          95% CI                            (31.1, 34.1)   (35.7, 38.8)
        Difference in Adjusted Means (SE)                   4.7 (1.1)  
          95% CI                                            (2.5, 6.8) 
          Relative Increase (%)                                14%     
          p-value (MMRM)                                       0.00    
      VIS2                                                             
        n                                        69             71     
        Adjusted Mean (SE)                   37.5 (0.6)     41.9 (0.6) 
          95% CI                            (36.3, 38.7)   (40.7, 43.0)
        Difference in Adjusted Means (SE)                   4.4 (0.9)  
          95% CI                                            (2.7, 6.1) 
          Relative Increase (%)                                12%     
          p-value (MMRM)                                       0.00    
      VIS3                                                             
        n                                        71             58     
        Adjusted Mean (SE)                   43.0 (0.5)     46.5 (0.6) 
          95% CI                            (41.9, 44.0)   (45.4, 47.6)
        Difference in Adjusted Means (SE)                   3.6 (0.8)  
          95% CI                                            (2.1, 5.1) 
          Relative Increase (%)                                 8%     
          p-value (MMRM)                                       0.00    
      VIS4                                                             
        n                                        67             67     
        Adjusted Mean (SE)                   48.0 (1.2)     53.0 (1.2) 
          95% CI                            (45.6, 50.4)   (50.6, 55.4)
        Difference in Adjusted Means (SE)                   5.0 (1.7)  
          95% CI                                            (1.6, 8.4) 
          Relative Increase (%)                                10%     
          p-value (MMRM)                                       0.00    

# Fixed effects table is produced correctly

    Code
      result
    Output
                            Estimate   Std. Error   t value   df    Pr(>|t|)
      ——————————————————————————————————————————————————————————————————————
      (Intercept)            25.72        1.58       16.26    253   <0.0001 
      SEXFemale              -0.00        0.59       -0.01    173    0.9946 
      FEV1_BL                 0.17        0.03       5.21     192   <0.0001 
      ARMCDTRT                4.66        1.09       4.26     142   <0.0001 
      AVISITVIS2              4.86        0.80       6.05     141   <0.0001 
      AVISITVIS3             10.33        0.84       12.35    155   <0.0001 
      AVISITVIS4             15.37        1.32       11.67    137   <0.0001 
      ARMCDTRT:AVISITVIS2    -0.30        1.13       -0.26    136    0.7932 
      ARMCDTRT:AVISITVIS3    -1.10        1.21       -0.91    158    0.3663 
      ARMCDTRT:AVISITVIS4     0.35        1.86       0.19     129    0.8517 

# Covariance matrix table is produced correctly

    Code
      result
    Output
             VIS1    VIS2    VIS3    VIS4 
      ————————————————————————————————————
      VIS1   42.89   15.52   8.06    16.59
      VIS2   15.52   26.63   4.63    10.07
      VIS3   8.06    4.63    19.20   7.79 
      VIS4   16.59   10.07   7.79    99.81

# Model diagnostics table is produced correctly

    Code
      result
    Output
                       Diagnostic statistic value
      ———————————————————————————————————————————
      REML criterion            3429.31          
      AIC                       3449.31          
      AICc                      3449.73          
      BIC                       3482.14          

