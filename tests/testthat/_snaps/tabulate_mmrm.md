# h_mmrm_fixed works as expected

    Code
      result
    Output
                                              Estimate   Std. Error   t value   df   Pr(>|t|)
      ———————————————————————————————————————————————————————————————————————————————————————
      (Intercept)                              51.97        2.35       22.14    42   <0.0001 
      BMRKR2LOW                                 1.58        1.27       1.24     36    0.2225 
      BMRKR2MEDIUM                             -2.08        1.01       -2.06    36    0.0471 
      ARMA: Drug X                              0.13        2.92       0.05     38    0.9635 
      ARMC: Combination                        -3.28        3.09       -1.06    38    0.2959 
      AVISITWEEK 2 DAY 15                      -5.17        3.51       -1.47    38    0.1491 
      AVISITWEEK 3 DAY 22                      -1.46        3.40       -0.43    38    0.6702 
      AVISITWEEK 4 DAY 29                      -1.58        3.25       -0.49    38    0.6293 
      AVISITWEEK 5 DAY 36                      -1.20        3.73       -0.32    38    0.7495 
      ARMA: Drug X:AVISITWEEK 2 DAY 15          4.04        4.51       0.90     38    0.3762 
      ARMC: Combination:AVISITWEEK 2 DAY 15     5.41        4.77       1.13     38    0.2638 
      ARMA: Drug X:AVISITWEEK 3 DAY 22          1.05        4.36       0.24     38    0.8116 
      ARMC: Combination:AVISITWEEK 3 DAY 22     5.44        4.62       1.18     38    0.2459 
      ARMA: Drug X:AVISITWEEK 4 DAY 29         -1.60        4.18       -0.38    38    0.7038 
      ARMC: Combination:AVISITWEEK 4 DAY 29     5.65        4.42       1.28     38    0.2093 
      ARMA: Drug X:AVISITWEEK 5 DAY 36         -1.54        4.78       -0.32    38    0.7490 
      ARMC: Combination:AVISITWEEK 5 DAY 36     6.92        5.06       1.37     38    0.1798 

# h_mmrm_cov works as expected

    Code
      result
    Output
                      WEEK 1 DAY 8   WEEK 2 DAY 15   WEEK 3 DAY 22   WEEK 4 DAY 29   WEEK 5 DAY 36
      ————————————————————————————————————————————————————————————————————————————————————————————
      WEEK 1 DAY 8       56.70           -8.87           -6.67           0.50           -14.57    
      WEEK 2 DAY 15      -8.87           61.29           -5.99          -16.25           7.92     
      WEEK 3 DAY 22      -6.67           -5.99           57.12           9.43            1.59     
      WEEK 4 DAY 29       0.50          -16.25           9.43            60.79          -14.50    
      WEEK 5 DAY 36      -14.57          7.92            1.59           -14.50           66.96    

# h_mmrm_diagnostic works as expected

    Code
      result
    Output
                       Diagnostic statistic value
      ———————————————————————————————————————————
      REML criterion             1341.7          
      AIC                        1371.7          
      AICc                       1374.5          
      BIC                        1397.4          

# summarize_lsmeans works as expected

    Code
      result
    Output
                                             B: Placebo     A: Drug X     C: Combination
      ——————————————————————————————————————————————————————————————————————————————————
      WEEK 1 DAY 8                                                                      
        n                                        11             17              13      
        Adjusted Mean (SE)                   51.5 (2.3)     51.6 (1.8)      48.2 (2.1)  
          95% CI                            (46.9, 56.1)   (47.9, 55.3)    (43.9, 52.4) 
        Difference in Adjusted Means (SE)                   0.1 (2.9)       -3.3 (3.1)  
          95% CI                                           (-5.8, 6.0)     (-9.5, 3.0)  
          Relative Increase (%)                                 0%             -6%      
          p-value (MMRM)                                       0.96            0.30     
      WEEK 2 DAY 15                                                                     
        n                                        11             17              13      
        Adjusted Mean (SE)                   46.3 (2.4)     50.5 (1.9)      48.4 (2.2)  
          95% CI                            (41.5, 51.1)   (46.6, 54.3)    (44.0, 52.8) 
        Difference in Adjusted Means (SE)                   4.2 (3.0)       2.1 (3.2)   
          95% CI                                           (-2.0, 10.3)    (-4.4, 8.6)  
          Relative Increase (%)                                 9%              5%      
          p-value (MMRM)                                       0.18            0.51     
      WEEK 3 DAY 22                                                                     
        n                                        11             17              13      
        Adjusted Mean (SE)                   50.0 (2.3)     51.2 (1.8)      52.2 (2.1)  
          95% CI                            (45.4, 54.6)   (47.5, 54.9)    (47.9, 56.4) 
        Difference in Adjusted Means (SE)                   1.2 (2.9)       2.2 (3.1)   
          95% CI                                           (-4.7, 7.1)     (-4.1, 8.5)  
          Relative Increase (%)                                 2%              4%      
          p-value (MMRM)                                       0.69            0.49     
      WEEK 4 DAY 29                                                                     
        n                                        11             17              13      
        Adjusted Mean (SE)                   49.9 (2.4)     48.4 (1.9)      52.3 (2.2)  
          95% CI                            (45.1, 54.6)   (44.6, 52.3)    (47.9, 56.6) 
        Difference in Adjusted Means (SE)                   -1.5 (3.0)      2.4 (3.2)   
          95% CI                                           (-7.6, 4.6)     (-4.1, 8.9)  
          Relative Increase (%)                                -3%              5%      
          p-value (MMRM)                                       0.63            0.46     
      WEEK 5 DAY 36                                                                     
        n                                        11             17              13      
        Adjusted Mean (SE)                   50.3 (2.5)     48.9 (2.0)      53.9 (2.3)  
          95% CI                            (45.3, 55.3)   (44.8, 52.9)    (49.3, 58.5) 
        Difference in Adjusted Means (SE)                   -1.4 (3.2)      3.6 (3.4)   
          95% CI                                           (-7.8, 5.0)     (-3.2, 10.4) 
          Relative Increase (%)                                -3%              7%      
          p-value (MMRM)                                       0.66            0.29     

# summarize_lsmeans works as expected when treatment is not considered in the model

    Code
      result
    Output
                               all obs   
      ———————————————————————————————————
      WEEK 1 DAY 8                       
        n                         41     
        Adjusted Mean (SE)    50.5 (1.2) 
          95% CI             (48.1, 52.8)
      WEEK 2 DAY 15                      
        n                         41     
        Adjusted Mean (SE)    48.7 (1.2) 
          95% CI             (46.3, 51.1)
      WEEK 3 DAY 22                      
        n                         41     
        Adjusted Mean (SE)    51.2 (1.2) 
          95% CI             (48.7, 53.7)
      WEEK 4 DAY 29                      
        n                         41     
        Adjusted Mean (SE)    50.0 (1.3) 
          95% CI             (47.5, 52.6)
      WEEK 5 DAY 36                      
        n                         41     
        Adjusted Mean (SE)    50.8 (1.3) 
          95% CI             (48.2, 53.5)

# summarize_lsmeans works with averages of visits as expected

    Code
      result
    Output
                                             B: Placebo     A: Drug X     C: Combination
      ——————————————————————————————————————————————————————————————————————————————————
      WEEK 1 DAY 8                                                                      
        n                                        11             17              13      
        Adjusted Mean (SE)                   51.5 (2.3)     51.6 (1.8)      48.2 (2.1)  
          95% CI                            (46.9, 56.1)   (47.9, 55.3)    (43.9, 52.4) 
        Difference in Adjusted Means (SE)                   0.1 (2.9)       -3.3 (3.1)  
          95% CI                                           (-5.8, 6.0)     (-9.5, 3.0)  
          Relative Reduction (%)                               -0%              6%      
          p-value (MMRM)                                       0.96            0.30     
      WEEK 2 DAY 15                                                                     
        n                                        11             17              13      
        Adjusted Mean (SE)                   46.3 (2.4)     50.5 (1.9)      48.4 (2.2)  
          95% CI                            (41.5, 51.1)   (46.6, 54.3)    (44.0, 52.8) 
        Difference in Adjusted Means (SE)                   4.2 (3.0)       2.1 (3.2)   
          95% CI                                           (-2.0, 10.3)    (-4.4, 8.6)  
          Relative Reduction (%)                               -9%             -5%      
          p-value (MMRM)                                       0.18            0.51     
      WEEK 3 DAY 22                                                                     
        n                                        11             17              13      
        Adjusted Mean (SE)                   50.0 (2.3)     51.2 (1.8)      52.2 (2.1)  
          95% CI                            (45.4, 54.6)   (47.5, 54.9)    (47.9, 56.4) 
        Difference in Adjusted Means (SE)                   1.2 (2.9)       2.2 (3.1)   
          95% CI                                           (-4.7, 7.1)     (-4.1, 8.5)  
          Relative Reduction (%)                               -2%             -4%      
          p-value (MMRM)                                       0.69            0.49     
      WEEK 4 DAY 29                                                                     
        n                                        11             17              13      
        Adjusted Mean (SE)                   49.9 (2.4)     48.4 (1.9)      52.3 (2.2)  
          95% CI                            (45.1, 54.6)   (44.6, 52.3)    (47.9, 56.6) 
        Difference in Adjusted Means (SE)                   -1.5 (3.0)      2.4 (3.2)   
          95% CI                                           (-7.6, 4.6)     (-4.1, 8.9)  
          Relative Reduction (%)                                3%             -5%      
          p-value (MMRM)                                       0.63            0.46     
      WEEK 5 DAY 36                                                                     
        n                                        11             17              13      
        Adjusted Mean (SE)                   50.3 (2.5)     48.9 (2.0)      53.9 (2.3)  
          95% CI                            (45.3, 55.3)   (44.8, 52.9)    (49.3, 58.5) 
        Difference in Adjusted Means (SE)                   -1.4 (3.2)      3.6 (3.4)   
          95% CI                                           (-7.8, 5.0)     (-3.2, 10.4) 
          Relative Reduction (%)                                3%             -7%      
          p-value (MMRM)                                       0.66            0.29     
      W1D8 + W2D15                                                                      
        n                                        11             17              13      
        Adjusted Mean (SE)                   48.9 (1.5)     51.0 (1.2)      48.3 (1.4)  
          95% CI                            (45.8, 51.9)   (48.6, 53.5)    (45.5, 51.2) 
        Difference in Adjusted Means (SE)                   2.2 (1.9)       -0.6 (2.1)  
          95% CI                                           (-1.8, 6.1)     (-4.8, 3.6)  
          Relative Reduction (%)                               -4%              1%      
          p-value (MMRM)                                       0.27            0.78     
      W3D22 + W5D36                                                                     
        n                                        11             17              13      
        Adjusted Mean (SE)                   50.1 (1.7)     50.0 (1.4)      53.0 (1.6)  
          95% CI                            (46.7, 53.6)   (47.2, 52.8)    (49.9, 56.2) 
        Difference in Adjusted Means (SE)                   -0.1 (2.2)      2.9 (2.3)   
          95% CI                                           (-4.5, 4.3)     (-1.8, 7.6)  
          Relative Reduction (%)                                0%             -6%      
          p-value (MMRM)                                       0.96            0.22     

