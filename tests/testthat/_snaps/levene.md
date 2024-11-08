# output from levene test is as expected

    Code
      ifr_levene_test(hsb, read, group_var = race)
    Output
                 Summary Statistics             
      Levels    Frequency    Mean     Std. Dev  
      -----------------------------------------
        1          24        46.67      10.24   
        2          11        51.91      7.66    
        3          20        46.8       7.12    
        4          145       53.92      10.28   
      -----------------------------------------
      Total        200       52.23      10.25   
      -----------------------------------------
      
                                   Test Statistics                              
      -------------------------------------------------------------------------
      Statistic                            Num DF    Den DF         F    Pr > F 
      -------------------------------------------------------------------------
      Brown and Forsythe                        3       196      3.44    0.0179 
      Levene                                    3       196    3.4792     0.017 
      Brown and Forsythe (Trimmed Mean)         3       196    3.3936     0.019 
      -------------------------------------------------------------------------

