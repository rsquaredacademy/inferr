# output from 2 sample variance test is as expected when alternative is less

    Code
      ifr_ts_var_test(hsb, read, write, alternative = "less")
    Output
                     Variance Ratio Test                 
      --------------------------------------------------
        Group      Obs    Mean     Std. Err.    Std. Dev. 
      --------------------------------------------------
        read      200    52.23      0.72         10.25   
       write      200    52.77      0.67         9.48    
      --------------------------------------------------
       combined    400    52.5       0.49         9.86    
      --------------------------------------------------
      
                 Lower Tail Test           
                 ---------------           
            ratio = sd(read) / (write)     
                  Ho: ratio = 1            
                  Ha: ratio < 1             
      
              Variance Ratio Test          
      ------------------------------------
         F       Num DF    Den DF      p    
      ------------------------------------
       1.1701     199       199      0.8656 
      ------------------------------------

# output from 2 sample variance test is as expected when alternative is greater

    Code
      ifr_ts_var_test(hsb, read, write, alternative = "greater")
    Output
                     Variance Ratio Test                 
      --------------------------------------------------
        Group      Obs    Mean     Std. Err.    Std. Dev. 
      --------------------------------------------------
        read      200    52.23      0.72         10.25   
       write      200    52.77      0.67         9.48    
      --------------------------------------------------
       combined    400    52.5       0.49         9.86    
      --------------------------------------------------
      
                 Upper Tail Test           
                 ---------------           
                  Ho: ratio = 1            
                  Ha: ratio > 1             
      
              Variance Ratio Test          
      ------------------------------------
         F       Num DF    Den DF      p    
      ------------------------------------
       1.1701     199       199      0.1344 
      ------------------------------------

# output from 2 sample variance test is as expected when alternative is all

    Code
      ifr_ts_var_test(hsb, read, write, alternative = "all")
    Output
                     Variance Ratio Test                 
      --------------------------------------------------
        Group      Obs    Mean     Std. Err.    Std. Dev. 
      --------------------------------------------------
        read      200    52.23      0.72         10.25   
       write      200    52.77      0.67         9.48    
      --------------------------------------------------
       combined    400    52.5       0.49         9.86    
      --------------------------------------------------
      
                      Variance Ratio Test                 
      --------------------------------------------------
              F              Num DF           Den DF      
      --------------------------------------------------
            1.1701            199              199        
      --------------------------------------------------
      
             Null & Alternate Hypothesis        
      ----------------------------------------
             ratio = sd(read) / (write)       
                    Ho: ratio = 1              
      
          Ha: ratio < 1        Ha: ratio > 1    
        Pr(F < f) = 0.8656   Pr(F > f) = 0.1344 
      ----------------------------------------

