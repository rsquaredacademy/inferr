# output from one way anova is as expected

    Code
      ifr_oneway_anova(mtcars, mpg, cyl)
    Output
                                     ANOVA                                 
      --------------------------------------------------------------------
                         Sum of                                           
                        Squares     DF    Mean Square      F        Sig.  
      --------------------------------------------------------------------
      Between Groups    824.785     2       412.392      39.699      0    
      Within Groups     301.263     29      10.388                        
      Total             1126.048    31                                    
      --------------------------------------------------------------------
      
                       Report                  
      ----------------------------------------
      Category    N       Mean      Std. Dev. 
      ----------------------------------------
         4        11     26.664       4.510   
         6        7      19.743       1.454   
         8        14     15.100       2.560   
      ----------------------------------------
      
      Number of obs = 32        R-squared     = 0.7325 
      Root MSE      = 3.2231    Adj R-squared = 0.714 
      

