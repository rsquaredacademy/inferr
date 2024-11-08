# output from one sample variance test is as expected when alternative is less

    Code
      ifr_os_var_test(mtcars, mpg, 5, alternative = "less")
    Output
                                  One-Sample Statistics                             
      -----------------------------------------------------------------------------
       Variable    Obs     Mean      Std. Err.    Std. Dev.    [95% Conf. Interval] 
      -----------------------------------------------------------------------------
         mpg       32     20.0906     1.0654       6.0269        3.8737    10.6527   
      -----------------------------------------------------------------------------
      
                  Lower Tail Test           
                  ---------------           
                 Ho: sd(mpg) >= 5           
                  Ha: sd(mpg) < 5            
      
          Chi-Square Test for Variance      
      -------------------------------------
       Variable       c       DF      Sig       
      -------------------------------------
         mpg       45.042     31     0.9506  
      -------------------------------------

# output from one sample variance test is as expected when alternative is greater

    Code
      ifr_os_var_test(mtcars, mpg, 5, alternative = "greater")
    Output
                                  One-Sample Statistics                             
      -----------------------------------------------------------------------------
       Variable    Obs     Mean      Std. Err.    Std. Dev.    [95% Conf. Interval] 
      -----------------------------------------------------------------------------
         mpg       32     20.0906     1.0654       6.0269        3.8737    10.6527   
      -----------------------------------------------------------------------------
      
                  Upper Tail Test           
                  ---------------           
                 Ho: sd(mpg) <= 5           
                  Ha: sd(mpg) > 5            
      
          Chi-Square Test for Variance      
      -------------------------------------
       Variable       c       DF      Sig    
      -------------------------------------
         mpg       45.042     31     0.0494  
      -------------------------------------

# output from one sample variance test is as expected when alternative is both

    Code
      ifr_os_var_test(mtcars, mpg, 5, alternative = "both")
    Output
                                  One-Sample Statistics                             
      -----------------------------------------------------------------------------
       Variable    Obs     Mean      Std. Err.    Std. Dev.    [95% Conf. Interval] 
      -----------------------------------------------------------------------------
         mpg       32     20.0906     1.0654       6.0269        3.8737    10.6527   
      -----------------------------------------------------------------------------
      
                   Two Tail Test            
                  ---------------           
                  Ho: sd(mpg) = 5           
                 Ha: sd(mpg) != 5            
      
          Chi-Square Test for Variance      
      -------------------------------------
       Variable       c       DF      Sig    
      -------------------------------------
         mpg       45.042     31     0.0988  
      -------------------------------------

# output from one sample variance test is as expected when alternative is all

    Code
      ifr_os_var_test(mtcars, mpg, 5, alternative = "all")
    Output
                                  One-Sample Statistics                             
      -----------------------------------------------------------------------------
       Variable    Obs     Mean      Std. Err.    Std. Dev.    [95% Conf. Interval] 
      -----------------------------------------------------------------------------
         mpg       32     20.0906     1.0654       6.0269        3.8737    10.6527   
      -----------------------------------------------------------------------------
      
                                      Ho: sd(mpg) = 5                                
      
               Ha: sd < 5                Ha: sd != 5                 Ha: sd > 5         
              c = 45.0419                c = 45.0419                c = 45.0419       
           Pr(C < c) = 0.9506       2 * Pr(C > c) = 0.0988       Pr(C > c) = 0.0494    

