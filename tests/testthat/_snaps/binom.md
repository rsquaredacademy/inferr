# output from ifr_binom_calc is as expected when k < exp_k

    Code
      ifr_binom_calc(32, 8)
    Output
                  Binomial Test              
       --------------------------------------
        Group    N     Obs. Prop    Exp. Prop 
       --------------------------------------
          0      24         0.75        0.500 
          1       8         0.25        0.500 
       --------------------------------------
      
      
                      Test Summary                
       ------------------------------------------
        Tail             Prob            p-value  
       ------------------------------------------
        Lower         Pr(k <= 8)          0.0035  
        Upper         Pr(k >= 8)         0.998949 
       ------------------------------------------

# output from ifr_binom_calc is as expected when k > exp_k

    Code
      ifr_binom_calc(32, 20)
    Output
                  Binomial Test              
       --------------------------------------
        Group    N     Obs. Prop    Exp. Prop 
       --------------------------------------
          0      12        0.375        0.500 
          1      20        0.625        0.500 
       --------------------------------------
      
      
                       Test Summary                 
       --------------------------------------------
        Tail              Prob             p-value  
       --------------------------------------------
        Lower         Pr(k <= 20)          0.944908 
        Upper         Pr(k >= 20)          0.107664 
       --------------------------------------------

