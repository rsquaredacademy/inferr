context('os-vartest')

test_that('output from os_vartest matches the expected result', {

    k <- os_vartest(mtcars$mpg, 0.3)
    expect_equal(k$n, 32)
    expect_equal(k$sd, 0.3)
    expect_equal(k$sigma, 6.0269)
    expect_equal(k$se, 1.0654)
    expect_equal(k$chi, 12511.4359)
    expect_equal(k$df, 31)
    expect_equal(k$p_lower, 1)
    expect_equal(k$p_upper, 0)
    expect_equal(k$p_two, 0)
    expect_equal(k$xbar, 20.0906)
    expect_equal(k$c_lwr, 3.8737)
    expect_equal(k$c_upr, 10.6526)
    expect_equal(k$conf, 0.95)
    expect_equivalent(k$var_name, 'mpg')
    expect_equivalent(k$type, 'both')

})


test_that('os_vartest returns appropriate errors', {
    expect_error(os_vartest('mtcars$mpg', 0.3),
                 'x must be numeric')
    expect_error(os_vartest(mtcars$mpg, '0.3'),
                 'sd must be numeric')
    expect_error(os_vartest(mtcars$mpg, 0.3, '0.95'),
                 'confint must be numeric')
})


test_that('output from one sample variance test is as expected when alternative is less', {

  x <- cat("                            One-Sample Statistics                             
-----------------------------------------------------------------------------
 Variable    Obs     Mean      Std. Err.    Std. Dev.    [95% Conf. Interval] 
-----------------------------------------------------------------------------
   mpg       32     20.0906     1.0654       6.0269        3.8737    10.6526   
-----------------------------------------------------------------------------

            Lower Tail Test           
            ---------------           
           Ho: sd(mpg) >= 5           
            Ha: sd(mpg) < 5            

    Chi-Square Test for Variance      
-------------------------------------
 Variable       c       DF      Sig       
-------------------------------------
   mpg       45.041     31     0.9506  
-------------------------------------")

  expect_equivalent(print(os_vartest(mtcars$mpg, 5, alternative = 'less')), x)

})

test_that('output from one sample variance test is as expected when alternative is greater', {

  x <- cat("                            One-Sample Statistics                             
-----------------------------------------------------------------------------
 Variable    Obs     Mean      Std. Err.    Std. Dev.    [95% Conf. Interval] 
-----------------------------------------------------------------------------
   mpg       32     20.0906     1.0654       6.0269        3.8737    10.6526   
-----------------------------------------------------------------------------

            Upper Tail Test           
            ---------------           
           Ho: sd(mpg) <= 5           
            Ha: sd(mpg) > 5            

    Chi-Square Test for Variance      
-------------------------------------
 Variable       c       DF      Sig    
-------------------------------------
   mpg       45.041     31     0.0494  
-------------------------------------")

  expect_equivalent(print(os_vartest(mtcars$mpg, 5, alternative = 'greater')), x)

})

test_that('output from one sample variance test is as expected when alternative is both', {

  x <- cat("                            One-Sample Statistics                             
-----------------------------------------------------------------------------
 Variable    Obs     Mean      Std. Err.    Std. Dev.    [95% Conf. Interval] 
-----------------------------------------------------------------------------
   mpg       32     20.0906     1.0654       6.0269        3.8737    10.6526   
-----------------------------------------------------------------------------

             Two Tail Test            
            ---------------           
            Ho: sd(mpg) = 5           
           Ha: sd(mpg) != 5            

    Chi-Square Test for Variance      
-------------------------------------
 Variable       c       DF      Sig    
-------------------------------------
   mpg       45.041     31     0.0989  
-------------------------------------")

  expect_equivalent(print(os_vartest(mtcars$mpg, 5, alternative = 'both')), x)

})

test_that('output from one sample variance test is as expected when alternative is all', {

  x <- cat("                            One-Sample Statistics                             
-----------------------------------------------------------------------------
 Variable    Obs     Mean      Std. Err.    Std. Dev.    [95% Conf. Interval] 
-----------------------------------------------------------------------------
   mpg       32     20.0906     1.0654       6.0269        3.8737    10.6526   
-----------------------------------------------------------------------------

                                Ho: sd(mpg) = 5                                

         Ha: sd < 5                Ha: sd != 5                 Ha: sd > 5         
        c = 45.0412                c = 45.0412                c = 45.0412       
     Pr(C < c) = 0.9506       2 * Pr(C > c) = 0.0989       Pr(C > c) = 0.0494")

  expect_equivalent(print(os_vartest(mtcars$mpg, 5, alternative = 'all')), x)

})