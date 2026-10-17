# Introduction to inferr

Inferential statistics allows us to make generalizations about
populations using data drawn from the population. We use them when it is
impractical or impossible to collect data about the whole population
under study and instead, we have a sample that represents the population
under study and using inferential statistics technique, we make
generalizations about the population from the sample. **inferr** builds
upon the solid set of statistical tests provided in **stats** package by
including additional data types as inputs, expanding and restructuring
the test results.

The **inferr** package:

- builds upon the statistical tests provided in **stats**\
- provides additional and flexible options
- more detailed and structured test results

As of version 0.1, **inferr** includes a select set of parametric and
non-parametric statistical tests which are listed below:

- One Sample t Test
- Paired Sample t Test
- Independent Sample t Test
- One Sample Proportion Test
- Two Sample Proportion Test
- One Sample Variance Test
- Two Sample Variance Test
- Binomial Test
- ANOVA
- Chi Square Goodness of Fit Test
- Chi Square Independence Test
- Levene’s Test
- Cochran’s Q Test
- McNemar Test
- Runs Test for Randomness

These tests are described in more detail in the following sections.

## One Sample t Test

A one sample t-test is used to determine whether a sample of
observations comes from a population with a specific mean. The
observations must be continuous, independent of each other,
approximately distributed and should not contain any outliers.

### Example

Using the hsb data, test whether the average of write differs
significantly from 50.

[`ifr_os_t_test`](https://inferr.rsquaredacademy.com/reference/ifr_os_t_test.md)`(``hsb``, ``write``, mu ``=`` ``50``, alternative ``=`` ``'all'``)`

    ##                               One-Sample Statistics                               
    ## ---------------------------------------------------------------------------------
    ##  Variable    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
    ## ---------------------------------------------------------------------------------
    ##   write      200    52.775     0.6702       9.4786       51.4537    54.0969   
    ## ---------------------------------------------------------------------------------
    ## 
    ##                                Ho: mean(write) ~=50                              
    ## 
    ##         Ha: mean < 50              Ha: mean ~= 50               Ha: mean > 50        
    ##          t = 4.141                   t = 4.141                   t = 4.141         
    ##        P < t = 0.99997            P > |t| = 0.00005            P > t = 0.00003

## Paired t test

A paired (samples) t-test is used when you want to compare the means
between two related groups of observations on some continuous dependent
variable. In a paired sample test, each subject or entity is measured
twice. It can be used to evaluate the effectiveness of training programs
or treatments. If the dependent variable is dichotomous, use the McNemar
test.

### Examples

Using the hsb data, test whether the mean of read is equal to the mean
of write.

`# Lower Tail Test`` `[`ifr_ts_paired_ttest`](https://inferr.rsquaredacademy.com/reference/ifr_ts_paired_ttest.md)`(``hsb``, ``read``, ``write``, alternative ``=`` ``'less'``)`

    ##                          Paired Samples Statistics                           
    ## ----------------------------------------------------------------------------
    ## Variables    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
    ## ----------------------------------------------------------------------------
    ##    read       200    52.23     0.7249921    10.25294        50.8      53.66   
    ##    write      200    52.775    0.6702372    9.478586       51.45       54.1    
    ## ----------------------------------------------------------------------------
    ##    diff       200    -0.545    0.6283822    8.886666       -1.78       0.69    
    ## ----------------------------------------------------------------------------
    ## 
    ##          Paired Samples Correlations         
    ## -------------------------------------------
    ##   Variables      Obs    Correlation    Sig.
    ##  read & write    200       0.60        0 
    ## -------------------------------------------
    ## 
    ##           Paired Samples Test            
    ##           -------------------            
    ##        Ho: mean(read - write) = 0        
    ##        Ha: mean(read - write) < 0        
    ## 
    ## ----------------------------------------
    ##   Variables         t       df     Sig.  
    ## ----------------------------------------
    ##  read - write    -0.8673    199    0.193 
    ## ----------------------------------------

`# Test all alternatives`` `[`ifr_ts_paired_ttest`](https://inferr.rsquaredacademy.com/reference/ifr_ts_paired_ttest.md)`(``hsb``, ``read``, ``write``, alternative ``=`` ``'all'``)`

    ##                          Paired Samples Statistics                           
    ## ----------------------------------------------------------------------------
    ## Variables    Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
    ## ----------------------------------------------------------------------------
    ##    read       200    52.23     0.7249921    10.25294        50.8      53.66   
    ##    write      200    52.775    0.6702372    9.478586       51.45       54.1    
    ## ----------------------------------------------------------------------------
    ##    diff       200    -0.545    0.6283822    8.886666       -1.78       0.69    
    ## ----------------------------------------------------------------------------
    ## 
    ##          Paired Samples Correlations         
    ## -------------------------------------------
    ##   Variables      Obs    Correlation    Sig.
    ##  read & write    200       0.60        0 
    ## -------------------------------------------
    ## 
    ##                 Ho: mean(read - write) = mean(diff) = 0                  
    ## 
    ##    Ha: mean(diff) < 0      Ha: mean(diff) ~= 0       Ha: mean(diff) > 0    
    ##        t = -0.8673              t = -0.8673              t = -0.8673       
    ##      P < t = 0.193           P > |t| = 0.387           P > t = 0.807

## Two Independent Sample t Test

An independent samples t-test is used to compare the means of a normally
distributed continuous dependent variable for two unrelated groups. The
dependent variable must be approximately normally distributed and the
cases/subjects in the two groups must be different i.e. a subject in one
group cannot also be a subject of the other group. It can be used to
answer whether:

- average number of products produced by two machines differ
  significantly?
- average salaries of graduate students differ based on gender?

### Example

Using the hsb data, test whether the mean for write is the same for
males and females.

[`ifr_ts_ind_ttest`](https://inferr.rsquaredacademy.com/reference/ifr_ts_ind_ttest.md)`(``hsb``, ``female``, ``write``, alternative ``=`` ``'all'``)`

    ##                               Group Statistics                                
    ## -----------------------------------------------------------------------------
    ##   Group       Obs     Mean     Std. Err.    Std. Dev.    [95% Conf. Interval] 
    ## -----------------------------------------------------------------------------
    ##     0          91    50.121      1.080       10.305       47.97539   52.26661  
    ##     1         109    54.991      0.779        8.134       53.44689   56.53511  
    ## -----------------------------------------------------------------------------
    ##  combined     200    52.775      0.67         9.479       51.45379   54.09621  
    ## -----------------------------------------------------------------------------
    ##    diff       200    -4.87       1.304        9.231       -7.42623   -2.31377  
    ## -----------------------------------------------------------------------------
    ## 
    ##                         Independent Samples Test                         
    ##                       ------------------------                        
    ## 
    ##                     Ho: mean(0) - mean(1) = diff = 0                     
    ## 
    ##       Ha: diff < 0            Ha: diff ~= 0             Ha: diff > 0       
    ## 
    ##                                   Pooled                                   
    ## ------------------------------------------------------------------------
    ##        t = -3.7341              t = -3.7341              t = -3.7341       
    ##      P < t = 0.0001          P > |t| = 0.0002          P > t = 0.9999      
    ## 
    ##                               Satterthwaite                                
    ## ------------------------------------------------------------------------
    ##        t = -3.6564              t = -3.6564              t = -3.6564       
    ##      P < t = 0.0002          P > |t| = 0.0003          P > t = 0.9998      
    ## 
    ## 
    ##                 Test for Equality of Variances                  
    ## ---------------------------------------------------------------
    ##  Variable      Method     Num DF    Den DF    F Value    P > F  
    ## ---------------------------------------------------------------
    ##   write       Folded F      90       108       1.605     0.0188 
    ## ---------------------------------------------------------------

## One Sample Test of Proportion

One sample test of proportion compares proportion in one group to a
specified population proportion.

### Examples

Using hsb data, test whether the proportion of females is 50%.

`# Using Variables`` `[`ifr_os_prop_test`](https://inferr.rsquaredacademy.com/reference/ifr_os_prop_test.md)`(``hsb``, ``female``, prob ``=`` ``0.5``)`

    ##      Test Statistics      
    ## -------------------------
    ## Sample Size           200 
    ## Exp Prop              0.5 
    ## Obs Prop            0.545 
    ## z                  1.2728 
    ## Pr(|Z| > |z|)      0.2031 
    ## 
    ## -----------------------------------------------------------------
    ## Category    Observed    Expected    % Deviation    Std. Residuals 
    ## -----------------------------------------------------------------
    ##    0           91         100          -9.00           -0.90      
    ##    1          109         100           9.00            0.90      
    ## -----------------------------------------------------------------

Using Calculator

`# Calculator`` `[`ifr_os_prop_test`](https://inferr.rsquaredacademy.com/reference/ifr_os_prop_test.md)`(``200``, prob ``=`` ``0.5``, phat ``=`` ``0.3``)`

    ##      Test Statistics       
    ## --------------------------
    ## Sample Size            200 
    ## Exp Prop               0.5 
    ## Obs Prop               0.3 
    ## z                  -5.6569 
    ## Pr(|Z| > |z|)            0 
    ## 
    ## -----------------------------------------------------------------
    ## Category    Observed    Expected    % Deviation    Std. Residuals 
    ## -----------------------------------------------------------------
    ##    0          140         100          40.00            4.00      
    ##    1           60         100         -40.00           -4.00      
    ## -----------------------------------------------------------------

## Two Sample Test of Proportion

Two sample test of proportion performs tests on the equality of
proportions using large-sample statistics. It tests that a categorical
variable has the same proportion within two groups or that two variables
have the same proportion.

### Examples

#### Using Variables

Using the treatment data, test equality of proportion of two treatments

`# Using Variables`` `[`ifr_ts_prop_test`](https://inferr.rsquaredacademy.com/reference/ifr_ts_prop_test.md)`(``treatment``, ``treatment1``, ``treatment2``, alternative ``=`` ``'all'``)`

    ##        Test Statistics        
    ## -----------------------------
    ## Total Observations        100 
    ## z                       0.403 
    ## Pr(|Z| < |z|)           0.687 
    ## Pr(Z < z)               0.656 
    ## Pr(Z > z)               0.344

#### Use Grouping Variable

Using the treatment2 data, test whether outcome has same proportion for
male and female

`# Using Grouping Variable`` `[`ifr_ts_prop_group`](https://inferr.rsquaredacademy.com/reference/ifr_ts_prop_test.md)`(``treatment2``, ``outcome``, ``female``, alternative ``=`` ``'all'``)`

    ##        Test Statistics        
    ## -----------------------------
    ## Total Observations        200 
    ## z                       0.351 
    ## Pr(|Z| < |z|)           0.726 
    ## Pr(Z < z)               0.637 
    ## Pr(Z > z)               0.363

#### Using Calculator

Test whether the same proportion of people from two batches will pass a
review exam for a training program. In the first batch of 30
participants, 30% passed the review, whereas in the second batch of 25
participants, 50% passed the review.

`# Calculator`` `[`ifr_ts_prop_calc`](https://inferr.rsquaredacademy.com/reference/ifr_ts_prop_test.md)`(``n1 ``=`` ``30``, n2 ``=`` ``25``, p1 ``=`` ``0.3``, p2 ``=`` ``0.5``, alternative ``=`` ``'all'``)`

    ##        Test Statistics         
    ## ------------------------------
    ## Total Observations          55 
    ## z                       -1.514 
    ## Pr(|Z| < |z|)             0.13 
    ## Pr(Z < z)                0.065 
    ## Pr(Z > z)                0.935

## One Sample Variance Test

One sample variance comparison test compares the standard deviation
(variances) to a hypothesized value. It determines whether the standard
deviation of a population is equal to a hypothesized value. It can be
used to answer the following questions:

- Is the variance equal to some pre-determined threshold value?
- Is the variance greater than some pre-determined threshold value?
- Is the variance less than some pre-determined threshold value?

### Examples

Using the mtcars data, compare the standard deviation of mpg to a
hypothesized value.

`# Lower Tail Test`` `[`ifr_os_var_test`](https://inferr.rsquaredacademy.com/reference/ifr_os_var_test.md)`(``mtcars``, ``mpg``, ``0.3``, alternative ``=`` ``'less'``)`

    ##                             One-Sample Statistics                             
    ## -----------------------------------------------------------------------------
    ##  Variable    Obs     Mean      Std. Err.    Std. Dev.    [95% Conf. Interval] 
    ## -----------------------------------------------------------------------------
    ##    mpg       32     20.0906     1.0654       6.0269        3.8737    10.6527   
    ## -----------------------------------------------------------------------------
    ## 
    ##              Lower Tail Test             
    ##              ---------------             
    ##             Ho: sd(mpg) >= 0.3           
    ##              Ha: sd(mpg) < 0.3            
    ## 
    ##       Chi-Square Test for Variance       
    ## ----------------------------------------
    ##  Variable        c         DF      Sig       
    ## ----------------------------------------
    ##    mpg       12511.635     31     1.0000  
    ## ----------------------------------------

`# Test all alternatives`` `[`ifr_os_var_test`](https://inferr.rsquaredacademy.com/reference/ifr_os_var_test.md)`(``mtcars``, ``mpg``, ``0.3``, alternative ``=`` ``'all'``)`

    ##                             One-Sample Statistics                             
    ## -----------------------------------------------------------------------------
    ##  Variable    Obs     Mean      Std. Err.    Std. Dev.    [95% Conf. Interval] 
    ## -----------------------------------------------------------------------------
    ##    mpg       32     20.0906     1.0654       6.0269        3.8737    10.6527   
    ## -----------------------------------------------------------------------------
    ## 
    ##                                Ho: sd(mpg) = 0.3                               
    ## 
    ##         Ha: sd < 0.3              Ha: sd != 0.3               Ha: sd > 0.3        
    ##       c = 12511.6354             c = 12511.6354             c = 12511.6354      
    ##      Pr(C < c) = 1.0000       2 * Pr(C > c) = 0.0000       Pr(C > c) = 0.0000

## Two Sample Variance Test

Two sample variance comparison tests equality of standard deviations
(variances). It tests that the standard deviation of a continuous
variable is same within two groups or the standard deviation of two
continuous variables is equal.

### Example

#### Use Grouping Variable

Using the mtcars data, compare the standard deviation in miles per
gallon for automatic and manual vehicles.

`# Using Grouping Variable`` `[`ifr_ts_var_test`](https://inferr.rsquaredacademy.com/reference/ifr_ts_var_test.md)`(``hsb``, ``read``, group_var ``=`` ``female``, alternative ``=`` ``'all'``)`

    ##                Variance Ratio Test                 
    ## --------------------------------------------------
    ##   Group      Obs    Mean     Std. Err.    Std. Dev. 
    ## --------------------------------------------------
    ##    0        91     52.82       1.1         10.51   
    ##    1        109    51.73      0.96         10.06   
    ## --------------------------------------------------
    ##  combined    200    52.23      0.72         10.25   
    ## --------------------------------------------------
    ## 
    ##                 Variance Ratio Test                 
    ## --------------------------------------------------
    ##         F              Num DF           Den DF      
    ## --------------------------------------------------
    ##       1.0913             90              108        
    ## --------------------------------------------------
    ## 
    ##        Null & Alternate Hypothesis        
    ## ----------------------------------------
    ##           ratio = sd(0) / (1)           
    ##               Ho: ratio = 1              
    ## 
    ##     Ha: ratio < 1        Ha: ratio > 1    
    ##   Pr(F < f) = 0.6694   Pr(F > f) = 0.3306 
    ## ----------------------------------------

#### Using Variables

Using the hsb data, compare the standard deviation of reading and
writing scores.

`# Using Variables`` `[`ifr_ts_var_test`](https://inferr.rsquaredacademy.com/reference/ifr_ts_var_test.md)`(``hsb``, ``read``, ``write``, alternative ``=`` ``'all'``)`

    ##                Variance Ratio Test                 
    ## --------------------------------------------------
    ##   Group      Obs    Mean     Std. Err.    Std. Dev. 
    ## --------------------------------------------------
    ##   read      200    52.23      0.72         10.25   
    ##  write      200    52.77      0.67         9.48    
    ## --------------------------------------------------
    ##  combined    400    52.5       0.49         9.86    
    ## --------------------------------------------------
    ## 
    ##                 Variance Ratio Test                 
    ## --------------------------------------------------
    ##         F              Num DF           Den DF      
    ## --------------------------------------------------
    ##       1.1701            199              199        
    ## --------------------------------------------------
    ## 
    ##        Null & Alternate Hypothesis        
    ## ----------------------------------------
    ##        ratio = sd(read) / (write)       
    ##               Ho: ratio = 1              
    ## 
    ##     Ha: ratio < 1        Ha: ratio > 1    
    ##   Pr(F < f) = 0.8656   Pr(F > f) = 0.1344 
    ## ----------------------------------------

## Binomial Probability Test

A one sample binomial test allows us to test whether the proportion of
successes on a two-level categorical dependent variable significantly
differs from a hypothesized value.

### Examples

Using the hsb data, test whether the proportion of females and males are
equal.

`# Using variables`` `[`ifr_binom_test`](https://inferr.rsquaredacademy.com/reference/ifr_binom_calc.md)`(``hsb``, ``female``, prob ``=`` ``0.5``)`

    ##              Binomial Test              
    ##  ---------------------------------------
    ##   Group     N     Obs. Prop    Exp. Prop 
    ##  ---------------------------------------
    ##     0       91        0.455        0.500 
    ##     1      109        0.545        0.500 
    ##  ---------------------------------------
    ## 
    ## 
    ##                   Test Summary                  
    ##  ----------------------------------------------
    ##   Tail               Prob              p-value  
    ##  ----------------------------------------------
    ##   Lower          Pr(k <= 109)          0.910518 
    ##   Upper          Pr(k >= 109)          0.114623 
    ##  ----------------------------------------------

#### Using Calculator

`# calculator`` `[`ifr_binom_calc`](https://inferr.rsquaredacademy.com/reference/ifr_binom_calc.md)`(``32``, ``16``, prob ``=`` ``0.5``)`

    ##             Binomial Test              
    ##  --------------------------------------
    ##   Group    N     Obs. Prop    Exp. Prop 
    ##  --------------------------------------
    ##     0      16          0.5        0.500 
    ##     1      16          0.5        0.500 
    ##  --------------------------------------
    ## 
    ## 
    ##                  Test Summary                 
    ##  --------------------------------------------
    ##   Tail              Prob             p-value  
    ##  --------------------------------------------
    ##   Lower         Pr(k <= 16)          0.569975 
    ##   Upper         Pr(k >= 16)          0.569975 
    ##  --------------------------------------------

## ANOVA

The one-way analysis of variance (ANOVA) is used to determine whether
there are any statistically significant differences between the means of
two or more independent (unrelated) groups. It tests the null hypothesis
that samples in two or more groups are drawn from populations with the
same mean values. It cannot tell you which specific groups were
statistically significantly different from each other but only that at
least two groups were different and can be used only for numerical data.

### Examples

Using the hsb data, test whether the mean of write differs between the
three program types.

[`ifr_oneway_anova`](https://inferr.rsquaredacademy.com/reference/ifr_oneway_anova.md)`(``hsb``, ``write``, ``prog``)`

    ##                                 ANOVA                                  
    ## ----------------------------------------------------------------------
    ##                    Sum of                                             
    ##                    Squares     DF     Mean Square      F        Sig.  
    ## ----------------------------------------------------------------------
    ## Between Groups    3175.698      2      1587.849      21.275      0    
    ## Within Groups     14703.177    197      74.635                        
    ## Total             17878.875    199                                    
    ## ----------------------------------------------------------------------
    ## 
    ##                  Report                   
    ## -----------------------------------------
    ## Category     N       Mean      Std. Dev. 
    ## -----------------------------------------
    ##    1        45      51.333       9.398   
    ##    2        105     56.257       7.943   
    ##    3        50      46.760       9.319   
    ## -----------------------------------------
    ## 
    ## Number of obs = 200       R-squared     = 0.1776 
    ## Root MSE      = 8.6392    Adj R-squared = 0.1693

## Chi Square Goodness of Fit Test

A chi-square goodness of fit test allows us to compare the observed
sample distribution with expected probability distribution. It tests
whether the observed proportions for a categorical variable differ from
hypothesized proportions. The proportion of cases expected in each group
of categorical variable may be equal or unequal. It can be applied to
any univariate distribution for which you can calculate the cumulative
distribution function. It is applied to binned data and the value of the
chi square test depends on how the data is binned. For the chi square
approximation to be valid, the sample size must be sufficiently large.

### Example

Using the hsb data, test whether the observed proportions for race
differs significantly from the hypothesized proportions.

`# basic example`` `[`ifr_chisq_gof_test`](https://inferr.rsquaredacademy.com/reference/ifr_chisq_gof_test.md)`(``hsb``, ``race``, `[`c`](https://rdrr.io/r/base/c.html)`(``20``, ``20``, ``20`` , ``140``)``)`

    ##     Test Statistics     
    ## -----------------------
    ## Chi-Square       5.0286 
    ## DF                    3 
    ## Pr > Chi Sq      0.1697 
    ## Sample Size         200 
    ## 
    ##                          Variable: race                           
    ## -----------------------------------------------------------------
    ## Category    Observed    Expected    % Deviation    Std. Residuals 
    ## -----------------------------------------------------------------
    ##    1           24          20          20.00            0.89      
    ##    2           11          20         -45.00           -2.01      
    ##    3           20          20           0.00            0.00      
    ##    4          145         140           3.57            0.42      
    ## -----------------------------------------------------------------

#### Continuity Correction

`# using continuity correction`` `[`ifr_chisq_gof_test`](https://inferr.rsquaredacademy.com/reference/ifr_chisq_gof_test.md)`(``hsb``, ``race``, `[`c`](https://rdrr.io/r/base/c.html)`(``20``, ``20``, ``20`` , ``140``)``, correct ``=`` ``TRUE``)`

    ##     Test Statistics     
    ## -----------------------
    ## Chi-Square       4.3821 
    ## DF                    3 
    ## Pr > Chi Sq      0.2231 
    ## Sample Size         200 
    ## 
    ##                          Variable: race                           
    ## -----------------------------------------------------------------
    ## Category    Observed    Expected    % Deviation    Std. Residuals 
    ## -----------------------------------------------------------------
    ##    1           24          20          17.50            0.78      
    ##    2           11          20         -47.50           -2.12      
    ##    3           20          20          -2.50           -0.11      
    ##    4          145         140           3.21            0.38      
    ## -----------------------------------------------------------------

## Chi Square Test of Independence

A chi-square test is used when you want to test if there is a
significant relationship between two nominal (categorical) variables.

### Examples

Using the hsb data, test if there is a relationship between the type of
school attended (schtyp) and students’ gender (female).

[`ifr_chisq_assoc_test`](https://inferr.rsquaredacademy.com/reference/ifr_chisq_assoc_test.md)`(``hsb``, ``female``, ``schtyp``)`

    ##                Chi Square Statistics                 
    ## 
    ## Statistics                     DF    Value      Prob 
    ## ----------------------------------------------------
    ## Chi-Square                     1    0.0470    0.8284
    ## Likelihood Ratio Chi-Square    1    0.0471    0.8282
    ## Continuity Adj. Chi-Square     1    0.0005    0.9822
    ## Mantel-Haenszel Chi-Square     1    0.0468    0.8287
    ## Phi Coefficient                     0.0153          
    ## Contingency Coefficient             0.0153          
    ## Cramer's V                          0.0153          
    ## ----------------------------------------------------

Using the hsb data, test if there is a relationship between the type of
school attended (schtyp) and students’ socio economic status (ses).

[`ifr_chisq_assoc_test`](https://inferr.rsquaredacademy.com/reference/ifr_chisq_assoc_test.md)`(``hsb``, ``schtyp``, ``ses``)`

    ##                Chi Square Statistics                 
    ## 
    ## Statistics                     DF    Value      Prob 
    ## ----------------------------------------------------
    ## Chi-Square                     2    6.3342    0.0421
    ## Likelihood Ratio Chi-Square    2    7.9060    0.0192
    ## Phi Coefficient                     0.1780          
    ## Contingency Coefficient             0.1752          
    ## Cramer's V                          0.1780          
    ## ----------------------------------------------------

## Levene’s Test

Levene’s test is used to determine if k samples have equal variances. It
is less sensitive to departures from normality and is an alternative to
Bartlett’s test. This test returns Levene’s robust test statistic and
the two statistics proposed by Brown and Forsythe that replace the mean
in Levene’s formula with alternative location estimators. The first
alternative replaces the mean with the median and the second alternative
replaces the mean with the 10% trimmed mean.

### Examples

#### Use Grouping Variable

Using the hsb data, test whether variance in reading score is same
across race.

`# Using Grouping Variable`` `[`ifr_levene_test`](https://inferr.rsquaredacademy.com/reference/ifr_levene_test.md)`(``hsb``, ``read``, group_var ``=`` ``race``)`

    ##            Summary Statistics             
    ## Levels    Frequency    Mean     Std. Dev  
    ## -----------------------------------------
    ##   1          24        46.67      10.24   
    ##   2          11        51.91      7.66    
    ##   3          20        46.8       7.12    
    ##   4          145       53.92      10.28   
    ## -----------------------------------------
    ## Total        200       52.23      10.25   
    ## -----------------------------------------
    ## 
    ##                              Test Statistics                              
    ## -------------------------------------------------------------------------
    ## Statistic                            Num DF    Den DF         F    Pr > F 
    ## -------------------------------------------------------------------------
    ## Brown and Forsythe                        3       196      3.44    0.0179 
    ## Levene                                    3       196    3.4792     0.017 
    ## Brown and Forsythe (Trimmed Mean)         3       196    3.3936     0.019 
    ## -------------------------------------------------------------------------

#### Using Variables

Using the hsb data, test whether variance is equal for reading, writing
and social studies scores.

`# Using Variables`` `[`ifr_levene_test`](https://inferr.rsquaredacademy.com/reference/ifr_levene_test.md)`(``hsb``, ``read``, ``write``, ``socst``)`

    ##            Summary Statistics             
    ## Levels    Frequency    Mean     Std. Dev  
    ## -----------------------------------------
    ##   0          200       52.23      10.25   
    ##   1          200       52.77      9.48    
    ##   2          200       52.41      10.74   
    ## -----------------------------------------
    ## Total        600       52.47      10.15   
    ## -----------------------------------------
    ## 
    ##                              Test Statistics                              
    ## -------------------------------------------------------------------------
    ## Statistic                            Num DF    Den DF         F    Pr > F 
    ## -------------------------------------------------------------------------
    ## Brown and Forsythe                        2       597    1.1683    0.3116 
    ## Levene                                    2       597    1.3803    0.2523 
    ## Brown and Forsythe (Trimmed Mean)         2       597    1.3258    0.2664 
    ## -------------------------------------------------------------------------

## Cochran’s Q Test

Cochran’s Q test is an extension to the McNemar test for related samples
that provides a method for testing for differences between three or more
matched sets of frequencies or proportions. It is a procedure for
testing if the proportions of 3 or more dichotomous variables are equal
in some population. These outcome variables have been measured on the
same people or other statistical units.

### Example

The exam data set contains scores of 15 students for three exams (exam1,
exam2, exam3). Test if three exams are equally difficult.

[`ifr_cochran_qtest`](https://inferr.rsquaredacademy.com/reference/ifr_cochran_qtest.md)`(``exam``, ``exam1``, ``exam2``, ``exam3``)`

    ##    Test Statistics     
    ## ----------------------
    ## N                   15 
    ## Cochran's Q       4.75 
    ## df                   2 
    ## p value          0.093 
    ## ----------------------

## McNemar Test

McNemar test is a non parametric test created by Quinn McNemar and first
published in **Psychometrika** in 1947. It is similar to a paired t test
but applied to a dichotomous dependent variable. It is used to test if a
statistically significant change in proportions have occurred on a
dichotomous trait at two time points on the same population. It can be
used to answer whether:

- two products are equally appealing?
- proportion of success vs failure significantly change after treatment?
- proportion of voters significantly change prior to and following a
  significant political development?

### Examples

Using the hsb data, test if the proportion of students in himath and
hiread group is equal.

`hb`` ``<-`` ``hsb`` ``hb``$``himath`` ``<-`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``hsb``$``math`` ``>`` ``60``, ``1``, ``0``)`` ``hb``$``hiread`` ``<-`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``hsb``$``read`` ``>`` ``60``, ``1``, ``0``)`` `[`ifr_mcnemar_test`](https://inferr.rsquaredacademy.com/reference/ifr_mcnemar_test.md)`(``hb``, ``himath``, ``hiread``)`

    ##            Controls 
    ## ---------------------------------
    ## Cases       0       1       Total 
    ## ---------------------------------
    ##   0        135      21        156 
    ##   1         18      26         44 
    ## ---------------------------------
    ## Total      153      47        200 
    ## ---------------------------------
    ## 
    ##        McNemar's Test        
    ## ----------------------------
    ## McNemar's chi2        0.2308 
    ## DF                         1 
    ## Pr > chi2              0.631 
    ## Exact Pr >= chi2      0.7493 
    ## ----------------------------
    ## 
    ##        Kappa Coefficient         
    ## --------------------------------
    ## Kappa                     0.4454 
    ## ASE                        0.075 
    ## 95% Lower Conf Limit      0.2984 
    ## 95% Upper Conf Limit      0.5923 
    ## --------------------------------
    ## 
    ## Proportion With Factor 
    ## ----------------------
    ## cases             0.78 
    ## controls         0.765 
    ## ratio           1.0196 
    ## odds ratio      1.1667 
    ## ----------------------

`himath`` ``<-`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``hsb``$``math`` ``>`` ``60``, ``1``, ``0``)`` ``hiread`` ``<-`` `[`ifelse`](https://rdrr.io/r/base/ifelse.html)`(``hsb``$``read`` ``>`` ``60``, ``1``, ``0``)`` `[`ifr_mcnemar_test`](https://inferr.rsquaredacademy.com/reference/ifr_mcnemar_test.md)`(`[`table`](https://rdrr.io/r/base/table.html)`(``himath``, ``hiread``)``)`

    ##            Controls 
    ## ---------------------------------
    ## Cases       0       1       Total 
    ## ---------------------------------
    ##   0        135      21        156 
    ##   1         18      26         44 
    ## ---------------------------------
    ## Total      153      47        200 
    ## ---------------------------------
    ## 
    ##        McNemar's Test        
    ## ----------------------------
    ## McNemar's chi2        0.2308 
    ## DF                         1 
    ## Pr > chi2              0.631 
    ## Exact Pr >= chi2      0.7493 
    ## ----------------------------
    ## 
    ##        Kappa Coefficient         
    ## --------------------------------
    ## Kappa                     0.4454 
    ## ASE                        0.075 
    ## 95% Lower Conf Limit      0.2984 
    ## 95% Upper Conf Limit      0.5923 
    ## --------------------------------
    ## 
    ## Proportion With Factor 
    ## ----------------------
    ## cases             0.78 
    ## controls         0.765 
    ## ratio           1.0196 
    ## odds ratio      1.1667 
    ## ----------------------

Perform the above test using matrix as input.

[`ifr_mcnemar_test`](https://inferr.rsquaredacademy.com/reference/ifr_mcnemar_test.md)`(`[`matrix`](https://rdrr.io/r/base/matrix.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``135``, ``18``, ``21``, ``26``)``, nrow ``=`` ``2``)``)`

    ##            Controls 
    ## ---------------------------------
    ## Cases       0       1       Total 
    ## ---------------------------------
    ##   0        135      21        156 
    ##   1         18      26         44 
    ## ---------------------------------
    ## Total      153      47        200 
    ## ---------------------------------
    ## 
    ##        McNemar's Test        
    ## ----------------------------
    ## McNemar's chi2        0.2308 
    ## DF                         1 
    ## Pr > chi2              0.631 
    ## Exact Pr >= chi2      0.7493 
    ## ----------------------------
    ## 
    ##        Kappa Coefficient         
    ## --------------------------------
    ## Kappa                     0.4454 
    ## ASE                        0.075 
    ## 95% Lower Conf Limit      0.2984 
    ## 95% Upper Conf Limit      0.5923 
    ## --------------------------------
    ## 
    ## Proportion With Factor 
    ## ----------------------
    ## cases             0.78 
    ## controls         0.765 
    ## ratio           1.0196 
    ## odds ratio      1.1667 
    ## ----------------------

## Runs Test for Randomness

Runs Test can be used to decide if a data set is from a random process.
It tests whether observations of a sequence are serially independent
i.e. whether they occur in a random order by counting how many runs
there are above and below a threshold. A run is defined as a series of
increasing values or a series of decreasing values. The number of
increasing, or decreasing, values is the length of the run. By default,
the median is used as the threshold. A small number of runs indicates
positive serial correlation; a large number indicates negative serial
correlation.

### Examples

We will use runs test to check regression residuals for serial
correlation.

`# basic example`` `[`ifr_runs_test`](https://inferr.rsquaredacademy.com/reference/ifr_runs_test.md)`(``hsb``, ``read``)`

    ## Runs Test
    ##  Total Cases:  200 
    ##  Test Value :  50 
    ##  Cases < Test Value:  101 
    ##  Cases > Test Value:  99 
    ##  Number of Runs:  95 
    ##  Expected Runs:  100.99 
    ##  Variance (Runs):  49.73874 
    ##  z Statistic:  -0.8493358 
    ##  p-value:  0.3956945

`# drop values equal to threshold`` `[`ifr_runs_test`](https://inferr.rsquaredacademy.com/reference/ifr_runs_test.md)`(``hsb``, ``read``, drop ``=`` ``TRUE``)`

    ## Runs Test
    ##  Total Cases:  200 
    ##  Test Value :  50 
    ##  Cases < Test Value:  83 
    ##  Cases > Test Value:  99 
    ##  Number of Runs:  89 
    ##  Expected Runs:  91.2967 
    ##  Variance (Runs):  44.54805 
    ##  z Statistic:  -0.3441046 
    ##  p-value:  0.7307676

`# recode data in binary format`` `[`ifr_runs_test`](https://inferr.rsquaredacademy.com/reference/ifr_runs_test.md)`(``hsb``, ``read``, split ``=`` ``TRUE``)`

    ## Runs Test
    ##  Total Cases:  200 
    ##  Test Value :  50 
    ##  Cases < Test Value:  101 
    ##  Cases > Test Value:  99 
    ##  Number of Runs:  95 
    ##  Expected Runs:  100.99 
    ##  Variance (Runs):  49.73874 
    ##  z Statistic:  -0.8493358 
    ##  p-value:  0.3956945

`# use mean as threshold`` `[`ifr_runs_test`](https://inferr.rsquaredacademy.com/reference/ifr_runs_test.md)`(``hsb``, ``read``, mean ``=`` ``TRUE``)`

    ## Runs Test
    ##  Total Cases:  200 
    ##  Test Value :  52.23 
    ##  Cases < Test Value:  115 
    ##  Cases > Test Value:  85 
    ##  Number of Runs:  93 
    ##  Expected Runs:  98.75 
    ##  Variance (Runs):  47.52418 
    ##  z Statistic:  -0.8340854 
    ##  p-value:  0.4042329

`# threshold to be used for counting runs`` `[`ifr_runs_test`](https://inferr.rsquaredacademy.com/reference/ifr_runs_test.md)`(``hsb``, ``read``, threshold ``=`` ``0``)`

    ## Runs Test
    ##  Total Cases:  200 
    ##  Test Value :  0 
    ##  Cases < Test Value:  0 
    ##  Cases > Test Value:  200 
    ##  Number of Runs:  1 
    ##  Expected Runs:  1 
    ##  Variance (Runs):  0 
    ##  z Statistic:  NaN 
    ##  p-value:  NaN
