#### **Black Educational Attainment in Large, Majority Black Cities** ####

# Read the data into a data frame in R
brookings <- read.csv ("~/Documents/brookings-related/brookingsforR2.csv")
View(brookings)


# Rename things to make them more accessible
bach <- brookings$percentblackbach
hs <- brookings$percentblackhs
medincome <-brookings$medincome
percapincome <- brookings$percapincome
SNAP <- brookings$percentSNAP

# Run a regression using lm( ), where bach (percentage of black bachelor’s degree earners) is the dependent variable, 
# hs (percentage of black high school graduates) is the independent variable
lin.mod<-lm(bach~hs)
lin.mod

Call:
  lm(formula = bach ~ hs)

Coefficients:
  (Intercept)           hs  
-52.9863       0.8467
summary(lin.mod)
Call:
  lm(formula = bach ~ hs)

Residuals:
  Min      1Q  Median      3Q     Max 
-4.0375 -1.7762 -0.7682  2.4735  6.1866 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)   
(Intercept) -52.9863    21.6435  -2.448   0.0242 * 
  hs            0.8467     0.2645   3.202   0.0047 **
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.256 on 19 degrees of freedom
Multiple R-squared:  0.3504,	Adjusted R-squared:  0.3163 
F-statistic: 10.25 on 1 and 19 DF,  p-value: 0.004697

# Reject null hypothesis; there’s a correlation here!

# Run a regression using lm( ), where bach (percentage of black bachelor’s degree earners) is the dependent variable, 
# medincome (median income) is the independent variable

lin.mod<-lm(bach~medincome)
lin.mod

Call:
  lm(formula = bach ~ medincome)

Coefficients:
  (Intercept)    medincome  
2.622399     0.000449  

summary(lin.mod)
Call:
  lm(formula = bach ~ medincome)

Residuals:
  Min      1Q  Median      3Q     Max 
-5.2575 -1.9186 -0.6108  0.8865  7.4966 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)   
(Intercept) 2.6223988  4.0612372   0.646  0.52619   
medincome   0.0004490  0.0001316   3.411  0.00293 **
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.181 on 19 degrees of freedom
Multiple R-squared:  0.3798,	Adjusted R-squared:  0.3472 
F-statistic: 11.64 on 1 and 19 DF,  p-value: 0.002931
# Again, kids, we’ve got some significance here!

# Run a regression using lm( ), where bach (percentage of black bachelor’s degree earners) is the dependent variable, 
# percapincome (per capita income) is the independent variable

lin.mod<-lm(bach~percapincome)
lin.mod

Call:
  lm(formula = bach ~ percapincome)

Coefficients:
  (Intercept)  percapincome  
-0.741702      0.000989  

summary(lin.mod)

Call:
  lm(formula = bach ~ percapincome)

Residuals:
  Min      1Q  Median      3Q     Max 
-3.8792 -1.6270 -0.5268  0.6804  5.6400 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  -0.7417025  3.6715370  -0.202 0.842053    
percapincome  0.0009890  0.0002106   4.697 0.000157 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.748 on 19 degrees of freedom
Multiple R-squared:  0.5373,	Adjusted R-squared:  0.5129 
F-statistic: 22.06 on 1 and 19 DF,  p-value: 0.000157

# Run a regression using lm( ), where bach (percentage of black bachelor’s degree earners) is the dependent variable, 
# SNAP (percentage of resident’s receiving food assistance) is the independent variable

lin.mod<-lm(bach~SNAP)
lin.mod

Call:
  lm(formula = bach ~ SNAP)

Coefficients:
  (Intercept)         SNAP  
24.4284      -0.3516  

summary(lin.mod)

Call:
  lm(formula = bach ~ SNAP)

Residuals:
  Min      1Q  Median      3Q     Max 
-4.0426 -2.1504 -0.5221  2.6478  5.6455 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 24.42840    2.41073  10.133 4.25e-09 ***
  SNAP        -0.35159    0.09963  -3.529  0.00224 ** 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.14 on 19 degrees of freedom
Multiple R-squared:  0.396,	Adjusted R-squared:  0.3642 
F-statistic: 12.45 on 1 and 19 DF,  p-value: 0.002242

# MULTIPLE REGRESSION
# Run a multiple regression with percentage of black bachelor’s degree earners (bach) as the dependent variable, 
# and high school graduation (hs), per capita income (percapincome), median income (medincome), 
# and SNAP enrollment (SNAP) as independent variables.

lin.mod2<-lm(bach~hs+percapincome+medincome+SNAP)
lin.mod2<-lm(bach~hs+percapincome+medincome+SNAP)
lin.mod2

Call:
  lm(formula = bach ~ hs + percapincome + medincome + SNAP)

Coefficients:
  (Intercept)            hs  percapincome     medincome          SNAP  
-1.195e+01     2.777e-01     5.885e-04     3.467e-06    -2.033e-01  

summary(lin.mod2)

Call:
  lm(formula = bach ~ hs + percapincome + medincome + SNAP)

Residuals:
  Min      1Q  Median      3Q     Max 
-2.4655 -1.5746 -0.7424  1.0193  4.6083 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)  
(Intercept)  -1.195e+01  2.206e+01  -0.542   0.5954  
hs            2.777e-01  3.024e-01   0.918   0.3721  
percapincome  5.885e-04  3.528e-04   1.668   0.1147  
medincome     3.467e-06  1.999e-04   0.017   0.9864  
SNAP         -2.033e-01  9.162e-02  -2.219   0.0413 *
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.542 on 16 degrees of freedom
Multiple R-squared:  0.6665,	Adjusted R-squared:  0.5831 
F-statistic: 7.993 on 4 and 16 DF,  p-value: 0.0009697

# The results indicate that 66.65% (or, adjusted based on the number of x values, 58.31%) 
# of the variation can be explained in terms of the listed variable. 
# Based on these results, I reject the null hypothesis. 
