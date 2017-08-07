# Survey-weighted generalized linear models
  





```r
## Create survey object
options(digits = 4)
options(survey.lonely.psu = "adjust")

des <- svydesign(ids = ~1, weights = ~weight, data = df[is.na(df$weight) == F, ])
#summary(des)
```

# Survey questions

## Q1. Before receiving this survey, did you know influenza is different from the stomach flu?
  
### Compare unweighted and weighted sex ratios for Q1

```r
# unweighted Q1
with(data, addmargins(table(Q1, PPGENDER)))
```

```
##      PPGENDER
## Q1    Female Male  Sum
##   Yes    888  776 1664
##   No     205  283  488
##   Sum   1093 1059 2152
```

```r
svyby(~Q1, ~Q1+PPGENDER, des, unwtd.count)
```

```
##             Q1 PPGENDER counts se
## Yes.Female Yes   Female    888  0
## No.Female   No   Female    205  0
## Yes.Male   Yes     Male    776  0
## No.Male     No     Male    283  0
```

```r
# weighted Q1
svytable(~Q1 + PPGENDER, design = des, round = T)
```

```
##      PPGENDER
## Q1    Female Male
##   Yes    887  725
##   No     231  304
```

```r
# unweighted proportion
(u <- with(data, prop.table(table(Q1, PPGENDER), margin = 2))*100)  
```

```
##      PPGENDER
## Q1    Female  Male
##   Yes  81.24 73.28
##   No   18.76 26.72
```

```r
# weighted proportion
(w <- prop.table(svytable(~Q1 + PPGENDER, design = des), margin = 2)*100)
```

```
##      PPGENDER
## Q1    Female  Male
##   Yes  79.37 70.45
##   No   20.63 29.55
```

```r
# table above with standard errors
ftable(svyby(formula = ~Q1, by = ~PPGENDER, design = des, FUN = svymean, na.rm = T))
```

```
##                     Q1Yes    Q1No
## PPGENDER                         
## Female   svymean  0.79371 0.20629
##          SE       0.01373 0.01373
## Male     svymean  0.70453 0.29547
##          SE       0.01562 0.01562
```


***

## Q2. Have you had an illness with influenza-like symptoms since August 2015?

### First look at unweighted. Calculate OR for being sick by gender.

```r
## unweighted
# recode datar$Q2
#head(datar$Q2)

# sick = 1; not sick = 0
df$sick <- car::recode(datar$Q2, recodes = "'Yes' = 1; 'No' = 0; NA = NA")
#head(df$sick)

# unwtd table Q2 by gender
with(df, table(Q2, PPGENDER))
```

```
##      PPGENDER
## Q2    Female Male
##   Yes    234  180
##   No     858  877
```

```r
# glm unwtd
fit1 <- glm(sick ~ PPGENDER, data = df, family = binomial())
summary(fit1)
```

```
## 
## Call:
## glm(formula = sick ~ PPGENDER, family = binomial(), data = df)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -0.695  -0.695  -0.611  -0.611   1.882  
## 
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)    
## (Intercept)   -1.2993     0.0737  -17.62   <2e-16 ***
## PPGENDERMale  -0.2843     0.1102   -2.58   0.0099 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2106.2  on 2148  degrees of freedom
## Residual deviance: 2099.5  on 2147  degrees of freedom
##   (19 observations deleted due to missingness)
## AIC: 2103
## 
## Number of Fisher Scoring iterations: 4
```

```r
(q2.u <- exp(coefficients(fit1)))  # OR
```

```
##  (Intercept) PPGENDERMale 
##       0.2727       0.7526
```

Odds ratio of being sick as a male is 0.7526 compared to females.


### Apply survey weights for Q2. Calculate OR for getting sick by gender.

```r
## weighted
# relevel PPGENDER, set male as reference group
df$PPGENDER <- relevel(datar$PPGENDER, "Male")

# update survey object
options(survey.lonely.psu = "adjust")
des2 <- svydesign(ids = ~1, weights = ~weight, data = df)  # data = df

# wtd svyglm
m1 <- svyglm(sick ~ PPGENDER, des2, family = quasibinomial())
summary(m1)
```

```
## 
## Call:
## svyglm(formula = sick ~ PPGENDER, des2, family = quasibinomial())
## 
## Survey design:
## svydesign(ids = ~1, weights = ~weight, data = df)
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     -1.5559     0.0888  -17.53   <2e-16 ***
## PPGENDERFemale   0.2981     0.1188    2.51    0.012 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.9992)
## 
## Number of Fisher Scoring iterations: 4
```

```r
# OR
(or1 <- exp(coefficients(m1)))
```

```
##    (Intercept) PPGENDERFemale 
##          0.211          1.347
```

```r
# 95% CI 
exp(confint(m1))
```

```
##                 2.5 % 97.5 %
## (Intercept)    0.1773 0.2511
## PPGENDERFemale 1.0676 1.7005
```

In the weighted data, females had 1.3474 times the odds of being sick compared to males.


### Calculate OR for being sick, adjusted for ethnicity

```r
# being sick = 1; by ethnicity; white = reference level
m2 <- svyglm(sick ~ PPETHM, des2, family = quasibinomial())
summary(m2)
```

```
## 
## Call:
## svyglm(formula = sick ~ PPETHM, des2, family = quasibinomial())
## 
## Survey design:
## svydesign(ids = ~1, weights = ~weight, data = df)
## 
## Coefficients:
##                              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                   -1.5566     0.0699  -22.28  < 2e-16 ***
## PPETHMBlack, Non-Hispanic      0.1689     0.1998    0.85  0.39789    
## PPETHMHispanic                 0.5917     0.1677    3.53  0.00043 ***
## PPETHMOther, Non-Hispanic      0.4717     0.2612    1.81  0.07106 .  
## PPETHM2+ Races, Non-Hispanic   0.4078     0.2799    1.46  0.14526    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.9992)
## 
## Number of Fisher Scoring iterations: 4
```

```r
# OR adjusted for ethnicity
(or2 <- exp(coefficients(m2)))
```

```
##                  (Intercept)    PPETHMBlack, Non-Hispanic 
##                       0.2109                       1.1840 
##               PPETHMHispanic    PPETHMOther, Non-Hispanic 
##                       1.8071                       1.6028 
## PPETHM2+ Races, Non-Hispanic 
##                       1.5035
```

```r
# note 95% CI for hispanic group
exp(confint(m2))
```

```
##                               2.5 % 97.5 %
## (Intercept)                  0.1839 0.2418
## PPETHMBlack, Non-Hispanic    0.8004 1.7514
## PPETHMHispanic               1.3008 2.5104
## PPETHMOther, Non-Hispanic    0.9606 2.6743
## PPETHM2+ Races, Non-Hispanic 0.8687 2.6023
```

Compared to whites, OR for being sick are 1.184, 1.8071, 1.6028, 1.5035 for blacks, hispanics, others, and 2+ races, respectively.
  
Notes: When a logistic regression is calculated, the regression coefficient (b1) is the estimated increase in the log odds of the outcome per unit increase in the value of the exposure; the exponential function of the regression coefficient (e^b1) is the odds ratio associated with a one-unit increase in the exposure.  



```r
## should I recode each variable as binomial? or re-level a reference group each time?
## same as above but with recoded 1,0 variables; can change reference level

# first recode xi variables
#summary(datar$PPETHM)
df$white <- car::recode(datar$PPETHM, recodes = "'White, Non-Hispanic' = 1; NA = NA; else = 0")
df$black <- car::recode(datar$PPETHM, recodes = "'Black, Non-Hispanic' = 1; NA = NA; else = 0")
df$hispanic <- car::recode(datar$PPETHM, recodes = "'Hispanic' = 1; NA = NA; else = 0")
df$otherrace <- car::recode(datar$PPETHM, recodes = "'Other, Non-Hispanic' = 1; NA = NA; else = 0")
df$more2race <- car::recode(datar$PPETHM, recodes = "'2+ Races, Non-Hispanic' = 1; NA = NA; else = 0")
#tail(df)

# update survey object mx
options(survey.lonely.psu = "adjust")
desx <- svydesign(ids = ~1, weights = ~weight, data = df)

# being sick; black = 1; not black = 0
mx <- svyglm(sick ~ black, desx, family = quasibinomial())
#summary(mx)
mx2 <- update(mx, ~ . + hispanic)
mx3 <- update(mx2, ~ . + otherrace)
mx4 <- update(mx3, ~ . + more2race)
mx5 <- update(mx4, ~ . + white)  # reference group

# compare
mtable(mx, mx2, mx3, mx4, mx5)
```

```
## 
## Calls:
## mx: svyglm(formula = sick ~ black, desx, family = quasibinomial())
## mx2: svyglm(formula = sick ~ black + hispanic, desx, family = quasibinomial())
## mx3: svyglm(formula = sick ~ black + hispanic + otherrace, desx, family = quasibinomial())
## mx4: svyglm(formula = sick ~ black + hispanic + otherrace + more2race, 
##     desx, family = quasibinomial())
## mx5: svyglm(formula = sick ~ black + hispanic + otherrace + more2race + 
##     white, desx, family = quasibinomial())
## 
## ===============================================================================
##                            mx         mx2        mx3        mx4        mx5     
## -------------------------------------------------------------------------------
##   (Intercept)           -1.395***  -1.499***  -1.548***  -1.557***  -1.557***  
##                         (0.062)    (0.067)    (0.068)    (0.070)    (0.070)    
##   black: 1/0             0.007      0.112      0.160      0.169      0.169     
##                         (0.197)    (0.199)    (0.199)    (0.200)    (0.200)    
##   hispanic: 1/0                     0.534**    0.583***   0.592***   0.592***  
##                                    (0.167)    (0.167)    (0.168)    (0.168)    
##   otherrace: 1/0                               0.463      0.472      0.472     
##                                               (0.261)    (0.261)    (0.261)    
##   more2race: 1/0                                          0.408      0.408     
##                                                          (0.280)    (0.280)    
## -------------------------------------------------------------------------------
##   Aldrich-Nelson R-sq.      0.0        0.0        0.0        0.0        0.0    
##   McFadden R-sq.            0.0        0.0        0.0        0.0        0.0    
##   Cox-Snell R-sq.           0.0        0.0        0.0        0.0        0.0    
##   Nagelkerke R-sq.          0.0        0.0        0.0        0.0        0.0    
##   phi                       1.0        1.0        1.0        1.0        1.0    
##   Likelihood-ratio          0.0       13.9       18.8       19.5       19.5    
##   p                         1.0        0.0        0.0        0.0        0.0    
##   Log-likelihood         2140.7     2126.8     2122.0     2121.2     2121.2    
##   Deviance               2140.7     2126.8     2122.0     2121.2     2121.2    
##   N                      2146       2146       2146       2146       2146      
## ===============================================================================
```

```r
# odds ratios
exp(coefficients(mx5))
```

```
## (Intercept)      black1   hispanic1  otherrace1  more2race1 
##      0.2109      1.1840      1.8071      1.6028      1.5035
```

```r
## answer: re-level reference group
```

***

### Compare model variables for being sick: adjust for gender, ethnicity, employment, and marital status

```r
des3 <- svydesign(ids = ~1, weights = ~weight, data = df)

## sick = 1, not sick = 0
# add variables to glm with survey design
a1 <- svyglm(sick ~ PPGENDER, des3, family = quasibinomial())  # by gender
a2 <- update(a1, ~ . + PPETHM)  # adjust for race
a3 <- update(a2, ~ . + work)  #  work
a4 <- update(a3, ~ . + marital)  # marital status

# memisc; can modify summary function
mtable(a1, a2, a3, a4)
```

```
## 
## Calls:
## a1: svyglm(formula = sick ~ PPGENDER, des3, family = quasibinomial())
## a2: svyglm(formula = sick ~ PPGENDER + PPETHM, des3, family = quasibinomial())
## a3: svyglm(formula = sick ~ PPGENDER + PPETHM + work, des3, family = quasibinomial())
## a4: svyglm(formula = sick ~ PPGENDER + PPETHM + work + marital, des3, 
##     family = quasibinomial())
## 
## ==================================================================================================
##                                                          a1         a2         a3         a4      
## --------------------------------------------------------------------------------------------------
##   (Intercept)                                         -1.556***  -1.729***  -1.886***  -1.883***  
##                                                       (0.089)    (0.095)    (0.126)    (0.144)    
##   PPGENDER: Female/Male                                0.298*     0.314**    0.337**    0.337**   
##                                                       (0.119)    (0.119)    (0.120)    (0.120)    
##   PPETHM: Black, Non-Hispanic/White, Non-Hispanic                 0.162      0.164      0.162     
##                                                                  (0.201)    (0.202)    (0.205)    
##   PPETHM: Hispanic/White, Non-Hispanic                            0.603***   0.616***   0.615***  
##                                                                  (0.168)    (0.167)    (0.168)    
##   PPETHM: Other, Non-Hispanic/White, Non-Hispanic                 0.487      0.458      0.458     
##                                                                  (0.264)    (0.265)    (0.265)    
##   PPETHM: 2+ Races, Non-Hispanic/White, Non-Hispanic              0.401      0.397      0.397     
##                                                                  (0.284)    (0.284)    (0.284)    
##   work: employed/unemployed                                                  0.237      0.238     
##                                                                             (0.124)    (0.124)    
##   marital: partnered/single                                                            -0.006     
##                                                                                        (0.124)    
## --------------------------------------------------------------------------------------------------
##   Aldrich-Nelson R-sq.                                    0.0        0.0        0.0        0.0    
##   McFadden R-sq.                                          0.0        0.0        0.0        0.0    
##   Cox-Snell R-sq.                                         0.0        0.0        0.0        0.0    
##   Nagelkerke R-sq.                                        0.0        0.0        0.0        0.0    
##   phi                                                     1.0        1.0        1.0        1.0    
##   Likelihood-ratio                                        7.5       27.7       32.2       32.2    
##   p                                                       0.0        0.0        0.0        0.0    
##   Log-likelihood                                       2133.2     2113.0     2108.6     2108.5    
##   Deviance                                             2133.2     2113.0     2108.6     2108.5    
##   N                                                    2146       2146       2146       2146      
## ==================================================================================================
```

```r
# AOR for last glm
exp(coefficients(a4))
```

```
##                  (Intercept)               PPGENDERFemale 
##                       0.1521                       1.4011 
##    PPETHMBlack, Non-Hispanic               PPETHMHispanic 
##                       1.1763                       1.8491 
##    PPETHMOther, Non-Hispanic PPETHM2+ Races, Non-Hispanic 
##                       1.5806                       1.4872 
##                 workemployed             maritalpartnered 
##                       1.2682                       0.9943
```

When adjusting for gender, ethnicity, employment, and marital status, significant variables for being sick included being female (compared to males) and hispanic (compared to white).


***

## Q13. Do you get an influenza vaccine? (Yes, every year; Yes, some years; No, never)


```r
# first make sure Q13 has been re-grouped
head(datar$Q13)  # original: 3 answers
```

```
## [1] Yes, every year <NA>            Yes, every year Yes, some years
## [5] Yes, every year Yes, some years
## Levels: Yes, every year Yes, some years No, never
```

```r
head(df$q13)  # updated: 2 answers
```

```
## [1] Yes  <NA> Yes  Yes  Yes  Yes 
## Levels: Yes No
```

```r
# relevel q13; no vaccine = reference
df$q13 <- relevel(df$q13, "No")

# update survey object
options(survey.lonely.psu = "adjust")
des4 <- svydesign(ids = ~1, weights = ~weight, data = df)

# getting sick ~ getting vaccine
m <- svyglm(sick ~ q13, des4, family = quasibinomial)
summary(m)
```

```
## 
## Call:
## svyglm(formula = sick ~ q13, des4, family = quasibinomial)
## 
## Survey design:
## svydesign(ids = ~1, weights = ~weight, data = df)
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -1.5648     0.0988  -15.84   <2e-16 ***
## q13Yes        0.2800     0.1231    2.27    0.023 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for quasibinomial family taken to be 0.9979)
## 
## Number of Fisher Scoring iterations: 4
```

```r
# OR
exp(coefficients(m))
```

```
## (Intercept)      q13Yes 
##      0.2091      1.3231
```

Those receiving the vaccine had 1.3231 higher odds of getting sick compared to those who did not get the vaccine.



```r
# Q2 + Q13 tables with weights
svytable(~q13 + sick, des4, round = T)  # sick = 1, not sick = 0
```

```
##      sick
## q13     0   1
##   No  711 149
##   Yes 999 276
```

```r
(t2 <- prop.table(svytable(~q13 + sick, des4), margin = 2)* 100)
```

```
##      sick
## q13       0     1
##   No  41.57 34.97
##   Yes 58.43 65.03
```

```r
(t1 <- prop.table(svytable(~q13 + sick, des4), margin = 1)* 100)
```

```
##      sick
## q13       0     1
##   No  82.70 17.30
##   Yes 78.33 21.67
```

Out of those sick, 65.0288% reported receiving the vaccine.  
Out of those healthy, 34.9712% received the vaccine (41.573% did not).  
  
Vaccinated group: 17.2955% report no illness.  
Unvaccinated group: 82.7045% report no illness.  
  

***

### Subset sick + vaccinated group. 

```r
# subset data for sick and vaccinated
sick_vax <- df %>%
  mutate(sickvax = ifelse((sick == 1 & q13 == "Yes"), 1, 0))

sick_vax$sickvax <- as.factor(sick_vax$sickvax)
str(sick_vax$sickvax)
```

```
##  Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 2 1 ...
```

```r
# table of sick and vaccine status
xtabs(~sick + q13, sick_vax)
```

```
##     q13
## sick   No  Yes
##    0  674 1053
##    1  144  269
```

```r
# sick and vaccinated = 1, else = 0
table(sick_vax$sickvax)
```

```
## 
##    0    1 
## 1880  269
```

```r
# update survey object
options(survey.lonely.psu = "adjust")
des5 <- svydesign(ids = ~1, weights = ~weight, data = sick_vax)

## who is getting sick after vaccinations?
# age group
ftable(svyby(~sickvax, ~ppagecat, des5, svymean, na.rm = T))
```

```
##                   sickvax0 sickvax1
## ppagecat                           
## 18-24    svymean   0.85953  0.14047
##          SE        0.02729  0.02729
## 25-34    svymean   0.89066  0.10934
##          SE        0.01887  0.01887
## 35-44    svymean   0.83698  0.16302
##          SE        0.02172  0.02172
## 45-54    svymean   0.87832  0.12168
##          SE        0.01742  0.01742
## 55-64    svymean   0.88228  0.11772
##          SE        0.01527  0.01527
## 65-74    svymean   0.84925  0.15075
##          SE        0.02287  0.02287
## 75+      svymean   0.93337  0.06663
##          SE        0.02197  0.02197
```

```r
ftable(svyby(~ppagecat, ~sickvax, des5, svymean, na.rm = T))
```

```
##                  ppagecat18-24 ppagecat25-34 ppagecat35-44 ppagecat45-54 ppagecat55-64 ppagecat65-74 ppagecat75+
## sickvax                                                                                                         
## 0       svymean       0.117019      0.176415      0.164462      0.158853      0.200725      0.124578    0.057948
##         SE            0.009241      0.010315      0.009539      0.008519      0.009350      0.007300    0.005136
## 1       svymean       0.129351      0.146472      0.216646      0.148848      0.181136      0.149569    0.027978
##         SE            0.024790      0.024425      0.027704      0.021386      0.023224      0.023188    0.009463
```

```r
# gender
ftable(svyby(~sickvax, ~PPGENDER, des5, svymean, na.rm = T))
```

```
##                   sickvax0 sickvax1
## PPGENDER                           
## Male     svymean   0.88367  0.11633
##          SE        0.01070  0.01070
## Female   svymean   0.85971  0.14029
##          SE        0.01145  0.01145
```

```r
ftable(svyby(~PPGENDER, ~sickvax, des5, svymean, na.rm = T))
```

```
##                  PPGENDERMale PPGENDERFemale
## sickvax                                     
## 0       svymean       0.48603        0.51397
##         SE            0.01243        0.01243
## 1       svymean       0.43275        0.56725
##         SE            0.03236        0.03236
```


### Run regression and adjust for demographic variables.

```r
m1 <- svyglm(sickvax ~ PPGENDER, des5, family = quasibinomial())
m2 <- update(m1, ~ . + PPETHM)
m3 <- update(m2, ~ . + work)
m4 <- update(m3, ~ . + ppagecat)

mtable(m1, m2, m3, m4)  # this isn't very helpful
```

```
## 
## Calls:
## m1: svyglm(formula = sickvax ~ PPGENDER, des5, family = quasibinomial())
## m2: svyglm(formula = sickvax ~ PPGENDER + PPETHM, des5, family = quasibinomial())
## m3: svyglm(formula = sickvax ~ PPGENDER + PPETHM + work, des5, family = quasibinomial())
## m4: svyglm(formula = sickvax ~ PPGENDER + PPETHM + work + ppagecat, 
##     des5, family = quasibinomial())
## 
## ==================================================================================================
##                                                          m1         m2         m3         m4      
## --------------------------------------------------------------------------------------------------
##   (Intercept)                                         -2.028***  -2.215***  -2.352***  -2.334***  
##                                                       (0.104)    (0.111)    (0.153)    (0.287)    
##   PPGENDER: Female/Male                                0.215      0.233      0.254      0.274     
##                                                       (0.141)    (0.141)    (0.144)    (0.144)    
##   PPETHM: Black, Non-Hispanic/White, Non-Hispanic                 0.123      0.124      0.119     
##                                                                  (0.243)    (0.243)    (0.246)    
##   PPETHM: Hispanic/White, Non-Hispanic                            0.624**    0.634**    0.639**   
##                                                                  (0.194)    (0.193)    (0.198)    
##   PPETHM: Other, Non-Hispanic/White, Non-Hispanic                 0.670*     0.645*     0.595*    
##                                                                  (0.290)    (0.293)    (0.291)    
##   PPETHM: 2+ Races, Non-Hispanic/White, Non-Hispanic             -0.285     -0.289     -0.287     
##                                                                  (0.411)    (0.411)    (0.419)    
##   work: employed/unemployed                                                  0.207      0.224     
##                                                                             (0.149)    (0.175)    
##   ppagecat: 25-34/18-24                                                                -0.272     
##                                                                                        (0.301)    
##   ppagecat: 35-44/18-24                                                                 0.196     
##                                                                                        (0.282)    
##   ppagecat: 45-54/18-24                                                                -0.137     
##                                                                                        (0.283)    
##   ppagecat: 55-64/18-24                                                                -0.115     
##                                                                                        (0.275)    
##   ppagecat: 65-74/18-24                                                                 0.269     
##                                                                                        (0.307)    
##   ppagecat: 75+/18-24                                                                  -0.562     
##                                                                                        (0.437)    
## --------------------------------------------------------------------------------------------------
##   Aldrich-Nelson R-sq.                                    0.0        0.0        0.0        0.0    
##   McFadden R-sq.                                          0.0        0.0        0.0        0.0    
##   Cox-Snell R-sq.                                         0.0        0.0        0.0        0.0    
##   Nagelkerke R-sq.                                        0.0        0.0        0.0        0.0    
##   phi                                                     1.0        1.0        1.0        1.0    
##   Likelihood-ratio                                        2.7       21.8       24.1       34.7    
##   p                                                       0.1        0.0        0.0        0.0    
##   Log-likelihood                                       1645.5     1626.5     1624.2     1613.6    
##   Deviance                                             1645.5     1626.5     1624.2     1613.6    
##   N                                                    2145       2145       2145       2145      
## ==================================================================================================
```


***

## in progress




