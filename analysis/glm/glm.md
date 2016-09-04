# Survey-weighted generalized linear models
  



```r
## Load data and recoded variables.

load("~/git/flu-survey/data/cleaning2.RData")
data <- dataf

#source(recoding.R)
load("~/git/flu-survey/data/recoding.RData")
df <- datar  # datar contains recoded variables
#tail(df)
```


```r
## Create survey object with weights, des

options(digits = 4)
options(survey.lonely.psu = "adjust")
des <- svydesign(ids = ~1, weights = ~weight, data = data[is.na(data$weight) == F, ])
#summary(des)
```

# Survey questions

## Q1. Before receiving this survey, did you know influenza is different from the stomach flu?
  
### Compare sex ratios in unweighted and weighted data frames for Q1.


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
# another unweighted table
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
# unweighted prop
(u <- with(data, prop.table(table(Q1, PPGENDER), margin = 2))*100)  
```

```
##      PPGENDER
## Q1    Female  Male
##   Yes  81.24 73.28
##   No   18.76 26.72
```

```r
# weighted prop
(w <- prop.table(svytable(~Q1 + PPGENDER, design = des), margin = 2)*100)
```

```
##      PPGENDER
## Q1    Female  Male
##   Yes  79.37 70.45
##   No   20.63 29.55
```

```r
# figure out how to plot weighted survey designs

# table above with standard errors
svyby(formula = ~Q1, by = ~PPGENDER, design = des, FUN = svymean, na.rm = T)
```

```
##        PPGENDER  Q1Yes   Q1No se.Q1Yes se.Q1No
## Female   Female 0.7937 0.2063  0.01373 0.01373
## Male       Male 0.7045 0.2955  0.01562 0.01562
```

In the unweighted data frame, 81.2442818% of females and 73.2766761% of males answered Yes. 26.7233239% of males and 18.7557182% of females answered No.  
In the weighted data frame, 79.3709018% of females and 70.4534438% of males answered Yes. 29.5465562% of males and 20.6290982% of females answered No.  
  

## Q2. Have you had an illness with influenza-like symptoms since August 2015?

### First look at unweighted data. Calculate unadjusted OR for being sick by gender.


```r
## without weights

# recode datar$Q2
head(datar$Q2)
```

```
## [1] No No No No No No
## Levels: Yes No
```

```r
df$sick <- car::recode(datar$Q2, recodes = "'Yes' = 1; 'No' = 0; NA = NA")
head(df$sick)
```

```
## [1] 0 0 0 0 0 0
## Levels: 0 1
```

```r
# table Q2 by gender
with(df, table(Q2, PPGENDER))
```

```
##      PPGENDER
## Q2    Female Male
##   Yes    234  180
##   No     858  877
```

```r
# glm without weights
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

The unadjusted odds ratio of being sick as a male is 0.7525656 compared to females.


### Apply survey design and weights for Q2. Calculate unadjusted OR for getting sick by gender.


```r
## With survey weights

# relevel PPGENDER
df$PPGENDER <- relevel(datar$PPGENDER, "Male")

# create updated survey object with weights = des2
options(survey.lonely.psu = "adjust")
des2 <- svydesign(ids = ~1, weights = ~weight, data = df)  # data = df

# svyglm with weighted model
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
(or1 <- exp(coefficients(m1)))  # unadjusted OR
```

```
##    (Intercept) PPGENDERFemale 
##          0.211          1.347
```

```r
exp(confint(m1))  # 95% CI 
```

```
##                 2.5 % 97.5 %
## (Intercept)    0.1773 0.2511
## PPGENDERFemale 1.0676 1.7005
```

In the weighted data frame, females had 1.3473578 times the odds of being sick compared to males.

### Calculate OR for being sick, adjusted for ethnicity


```r
# being sick, by ethnicity
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
(or2 <- exp(coefficients(m2)))  # OR adjusted for race
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
exp(confint(m2))  # note 95% CI for hispanic group
```

```
##                               2.5 % 97.5 %
## (Intercept)                  0.1839 0.2418
## PPETHMBlack, Non-Hispanic    0.8004 1.7514
## PPETHMHispanic               1.3008 2.5104
## PPETHMOther, Non-Hispanic    0.9606 2.6743
## PPETHM2+ Races, Non-Hispanic 0.8687 2.6023
```

Compared to whites, OR for being sick are 1.1840113, 1.8070876, 1.6027619, 1.5035254 for blacks, hispanics, others, and 2+ races, respectively.  
  
Notes: When a logistic regression is calculated, the regression coefficient (b1) is the estimated increase in the log odds of the outcome per unit increase in the value of the exposure; the exponential function of the regression coefficient (e^b1) is the odds ratio associated with a one-unit increase in the exposure.  
  


***

### Compare model variables for being sick.


```r
#des2 <- svydesign(ids = ~1, weights = ~weight, data = df)

## sick = 1, not sick = 0
# add variables to glm with survey design
a1 <- svyglm(sick ~ PPGENDER, des2, family = quasibinomial())  # by gender
a2 <- update(a1, ~ . + PPETHM)  # adjust for race
a3 <- update(a2, ~ . + work)  #  work
a4 <- update(a3, ~ . + marital)  # marital status

# memisc: need to modify summary function
mtable(a1, a2, a3, a4)
```

```
## 
## Calls:
## a1: svyglm(formula = sick ~ PPGENDER, des2, family = quasibinomial())
## a2: svyglm(formula = sick ~ PPGENDER + PPETHM, des2, family = quasibinomial())
## a3: svyglm(formula = sick ~ PPGENDER + PPETHM + work, des2, family = quasibinomial())
## a4: svyglm(formula = sick ~ PPGENDER + PPETHM + work + marital, des2, 
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
(or <- exp(coefficients(a4)))
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

When adjusting for gender, ethnicity, employment, and marital status, significant variables for being sick included female gender (OR = 1.4010761) and hispanic ethnicity (OR = 1.8491467)

***

## Q13. Do you get an influenza vaccine? (Yes, every year; Yes, some years; No, never)


```r
# first make sure Q13 has been re-grouped
head(datar$Q13)  # original
```

```
## [1] Yes, every year <NA>            Yes, every year Yes, some years
## [5] Yes, every year Yes, some years
## Levels: Yes, every year Yes, some years No, never
```

```r
head(df$q13)  # updated
```

```
## [1] Yes  <NA> Yes  Yes  Yes  Yes 
## Levels: Yes No
```

```r
# relevel q13
df$q13 <- relevel(df$q13, "No")

# update survey object
options(survey.lonely.psu = "adjust")
des3 <- svydesign(ids = ~1, weights = ~weight, data = df)

# getting sick ~ getting vaccine
m5 <- svyglm(sick ~ q13, des3, family = quasibinomial)
summary(m5)
```

```
## 
## Call:
## svyglm(formula = sick ~ q13, des3, family = quasibinomial)
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
(or5 <- exp(coefficients(m5)))
```

```
## (Intercept)      q13Yes 
##      0.2091      1.3231
```

Those who get the vaccine had 1.3231002 the odds of getting sick compared to those who do not get the vaccine.


```r
# Q2 + Q13 tables with weights
svytable(~q13 + sick, design = des3, round = T)  # sick = 1, not sick = 0
```

```
##      sick
## q13     0   1
##   No  711 149
##   Yes 999 276
```

```r
(t2 <- prop.table(svytable(~q13 + sick, design = des2), margin = 2)* 100)
```

```
##      sick
## q13       0     1
##   Yes 58.43 65.03
##   No  41.57 34.97
```

```r
(t1 <- prop.table(svytable(~q13 + sick, design = des2), margin = 1)* 100)
```

```
##      sick
## q13       0     1
##   Yes 78.33 21.67
##   No  82.70 17.30
```

Out of those who reported being sick, 34.9712462% reported receiving vaccine.  
Out of those who were healthy, 65.0287538% received vaccine (58.4269653% did not).  
  
Vaccinated group: 21.6725711% report no illness.  
Unvaccinated group: 78.3274289% report no illness.  
  
***

### Logistic regression for Q2 and Q13. Subset sick + vaccinated group. 


```r
# subset sick and vaccinated into new df2
df2 <- df %>%
  mutate(sickq13 = ifelse((sick == 1 & q13 == "Yes"), 1, 0))

df2$sickq13 <- as.factor(df2$sickq13)
str(df2$sickq13)
```

```
##  Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 2 1 ...
```

```r
# table of sick and vaccine status
xtabs(~sick + q13, df2)
```

```
##     q13
## sick   No  Yes
##    0  674 1053
##    1  144  269
```

```r
# sick and vaccinated = 1, else = 0
table(df2$sickq13)
```

```
## 
##    0    1 
## 1880  269
```

### Run regression and adjust for demographic variables.


```r
# update survey object
options(survey.lonely.psu = "adjust")
des4 <- svydesign(ids = ~1, weights = ~weight, data = df2)

m1 <- svyglm(sickq13 ~ PPGENDER, des4, family = quasibinomial())
m2 <- update(m1, ~ . + PPETHM)  # add race
m3 <- update(m2, ~ . + work)  # add work
m4 <- update(m3, ~ . + ppagecat)  # add age

# memisc, need to modify summary function
mtable(m1, m2, m3, m4)  # this isn't very helpful
```

```
## 
## Calls:
## m1: svyglm(formula = sickq13 ~ PPGENDER, des4, family = quasibinomial())
## m2: svyglm(formula = sickq13 ~ PPGENDER + PPETHM, des4, family = quasibinomial())
## m3: svyglm(formula = sickq13 ~ PPGENDER + PPETHM + work, des4, family = quasibinomial())
## m4: svyglm(formula = sickq13 ~ PPGENDER + PPETHM + work + ppagecat, 
##     des4, family = quasibinomial())
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

```r
# who is getting sick after vaccinations?
svyby(formula = ~ppagecat, by = ~sickq13, design = des4, FUN = svymean, na.rm = T)
```

```
##   sickq13 ppagecat18-24 ppagecat25-34 ppagecat35-44 ppagecat45-54
## 0       0        0.1170        0.1764        0.1645        0.1589
## 1       1        0.1294        0.1465        0.2166        0.1488
##   ppagecat55-64 ppagecat65-74 ppagecat75+ se.ppagecat18-24
## 0        0.2007        0.1246     0.05795         0.009241
## 1        0.1811        0.1496     0.02798         0.024790
##   se.ppagecat25-34 se.ppagecat35-44 se.ppagecat45-54 se.ppagecat55-64
## 0          0.01032         0.009539         0.008519          0.00935
## 1          0.02443         0.027704         0.021386          0.02322
##   se.ppagecat65-74 se.ppagecat75+
## 0          0.00730       0.005136
## 1          0.02319       0.009463
```

```r
svyby(formula = ~PPGENDER, by = ~sick + q13, design = des4, FUN = svymean, na.rm = T)
```

```
##       sick q13 PPGENDERMale PPGENDERFemale se.PPGENDERMale
## 0.No     0  No       0.5169         0.4831         0.02082
## 1.No     1  No       0.3904         0.6096         0.04385
## 0.Yes    0 Yes       0.4752         0.5248         0.01651
## 1.Yes    1 Yes       0.4328         0.5672         0.03236
##       se.PPGENDERFemale
## 0.No            0.02082
## 1.No            0.04385
## 0.Yes           0.01651
## 1.Yes           0.03236
```


## in progress






