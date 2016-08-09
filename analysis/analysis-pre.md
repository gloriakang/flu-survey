# Comparing weighted and unweighted survey data

# Comparing weighted and unweighted survey data




```r
## Load data variables.

load("~/git/flu-survey/data/cleaning2.RData")
load("~/git/flu-survey/data/recoding.RData")  # load "datar"
df <- datar  # contains recoded variables
```


```r
## Create survey object.

options(digits = 4)
options(survey.lonely.psu = "adjust")
des <- svydesign(ids = ~1, weights = ~weight, data = df[is.na(df$weight) == F, ])
```

### Example of Q1 by gender.


```r
## Example tables of Q1 by gender.

as.data.frame(svytable(~Q1 + PPGENDER, design = des, round = T))  # weighted counts
```

```
##    Q1 PPGENDER Freq
## 1 Yes   Female  887
## 2  No   Female  231
## 3 Yes     Male  725
## 4  No     Male  304
```

```r
svyby(~Q1, ~Q1+PPGENDER, des, unwtd.count)  # unweighted counts
```

```
##             Q1 PPGENDER counts se
## Yes.Female Yes   Female    888  0
## No.Female   No   Female    205  0
## Yes.Male   Yes     Male    776  0
## No.Male     No     Male    283  0
```

```r
svyby(~Q1, ~PPGENDER, design = des, FUN = svymean, na.rm = T)  # weighted %
```

```
##        PPGENDER  Q1Yes   Q1No se.Q1Yes se.Q1No
## Female   Female 0.7937 0.2063  0.01373 0.01373
## Male       Male 0.7045 0.2955  0.01562 0.01562
```

```r
plot(svytable(~Q1 + PPGENDER, des))  # default survey plot
```

![](analysis-pre_files/figure-html/example-1.png)<!-- -->

```r
## generic ggplot
# ggplot(data.frame.here, aes(Q1, Freq, fill = PPGENDER) + geom_bar(stat = 'identity', position = position_dodge())
```

## Q1.

### By demographic variables:
- gender, ethnicity, age, education, income, employment, marital status, metro location, region, house type, head of household, rent status, state, internet availability


```r
# save freq table as data.frame
q1 <- as.data.frame(svytable(
  ~Q1 + PPGENDER + PPETHM + ppagect4, design = des, round = T))

# make ggplot object
g1 <- ggplot(q1)
g1 + aes(Q1, Freq, fill = PPGENDER) + 
  geom_bar(stat = 'identity', position = position_dodge())
```

![](analysis-pre_files/figure-html/q1-1.png)<!-- -->

```r
g1 + aes(Q1, Freq, fill = PPETHM) + 
  geom_bar(stat = 'identity', position = position_dodge())
```

![](analysis-pre_files/figure-html/q1-2.png)<!-- -->

```r
g1 + aes(Q1, Freq, fill = ppagect4) + 
  geom_bar(stat = 'identity', position = position_stack())
```

![](analysis-pre_files/figure-html/q1-3.png)<!-- -->

### Examine the % of US adults sick with ILI last year by sex, ethnicity, and age. Do a survey-corrected chi-square test for independence.


```r
## % of US adults sick last year with ILI by sex
sex <- svyby(formula = ~Q2, by = ~PPGENDER, design = des, FUN = svymean, na.rm = T)
svychisq(~Q2 + PPGENDER, design = des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + PPGENDER, design = des)
## F = 6.3, ndf = 1, ddf = 2200, p-value = 0.01
```

```r
qplot(x = sex$PPGENDER, y = sex$Q2Yes, data = sex, xlab = "sex", ylab = "% sick") + geom_errorbar(aes(x = PPGENDER, ymin = Q2Yes - se.Q2Yes, ymax = Q2Yes + se.Q2Yes), width = .25) + ggtitle(label = "% of adults sick last year with ILI by sex")
```

![](analysis-pre_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
## % of US adults sick last year with ILI by ethnicity
eth <- svyby(formula = ~Q2, by = ~PPETHM, design = des, FUN = svymean, na.rm = T)
svychisq(~Q2 + PPETHM, design = des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + PPETHM, design = des)
## F = 4.3, ndf = 3.4, ddf = 7400.0, p-value = 0.003
```

```r
qplot(x = eth$PPETHM, y = eth$Q2Yes, data = eth, xlab = "ethnicity", ylab = "% sick") + geom_errorbar(aes(x = PPETHM, ymin = Q2Yes - se.Q2Yes, ymax = Q2Yes + se.Q2Yes), width = .25) + ggtitle(label = "% of adults sick last year with ILI by ethnicity")
```

![](analysis-pre_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
## % of US adults sick last year with ILI by age
age <- svyby(formula = ~Q2, by = ~ppagecat, design = des, FUN = svymean, na.rm = T)
svychisq(~Q2 + ppagecat, design = des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + ppagecat, design = des)
## F = 2.1, ndf = 5.8, ddf = 13000.0, p-value = 0.06
```

```r
qplot(x = age$ppagecat, y = age$Q2Yes, data = age, xlab = "age", ylab = "% sick") + geom_errorbar(aes(x = ppagecat, ymin = Q2Yes - se.Q2Yes, ymax = Q2Yes + se.Q2Yes), width = .25) + ggtitle(label = "% of adults sick last year with ILI by age")
```

![](analysis-pre_files/figure-html/unnamed-chunk-2-3.png)<!-- -->

### Simple weighted plots.


```r
## weighted bivariate plots with svytable
qtest <- as.data.frame(svytable(
  ~Q1 + PPGENDER + PPETHM + ppagect4, design = des, round = T))

p <- ggplot(qtest, aes(weight = Freq))

#svytable(~Q1 + PPGENDER, des, round = T)
(a <- p + aes(PPGENDER, fill = Q1) + geom_bar(width = 0.7, position = "fill"))
```

![](analysis-pre_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#svytable(~Q1 + PPETHM, des, round = T)
(b <- p + aes(PPETHM, fill = Q1) + geom_bar(width = 0.7, position = "fill") +
  geom_point(aes(y = -0.05), size = 0.75, alpha = 0.3, position = position_jitter(h = 0.01)))
```

![](analysis-pre_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
#svytable(~Q1 + ppagect4, des, round = T)
(c <- p + aes(ppagect4, fill = Q1) + geom_bar(position = "fill") +
  geom_point(aes(y = -0.05), size = 0.75, alpha = 0.3, position = position_jitter(h = 0.01)))
```

![](analysis-pre_files/figure-html/unnamed-chunk-3-3.png)<!-- -->

```r
grid.arrange(a, b, c)
```

![](analysis-pre_files/figure-html/unnamed-chunk-3-4.png)<!-- -->

### Compare weighted plots to unweighted plots.


```r
## unweighted plots
q <- ggplot(df[!is.na(df$Q1), ])

with(df, table(PPGENDER, Q1))
```

```
##         Q1
## PPGENDER Yes  No
##   Female 888 205
##   Male   776 283
```

```r
a <- q + geom_bar(aes(PPGENDER, fill = Q1), position = "dodge")

with(df, table(PPETHM, Q1))
```

```
##                         Q1
## PPETHM                    Yes   No
##   White, Non-Hispanic    1235  322
##   Black, Non-Hispanic     143   50
##   Hispanic                161   69
##   Other, Non-Hispanic      63   29
##   2+ Races, Non-Hispanic   62   18
```

```r
b <- q + geom_bar(aes(Q1, fill = PPETHM), position = "dodge")

with(df, table(ppagect4, Q1))
```

```
##         Q1
## ppagect4 Yes  No
##    18-29 222  98
##    30-44 360 108
##    45-59 514 132
##    60+   568 150
```

```r
c <- q + geom_bar(aes(Q1, fill = ppagect4), position = "dodge")

grid.arrange(a, b, c)
```

![](analysis-pre_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


## Testing.





