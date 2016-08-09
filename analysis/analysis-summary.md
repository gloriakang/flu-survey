# Analysis summary

Hide all code: include = FALSE








```r
## Create survey object.

options(digits = 4)
options(survey.lonely.psu = "adjust")

# subset data?
des <- svydesign(ids = ~1, weights = ~weight, data = df[is.na(df$weight) == F, ])
```

## Summary

- Default survey plot
- Univariate bar graph
- Bivariate graphs: gender, age, ethnicity, income, education, work status, marital status
- Plot means with error bars

## Q1. Before receiving this survey, did you know influenza is different from the stomach flu?


```r
# default survey plot
plot(svytable(~Q1 + PPGENDER, des))
```

![](analysis-summary_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# save weighted data frame for Q1 + variables
q1 <- as.data.frame(svytable(~Q1 + PPGENDER + ppagect4 + PPETHM + income + PPEDUCAT + work + marital, des, round = T))

# create ggplot templates
ptext <- theme(axis.text = element_text(size = rel(0.9)), axis.text.x = element_text(angle = 45, hjust = 1))
# ggplot object
p <- ggplot(q1, aes(Q1, weight = Freq)) + ptext

# univarate plot
p + geom_bar()
```

![](analysis-summary_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
#svytable(~Q1 + PPGENDER, des, round = T)
(gen <- p + aes(PPGENDER, fill = Q1) + geom_bar(position = "dodge"))
```

![](analysis-summary_files/figure-html/unnamed-chunk-3-3.png)<!-- -->

```r
#svytable(~Q1 + ppagect4, des, round = T)
(age <- p + aes(ppagect4, fill = Q1) + geom_bar(position = "dodge"))
```

![](analysis-summary_files/figure-html/unnamed-chunk-3-4.png)<!-- -->

```r
#svytable(~Q1 + PPETHM, des, round = T)
(eth <- p + aes(PPETHM, fill = Q1) + coord_flip() + geom_bar(position = "dodge"))
```

![](analysis-summary_files/figure-html/unnamed-chunk-3-5.png)<!-- -->

```r
#svytable(~Q1 + income, des, round = T)
(inc <- p + ptext + aes(income, fill = Q1) + geom_bar(position = "dodge"))
```

![](analysis-summary_files/figure-html/unnamed-chunk-3-6.png)<!-- -->

```r
#svytable(~Q1 + PPEDUCAT, des, round = T)
(edu <- p + aes(PPEDUCAT, fill = Q1) + geom_bar(position = "dodge"))
```

![](analysis-summary_files/figure-html/unnamed-chunk-3-7.png)<!-- -->

```r
#svytable(~Q1 + work, des, round = T)
(wor <- p + aes(work, fill = Q1) + geom_bar(position = "dodge"))
```

![](analysis-summary_files/figure-html/unnamed-chunk-3-8.png)<!-- -->

```r
#svytable(~Q1 + marital, des, round = T)
(mar <- p + aes(marital, fill = Q1) + geom_bar(position = "dodge"))
```

![](analysis-summary_files/figure-html/unnamed-chunk-3-9.png)<!-- -->

```r
#grid.arrange(all, gen, age, eth)
#grid.arrange(inc, edu, wor, mar)
```

## Q2. Have you had an illness with influenza-like symptoms since August 2015?


```r
q2 <- as.data.frame(svytable(~Q2 + PPGENDER + ppagect4 + PPETHM + income + PPEDUCAT + work + marital, des, round = T))

p <- ggplot(q2, aes(Q2, weight = Freq)) + ptext
all <- p + geom_bar()

#svytable(~Q2 + PPGENDER, des, round = T)
gen <- p + aes(PPGENDER, fill = Q2) + geom_bar(position = "dodge")
#svytable(~Q2 + ppagect4, des, round = T)
age <- p + aes(ppagect4, fill = Q2) + geom_bar(position = "dodge")
#svytable(~Q2 + PPETHM, des, round = T)
eth <- p + aes(PPETHM, fill = Q2) + geom_bar(position = "dodge")
#svytable(~Q2 + income, des, round = T)
inc <- p + ptext + aes(income, fill = Q2) + geom_bar(position = "dodge")
#svytable(~Q2 + PPEDUCAT, des, round = T)
edu <- p + ptext + aes(PPEDUCAT, fill = Q2) + geom_bar(position = "dodge")
#svytable(~Q2 + work, des, round = T)
wor <- p + aes(work, fill = Q2) + geom_bar(position = "dodge")
#svytable(~Q2 + marital, des, round = T)
mar <- p + aes(marital, fill = Q2) + geom_bar(position = "dodge")

grid.arrange(all, gen, age, eth)
```

![](analysis-summary_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
grid.arrange(inc, edu, wor, mar)
```

![](analysis-summary_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

### Examine the % of US adults sick with ILI last year by sex, ethnicity, and age. Do a survey-corrected chi-square test for independence.


```r
## ggplot template
er <- geom_errorbar(aes(ymin = Q2Yes - se.Q2Yes, ymax = Q2Yes + se.Q2Yes), width = .25)

## % of US adults sick last year with ILI by sex
gen <- svyby(~Q2, ~PPGENDER, des, svymean, na.rm = T)
svychisq(~Q2 + PPGENDER, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + PPGENDER, des)
## F = 6.3, ndf = 1, ddf = 2200, p-value = 0.01
```

```r
ggplot(gen, aes(PPGENDER, Q2Yes)) + geom_point() + xlab("sex") + ylab("% sick") +
  er + ggtitle(label = "% of adults sick last year with ILI by sex") 
```

![](analysis-summary_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
## % of US adults sick last year with ILI by age
age <- svyby(~Q2, ~ppagecat, des, svymean, na.rm = T)
svychisq(~Q2 + ppagecat, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + ppagecat, des)
## F = 2.1, ndf = 5.8, ddf = 13000.0, p-value = 0.06
```

```r
ggplot(age, aes(ppagecat, Q2Yes)) + geom_point() + xlab("age") + ylab("% sick") +
  er + ggtitle(label = "% of adults sick last year with ILI by age") 
```

![](analysis-summary_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```r
## % of US adults sick last year with ILI by ethnicity
eth <- svyby(~Q2, ~PPETHM, des, svymean, na.rm = T)
svychisq(~Q2 + PPETHM, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + PPETHM, des)
## F = 4.3, ndf = 3.4, ddf = 7400.0, p-value = 0.003
```

```r
ggplot(eth, aes(PPETHM, Q2Yes)) + geom_point() + xlab("ethnicity") + ylab("% sick") + ptext +
  er + ggtitle(label = "% of adults sick last year with ILI by ethnicity") 
```

![](analysis-summary_files/figure-html/unnamed-chunk-5-3.png)<!-- -->

```r
## by income
inc <- svyby(~Q2, ~PPINCIMP, des, svymean, na.rm = T)
svychisq(~Q2 + PPINCIMP, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + PPINCIMP, des)
## F = 0.84, ndf = 18, ddf = 38000, p-value = 0.7
```

```r
ggplot(inc, aes(PPINCIMP, Q2Yes)) + geom_point() + xlab("income") + ylab("% sick") + coord_flip() +
  er + ggtitle(label = "% of adults sick last year with ILI by income") 
```

![](analysis-summary_files/figure-html/unnamed-chunk-5-4.png)<!-- -->

```r
## by education
edu <- svyby(~Q2, ~PPEDUCAT, des, svymean, na.rm = T)
svychisq(~Q2 + PPEDUCAT, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + PPEDUCAT, des)
## F = 0.67, ndf = 2.9, ddf = 6300.0, p-value = 0.6
```

```r
ggplot(edu, aes(PPEDUCAT, Q2Yes)) + geom_point() + xlab("education") + ylab("% sick") +
  er + ggtitle(label = "% of adults sick last year with ILI by education") 
```

![](analysis-summary_files/figure-html/unnamed-chunk-5-5.png)<!-- -->

```r
## by work status
wor <- svyby(~Q2, ~work, des, svymean, na.rm = T)
svychisq(~Q2 + work, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + work, des)
## F = 2.7, ndf = 1, ddf = 2200, p-value = 0.1
```

```r
ggplot(wor, aes(work, Q2Yes)) + geom_point() + xlab("work") + ylab("% sick") +
  er + ggtitle(label = "% of adults sick last year with ILI by work") 
```

![](analysis-summary_files/figure-html/unnamed-chunk-5-6.png)<!-- -->

```r
## by marital status
mar <- svyby(~Q2, ~PPMARIT, des, svymean, na.rm = T)
svychisq(~Q2 + PPMARIT, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + PPMARIT, des)
## F = 0.71, ndf = 5, ddf = 11000, p-value = 0.6
```

```r
ggplot(mar, aes(PPMARIT, Q2Yes)) + geom_point() + xlab("marital status") + ylab("% sick") +
  er + ggtitle(label = "% of adults sick last year with ILI by marital status") 
```

![](analysis-summary_files/figure-html/unnamed-chunk-5-7.png)<!-- -->

```r
# re-grouped marital status
mar2 <- svyby(~Q2, ~marital, des, svymean, na.rm = T)
svychisq(~Q2 + marital, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + marital, des)
## F = 0.094, ndf = 1, ddf = 2200, p-value = 0.8
```

```r
ggplot(mar2, aes(marital, Q2Yes)) + geom_point() + xlab("") + ylab("% sick") +
  er + ggtitle(label = "% of adults sick last year with ILI by marital status") 
```

![](analysis-summary_files/figure-html/unnamed-chunk-5-8.png)<!-- -->

```r
## by metro status
met <- svyby(~Q2, ~PPMSACAT, des, svymean, na.rm = T)
svychisq(~Q2 + PPMSACAT, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + PPMSACAT, des)
## F = 3.1, ndf = 1, ddf = 2200, p-value = 0.08
```

```r
ggplot(met, aes(PPMSACAT, Q2Yes)) + geom_point() + xlab("metro status") + ylab("% sick") +
  er + ggtitle(label = "% of adults sick last year with ILI by metro status")
```

![](analysis-summary_files/figure-html/unnamed-chunk-5-9.png)<!-- -->

```r
## by region cat4
reg <- svyby(~Q2, ~PPREG4, des, svymean, na.rm = T)
svychisq(~Q2 + PPREG4, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + PPREG4, des)
## F = 4.5, ndf = 3, ddf = 6500, p-value = 0.004
```

```r
ggplot(reg, aes(PPREG4, Q2Yes)) + geom_point() + xlab(" ") + ylab("% sick") +
  er + ggtitle(label = "% of adults sick last year with ILI by region")
```

![](analysis-summary_files/figure-html/unnamed-chunk-5-10.png)<!-- -->

```r
## by region cat9
reg9 <- svyby(~Q2, ~ppreg9, des, svymean, na.rm = T)
svychisq(~Q2 + ppreg9, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + ppreg9, des)
## F = 2, ndf = 8, ddf = 17000, p-value = 0.04
```

```r
ggplot(reg9, aes(ppreg9, Q2Yes)) + geom_point() + xlab(" ") + ylab("% sick") + ptext +
  er + ggtitle(label = "% of adults sick last year with ILI by region")
```

![](analysis-summary_files/figure-html/unnamed-chunk-5-11.png)<!-- -->

```r
## by state
sta <- svyby(~Q2, ~PPSTATEN, des, svymean, na.rm = T)
svychisq(~Q2 + PPSTATEN, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + PPSTATEN, des)
## F = 1.4, ndf = 48, ddf = 100000, p-value = 0.05
```

```r
ggplot(sta, aes(PPSTATEN, Q2Yes)) + geom_point() + xlab(" ") + ylab("% sick") + ptext + coord_flip() +
  er + ggtitle(label = "% of adults sick last year with ILI by state")
```

![](analysis-summary_files/figure-html/unnamed-chunk-5-12.png)<!-- -->

```r
## rent status
ren <- svyby(~Q2, ~PPRENT, des, svymean, na.rm = T)
svychisq(~Q2 + PPRENT, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + PPRENT, des)
## F = 2.9, ndf = 2, ddf = 4300, p-value = 0.05
```

```r
ggplot(ren, aes(PPRENT, Q2Yes)) + geom_point() + xlab(" ") + ylab("% sick") + ptext +
  er + ggtitle(label = "% of adults sick last year with ILI by rent")
```

![](analysis-summary_files/figure-html/unnamed-chunk-5-13.png)<!-- -->

## Q3. Has any other person in your household had an illness with influenza like symptoms since August 2015?


```r
q3 <- as.data.frame(svytable(~Q3 + PPGENDER + ppagect4 + PPETHM + income + PPEDUCAT + work + marital, des, round = T))

p <- ggplot(q3, aes(Q3, weight = Freq)) + ptext
p + geom_bar()
```

![](analysis-summary_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
#svytable(~Q + PPGENDER, des, round = T)

## % sick with sick household member
svychisq(~Q2 + Q3, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + Q3, des)
## F = 230, ndf = 2, ddf = 4300, p-value <2e-16
```

```r
q <- svyby(~Q2, ~Q3, des, svymean, na.rm = T)
ggplot(q, aes(Q3, Q2Yes)) + geom_point() + xlab(" ") + ylab("% sick") + er +
  ggtitle(label = "% of adults sick vs. having sick household member ") 
```

![](analysis-summary_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

## Q4. Does your job require you to have a lot of contact with the public?


```r
q4 <- as.data.frame(svytable(~Q4 + PPGENDER + ppagect4 + PPETHM + income + PPEDUCAT + work + marital, des, round = T))

p <- ggplot(q4, aes(Q4, weight = Freq)) + ptext
p + geom_bar()
```

![](analysis-summary_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
gen <- p + aes(PPGENDER, fill = Q4) + geom_bar(position = "dodge")
age <- p + aes(ppagect4, fill = Q4) + geom_bar(position = "dodge")
eth <- p + aes(PPETHM, fill = Q4) + geom_bar(position = "dodge")
inc <- p + aes(income, fill = Q4) + geom_bar(position = "dodge")

grid.arrange(gen, age)
```

![](analysis-summary_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

```r
grid.arrange(eth, inc)
```

![](analysis-summary_files/figure-html/unnamed-chunk-7-3.png)<!-- -->

```r
## % sick plot
svychisq(~Q2 + Q4, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + Q4, des)
## F = 14, ndf = 2, ddf = 4300, p-value = 7e-07
```

```r
q <- svyby(~Q2, ~Q4, des, svymean, na.rm = T)
ggplot(q, aes(Q4, Q2Yes)) + geom_point() + xlab(" ") + ylab("% sick") + ptext + er +
  ggtitle(label = "% of adults sick and having job with public contact ") 
```

![](analysis-summary_files/figure-html/unnamed-chunk-7-4.png)<!-- -->

## Q5. Do you have a car that you can use to travel to work?


```r
q5 <- as.data.frame(svytable(~Q5 + PPGENDER + ppagect4 + PPETHM + income + PPEDUCAT + work + marital, des, round = T))

p <- ggplot(q5, aes(Q5, weight = Freq)) + ptext
p + geom_bar()
```

![](analysis-summary_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
#svytable(~Q + PPGENDER, des, round = T)
```


## Q6. Do you regularly use public transportation?


```r
q6 <- as.data.frame(svytable(~Q6 + PPGENDER + ppagect4 + PPETHM + income + PPEDUCAT + work + marital, des, round = T))

p <- ggplot(q6, aes(Q6, weight = Freq)) + ptext
p + geom_bar()
```

![](analysis-summary_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
#svytable(~Q + PPGENDER, des, round = T)
gen <- p + aes(PPGENDER, fill = Q6) + geom_bar(position = "dodge")
age <- p + aes(ppagect4, fill = Q6) + geom_bar(position = "dodge")
eth <- p + aes(PPETHM, fill = Q6) + geom_bar(position = "dodge")
inc <- p + aes(income, fill = Q6) + geom_bar(position = "dodge")
edu <- p + aes(PPEDUCAT, fill = Q6) + geom_bar(position = "dodge")
wor <- p + aes(work, fill = Q6) + geom_bar(position = "dodge")
mar <- p + aes(marital, fill = Q6) + geom_bar(position = "dodge")
# add metro status

grid.arrange(gen, age, eth)
```

![](analysis-summary_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

```r
grid.arrange(inc, edu, wor, mar)
```

![](analysis-summary_files/figure-html/unnamed-chunk-9-3.png)<!-- -->

```r
## % sick plot
svychisq(~Q2 + Q6, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + Q6, des)
## F = 17, ndf = 1, ddf = 2200, p-value = 3e-05
```

```r
q <- svyby(~Q2, ~Q6, des, svymean, na.rm = T)
ggplot(q, aes(Q6, Q2Yes)) + geom_point() + xlab(" ") + ylab("% sick") + er +
  ggtitle(label = "% of adults sick and frequent public transportation use") 
```

![](analysis-summary_files/figure-html/unnamed-chunk-9-4.png)<!-- -->

## Q7. What types of public transportation do you regularly use?

## Q8. For what types of activities do you regularly use public transportation?

## Q9. Do other members of your household regularly use public transportation?


```r
q9 <- as.data.frame(svytable(
  ~Q9 + PPGENDER + ppagect4 + PPETHM + income + PPEDUCAT + work + marital, des, round = T))

p <- ggplot(q9, aes(Q9, weight = Freq)) + ptext
p + geom_bar()
```

![](analysis-summary_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
#svytable(~Q + PPGENDER, des, round = T)
```
## Q10. What types of public transportation do other members of your household regularly use?

## Q11. How do you rate your risk of getting influenza if you visited each of the following locations?

## Q12. Which of the following actions do you take to avoid getting sick?

## Q13. Do you get the flu vaccine?


```r
q13 <- as.data.frame(svytable(
  ~Q13 + PPGENDER + ppagect4 + PPETHM + income + PPEDUCAT + work + marital, des, round = T))

p <- ggplot(q13, aes(Q13, weight = Freq)) + ptext
p + geom_bar()
```

![](analysis-summary_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
#svytable(~Q + PPGENDER, des, round = T)
gen <- p + aes(PPGENDER, fill = Q13) + geom_bar(position = "dodge")
age <- p + aes(ppagect4, fill = Q13) + geom_bar(position = "dodge")
eth <- p + aes(PPETHM, fill = Q13) + geom_bar(position = "dodge")
inc <- p + aes(income, fill = Q13) + geom_bar(position = "dodge")
edu <- p + aes(PPEDUCAT, fill = Q13) + geom_bar(position = "dodge")
wor <- p + aes(work, fill = Q13) + geom_bar(position = "dodge")
mar <- p + aes(marital, fill = Q13) + geom_bar(position = "dodge")

grid.arrange(gen, age, eth)
```

![](analysis-summary_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

```r
grid.arrange(inc, edu, wor, mar)
```

![](analysis-summary_files/figure-html/unnamed-chunk-11-3.png)<!-- -->

```r
## sick plot
svychisq(~Q2 + Q13, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + Q13, des)
## F = 5.9, ndf = 2, ddf = 4300, p-value = 0.003
```

```r
q <- svyby(~Q2, ~Q13, des, svymean, na.rm = T)
ggplot(q, aes(Q13, Q2Yes)) + geom_point() + xlab(" ") + ylab("% sick") + er +
  ggtitle(label = "% of adults sick and getting flu vaccine") 
```

![](analysis-summary_files/figure-html/unnamed-chunk-11-4.png)<!-- -->

## Q14. How much do you pay to get an influenza vaccine?


```r
q14 <- as.data.frame(svytable(
  ~Q14 + PPGENDER + ppagect4 + PPETHM + income + PPEDUCAT + work + marital, des, round = T))

p <- ggplot(q14, aes(Q14, weight = Freq)) + ptext
p + geom_bar()
```

![](analysis-summary_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
#svytable(~Q + PPGENDER, des, round = T)
gen <- p + aes(PPGENDER, fill = Q14) + geom_bar(position = "dodge")
age <- p + aes(ppagect4, fill = Q14) + geom_bar(position = "dodge")
eth <- p + aes(PPETHM, fill = Q14) + geom_bar(position = "dodge")
inc <- p + aes(income, fill = Q14) + geom_bar(position = "dodge")
edu <- p + aes(PPEDUCAT, fill = Q14) + geom_bar(position = "dodge")
wor <- p + aes(work, fill = Q14) + geom_bar(position = "dodge")
mar <- p + aes(marital, fill = Q14) + geom_bar(position = "dodge")

grid.arrange(gen, age, eth)
```

![](analysis-summary_files/figure-html/unnamed-chunk-12-2.png)<!-- -->

```r
grid.arrange(inc, edu, wor, mar)
```

![](analysis-summary_files/figure-html/unnamed-chunk-12-3.png)<!-- -->

## Q15. Are you more likely to get a vaccine if others around you get a vaccine?


```r
q15 <- as.data.frame(svytable(
  ~Q15 + PPGENDER + ppagect4 + PPETHM + income + PPEDUCAT + work + marital, des, round = T))

p <- ggplot(q15, aes(Q15, weight = Freq)) + ptext
p + geom_bar()
```

![](analysis-summary_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
#svytable(~Q + PPGENDER, des, round = T)
gen <- p + aes(PPGENDER, fill = Q15) + geom_bar(position = "dodge")
age <- p + aes(ppagect4, fill = Q15) + geom_bar(position = "dodge")
eth <- p + aes(PPETHM, fill = Q15) + geom_bar(position = "dodge")
inc <- p + aes(income, fill = Q15) + geom_bar(position = "dodge")
edu <- p + aes(PPEDUCAT, fill = Q15) + geom_bar(position = "dodge")
wor <- p + aes(work, fill = Q15) + geom_bar(position = "dodge")
mar <- p + aes(marital, fill = Q15) + geom_bar(position = "dodge")

grid.arrange(gen, age, eth)
```

![](analysis-summary_files/figure-html/unnamed-chunk-13-2.png)<!-- -->

```r
grid.arrange(inc, edu, wor, mar)
```

![](analysis-summary_files/figure-html/unnamed-chunk-13-3.png)<!-- -->

## Q16. Are you more likely to get a vaccine if others around you do not get a vaccine?


```r
q16 <- as.data.frame(svytable(
  ~Q16 + PPGENDER + ppagect4 + PPETHM + income + PPEDUCAT + work + marital, des, round = T))

p <- ggplot(q16, aes(Q16, weight = Freq)) + ptext
p + geom_bar()
```

![](analysis-summary_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
#svytable(~Q + PPGENDER, des, round = T)
gen <- p + aes(PPGENDER, fill = Q16) + geom_bar(position = "dodge")
age <- p + aes(ppagect4, fill = Q16) + geom_bar(position = "dodge")
eth <- p + aes(PPETHM, fill = Q16) + geom_bar(position = "dodge")
inc <- p + aes(income, fill = Q16) + geom_bar(position = "dodge")
edu <- p + aes(PPEDUCAT, fill = Q16) + geom_bar(position = "dodge")
wor <- p + aes(work, fill = Q16) + geom_bar(position = "dodge")
mar <- p + aes(marital, fill = Q16) + geom_bar(position = "dodge")

grid.arrange(gen, age, eth)
```

![](analysis-summary_files/figure-html/unnamed-chunk-14-2.png)<!-- -->

```r
grid.arrange(inc, edu, wor, mar)
```

![](analysis-summary_files/figure-html/unnamed-chunk-14-3.png)<!-- -->

## Q17. Do you get a vaccine to protect yourself, protect others, or protect yourself and others?


```r
q17 <- as.data.frame(svytable(
  ~Q17 + PPGENDER + ppagect4 + PPETHM + income + PPEDUCAT + work + marital, des, round = T))

p <- ggplot(q17, aes(Q17, weight = Freq)) + ptext
p + geom_bar()
```

![](analysis-summary_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
#svytable(~Q + PPGENDER, des, round = T)
gen <- p + aes(PPGENDER, fill = Q17) + geom_bar(position = "dodge")
age <- p + aes(ppagect4, fill = Q17) + geom_bar(position = "dodge")
eth <- p + aes(PPETHM, fill = Q17) + geom_bar(position = "dodge")
inc <- p + aes(income, fill = Q17) + geom_bar(position = "dodge")
edu <- p + aes(PPEDUCAT, fill = Q17) + geom_bar(position = "dodge")
wor <- p + aes(work, fill = Q17) + geom_bar(position = "dodge")
mar <- p + aes(marital, fill = Q17) + geom_bar(position = "dodge")

grid.arrange(gen, age, eth)
```

![](analysis-summary_files/figure-html/unnamed-chunk-15-2.png)<!-- -->

```r
grid.arrange(inc, edu, wor, mar)
```

![](analysis-summary_files/figure-html/unnamed-chunk-15-3.png)<!-- -->

## Q18. What are the reasons you would not get an influenza vaccine?

## Q19. Do you have health insurance?


```r
q19 <- as.data.frame(svytable(
  ~Q19 + PPGENDER + ppagect4 + PPETHM + income + PPEDUCAT + work + marital, des, round = T))

p <- ggplot(q19, aes(Q19, weight = Freq)) + ptext
p + geom_bar()
```

![](analysis-summary_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

```r
#svytable(~Q + PPGENDER, des, round = T)
gen <- p + aes(PPGENDER, fill = Q19) + geom_bar(position = "dodge")
age <- p + aes(ppagect4, fill = Q19) + geom_bar(position = "dodge")
eth <- p + aes(PPETHM, fill = Q19) + geom_bar(position = "dodge")
inc <- p + aes(income, fill = Q19) + geom_bar(position = "dodge")
edu <- p + aes(PPEDUCAT, fill = Q19) + geom_bar(position = "dodge")
wor <- p + aes(work, fill = Q19) + geom_bar(position = "dodge")
mar <- p + aes(marital, fill = Q19) + geom_bar(position = "dodge")

grid.arrange(gen, age, eth)
```

![](analysis-summary_files/figure-html/unnamed-chunk-16-2.png)<!-- -->

```r
grid.arrange(inc, edu, wor, mar)
```

![](analysis-summary_files/figure-html/unnamed-chunk-16-3.png)<!-- -->

```r
## sick plot
q <- svyby(~Q2, ~Q19, des, svymean, na.rm = T)
ggplot(q, aes(Q19, Q2Yes)) + geom_point() + xlab(" ") + ylab("% sick") + er +
  ggtitle(label = "% of adults sick and having health insurance ") 
```

![](analysis-summary_files/figure-html/unnamed-chunk-16-4.png)<!-- -->

## Q20. How effective do you think the influenza vaccine is in protecting people from becoming sick with influenza?


```r
q20 <- as.data.frame(svytable(
  ~Q20 + PPGENDER + ppagect4 + PPETHM + income + PPEDUCAT + work + marital, des, round = T))

p <- ggplot(q20, aes(Q20, weight = Freq)) + ptext
p + geom_bar()
```

![](analysis-summary_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
#svytable(~Q + PPGENDER, des, round = T)
gen <- p + aes(PPGENDER, fill = Q20) + geom_bar(position = "dodge")
age <- p + aes(ppagect4, fill = Q20) + geom_bar(position = "dodge")
eth <- p + aes(PPETHM, fill = Q20) + geom_bar(position = "dodge")
inc <- p + aes(income, fill = Q20) + geom_bar(position = "dodge")
edu <- p + aes(PPEDUCAT, fill = Q20) + geom_bar(position = "dodge")
wor <- p + aes(work, fill = Q20) + geom_bar(position = "dodge")
mar <- p + aes(marital, fill = Q20) + geom_bar(position = "dodge")

grid.arrange(gen, age, eth)
```

![](analysis-summary_files/figure-html/unnamed-chunk-17-2.png)<!-- -->

```r
grid.arrange(inc, edu, wor, mar)
```

![](analysis-summary_files/figure-html/unnamed-chunk-17-3.png)<!-- -->

```r
## sick plot
svychisq(~Q2 + Q20, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + Q20, des)
## F = 4.4, ndf = 4, ddf = 8600, p-value = 0.002
```

```r
q <- svyby(~Q2, ~Q20, des, svymean, na.rm = T)
ggplot(q, aes(Q20, Q2Yes)) + geom_point() + xlab(" ") + ylab("% sick") + ptext + er +
  ggtitle(label = "% of adults sick vs. perception of flu vaccine efficacy") 
```

![](analysis-summary_files/figure-html/unnamed-chunk-17-4.png)<!-- -->

## Q21. Are influenza vaccines covered by your health insurance?


```r
q21 <- as.data.frame(svytable(
  ~Q21 + PPGENDER + ppagect4 + PPETHM + income + PPEDUCAT + work + marital, des, round = T))

p <- ggplot(q21, aes(Q21, weight = Freq)) + ptext
p + geom_bar()
```

![](analysis-summary_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


## Q22. Do you do any of the following when you have influenza symptoms?
## Q23. Which of the following actions do you take when you have influenza symptoms to avoid someone else from getting sick?
## Q24. What sources of information do you recall hearing or seeing about influenza outbreaks?
## Q25. If you received information from the news, internet or other public media that there was an influenza outbreak in your community would you do any of the following?

## Q26. Does your household have children?


```r
q26 <- as.data.frame(svytable(
  ~Q26 + PPGENDER + ppagect4 + PPETHM + income + PPEDUCAT + work + marital, des, round = T))

p <- ggplot(q26, aes(Q26, weight = Freq)) + ptext
p + geom_bar()
```

![](analysis-summary_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
## plot means
svychisq(~Q2 + Q26, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + Q26, des)
## F = 9, ndf = 1, ddf = 2200, p-value = 0.003
```

```r
q <- svyby(~Q2, ~Q26, des, svymean, na.rm = T)
ggplot(q, aes(Q26, Q2Yes)) + geom_point() + xlab(" ") + ylab("% sick") + er +
  ggtitle(label = "% of adults sick and having children in household") 
```

![](analysis-summary_files/figure-html/unnamed-chunk-19-2.png)<!-- -->

## Q27. What actions do you take when a child in your household has influenza symptoms?

## Q28. Are you a single parent?


```r
q28 <- as.data.frame(svytable(
  ~Q28 + PPGENDER + ppagect4 + PPETHM + income + PPEDUCAT + work + marital, des, round = T))

p <- ggplot(q28, aes(Q28, weight = Freq)) + ptext
p + geom_bar()
```

![](analysis-summary_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

```r
## plot means
svychisq(~Q2 + Q28, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + Q28, des)
## F = 2.6, ndf = 1, ddf = 2200, p-value = 0.1
```

```r
q <- svyby(~Q2, ~Q28, des, svymean, na.rm = T)
ggplot(q, aes(Q28, Q2Yes)) + geom_point() + xlab(" ") + ylab("% sick") + er +
  ggtitle(label = "% of adults sick and being single parent") 
```

![](analysis-summary_files/figure-html/unnamed-chunk-20-2.png)<!-- -->

## Q29. How do you care for a sick child? (married parent)
## Q30. How do you care for a sick child? (single parent)

## Q31. How many hours of screen time (time spent watching television, a computer, smartphone, iPad, etc.) do you spend each day on average when you are not sick? Enter 0 if none


```r
# histogram
```
## Q32. How many hours of screen time do you spend each day on average when you are sick? Enter 0 if none


```r
# histogram
```

## Q33. How many people, including yourself, reside in your household?

```r
# histogram
```



### TEMPLATE ###



