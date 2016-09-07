# Analysis summary part 2








```r
## Create survey object.
options(digits = 4)
options(survey.lonely.psu = "adjust")

# subset data?
des <- svydesign(ids = ~1, weights = ~weight, data = df[is.na(df$weight) == 
    F, ])
```




## Q11. How do you rate your risk of getting influenza if you visited each of the following locations?



```r
# subset question data
q11df <- df %>% select(CaseID, PPGENDER, ppagect4, PPETHM, income, PPEDUCAT, 
    work, marital, ppreg9, PPMSACAT, Q2, Q11_1:Q11_11, weight) %>% gather(Q11_q, 
    Q11_r, Q11_1:Q11_11, na.rm = T) %>% group_by(Q11_q, Q11_r)

# new survey design
des11 <- svydesign(ids = ~1, weights = ~weight, data = q11df[is.na(q11df$weight) == 
    F, ])

# weighted data frame
q11 <- data.frame(svytable(~Q11_q + Q11_r + PPGENDER + ppagect4 + PPETHM + income + 
    PPEDUCAT + work + ppreg9 + PPMSACAT, des11, round = T))


# plot
title <- ggtitle("Q11. How do you rate your risk of getting influenza if you visited each of the following locations?")
p <- ggplot(q11, aes(Q11_r, weight = Freq)) + ptext
fil <- aes(fill = Q11_r)

p + fil + geom_bar() + facet_wrap(~Q11_q) + title
```

![](summary-pt2_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
# gender
p + aes(fill = PPGENDER) + geom_bar(position = "dodge") + facet_wrap(~Q11_q)
```

![](summary-pt2_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
# age
p + page + fil + geom_bar(position = "dodge") + facet_wrap(~Q11_q) + title
```

![](summary-pt2_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
p + fil + geom_bar(position = "stack") + facet_wrap(~ppagect4 + ~Q11_q)
```

![](summary-pt2_files/figure-html/unnamed-chunk-1-4.png)<!-- -->

```r
# race
p + peth + fil + geom_bar(position = "stack")
```

![](summary-pt2_files/figure-html/unnamed-chunk-1-5.png)<!-- -->

```r
p + aes(fill = PPETHM) + geom_bar(position = "fill")
```

![](summary-pt2_files/figure-html/unnamed-chunk-1-6.png)<!-- -->

```r
# income
p + pinc + fil + geom_bar(position = "stack") + title
```

![](summary-pt2_files/figure-html/unnamed-chunk-1-7.png)<!-- -->

```r
p + aes(fill = income) + geom_bar(position = "dodge")
```

![](summary-pt2_files/figure-html/unnamed-chunk-1-8.png)<!-- -->

```r
# metro location
p + aes(PPMSACAT) + fil + geom_bar(position = "fill")
```

![](summary-pt2_files/figure-html/unnamed-chunk-1-9.png)<!-- -->

```r
p + aes(fill = PPMSACAT) + geom_bar(position = "stack")
```

![](summary-pt2_files/figure-html/unnamed-chunk-1-10.png)<!-- -->

```r
# region
p + fil + geom_bar(position = "dodge") + facet_wrap(~ppreg9) + title
```

![](summary-pt2_files/figure-html/unnamed-chunk-1-11.png)<!-- -->


## Q12. Which of the following actions do you take to avoid getting sick?



```r
title <- ggtitle("Q12. Which of the following actions do you take to avoid getting sick?")
```


## Q13. Do you get the flu vaccine?



```r
q13 <- as.data.frame(svytable(~Q13 + PPGENDER + ppagect4 + PPETHM + income + 
    PPEDUCAT + work + marital, des, round = T))

title <- ggtitle("Q13. Do you get the flu vaccine?")
p <- ggplot(q13, aes(Q13, weight = Freq)) + ptext
fil <- aes(fill = Q13)

p + geom_bar() + title
```

![](summary-pt2_files/figure-html/q13-plot-1.png)<!-- -->

```r
gen <- p + pgen + fil + geom_bar(position = "dodge") + title
age <- p + page + fil + geom_bar(position = "dodge")
eth <- p + peth + fil + geom_bar(position = "dodge") + coord_flip()
inc <- p + pinc + fil + geom_bar(position = "dodge") + title
edu <- p + pedu + fil + geom_bar(position = "dodge") + title
wor <- p + pwor + fil + geom_bar(position = "dodge")
mar <- p + pmar + fil + geom_bar(position = "dodge")

grid.arrange(gen, age, eth)
```

![](summary-pt2_files/figure-html/q13-plot-2.png)<!-- -->

```r
grid.arrange(inc, edu, wor, mar)
```

![](summary-pt2_files/figure-html/q13-plot-3.png)<!-- -->

```r
## sick plot
svychisq(~Q2 + Q13, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + Q13, des)
## F = 5.9196, ndf = 1.9983, ddf = 4330.4000, p-value = 0.002716
```

```r
q <- svyby(~Q2, ~Q13, des, svymean, na.rm = T)
ggplot(q, aes(Q13, Q2Yes)) + geom_point() + xlab(" ") + ylab("% sick") + er + 
    ggtitle(label = "% of adults sick and getting flu vaccine")
```

![](summary-pt2_files/figure-html/q13-plot-4.png)<!-- -->

```r
# chisquare for getting flu vaccine and sick household member
svychisq(~Q15 + Q3, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q15 + Q3, des)
## F = 3.1542, ndf = 3.9462, ddf = 8551.5000, p-value = 0.01377
```


## Q14. How much do you pay to get an influenza vaccine?



```r
q14 <- as.data.frame(svytable(~Q14 + PPGENDER + ppagect4 + PPETHM + income + 
    PPEDUCAT + work + marital, des, round = T))

title <- ggtitle("Q14. How much do you pay to get an influenza vaccine?")
p <- ggplot(q14, aes(Q14, weight = Freq)) + ptext
fil <- aes(fill = Q14)

p + geom_bar() + title
```

![](summary-pt2_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
gen <- p + pgen + fil + geom_bar(position = "dodge")
age <- p + page + fil + geom_bar(position = "dodge")
eth <- p + peth + fil + geom_bar(position = "dodge") + coord_flip()
inc <- p + pinc + fil + geom_bar(position = "dodge")
edu <- p + pedu + fil + geom_bar(position = "dodge")
wor <- p + pwor + fil + geom_bar(position = "dodge")
mar <- p + pmar + fil + geom_bar(position = "dodge")

grid.arrange(gen, age, eth)
```

![](summary-pt2_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
grid.arrange(inc, edu, wor, mar)
```

![](summary-pt2_files/figure-html/unnamed-chunk-3-3.png)<!-- -->


## Q15. Are you more likely to get a vaccine if others around you get a vaccine?



```r
# chisquare
svychisq(~Q15 + Q2, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q15 + Q2, des)
## F = 2.6367, ndf = 1.9761, ddf = 4282.2000, p-value = 0.0724
```

```r
q15 <- as.data.frame(svytable(~Q15 + PPGENDER + ppagect4 + PPETHM + income + 
    PPEDUCAT + work + marital, des, round = T))

title <- ggtitle("Q15. Are you more likely to get a vaccine if others around you get a vaccine?")
p <- ggplot(q15, aes(Q15, weight = Freq)) + ptext
fil <- aes(fill = Q15)

p + geom_bar() + title
```

![](summary-pt2_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
gen <- p + pgen + fil + geom_bar(position = "dodge") + title
age <- p + page + fil + geom_bar(position = "dodge")
eth <- p + peth + fil + geom_bar(position = "dodge") + coord_flip()
inc <- p + pinc + fil + geom_bar(position = "dodge") + title
edu <- p + pedu + fil + geom_bar(position = "dodge")
wor <- p + pwor + fil + geom_bar(position = "dodge")
mar <- p + pmar + fil + geom_bar(position = "dodge")

grid.arrange(gen, age, eth)
```

![](summary-pt2_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```r
grid.arrange(inc, edu, wor, mar)
```

![](summary-pt2_files/figure-html/unnamed-chunk-4-3.png)<!-- -->


## Q16. Are you more likely to get a vaccine if others around you do not get a vaccine?



```r
# chisquare
svychisq(~Q16 + Q2, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q16 + Q2, des)
## F = 4.8451, ndf = 1.9971, ddf = 4327.8000, p-value = 0.007938
```

```r
q16 <- as.data.frame(svytable(~Q16 + PPGENDER + ppagect4 + PPETHM + income + 
    PPEDUCAT + work + marital, des, round = T))

title <- ggtitle("Q16. Are you more likely to get a vaccine if others around you do not get a vaccine?")
p <- ggplot(q16, aes(Q16, weight = Freq)) + ptext
fil <- aes(fill = Q16)

p + geom_bar() + title
```

![](summary-pt2_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
gen <- p + pgen + fil + geom_bar(position = "dodge") + title
age <- p + page + fil + geom_bar(position = "dodge")
eth <- p + peth + fil + geom_bar(position = "dodge") + coord_flip()
inc <- p + pinc + fil + geom_bar(position = "dodge") + title
edu <- p + pedu + fil + geom_bar(position = "dodge") + title
wor <- p + pwor + fil + geom_bar(position = "dodge")
mar <- p + pmar + fil + geom_bar(position = "dodge")

grid.arrange(gen, age, eth)
```

![](summary-pt2_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```r
grid.arrange(inc, edu, wor, mar)
```

![](summary-pt2_files/figure-html/unnamed-chunk-5-3.png)<!-- -->


## Q17. Do you get a vaccine to protect yourself, protect others, or protect yourself and others?



```r
q17 <- as.data.frame(svytable(~Q17 + PPGENDER + ppagect4 + PPETHM + income + 
    PPEDUCAT + work + marital, des, round = T))

title <- ggtitle("Q17. Do you get a vaccine to protect yourself, protect others, or protect yourself and others?")
p <- ggplot(q17, aes(Q17, weight = Freq)) + ptext
fil <- aes(fill = Q17)

p + geom_bar() + title
```

![](summary-pt2_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
gen <- p + pgen + fil + geom_bar(position = "dodge") + title
age <- p + page + fil + geom_bar(position = "dodge")
eth <- p + peth + fil + geom_bar(position = "dodge") + coord_flip()
inc <- p + pinc + fil + geom_bar(position = "dodge")
edu <- p + pedu + fil + geom_bar(position = "dodge") + title
wor <- p + pwor + fil + geom_bar(position = "dodge")
mar <- p + pmar + fil + geom_bar(position = "dodge")

grid.arrange(gen, age, eth)
```

![](summary-pt2_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```r
grid.arrange(inc, edu, wor, mar)
```

![](summary-pt2_files/figure-html/unnamed-chunk-6-3.png)<!-- -->


## Q18. What are the reasons you would not get an influenza vaccine?



```r
title <- ggtitle("Q18. What are the reasons you would not get an influenza vaccine?")
```


## Q19. Do you have health insurance?



```r
# chisquare
svychisq(~Q19 + Q2, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q19 + Q2, des)
## F = 0.16425, ndf = 1, ddf = 2167, p-value = 0.6853
```

```r
q19 <- as.data.frame(svytable(~Q19 + PPGENDER + ppagect4 + PPETHM + income + 
    PPEDUCAT + work + marital, des, round = T))

title <- ggtitle("Q19. Do you have health insurance?")
p <- ggplot(q19, aes(Q19, weight = Freq)) + ptext
fil <- aes(fill = Q19)

p + geom_bar() + title
```

![](summary-pt2_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
gen <- p + pgen + fil + geom_bar(position = "dodge") + title
age <- p + page + fil + geom_bar(position = "dodge")
eth <- p + peth + fil + geom_bar(position = "dodge") + coord_flip()
inc <- p + pinc + fil + geom_bar(position = "dodge")
edu <- p + pedu + fil + geom_bar(position = "dodge") + title
wor <- p + pwor + fil + geom_bar(position = "dodge")
mar <- p + pmar + fil + geom_bar(position = "dodge")

grid.arrange(gen, age, eth)
```

![](summary-pt2_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

```r
grid.arrange(inc, edu, wor, mar)
```

![](summary-pt2_files/figure-html/unnamed-chunk-8-3.png)<!-- -->

```r
## sick plot
q <- svyby(~Q2, ~Q19, des, svymean, na.rm = T)
ggplot(q, aes(Q19, Q2Yes)) + geom_point() + xlab(" ") + ylab("% sick") + er + 
    ggtitle(label = "% of adults sick and having health insurance ")
```

![](summary-pt2_files/figure-html/unnamed-chunk-8-4.png)<!-- -->


## Q20. How effective do you think the influenza vaccine is in protecting people from becoming sick with influenza?



```r
q20 <- as.data.frame(svytable(~Q20 + PPGENDER + ppagect4 + PPETHM + income + 
    PPEDUCAT + work + marital, des, round = T))

title <- ggtitle("Q20. How effective do you think the influenza vaccine is in protecting people from becoming sick with influenza?")
p <- ggplot(q20, aes(Q20, weight = Freq)) + ptext
fil <- aes(fill = Q20)

p + geom_bar() + title
```

![](summary-pt2_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
gen <- p + pgen + fil + geom_bar(position = "dodge") + title
age <- p + page + fil + geom_bar(position = "dodge")
eth <- p + peth + fil + geom_bar(position = "dodge") + coord_flip()
inc <- p + pinc + fil + geom_bar(position = "dodge")
edu <- p + pedu + fil + geom_bar(position = "dodge")
wor <- p + pwor + fil + geom_bar(position = "dodge")
mar <- p + pmar + fil + geom_bar(position = "dodge")

grid.arrange(gen, age, eth)
```

![](summary-pt2_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

```r
grid.arrange(inc, edu, wor, mar)
```

![](summary-pt2_files/figure-html/unnamed-chunk-9-3.png)<!-- -->

```r
## sick plot
svychisq(~Q2 + Q20, des)
```

```
## 
## 	Pearson's X^2: Rao & Scott adjustment
## 
## data:  svychisq(~Q2 + Q20, des)
## F = 4.3873, ndf = 3.9876, ddf = 8641.0000, p-value = 0.001542
```

```r
q <- svyby(~Q2, ~Q20, des, svymean, na.rm = T)
ggplot(q, aes(Q20, Q2Yes)) + geom_point() + xlab(" ") + ylab("% sick") + ptext + 
    er + ggtitle(label = "% of adults sick vs. perception of flu vaccine efficacy")
```

![](summary-pt2_files/figure-html/unnamed-chunk-9-4.png)<!-- -->

