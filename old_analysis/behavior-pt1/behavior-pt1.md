# Behavior part 1: Transportation

Questions 7, 8, 9, 10.
Transportation behavior.








```r
## Create survey object.
options(digits = 4)
options(survey.lonely.psu = "adjust")

des <- svydesign(ids = ~1, weights = ~weight, data = df[is.na(df$weight)==F, ])
```





## Q7. What types of public transportation do you regularly use?


```r
# subset question data, rename columns, gather into single column
q7_df <- df %>%
  select(CaseID, PPGENDER, PPAGE, ppagecat, PPETHM, PPINCIMP, PPEDUC, PPEDUCAT,
         work, PPWORK, marital, PPMARIT, PPMSACAT, ppreg9, PPSTATEN, PPHOUSE, PPRENT, PPNET,
         Q7_1:Q7_7, Q7_otherText, weight) %>%
  rename(Bus = Q7_1, Carpool = Q7_2, Subway = Q7_3, Train = Q7_4,
         Taxi = Q7_5, Airplane = Q7_6, Other = Q7_7) %>%
  gather(Q7_q, Q7_r, Bus:Other, na.rm = T) %>%
  mutate(Q7_q = as.factor(Q7_q))

# select only Yes responses
q7_df <- q7_df[(q7_df$Q7_r)=='Yes', ]

# survey design
options(digits = 4)
options(survey.lonely.psu = "adjust")
des7 <- svydesign(ids = ~1, weights = ~weight, data = q7_df[is.na(q7_df$weight)==F, ])
```

### Gender, age, ethnicity, income, education, work


```r
# weighted data frame
q7 <- data.frame(svytable(~Q7_q + Q7_r + PPGENDER + ppagecat + PPETHM + PPINCIMP +
                            PPEDUC + PPEDUCAT + work + PPWORK, des7, round = T))
# plot templates
title <- ggtitle("What types of public transportation do you regularly use?")

## main plot
p <- ggplot(q7, aes(Q7_q, weight = Freq)) + ptext
p + geom_bar() + title
```

![](behavior-pt1_files/figure-html/q7-plot-1-1.png)<!-- -->


```r
## plot2: exclude 'Other' column
p2 <- ggplot(q7[!(q7$Q7_q)=='Other', ], aes(Q7_q, weight = Freq)) + ptext

# by gender
p2 + geom_bar() + aes(PPGENDER, fill = PPGENDER) + facet_wrap(~Q7_q) + ggtitle("By gender")
```

![](behavior-pt1_files/figure-html/q7-plot-1b-1.png)<!-- -->

```r
# age boxplot
svyboxplot(PPAGE~Q7_q, des7, main = "Age boxplot per response")
```

![](behavior-pt1_files/figure-html/q7-plot-1b-2.png)<!-- -->

```r
# by age group
p2 + geom_bar() + aes(ppagecat, fill = ppagecat) + facet_wrap(~Q7_q) + ggtitle("By age group")
```

![](behavior-pt1_files/figure-html/q7-plot-1b-3.png)<!-- -->

```r
p2 + geom_bar(position = "fill") + aes(ppagecat, fill = Q7_q)
```

![](behavior-pt1_files/figure-html/q7-plot-1b-4.png)<!-- -->

```r
# by ethnic group
p + geom_bar() + aes(PPETHM, fill = PPETHM) + facet_wrap(~Q7_q) + ggtitle("By ethnic group") +
  ptext2
```

![](behavior-pt1_files/figure-html/q7-plot-1b-5.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(PPETHM, fill = Q7_q)
```

![](behavior-pt1_files/figure-html/q7-plot-1b-6.png)<!-- -->

```r
p + geom_bar() + aes(fill = Q7_q) + facet_wrap(~PPETHM) + ggtitle("By ethnic group")
```

![](behavior-pt1_files/figure-html/q7-plot-1b-7.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(fill = PPETHM)
```

![](behavior-pt1_files/figure-html/q7-plot-1b-8.png)<!-- -->

```r
# by income
p + geom_bar() + aes(PPINCIMP, fill = PPINCIMP) + facet_wrap(~Q7_q) + ggtitle("By income") + 
  ptext2
```

![](behavior-pt1_files/figure-html/q7-plot-1b-9.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(PPINCIMP, fill = Q7_q)
```

![](behavior-pt1_files/figure-html/q7-plot-1b-10.png)<!-- -->

```r
# by education
p + geom_bar() + aes(PPEDUCAT, fill = PPEDUCAT) + facet_wrap(~Q7_q) + ggtitle("By education")
```

![](behavior-pt1_files/figure-html/q7-plot-1b-11.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(PPEDUCAT, fill = Q7_q)
```

![](behavior-pt1_files/figure-html/q7-plot-1b-12.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(PPEDUC, fill = Q7_q)
```

![](behavior-pt1_files/figure-html/q7-plot-1b-13.png)<!-- -->

```r
p + geom_bar() + aes(fill = Q7_q) + facet_wrap(~PPEDUCAT) + ggtitle("By education")
```

![](behavior-pt1_files/figure-html/q7-plot-1b-14.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(fill = PPEDUCAT)
```

![](behavior-pt1_files/figure-html/q7-plot-1b-15.png)<!-- -->

```r
# by work status
p + geom_bar() + aes(work, fill = work) + facet_wrap(~Q7_q) + ggtitle("By employment status")
```

![](behavior-pt1_files/figure-html/q7-plot-1b-16.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(PPWORK, fill = Q7_q) + ggtitle("By employment status")
```

![](behavior-pt1_files/figure-html/q7-plot-1b-17.png)<!-- -->

### Marital status, metro status, region, state of residency, house type, housing status, internet availability


```r
# update weighted data frame
q7.2 <- data.frame(svytable(~Q7_q + Q7_r + marital + PPMARIT + PPMSACAT + ppreg9 +
                              PPSTATEN + PPHOUSE + PPRENT + PPNET, des7, round = T))

# restate plots
p3 <- ggplot(q7.2, aes(Q7_q, weight = Freq)) + ptext
p4 <- ggplot(q7.2[!(q7.2$Q7_q)=='Other', ], aes(Q7_q, weight = Freq)) + ptext
```


```r
# by marital status
p3 + geom_bar() + aes(marital, fill = marital) + facet_wrap(~Q7_q) + ggtitle("By marital status")
```

![](behavior-pt1_files/figure-html/q7-plot-2b-1.png)<!-- -->

```r
p3 + geom_bar(position = "fill") + aes(marital, fill = Q7_q)
```

![](behavior-pt1_files/figure-html/q7-plot-2b-2.png)<!-- -->

```r
p3 + geom_bar() + aes(PPMARIT, fill = PPMARIT) + facet_wrap(~Q7_q)
```

![](behavior-pt1_files/figure-html/q7-plot-2b-3.png)<!-- -->

```r
p3 + geom_bar() + aes(PPMARIT, fill = Q7_q) + ggtitle("By marital status")
```

![](behavior-pt1_files/figure-html/q7-plot-2b-4.png)<!-- -->

```r
# by metro status
p3 + geom_bar() + aes(PPMSACAT, fill = PPMSACAT) + facet_wrap(~Q7_q) + ggtitle("By metro status")
```

![](behavior-pt1_files/figure-html/q7-plot-2b-5.png)<!-- -->

```r
p3 + geom_bar(position = "stack") + aes(fill = PPMSACAT)
```

![](behavior-pt1_files/figure-html/q7-plot-2b-6.png)<!-- -->

```r
p3 + geom_bar() + aes(fill = PPMSACAT) + facet_wrap(~PPMSACAT)
```

![](behavior-pt1_files/figure-html/q7-plot-2b-7.png)<!-- -->

```r
# by region
p3 + geom_bar(position = 'fill') + aes(fill = ppreg9) + ggtitle("Responses by US region")
```

![](behavior-pt1_files/figure-html/q7-plot-2b-8.png)<!-- -->

```r
p3 + geom_bar(position = 'fill') + aes(ppreg9, fill = Q7_q) + ggtitle("US regions by response")
```

![](behavior-pt1_files/figure-html/q7-plot-2b-9.png)<!-- -->

```r
# by state
p3 + geom_bar() + aes(PPSTATEN, fill = Q7_q) + coord_flip() + ggtitle("By state")
```

![](behavior-pt1_files/figure-html/q7-plot-2b-10.png)<!-- -->

```r
p3 + geom_bar(position = 'fill') + aes(PPSTATEN, fill = Q7_q) + coord_flip() + ggtitle("By state")
```

![](behavior-pt1_files/figure-html/q7-plot-2b-11.png)<!-- -->

```r
# by house type
p3 + geom_bar() + aes(fill = PPHOUSE) + ggtitle("By house type")
```

![](behavior-pt1_files/figure-html/q7-plot-2b-12.png)<!-- -->

```r
p3 + geom_bar(position = 'fill') + aes(fill = PPHOUSE)
```

![](behavior-pt1_files/figure-html/q7-plot-2b-13.png)<!-- -->

```r
p3 + geom_bar() + aes(PPHOUSE, fill = PPHOUSE) + facet_wrap(~Q7_q) + ptext2
```

![](behavior-pt1_files/figure-html/q7-plot-2b-14.png)<!-- -->

```r
# by housing status

# by internet availability
p3 + geom_bar(position = "fill") + aes(fill = PPNET) + ggtitle("Internet status")
```

![](behavior-pt1_files/figure-html/q7-plot-2b-15.png)<!-- -->

```r
p3 + geom_bar(position = 'fill') + aes(PPNET, fill = Q7_q) + ggtitle("Internet status")
```

![](behavior-pt1_files/figure-html/q7-plot-2b-16.png)<!-- -->



## Q8. For what types of activities do you regularly use public transportation?


```r
# subset question data, rename columns, gather into single column
q8_df <- df %>%
  select(CaseID, PPGENDER, PPAGE, ppagecat, PPETHM, PPINCIMP, PPEDUC, PPEDUCAT,
         work, PPWORK, marital, PPMARIT, PPMSACAT, ppreg9, PPSTATEN, PPHOUSE, PPRENT, PPNET,
         Q8_1:Q8_6, weight) %>%
  rename(Work = Q8_1, School = Q8_2, Shopping = Q8_3, Visiting_people = Q8_4,
         Recreation = Q8_5, Other = Q8_6) %>%
  gather(Q8_q, Q8_r, Work:Other, na.rm = T) %>%
  mutate(Q8_q = as.factor(Q8_q))

# select only Yes responses
q8_df <- q8_df[q8_df$Q8_r == 'Yes', ]

# survey design
options(digits = 4)
options(survey.lonely.psu = "adjust")
des8 <- svydesign(ids = ~1, weights = ~weight, data = q8_df[is.na(q8_df$weight)==F, ])
```

### Gender, age, ethnicity, income, education


```r
# weighted data frame
q8 <- data.frame(svytable(~Q8_q + Q8_r + PPGENDER + ppagecat + PPETHM + PPINCIMP + 
                            PPEDUC + PPEDUCAT + work + PPWORK, des8, round = T))
# plot templates
title <- ggtitle("For what types of activities do you regularly use public transportation?")

## main plot
p <- ggplot(q8, aes(Q8_q, weight = Freq)) + ptext
p + geom_bar() + title
```

![](behavior-pt1_files/figure-html/q8-plot-1-1.png)<!-- -->


```r
## plot2: exclude 'Other' column
p2 <- ggplot(q8[!(q8$Q8_q)=='Other', ], aes(Q8_q, weight = Freq)) + ptext

# by gender
p2 + geom_bar() + aes(PPGENDER, fill = PPGENDER) + facet_wrap(~Q8_q) + ggtitle("By gender")
```

![](behavior-pt1_files/figure-html/q8-plot-1b-1.png)<!-- -->

```r
# age boxplot
svyboxplot(PPAGE~Q8_q, des8, main = "Age boxplot per response")
```

![](behavior-pt1_files/figure-html/q8-plot-1b-2.png)<!-- -->

```r
# by age group
p2 + geom_bar() + aes(ppagecat, fill = ppagecat) + facet_wrap(~Q8_q) + ggtitle("By age group")
```

![](behavior-pt1_files/figure-html/q8-plot-1b-3.png)<!-- -->

```r
p2 + geom_bar(position = "fill") + aes(ppagecat, fill = Q8_q)
```

![](behavior-pt1_files/figure-html/q8-plot-1b-4.png)<!-- -->

```r
# by ethnic group
p + geom_bar() + aes(PPETHM, fill = PPETHM) + facet_wrap(~Q8_q) + ggtitle("By ethnic group") +
  ptext2
```

![](behavior-pt1_files/figure-html/q8-plot-1b-5.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(PPETHM, fill = Q8_q)
```

![](behavior-pt1_files/figure-html/q8-plot-1b-6.png)<!-- -->

```r
p + geom_bar() + aes(fill = Q8_q) + facet_wrap(~PPETHM) + ggtitle("By ethnic group")
```

![](behavior-pt1_files/figure-html/q8-plot-1b-7.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(fill = PPETHM)
```

![](behavior-pt1_files/figure-html/q8-plot-1b-8.png)<!-- -->

```r
# by income
p + geom_bar() + aes(PPINCIMP, fill = PPINCIMP) + facet_wrap(~Q8_q) + ggtitle("By income") + 
  ptext2
```

![](behavior-pt1_files/figure-html/q8-plot-1b-9.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(PPINCIMP, fill = Q8_q)
```

![](behavior-pt1_files/figure-html/q8-plot-1b-10.png)<!-- -->

```r
# by education
p + geom_bar() + aes(PPEDUCAT, fill = PPEDUCAT) + facet_wrap(~Q8_q) + ggtitle("By education")
```

![](behavior-pt1_files/figure-html/q8-plot-1b-11.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(PPEDUCAT, fill = Q8_q)
```

![](behavior-pt1_files/figure-html/q8-plot-1b-12.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(PPEDUC, fill = Q8_q)
```

![](behavior-pt1_files/figure-html/q8-plot-1b-13.png)<!-- -->

```r
p + geom_bar() + aes(fill = Q8_q) + facet_wrap(~PPEDUCAT) + ggtitle("By education")
```

![](behavior-pt1_files/figure-html/q8-plot-1b-14.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(fill = PPEDUCAT)
```

![](behavior-pt1_files/figure-html/q8-plot-1b-15.png)<!-- -->

```r
# by work status
p + geom_bar() + aes(work, fill = work) + facet_wrap(~Q8_q) + ggtitle("By employment status")
```

![](behavior-pt1_files/figure-html/q8-plot-1b-16.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(PPWORK, fill = Q8_q) + ggtitle("By employment status")
```

![](behavior-pt1_files/figure-html/q8-plot-1b-17.png)<!-- -->

### Marital status, metro status, region, state of residency, house type, housing status, internet availability


```r
# weighted data frame
q8.2 <- data.frame(svytable(~Q8_q + Q8_r + work + marital + PPMARIT + PPMSACAT + ppreg9 +
                              PPSTATEN + PPHOUSE + PPRENT + PPNET, des8, round = T))
# restate plots
p3 <- ggplot(q8.2, aes(Q8_q, weight = Freq)) + ptext
p4 <- ggplot(q8.2[!(q8.2$Q8_q)=='Other', ], aes(Q8_q, weight = Freq)) + ptext
```


```r
# by marital status
p3 + geom_bar() + aes(marital, fill = marital) + facet_wrap(~Q8_q) + ggtitle("By marital status")
```

![](behavior-pt1_files/figure-html/q8-plot-2b-1.png)<!-- -->

```r
p3 + geom_bar(position = "fill") + aes(marital, fill = Q8_q)
```

![](behavior-pt1_files/figure-html/q8-plot-2b-2.png)<!-- -->

```r
p3 + geom_bar() + aes(PPMARIT, fill = PPMARIT) + facet_wrap(~Q8_q)
```

![](behavior-pt1_files/figure-html/q8-plot-2b-3.png)<!-- -->

```r
p3 + geom_bar() + aes(PPMARIT, fill = Q8_q) + ggtitle("By marital status")
```

![](behavior-pt1_files/figure-html/q8-plot-2b-4.png)<!-- -->

```r
# by metro status
p3 + geom_bar() + aes(PPMSACAT, fill = PPMSACAT) + facet_wrap(~Q8_q) + ggtitle("By metro status")
```

![](behavior-pt1_files/figure-html/q8-plot-2b-5.png)<!-- -->

```r
p3 + geom_bar(position = "stack") + aes(fill = PPMSACAT)
```

![](behavior-pt1_files/figure-html/q8-plot-2b-6.png)<!-- -->

```r
p3 + geom_bar() + aes(fill = PPMSACAT) + facet_wrap(~PPMSACAT)
```

![](behavior-pt1_files/figure-html/q8-plot-2b-7.png)<!-- -->

```r
# by region
p3 + geom_bar(position = 'fill') + aes(fill = ppreg9) + ggtitle("Responses by US region")
```

![](behavior-pt1_files/figure-html/q8-plot-2b-8.png)<!-- -->

```r
p3 + geom_bar(position = 'fill') + aes(ppreg9, fill = Q8_q) + ggtitle("US regions by response")
```

![](behavior-pt1_files/figure-html/q8-plot-2b-9.png)<!-- -->

```r
# by state
p3 + geom_bar() + aes(PPSTATEN, fill = Q8_q) + coord_flip() + ggtitle("By state")
```

![](behavior-pt1_files/figure-html/q8-plot-2b-10.png)<!-- -->

```r
p3 + geom_bar(position = 'fill') + aes(PPSTATEN, fill = Q8_q) + coord_flip() + ggtitle("By state")
```

![](behavior-pt1_files/figure-html/q8-plot-2b-11.png)<!-- -->

```r
# by house type
p3 + geom_bar() + aes(fill = PPHOUSE) + ggtitle("By house type")
```

![](behavior-pt1_files/figure-html/q8-plot-2b-12.png)<!-- -->

```r
p3 + geom_bar(position = 'fill') + aes(fill = PPHOUSE)
```

![](behavior-pt1_files/figure-html/q8-plot-2b-13.png)<!-- -->

```r
p3 + geom_bar() + aes(PPHOUSE, fill = PPHOUSE) + facet_wrap(~Q8_q) + ptext2
```

![](behavior-pt1_files/figure-html/q8-plot-2b-14.png)<!-- -->

```r
# by housing status

# by internet availability
p3 + geom_bar(position = "fill") + aes(fill = PPNET) + ggtitle("Internet status")
```

![](behavior-pt1_files/figure-html/q8-plot-2b-15.png)<!-- -->

```r
p3 + geom_bar(position = 'fill') + aes(PPNET, fill = Q8_q) + ggtitle("Internet status")
```

![](behavior-pt1_files/figure-html/q8-plot-2b-16.png)<!-- -->



## Q9. Do other members of your household regularly use public transportation?

### Ethnicity, income, metro status, region, state, house type, housing status


```r
# weighted data frame
q9 <- data.frame(svytable(~Q9 + PPETHM + PPINCIMP + PPMSACAT + ppreg9 + 
                            PPSTATEN + PPHOUSE + PPRENT, des, round = T))
# plot templates
title <- ggtitle("Do other members of your household regularly use public transportation?")

## main plot
p <- ggplot(q9, aes(Q9, weight = Freq)) + ptext
p + geom_bar() + title
```

![](behavior-pt1_files/figure-html/q9-plot-1-1.png)<!-- -->


```r
## plot2: exclude 'Don_t know' column
p2 <- ggplot(q9[!(q9$Q9)=='Don_t know', ], aes(Q9, weight = Freq)) + ptext

# by ethnic group
p2 + geom_bar() + aes(fill = PPETHM) + ggtitle("By ethnic group") + ptext2
```

![](behavior-pt1_files/figure-html/q9-plot-1b-1.png)<!-- -->

```r
p2 + geom_bar(position = "fill") + aes(PPETHM, fill = Q9)
```

![](behavior-pt1_files/figure-html/q9-plot-1b-2.png)<!-- -->

```r
p2 + geom_bar() + aes(fill = Q9) + facet_wrap(~PPETHM) + ggtitle("By ethnic group")
```

![](behavior-pt1_files/figure-html/q9-plot-1b-3.png)<!-- -->

```r
p2 + geom_bar(position = "fill") + aes(fill = PPETHM)
```

![](behavior-pt1_files/figure-html/q9-plot-1b-4.png)<!-- -->

```r
# by income
p2 + geom_bar() + aes(fill = PPINCIMP) + ggtitle("By income") + ptext2
```

![](behavior-pt1_files/figure-html/q9-plot-1b-5.png)<!-- -->

```r
p2 + geom_bar(position = "fill") + aes(PPINCIMP, fill = Q9)
```

![](behavior-pt1_files/figure-html/q9-plot-1b-6.png)<!-- -->

```r
# by metro status
p2 + geom_bar() + aes(fill = PPMSACAT) + ggtitle("By metro status")
```

![](behavior-pt1_files/figure-html/q9-plot-1b-7.png)<!-- -->

```r
p2 + geom_bar(position = "stack") + aes(PPMSACAT, fill = Q9)
```

![](behavior-pt1_files/figure-html/q9-plot-1b-8.png)<!-- -->

```r
# by region
p2 + geom_bar(position = 'fill') + aes(fill = ppreg9) + ggtitle("Responses by US region")
```

![](behavior-pt1_files/figure-html/q9-plot-1b-9.png)<!-- -->

```r
p2 + geom_bar(position = 'fill') + aes(ppreg9, fill = Q9) + ggtitle("US regions by response")
```

![](behavior-pt1_files/figure-html/q9-plot-1b-10.png)<!-- -->

```r
# by state
p2 + geom_bar() + aes(PPSTATEN, fill = Q9) + coord_flip() + ggtitle("By state")
```

![](behavior-pt1_files/figure-html/q9-plot-1b-11.png)<!-- -->

```r
p2 + geom_bar(position = 'fill') + aes(PPSTATEN, fill = Q9) + coord_flip() + ggtitle("By state")
```

![](behavior-pt1_files/figure-html/q9-plot-1b-12.png)<!-- -->

```r
# by house type
p2 + geom_bar() + aes(fill = PPHOUSE) + ggtitle("By housing")
```

![](behavior-pt1_files/figure-html/q9-plot-1b-13.png)<!-- -->

```r
p2 + geom_bar(position = 'fill') + aes(fill = PPHOUSE)
```

![](behavior-pt1_files/figure-html/q9-plot-1b-14.png)<!-- -->

```r
p2 + geom_bar() + aes(PPHOUSE, fill = Q9) + ptext2
```

![](behavior-pt1_files/figure-html/q9-plot-1b-15.png)<!-- -->

```r
# by housing status
```



## Q10. What types of public transportation do other members of your household regularly use?


```r
# subset question data, rename columns, gather into single column
q10_df <- df %>%
  select(CaseID, PPGENDER, PPAGE, ppagecat, PPETHM, PPINCIMP, PPEDUC, PPEDUCAT,
         work, PPWORK, marital, PPMARIT, PPMSACAT, ppreg9, PPSTATEN, PPHOUSE, PPRENT, PPNET,
       Q10_1:Q10_8, weight) %>%
  rename(Bus = Q10_1, Carpool = Q10_2, Subway = Q10_3, Train = Q10_4,
         Taxi = Q10_5, Airplane = Q10_6, Don_t_know = Q10_7, Other = Q10_8) %>%
  gather(Q10_q, Q10_r, Bus:Other, na.rm = T) %>%
  mutate(Q10_q = as.factor(Q10_q))

# select only Yes responses
q10_df <- q10_df[q10_df$Q10_r == 'Yes', ]

# survey design
options(digits = 4)
options(survey.lonely.psu = "adjust")
des10 <- svydesign(ids = ~1, weights = ~weight, data = q10_df[is.na(q10_df$weight)==F, ])
```

### Gender, age, ethnicity, income, education, work


```r
# weighted data frame
q10 <- data.frame(svytable(~Q10_q + Q10_r + PPGENDER + ppagecat + PPETHM + PPINCIMP +
                            PPEDUC + PPEDUCAT + work + PPWORK, des10, round = T))
# plot templates
title <- ggtitle("What types of public transportation do other members of your household regularly use?")

## main plot
p <- ggplot(q10, aes(Q10_q, weight = Freq)) + ptext
p + geom_bar() + title
```

![](behavior-pt1_files/figure-html/q10-plot-1-1.png)<!-- -->


```r
## plot2: exclude 'Don_t know' column
p2 <- ggplot(q10[!(q10$Q10_q)=='Don_t_know', ], aes(Q10_q, weight = Freq)) + ptext

# by gender
p2 + geom_bar() + aes(PPGENDER, fill = PPGENDER) + facet_wrap(~Q10_q) + ggtitle("By gender")
```

![](behavior-pt1_files/figure-html/q10-plot-1b-1.png)<!-- -->

```r
# by age group
p2 + geom_bar() + aes(ppagecat, fill = ppagecat) + facet_wrap(~Q10_q) + ggtitle("By age group")
```

![](behavior-pt1_files/figure-html/q10-plot-1b-2.png)<!-- -->

```r
p2 + geom_bar(position = "fill") + aes(ppagecat, fill = Q10_q)
```

![](behavior-pt1_files/figure-html/q10-plot-1b-3.png)<!-- -->

```r
# by ethnic group
p + geom_bar() + aes(PPETHM, fill = PPETHM) + facet_wrap(~Q10_q) + ggtitle("By ethnic group") +
  ptext2
```

![](behavior-pt1_files/figure-html/q10-plot-1b-4.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(PPETHM, fill = Q10_q)
```

![](behavior-pt1_files/figure-html/q10-plot-1b-5.png)<!-- -->

```r
p + geom_bar() + aes(fill = Q10_q) + facet_wrap(~PPETHM) + ggtitle("By ethnic group")
```

![](behavior-pt1_files/figure-html/q10-plot-1b-6.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(fill = PPETHM)
```

![](behavior-pt1_files/figure-html/q10-plot-1b-7.png)<!-- -->

```r
# by income
p + geom_bar() + aes(PPINCIMP, fill = PPINCIMP) + facet_wrap(~Q10_q) + ggtitle("By income") + 
  ptext2
```

![](behavior-pt1_files/figure-html/q10-plot-1b-8.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(PPINCIMP, fill = Q10_q)
```

![](behavior-pt1_files/figure-html/q10-plot-1b-9.png)<!-- -->

```r
# by education
p + geom_bar() + aes(PPEDUCAT, fill = PPEDUCAT) + facet_wrap(~Q10_q) + ggtitle("By education")
```

![](behavior-pt1_files/figure-html/q10-plot-1b-10.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(PPEDUCAT, fill = Q10_q)
```

![](behavior-pt1_files/figure-html/q10-plot-1b-11.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(PPEDUC, fill = Q10_q)
```

![](behavior-pt1_files/figure-html/q10-plot-1b-12.png)<!-- -->

```r
p + geom_bar() + aes(fill = Q10_q) + facet_wrap(~PPEDUCAT) + ggtitle("By education")
```

![](behavior-pt1_files/figure-html/q10-plot-1b-13.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(fill = PPEDUCAT)
```

![](behavior-pt1_files/figure-html/q10-plot-1b-14.png)<!-- -->

```r
# by work status
p + geom_bar() + aes(work, fill = work) + facet_wrap(~Q10_q) + ggtitle("By employment status")
```

![](behavior-pt1_files/figure-html/q10-plot-1b-15.png)<!-- -->

```r
p + geom_bar(position = "fill") + aes(PPWORK, fill = Q10_q) + ggtitle("By employment status")
```

![](behavior-pt1_files/figure-html/q10-plot-1b-16.png)<!-- -->

### Marital status, metro status, region, state of residency, house type, housing status, internet availability


```r
# update weighted data frame
q10.2 <- data.frame(svytable(~Q10_q + Q10_r + marital + PPMARIT + PPMSACAT + ppreg9 +
                              PPSTATEN + PPHOUSE + PPRENT + PPNET, des10, round = T))
# restate plots
p3 <- ggplot(q10.2, aes(Q10_q, weight = Freq)) + ptext
p4 <- ggplot(q10.2[!(q10.2$Q10_q)=='Don_t_know', ], aes(Q10_q, weight = Freq)) + ptext
```


```r
# by marital status
p3 + geom_bar() + aes(marital, fill = marital) + facet_wrap(~Q10_q) + ggtitle("By marital status")
```

![](behavior-pt1_files/figure-html/q10-plot-2b-1.png)<!-- -->

```r
p3 + geom_bar(position = 'fill') + aes(marital, fill = Q10_q)
```

![](behavior-pt1_files/figure-html/q10-plot-2b-2.png)<!-- -->

```r
p3 + geom_bar() + aes(PPMARIT, fill = PPMARIT) + facet_wrap(~Q10_q)
```

![](behavior-pt1_files/figure-html/q10-plot-2b-3.png)<!-- -->

```r
p3 + geom_bar() + aes(PPMARIT, fill = Q10_q) + ggtitle("By marital status")
```

![](behavior-pt1_files/figure-html/q10-plot-2b-4.png)<!-- -->

```r
# by metro status
p3 + geom_bar() + aes(PPMSACAT, fill = PPMSACAT) + facet_wrap(~Q10_q) + ggtitle("By metro status")
```

![](behavior-pt1_files/figure-html/q10-plot-2b-5.png)<!-- -->

```r
p3 + geom_bar(position = 'stack') + aes(fill = PPMSACAT)
```

![](behavior-pt1_files/figure-html/q10-plot-2b-6.png)<!-- -->

```r
p3 + geom_bar() + aes(fill = PPMSACAT) + facet_wrap(~PPMSACAT)
```

![](behavior-pt1_files/figure-html/q10-plot-2b-7.png)<!-- -->

```r
# by region
p3 + geom_bar(position = 'fill') + aes(fill = ppreg9) + ggtitle("Responses by US region")
```

![](behavior-pt1_files/figure-html/q10-plot-2b-8.png)<!-- -->

```r
p3 + geom_bar(position = 'fill') + aes(ppreg9, fill = Q10_q) + ggtitle("US regions by response")
```

![](behavior-pt1_files/figure-html/q10-plot-2b-9.png)<!-- -->

```r
# by state
p3 + geom_bar() + aes(PPSTATEN, fill = Q10_q) + coord_flip() + ggtitle("By state")
```

![](behavior-pt1_files/figure-html/q10-plot-2b-10.png)<!-- -->

```r
p3 + geom_bar(position = 'fill') + aes(PPSTATEN, fill = Q10_q) + coord_flip() + ggtitle("By state")
```

![](behavior-pt1_files/figure-html/q10-plot-2b-11.png)<!-- -->

```r
# by house type
p3 + geom_bar() + aes(fill = PPHOUSE) + ggtitle("By housing")
```

![](behavior-pt1_files/figure-html/q10-plot-2b-12.png)<!-- -->

```r
p3 + geom_bar(position = 'fill') + aes(fill = PPHOUSE)
```

![](behavior-pt1_files/figure-html/q10-plot-2b-13.png)<!-- -->

```r
p3 + geom_bar() + aes(PPHOUSE, fill = PPHOUSE) + facet_wrap(~Q10_q) + ptext2
```

![](behavior-pt1_files/figure-html/q10-plot-2b-14.png)<!-- -->

```r
# by housing status

# by internet availability
p3 + geom_bar(position = 'fill') + aes(fill = PPNET) + ggtitle("Internet status")
```

![](behavior-pt1_files/figure-html/q10-plot-2b-15.png)<!-- -->

```r
p3 + geom_bar(position = 'fill') + aes(PPNET, fill = Q10_q) + ggtitle("Internet status")
```

![](behavior-pt1_files/figure-html/q10-plot-2b-16.png)<!-- -->



