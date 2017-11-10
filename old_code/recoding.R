# Re-group and re-code categorical variables

rm(list = ls(all.names = TRUE))
load("old_data/cleaning2.RData")
library(car)
library(dplyr)


data2 <- dataf  # temporarily change to data2 to match rest of code
datar <- dataf  # make new data frame for recoded variables

# use code function for response variables
# reset the "default" level on categorical variables
code <- function(col, map, ref) {
  relevel(as.factor(map[col]), ref=ref)
}


# ethnicity
summary(data2$PPETHM)
# datar$white <- car::recode(data2$PPETHM, recodes = "'White, Non-Hispanic' = 1; NA = NA; else = 0")
# datar$black <- car::recode(data2$PPETHM, recodes = "'Black, Non-Hispanic' = 1; NA = NA; else = 0")
# datar$hispanic <- car::recode(data2$PPETHM, recodes = "'Hispanic' = 1; NA = NA; else = 0")
# datar$otherrace <- car::recode(data2$PPETHM, recodes = "'Other, Non-Hispanic' = 1; NA = NA; else = 0")
# datar$mixedrace <- car::recode(data2$PPETHM, recodes = "'2+ Races, Non-Hispanic' = 1; NA = NA; else = 0")


# female
summary(data2$PPGENDER)
#datar$female <- car::recode(data2$PPGENDER, recodes = "'Female' = 1; 'Male' = 0")


# re-group income level
summary(data2$PPINCIMP)
income.map <- c(rep("under $10k", 3),
                rep("$10k to $25k", 4),
                rep("$25k to $50k", 4),
                rep("$50k to $75k", 2),
                rep("$75k to $100k", 2),
                rep("$100k to $150k", 2),
                rep("over $150k", 2))
datar$income <- code(data2$PPINCIMP, income.map, "under $10k")


# re-group marital staus
summary(data2$PPMARIT)
marital.map <- c("single", "partnered", "partnered", "single", "single", "single")
datar$marital <- code(data2$PPMARIT, marital.map, "single")
#summary(datar$marital)


# regroup work status
summary(data2$PPWORK)
levels(data2$PPWORK)
work.map <- c(rep("unemployed", 5),
              rep("employed", 2))
datar$work <- code(data2$PPWORK, work.map, "unemployed")
#str(datar$work)


##### -- survey questions --- #####


# Q1. flu knowledge
#datar$q1 <- car::recode(data2$Q1, recodes = "'Yes' = 1; 'No' = 0; NA = NA")
#summary(datar$q1)
summary(data2$Q1)


# Q2. sick with flu last year
#datar$sick <- car::recode(data2$Q2, recodes = "'Yes' = 1; 'No' = 0; NA = NA")
#datar$sick <- datar$q2
#summary(datar$q2)
summary(data2$Q2)


# Q3. household sick last year
#datar$q3 <- car::recode(data2$Q3, recodes = "'Yes' = 1; c('No', 'Don_t know') = 0; NA = NA")
#datar$sickhh <- datar$q3


# re-group Q4. job requires contact with public
summary(data2$Q4)
q4.map <- c("Yes", "No", "No")
datar$q4 <- code(data2$Q4, q4.map, "Yes")
#summary(datar$q4)


# re-group Q13. flu vaccine
summary(data2$Q13)
q13.map <- c("Yes", "Yes", "No")
datar$q13 <- code(data2$Q13, q13.map, "Yes")
#summary(datar$q13)


# Q26. children in household
#datar$q26 <- data2$Q26 %>%
#  car::recode(recodes = "'Yes' = 1; NA = NA; 'No' = 0")
#summary(data2$Q26)

## if you get confused, small q is the re-grouped variable
#str(datar$Q26)
#str(datar$q26)


##### ----- save ----- #####
# switch back temp name
dataf <- data2
rm(data2)

save(datar, code, file = "old_data/recoding.RData")
#rm(list = ls(all.names = TRUE))



