# data cleaning

rm(list = ls(all.names = TRUE))

library(rmarkdown); library(dplyr); library(ggplot2); library(survey);
library(gridExtra)

## Load data variables.

load("~/git/flu-survey/data/cleaning2.RData")
load("~/git/flu-survey/data/recoding.RData")  # load "datar"
df <- datar  # contains recoded variables