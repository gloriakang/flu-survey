# convert to stata file format
library(haven)
library(readr)


# read csv
surveydata <- read_csv("~/git/flu-survey/data/surveydata.csv")

# write csv dataframe to dta
haven::write_dta(data = surveydata, path = 'data/datafile.dta', version = 13)


## read spss data
#df <- haven::read_spss(file = 'data/datafile.sav')

## write spss dataframe to stata
#haven::write_dta(data = df, path = 'data/datafile.dta', version = 13)

