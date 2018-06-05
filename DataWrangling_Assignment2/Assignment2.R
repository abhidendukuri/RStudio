library(dplyr)
library(tidyr)
library(gsubfn)

df <- read.csv("titanic_original.csv")
titanic <- tbl_df(df)

titanicClean <- titanic %>% 
  # replace empty strings in embarked with 'S'
  mutate(embarked = sub("^$", "S", titanic$embarked)) %>%

  # replace NA ages with the mean
  replace_na(list(age = mean(titanic$age, na.rm = TRUE))) %>%

  # replace empty values with NA
  mutate(boat = sub("^$", "NA", titanic$boat)) %>%

  # create new column has_cabin_number
  mutate(has_cabin_number = sub("^$", 0, sub("[A-Z].*$", 1, titanic$cabin))) %>%

  
  select(pclass, survived, name, sex, age, sibsp, parch, ticket, fare, cabin, has_cabin_number, 
         embarked, boat, body, home.dest)

write.csv(titanicClean, "titanic_clean.csv")

