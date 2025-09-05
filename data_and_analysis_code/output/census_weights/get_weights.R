
library(tidyverse)

df <- readxl::read_xlsx("ukcensus_age_sex_edu.xlsx")

names(df)

total_n <- 
  df %>% 
  select(`No qualifications`, 
         `Below degree level [Note 18]`, 
         `Degree level or above [Note 18]`) %>% 
  sum()

target <-
  df %>% 
  mutate(no_degree = `No qualifications` + `Below degree level [Note 18]`) %>% 
  rename(degree = `Degree level or above [Note 18]`) %>% 
  select(Sex, Age, no_degree, degree) %>% 
  pivot_longer(cols = c(no_degree, degree),
               names_to = "Education") %>% 
  rename(n = value) %>% 
  mutate(prop = n/total_n)

target %>% saveRDS("census_weights_age_gender_edu.rds")


# df <- read_csv("census2021-ts009-ctry.csv")
# 
# names(df)
# 
# list_vars <-
#   c(
#   "Sex: Male; Age: Aged 4 years and under; measures: Value",
#   "Sex: Male; Age: Aged 5 to 9 years; measures: Value",
#   "Sex: Male; Age: Aged 10 to 15 years; measures: Value",
#   "Sex: Male; Age: Aged 16 to 19 years; measures: Value",
#   "Sex: Male; Age: Aged 20 to 24 years; measures: Value",
#   "Sex: Male; Age: Aged 25 to 34 years; measures: Value",
#   "Sex: Male; Age: Aged 35 to 49 years; measures: Value"  ,
#   "Sex: Male; Age: Aged 50 to 64 years; measures: Value",
#   "Sex: Male; Age: Aged 65 to 74 years; measures: Value",
#   "Sex: Male; Age: Aged 75 to 84 years; measures: Value",
#   "Sex: Male; Age: Aged 85 years and over; measures: Value",
#   
#   "Sex: Female; Age: Aged 4 years and under; measures: Value",
#   "Sex: Female; Age: Aged 5 to 9 years; measures: Value",
#   "Sex: Female; Age: Aged 10 to 15 years; measures: Value",
#   "Sex: Female; Age: Aged 16 to 19 years; measures: Value",
#   "Sex: Female; Age: Aged 20 to 24 years; measures: Value",
#   "Sex: Female; Age: Aged 25 to 34 years; measures: Value",
#   "Sex: Female; Age: Aged 35 to 49 years; measures: Value"  ,
#   "Sex: Female; Age: Aged 50 to 64 years; measures: Value",
#   "Sex: Female; Age: Aged 65 to 74 years; measures: Value",
#   "Sex: Female; Age: Aged 75 to 84 years; measures: Value",
#   "Sex: Female; Age: Aged 85 years and over; measures: Value"
# )
# 
# total_n_eng <-
#   df %>% 
#   filter(geography=="England") %>% 
#   select(all_of(list_vars)) %>% 
#   sum()
# 
# df_targets <-
#   df %>% 
#   filter(geography=="England") %>% 
#   select(all_of(list_vars)) %>% 
#   pivot_longer(everything()) %>% 
#   mutate(prop = value/total_n_eng,
#          name = str_remove_all(name, "; measures: Value|Sex: | Age: Aged ")) %>% 
#   separate(name, into = c("sex", "age"), sep = ";")
# 
# df_targets
# 
# df_targets %>% saveRDS("census_weights_age_gender.rds")

