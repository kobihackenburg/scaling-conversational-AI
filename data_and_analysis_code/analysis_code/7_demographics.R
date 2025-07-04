# library(tidyverse)
# library(broom)
# library(estimatr)
# library(brms)
# library(metafor)
# library(patchwork)
# library(tidybayes)

study1 <- readRDS("study_1/output/data_prepared.rds") %>% mutate(study = 1)
study2 <- readRDS("study_2/output/data_prepared.rds") %>% mutate(study = 2)
study3 <- readRDS("study_3/output/data_prepared.rds") %>% mutate(study = 3)

names(study1)
names(study2)
names(study3)

f_age <- function(df, title) {
  df %>% 
    ggplot(aes(x = age)) + 
    theme_bw() +
    geom_histogram() + 
    labs(title = title, x = "") +
    theme(plot.title = element_text(hjust=0.5))
}

f_gender <- function(df, title) {
  df %>% 
    mutate(gender = case_when(gender == 1 ~ "Male",
                              gender == 2 ~ "Female",
                              gender == 3 ~ "Other")) %>% 
    ggplot(aes(x = gender)) + 
    theme_bw() +
    geom_histogram(stat = "count") + 
    labs(title = title, x = "") +
    theme(plot.title = element_text(hjust=0.5))
}

f_ideology <- function(df, title) {
  df %>% 
    ggplot(aes(x = ideology)) + 
    theme_bw() +
    geom_histogram() + 
    labs(title = title, x = "", subtitle = "(1 = Left, 5 = Right)") +
    theme(plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5))
}

g <-
  f_age(study1, "Age") +
  f_gender(study1, "Gender") +
  f_ideology(study1, "Ideology") +
  plot_annotation(
    title = "Study 1",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )
  
ggsave(plot = g, "output/supplement/demographics/study1.pdf", height = 5, width = 12)

g <-
  f_age(study2, "Age") +
  f_gender(study2, "Gender") +
  f_ideology(study2, "Ideology") +
  plot_annotation(
    title = "Study 2",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

ggsave(plot = g, "output/supplement/demographics/study2.pdf", height = 5, width = 12)

g <-
  f_age(study3, "Age") +
  f_gender(study3, "Gender") +
  f_ideology(study3, "Ideology") +
  plot_annotation(
    title = "Study 3",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

ggsave(plot = g, "output/supplement/demographics/study3.pdf", height = 5, width = 12)
