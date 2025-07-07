
# library(tidyverse)
# library(fixest)
# library(tidymodels)

fun_my_kable <- function(df, my_caption, my_file, my_footnote) {
  df %>% 
    kable(
      "latex", 
      booktabs = TRUE, 
      caption = my_caption
    ) %>%
    kable_styling(latex_options = c("HOLD_position", "striped")) %>% 
    footnote(general = my_footnote) %>% 
    cat(file = my_file)
}

fun_round_fix_p <- function(table) {
  
  table %>% 
    mutate(`p.value` = ifelse(`p.value` <.001, "<.001", as.character(round(`p.value`, 3)))) %>% 
    mutate(across(where(is.numeric), ~round(.x, 2)))
  
}

fp <- "output/supplement/results_tables/"

#### Load responses & fact-checking data from all studies
study1 <- readRDS("study_1/output/data_prepared.rds") %>% mutate(study = 1)
study2 <- readRDS("study_2/output/data_prepared.rds") %>% mutate(study = 2)
study3 <- readRDS("study_3/output/data_prepared.rds") %>% mutate(study = 3)
df_information <- readRDS("output/df_for_variance_explained_analysis.rds")

df_attitudes = tibble(
  "data" = list(
    study1 %>% select(
      ResponseId,
      d1.issue_id, d1.model, d1.prompt_personalize, d1.prompt_rhetoric_id, d1.convo_n_facts_total, d1.post_average, d1.pre_average, d1.condition, d1.compliance,
      d2.issue_id, d2.prompt_personalize, d2.prompt_rhetoric_id, d2.convo_n_facts_total, d2.post_average, d2.pre_average, d2.condition
    ),
    study2 %>% select(
      ResponseId,
      issue_id, model, personalize, prompt_rhetoric_id, post_train, convo_n_facts_total, post_average, pre_average, condition
    ),
    study3 %>% select(
      ResponseId,
      issue_id, model, personalize, prompt_rhetoric_id, post_train, convo_n_facts_total, post_average, pre_average, condition, compliance
    )),
  "study" = 1:3
)

#### Merge into a single dataset for random forest training
df = bind_rows(
  df_attitudes$data[[1]] %>%
    mutate(d2.issue_id=as.character(d2.issue_id), d2.model="gpt-4o") %>%
    pivot_longer(-ResponseId, names_sep="\\.", names_to = c("study", ".value")) %>%
    mutate(study = c("d1"="1A", "d2"="1B")[study]) %>%
    rename(personalize=prompt_personalize),
  df_attitudes$data[[2]] %>% mutate(issue_id = as.character(issue_id), study="2"),
  df_attitudes$data[[3]] %>% mutate(issue_id = as.character(issue_id), study="3")
) %>%
  filter(!str_detect(condition, "static"), !is.na(post_average), !is.na(convo_n_facts_total)) %>%
  mutate(post_train = replace_na(post_train, "base"),
         prompt_rhetoric_id = replace_na(prompt_rhetoric_id, "NA"),
         delta = post_average - pre_average
  )
df_control = df %>% filter(str_detect(condition, "control"))
df = df %>% filter(!str_detect(condition, "control"))

d_facts <- df_information %>%
  mutate(study=case_when(
    study==1 & dialogue==1 ~ "1A",
    study==1 & dialogue==2 ~ "1B",
    study==2 ~ "2",
    study==3 ~ "3",
  )) %>%
  select(-dialogue) %>%
  right_join(df %>% select(ResponseId, study) %>% distinct) %>%
  mutate(across(c(n_facts, n_bad_facts, n_good_facts), ~replace_na(., 0)))

df_sizes = readRDS("output/processed_data/df_model_sizes.rds")
df = df %>% left_join(d_facts) %>% left_join(df_sizes)


#### Fit the random forest and get the out-of-fold predictions

df <- df %>% mutate(.row = row_number()) 
folds <- vfold_cv(df, v = 5)

get_oof <- function(outcome) {
  rec <- recipe(as.formula(paste(outcome, "~ post_train + model + study + personalize + prompt_rhetoric_id")), data = df) %>% 
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors())
  
  rf_spec <- rand_forest() %>% 
    set_engine("ranger") %>% 
    set_mode("regression")
  
  wf <- workflow() %>% add_recipe(rec) %>% add_model(rf_spec)
  
  map_dfr(folds$splits, function(s) {
    fit_wf <- fit(wf, data = analysis(s) %>% filter(!is.na(get(outcome))))
    assessment(s) %>%
      select(.row) %>%
      bind_cols(predict(fit_wf, new_data = assessment(s)))
  }) %>% 
    rename(!!paste0(outcome, "_hat") := .pred)
}

df_preds <- df 
df_preds = df_preds %>% left_join(get_oof("delta"), by = ".row")
df_preds = df_preds %>% left_join(get_oof("n_facts"), by = ".row")
df_preds = df_preds %>% left_join(get_oof("mean_accuracy"), by = ".row")



#### Analysis: variance explained by n_facts
# All models
d = df_preds
lwr = summary(lm(delta ~ pre_average + study, d))$r.squared
x = summary(lm(delta ~ pre_average + study + n_facts_hat, d))$r.squared
upr = summary(lm(delta ~ pre_average + study + n_facts_hat + delta_hat, d))$r.squared
(x-lwr) / (upr-lwr)

# Only developer post-trained
d = df_preds %>% filter(!str_detect(model, "llama|qwen"))
lwr = summary(lm(delta ~ pre_average + study, d))$r.squared
x = summary(lm(delta ~ pre_average + study + n_facts_hat, d))$r.squared
upr = summary(lm(delta ~ pre_average + study + n_facts_hat + delta_hat, d))$r.squared
(x-lwr) / (upr-lwr)

# Only study 1B, 2, 3
d = df_preds %>% filter(study!="1A")
lwr = summary(lm(delta ~ pre_average + pre_average + study, d))$r.squared
x = summary(lm(delta ~ pre_average + study + n_facts_hat, d))$r.squared
upr = summary(lm(delta ~ pre_average + study + n_facts_hat + delta_hat, d))$r.squared
(x-lwr) / (upr-lwr)


#### Analysis: Impact of 500 persusion-optimized conversations (vs average conversations)

d_all = bind_rows(
  df_preds %>%
    filter(study %in% c("1B", "2", "3")) %>%
    filter(!str_detect(model, "llama|qwen")) %>%
    mutate(t=1),
  df_control %>% filter(study %in% c("1B", "2", "3")) %>% mutate(t=0)
)

d_top500 = bind_rows(
  df_preds %>%
    filter(study %in% c("1B", "2", "3")) %>%
    filter(!str_detect(model, "llama|qwen")) %>%
    slice_max(delta_hat, n=500) %>%
    mutate(t=1),
  df_control %>% filter(study %in% c("1B", "2", "3")) %>% mutate(t=0)
)



# ATE
d_all %>% feols(post_average ~ scale(pre_average) + t | study, .)
d_top500 %>% feols(post_average ~ scale(pre_average) + t | study, .)

# CATE among opponents
d_all %>%
  filter(pre_average<50) %>%
  feols(post_average ~ scale(pre_average) + t | study, .)
d_top500 %>%
  filter(pre_average<50) %>%
  feols(post_average ~ scale(pre_average) + t | study, .)

# Number of facts
d_all %>% feols(n_facts ~ 1, .)
d_top500 %>% feols(n_facts ~ 1, .)

# Proportion with accuracy < 50%
d_all %>% feols(n_bad_facts/n_facts ~ 1, .)
d_top500 %>% feols(n_bad_facts/n_facts ~ 1, .)

## Analysis: persuasion vs inaccurate facts, conditioning on n_facts
out <-
  df %>%
  group_by(study, model, post_train, prompt_rhetoric_id, personalize) %>%
  summarize(across(c(delta, n_facts, n_bad_facts), mean)) %>%
  group_by(study) %>%
  group_modify(~broom::tidy(lm(delta ~ n_facts + n_bad_facts, .), conf.int=T)) %>%
  filter(term != "(Intercept)")

# Save table
out %>% 
  mutate(term = ifelse(term=="n_facts", "N claims", "N inaccurate claims")) %>% 
  fun_round_fix_p() %>% 
  fun_my_kable(
    my_caption = "Association between N inaccurate claims and persuasion adjusting for total N claims.",
    my_file = paste0(fp, "9_inaccurate_claims_vs_persuasion/n_inaccurate_claims_on_persuasion.txt"),
    my_footnote = "Estimates are in percentage points."
  )


