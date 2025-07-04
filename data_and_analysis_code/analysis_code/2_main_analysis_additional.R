
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

# Total messages and claims ----

# AI Messages
sum(
  study1 %>% filter(d1.condition=="treat_dialogue") %>% pull(d1.count_ai_msgs) %>% sum(na.rm=T),
  study1 %>% filter(d2.condition=="treatment") %>% pull(d2.count_ai_msgs) %>% sum(na.rm=T),
  study2 %>% filter(condition!="control") %>% pull(n_msgs_ai) %>% sum(na.rm=T),
  study3 %>% filter(!(condition %in% c("control", "treatment-3"))) %>% pull(n_msgs_ai) %>% sum(na.rm=T)
)

# AI + human Messages
sum(
  study1 %>% filter(d1.condition=="treat_dialogue") %>% pull(d1.count_ai_msgs) %>% sum(na.rm=T),
  study1 %>% filter(d2.condition=="treatment") %>% pull(d2.count_ai_msgs) %>% sum(na.rm=T),
  study2 %>% filter(condition!="control") %>% pull(n_msgs_ai) %>% sum(na.rm=T),
  study3 %>% filter(!(condition %in% c("control", "treatment-3"))) %>% pull(n_msgs_ai) %>% sum(na.rm=T),
  study1 %>% filter(d1.condition=="treat_dialogue") %>% pull(d1.count_user_msgs) %>% sum(na.rm=T),
  study1 %>% filter(d2.condition=="treatment") %>% pull(d2.count_user_msgs) %>% sum(na.rm=T),
  study2 %>% filter(condition!="control") %>% pull(n_msgs_user) %>% sum(na.rm=T),
  study3 %>% filter(!(condition %in% c("control", "treatment-3"))) %>% pull(n_msgs_user) %>% sum(na.rm=T)
)

# Claims
sum(
  study1 %>% filter(d1.condition=="treat_dialogue") %>% pull(d1.convo_n_facts_total) %>% sum(na.rm=T),
  study1 %>% filter(d2.condition=="treatment") %>% pull(d2.convo_n_facts_total) %>% sum(na.rm=T),
  study2 %>% filter(condition!="control") %>% pull(convo_n_facts_total) %>% sum(na.rm=T),
  study3 %>% filter(!(condition %in% c("control", "treatment-3"))) %>% pull(convo_n_facts_total) %>% sum(na.rm=T)
)

# Total N ----
sum(
  study1 %>% filter(!is.na(d1.post_average)) %>% nrow,
  study2 %>% filter(!is.na(post_average)) %>% nrow,
  study3 %>% filter(!is.na(post_average)) %>% nrow
)

# Llama deceptive-info vs. info ----

bind_rows(
  study2 %>% 
    filter(str_detect(model, "405b"), 
           str_detect(prompt_rhetoric_id, "information")) %>% 
    lm_robust(post_average ~ prompt_rhetoric_id + pre_average, data = .) %>% 
    tidy(),
  
  study2 %>% 
    filter(str_detect(model, "405b"), 
           str_detect(prompt_rhetoric_id, "information")) %>% 
    lm_robust(convo_prop_facts_above_veracity_threshold ~ prompt_rhetoric_id, data = .) %>% 
    tidy() %>% 
    mutate(across(c(estimate, std.error, conf.low, conf.high), ~.x*100))
) %>% 
  saveRDS("output/processed_data/df_estimates_405b_deception.rds")


# SFT / RM interaction ----

study2 %>% 
  filter(condition=="treatment-1") %>% 
  mutate(rm = ifelse(str_detect(post_train, "rm"), 1, 0),
         sft = ifelse(str_detect(post_train, "sft"), 1, 0)) %>% 
  lm_robust(post_average ~ rm*sft + pre_average, data = .) %>% 
  tidy() %>% 
  saveRDS("output/processed_data/df_estimates_sft_rm_interaction.rds")

# Convo descriptives ----

# Total AI treatment messages
sum(
  study1 %>% filter(d1.condition=="treat_dialogue") %>% pull(d1.count_ai_msgs) %>% sum(na.rm = T),
  study1 %>% filter(d2.condition=="treatment") %>% pull(d2.count_ai_msgs) %>% sum(na.rm = T),
  study2 %>% filter(condition!="control") %>% pull(n_msgs_ai) %>% sum(na.rm = T),
  study3 %>% filter(condition!="control", condition != "treatment-3") %>% pull(n_msgs_ai) %>% sum(na.rm = T)  
)

# Total treatment convos
sum(
  study1 %>% filter(d1.condition=="treat_dialogue") %>% filter(!is.na(d1.post_average)) %>% nrow,
  study1 %>% filter(d2.condition=="treatment") %>% filter(!is.na(d2.post_average)) %>% nrow,
  study2 %>% filter(condition!="control") %>% filter(!is.na(post_average)) %>% nrow,
  study3 %>% filter(condition!="control", condition != "treatment-3") %>% filter(!is.na(post_average)) %>% nrow  
)


fun_convo <- function(data, variable) {
  median(data[[variable]], na.rm = T)
}

# Duration in mins
median(
  fun_convo(study1 %>% filter(d1.condition=="treat_dialogue"), "d1.chat_duration_in_s")/60,
  fun_convo(study1 %>% filter(d2.condition=="treatment"), "d2.chat_duration_in_s")/60,
  fun_convo(study2 %>% filter(condition!="control"), "chat_duration_in_sec")/60,
  fun_convo(study3 %>% filter(condition %in% c("treatment-1", "treatment-2")), "chat_duration_in_sec")/60  
)

# N turns
median(
  fun_convo(study1 %>% filter(d1.condition=="treat_dialogue"), "d1.count_user_msgs"),
  fun_convo(study1 %>% filter(d2.condition=="treatment"), "d2.count_user_msgs"),
  fun_convo(study2 %>% filter(condition!="control"), "n_msgs_user"),
  fun_convo(study3 %>% filter(condition %in% c("treatment-1", "treatment-2")), "n_msgs_user")
)

# Average accuracy
mean(
  c(
    mean(study1$d1.convo_mean_veracity, na.rm=T),
    mean(study1$d2.convo_mean_veracity, na.rm=T),
    mean(study2$convo_mean_veracity, na.rm=T),
    mean(study3$convo_mean_veracity, na.rm=T)
  )
)

# Average prop accuracy
mean(
  c(
    mean(study1$d1.convo_prop_facts_above_veracity_threshold, na.rm=T),
    mean(study1$d2.convo_prop_facts_above_veracity_threshold, na.rm=T),
    mean(study2$convo_prop_facts_above_veracity_threshold, na.rm=T),
    mean(study3$convo_prop_facts_above_veracity_threshold, na.rm=T)
  )
)


# GPT-4o-new opposing viewpoint ----

study3 %>% 
  filter(post_train == "base",
         pre_average < 50) %>% 
  lm_robust(reformulate(c("models_vs_control", "pre_average"), response = "post_average"), .) %>% 
  tidy() %>% 
  mutate(study = 3)

# GPT-4o-new vs. 4.5 / grok / 4o (all base) ----

study3 %>% 
  filter(post_train == "base", condition %in% c("treatment-1", "treatment-2")) %>% 
  lm_robust(reformulate(c("fct_relevel(model, 'chatgpt-4o-latest')", "pre_average"), 
                        response = "post_average"), .) %>% 
  tidy() %>% 
  mutate(study = 3) %>% 
  saveRDS("output/processed_data/df_estimates_4o_new_vs_others_base_only.rds")

# GPT-4o-new vs. old accuracy ----

study3 %>% 
  filter(post_train == "base",
         condition == "treatment-1",
         str_detect(model, "gpt-4o")) %>% 
  lm_robust(reformulate(c('model=="chatgpt-4o-latest"'), response = "convo_prop_facts_above_veracity_threshold"), .) %>% 
  tidy() %>% 
  mutate(study = 3)

# Prompt-level treatments effects ----

df_prompt_ates_meta <-
  bind_rows(
  
  study1 %>% 
    filter(d1.condition != "treat_static") %>% 
    mutate(prompt_vs_control = case_when(str_detect(d1.condition, "control") ~ "aa_control", T ~ d1.prompt_rhetoric_id)) %>% 
    lm_robust(d1.post_average ~ prompt_vs_control + d1.pre_average, .) %>% 
    tidy() %>% 
    mutate(study = 1, dialogue = 1),
  
  study1 %>% 
    mutate(prompt_vs_control = case_when(str_detect(d2.condition, "control") ~ "aa_control", T ~ d2.prompt_rhetoric_id)) %>% 
    lm_robust(d2.post_average ~ prompt_vs_control + d2.pre_average, .) %>% 
    tidy() %>% 
    mutate(study = 1, dialogue = 2),
  
  study2 %>% 
    filter(condition != "treatment-3",
           post_train %in% c(NA, "base", "rm")) %>% 
    mutate(prompt_vs_control = case_when(str_detect(condition, "control") ~ "aa_control", T ~ prompt_rhetoric_id)) %>% 
    lm_robust(post_average ~ prompt_vs_control + pre_average, .) %>% 
    tidy() %>% 
    mutate(study = 2),
  
  study3 %>% 
    filter(condition != "treatment-3",
           post_train %in% c(NA, "base", "rm")) %>% 
    mutate(prompt_vs_control = case_when(str_detect(condition, "control") ~ "aa_control", T ~ prompt_rhetoric_id)) %>% 
    lm_robust(post_average ~ prompt_vs_control + pre_average, .) %>% 
    tidy() %>% 
    mutate(study = 3)
  
) %>% 
  filter(term != "(Intercept)", str_detect(term, "pre_average", negate = T)) %>% 
  group_by(term) %>% 
  nest() %>%
  mutate(
    model = map(data, ~ rma(yi = estimate, sei = std.error, method = "FE", data = .x) %>% tidy(conf.int=T)),
  ) %>%
  ungroup() %>% 
  rename(prompt = term) %>% 
  unnest(model)

df_prompt_ates_meta %>% filter(str_detect(prompt, "info")) %>% pull(estimate) /
df_prompt_ates_meta %>% filter(str_detect(prompt, "none")) %>% pull(estimate)

df_prompt_ates_meta %>% 
  rma(yi = estimate,
      sei = std.error,
      mods = ~ fct_relevel(prompt, "prompt_vs_controlinformation", after = 0),
      data = .,
      method = "FE")


# RM and SFT main effects (collapsed across model) on persuasion / info density / veracity ----

# > Persuasion ----
f_persuasion <- function(data, outcome_var) {
  data %>% 
    mutate(rm = ifelse(str_detect(post_train, "rm"), 1, 0),
           sft = ifelse(str_detect(post_train, "sft"), 1, 0)) %>% 
    lm_robust(reformulate(c("rm", "sft", "pre_average"), response = outcome_var), .) %>% 
    tidy()
}

list_outcomes <- c("post_average", "post_average_imputed_with_pre")

s2_llama_persuade    <- map(list_outcomes, ~f_persuasion(study2 %>% filter(condition == "treatment-1"), .x)) %>% bind_rows()
s2_gpt_persuade      <- map(list_outcomes, ~f_persuasion(study2 %>% filter(condition == "treatment-2"), .x)) %>% bind_rows()
s3_gpt_grok_persuade <- map(list_outcomes, ~f_persuasion(study3 %>% filter(condition %in% c("treatment-1" ,"treatment-2")), .x)) %>% bind_rows()

# PW mean for developer models:
map(list_outcomes,
    function(.x) {
      rma(yi = estimate, 
          sei = std.error, 
          method = "FE",
          data = bind_rows(
            s2_gpt_persuade %>% filter(term=="rm", outcome==.x),
            s3_gpt_grok_persuade %>% filter(term=="rm", outcome==.x)
          )) %>% 
        tidy(conf.int = T) %>% 
        mutate(outcome_id = .x)
    }) %>% 
  bind_rows() %>% 
  saveRDS("output/processed_data/df_estimates_post_train_main_fx_pw_mean.rds")


# > Info density ----
f_density <- function(data) {
  data %>% 
    mutate(rm = ifelse(str_detect(post_train, "rm"), 1, 0),
           sft = ifelse(str_detect(post_train, "sft"), 1, 0)) %>% 
    lm_robust(convo_n_facts_total ~ rm + sft, .) %>% 
    tidy()
}

s2_llama_density    <- f_density(study2 %>% filter(condition == "treatment-1"))
s2_gpt_density      <- f_density(study2 %>% filter(condition == "treatment-2"))
s3_gpt_grok_density <- f_density(study3 %>% filter(condition %in% c("treatment-1" ,"treatment-2")))

# > Average veracity ----
f_veracity <- function(data) {
  data %>% 
    mutate(rm = ifelse(str_detect(post_train, "rm"), 1, 0),
           sft = ifelse(str_detect(post_train, "sft"), 1, 0)) %>% 
    lm_robust(convo_mean_veracity ~ rm + sft, .) %>% 
    tidy()
}

s2_llama_veracity    <- f_veracity(study2 %>% filter(condition == "treatment-1"))
s2_gpt_veracity      <- f_veracity(study2 %>% filter(condition == "treatment-2"))
s3_gpt_grok_veracity <- f_veracity(study3 %>% filter(condition %in% c("treatment-1" ,"treatment-2")))

# > Veracity threshold ----
f_prop_veracity <- function(data) {
  data %>% 
    mutate(rm = ifelse(str_detect(post_train, "rm"), 1, 0),
           sft = ifelse(str_detect(post_train, "sft"), 1, 0)) %>% 
    lm_robust(convo_prop_facts_above_veracity_threshold ~ rm + sft, .) %>% 
    tidy()
}

s2_llama_prop_veracity    <- f_prop_veracity(study2 %>% filter(condition == "treatment-1"))
s2_gpt_prop_veracity      <- f_prop_veracity(study2 %>% filter(condition == "treatment-2"))
s3_gpt_grok_prop_veracity <- f_prop_veracity(study3 %>% filter(condition %in% c("treatment-1" ,"treatment-2")))


bind_rows(
  s2_llama_persuade %>% mutate(study = 2, type = "chat-tuned"),
  s2_gpt_persuade %>% mutate(study = 2, type = "developer"),
  s3_gpt_grok_persuade %>% mutate(study = 3, type = "developer"),
  s2_llama_density %>% mutate(study = 2, type = "chat-tuned"),
  s2_gpt_density %>% mutate(study = 2, type = "developer"),
  s3_gpt_grok_density %>% mutate(study = 3, type = "developer"),
  s2_llama_veracity %>% mutate(study = 2, type = "chat-tuned"),
  s2_gpt_veracity %>% mutate(study = 2, type = "developer"),
  s3_gpt_grok_veracity %>% mutate(study = 3, type = "developer"),
  s2_llama_prop_veracity %>% mutate(study = 2, type = "chat-tuned"),
  s2_gpt_prop_veracity %>% mutate(study = 2, type = "developer"),
  s3_gpt_grok_prop_veracity %>% mutate(study = 3, type = "developer")
) %>% 
  filter(term %in% c("rm", "sft")) %>% 
  drop_na() %>% 
  saveRDS("output/processed_data/df_estimates_post_train_main_fx.rds")


# Personalization ----

study1 <-
  study1 %>% 
  rename(d1.personalize = d1.prompt_personalize,
         d2.personalize = d2.prompt_personalize)

f <- function(data, prefix, outcome_var) {
  
  data %>% 
    lm_robust(reformulate(c(paste0(prefix, "personalize"), 
                            paste0(prefix, "pre_average")), 
                          response = paste0(prefix, outcome_var)), data = .) %>% 
    tidy()
  
}


df_personalize_out <-
  map(list_outcomes,
    ~bind_rows(
      
      # > Study 1 chat 1: chat-tuned, base ----
      f(study1 %>% 
          filter(d1.condition %in% c("treat_dialogue"), 
                 str_detect(d1.model, "gpt|chat", negate = T)), 
        "d1.", .x) %>% 
        mutate(study = 1, 
               dialogue = 1, 
               outcome_id = .x, 
               model_type = "chat-tuned",
               ppt_type = "base"),
      
      # > Study 1 chat 1: developer, base ----
      f(study1 %>% 
          filter(d1.condition %in% c("treat_dialogue"), 
                 str_detect(d1.model, "gpt|chat")), 
        "d1.", .x) %>% 
        mutate(study = 1, 
               dialogue = 1, 
               outcome_id = .x, 
               model_type = "developer",
               ppt_type = "base"),
      
      # > Study 1 chat 2: developer, base ----
      f(study1 %>% 
          filter(d2.condition %in% c("treatment")), 
        "d2.", .x) %>% 
        mutate(study = 1, 
               dialogue = 2, 
               outcome_id = .x, 
               model_type = "developer",
               ppt_type = "base"),
      
      # > Study 2: chat-tuned, base ----
      f(study2 %>% 
          filter(condition %in% c("treatment-1", "treatment-3"),
                 post_train %in% c(NA, "base")), 
        "", .x) %>% 
        mutate(study = 2, 
               outcome_id = .x, 
               model_type = "chat-tuned",
               ppt_type = "base"),
      
      # > Study 2: chat-tuned, RM-only ----
      f(study2 %>% 
          filter(condition %in% c("treatment-1"),
                 post_train %in% c("rm")), 
        "", .x) %>% 
        mutate(study = 2, 
               outcome_id = .x, 
               model_type = "chat-tuned",
               ppt_type = "rm-only"),
      
      # > Study 2: chat-tuned, SFT-only ----
      f(study2 %>% 
          filter(condition %in% c("treatment-1"),
                 post_train %in% c("sft")), 
        "", .x) %>% 
        mutate(study = 2, 
               outcome_id = .x, 
               model_type = "chat-tuned",
               ppt_type = "sft-only"),
      
      # > Study 2: chat-tuned, SFT+RM ----
      f(study2 %>% 
          filter(condition %in% c("treatment-1"),
                 post_train %in% c("sft_and_rm")), 
        "", .x) %>% 
        mutate(study = 2, 
               outcome_id = .x, 
               model_type = "chat-tuned",
               ppt_type = "sft_and_rm"),
      
      # > Study 2: developer, base ----
      f(study2 %>% 
          filter(condition %in% c("treatment-2"),
                 post_train %in% c("base")), 
        "", .x) %>% 
        mutate(study = 2, 
               outcome_id = .x, 
               model_type = "developer",
               ppt_type = "base"),
      
      # > Study 2: developer, RM-only ----
      f(study2 %>% 
          filter(condition %in% c("treatment-2"),
                 post_train %in% c("rm")), 
        "", .x) %>% 
        mutate(study = 2, 
               outcome_id = .x, 
               model_type = "developer",
               ppt_type = "rm-only"),
      
      # > Study 3: developer, base ----
      f(study3 %>% 
          filter(condition %in% c("treatment-1", "treatment-2"),
                 post_train %in% c("base")), 
        "", .x) %>% 
        mutate(study = 3, 
               outcome_id = .x, 
               model_type = "developer",
               ppt_type = "base"),
      
      # > Study 3: developer, RM-only ----
      f(study3 %>% 
          filter(condition %in% c("treatment-1", "treatment-2"),
                 post_train %in% c("rm")), 
        "", .x) %>% 
        mutate(study = 3, 
               outcome_id = .x, 
               model_type = "developer",
               ppt_type = "rm-only")
      
    )) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "personalize"))

saveRDS(df_personalize_out, "output/processed_data/df_estimates_personalization.rds")

# > PW mean overall ----
# df_personalize_out %>% 
#   filter(outcome_id == "post_average") %>% 
#   rma(yi = estimate, 
#       sei = std.error, 
#       method = "FE", 
#       data = .)

df_personalize_out %>%
  group_by(outcome_id) %>%
  group_map(~ {
    model <- rma(yi = estimate,
                 sei = std.error,
                 method = "FE",
                 data = .x)
    
    broom::tidy(model) %>%
      mutate(outcome_id = .y$outcome_id)
  }) %>%
  bind_rows() %>% 
  saveRDS(., "output/processed_data/df_estimates_personalization_pw_mean.rds")

# 0... Convo vs. static ----

map(list_outcomes,
    function(.x) {
      
      # > Study 1 ----
      out_static_s1 <-
        study1 %>% 
        filter(d1.model=="gpt-4o",
               str_detect(d1.condition, "control", negate = T)) %>% 
        select(paste0("d1.", .x), 
               d1.post_average_followup, d1.model, d1.condition) %>% 
        pivot_longer(cols = contains("post")) %>% 
        drop_na() %>% 
        lm_robust(value ~ (d1.condition=="treat_dialogue")*(name=="d1.post_average_followup"), .) %>% 
        tidy() %>% 
        mutate(study = 1,
               outcome_id = .x)
      
      # > Study 3 ----
      
      out_static_s3 <-
        study3 %>% 
        filter(model == "gpt-4.5",
               post_train == "base") %>% 
        lm_robust(reformulate(c('(condition=="treatment-1")', "pre_average"),
                              response = .x), .) %>% 
        tidy() %>% 
        mutate(study = 3,
               outcome_id = .x)
      
      # > Join ----
      bind_rows(
        out_static_s1,
        out_static_s3
      )
      
    }) %>% 
  bind_rows() %>% 
  saveRDS("output/processed_data/df_estimates_static.rds")


map(c("post_average", "post_average_followup"),
    function(.x) {
      
      bind_rows(
        study1 %>% 
          filter(d1.model=="gpt-4o",
                 !is.na(d1.post_average), !is.na(d1.post_average_followup)) %>% 
          lm_robust(reformulate(
            c("d1.models_vs_control", "d1.pre_average"), response = paste0("d1.", .x)
          ), .) %>% 
          tidy() %>% 
          mutate(dataset = "S1 chat 1"),
        
        study1 %>% 
          filter(!is.na(d2.post_average), !is.na(d2.post_average_followup)) %>% 
          lm_robust(reformulate(
            c("d2.condition", "d2.pre_average"), response = paste0("d2.", .x)
          ), .) %>% 
          tidy() %>% 
          mutate(dataset = "S1 chat 2")
      ) %>% 
        mutate(outcome_id = .x)
      
      
    }) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "Intercept|pre_average", negate=T)) %>% 
  saveRDS("output/processed_data/df_estimates_durability.rds")




# 1... Persuasion ~ prompts vs. none ----

f <- function(list_data, outcome) {
  
  stopifnot(unique(list_data[[1]]$study) == 1)
  stopifnot(unique(list_data[[2]]$study) == 2)
  stopifnot(unique(list_data[[3]]$study) == 3)
  
  out_prompt_vs_none <- bind_rows(
    
    list_data[[1]] %>% 
      filter(d1.condition == "treat_dialogue") %>% 
      mutate(d1.prompt_rhetoric_id = fct_relevel(d1.prompt_rhetoric_id, "none")) %>% 
      lm_robust(reformulate(
        c("d1.prompt_rhetoric_id", "d1.pre_average"), paste0("d1.", outcome)), # Formula
        data = .) %>% 
      tidy() %>% 
      mutate(dataset = "S1, chat 1"),
    
    list_data[[1]] %>% 
      filter(d2.condition == "treatment") %>% 
      mutate(d2.prompt_rhetoric_id = fct_relevel(d2.prompt_rhetoric_id, "none")) %>% 
      lm_robust(reformulate(
        c("d2.prompt_rhetoric_id", "d2.pre_average"), paste0("d2.", outcome)), # Formula
        data = .) %>% 
      tidy() %>% 
      mutate(dataset = "S1, chat 2"),
    
    list_data[[2]] %>% 
      filter(condition %in% c("treatment-1", "treatment-2"),
             post_train %in% c("base", "rm")) %>% 
      mutate(prompt_rhetoric_id = fct_relevel(prompt_rhetoric_id, "none")) %>% 
      lm_robust(reformulate(
        c("prompt_rhetoric_id", "pre_average"), outcome), # Formula
        data = .) %>% 
      tidy() %>% 
      mutate(dataset = "S2"),
    
    list_data[[3]] %>% 
      filter(condition %in% c("treatment-1", "treatment-2"),
             post_train %in% c("base", "rm")) %>% 
      mutate(prompt_rhetoric_id = fct_relevel(prompt_rhetoric_id, "none")) %>% 
      lm_robust(reformulate(
        c("prompt_rhetoric_id", "pre_average"), outcome), # Formula
        data = .) %>% 
      tidy() %>% 
      mutate(dataset = "S3")
    
    
  ) %>% 
    filter(str_detect(term, "(Intercept)|pre_average", negate = T)) %>% 
    mutate(term = str_replace_all(term, c("d1.prompt_rhetoric_id" = "",
                                          "d2.prompt_rhetoric_id" = "",
                                          "prompt_rhetoric_id" = "")))
  
  out_prompt_vs_none_summary <-
    out_prompt_vs_none %>%
    group_by(term) %>%
    summarise(model = list(rma(yi = estimate, sei = std.error, data = pick(everything()), method = "FE")),
              .groups = "drop") %>%
    mutate(tidy_out = map(model, tidy)) %>%
    unnest(tidy_out, names_sep = "_")
  
  list(
    "meta" = out_prompt_vs_none_summary,
    "raw_estimates" = out_prompt_vs_none
  )
  
}

out_prompts_vs_none <- map(list_outcomes, ~f(list(study1, study2, study3), .x))
names(out_prompts_vs_none) <- list_outcomes

# Write to file
imap(out_prompts_vs_none, ~.x$meta %>% mutate(outcome = .y)) %>% 
  bind_rows() %>% 
  saveRDS("output/processed_data/df_estimates_prompt_vs_none_meta.rds")

imap(out_prompts_vs_none, ~.x$meta %>% mutate(outcome = .y)) %>% 
  bind_rows() %>% 
  mutate(lwr = tidy_out_estimate-1.96*tidy_out_std.error,
         upr = tidy_out_estimate+1.96*tidy_out_std.error) %>% 
  filter(term %in% c("information", "mega"),
         outcome == "post_average")

imap(out_prompts_vs_none, ~.x$raw_estimates %>% mutate(outcome = .y)) %>% 
  bind_rows() %>% 
  saveRDS("output/processed_data/df_estimates_prompt_vs_none_raw.rds")
  

# 2... Persuasion ~ N facts / informed / avg. veracity, by prompts ----

# > Function to compute prompt means ----
f <- function(data, outcome, s_prefix) {
  
  prompt_rhetoric_id  <- sym(paste0(s_prefix, "prompt_rhetoric_id"))
  convo_n_facts_total <- sym(paste0(s_prefix, "convo_n_facts_total"))
  convo_mean_veracity <- sym(paste0(s_prefix, "convo_mean_veracity"))
  post_average        <- sym(paste0(s_prefix, outcome))
  pre_average         <- sym(paste0(s_prefix, "pre_average"))
  eval_2              <- sym(paste0(s_prefix, "evaluate_2"))
  eval_3              <- sym(paste0(s_prefix, "evaluate_3"))
  
  data %>%
    mutate(inform = (!!eval_2 + !!eval_3)/2) %>% 
    group_by(!!prompt_rhetoric_id) %>%
    summarise(
      mean_n_facts = mean(!!convo_n_facts_total, na.rm = TRUE),
      mean_veracity = mean(!!convo_mean_veracity, na.rm = TRUE),
      mean_inform = mean(inform, na.rm = TRUE),
      n = n(),
      sd_n_facts = sd(!!convo_n_facts_total, na.rm = TRUE),
      sd_veracity = sd(!!convo_mean_veracity, na.rm = TRUE),
      sd_inform = sd(inform, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      se_n_facts = sd_n_facts / sqrt(n),
      se_inform = sd_inform / sqrt(n),
      se_veracity = sd_veracity / sqrt(n),
      lwr_n_facts = mean_n_facts - 1.96 * se_n_facts,
      upr_n_facts = mean_n_facts + 1.96 * se_n_facts,
      lwr_inform = mean_inform - 1.96 * se_inform,
      upr_inform = mean_inform + 1.96 * se_inform,
      lwr_veracity = mean_veracity - 1.96 * se_veracity,
      upr_veracity = mean_veracity + 1.96 * se_veracity
    ) %>%
    left_join(
      data %>%
        lm_robust(as.formula(paste0(s_prefix, outcome, " ~ 0 + ", 
                                    s_prefix, "prompt_rhetoric_id + scale(", 
                                    s_prefix, "pre_average)")), data = .) %>%
        tidy() %>%
        filter(str_detect(term, "rhetoric")) %>%
        mutate(term = str_remove_all(term, paste0(s_prefix, "prompt_rhetoric_id"))) %>%
        rename(!!prompt_rhetoric_id := term),
      by = rlang::as_name(prompt_rhetoric_id)
    ) %>% 
    rename(prompt_id = !!prompt_rhetoric_id) %>% 
    pivot_longer(cols = c(mean_n_facts, mean_inform, mean_veracity),
                 names_to = "x_variable",
                 values_to = "x_value") %>%
    mutate(lwr = case_when(x_variable == "mean_inform" ~ lwr_inform,
                           x_variable == "mean_n_facts" ~ lwr_n_facts,
                           x_variable == "mean_veracity" ~ lwr_veracity),
           upr = case_when(x_variable == "mean_inform" ~ upr_inform,
                           x_variable == "mean_n_facts" ~ upr_n_facts,
                           x_variable == "mean_veracity" ~ upr_veracity),
           se_x = case_when(x_variable == "mean_inform" ~ se_inform,
                            x_variable == "mean_n_facts" ~ se_n_facts,
                            x_variable == "mean_veracity" ~ se_veracity))
  
}

# > Compute prompt means ----
df_prompt_means <-
  map(list_outcomes,
      function(.x) {
        
        bind_rows(
          
          f(data = study1 %>% 
              filter(d1.condition == "treat_dialogue"), 
            outcome = .x, 
            s_prefix = "d1.") %>% 
            mutate(dataset = "S1, chat 1"),
          
          f(data = study1 %>% 
              filter(d2.condition == "treatment"), 
            outcome = .x, 
            s_prefix = "d2.") %>% 
            mutate(dataset = "S1, chat 2"),
          
          f(data = study2 %>% 
              filter(condition %in% c("treatment-1", "treatment-2"),
                     post_train %in% c("base", "rm")), 
            outcome = .x, 
            s_prefix = "") %>% 
            mutate(dataset = "S2"), 
          
          f(data = study3 %>% 
              filter(condition %in% c("treatment-1", "treatment-2")),
            outcome = .x,
            s_prefix = "") %>%
            mutate(dataset = "S3")
          
        ) %>% 
          mutate(outcome_id = .x)
        
      }) %>% 
  bind_rows()

# Save to file
df_prompt_means %>% saveRDS("output/processed_data/df_prompt_means.rds")


# df_prompt_means %>% 
#   filter(x_variable == "mean_n_facts",
#          outcome_id == "post_average") %>% 
#   rma(
#     yi = estimate,
#     sei = std.error,
#     mods = ~ fct_relevel(prompt_id, "information", after = 0),
#     data = .,
#     method = "FE"
#   )

# > Analyze slopes ----

# >> Function to fit slopes ----
fun_fit_slope <- function(data, x_var, out_var) {
  
  data <-
    data %>%
    filter(x_variable == x_var,
           outcome_id == out_var)
  
  mod_slope <- 
    brm(
      estimate | se(std.error, sigma = TRUE) ~ me(x_value, se_x) + dataset,
      data = data,
      family = gaussian(),
      prior = c(
        prior(normal(50, 25), class = "Intercept"),      
        prior(normal(0, 4), class = "b"),
        prior(exponential(1), class = "sigma")
      ),
      control = list(adapt_delta = 0.8),
      chains = 4, cores = 4, 
      iter = 3000,
      seed = 42
    )
  
  mod_slope
  
}

# >> Fit slopes ----
out_slope_fits <-
  map(list_outcomes, 
      ~fun_fit_slope(data = df_prompt_means, 
                     x_var = "mean_n_facts", 
                     out_var = .x))

names(out_slope_fits) <- list_outcomes

# Write to file
imap(out_slope_fits,
     function(.x, .y) {
       
       as_draws_df(.x) %>% 
         select(bsp_mex_valuese_x) %>% 
         mean_qi() %>% 
         mutate(outcome = .y,
                param = "slope")
       
     }) %>% 
  bind_rows() %>% 
  saveRDS("output/processed_data/df_estimates_brm_slope.rds")

imap(out_slope_fits,
     function(.x, .y) {
       
       temp <- summary(.x)
       temp$fixed %>% 
         rownames_to_column(var = "Term") %>% 
         tibble() %>% 
         mutate(outcome = .y,
                param = "slope")
       
     }) %>% 
  bind_rows() %>% 
  saveRDS("output/processed_data/df_estimates_brm_slope_diagnostics.rds")


# > Analyze correlations ----

# >> Function to fit correlations ----
fun_fit_corr <- function(data, x_var, out_var) {
  
  data_long <-
    data %>%
    filter(x_variable == x_var,
           outcome_id == out_var) %>% 
    pivot_longer(cols = c(x_value, estimate),
                 names_to = "variable") %>% 
    mutate(sei = ifelse(variable == "estimate", std.error, se_x))
  
  mod_corr <- 
    brm(
      formula = value | se(sei, sigma = TRUE) ~ 0 + variable*dataset + (0 + variable | prompt_id),
      data = data_long,
      family = gaussian(),
      prior = c(
        prior(normal(0, 5), class = "b"),  
        prior(normal(50, 25), class = "b", coef = "variableestimate"),  
        if(str_detect(x_var, "facts")) { } else { 
          prior(normal(50, 25), class = "b", coef = "variablex_value") # Set prior for x value
        },
        prior(exponential(1), class = "sd"),           
        prior(lkj(1), class = "cor"),
        prior(exponential(1), class = "sigma")
      ),
      control = list(adapt_delta = 0.95),
      chains = 4, cores = 4,
      seed = 42
    )
  
  mod_corr
  
}

# >> Fit corrs ----
out_corr_fits <-
  map(list_outcomes, 
      ~fun_fit_corr(data = df_prompt_means, 
                    x_var = "mean_n_facts", 
                    out_var = .x))

names(out_corr_fits) <- list_outcomes

# Write to file
imap(out_corr_fits,
     function(.x, .y) {
       
       as_draws_df(.x) %>% 
         select(contains("cor")) %>% 
         mean_qi() %>% 
         mutate(outcome = .y,
                param = "corr")
       
     }) %>% 
  bind_rows() %>% 
  saveRDS("output/processed_data/df_estimates_brm_corr.rds")

imap(out_corr_fits,
     function(.x, .y) {
       
       temp <- summary(.x)
       bind_rows(
         temp$fixed %>% 
           rownames_to_column(var = "Term") %>% 
           tibble() %>% 
           mutate(outcome = .y,
                  param = "corr"),
         temp$random$prompt_id %>% 
           rownames_to_column(var = "Term") %>% 
           tibble() %>% 
           mutate(outcome = .y,
                  param = "corr")
         
       )
       
     }) %>% 
  bind_rows() %>% 
  saveRDS("output/processed_data/df_estimates_brm_corr_diagnostics.rds")

# 3... Persuasion / N facts / informed / veracity (avg. and threshold)  ~ info*model DiDs ----

f <- function(data, outcome_var) {
  
  data <-
    data %>% 
    mutate(info = ifelse(prompt_rhetoric_id %in% c("information"), 1, 0)) %>% 
    mutate(inform = (evaluate_2 + evaluate_3)/2)
  
  map(unique(data$info),
      function(.x) {
        
        # For attitude outcome compare vs. control
        if(str_detect(outcome_var, "post")) {
          
          data %>% 
            filter(info == .x) %>% 
            bind_rows(data %>% filter(condition=="control")) %>% 
            lm_robust(reformulate(c("models_vs_control", "scale(pre_average)"), response = outcome_var), .) %>% 
            tidy() %>% 
            mutate(info = .x)
          
          # For other outcomes do not
        } else {
          
          data %>% 
            filter(info == .x,
                   condition != "control") %>% 
            lm_robust(reformulate(c("models_vs_control"), response = outcome_var, intercept = F), .) %>% 
            tidy() %>% 
            mutate(info = .x)
          
        }
        
        
        
      }) %>% 
    bind_rows() %>% 
    filter(str_detect(term, "model")) %>% 
    mutate(term = str_remove_all(term, "models_vs_control"))
  
  
}

list_outcomes <- c(
  "post_average", "post_average_imputed_with_pre", 
  "convo_n_facts_total", "convo_mean_veracity", "convo_prop_facts_above_veracity_threshold"
)

# > Estimate ATEs ----
out_did_ates <-
  map(list_outcomes,
      function(.x) {
        
        bind_rows(
          
          f(study2 %>% 
              filter(condition %in% c("treatment-2", "control"),
                     model %in% c("gpt-4o", "gpt-4.5")),
            outcome_var = .x) %>% 
            mutate(study = "Study 2"),
          
          f(study3 %>% 
              filter(condition %in% c("treatment-1", "treatment-2", "control"),
                     model %in% c("gpt-4o", "gpt-4.5", "chatgpt-4o-latest", "grok-3")),
            outcome_var = .x) %>% 
            mutate(study = "Study 3")
          
        ) %>% 
          mutate(outcome_id = .x)
        
      }) %>% 
  bind_rows()

# Write to file
out_did_ates %>% saveRDS("output/processed_data/df_estimates_DiD_ates.rds")


# > Estimate DiD ----

f <- function(data, outcome_var) {
  
  data %>% 
    mutate(info = ifelse(prompt_rhetoric_id %in% c("information"), 1, 0)) %>% 
    mutate(inform = (evaluate_2 + evaluate_3)/2) %>% 
    lm_robust(reformulate(c('info*fct_relevel(model, "gpt-4o")', 
                            if(str_detect(outcome_var, "post_average")) { "pre_average" } else NULL), 
                          response = outcome_var), .) %>% 
    tidy()
  
}

# Estimate DiDs
out_dids <-
  map(list_outcomes,
      function(.x) {
        
        bind_rows(
          
          # Study 2
          f(study2 %>% filter(condition == "treatment-2", model %in% c("gpt-4o", "gpt-4.5")), 
            .x) %>% mutate(study = 2, model = "gpt-4.5"),
          
          # Study 3
          f(study3 %>% filter(condition == "treatment-1", model %in% c("gpt-4o", "gpt-4.5")), 
            .x) %>% mutate(study = 3, model = "gpt-4.5"),
          f(study3 %>% filter(condition == "treatment-1", model %in% c("gpt-4o", "chatgpt-4o-latest")), 
            .x) %>% mutate(study = 3, model = "chatgpt-4o-latest"),
          f(study3 %>% filter(condition %in% c("treatment-1", "treatment-2"), model %in% c("gpt-4o", "grok-3")), 
            .x) %>% mutate(study = 3, model = "grok-3")
          
        )
        
      }) %>% 
  bind_rows() %>% 
  filter(!(term %in% c("(Intercept)", "pre_average")))

# Write to file
out_dids %>% saveRDS("output/processed_data/df_estimates_DiD_interactions.rds")

# 4... Persuasion / N facts / veracity (avg. and threshold) ~ in-domain post-training (at individual model level) ----

# > ATEs ----

f <- function(data, outcome_var) {
  
  # model_id <- "llama-3-1-8b"
  # treat_id <- "treatment-1"
  # outcome_var <- "convo_n_facts_total"
  # study2 %>%
  #   mutate(model = ifelse(condition == "control", NA, model)) %>%
  #   filter(condition %in% c(treat_id, "control"),
  #          model == model_id | is.na(model)) %>%
  #   count(condition, model)
  
  # For attitude outcome, compare to control
  if(str_detect(outcome_var, "post_average")) {
    
    mod <-
      data %>% 
      mutate(post_train = ifelse(condition == "control", "aa_control", post_train)) %>% 
      lm_robust(reformulate(c('post_train', "scale(pre_average)"), response = outcome_var), .)
    
    # Otherwise (n facts, veracity etc.) do not  
  } else {
    
    mod <- 
      data %>% 
      filter(condition != "control") %>% 
      lm_robust(reformulate(c('post_train'), response = outcome_var, intercept = F), .)
    
  }
  
  mod %>% 
    tidy() %>% 
    filter(str_detect(term, "post_train")) %>% 
    mutate(term = str_remove_all(term, "post_train"))
  
}

list_outcomes <- c(
  "post_average", 
  "post_average_imputed_with_pre",
  "convo_n_facts_total",
  "convo_mean_veracity",
  "convo_prop_facts_above_veracity_threshold"
)

list_models <- 
  bind_rows(
    study2 %>% select(condition, model, study) %>% distinct() %>% filter(!(condition %in% c("control", "treatment-3"))),
    study3 %>% select(condition, model, study) %>% distinct() %>% filter(!(condition %in% c("control", "treatment-3")))
  ) %>% 
  expand_grid(outcome = list_outcomes)

out_pt_ates <- 
  map(1:nrow(list_models),
      function(.x) {
        
        study_id <- list_models[[.x,"study"]]
        treat_id <- list_models[[.x,"condition"]]
        model_id <- list_models[[.x,"model"]]
        out_id   <- list_models[[.x,"outcome"]]
        
        f(data = get(paste0("study", study_id)) %>% 
            mutate(model = ifelse(condition == "control", NA, model)) %>% 
            filter(condition %in% c(treat_id, "control"),
                   model == model_id | is.na(model)),
          outcome_var = out_id) %>% 
          mutate(study = study_id,
                 condition = treat_id,
                 model = model_id)
        
      }) %>% 
  bind_rows()

# Write to file
out_pt_ates %>% saveRDS("output/processed_data/df_estimates_post_train_vs_control_by_models.rds")

# > Difference vs. base ----

f <- function(data, outcome_var) {
  
  data %>% 
    lm_robust(reformulate(c("post_train", 
                            if(str_detect(outcome_var, "post_average")) { "pre_average" } else NULL), 
                          response = outcome_var), .) %>% 
    tidy() %>% 
    filter(str_detect(term, "post_train")) %>% 
    mutate(term = str_remove_all(term, "post_train"))
  
}

out_pt_vs_base <- 
  map(1:nrow(list_models),
      function(.x) {
        
        study_id <- list_models[[.x,"study"]]
        treat_id <- list_models[[.x,"condition"]]
        model_id <- list_models[[.x,"model"]]
        out_id   <- list_models[[.x,"outcome"]]
        
        f(data = get(paste0("study", study_id)) %>% 
            filter(condition == treat_id,
                   model == model_id),
          outcome_var = out_id) %>% 
          mutate(study = study_id,
                 condition = treat_id,
                 model = model_id)
        
      }) %>% 
  bind_rows()

# Write to file
out_pt_vs_base %>% saveRDS("output/processed_data/df_estimates_post_train_vs_base_by_models.rds")

# 5... Veracity / density ~ models / in-domain post-training ----

# > Models ----
f <- function(data, s_prefix, outcome_id) {
  
  if(s_prefix != "d2.") {
    
    data %>% 
      lm_robust(reformulate(c(paste0(s_prefix, "model")), 
                            response = paste0(s_prefix, outcome_id), 
                            intercept = F), .) %>% 
      tidy()
    
  } else {
    
    data %>% 
      lm_robust(formula = as.formula(paste0(s_prefix, outcome_id, " ~ 1")), .) %>% 
      tidy()
    
  }
  
  
}

out_mod_veracity_density <-
  map(c("convo_mean_veracity", "convo_n_facts_total", "convo_prop_facts_above_veracity_threshold"),
      function(.x) {
        
        bind_rows(
          
          f(study1 %>% filter(d1.condition=="treat_dialogue"), "d1.", .x) %>% 
            mutate(study = 1,
                   dialogue = 1),
          
          f(study1 %>% filter(d2.condition=="treatment"), "d2.", .x) %>% 
            mutate(study = 1,
                   dialogue = 2,
                   term = "gpt-4o"),
          
          f(study2 %>% filter(condition!="control", post_train=="base"), "", .x) %>% 
            mutate(study = 2),
          
          study2 %>% filter(condition=="treatment-3") %>% 
            lm_robust(formula = as.formula(paste0(.x, " ~ 1")), .) %>% 
            tidy() %>% 
            mutate(study = 2, 
                   term = "llama-3-1-405b-deceptive-info"),
          
          f(study3 %>% filter(condition!="control", condition!="treatment-3", post_train=="base"), "", .x) %>% 
            mutate(study = 3)
          
        ) %>% 
          mutate(term = str_remove_all(term, "model|d1.model"),
                 outcome_id = .x)
        
      }) %>% 
  bind_rows()


# Write to file
out_mod_veracity_density %>% saveRDS("output/processed_data/df_estimates_veracity_infodensity_by_models.rds")


# 6... Persuasion ~ developer post-training ----

list_outcomes <- list_outcomes[str_detect(list_outcomes, "post")]

# > Qwen chat ----
map(list_outcomes,
    function(.x) {
      
      study1 %>% 
        filter(str_detect(d1.model, "qwen-1-5-72b")) %>% 
        lm_robust(reformulate(c("d1.model", "d1.pre_average"), 
                              response = paste0("d1.", .x)), data = .) %>% 
        tidy() %>% 
        filter(str_detect(term, "72b")) %>% 
        mutate(term = str_remove_all(term, "d1.model"))
      
    }) %>% 
  bind_rows() %>% 
  saveRDS("output/processed_data/df_estimates_qwen_chat_vs_qwen.rds")

# > 4o-new ----
map(list_outcomes,
    function(.x) {
      
      study3 %>% 
        filter(
          condition %in% c("treatment-1"),
          str_detect(model, "gpt-4o")
        ) %>% 
        mutate(model = fct_relevel(model, "gpt-4o")) %>% 
        lm_robust(reformulate(c("model", "pre_average"), 
                              response = .x), data = .) %>% 
        tidy() %>% 
        filter(str_detect(term, "4o")) %>% 
        mutate(term = str_remove_all(term, "model"))
      
    }) %>% 
  bind_rows() %>% 
  saveRDS("output/processed_data/df_estimates_4o_new_vs_4o_old.rds")

# 7... Persuasion ~ model (base only) ----

map(list_outcomes,
    function(.x) {
      
      bind_rows(
        study1 %>% 
          lm_robust(reformulate(c("d1.models_vs_control", "d1.pre_average"), response = paste0("d1.", .x)), .) %>% 
          tidy() %>% 
          mutate(study = 1,
                 dialogue = 1),
        
        study1 %>% 
          lm_robust(reformulate(c("d2.condition", "d2.pre_average"), response = paste0("d2.", .x)), .) %>% 
          tidy() %>% 
          mutate(study = 1,
                 dialogue = 2),
        
        study2 %>% 
          mutate(models_vs_control = case_when(condition == "treatment-3" ~ "llama-3-1-405b-deceptive-info",
                                               T ~ models_vs_control)) %>% 
          filter(post_train == "base" | is.na(post_train)) %>% 
          lm_robust(reformulate(c("models_vs_control", "pre_average"), response = .x), .) %>% 
          tidy() %>% 
          mutate(study = 2),
        
        study3 %>% 
          filter(post_train == "base") %>% 
          lm_robust(reformulate(c("models_vs_control", "pre_average"), response = .x), .) %>% 
          tidy() %>% 
          mutate(study = 3)
      ) %>% 
        mutate(outcome_id = .x)
      
      
      
    }) %>% 
  bind_rows() %>% 
  filter(str_detect(term, "Intercept|pre_average", negate = T)) %>% 
  mutate(term = str_remove_all(term, "d1.|d2.|models_vs_control")) %>% 
  mutate(term = str_replace_all(term, "conditiontreatment", "gpt-4o")) %>% 
  mutate(term = case_when(term == "treat_static" & study == 1 ~ "gpt-4o-static",
                          term == "treat_static" & study == 3 ~ "gpt-4.5-static",
                          T ~ term)) %>% 
  saveRDS("output/processed_data/df_estimates_all_base_models_vs_control.rds")


