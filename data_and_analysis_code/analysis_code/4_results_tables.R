
# library(tidyverse)
# library(broom)
# library(estimatr)
# library(brms)
# library(metafor)
# library(patchwork)
# library(tidybayes)
# library(ggrepel)
# library(cowplot)
# library(kableExtra)

df_list <- map(
  list.files("output/processed_data"),
  function(.x) {
    readRDS(paste0("output/processed_data/", .x))
  }
)

names(df_list) <- list.files("output/processed_data") %>% str_remove_all(".rds")

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
  

model_names <- 
  c(
    "gpt-4.5" = "GPT-4.5",
    "chatgpt-4o-latest" = "GPT-4o (3/25)",
    "gpt-4o" = "GPT-4o (8/24)",
    "grok-3" = "Grok-3",
    "gpt-3.5" = "GPT-3.5",
    "llama-3-1-8b" = "Llama3.1-8b",
    "llama-3-1-405b" = "Llama3.1-405b",
    "treat_static" = "Static message",
    "aa_control" = "Control",
    "qwen" = "Qwen",
    "control" = "Control",
    "treatment" = "Treatment",
    "generic" = "Generic",
    "personalized" = "Personalized"
  )

pt_names <-
  c(
    "base" = "Base",
    "sft" = "SFT",
    "rm" = "RM",
    "_and_" = " + "
  )

outcome_names <-
  c(
    "post_average_imputed_with_pre" = "Policy attitude (with post-treatment missing values imputed with pre-treatment values)",
    "post_average" = "Policy attitude (main persuasion outcome)",
    "convo_n_facts_total" = "Information density (N claims)",
    "convo_mean_veracity" = "Accuracy (0-100 scale)",
    "convo_prop_facts_above_veracity_threshold" = "Accuracy (>50/100 on the scale)"
  )

tuning_names <-
  c(
    "full" = "All models",
    "chat_pt" = "Chat-tuned models",
    "dev_pt" = "Developer-tuned models"
  )

fun_round_fix_p <- function(table) {
  
  table %>% 
    mutate(`p.value` = ifelse(`p.value` <.001, "<.001", as.character(round(`p.value`, 3)))) %>% 
    mutate(across(where(is.numeric), ~round(.x, 2)))
  
}

fp <- "output/supplement/results_tables/"

# 1. Static vs. dialogue + durability ----

cond_names <-
  c(
  `d1.condition == "treat_dialogue"TRUE` = "dialogue (vs. static)",
  `name == "d1.post_average_followup"TRUE` = "time",
  `condition == "treatment-1"TRUE` = "dialogue (vs. static)",
  ":" = " x "
)

combinations <- 
  expand.grid(
    study = unique(df_list$df_estimates_static$study),
    outcome_id = unique(df_list$df_estimates_static$outcome_id)
)

map2(
  combinations$study,
  combinations$outcome_id,
  function(.x, .y) {
    
    out <-
      df_list$df_estimates_static %>%
      filter(study == .x, 
             outcome_id == .y, 
             !(term %in% c("pre_average", "(Intercept)"))) %>% 
      mutate(term = str_replace_all(term, cond_names)) %>% 
      select(-contains("outcome"), -study) %>% 
      fun_round_fix_p()
    
    # write_csv(out, 
    #           paste0(fp, "1_static_and_durability/study", .x, "__", .y, ".csv"))
    
    out %>%
      fun_my_kable(
        my_caption = paste0("Direct comparisons. Study ", .x, if(.x==1) { " Chat 1" } else NULL,
                            ". Outcome: ", str_replace_all(.y, outcome_names), "."),
        my_file = paste0(fp, "1_static_and_durability/study", .x, "__", .y, ".txt"),
        my_footnote = "Estimates are in percentage points."
      )
      # kable(
      #   "latex", 
      #   booktabs = TRUE, 
      #   caption = paste0("Study ", .x, ". Outcome: ", str_replace_all(.y, outcome_names), ".")
      # ) %>%
      # kable_styling(latex_options = c("hold_position", "striped")) %>% 
      # cat(file = paste0(fp, "1_static_and_durability/study", .x, "__", .y, ".txt"))
    
  }
)

df_list$df_estimates_durability %>% 
  mutate(term = str_remove_all(term, "d1.models_vs_control|d2.condition"),
         time = ifelse(outcome_id=="post_average", 0, 1)) %>% 
  mutate(term = str_replace_all(term, model_names)) %>% 
  mutate(term = ifelse(term=="Treatment", "GPT-4o (8/24)", term)) %>% 
  select(-c(outcome, outcome_id)) %>% 
  rename(study = dataset) %>% 
  fun_round_fix_p() %>% 
  arrange(study, term) %>% 
  fun_my_kable(
    my_caption = paste0("Persuasion effects (vs. control) of dialogue and static messaging, immediately post-treatment (time = 0) and +1 month later (time = 1). ",
                        "Outcome: Policy attitude (main persuasion outcome)."),
    my_file = paste0(fp, "1_static_and_durability/ates_durability.txt"),
    my_footnote = "Estimates are in percentage points."
  )

map(
  list.files(paste0(fp, "1_static_and_durability/")) %>% discard(~ str_detect(.x, "joint")),
  ~paste(readLines(paste0(fp, "1_static_and_durability/", .x)), collapse = "\n")
) %>% 
  paste0(collapse = "\n\n\\vspace{3em}\n\n") %>% 
  cat(file = paste0(fp, "1_static_and_durability/joint/JOINT_FILE.txt"))

# 2. Scaling curve: persuasion ----

combinations <- 
  expand.grid(
    data_label = unique(df_list$df_estimates_scaling_curve$data_label),
    outcome = unique(df_list$df_estimates_scaling_curve$outcome)
  )

map2(
  combinations$data_label,
  combinations$outcome,
  function(.x, .y) {
    
    # .x <- "full"
    # .y <- "post_average"
    
    df_temp <- 
      df_list$df_estimates_scaling_curve %>% 
      filter(data_label==.x, outcome==.y)
    
    # OLS estimates
    if(.x == "full") {
      df_temp %>% 
        select(ols_dialogue) %>% 
        unnest(ols_dialogue) %>% 
        select(model:study) %>% 
        select(-outcome) %>% 
        mutate(model = str_replace_all(model, model_names)) %>% 
        fun_round_fix_p() %>% 
        #write_csv(paste0(fp, "2_scaling_curve_persuasion/ols_estimates__", .y, ".csv"))
        fun_my_kable(
          my_caption = paste0("OLS estimates (base models only). Outcome: ", str_replace_all(.y, outcome_names), "."),
          my_file = paste0(fp, "2_scaling_curve_persuasion/1_ols_estimates__", .y, ".txt"),
          my_footnote = "Estimates are in percentage points."
        )
        # kable(
        #   "latex", 
        #   booktabs = TRUE, 
        #   caption = paste0("OLS estimates. Outcome: ", str_replace_all(.y, outcome_names), ".")
        # ) %>%
        # kable_styling(latex_options = c("hold_position", "striped")) %>% 
        # cat(file = paste0(fp, "2_scaling_curve_persuasion/1_ols_estimates__", .y, ".txt"))
      
    }
    
    # Meta-reg estimates
    out <-
      df_temp %>% 
      pull(metareg_lin) %>% 
      .[[1]] %>% 
      summary()
    
    out$fixed %>%
      rownames_to_column(var = "Term") %>% 
      as_tibble() %>% 
      mutate(across(where(is.numeric), ~format(round(.x, 2), nsmall = 2)),
             Term = ifelse(str_detect(Term, "log10"), "log10(flops)", Term)) %>% 
      # write_csv(
      #   paste0(fp, "2_scaling_curve_persuasion/linear_metareg_estimates__", .x, "__", .y, ".csv")
      # )
      fun_my_kable(
        my_caption = paste0("Meta-regression output. Models: ", str_replace_all(.x, tuning_names), 
                            ". Outcome: ", str_replace_all(.y, outcome_names), "."),
        my_file = paste0(fp, "2_scaling_curve_persuasion/2_linear_metareg_estimates__", .x, "__", .y, ".txt"),
        my_footnote = "Estimates are in percentage points. ESS = effective sample size of the posterior distribution."
      )
      # kable(
      #   "latex", 
      #   booktabs = TRUE, 
      #   caption = paste0("Meta-regression output. Models: ", str_replace_all(.x, tuning_names), 
      #                    ". Outcome: ", str_replace_all(.y, outcome_names), ".")
      # ) %>%
      # kable_styling(latex_options = c("hold_position", "striped")) %>% 
      # cat(file = paste0(fp, "2_scaling_curve_persuasion/2_linear_metareg_estimates__", .x, "__", .y, ".txt"))
    
    # LOO-CV
    df_temp %>% 
      pull(loo) %>% 
      .[[1]] %>% 
      data.frame() %>% 
      rownames_to_column(var = "model") %>% 
      mutate(model = ifelse(model == "mdl_lin", "Linear", "GAM")) %>% 
      mutate(across(where(is.numeric), ~format(round(.x, 2), nsmall = 2))) %>% 
      # write_csv(
      #   paste0(fp, "2_scaling_curve_persuasion/loocv__", .x, "__", .y, ".csv")
      # )
      fun_my_kable(
        my_caption = paste0(
          "Leave-one-out cross-validation comparing linear and nonlinear (GAM) meta-regressions. ", 
          "Models: ", str_replace_all(.x, tuning_names), 
           ". Outcome: ", str_replace_all(.y, outcome_names), "."),
        my_file = paste0(fp, "2_scaling_curve_persuasion/3_loocv__", .x, "__", .y, ".txt"),
        my_footnote = "ELPD = expected log pointwise predictive density. LOO = leave-one-out."
      )
      # kable(
      #   "latex", 
      #   booktabs = TRUE, 
      #   caption = paste0("LOO-CV. Models: ", str_replace_all(.x, tuning_names), 
      #                    ". Outcome: ", str_replace_all(.y, outcome_names), ".")
      # ) %>%
      # kable_styling(latex_options = c("hold_position", "striped")) %>% 
      # cat(file = paste0(fp, "2_scaling_curve_persuasion/3_loocv__", .x, "__", .y, ".txt"))
    
  }
)

# Interaction curve
map(unique(df_list$df_estimates_scaling_curve_interaction$outcome),
    function(.x) {
      out <-
        df_list$df_estimates_scaling_curve_interaction %>% 
        filter(outcome == .x) %>% 
        pull(metareg_lin) %>% 
        .[[1]] %>% 
        summary()
      
      out$fixed %>%
        rownames_to_column(var = "Term") %>% 
        as_tibble() %>% 
        mutate(across(where(is.numeric), ~format(round(.x, 2), nsmall = 2)),
               Term = str_replace_all(Term, c("log10flops_1e21_epoch" = "log10(flops)",
                                              "dev_pt1" = "Developer-tuned",
                                              ":" = " x "))) %>% 
        # write_csv(
        #   paste0(fp, "2_scaling_curve_persuasion/linear_metareg_estimates_interaction__", .x, ".csv")
        # )
        fun_my_kable(
          my_caption = paste0("Meta-regression output: Interaction between developer-tuned models and FLOPs", 
                              ". Outcome: ", str_replace_all(.x, outcome_names), "."),
          my_file = paste0(fp, "2_scaling_curve_persuasion/4_linear_metareg_estimates_interaction__", .x, ".txt"),
          my_footnote = "Estimates are in percentage points."
        )
        # kable(
        #   "latex", 
        #   booktabs = TRUE, 
        #   caption = paste0("Meta-regression output: Interaction", ". Outcome: ", str_replace_all(.x, outcome_names), ".")
        # ) %>%
        # kable_styling(latex_options = c("hold_position", "striped")) %>% 
        # cat(file = paste0(fp, "2_scaling_curve_persuasion/4_linear_metareg_estimates_interaction__", .x, ".txt"))
    })

map(
  list.files(paste0(fp, "2_scaling_curve_persuasion/")) %>% discard(~ str_detect(.x, "joint")),
  ~paste(readLines(paste0(fp, "2_scaling_curve_persuasion/", .x)), collapse = "\n")
) %>% 
  paste0(collapse = "\n\n\\vspace{3em}\n\n") %>% 
  cat(file = paste0(fp, "2_scaling_curve_persuasion/joint/JOINT_FILE.txt"))


# > 4o new vs. others ----
out <-
  df_list$df_estimates_4o_new_vs_4o_old %>% 
  fun_round_fix_p() %>% 
  mutate(term = str_replace_all(term, model_names))

map(out$outcome, 
    ~out %>% 
      filter(outcome==.x) %>% 
      select(-outcome) %>% 
      #write_csv(paste0(fp, "3_4o_new_vs_others/4o_new_vs_old_s3__", .x, ".csv"))
      fun_my_kable(
        my_caption = paste0("GPT-4o (3/25) vs.GPT-4o (8/24) (collapsed across all study 3 conditions)", 
                            ". Outcome: ", str_replace_all(.x, outcome_names), "."),
        my_file = paste0(fp, "3_4o_new_vs_others/4o_new_vs_old_s3__", .x, ".txt"),
        my_footnote = "Estimates are in percentage points."
      )
      # kable(
      #   "latex", 
      #   booktabs = TRUE, 
      #   caption = paste0("GPT-4o (3/25) vs.GPT-4o (8/24)", ". Outcome: ", str_replace_all(.x, outcome_names), ".")
      # ) %>%
      # kable_styling(latex_options = c("hold_position", "striped")) %>% 
      # cat(file = paste0(fp, "3_4o_new_vs_others/4o_new_vs_old_s3__", .x, ".txt"))
)

df_list$df_estimates_4o_new_vs_others_base_only %>% 
  fun_round_fix_p() %>% 
  mutate(term = str_remove_all(term, "fct_relevel\\(model, \"chatgpt-4o-latest\"\\)")) %>% 
  mutate(term = str_replace_all(term, model_names)) %>% 
  filter(!(term %in% c("(Intercept)", "pre_average"))) %>% 
  select(-outcome, -study) %>% 
  #write_csv(paste0(fp, "3_4o_new_vs_others/4o_new_vs_others_base_only__post_average.csv"))
  fun_my_kable(
    my_caption = paste0("GPT-4o (3/25) vs. other base models in study 3 (restricted to base models only)", 
                        ". Outcome: ", outcome_names[["post_average"]], "."),
    my_file = paste0(fp, "3_4o_new_vs_others/4o_new_vs_others_base_only__post_average.txt"),
    my_footnote = "Estimates are in percentage points."
  )
  # kable(
  #   "latex", 
  #   booktabs = TRUE, 
  #   caption = paste0("GPT-4o (3/25) vs. other base models", ". Outcome: ", outcome_names[["post_average"]], ".")
  # ) %>%
  # kable_styling(latex_options = c("hold_position", "striped")) %>% 
  # cat(file = paste0(fp, "3_4o_new_vs_others/4o_new_vs_others_base_only__post_average.txt"))

map(
  list.files(paste0(fp, "3_4o_new_vs_others/")) %>% discard(~ str_detect(.x, "joint")),
  ~paste(readLines(paste0(fp, "3_4o_new_vs_others/", .x)), collapse = "\n")
) %>% 
  paste0(collapse = "\n\n\\vspace{3em}\n\n") %>% 
  cat(file = paste0(fp, "3_4o_new_vs_others/joint/JOINT_FILE.txt"))

# 3. PPT ----

# > SFT * RM interaction ----
df_list$df_estimates_sft_rm_interaction %>% 
  mutate(term = str_replace_all(term, pt_names)) %>% 
  mutate(term = str_replace_all(term, c(":" = " x "))) %>% 
  select(-outcome) %>% 
  fun_round_fix_p() %>% 
  fun_my_kable(
    my_caption = paste0("No significant interaction between SFT and RM in Study 2", 
                        ". Outcome: ", outcome_names[["post_average"]], "."),
    my_file = paste0(fp, "4_PPT/1_ppt_interaction_effect.txt"),
    my_footnote = "RM = reward modeling; SFT = supervised fine-tuning."
  )

# > Main effects ----
combinations <- df_list$df_estimates_post_train_main_fx %>% distinct(outcome)

map(
  combinations$outcome,
  function(.x) {
    
    out <-
      df_list$df_estimates_post_train_main_fx
    
    if(.x == "convo_prop_facts_above_veracity_threshold"){
      out <-
        out %>% 
        mutate(across(c(estimate, std.error, conf.low, conf.high), ~.x*100))
    }
    
    out <-
      out %>%
      filter(outcome == .x) %>% 
      mutate(term = str_replace_all(term, pt_names)) %>% 
      select(-contains("outcome")) %>% 
      fun_round_fix_p() %>% 
      rename(`model type` = type)
    
    # write_csv(out, 
    #           paste0(fp, "4_PPT/ppt_main_effects__", .x, ".csv"))
    
    out %>% 
      fun_my_kable(
        my_caption = paste0("PPT main effects (i.e., vs. Base model)", ". Outcome: ", outcome_names[[.x]], "."),
        my_file = paste0(fp, "4_PPT/1_ppt_main_effects__", .x, ".txt"),
        my_footnote = "RM = reward modeling; SFT = supervised fine-tuning."
      )
    
  }
)

# > PW mean of main fx for developer models ----
out <- df_list$df_estimates_post_train_main_fx_pw_mean

map(out$outcome_id,
    ~out %>% 
      filter(outcome_id==.x) %>% 
      select(-c(outcome_id, term, type)) %>% 
      fun_round_fix_p() %>% 
      fun_my_kable(
        my_caption = paste0("PPT main effects (i.e., vs. Base model): precision-weighted mean across studies for Developer models", 
                            ". Outcome: ", outcome_names[[.x]], "."),
        my_file = paste0(fp, "4_PPT/2_ppt_main_effects_pw_mean_developer_models_", .x, ".txt"),
        my_footnote = "Estimates are in percentage points."
      )
      #write_csv(paste0(fp, "4_PPT/ppt_main_effects_pw_mean_developer_models_", .x, ".csv"))
)

# > Post-training persuasion effects vs. control (by model) ----
out <- df_list$df_estimates_post_train_vs_control_by_models

map(out$outcome %>% unique %>% .[1:2],
    function(.x){
      
      df_list$df_estimates_post_train_vs_control_by_models %>% 
        filter(outcome==.x) %>% 
        select(-c(outcome, condition)) %>% 
        mutate(term = str_replace_all(term, pt_names),
               model = str_replace_all(model, model_names)) %>% 
        fun_round_fix_p() %>% 
        fun_my_kable(
          my_caption = paste0("PPT persuasion effects vs. control group", ". Outcome: ", outcome_names[[.x]], "."),
          my_file = paste0(fp, "4_PPT/3_ppt_condition_effects_vs_control__", .x, ".txt"),
          my_footnote = "Estimates are in percentage points. RM = reward modeling; SFT = supervised fine-tuning."
        )
        #write_csv(paste0(fp, "4_PPT/ppt_condition_effects_vs_control__", .x, ".csv"))
      
    })

map(
  list.files(paste0(fp, "4_PPT/")) %>% discard(~ str_detect(.x, "joint")),
  ~paste(readLines(paste0(fp, "4_PPT/", .x)), collapse = "\n")
) %>% 
  paste0(collapse = "\n\n\\vspace{3em}\n\n") %>% 
  cat(file = paste0(fp, "4_PPT/joint/JOINT_FILE.txt"))


# 4. Personalization ----
combinations <- 
  expand.grid(
    study = unique(df_list$df_estimates_personalization$study),
    outcome_id = unique(df_list$df_estimates_personalization$outcome_id)
  )

map2(combinations$study, 
     combinations$outcome_id,
      function(.x, .y) {
      
        # .x <- "2"
        # .y <- "post_average"
        # .y <- "post_average_imputed_with_pre"
        
        df_list$df_estimates_personalization %>%
          filter(study == .x,
                 outcome_id == .y) %>%
          select(where(~ !all(is.na(.))), -contains("outcome"), -term, -study) %>%
          mutate(ppt_type = str_replace_all(ppt_type, pt_names)) %>%
          rename(`model type` = model_type,
                 `PPT` = ppt_type) %>%
          fun_round_fix_p() %>%
          fun_my_kable(
            my_caption = paste0("Effect of personalization (vs. generic).", " Study: ", .x,
                                ". Outcome: ", str_replace_all(.y, outcome_names), "."),
            my_file = paste0(fp, "5_personalization/1_study", .x, "__", .y, ".txt"),
            my_footnote = paste0(
              "Estimates are in percentage points. ", 
              "RM = reward modeling; SFT = supervised fine-tuning."
          ))
        # write_csv(paste0(fp, "5_personalization/study", .x, "__", .y, ".csv"))
      
        # .x
        # outcome_names[[.y]]
        # .y
        
    })

# PW means
map(combinations$outcome_id %>% unique,
    ~ df_list$df_estimates_personalization_pw_mean %>% 
      filter(outcome_id==.x) %>% 
      select(-c(term, type, outcome_id)) %>% 
      fun_round_fix_p() %>% 
      fun_my_kable(
        my_caption = paste0("Effect of personalization (vs. generic).", 
                            " Precision-weighted mean across studies. Outcome: ", str_replace_all(.x, outcome_names), "."),
        my_file = paste0(fp, "5_personalization/2_pw_mean__", .x, ".txt"),
        my_footnote = "Estimates are in percentage points."
      ))

map(
  list.files(paste0(fp, "5_personalization/")) %>% discard(~ str_detect(.x, "joint")),
  ~paste(readLines(paste0(fp, "5_personalization/", .x)), collapse = "\n")
) %>% 
  paste0(collapse = "\n\n\\vspace{3em}\n\n") %>% 
  cat(file = paste0(fp, "5_personalization/joint/JOINT_FILE.txt"))


# 5. Info density ----

# > Prompts vs. none ----
df_list$df_estimates_prompt_vs_none_raw

combinations <- 
  expand.grid(
    dataset = unique(df_list$df_estimates_prompt_vs_none_raw$dataset),
    outcome_id = unique(df_list$df_estimates_prompt_vs_none_raw$outcome)
  )

map2(combinations$dataset, 
     combinations$outcome_id,
     function(.x, .y) {
       
       df_list$df_estimates_prompt_vs_none_raw %>% 
         filter(dataset == .x,
                outcome == .y) %>% 
         select(-c(outcome, dataset)) %>% 
         fun_round_fix_p() %>% 
         fun_my_kable(
           my_caption = paste0("Effect of prompt (vs. basic prompt). Study: ", .x, 
                               ". Outcome: ", str_replace_all(.y, outcome_names), "."),
           my_file = paste0(fp, "6_information_density/prompts/1_prompt_vs_none__", 
                            str_replace_all(.x, c("," = "", " " = "_")), "__", .y, ".txt"),
           my_footnote = "Estimates are in percentage points."
         )
         # write_csv(paste0(fp, "6_information_density/prompts/prompt_vs_none__", 
         #                  str_replace_all(.x, c("," = "", " " = "_")), "__", .y, ".csv"))
       
     })

map(combinations$outcome_id %>% unique,
    function(.x) {
      
      #.x <- "post_average_imputed_with_pre"
      
      df_list$df_estimates_prompt_vs_none_meta %>% 
        filter(outcome == .x) %>% 
        select(-model, -tidy_out_term, -tidy_out_type, -outcome) %>% 
        rename_with(~ str_remove(.x, "tidy_out_")) %>% 
        fun_round_fix_p() %>% 
        fun_my_kable(
          my_caption = paste0("Effect of prompt (vs. basic prompt). Precision-weighted mean across studies", 
                              ". Outcome: ", str_replace_all(.x, outcome_names), "."),
          my_file = paste0(fp, "6_information_density/prompts/2_prompt_vs_none__meta__", .x, ".txt"),
          my_footnote = "Estimates are in percentage points."
        )
        #write_csv(paste0(fp, "6_information_density/prompts/prompt_vs_none__meta__", .x, ".csv"))
      
    })


# > Prompts: persuasion vs. density, means ----
out <- 
  df_list$df_prompt_means %>% 
  filter(x_variable=="mean_n_facts") %>% 
  select(prompt_id, estimate, std.error, x_value, se_x, dataset, outcome_id) %>% 
  rename(`Mean policy attitude` = estimate,
         `SE policy attitude` = `std.error`,
         `Mean N claims` = x_value,
         `SE N claims` = `se_x`,
         `Prompt` = prompt_id)

map2(combinations$dataset,
     combinations$outcome_id,
     function(.x, .y) {
       
       out %>% 
         filter(dataset == .x,
                outcome_id == .y) %>% 
         select(-dataset, -outcome_id) %>% 
         mutate(across(where(is.numeric), ~round(.x, 2))) %>% 
         fun_my_kable(
           my_caption = paste0("Prompt means.", " Study: ", .x, 
                               ". Outcome: ", str_replace_all(.y, outcome_names), "."),
           my_file = paste0(fp, "6_information_density/prompts/3_persuasion_vs_density_means__", 
                            str_replace_all(.x, c("," = "", " " = "_")), "__", .y, ".txt"),
           my_footnote = "Estimates are in percentage points."
         )
         # write_csv(paste0(fp, "6_information_density/prompts/persuasion_vs_density_means__", 
         #                  str_replace_all(.x, c("," = "", " " = "_")), "__", .y, ".csv"))
       
     })

# > Prompts: persuasion vs. density, associations ----
df_list$df_estimates_brm_corr_diagnostics
df_list$df_estimates_brm_slope_diagnostics

map(combinations$outcome_id %>% unique, 
    function(.x) {
      
      df_list$df_estimates_brm_corr_diagnostics %>% 
        filter(outcome == .x) %>% 
        mutate(Term = str_replace_all(Term, c("variable" = "", 
                                              "dataset" = "", 
                                              ":" = " x ", 
                                              "estimate" = "attitude",
                                              "x_value" = "N claims"))) %>% 
        mutate(Parameter = ifelse(str_detect(Term, "sd|cor"), "random", "fixed")) %>% 
        select(-c(outcome, param)) %>% 
        mutate(across(where(is.numeric), ~format(round(.x, 2), nsmall = 2))) %>% 
        fun_my_kable(
          my_caption = paste0(
            "Bayesian model output: Estimating the disattenuated correlation between N claims and attitudes (across prompts)", 
            ". Outcome: ", str_replace_all(.x, outcome_names), "."),
          my_file = paste0(fp, "6_information_density/prompts/4_persuasion_vs_density_disattenuated_correlation__", .x, ".txt"),
          my_footnote = "ESS = effective sample size of the posterior distribution."
        )
        # write_csv(
        #   paste0(fp, "6_information_density/prompts/persuasion_vs_density_disattenuated_correlation__", .x, ".csv")
        # )
      
      df_list$df_estimates_brm_slope_diagnostics %>% 
        filter(outcome == .x) %>% 
        mutate(Term = str_replace_all(Term, c("dataset" = "", 
                                              "mex_valuese_x" = "n claims"))) %>% 
        select(-c(outcome, param)) %>% 
        mutate(across(where(is.numeric), ~format(round(.x, 2), nsmall = 2))) %>% 
        fun_my_kable(
          my_caption = paste0(
            "Bayesian model output: Estimating the disattenuated slope of N claims on attitudes (across prompts)", 
            ". Outcome: ", str_replace_all(.x, outcome_names), "."),
          my_file = paste0(fp, "6_information_density/prompts/4_persuasion_vs_density_disattenuated_slope__", .x, ".txt"),
          my_footnote = "ESS = effective sample size of the posterior distribution."
        )
        # write_csv(
        #   paste0(fp, "6_information_density/prompts/persuasion_vs_density_disattenuated_slope__", .x, ".csv")
        # )
      
    })

map(
  list.files(paste0(fp, "6_information_density/prompts/")) %>% discard(~ str_detect(.x, "joint")),
  ~paste(readLines(paste0(fp, "6_information_density/prompts/", .x)), collapse = "\n")
) %>% 
  paste0(collapse = "\n\n\\vspace{3em}\n\n") %>% 
  cat(file = paste0(fp, "6_information_density/prompts/joint/JOINT_FILE.txt"))

# > DiDs: persuasion, density, accuracy ----
df_list$df_estimates_DiD_ates
df_list$df_estimates_DiD_interactions

combinations <- 
  expand.grid(
    study = unique(df_list$df_estimates_DiD_ates$study),
    outcome_id = unique(df_list$df_estimates_DiD_ates$outcome_id)
  )

map2(combinations$study,
     combinations$outcome_id,
     function(.x, .y) {
       
       out <- df_list$df_estimates_DiD_ates
       
       if(str_detect(.y, "prop")) {
         out <- out %>% mutate(across(c(estimate, `std.error`, `conf.low`, `conf.high`), ~.x*100))
       }
       
       out %>% 
         filter(study == .x,
                outcome_id == .y) %>% 
         select(-study, -outcome_id, -outcome) %>% 
         rename(`info prompt?` = info) %>% 
         mutate(term = str_replace_all(term, model_names)) %>% 
         fun_round_fix_p() %>% 
         fun_my_kable(
           my_caption = paste0("Model estimates under information prompt or other prompt. ", 
                               "Study: ", str_replace_all(.x, c("Study " = "S")), 
                               ". Outcome: ", str_replace_all(.y, outcome_names), "."),
           my_file = paste0(fp, "6_information_density/DiDs/ATEs__", 
                            str_replace_all(.x, c("Study " = "S")), "__", .y, ".txt"),
           my_footnote = "1 = information prompt; 0 = any other prompt."
         )
         # write_csv(paste0(fp, "6_information_density/DiDs/ATEs__", 
         #                  str_replace_all(.x, c("Study " = "S")), "__", .y, ".csv"))
       
     })

combinations <- 
  expand.grid(
    model = unique(df_list$df_estimates_DiD_interactions$model),
    outcome_id = unique(df_list$df_estimates_DiD_interactions$outcome)
  )

map2(combinations$model,
     combinations$outcome_id,
     function(.x, .y) {
       
       out <- df_list$df_estimates_DiD_interactions
       
       if(str_detect(.y, "prop")) {
         out <- out %>% mutate(across(c(estimate, `std.error`, `conf.low`, `conf.high`), ~.x*100))
       }
       
       out %>% 
         filter(model == .x,
                outcome == .y) %>% 
         select(-model, -outcome) %>% 
         mutate(term = str_remove_all(term, "fct_relevel\\(model, \"gpt-4o\"\\)")) %>% 
         mutate(term = str_replace_all(term, model_names)) %>% 
         mutate(term = str_replace_all(term, c("info" = "Info prompt", ":" = " x "))) %>% 
         fun_round_fix_p() %>% 
         fun_my_kable(
           my_caption = paste0(
             "Estimating the interaction between the listed model (vs. GPT-4o 8/24) and information prompt (vs. other prompt). ", 
              "Model: ", str_replace_all(.x, model_names), 
              ". Outcome: ", str_replace_all(.y, outcome_names), "."),
           my_file = paste0(fp, "6_information_density/DiDs/interactions__", .x, "_vs_4o__", .y, ".txt"),
           my_footnote = ""
         )
         # write_csv(paste0(fp, "6_information_density/DiDs/interactions__", 
         #                 .x, "_vs_4o__", .y, ".csv"))
       
     })

map(
  list.files(paste0(fp, "6_information_density/DiDs/")) %>% discard(~ str_detect(.x, "joint")),
  ~paste(readLines(paste0(fp, "6_information_density/DiDs/", .x)), collapse = "\n")
) %>% 
  paste0(collapse = "\n\n\\vspace{3em}\n\n") %>% 
  cat(file = paste0(fp, "6_information_density/DiDs/joint/JOINT_FILE.txt"))

# 6. Scaling curve: accuracy ----
df_list$df_estimates_scaling_curve_accuracy

combinations <- 
  expand.grid(
    data_label = unique(df_list$df_estimates_scaling_curve_accuracy$data_label),
    outcome = unique(df_list$df_estimates_scaling_curve_accuracy$outcome_y)
  )

map2(
  combinations$data_label,
  combinations$outcome,
  function(.x, .y) {
    
    # .x <- "full"
    # .y <- "convo_prop_facts_above_veracity_threshold"
    
    df_temp <- 
      df_list$df_estimates_scaling_curve_accuracy %>% 
      filter(data_label==.x, outcome_y==.y)
    
    # OLS estimates
    if(.x == "full") {
      out <-
        df_temp %>% 
        select(ols_dialogue) %>% 
        unnest(ols_dialogue) %>% 
        select(model:study) %>% 
        select(-outcome)
      
      if(str_detect(.y, "prop")) {
        out <- out %>% mutate(across(c(estimate, `std.error`, `conf.low`, `conf.high`), ~.x*100))
      }
      out %>% 
        mutate(model = str_replace_all(model, model_names)) %>% 
        fun_round_fix_p() %>% 
        fun_my_kable(
          my_caption = paste0("Mean estimates. Outcome: ", str_replace_all(.y, outcome_names), "."),
          my_file = paste0(fp, "7_scaling_curve_accuracy/1_mean_estimates__", .y, ".txt"),
          my_footnote = "Estimates are in percentage points."
        )
        # write_csv(paste0(fp, "7_scaling_curve_accuracy/ols_estimates__", .y, ".csv"))
    }
    
    # Meta-reg estimates
    out <-
      df_temp %>% 
      pull(metareg_lin) %>% 
      .[[1]] %>% 
      summary()
    
    out <-
      out$fixed %>%
      rownames_to_column(var = "Term") %>% 
      as_tibble()
    
    if(str_detect(.y, "prop")) {
      out <- out %>% mutate(across(c(Estimate, `Est.Error`, `l-95% CI`, `u-95% CI`), ~.x*100))
    }
    
     out %>% 
      mutate(across(where(is.numeric), ~format(round(.x, 2), nsmall = 2)),
             Term = ifelse(str_detect(Term, "log10"), "log10(flops)", Term)) %>% 
       fun_my_kable(
         my_caption = paste0("Meta-regression output. Models: ", str_replace_all(.x, tuning_names), 
                             ". Outcome: ", str_replace_all(.y, outcome_names), "."),
         my_file = paste0(fp, "7_scaling_curve_accuracy/2_linear_metareg_estimates__", .x, "__", .y, ".txt"),
         my_footnote = "Estimates are in percentage points. ESS = effective sample size of the posterior distribution."
       )
      # write_csv(
      #   paste0(fp, "7_scaling_curve_accuracy/linear_metareg_estimates__", .x, "__", .y, ".csv")
      # )
    
    # LOO-CV
    df_temp %>% 
      pull(loo) %>% 
      .[[1]] %>% 
      data.frame() %>% 
      rownames_to_column(var = "model") %>% 
      mutate(model = ifelse(model == "mdl_lin", "Linear", "GAM")) %>% 
      mutate(across(where(is.numeric), ~format(round(.x, 2), nsmall = 2))) %>% 
      fun_my_kable(
        my_caption = paste0(
          "Leave-one-out cross-validation comparing linear and nonlinear (GAM) meta-regressions. ", 
          "Models: ", str_replace_all(.x, tuning_names), 
          ". Outcome: ", str_replace_all(.y, outcome_names), "."),
        my_file = paste0(fp, "7_scaling_curve_accuracy/3_loocv__", .x, "__", .y, ".txt"),
        my_footnote = "ELPD = expected log pointwise predictive density. LOO = leave-one-out."
      )
      # write_csv(
      #   paste0(fp, "7_scaling_curve_accuracy/loocv__", .x, "__", .y, ".csv")
      # )
    
  }
)

map(
  list.files(paste0(fp, "7_scaling_curve_accuracy/")) %>% discard(~ str_detect(.x, "joint")),
  ~paste(readLines(paste0(fp, "7_scaling_curve_accuracy/", .x)), collapse = "\n")
) %>% 
  paste0(collapse = "\n\n\\vspace{3em}\n\n") %>% 
  cat(file = paste0(fp, "7_scaling_curve_accuracy/joint/JOINT_FILE.txt"))

# 7. Deceptive llama405b ----
out <-
  df_list$df_estimates_405b_deception %>% 
  filter(term=="prompt_rhetoric_idinformation_with_deception") %>% 
  mutate(term = "Deceptive info prompt (vs. info prompt)") %>% 
  fun_round_fix_p()

map(out$outcome,
    function(.x) {
      
      out %>% 
        filter(outcome==.x) %>% 
        select(-outcome) %>% 
        fun_my_kable(
          my_caption = paste0("Comparing deceptive-information prompt against information prompt.", 
                              " Model: Llama3.1-405B. Study: 2. Outcome: ", str_replace_all(.x, outcome_names), "."),
          my_file = paste0(fp, "8_deceptive_prompt/deceptive_vs_not__", .x, ".txt"),
          my_footnote = "Estimates are in percentage points."
        )
      
    })

map(
  list.files(paste0(fp, "8_deceptive_prompt/")) %>% discard(~ str_detect(.x, "joint")),
  ~paste(readLines(paste0(fp, "8_deceptive_prompt/", .x)), collapse = "\n")
) %>% 
  paste0(collapse = "\n\n\\vspace{3em}\n\n") %>% 
  cat(file = paste0(fp, "8_deceptive_prompt/joint/JOINT_FILE.txt"))
