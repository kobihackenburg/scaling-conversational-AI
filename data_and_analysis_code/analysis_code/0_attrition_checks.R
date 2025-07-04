
# library(tidyverse)
# library(metafor)
# library(broom)
# library(estimatr)
# library(ggrepel)
# library(cowplot)
# library(tidybayes)
# library(brms)
# library(mgcv)
# library(modelr)
# library(ggdist)
# library(kableExtra)
set.seed(42)

# Read in data ----
study1 <- readRDS("study_1/output/data_prepared.rds") %>% mutate(study = 1)
study2 <- readRDS("study_2/output/data_prepared.rds") %>% mutate(study = 2)
study3 <- readRDS("study_3/output/data_prepared.rds") %>% mutate(study = 3)

names(study1)
names(study2)
names(study3)

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

# Overall post-treatment attrition ----
study1 %>% count(d1.post_missing) %>% slice(-3) %>% mutate(perc = (n/sum(n))*100)
study1 %>% count(d2.post_missing) %>% slice(-3) %>% mutate(perc = (n/sum(n))*100)
study2 %>% count(post_missing) %>% slice(-3) %>% mutate(perc = (n/sum(n))*100)
study3 %>% count(post_missing) %>% slice(-3) %>% mutate(perc = (n/sum(n))*100)

# Post-treatment attrition checks ----
f <- function(df, grp_var, prefix, title_suffix = "", file_name = NULL) {
  
  group_var_sym <- enquo(grp_var)
  
  # Construct variable names
  post_missing_var    <- paste0(prefix, "post_missing")
  outcome_missing_var <- paste0(prefix, "outcome_missing")
  grouping_var        <- paste0(prefix, quo_name(group_var_sym))
  
  # Filter data
  df_filtered <- 
    df %>%
    filter(.data[[post_missing_var]] != "pre_missing") %>%
    drop_na(.data[[grouping_var]])
  
  # Count NAs
  df_count <-
    df_filtered %>%
    group_by(.data[[grouping_var]]) %>%
    summarise(
      prop_post_na = mean(.data[[outcome_missing_var]], na.rm = TRUE),
      n = n(),
      sd_y = sd(.data[[outcome_missing_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      se = sd_y / sqrt(n),
      lwr = prop_post_na - 1.96 * se,
      upr = prop_post_na + 1.96 * se
    ) %>% 
    mutate(!!grouping_var := str_replace_all(.data[[grouping_var]], model_names)) %>% 
    mutate(!!grouping_var := str_replace_all(.data[[grouping_var]], pt_names))
  
  if(str_detect(grp_var, "prompt")) {
    df_count <- df_count %>% mutate(!!grouping_var := str_to_sentence(.data[[grouping_var]]))
  }
  
  # Plot
  g <-
    df_count %>%
    ggplot(aes(x = .data[[grouping_var]], y = prop_post_na)) +
    theme_bw() +
    geom_col() +
    geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0) +
    geom_text(aes(label = paste0(round(prop_post_na, 3)*100, "% NA", "\n", "(N = ", n, ")")), 
              nudge_y = 0.05) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5)) +
    labs(title = paste0("Study ", df %>% pull(study) %>% unique),
         y = "Proportion post-treatment missing (NA) outcome",
         subtitle = title_suffix,
         x = "")
  
  # F-test on post-treat NA
  f_test <- anova(
    lm(
      formula = reformulate(grouping_var, outcome_missing_var),
      data = df_filtered
    )
  ) %>% 
    tidy() %>% 
    mutate(term = ifelse(term == "Residuals", term, "Condition"))
  
  # Save
  df_count <-
    df_count %>% 
    rename(`Proportion NA` = prop_post_na,
           `Total N` = n,
           SD = sd_y,
           SE = se,
           `95% Lower` = lwr,
           `95% Upper` = upr,
           Condition = !!grouping_var) %>% 
    select(Condition, `Proportion NA`, `Total N`) %>% 
    mutate(`Proportion NA` = round(`Proportion NA`, 3))
  
  f_test <-
    f_test %>% 
    mutate(`p.value` = ifelse(`p.value` <.001, "<.001", as.character(round(`p.value`, 3)))) %>% 
    mutate(across(where(is.numeric), ~round(.x, 2)))
  
  fp <- paste0("output/supplement/attrition/study", df %>% pull(study) %>% unique, "/", file_name)
  
  df_count %>% 
    fun_my_kable(
      my_caption = paste0("Proportion post-treatment missingness (NA). Study ", df %>% pull(study) %>% unique, ". ",
                          title_suffix, "."),
      my_file = paste0(fp, "__count.txt"),
      my_footnote = ""
    )
  
  f_test %>% 
    fun_my_kable(
      my_caption = paste0("F-test on post-treatment missingness. Study ", df %>% pull(study) %>% unique, ". ",
                          title_suffix, "."),
      my_file = paste0(fp, "__ftest.txt"),
      my_footnote = ""
    )
  
  
}

# > Study 1 ----

# Chat 1
f(study1, "models_vs_control", "d1.", "Chat 1", 
  file_name = "s1_chat1_models_vs_control")

f(study1 %>% filter(str_detect(d1.condition, "control", negate = T)), 
  "prompt_rhetoric_id", "d1.", "Chat 1: Prompts", file_name = "s1_chat1_prompt")

f(study1 %>% filter(str_detect(d1.condition, "control", negate = T)), 
  "prompt_personalize", "d1.", "Chat 1: Personalization", file_name = "s1_chat1_personalize")

# Chat 2
f(study1, "condition", "d2.", "Chat 2 (GPT-4o)", 
  file_name = "s1_chat2_model_vs_control")

f(study1 %>% filter(str_detect(d2.condition, "control", negate = T)), 
  "prompt_rhetoric_id", "d2.", "Chat 2 (GPT-4o): Prompts", file_name = "s1_chat2_prompt")

f(study1 %>% filter(str_detect(d2.condition, "control", negate = T)), 
  "prompt_personalize", "d2.", "Chat 2 (GPT-4o): Personalization", file_name = "s1_chat2_personalize")

map(
  list.files("output/supplement/attrition/study1/") %>% discard(~ str_detect(.x, "joint")),
  ~paste(readLines(paste0("output/supplement/attrition/study1/", .x)), collapse = "\n")
) %>% 
  paste0(collapse = "\n\n\\vspace{3em}\n\n") %>% 
  cat(file = "output/supplement/attrition/study1/joint/JOINT_FILE.txt")

# > Study 2 ----
f(study2, "models_vs_control", "", title_suffix = "Model conditions", 
  file_name = "s2_models_vs_control")

f(study2 %>% filter(condition %in% c("treatment-1"), str_detect(model, "8b")), 
  "post_train", "", "PPT: Llama-8B", 
  file_name = "s2_post_train_llama_8b")

f(study2 %>% filter(condition %in% c("treatment-1"), str_detect(model, "405b")), 
  "post_train", "", "PPT: Llama-405B", 
  file_name = "s2_post_train_llama_405b")

f(study2 %>% filter(condition %in% c("treatment-2")),
  "post_train", "", "PPT: GPT-3.5 / 4o (8/24) / 4.5", 
  file_name = "s2_post_train_developer_models")

f(study2 %>% filter(str_detect(condition, "control", negate = T)), 
  "prompt_rhetoric_id", "", "Prompts (open- and closed-source models)", file_name = "s2_prompt")

f(study2 %>% filter(str_detect(condition, "control", negate = T)), 
  "personalize", "", "Personalizaton (open- and closed-source models)", file_name = "s2_personalize")

map(
  list.files("output/supplement/attrition/study2/") %>% discard(~ str_detect(.x, "joint")),
  ~paste(readLines(paste0("output/supplement/attrition/study2/", .x)), collapse = "\n")
) %>% 
  paste0(collapse = "\n\n\\vspace{3em}\n\n") %>% 
  cat(file = "output/supplement/attrition/study2/joint/JOINT_FILE.txt")

# > Study 3 ----
f(study3, "models_vs_control", "", file_name = "s3_models_vs_control", title_suffix = "Model conditions")

f(study3 %>% filter(condition %in% c("treatment-1", "treatment-2")), 
  "post_train", "", "PPT", file_name = "s3_post_train")

f(study3 %>% filter(condition %in% c("treatment-1", "treatment-2")), 
  "prompt_rhetoric_id", "", "Prompts", file_name = "s3_prompt")

f(study3 %>% filter(condition %in% c("treatment-1", "treatment-2")), 
  "personalize", "", "Personalization", file_name = "s3_personalize")

map(
  list.files("output/supplement/attrition/study3/") %>% discard(~ str_detect(.x, "joint")),
  ~paste(readLines(paste0("output/supplement/attrition/study3/", .x)), collapse = "\n")
) %>% 
  paste0(collapse = "\n\n\\vspace{3em}\n\n") %>% 
  cat(file = "output/supplement/attrition/study3/joint/JOINT_FILE.txt")

