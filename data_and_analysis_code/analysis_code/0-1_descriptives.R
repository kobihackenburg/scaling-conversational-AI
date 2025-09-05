
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
    "qwen" = "Qwen"
    # "treat_static" = "Static message",
    # "aa_control" = "Control",
    # "control" = "Control",
    # "treatment" = "Treatment",
    # "generic" = "Generic",
    # "personalized" = "Personalized"
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


# N per condition ----

fun_count_n <- function(data, conds, title_suffix, file_name = NULL) {
  
  fp <- paste0("output/supplement/condition_ns/study", data %>% pull(study) %>% unique, "/", file_name)
  
  out <- 
    data %>% 
    group_by_at(conds) %>% 
    count() %>% 
    rename_with(~str_remove_all(.x, "d1.|d2."), everything()) %>%
    rename_with(~str_replace_all(.x, "_", " "), everything()) %>%
    mutate(condition = str_replace_all(condition, "_", "-"))
  
  out %>% 
    fun_my_kable(
      my_caption = paste0("Sample sizes (n) per condition. Study ", data %>% pull(study) %>% unique, ". ",
                          title_suffix, "."),
      my_file = paste0(fp, ".txt"),
      my_footnote = "NA denotes missingness. It means the randomized factor was not assigned."
    )
    
}

# > Study 1 ----
fun_count_n(study1 %>% 
              drop_na(d1.post_average, d1.condition) %>% 
              mutate(d1.model = str_replace_all(d1.model, model_names)), 
            c("d1.condition", "d1.model"),
            "Chat 1",
            "s1_chat1_by_condition_model") 

fun_count_n(study1 %>% drop_na(d1.post_average, d1.condition), 
            c("d1.condition", "d1.prompt_rhetoric_id", "d1.prompt_personalize"),
            "Chat 1",
            "s1_chat1_by_prompt") 

fun_count_n(study1 %>% drop_na(d2.post_average), 
            c("d2.condition", "d2.prompt_rhetoric_id", "d2.prompt_personalize"),
            "Chat 2",
            "s1_chat2_by_prompt") 

map(
  list.files("output/supplement/condition_ns/study1/") %>% discard(~ str_detect(.x, "joint")),
  ~paste(readLines(paste0("output/supplement/condition_ns/study1/", .x)), collapse = "\n")
) %>% 
  paste0(collapse = "\n\n\\vspace{3em}\n\n") %>% 
  cat(file = "output/supplement/condition_ns/study1/joint/JOINT_FILE.txt")

# > Study 2 ----
fun_count_n(study2 %>% drop_na(post_average) %>% mutate(model = str_replace_all(model, model_names)), 
            c("condition", "model", "post_train"),
            "",
            "s2_by_condition_model_pt") 

fun_count_n(study2 %>% drop_na(post_average), 
            c("condition", "prompt_rhetoric_id", "personalize"),
            "",
            "s2_by_condition_prompt") 

map(
  list.files("output/supplement/condition_ns/study2/") %>% discard(~ str_detect(.x, "joint")),
  ~paste(readLines(paste0("output/supplement/condition_ns/study2/", .x)), collapse = "\n")
) %>% 
  paste0(collapse = "\n\n\\vspace{3em}\n\n") %>% 
  cat(file = "output/supplement/condition_ns/study2/joint/JOINT_FILE.txt")

# > Study 3 ----
fun_count_n(study3 %>% drop_na(post_average) %>% mutate(model = str_replace_all(model, model_names)), 
            c("condition", "model", "post_train"),
            "",
            "s3_by_condition_model_pt") 

fun_count_n(study3 %>% drop_na(post_average), 
            c("condition", "prompt_rhetoric_id", "personalize"),
            "",
            "s3_by_condition_prompt") 

# Join files into table file for the SI
map(
  list.files("output/supplement/condition_ns/study3/") %>% discard(~ str_detect(.x, "joint")),
  ~paste(readLines(paste0("output/supplement/condition_ns/study3/", .x)), collapse = "\n")
) %>% 
  paste0(collapse = "\n\n\\vspace{3em}\n\n") %>% 
  cat(file = "output/supplement/condition_ns/study3/joint/JOINT_FILE.txt")


# Conversation stats by condition ----

# Update kable function to shrink font size for the tables below
fun_my_kable <- function(df, my_caption, my_file, my_footnote) {
  df %>% 
    kable(
      "latex", 
      booktabs = TRUE, 
      caption = my_caption
    ) %>%
    kable_styling(latex_options = c("HOLD_position", "striped"),
                  font_size = 7) %>% # shrinks font size 
    footnote(general = my_footnote) %>% 
    cat(file = my_file)
}

fun_describe_convos <- function(data, conds, title_suffix, file_name = NULL) {
  
  # data <- study1
  # conds <- c("d1.condition", "d1.model")
  # file_name <- "test"
  
  fp <- paste0("output/supplement/convo_stats/", file_name)
  
  out <- 
    data %>% 
    group_by_at(conds) %>% 
    summarise(
      total_convos = n(),
      total_AI_msgs = sum(n_msgs_ai, na.rm = T),
      M_AI_msgs = mean(n_msgs_ai, na.rm = T),
      SD_AI_msgs = sd(n_msgs_ai, na.rm = T),
      total_user_msgs = sum(n_msgs_user, na.rm = T),
      M_user_msgs = mean(n_msgs_user, na.rm = T),
      SD_user_msgs = sd(n_msgs_user, na.rm = T)) %>% 
    rename_with(~str_remove_all(.x, "d1.|d2."), everything()) %>%
    rename_with(~str_replace_all(.x, "_", " "), everything()) %>%
    mutate(condition = str_replace_all(condition, "_", "-")) %>% 
    mutate_if(is.numeric, ~round(., 2))
  
  out %>% 
    fun_my_kable(
      my_caption = paste0("Conversation statistics by condition. Study ", data %>% pull(study) %>% unique, ". ",
                          title_suffix, "."),
      my_file = paste0(fp, ".txt"),
      my_footnote = "M = Mean; SD = standard deviation."
    )
  
}

# > Study 1 ----

# >> By condition ----

# Chat 1
fun_describe_convos(
  study1 %>% 
    filter(str_detect(d1.condition, "dialogue")) %>% 
    drop_na(d1.post_average, d1.condition) %>% 
    rename(n_msgs_ai = d1.count_ai_msgs,
           n_msgs_user = d1.count_user_msgs),
  conds = c("d1.condition"),
  title_suffix = "Chat 1", 
  file_name = "01_s1_chat1_by_condition"
)

# Chat 2
fun_describe_convos(
  study1 %>% 
    drop_na(d2.post_average, d2.condition) %>% 
    rename(n_msgs_ai = d2.count_ai_msgs,
           n_msgs_user = d2.count_user_msgs),
  conds = c("d2.condition"),
  title_suffix = "Chat 2", 
  file_name = "02_s1_chat2_by_condition"
)

# >> By condition and model ----

# Chat 1
fun_describe_convos(
  study1 %>% 
    filter(str_detect(d1.condition, "dialogue")) %>% 
    drop_na(d1.post_average, d1.condition) %>% 
    mutate(d1.model = str_replace_all(d1.model, model_names)) %>% 
    rename(n_msgs_ai = d1.count_ai_msgs,
           n_msgs_user = d1.count_user_msgs),
  conds = c("d1.condition", "d1.model"),
  title_suffix = "Chat 1", 
  file_name = "03_s1_chat1_by_condition_model"
)

# >> By prompt ----

# Chat 1
fun_describe_convos(
  study1 %>% 
    filter(str_detect(d1.condition, "dialogue")) %>% 
    drop_na(d1.post_average, d1.condition) %>% 
    rename(n_msgs_ai = d1.count_ai_msgs,
           n_msgs_user = d1.count_user_msgs),
  conds = c("d1.condition", "d1.prompt_rhetoric_id", "d1.prompt_personalize"),
  title_suffix = "Chat 1", 
  file_name = "04_s1_chat1_by_prompt"
)

# Chat 2
fun_describe_convos(
  study1 %>% 
    drop_na(d2.post_average, d2.condition) %>% 
    rename(n_msgs_ai = d2.count_ai_msgs,
           n_msgs_user = d2.count_user_msgs),
  conds = c("d2.condition", "d2.prompt_rhetoric_id", "d2.prompt_personalize"),
  title_suffix = "Chat 2", 
  file_name = "05_s1_chat2_by_prompt"
)

# > Study 2 ----

# >> By condition ----
fun_describe_convos(
  study2 %>% 
    drop_na(post_average, condition),
  conds = c("condition"),
  title_suffix = "", 
  file_name = "06_s2_by_condition"
)

# >> By condition, model, PT ----
fun_describe_convos(
  study2 %>% 
    drop_na(post_average, condition) %>% 
    mutate(model = str_replace_all(model, model_names)),
  conds = c("condition", "model", "post_train"),
  title_suffix = "", 
  file_name = "07_s2_by_condition_model_pt"
)

# >> By prompt ----
fun_describe_convos(
  study2 %>% 
    drop_na(post_average, condition),
  conds = c("condition", "prompt_rhetoric_id", "personalize"),
  title_suffix = "", 
  file_name = "08_s2_by_prompt"
)

# > Study 3 ----

# >> By condition ----
fun_describe_convos(
  study3 %>% 
    drop_na(post_average, condition),
  conds = c("condition"),
  title_suffix = "", 
  file_name = "09_s3_by_condition"
)

# >> By condition, model, PT ----
fun_describe_convos(
  study3 %>% 
    drop_na(post_average, condition) %>% 
    mutate(model = str_replace_all(model, model_names)),
  conds = c("condition", "model", "post_train"),
  title_suffix = "", 
  file_name = "10_s3_by_condition_model_pt"
)

# >> By prompt ----
fun_describe_convos(
  study3 %>% 
    drop_na(post_average, condition),
  conds = c("condition", "prompt_rhetoric_id", "personalize"),
  title_suffix = "", 
  file_name = "11_s3_by_prompt"
)

# Join files into table file for the SI
map(
  list.files("output/supplement/convo_stats/") %>% discard(~ str_detect(.x, "joint")),
  ~paste(readLines(paste0("output/supplement/convo_stats/", .x)), collapse = "\n")
) %>% 
  paste0(collapse = "\n\n\\vspace{3em}\n\n") %>% 
  cat(file = "output/supplement/convo_stats/joint/JOINT_FILE.txt")

