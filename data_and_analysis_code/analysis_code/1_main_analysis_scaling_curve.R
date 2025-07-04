
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
# library(patchwork)
set.seed(42)

source("analysis_code/function_scaling_curve.R")
source("analysis_code/function_interaction_tuning_type.R")

# Read in data ----
study1 <- readRDS("study_1/output/data_prepared.rds") %>% mutate(study = 1)
study2 <- readRDS("study_2/output/data_prepared.rds") %>% mutate(study = 2)
study3 <- readRDS("study_3/output/data_prepared.rds") %>% mutate(study = 3)

names(study1)
names(study2)
names(study3)


# Restrict to base models only
study2 <- study2 %>% filter(is.na(post_train) | post_train == "base")
study3 <- study3 %>% filter(is.na(post_train) | post_train == "base")

study1 <- 
  study1 %>% 
  rename_with(~ str_remove_all(.x, "d1."),
              c("d1.post_average",
                "d1.post_average_imputed_with_pre",
                "d1.pre_average",
                "d1.models_vs_control"))


# Define model sizes ----
model_sizes <-
  study1 %>% 
  select(contains("flops"), d1.model) %>% 
  distinct(d1.model, .keep_all = T) %>% 
  drop_na() %>% 
  rename(model = d1.model) %>% 
  add_row(model            = c("gpt-4.5", 
                               "gpt-3.5", 
                               "chatgpt-4o-latest",
                               "grok-3"),
          flops_1e21       = c(210000, 
                               2580, 
                               study1 %>% filter(d1.model=="gpt-4o") %>% pull(flops_1e21) %>% unique,
                               464000
                               ),
          flops_1e21_epoch = c(210000, 
                               2580,
                               study1 %>% filter(d1.model=="gpt-4o") %>% pull(flops_1e21_epoch) %>% unique,
                               464000
                               )
  )

saveRDS(model_sizes, "output/processed_data/df_model_sizes.rds")

# Fit scaling curve ----

# > Define data specifications ----
data_specs <-
  tribble( 
    
    ~data_label,          ~list_data,                                                                ~k_value,
    
    "full",               list(study1, study2, study3),                                              10,
    
    "dev_pt",             list(study1 %>%                                                            
                                 filter(str_detect(models_vs_control, "chat|gpt|control|static")), 
                               study2 %>% 
                                 filter(str_detect(models_vs_control, "llama", negate = T)),
                               study3),                                                              5,
    
    "chat_pt",            list(study1 %>%                                                            
                                 filter(str_detect(models_vs_control, "chat|gpt", negate = T)), 
                               study2 %>% 
                                 filter(str_detect(models_vs_control, "llama|control"))),            5
    
    
    
  )

# > Define other pieces ----
list_outcomes <- c(
  "post_average" ,
  "post_average_imputed_with_pre"
)

list_size_metrics <- c("flops_1e21_epoch")

specifications <- 
  expand_grid(
    outcome = list_outcomes,
    size_metric = list_size_metrics,
    data_specs
  )

specifications %>% print(n=100)

out_meta_regs <- list()

# > Fit! ----
for (i in 1:nrow(specifications)) {
  
  t0 <- Sys.time()
  
  out_meta_regs[[i]] <-
    fun_scaling_curve(
      outcome = specifications[[i,"outcome"]],
      size_metric = specifications[[i,"size_metric"]],
      list_data = specifications[[i,"list_data"]][[1]],
      n_chains = 4, 
      n_cores = 4, 
      gran_factor = 5, 
      oom_factor = 2, 
      model_sizes = model_sizes,
      k_value = specifications[[i,"k_value"]]
    )

  print(paste0("Metaregs fitted for spec ", i, "/", nrow(specifications), "."))
  print(Sys.time() - t0)
  
}

# Join
specifications$ols_dialogue        <- map(out_meta_regs, ~.x$estimates)
specifications$ols_static          <- map(out_meta_regs, ~.x$static)
specifications$metareg_lin         <- map(out_meta_regs, ~.x$mdl_lin)
specifications$metareg_gam         <- map(out_meta_regs, ~.x$mdl_gam)
specifications$loo                 <- map(out_meta_regs, ~.x$loo)
specifications$mod_weights         <- map(out_meta_regs, ~.x$mod_weights)
specifications$cond_means          <- map(out_meta_regs, ~.x$conditional_means)
specifications$oom_compare         <- map(out_meta_regs, ~.x$oom_compare)
specifications$linear_cond_means   <- map(out_meta_regs, ~.x$linear_conditional_means)
specifications$linear_oom_compare  <- map(out_meta_regs, ~.x$linear_oom_compare)

saveRDS(specifications, "output/processed_data/df_estimates_scaling_curve.rds")

# Fit tuning type interaction curve ----

list_data <-
  list(
    study1 %>%
     mutate(dev_pt = factor(ifelse(str_detect(models_vs_control, "gpt|grok|chat"), 1, 0))) %>%
     filter(str_detect(models_vs_control, "static", negate=T)),
   study2 %>%
     mutate(dev_pt = factor(ifelse(str_detect(models_vs_control, "gpt|grok|chat"), 1, 0))) %>%
     filter(str_detect(models_vs_control, "static", negate=T)),
   study3 %>%
     mutate(dev_pt = factor(ifelse(str_detect(models_vs_control, "gpt|grok|chat"), 1, 0))) %>%
     filter(str_detect(models_vs_control, "static", negate=T))
  )

specifications_int <- 
  expand_grid(
    outcome = list_outcomes,
    size_metric = list_size_metrics,
    list_data = list(list_data)
  )

out_meta_regs_int <- list()

for (i in 1:nrow(specifications_int)) {
  
  t0 <- Sys.time()
  
  out_meta_regs_int[[i]] <-
    fun_evaluate_interaction(
      outcome = specifications_int[[i,"outcome"]],
      size_metric = specifications_int[[i,"size_metric"]],
      list_data = specifications_int[[i,"list_data"]][[1]],
      n_chains = 4, 
      n_cores = 4,
      model_sizes = model_sizes,
      k_value = NULL,
      sub_compare = "dev_pt",
      n_iter = 2000,
      adapt_delta_value = 0.9
    )
  
  print(paste0("Metaregs fitted for spec ", i, "/", nrow(specifications_int), "."))
  print(Sys.time() - t0)
  
}

specifications_int$ols_dialogue <- map(out_meta_regs_int, ~.x$raw_estimates)
specifications_int$metareg_lin  <- map(out_meta_regs_int, ~.x$mdl_lin_int)

saveRDS(specifications_int, "output/processed_data/df_estimates_scaling_curve_interaction.rds")
