
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

source("analysis_code/function_scaling_curve_accuracy.R")
source("analysis_code/function_interaction_tuning_type_accuracy.R")

# Read in data ----
df_list <- map(
  list.files("output/processed_data"),
  function(.x) {
    readRDS(paste0("output/processed_data/", .x))
  }
)

names(df_list) <- list.files("output/processed_data") %>% str_remove_all(".rds")

df_estimates <- 
  df_list$df_estimates_veracity_infodensity_by_models %>% 
  filter(str_detect(outcome_id, "veracity")) %>% 
  filter(dialogue %in% c(1, NA)) %>% # omit study 1 dialogue 2
  rename(model = term)

# Join model sizes
df_estimates <-
  df_estimates %>% 
  left_join(df_list$df_model_sizes) %>% 
  mutate(flops_1e21 = ifelse(is.na(flops_1e21), 
                             df_list$df_model_sizes %>% 
                               filter(model == "llama-3-1-405b") %>% 
                               pull(flops_1e21) %>% unique, 
                             flops_1e21),
         flops_1e21_epoch = ifelse(is.na(flops_1e21_epoch), 
                                   df_list$df_model_sizes %>% 
                                     filter(model == "llama-3-1-405b") %>% 
                                     pull(flops_1e21_epoch) %>% unique,
                                   flops_1e21_epoch))

# Fit scaling curve ----

# > Define data specifications ----
data_specs <-
  tribble( 
    
    ~data_label,          ~list_data,                                                     ~k_value,
    
    "full",               df_estimates,                                                   10,
    
    "dev_pt",             df_estimates %>%  
                              filter(str_detect(model, "chat|gpt|grok")),                 5,
    
    "chat_pt",            df_estimates %>%  
                              filter(str_detect(model, "chat|gpt|grok", negate = T)),     5,
    
    
    
  )

# > Define other pieces ----
list_outcomes <- c(
  "convo_prop_facts_above_veracity_threshold" ,
  "convo_mean_veracity"
)

list_size_metrics <- c("flops_1e21_epoch")

specifications <- 
  expand_grid(
    outcome_y = list_outcomes,
    size_metric = list_size_metrics,
    data_specs
  )

specifications %>% print(n=100)

out_meta_regs <- list()

# > Fit! ----
for (i in 1:nrow(specifications)) {
  
  t0 <- Sys.time()
  
  out_meta_regs[[i]] <-
    fun_scaling_curve_accuracy(
      outcome_y = specifications[[i,"outcome_y"]],
      size_metric = specifications[[i,"size_metric"]],
      list_data = specifications[[i,"list_data"]][[1]],
      n_chains = 4, 
      n_cores = 4, 
      gran_factor = 5, 
      oom_factor = 0, 
      k_value = specifications[[i,"k_value"]]
    )
  
  print(paste0("Metaregs fitted for spec ", i, "/", nrow(specifications), "."))
  print(Sys.time() - t0)
  
}

# Join
specifications$ols_dialogue        <- map(out_meta_regs, ~.x$estimates)
# specifications$ols_static          <- map(out_meta_regs, ~.x$static)
specifications$metareg_lin         <- map(out_meta_regs, ~.x$mdl_lin)
specifications$metareg_gam         <- map(out_meta_regs, ~.x$mdl_gam)
specifications$loo                 <- map(out_meta_regs, ~.x$loo)
specifications$mod_weights         <- map(out_meta_regs, ~.x$mod_weights)
specifications$cond_means          <- map(out_meta_regs, ~.x$conditional_means)
specifications$oom_compare         <- map(out_meta_regs, ~.x$oom_compare)
specifications$linear_cond_means   <- map(out_meta_regs, ~.x$linear_conditional_means)
specifications$linear_oom_compare  <- map(out_meta_regs, ~.x$linear_oom_compare)

saveRDS(specifications, "output/processed_data/df_estimates_scaling_curve_accuracy.rds")


# Fit tuning type interaction curve ----

df_estimates <-
  df_estimates %>%
      mutate(dev_pt = factor(ifelse(str_detect(model, "gpt|grok|chat"), 1, 0)))

specifications_int <- 
  expand_grid(
    outcome_y = list_outcomes,
    size_metric = list_size_metrics,
    list_data = list(df_estimates)
  )

out_meta_regs_int <- list()

for (i in 1:nrow(specifications_int)) {
  
  t0 <- Sys.time()
  
  out_meta_regs_int[[i]] <-
    fun_evaluate_interaction(
      outcome_y = specifications_int[[i,"outcome_y"]],
      size_metric = specifications_int[[i,"size_metric"]],
      list_data = specifications_int[[i,"list_data"]][[1]],
      n_chains = 4, 
      n_cores = 4,
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

