
fun_evaluate_interaction <- function(
    outcome_y, # Outcome variable
    size_metric, # Name of language model size variable
    list_data, # Data frames
    n_chains = 4, # Chains for MCMC
    n_cores = 4, # Cores for MCMC
    k_value, # GAM flexibility
    sub_compare, # Subgroup to compare
    n_iter,
    adapt_delta_value
) {
  
  # For debugging:
  # outcome_y    <- "convo_prop_facts_above_veracity_threshold"
  # size_metric  <- "flops_1e21_epoch"
  # n_chains     <- 4
  # n_cores      <- 4
  # k_value      <- 10
  # sub_compare  <- "dev_pt"
  # n_iter       <- 2000
  # adapt_delta_value <- 0.9
  # 
  # list_data <- df_estimates
  
  # Interaction variable must be a factor
  stopifnot(is.factor(list_data[[sub_compare]]))
  
  
  # > 1. Prepare data for meta-regression ----
  
  # Filter to outcome
  list_data <- list_data %>% filter(outcome_id == outcome_y)
  
  # Create study dummies
  study_dummies <-
    model.matrix(~ study, data = list_data %>% mutate(study = factor(study))) %>% 
    as_tibble %>% 
    select(-1)
  
  list_data <- list_data %>% add_column(study_dummies)
  
  # Bind vcov
  vcov_big <- diag(list_data$std.error^2) 
  
  # > 2. Fit meta-regression ----
  
  lin_formula_int <- paste0("estimate ~ log10(", size_metric, ")*", sub_compare, " + study2 + study3 + fcor(V)")
  
  mdl_lin_int <- 
    brm(formula = as.formula(lin_formula_int),
        data = list_data, 
        data2 = list(V = vcov_big),
        family = student(),
        prior = c(prior(constant(1), class = sigma),
                  prior(constant(2), class = nu)),
        seed = 17,
        iter = n_iter, 
        warmup = 1000, 
        control = list(adapt_delta = adapt_delta_value),
        chains = n_chains,
        cores = n_cores)
  
  # Return
  list("mdl_lin_int" = mdl_lin_int,
       "raw_estimates" = list_data)
  
}


