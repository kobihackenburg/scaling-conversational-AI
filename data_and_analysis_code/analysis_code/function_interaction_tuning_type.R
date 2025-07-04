
fun_evaluate_interaction <- function(
    outcome, # Outcome variable
    size_metric, # Name of language model size variable
    list_data, # Data frames
    n_chains = 4, # Chains for MCMC
    n_cores = 4, # Cores for MCMC
    model_sizes, # df of language model sizes
    k_value, # GAM flexibility
    sub_compare, # Subgroup to compare
    n_iter,
    adapt_delta_value
) {
  
  # For debugging:
  # outcome      <- "post_average"
  # size_metric  <- "flops_1e21_epoch"
  # n_chains     <- 4
  # n_cores      <- 4
  # model_sizes  <- model_sizes
  # k_value      <- 10
  # sub_compare  <- "dev_pt"
  # n_iter       <- 2000
  # adapt_delta_value <- 0.9
  # 
  # list_data <-
  #   list(
  # 
  #     study1 %>%
  #       mutate(dev_pt = factor(ifelse(str_detect(models_vs_control, "gpt|grok|chat"), 1, 0))) %>%
  #       filter(str_detect(models_vs_control, "static", negate=T)),
  #     study2 %>%
  #       mutate(dev_pt = factor(ifelse(str_detect(models_vs_control, "gpt|grok|chat"), 1, 0))) %>%
  #       filter(str_detect(models_vs_control, "static", negate=T)),
  #     study3 %>%
  #       mutate(dev_pt = factor(ifelse(str_detect(models_vs_control, "gpt|grok|chat"), 1, 0))) %>%
  #       filter(str_detect(models_vs_control, "static", negate=T))
  # 
  # )
  
  # Interaction variable must be a factor
  stopifnot(map_lgl(list_data, ~is.factor(.x[[sub_compare]])))
  
  # > 1. Fit OLS regression ----
  ols_formula <- paste0(outcome, " ~ factor(models_vs_control) + pre_average")
  
  subgroup_lvls <- map(list_data, ~.x[[sub_compare]]) %>% unlist %>% unique %>% sort
  
  # Get ATEs
  ols_models <- 
    map(list_data, 
        function(.x) { 
          
          # Fit
          lm_fit <- lm_robust(formula = as.formula(ols_formula), data = .x)
          
          # Get vcov matrixes
          vcov_mat <- lm_fit$vcov
          vcov_mat <- vcov_mat[str_detect(rownames(vcov_mat), "model"),
                               str_detect(colnames(vcov_mat), "model")]
          vcov_mat <- vcov_mat[str_detect(rownames(vcov_mat), "static", negate = T),
                               str_detect(colnames(vcov_mat), "static", negate = T)]
          
          # Tidy estimates
          lm_fit_tidied <- 
            tidy(lm_fit) %>% 
            filter(str_detect(term, "model")) %>% 
            mutate(term = str_remove(term, "factor\\s*\\([^\\)]+\\)"),
                   study = .x$study %>% unique) %>% 
            rename(model = term) %>% 
            left_join(.x %>% 
                        select(models_vs_control, dev_pt) %>% 
                        distinct() %>% 
                        rename(model = models_vs_control), 
                      by = "model")
          
          list("vcov" = vcov_mat,
               "estimates" = lm_fit_tidied)
          
        })
  
  # > 2. Prepare data for meta-regression ----
  
  # Get dialogue estimates
  df_estimates <- 
    map(ols_models, ~.x$estimates) %>% 
    bind_rows()
  
  # Store static message effects
  df_static <- df_estimates %>% filter(str_detect(model, "static"))
  
  # Drop static message effects
  df_estimates <-
    df_estimates %>% 
    filter(str_detect(model, "static", negate = T)) %>% 
    left_join(model_sizes, by = "model") # Add model sizes
  
  # Create study dummies
  study_dummies <-
    model.matrix(~ study, data = df_estimates %>% mutate(study = factor(study))) %>% 
    as_tibble %>% 
    select(-1)
  
  df_estimates <- df_estimates %>% add_column(study_dummies)
  
  # Bind vcov
  vcov_big <-
    map(ols_models, ~.x$vcov) %>% 
    bdiag() %>% 
    as.matrix()
  
  stopifnot(identical(vcov_big %>% diag %>% sqrt %>% unname, df_estimates$std.error)) # Check SEs match
  stopifnot(isSymmetric.matrix(vcov_big)) # Check symmetric
  stopifnot(is.factor(df_estimates[[sub_compare]]))
  
  # > 3. Fit meta-regression ----
  
  lin_formula_int <- paste0("estimate ~ log10(", size_metric, ")*", sub_compare, " + study2 + study3 + fcor(V)")
  
  mdl_lin_int <- 
    brm(formula = as.formula(lin_formula_int),
        data = df_estimates, 
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
       "raw_estimates" = df_estimates)
  
}


