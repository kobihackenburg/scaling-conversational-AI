

fun_scaling_curve <- 
  function(
    outcome, # Outcome variable
    size_metric, # Name of language model size variable
    list_data, # Data frames
    n_chains = 4, # Chains for MCMC
    n_cores = 4, # Cores for MCMC
    gran_factor = 1, # How granular do you want the conditional means? (1 = granularity of underlying data)
    oom_factor = 0, # How many orders of magnitude beyond data to forecast? (0 = no forecast)
    model_sizes, # df of language model sizes
    k_value # GAM flexibility
  ) {
    
    # For debugging:
    # outcome      <- "post_average"
    # size_metric  <- "flops_1e21_epoch"
    # n_chains     <- 4
    # n_cores      <- 4
    # gran_factor  <- 5
    # oom_factor   <- 2
    # k_value      <- 5
    # 
    # list_data <-
    #   list(
    # 
    #     # Full
    #     study1,
    #     study2,
    #     study3
    # 
    #     # Dev PT
    #     # study1 %>% filter(str_detect(models_vs_control, "chat|gpt|control|static")),
    #     # study2 %>% filter(str_detect(models_vs_control, "llama", negate = T)),
    #     # study3
    # 
    #     # Our PT
    #     # study1 %>% filter(str_detect(models_vs_control, "chat|gpt", negate = T)),
    #     # study2 %>% filter(str_detect(models_vs_control, "llama|control"))
    # 
    # )

    
    # > 0. Tidy study 1 variables ----
    
    # stopifnot(unique(list_data[[1]]$study) == 1)
    # 
    # list_data[[1]] <-
    #   list_data[[1]] %>% 
    #   rename_with(~ str_remove_all(.x, "d1."),
    #               c(paste0("d1.", outcome),
    #                 "d1.pre_average",
    #                 "d1.models_vs_control"))
    
    
    # > 1. Fit OLS regressions ----
    
    ols_formula <- paste0(outcome, " ~ factor(models_vs_control) + pre_average")
    
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
              rename(model = term)
            
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
    
    # > 3. Fit meta-regression ----
    
    study_terms <- paste(names(df_estimates)[names(df_estimates) %in% c("study2", "study3")], collapse = "+")
    
    lin_formula <- paste0("estimate ~ log10(", size_metric, ") + ", study_terms, " + fcor(V)")
    gam_formula <- paste0("estimate ~ s(log10(", size_metric, "), k = ", k_value, ") + ", study_terms, " + fcor(V)")
    
    # Log-linear
    mdl_lin <- 
      brm(formula = as.formula(lin_formula),
          data = df_estimates, 
          data2 = list(V = vcov_big),
          family = student(),
          prior = c(prior(constant(1), class = sigma),
                    prior(constant(2), class = nu)), 
          seed = 17,
          iter = 4000, 
          warmup = 1000, 
          control = list(adapt_delta = 0.99),
          chains = n_chains,
          cores = n_cores)
    
    # Log-nonlinear (GAM)
    mdl_gam <- 
      brm(formula = as.formula(gam_formula),
          data = df_estimates, 
          data2 = list(V = vcov_big),
          family = student(),
          prior = c(prior(constant(1), class = sigma),
                    prior(constant(2), class = nu)),
          seed = 17,
          iter = 4000, 
          warmup = 1000, 
          control = list(adapt_delta = 0.99),
          chains = n_chains,
          cores = n_cores)
    
    
    # > 4. Model comparison ----
    mdl_lin <- add_criterion(mdl_lin, criterion = "loo", reloo = T, 
                             reloo_args = list(cores = n_cores, control = list(adapt_delta = 0.99)))
    mdl_gam <- add_criterion(mdl_gam, criterion = "loo", reloo = T, 
                             reloo_args = list(cores = n_cores, control = list(adapt_delta = 0.99)))
    
    out_loo     <- loo_compare(mdl_lin, mdl_gam)
    out_weights <- model_weights(mdl_lin, mdl_gam, weights = "loo") %>% round(digits = 2)
    
    # > 5. Conditional means ----
    n_models <- nrow(df_estimates)
    min_size <- min(df_estimates[[size_metric]])
    max_size <- max(df_estimates[[size_metric]])
    
    newdata <- 
      data.frame(
        # x = exp(seq(log10(min_size), 
        #             log10(max_size), 
        #             length.out = n_models*gran_factor)),
        x = 10^(seq(log10(min_size), 
                    log10(max_size), 
                    length.out = n_models*gran_factor)),
        study2 = mean(df_estimates$study2),
        study3 = mean(df_estimates$study3)
      )
    
    if(oom_factor > 0) {
      
      for (i in 1:oom_factor) {
        
        newdata <-
          bind_rows(
            newdata,
            data.frame(
              # x = exp(seq(log10(max(newdata$x) + 1), 
              #             log10(max_size*10^i), 
              #             length.out = n_models*gran_factor)),
              x = 10^(seq(log10(max(newdata$x) + 1), 
                          log10(max_size*10^i), 
                          length.out = n_models*gran_factor)),
              study2 = mean(df_estimates$study2),
              study3 = mean(df_estimates$study3)
            )
          )
        
      }
      
    }
    
    names(newdata)[1] <- size_metric
    
    # Weighted GAM/linear
    weighted_conditional_means_summary <-
      pp_average(mdl_gam, mdl_lin,
                 weights = "loo",
                 method = "posterior_epred",
                 newdata = newdata,
                 newdata2 = list(V = map(1:(nrow(newdata)/n_models), ~vcov_big) %>% bdiag)) %>% 
      as_tibble() %>% 
      bind_cols(newdata) %>% 
      rename(estimate = Estimate,
             .lower = Q2.5,
             .upper = Q97.5)
    
    # Linear
    linear_conditional_means_summary <-
      posterior_epred(
        mdl_lin,
        newdata = newdata,
        newdata2 = list(V = map(1:(nrow(newdata)/n_models), ~vcov_big) %>% bdiag)) %>% 
      as.data.frame() %>% 
      set_names(newdata[[1]]) %>% 
      pivot_longer(cols = everything()) %>% 
      group_by(name) %>% 
      tidybayes::mean_qi() %>% 
      ungroup() %>% 
      arrange(as.numeric(name)) %>% 
      bind_cols(newdata) %>% 
      rename(estimate = value)
    
    
   # df_curve <- 
   #    weighted_conditional_means_summary %>%
   #    mutate(log_compute = log(!!sym(size_metric))) %>%
   #    arrange(log_compute) %>%
   #    mutate(
   #      slope = c(NA, diff(estimate) / diff(log_compute)),
   #      slope_lower = c(NA, diff(.lower) / diff(log_compute)),
   #      slope_upper = c(NA, diff(.upper) / diff(log_compute))
   #    )
   #  
   #  ggplot(df_curve, aes(x = flops_1e21_epoch, y = slope)) +
   #    geom_line() +
   #    geom_ribbon(aes(ymin = slope_lower, ymax = slope_upper), alpha = 0.2) +
   #    labs(title = "Estimated Derivative of Persuasion Curve",
   #         x = "log(Compute)", y = "Rate of Change (Δ Persuasion / Δ log-compute)") +
   #    scale_x_log10()
    
    # > 6. Compute OOM comparisons ----
    list_mod_types <- c("weighted", "linear")
    out_full_post_epred <-
      map(list_mod_types,
          function(mod_type) {
        
        if(mod_type == "weighted") {
          x <-
            pp_average(mdl_gam, mdl_lin,
                       weights = "loo",
                       method = "posterior_epred",
                       newdata = newdata,
                       newdata2 = list(V = map(1:(nrow(newdata)/n_models), ~vcov_big) %>% bdiag),
                       summary = F) %>% 
            as.data.frame() %>% 
            set_names(newdata[[1]])
        }
        
        if(mod_type == "linear") {
          x <-
            posterior_epred(
              mdl_lin,
              newdata = newdata,
              newdata2 = list(V = map(1:(nrow(newdata)/n_models), ~vcov_big) %>% bdiag)) %>% 
            as.data.frame() %>% 
            set_names(newdata[[1]])
        }
        
        
            if(oom_factor > 0) {
              
              get_col_names <- c(
                max(df_estimates[[size_metric]]),
                map_dbl(1:oom_factor, ~max(df_estimates[[size_metric]])*10^.x)
              )
              
              x <- x[, names(x) %in% get_col_names]
              
              new_columns <- x[, -1] - x[, 1]
              
              if(oom_factor > 1) {
                new_columns_summarise <- 
                  new_columns %>% 
                  pivot_longer(cols = everything()) %>% 
                  group_by(name) %>% 
                  tidybayes::mean_qi() %>% 
                  ungroup() %>% 
                  mutate(name = as.numeric(name)) %>% 
                  arrange(name)
              } else {
                new_columns_summarise <- 
                  new_columns %>% 
                  tidybayes::mean_qi() %>% 
                  add_column(name = get_col_names[2], .before = 1) %>% 
                  rename(value = y,
                         .lower = ymin,
                         .upper = ymax)
              }
            
        
      }
    
            # Return
            new_columns_summarise

    })
    
    names(out_full_post_epred) <- list_mod_types
    
    # > 7. Return ----
    list("estimates" = df_estimates,
         "static" = df_static,
         "mdl_lin" = mdl_lin,
         "mdl_gam" = mdl_gam,
         "loo" = out_loo,
         "mod_weights" = out_weights,
         "conditional_means" = weighted_conditional_means_summary,
         "oom_compare" = if(oom_factor > 0) { out_full_post_epred$weighted } else { NULL },
         "linear_conditional_means" = linear_conditional_means_summary,
         "linear_oom_compare" = if(oom_factor > 0) { out_full_post_epred$linear } else { NULL })
    
  }


# weighted_conditional_means_summary %>% 
#   ggplot(aes(x = flops_1e21_epoch, y = estimate)) +
#   geom_line() +
#   scale_x_log10() +
#   geom_point()

