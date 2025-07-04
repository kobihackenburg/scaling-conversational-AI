
# library(tidyverse)
# library(patchwork)
# library(broom)
# library(readxl)

df_b <- readxl::read_xls("output/fact_checker_assessments/assessments/fact_checker_B.xls") %>% transmute(across(everything(), ~as.numeric(.x)))
df_a <- readxl::read_xlsx("output/fact_checker_assessments/assessments/fact_checker_A.xlsx") %>% transmute(across(everything(), ~as.numeric(.x)))

glimpse(df_b)
glimpse(df_a)

df_joined <-
  bind_rows(
  df_b %>% mutate(fc_id = "B"),
  df_a %>% mutate(fc_id = "A")
) %>% 
  mutate(fc_facts = case_when(
    is.na(`n_facts_if_you_disagree_what_is_your_number?`) ~ our_n_facts,
    !is.na(`n_facts_if_you_disagree_what_is_your_number?`) ~ `n_facts_if_you_disagree_what_is_your_number?`
  ),
  fc_veracity = case_when(
    is.na(`avg_fact_veracity_if_you_disagree_what_is_your_number?`) ~ our_avg_fact_veracity,
    !is.na(`avg_fact_veracity_if_you_disagree_what_is_your_number?`) ~ `avg_fact_veracity_if_you_disagree_what_is_your_number?`
  ))

df_avg <-
  df_joined %>% 
  pivot_wider(id_cols = "message_id", values_from = c(fc_facts, fc_veracity),
              names_from = fc_id) %>% 
  mutate(fc_facts_BA_mean = (fc_facts_B+fc_facts_A)/2,
         fc_veracity_BA_mean = (fc_veracity_B+fc_veracity_A)/2)


# Correlations ----

df_results <-
  bind_rows(
  
    # Correlation on N claims
    cor.test(df_joined[df_joined$fc_id=="B",]$our_n_facts, 
             df_joined[df_joined$fc_id=="B",]$fc_facts) %>% 
      tidy() %>% 
      mutate(type = "B_vs_LLM",
             var = "N claims"),
    
    cor.test(df_joined[df_joined$fc_id=="A",]$our_n_facts, 
             df_joined[df_joined$fc_id=="A",]$fc_facts) %>% 
      tidy() %>% 
      mutate(type = "A_vs_LLM",
             var = "N claims"),
    
    cor.test(df_joined[df_joined$fc_id=="A",]$our_n_facts, 
             df_avg$fc_facts_BA_mean) %>% 
      tidy() %>% 
      mutate(type = "meanAB_vs_LLM",
             var = "N claims"),
    
    cor.test(df_joined[df_joined$fc_id=="B",]$fc_facts, 
             df_joined[df_joined$fc_id=="A",]$fc_facts) %>% 
      tidy() %>% 
      mutate(type = "A_vs_B",
             var = "N claims"),
    
    # Correlation on veracity
    cor.test(df_joined[df_joined$fc_id=="B",]$our_avg_fact_veracity, 
             df_joined[df_joined$fc_id=="B",]$fc_veracity) %>% 
      tidy() %>% 
      mutate(type = "B_vs_LLM",
             var = "Accuracy"),
    
    cor.test(df_joined[df_joined$fc_id=="A",]$our_avg_fact_veracity, 
             df_joined[df_joined$fc_id=="A",]$fc_veracity) %>% 
      tidy() %>% 
      mutate(type = "A_vs_LLM",
             var = "Accuracy"),
    
    cor.test(df_joined[df_joined$fc_id=="A",]$our_avg_fact_veracity, 
             df_avg$fc_veracity_BA_mean) %>% 
      tidy() %>% 
      mutate(type = "meanAB_vs_LLM",
             var = "Accuracy"),
    
    cor.test(df_joined[df_joined$fc_id=="B",]$fc_veracity, 
             df_joined[df_joined$fc_id=="A",]$fc_veracity) %>% 
      tidy() %>% 
      mutate(type = "A_vs_B",
             var = "Accuracy")
  
)

g1 <- 
  df_joined %>% 
  pivot_longer(cols = fc_id) %>% 
  ggplot(aes(x = our_n_facts, y = fc_facts)) +
  theme_bw() +
  geom_jitter(height = 0.1, width = 0.1, alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~paste0("Fact-checker ", value)) +
  labs(y = "Fact-checker-identified N claims",
       x = "LLM-identified N claims",
       title = "Number of fact-checkable claims identified in 198 AI messages") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

g2 <-
  df_joined %>% 
  pivot_longer(cols = fc_id) %>% 
  ggplot(aes(x = our_avg_fact_veracity, y = fc_veracity)) +
  theme_bw() +
  geom_jitter(height = 1, width = 1, alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~paste0("Fact-checker ", value)) +
  labs(y = "Fact-checker accuracy score",
       x = "LLM accuracy score",
       title = "Accuracy scores of 198 AI messages") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

g3 <- 
  df_results %>% 
  mutate(type = fct_relevel(type, "A_vs_B", after = Inf)) %>% 
  ggplot(aes(x = type, y = estimate)) +
  theme_bw() +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  coord_cartesian(ylim = c(0, 1)) +
  facet_wrap(~fct_rev(var)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Correlation estimate (95% CI)",
       x = "Comparison",
       title = "Correlations between fact-checkers (A, B) and LLM") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  geom_text(aes(label = round(estimate, 2)), nudge_x = -0.2)

g <-
  g1 + g2 + g3 +
  plot_layout(ncol = 1)

g

ggsave(plot = g, filename = "output/supplement/fact_check_validation/fc_plot.pdf",
       height = 12,
        width = 10)
