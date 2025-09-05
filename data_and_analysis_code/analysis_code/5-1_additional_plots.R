
# THIS SCRIPT RECREATES FIG 3 BUT USES PPTS PERCEIVED INFORMATIVENESS NOT N CLAIMS

library(tidyverse)
library(broom)
library(estimatr)
library(brms)
library(metafor)
library(patchwork)
library(tidybayes)
library(ggrepel)
library(cowplot)
library(showtext)
library(sysfonts)
library(ggplot2)
library(ggnewscale)

df_list <- map(
  list.files("output/processed_data"),
  function(.x) {
    readRDS(paste0("output/processed_data/", .x))
  }
)

names(df_list) <- list.files("output/processed_data") %>% str_remove_all(".rds")


model_names <- 
  c(
    "gpt-4.5" = "GPT-4.5",
    "chatgpt-4o-latest" = "GPT-4o (3/25)",
    "gpt-4o" = "GPT-4o (8/24)",
    "grok-3" = "Grok-3-Beta",
    "gpt-3.5" = "GPT-3.5-Turbo",
    "llama-3-1-8b" = "llama3.1-8b",
    "llama-3-1-405b" = "llama3.1-405b"
  )


pt_names <-
  c(
    "base" = "Base",
    "sft" = "SFT",
    "rm" = "RM",
    "_and_" = " + "
  )


list_outcomes <- list("post_average", "post_average_imputed_with_pre")

#Globally set font to CMU Modern               

# --- 1. Load font files -------------------------------------------------------
# font_paths("~/Library/Fonts")   # <-- change to where CMU lives
# font_add(
#   "CMU Serif",                       # family name you will refer to
#   regular     = "cmunrm.otf",
#   bold        = "cmunbx.otf",   
#   italic      = "cmunti.otf",   
#   bolditalic  = "cmunbi.otf"    
# )

# --- 2. Turn on showtext ------------------------------------------------------
showtext_auto()                  

# --- 3. Make CMU Serif the global ggplot2 default ----------------------------
## Global theme
theme_set(theme_bw(base_family = "CMU Serif"))

## Force every text-bearing theme element to inherit that family
theme_update(
  text         = element_text(family = "CMU Serif"),
  axis.title   = element_text(family = "CMU Serif"),
  axis.text    = element_text(family = "CMU Serif"),
  legend.text  = element_text(family = "CMU Serif"),
  legend.title = element_text(family = "CMU Serif"),
  plot.title   = element_text(family = "CMU Serif"),
  strip.text   = element_text(family = "CMU Serif")
)

# --- 4. Tell every text-producing geom to use the same family ----------------
for (g in c("text", "label", "text_repel", "label_repel"))
  update_geom_defaults(g, list(family = "CMU Serif"))


for (i in list_outcomes) {
  
  #i <- list_outcomes[[1]]
  outcome_variable <- i
  # Figure 4 ----
  
  # --- Figure 4 · Panel A (Prompt means meta-analysis) ------------------------
  
  g1 <- df_list$df_prompt_means_meta_inform %>% 
    rename(tidy_out_estimate = estimate,
           tidy_out_std.error = std.error) %>% 
    #filter(outcome == outcome_variable) %>% 
    ggplot(
      aes(
        y = forcats::fct_reorder(prompt_id, tidy_out_estimate),   # order by effect size
        x = tidy_out_estimate
      )
    ) +
    
    ## ── Main points & 95 % CIs ────────────────────────────────────────────────
    geom_point(size = 4, shape = 21, fill = "black") +
    geom_errorbarh(
      aes(
        xmin = tidy_out_estimate - 1.96 * tidy_out_std.error,
        xmax = tidy_out_estimate + 1.96 * tidy_out_std.error
      ),
      height    = 0,
      linewidth = .4
    ) +
    
    ## ── Study-level points ────────────────────────────────────────────────────
    geom_point(
      data = df_list$df_prompt_means %>% 
        filter(outcome_id == outcome_variable,
               x_variable == "mean_inform") %>% 
        rename(tidy_out_estimate = x_value),
      aes(
        y      = prompt_id,
        x      = tidy_out_estimate,
        colour = dataset,
        shape  = dataset,
      ),
      position = position_dodge(.5),
      alpha    = .75,
      size     = 2
    ) +
    
    ## ── Zero reference line ──────────────────────────────────────────────────
    #geom_vline(xintercept = 0, linetype = "dashed", linewidth = .5, alpha = .6) +
    
    ## ── Numeric labels ───────────────────────────────────────────────────────
    geom_label(
      aes(label = sprintf("%.2f", tidy_out_estimate)),
      nudge_y      = .3,                
      size         = 5,               
      family       = "CMU Serif",
      fontface     = "bold",
      fill         = "white",
      label.size   = NA,                
      label.padding = unit(0, "lines"),  
      alpha         = 0.6
    ) +
    
    ## ── Scales & labels ──────────────────────────────────────────────────────
    scale_colour_manual(values = c("S1, chat 1" = "#6A994E", "S1, chat 2" = "#9D57D6", "S2" = "#142556", "S3" = "#669DC4")) +
    labs(
      y = "Prompt",
      x = "Perceived informativeness (0-100, 95% CI)"
    ) + scale_y_discrete(
      labels = function(x) {
        x |>
          stringr::str_replace_all("_", " ") |>  
          stringr::str_replace_all(" ", "\n") |> 
          stringr::str_to_title()                
      }
    ) +
    
    ## ── Theme tweaks to match previous figures ───────────────────────────────
    theme_bw(base_family = "CMU Serif") +
    theme(
      panel.grid            = element_blank(),
      panel.border          = element_blank(),
      axis.line             = element_line(linewidth = .3),
      axis.ticks            = element_line(linewidth = .3),
      axis.title            = element_text(size = 18, face = "bold"),
      axis.title.y          = element_text(vjust = 0.5, hjust = .5, margin  = margin(r = -0)),
      axis.title.x          = element_text(vjust = 0.5, hjust = .5, margin  = margin(t = 10)),
      axis.text             = element_text(size = 16),
      axis.text.y = element_text(
        colour = "black",  
        size   = 12
      ),
      legend.position       = c(.85, .2),
      legend.direction      = "vertical",
      legend.box.background = element_blank(),
      legend.title          = element_blank(),
      legend.text           = element_text(size = 16),
      plot.margin           = margin(5.5, 15.5, 15.5, 5.5)
    )
  
  
  # --- Figure 4 · Panel B (Informativeness vs. persuasion) -----------------
  
  df_slope <- df_list$df_estimates_brm_slope_inform %>% filter(outcome == outcome_variable)
  df_corr  <- df_list$df_estimates_brm_corr_inform %>% filter(outcome == outcome_variable)
  
  text_slope <- paste0(
    "Avg. slope = ", 
    df_slope %>% pull(1) %>% round(2) %>% format(nsmall = 2), " [",
    df_slope %>% pull(2) %>% round(2) %>% format(nsmall = 2), ", ",
    df_slope %>% pull(3) %>% round(2) %>% format(nsmall = 2), "]"
  )
  
  text_corr <- paste0(
    "Avg. corr = ", 
    df_corr %>% pull(1) %>% round(2) %>% format(nsmall = 2), " [",
    df_corr %>% pull(2) %>% round(2) %>% format(nsmall = 2), ", ",
    df_corr %>% pull(3) %>% round(2) %>% format(nsmall = 2), "]"
  )
  
  
  g2 <- df_list$df_prompt_means %>% 
    filter(
      outcome_id == outcome_variable,
      x_variable == "mean_inform"          # informedness
    ) %>% 
    mutate(
      prompt_id = if_else(prompt_id %in% "information", prompt_id, NA_character_)
    ) %>% 
    ggplot(aes(x = x_value, y = estimate, colour = dataset)) +
    
    ## ── Points, CIs, and regression line ─────────────────────────────────────
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0, linewidth = .4) +
    geom_errorbarh(aes(xmin = lwr, xmax = upr),
                   height = 0, linewidth = .4) +
    geom_smooth(method = "lm", se = FALSE, linewidth = .8, linetype = "dashed") +
    
    ## ── Highlight information prompt ─────────────────────────────────────────
    ggrepel::geom_text_repel(
      aes(label = prompt_id),
      family      = "CMU Serif",
      fontface    = "bold",
      max.overlaps = Inf,
      show.legend = FALSE,
      size = 5
    ) +
    
    ## ── Annotation: average slope & correlation ──────────────────────────────
    annotate(
      "text",
      #x        = 13, y = 67,               # adjust if needed
      x         = 68, y = 63,               # adjust if needed
      hjust     = 0, vjust = 1,
      size      = 5,
      family    = "CMU Serif",
      label     = paste0(text_slope, "\n", text_corr)
    ) +
    
    ## ── Scales & labels ──────────────────────────────────────────────────────
    scale_colour_manual(values = c("S1, chat 1" = "#6A994E", "S1, chat 2" = "#9D57D6", "S2" = "#142556", "S3" = "#669DC4")) +
    labs(
      x = "Perceived informativeness (0-100)",
      y = "Policy support (0–100)"
    ) +
    
    ## ── Theme adjustments for consistency ────────────────────────────────────
    theme_bw(base_family = "CMU Serif") +
    theme(
      panel.grid            = element_blank(),
      panel.border          = element_blank(),
      axis.line             = element_line(linewidth = .3),
      axis.ticks            = element_line(linewidth = .3),
      axis.title            = element_text(size = 18, face = "bold"),
      axis.title.y          = element_text(vjust = 0.5, hjust = .5, margin  = margin(r = 15), colour = "black"),
      axis.title.x          = element_text(vjust = 0.5, hjust = .5, margin  = margin(t = 10), colour = "black"),
      axis.text             = element_text(size = 16),
      legend.position       = c(.80, .4),
      legend.direction      = "vertical",
      legend.box.background = element_blank(),
      legend.title          = element_blank(),
      legend.text           = element_text(size = 16),
      plot.margin           = margin(5.5, 15.5, 15.5, 5.5)
    )
  
  
  # --- Figure 4 · Panel C (Model × prompt interaction) -------------------------
  
  g3 <- df_list$df_estimates_DiD_ates %>% 
    filter(outcome_id == outcome_variable) %>% 
    mutate(term = str_replace_all(term, model_names)) %>% 
    filter(!str_detect(term, "Grok")) %>%          # drop Grok models
    ggplot(aes(x = term, y = estimate, fill = factor(info))) +
    
    ## ── Bars, CIs, points ────────────────────────────────────────────────────
    geom_col(position = position_dodge(.9), alpha = .5) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width      = 0,
      linewidth  = .4,
      position   = position_dodge(.9)
    ) +
    geom_point(position = position_dodge(.9), show.legend = FALSE, size = 3) +
    
    ## ── Numeric labels ───────────────────────────────────────────────────────
    # geom_text(
    #   aes(label = sprintf("%.2f", estimate), y = estimate + 3),
    #   position  = position_dodge(.9),
    #   size      = 3,
    #   family    = "CMU Serif",
    #   fontface  = "bold",
    #   alpha = 0.75
    # ) +
    
  ## ── Facet by study ───────────────────────────────────────────────────────
  facet_grid(. ~ study,            
             scales = "free_x",    # each panel keeps its own x-axis
             space  = "free_x") +   # panel width ∝ no. of x breaks +
    
    ## ── Reference line ───────────────────────────────────────────────────────
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = .3, alpha = .5) +
    
    ## ── Scales & labels ──────────────────────────────────────────────────────
    scale_fill_manual(
      labels  = c("0" = "Other", "1" = "Information"),
      values  = c("0" = "grey70", "1" = "black")
    ) +
    labs(
      x    = "Model",
      y    = "Persuasive\neffect (pp)",
      fill = "Prompt:"
    ) +
    
    ## ── Theme: match earlier panels ──────────────────────────────────────────
    theme_bw(base_family = "CMU Serif") +
    theme(
      panel.grid            = element_blank(),
      panel.border          = element_blank(),
      axis.line             = element_line(linewidth = .3),
      axis.line.x           = element_blank(),
      axis.ticks            = element_line(linewidth = .3),
      axis.title            = element_text(size = 18, face = "bold"),
      axis.title.y          = element_text(vjust = 0.5, hjust = .5, margin  = margin(r = 10)),
      axis.text             = element_text(size = 16),
      axis.text.x           = element_blank(),   
      axis.ticks.x          = element_blank(),   
      axis.title.x          = element_blank(), 
      strip.background      = element_blank(),
      strip.placement = "outside",
      strip.text            = element_blank(),
      #legend.position       = c(.5, -.12),
      legend.position       = c(.5, -.075),
      legend.direction      = "horizontal",
      legend.box.background = element_blank(),
      legend.title          = element_text(size=16),
      legend.text           = element_text(family = "CMU Serif", size = 14),
      plot.margin           = margin(25.5, 15.5, 15.5, 5.5)
    ) +
    
    coord_cartesian(ylim = c(0, 18))
  
  
  # --- Figure 4 · Panel D (Informativeness ↑ for information-prompt) --------------
  
  g4 <- df_list$df_estimates_DiD_ates %>% 
    filter(
      outcome_id == "inform",
      !str_detect(term, "grok")
    ) %>% 
    mutate(term = str_replace_all(term, model_names)) %>% 
    ggplot(aes(x = term, y = estimate, fill = factor(info))) +
    
    ## ── Columns, CIs, points ─────────────────────────────────────────────────
    geom_col(position = position_dodge(.9), alpha = .5) +
    geom_errorbar(
      aes(ymin = conf.low, ymax = conf.high),
      width      = 0,
      linewidth  = .4,
      position   = position_dodge(.9)
    ) +
    geom_point(position = position_dodge(.9), size = 3, show.legend = FALSE) +
    
    ## ── Numeric labels just above each bar ───────────────────────────────────
    # geom_text(
    #   aes(label = sprintf("%.2f", estimate), y = estimate + 3),
    #   position  = position_dodge(.9),
    #   size      = 3,
    #   family    = "CMU Serif",
    #   fontface  = "bold",
    #   alpha = 0.75
    # ) +
    
  ## ── Facet by study ───────────────────────────────────────────────────────
  facet_grid(. ~ study,            
             scales = "free_x",    
             space  = "free_x",
             switch = "x") +  
    scale_x_discrete(
      labels = function(x) {
        x %>% 
          stringr::str_replace("GPT-4o \\(8/24\\)", "GPT-4o\n(8/24)") %>% 
          stringr::str_replace("GPT-4o \\(3/25\\)", "GPT-4o\n(3/25)")
      }
    ) +
    
    ## ── Scales & labels ──────────────────────────────────────────────────────
    scale_fill_manual(
      labels = c("0" = "Other", "1" = "Information"),
      values = c("0" = "grey70", "1" = "black")
    ) +
    labs(
      x    = "Model",
      y    = "Perceived\ninformativeness (0-100)",
      fill = "Prompt"
    ) +
    
    ## ── Theme for aesthetic parity ───────────────────────────────────────────
    theme_bw(base_family = "CMU Serif") +
    theme(
      panel.grid            = element_blank(),
      panel.border          = element_blank(),
      axis.line             = element_line(linewidth = .3),
      axis.ticks            = element_line(linewidth = .3),
      axis.title            = element_text(size = 18, face = "bold"),
      axis.title.y          = element_text(vjust = 0.5, hjust = .5, margin  = margin(r = 10)),
      axis.text             = element_text(size = 13),
      strip.background      = element_blank(),
      strip.text            = element_text(face = "bold", size = 14),
      strip.placement       = "outside",
      legend.position       = "none",          
      plot.margin           = margin(25.5, 15.5, 5.5, 5.5),
    ) +
    
    coord_cartesian(ylim = c(0, 100))
  
  
  # --- Figure 4 · Panel E (Main post-training effects) -------------------------
  temp_df <- 
    df_list$df_estimates_post_train_main_fx %>% 
    #filter(str_detect(outcome, "veracity", negate = T)) %>%
    filter(outcome %in% c(outcome_variable, "inform")) %>% 
    mutate(x_facet = paste0("Study ", study, "\n", str_to_sentence(type), " models"),
           term = str_replace_all(term, pt_names),
           outcome = ifelse(str_detect(outcome, "post_average"), "Persuasion (pp)", "Perceived informativeness (0-100)"))
  
  temp_df_wide <- 
    temp_df %>% 
    pivot_wider(id_cols = c(term, study, type), 
                names_from = outcome, 
                values_from = c(estimate, conf.low, conf.high)) 
  
  g5 <-
    temp_df_wide %>% 
    #mutate(labelpls = paste0("Study ", study, "\n", term, " vs. Base\n", str_to_sentence(type))) %>% 
    mutate(labelpls = paste0(term)) %>% 
    ggplot(aes(x = `estimate_Perceived informativeness (0-100)`, y = `estimate_Persuasion (pp)`, 
               color = paste0("Study ", factor(study)), shape = str_to_sentence(type))) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = `conf.low_Persuasion (pp)`, 
                      ymax = `conf.high_Persuasion (pp)`), width = 0, alpha = 0.5, linewidth = .75) +
    geom_errorbarh(aes(xmin = `conf.low_Perceived informativeness (0-100)`, 
                       xmax = `conf.high_Perceived informativeness (0-100)`), height = 0, alpha = 0.5, linewidth = .75) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_text_repel(aes(label = labelpls), show.legend = F, fontface = "bold", size =5) +
    theme_bw(base_family = "CMU Serif") +
    theme(
      panel.grid            = element_blank(),
      panel.border          = element_blank(),
      axis.line             = element_line(linewidth = .3),
      axis.ticks            = element_line(linewidth = .3),
      axis.title            = element_text(size = 18, face = "bold"),
      axis.title.y          = element_text(vjust = 0.5, hjust = .5, margin  = margin(r = 10)),
      axis.title.x          = element_text(vjust = 0.5, hjust = .5, margin  = margin(t = 10)),
      axis.text             = element_text(size = 16),
      strip.placement       = "bottom",
      strip.background      = element_blank(),
      strip.text            = element_text(face = "bold"),
      #legend.position       = c(.70, .15),
      legend.position       = c(.8, .15),
      legend.direction      = "vertical",
      legend.title          = element_blank(),
      legend.box.background = element_blank(),
      legend.text           = element_text(family = "CMU Serif", size = 16),
      plot.margin           = margin(5.5, 15.5, 15.5, 5.5)
    ) +
    labs(x = "Main effect of post-training on\nperceived informativeness (pp)",
         y = "Main effect of post-training on\npersuasion (pp)") +
    scale_color_manual(values = c("#142556", "#669DC4"))
  
  
  g <- (
    g1 + g2 + (g3 / g4) + g5
  ) +
    plot_layout(ncol = 2, heights = c(1.1, 1)) +
    plot_annotation(tag_levels = "A") &           
    theme(                                        
      plot.tag = element_text(size = 22, face = "bold",
                              family = "CMU Serif")
    )
  
  # > Save ----
  ggsave(plot = g, filename = paste0("output/figures/fig3_", outcome_variable, "__inform.pdf"),
         height = 13, width = 13)
  # ggsave(plot = g, filename = paste0("figures/fig4_", outcome_variable, ".pdf"),
  #        height = 13, width = 13)
  
}



