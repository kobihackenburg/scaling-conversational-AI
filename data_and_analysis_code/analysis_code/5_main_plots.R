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

# df_list <- map(
#   list.files("plot_data/"),
#   function(.x) {
#     readRDS(paste0("plot_data/", .x))
#   }
# )
# 
# 
# names(df_list) <- list.files("plot_data/") %>% str_remove_all(".rds")

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
  
  # FIGURE 1 ──────────────────────────────────────────────────────────────────
  ## 1 ── Data prep ----------------------------------------------------------
  
  specifications <- df_list$df_estimates_scaling_curve %>%
    filter(outcome == outcome_variable)
  
  df_static <- df_list$df_estimates_static %>%
    filter(outcome_id == outcome_variable)
  
  # 1·1 Conditional means ----------------------------------------------------
  temp_cond_means <- specifications %>%
    select(-contains("metareg"), -list_data) %>%
    unnest(linear_cond_means)
  
  # 1·2 OLS dialogue estimates ----------------------------------------------
  temp_ols_dialogue <- specifications %>%
    select(-contains("metareg"), -list_data, -outcome) %>%
    filter(data_label %in% c("dev_pt", "chat_pt")) %>%
    unnest(ols_dialogue) %>%
    mutate(
      model = str_replace_all(model, model_names),
      model = str_replace_all(model, c(
        "-1-5"             = "1.5",
        "-3-1"             = "3.1",
        "0-5b"             = "0.5b",
        "qwen1.5-1-8b"     = "qwen1.5-1.8b"
      )),
      ## --- Single-letter label  ---------------------------------------
      label_short = str_replace_all(model, c(
        "qwen1\\.5"  = "Q",
        "llama3\\.1" = "L"
      ))
    )
  
  # 1·3 Trim conditional means to dialogue range ----------------------------
  max_flops_all  <- max(temp_ols_dialogue$flops_1e21_epoch) + 0.5
  max_flops_chat <- max(
    temp_ols_dialogue %>%
      filter(data_label == "chat_pt") %>%
      pull(flops_1e21_epoch)
  ) + 0.5
  
  temp_cond_means <- bind_rows(
    temp_cond_means %>%
      filter(data_label == "dev_pt",  flops_1e21_epoch <= max_flops_all),
    temp_cond_means %>%
      filter(data_label == "chat_pt", flops_1e21_epoch <= max_flops_chat),
    temp_cond_means %>% filter(data_label == "full")
  )
  
  # 1·4 OLS static estimate --------------------------------------------------
  temp_ols_static <- specifications %>%
    select(-outcome) %>%
    filter(data_label == "dev_pt") %>%
    unnest(ols_static)
  
  gpt4.5_static <- temp_ols_static %>%
    filter(study == 3) %>%
    pull(estimate)
  
  ## 2 ── Main scaling-curve plot -------------------------------------------
  
  pt_cols <- c(dev_pt = "#6a994e", chat_pt = "#9D57D6")
  
  g <- temp_cond_means %>%
    filter(data_label == "full", flops_1e21_epoch <= max_flops_all) %>%
    ggplot(aes(flops_1e21_epoch, estimate)) +
    theme_bw(base_family = "CMU Serif") +
    
    # Static reference -------------------------------------------------------
  geom_hline(yintercept = gpt4.5_static, linetype = "dashed", alpha = 0.5, linewidth = .65) +
    annotate(
      "text",
      x     = min(temp_ols_dialogue$flops_1e21_epoch),
      y     = gpt4.5_static + 0.0,
      label = paste0(
        "Static message\n(",
        round(gpt4.5_static, 2), "pp, GPT-4.5, Study 3)"
      ),
      hjust = 0, size = 6, colour = "grey20"
    ) +
    
    # Full curve -------------------------------------------------------------
  geom_line(linewidth = 1, colour = "black", linetype = "solid", alpha = 0.5) +
    scale_x_log10() +
    
    # Dialogue points --------------------------------------------------------
  geom_errorbar(
    data  = temp_ols_dialogue,
    aes(ymin = conf.low, ymax = conf.high, colour = data_label),
    width = 0, alpha = 0.35
  ) +
    geom_point(
      data  = temp_ols_dialogue,
      aes(colour = data_label, shape = factor(study)),
      size  = 7
    ) +
    
    # Developer PT -----------------------------------------------------------
  geom_line(
    data      = temp_cond_means %>% filter(data_label == "dev_pt"),
    colour    = pt_cols["dev_pt"],
    linewidth = 1, linetype = "solid", alpha = 0.45
  ) +
    geom_ribbon(
      data   = temp_cond_means %>% filter(data_label == "dev_pt"),
      aes(ymin = .lower, ymax = .upper),
      fill   = pt_cols["dev_pt"],
      alpha  = 0.08, colour = NA
    ) +
    
    # Chat PT ----------------------------------------------------------------
  geom_line(
    data      = temp_cond_means %>% filter(data_label == "chat_pt"),
    colour    = pt_cols["chat_pt"],
    linewidth = 1, linetype = "solid", alpha = 0.45
  ) +
    geom_ribbon(
      data   = temp_cond_means %>% filter(data_label == "chat_pt"),
      aes(ymin = .lower, ymax = .upper),
      fill   = pt_cols["chat_pt"],
      alpha  = 0.08, colour = NA
    ) +
    
    # Scales & labels --------------------------------------------------------
  scale_color_manual(values = pt_cols,
                     labels  = c(dev_pt = "Developer", chat_pt = "Chat")) +
    labs(
      x      = "Effective compute (FLOPs 1e21)",
      y      = "Estimated persuasive effect (pp, 95% CI)",
      shape  = "Study:",
      colour = "Post-training:",
      fill   = "Post-training:"
    ) +
    guides(
      shape = guide_legend(override.aes = list(colour = "grey40", fill = "grey40"))
    ) +
    
    # Theme tweaks -----------------------------------------------------------
  theme(
    plot.margin           = margin(5.5, 5.5, 10.5, 25.5),
    panel.grid.minor      = element_blank(),
    panel.grid.major      = element_blank(),
    legend.position       = c(0.8, 0.20),
    legend.direction      = "horizontal",
    legend.box.background = element_blank(),
    legend.title = element_text(family = "CMU Serif", size = 16),
    legend.text           = element_text(family = "CMU Serif", size = 16),
    plot.title            = element_text(hjust = 0.5, face = "bold"),
    panel.border          = element_blank(),
    axis.line             = element_line(linewidth = 0.3),
    axis.title.x          = element_text(vjust = -2, margin = margin(t = 5),  size = 18, face = "bold"),
    axis.title.y          = element_text(
      vjust = 0.5, hjust = 0.5, #to place on top: angle =0, vjust = 1.1, margin (r = -66)
      margin = margin(r = 10), size = 18, face = "bold"
    ),
    axis.text.x           = element_text(size = 16, colour = "black"),
    axis.text.y           = element_text(size = 16, colour = "black")
  ) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    
    # Point labels -----------------------------------------------------------
  ggrepel::geom_text_repel(
    data        = temp_ols_dialogue,
    aes(label = ifelse(!is.na(label_short), label_short, model), 
        colour = data_label),
    fontface    = "bold",
    size        = 5,
    show.legend = FALSE
  )
  
  
  model_key <- tibble(
    flops_1e21_epoch = c(-Inf, -Inf),     # keeps points off-panel
    estimate         = c(-Inf, -Inf),
    model_lab        = factor(
      c("Q = qwen1.5", "L = llama3.1"),
      levels = c("Q = qwen1.5", "L = llama3.1")
    )
  )
  
  g <- g +
    ggnewscale::new_scale("shape") +      # generic call for a new shape scale
    geom_point(
      data  = model_key,
      aes(flops_1e21_epoch, estimate, shape = model_lab),
      size  = 0,
      alpha = 0
    ) +
    scale_shape_manual(
      name   = "Model:",
      values = c(15, 15),
      guide  = guide_legend(
        order = 3,
        override.aes = list(size = 5, colour = "grey20")
      )
    )
  
  g  # display
  
  ## 3 ── Extrapolation inset -------------------------------------------------
  
  current_frontier_est <- temp_cond_means %>%
    filter(data_label == "full", flops_1e21_epoch <= max_flops_all) %>%
    pull(estimate) %>%
    max()
  
  g_inset <- specifications %>%
    filter(data_label == "full") %>%
    select(linear_oom_compare) %>%
    unnest(linear_oom_compare) %>%
    mutate(
      name = factor(c("+1 OOM", "+2 OOM"), levels = c("+1 OOM", "+2 OOM"))
    ) %>%
    ggplot(aes(name, value)) +
    geom_point(size = 2) +
    geom_errorbar(
      aes(ymin = .lower, ymax = .upper),
      width = 0, linewidth = 0.5, alpha = 0.5
    ) +
    annotate(
      "text",
      x     = 0.17, y = 0, hjust = 0.5, size = 3,
      label = paste0(
        "Current frontier\npersuasion\n(",
        round(current_frontier_est, 2), " pp)"
      )
    ) +
    coord_cartesian(xlim = c(0.25, 2), ylim = c(-2, 5), clip = "off") +
    labs(
      title = "Extrapolating the curve\n+2 orders of magnitude (OOM)",
      x = NULL, y = NULL
    ) +
    theme_bw(base_family = "CMU Serif") +
    theme(
      panel.grid      = element_blank(),
      axis.title      = element_blank(),
      axis.ticks.x    = element_blank(),
      axis.line.x     = element_blank(),
      axis.ticks.y    = element_blank(),
      axis.text.y     = element_blank(),
      plot.title      = element_text(hjust = 0.5, size = 12),
      axis.text.x     = element_text(vjust = 12, margin = margin(t = -8), face = "bold", size = 11),
      panel.border    = element_rect(colour = "grey20", fill = NA, linewidth = 0.3)
    ) +
    # Draw x-axis baseline
    geom_segment(
      inherit.aes = FALSE,
      x = 0.75, xend = 2.25,
      y = 0,    yend = 0,
      linewidth = 0.45, linetype = "dashed", alpha = 0.25
    ) +
    geom_text(
      aes(label = paste0("+", round(value, 2))),
      nudge_x = 0.3, nudge_y = 0.05
    )
  
  ## 4 ── Combine main plot & inset ------------------------------------------
  
  # g_out <- ggdraw() +
  #   draw_plot(g) +
  #   draw_plot(g_inset, x = 0.695, y = 0.125, width = 0.25, height = 0.35)
  
  g_out <- g  # uncomment to save without the inset
  
  ## 5 ── Save ---------------------------------------------------------------
  
  ggsave(
    plot     = g_out,
    filename = paste0("output/figures/fig1_", outcome_variable, ".pdf"),
    height   = 8, width = 12
  )
  
  # ── FIGURE 2 ────────────────────────────────────────────────────────────
  # Libraries -------------------------------------------------------------
  theme_set(theme_get() + theme(text = element_text(family = "CMU Serif")))
  
  # 1. CONSTANTS ----------------------------------------------------------
  
  model_cols <- c(
    "llama3.1-8b"   = "#DABDEF",
    "llama3.1-405b" = "#9D57D6",
    "GPT-3.5-Turbo" = "#A6C893",
    "GPT-4o (8/24)" = "#6A994E",
    "GPT-4o (3/25)" = "#77856F",
    "GPT-4.5"       = "#25351C",
    "Grok-3-Beta"   = "#AEC2B2"
  )
  model_order <- names(model_cols)
  
  ylims      <- c(0, 15)
  pos_dodge  <- position_dodge(.40)
  
  base_theme <- theme_bw(base_family = "CMU Serif") +
    theme(
      panel.grid   = element_blank(),
      plot.margin = margin(5.5, 15.5, 5.5, 5.5),
      panel.border = element_blank(),
      axis.line.x.bottom = element_line(.3, colour = "black"),
      axis.line.y.left   = element_line(.3, colour = "black"),
      axis.line    = element_line(.3),
      axis.ticks   = element_line(.3),
      axis.title   = element_text(size = 16, face = "bold"),   # ← named `size`
      axis.title.y = element_text(vjust = .5, hjust = .5, margin = margin(5.5, 15.5, 5.5, 5.5)),
      axis.title.x = element_text(vjust = .5, hjust = .5, size = 16),
      axis.text    = element_text(size = 14),                  # ← named `size`
      strip.placement  = "outside",
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold"),
      plot.title       = element_text(hjust = .5, face = "bold"),
      plot.subtitle    = element_text(hjust = .5, face = "bold.italic"),
      legend.position  = "none"
    )
  
  # 2. HELPERS ------------------------------------------------------------
  clean_vars <- function(df, outcome_var) {
    df %>%
      mutate(
        outcome = recode(outcome, !!outcome_var := "Persuasion (pp)"),
        model   = str_replace_all(model, model_names) |> factor(levels = model_order),
        term    = str_replace_all(term,  pt_names)    |> factor(levels = c("Base", "RM", "SFT", "SFT + RM"))
      )
  }
  
  make_panel <- function(df, lab) {
    # Create a base plot without lines
    p <- df %>%
      ggplot(aes(term, estimate, colour = model)) +
      base_theme +
      geom_point(size = 4, position = pos_dodge) +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                    width = 0, alpha = .65, position = pos_dodge) +
      # geom_label(
      #   aes(y = estimate + 1.5,
      #       label = sprintf("%.2f", estimate)),
      #   position      = pos_dodge,
      #   size          = 3,
      #   fontface      = "bold",
      #   fill          = "white",
      #   label.size    = NA,
      #   label.padding = unit(0.00, "lines"),
      #   label.r       = unit(0.00, "lines"),
      #   show.legend   = FALSE
      # ) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = .5) +
      coord_cartesian(ylim = ylims) +
      scale_colour_manual(values = model_cols, breaks = model_order) +
      labs(
        x        = NULL,
        y        = "Estimated persuasive effect\n(pp, 95 % CI)",
        title    = lab$ttl,
        subtitle = lab$sub
      )
    
    # Add lines only for Base-RM connection
    if (any(df$term %in% c("Base", "RM"))) {
      p <- p + 
        geom_line(data = filter(df, term %in% c("Base", "RM")),
                  aes(group = model),
                  linewidth = .3, linetype = "dashed", position = pos_dodge)
    }
    
    # Add lines only for SFT-SFT+RM connection
    if (any(df$term %in% c("SFT", "SFT + RM"))) {
      p <- p + 
        geom_line(data = filter(df, term %in% c("SFT", "SFT + RM")),
                  aes(group = model),
                  linewidth = .3, linetype = "dashed", position = pos_dodge)
    }
    
    p
  }
  
  # adds dashed baseline to every panel and, optionally, the label -----------
  add_baseline <- function(plot, y, label = NULL, linewidth = 0.3) {
    plot <- plot +
      geom_hline(yintercept = y, linetype = "dashed", alpha = .75, linewidth = linewidth)
    
    if (!is.null(label)) {
      plot <- plot +
        annotate("text", x = .5, y = y + 1,
                 label = label, hjust = 0, size = 4, colour = "grey30", fontface = "bold")
    }
    plot
  }
  
  final_touch <- function(plot, show_y = FALSE) {
    plot +
      labs(title = NULL, subtitle = NULL) +
      guides(colour = guide_legend(title = NULL)) +
      theme(
        legend.position      = c(.98, .08),
        legend.justification = c(1, 0),
        legend.background    = element_blank(),
        legend.key           = element_blank(),
        legend.key.size      = unit(.4, "cm"),
        legend.text = element_text(size = 12),
        
        ## ── only keep the y-axis on the first panel ──────────────────────
        axis.title.y   = if (show_y) element_text(family = "CMU Serif") else element_blank(),
        axis.text.y    = if (show_y) element_text(size = 16)            else element_blank(),
        axis.ticks.y   = if (show_y) element_line(.3)                   else element_blank(),
        axis.line.y.left =
          if (show_y) element_line(.3, colour = "black")                else element_blank()
      )
  }
  
  # 3. PANEL SPECIFICATION -------------------------------------------------
  baseline <- df_list$df_estimates_post_train_vs_control %>%
    filter(model == "gpt-4o",
           outcome == outcome_variable,
           term == "base",
           study == 2) %>%
    pull(estimate)
  
  panels <- tibble::tribble(
    ~filter_expr,                                 ~label,                       ~x_limits,      ~show_y, ~show_baseline_label,
    quote(str_detect(model, "llama")  & study == 2), list(ttl = "Study 2"),           NULL,           TRUE,    TRUE,
    quote(str_detect(model, "gpt|grok") & study == 2), list(ttl = "Study 2",
                                                            sub = "Developer models"), NULL,           TRUE,    FALSE,
    quote(str_detect(model, "gpt|grok") & study == 3 &
            term %in% c("base", "rm")),              list(ttl = "Study 3",
                                                          sub = "Developer models"), c("Base", "RM"), TRUE,    FALSE
  )
  
  # 4. BUILD PLOTS ---------------------------------------------------------
  plots <- panels %>%
    pmap(function(filter_expr, label, x_limits, show_y, show_baseline_label) {
      
      df <- df_list$df_estimates_post_train_vs_control %>%
        filter(eval(filter_expr), outcome == outcome_variable) %>%
        clean_vars(outcome_variable)
      
      p <- make_panel(df, label)
      if (!is.null(x_limits))
        p <- p + scale_x_discrete(limits = x_limits)
      
      # Only add baseline for panel A
      if (show_baseline_label) {
        p <- p %>% add_baseline(baseline, "GPT-4o\n(8/24)", linewidth = .75)  
      }
      
      p %>% final_touch(show_y)
    })
  
  # 5. ASSEMBLE & SAVE -----------------------------------------------------
  x_shared <- "Persuasion post-training"   # shared x-axis label
  plots[[3]] <- plots[[3]] + theme(axis.title.y = element_blank())
  plots[[2]] <- plots[[2]] + theme(axis.title.y = element_blank())
  g_final <- wrap_plots(plots, nrow = 1, widths = c(2, 1, 1)) +
    plot_annotation(
      tag_levels = "A",
      caption    = x_shared            # put the label in the caption slot
    ) &
    theme(
      plot.tag              = element_text(size = 22, face = "bold"),
      plot.caption          = element_text(
        family = "CMU Serif",
        face   = "bold",
        size   = 18,
        hjust  = .63          
      ),
      plot.caption.position = "plot"   # places caption underneath the whole plot
    )
  
  ggsave(sprintf("output/figures/fig2_%s.pdf", outcome_variable),
         g_final, width = 10, height = 5)
  
  
  
  
  # # ---- Figure 3: Personalization Effects -----------------------------------
  # 
  # ## 1  Prepare data -----------------------------------------------------------
  # temp_data <- df_list$df_estimates_personalization %>% 
  #   mutate(
  #     x_facet    = if_else(is.na(dialogue),
  #                          paste0("S", study),
  #                          paste0("S", study, " chat ", dialogue)),
  #     ppt_type   = ppt_type %>% 
  #       fct_relevel("sft_and_rm", after = Inf) %>% 
  #       str_remove_all("-only") %>% 
  #       str_replace_all(pt_names),
  #     model_type = paste0(str_to_sentence(model_type), " models")
  #   ) %>% 
  #   filter(outcome_id == outcome_variable)
  # 
  # ## 2  Build per-prompt panels ------------------------------------------------
  # g_out <- map(
  #   unique(temp_data$ppt_type),
  #   function(ppt) {
  #     
  #     g <- temp_data %>% 
  #       filter(ppt_type == ppt) %>% 
  #       ggplot(aes(x = ppt_type,
  #                  y = estimate,
  #                  colour = x_facet,
  #                  shape  = model_type)) +
  #       theme_bw(base_family = "CMU Serif") +
  #       geom_point(position = position_dodge(0.9), size = 4, alpha = .75) +
  #       geom_errorbar(
  #         aes(ymin = conf.low, ymax = conf.high),
  #         width    = 0,
  #         alpha    = 0.45,
  #         position = position_dodge(0.9)
  #       ) +
  #       facet_wrap(~ppt_type, scales = "free_x", strip.position = "bottom") +
  #       geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  #       labs(x = ppt, y = NULL) +
  #       theme(
  #         plot.title            = element_text(hjust = 0.5, face = "bold"),
  #         legend.position       = "none",
  #         legend.title          = element_blank(),
  #         strip.placement       = "outside",
  #         strip.background      = element_blank(),
  #         strip.text            = element_blank(),
  #         panel.grid            = element_blank(),
  #         panel.border          = element_blank(),
  #         axis.line.x           = element_line(linewidth = 0.3),
  #         axis.text             = element_blank(),
  #         axis.ticks            = element_blank(),
  #         legend.box.background = element_blank(),
  #         text                  = element_text(family = "CMU Serif"),
  #         legend.text           = element_text(family = "CMU Serif"),
  #         axis.title.x          = element_text(size = 12),
  #         axis.title.y          = element_text(size = 12, face = "bold") 
  #       ) +
  #       geom_text(
  #         aes(label = format(round(estimate, 2), nsmall = 2), y = 2.5),
  #         position = position_dodge(0.9),
  #         size     = 3,
  #         fontface = "bold"
  #       ) +
  #       coord_cartesian(ylim = c(-0.5, 2.5)) +
  #       scale_color_manual(
  #         values = c(
  #           "S1 chat 1" = "#6a994e",
  #           "S1 chat 2" = "#669DC4",
  #           "S2"        = "#142556",
  #           "S3"        = "#9D57D6"
  #         )
  #       )
  #     
  #     # Add y-axis and legend only for the baseline panel
  #     if (ppt == "Base") {
  #       g <- g +
  #         theme(
  #           legend.position  = c(0.30, 0.70),
  #           legend.text      = element_text(size = 8),
  #           legend.key.size  = unit(0.5, "lines"),
  #           legend.spacing.y = unit(0, "cm"),
  #           axis.line.y      = element_line(linewidth = 0.3),
  #           axis.text.y      = element_text(size = 12),
  #           axis.ticks.y     = element_line(),
  #           axis.title.y     = element_text(angle = 0, vjust = 0.5, hjust = 0.5)
  #         ) +
  #         labs(
  #           y = "Persuasive effect\nof personalization\n(pp, 95% CI)",
  #           x = paste0(ppt, " (prompt-only)")
  #         )
  #     }
  #     
  #     g
  #   }
  # )
  # 
  # ## 3  Meta-analytic panel ----------------------------------------------------
  # meta_personalize <- df_list$df_estimates_personalization %>% 
  #   filter(outcome_id == i) %>% 
  #   rma(yi = estimate, sei = std.error, method = "FE", data = .) %>% 
  #   tidy(conf.int = TRUE)
  # 
  # g_meta <- meta_personalize %>% 
  #   mutate(ppt_type = "Meta-analytic") %>% 
  #   ggplot(aes(x = 1, y = estimate)) +
  #   theme_bw(base_family = "CMU Serif") +
  #   geom_point(size = 0.5, shape = 4, stroke = 3, colour = "#D1024E") +
  #   geom_errorbar(
  #     aes(ymin = conf.low, ymax = conf.high),
  #     width  = 0,
  #     colour = "#D1024E"
  #   ) +
  #   facet_wrap(~ppt_type, scales = "free_x", strip.position = "bottom") +
  #   geom_hline(yintercept = 0, linetype = "dashed") +
  #   labs(x = "Meta-analytic", y = NULL) +
  #   theme(
  #     plot.title            = element_text(hjust = 0.5, face = "bold"),
  #     legend.position       = "none",
  #     legend.title          = element_blank(),
  #     strip.placement       = "outside",
  #     strip.background      = element_blank(),
  #     strip.text            = element_blank(),
  #     panel.grid            = element_blank(),
  #     panel.border          = element_blank(),
  #     axis.line.x           = element_line(linewidth = 0.3),
  #     axis.text             = element_blank(),
  #     axis.ticks            = element_blank(),
  #     legend.box.background = element_blank(),
  #     panel.background      = element_rect(fill = alpha("grey", 0.3)),
  #     text                  = element_text(family = "CMU Serif"),
  #     legend.text           = element_text(family = "CMU Serif"),
  #     axis.title.x          = element_text(size = 12),
  #     axis.title.y          = element_text(size = 12, face = "bold")
  #   ) +
  #   geom_text(
  #     aes(label = format(round(estimate, 2), nsmall = 2), y = 2.5),
  #     size     = 3,
  #     colour   = "#D1024E",
  #     fontface = "bold"
  #   ) +
  #   coord_cartesian(ylim = c(-0.5, 2.5))
  # 
  # ## 4  Assemble panels & save -------------------------------------------------
  # g <- g_out[[1]] + g_out[[2]] + g_out[[3]] + g_out[[4]] + g_meta +
  #   plot_layout(nrow = 1, widths = c(4, 2, 1, 1, 1.2)) +
  #   plot_annotation(caption = "Personalization method") &
  #   theme(
  #     plot.caption          = element_text(family = "CMU Serif",
  #                                          face   = "bold",
  #                                          size   = 12,
  #                                          hjust  = .63),
  #     plot.caption.position = "plot",
  #     plot.margin           = margin(0, 5, 5, 5, "pt")
  #   )
  # 
  # g  # (prints the plot)
  # 
  # ggsave(
  #   plot     = g,
  #   filename = paste0("figures/fig3_", outcome_variable, ".pdf"),
  #   height   = 4,
  #   width    = 10
  # )

  
  
  # Figure 4 ----
  
  # --- Figure 4 · Panel A (Prompt–vs-none meta-analysis) ------------------------
  
  g1 <- df_list$df_estimates_prompt_vs_none_meta %>% 
    filter(outcome == outcome_variable) %>% 
    ggplot(
      aes(
        y = forcats::fct_reorder(term, tidy_out_estimate),   # order by effect size
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
      data = df_list$df_estimates_prompt_vs_none_raw %>% 
        filter(outcome == outcome_variable) %>% 
        rename(tidy_out_estimate = estimate),
      aes(
        y      = term,
        x      = tidy_out_estimate,
        colour = dataset,
        shape  = dataset,
      ),
      position = position_dodge(.5),
      alpha    = .75,
      size     = 2
    ) +
    
    ## ── Zero reference line ──────────────────────────────────────────────────
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = .5, alpha = .6) +
    
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
      x = "Persuasive effect vs.\nbasic prompt (pp, 95% CI)"
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
  
  
  # --- Figure 4 · Panel B (Information density vs. persuasion) -----------------
  
  df_slope <- df_list$df_estimates_brm_slope %>% filter(outcome == outcome_variable)
  df_corr  <- df_list$df_estimates_brm_corr %>% filter(outcome == outcome_variable)
  
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
      x_variable == "mean_n_facts"          # information density
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
      x        = 8, y = 63,               # adjust if needed
      hjust     = 0, vjust = 1,
      size      = 5,
      family    = "CMU Serif",
      label     = paste0(text_slope, "\n", text_corr)
    ) +
    
    ## ── Scales & labels ──────────────────────────────────────────────────────
    scale_colour_manual(values = c("S1, chat 1" = "#6A994E", "S1, chat 2" = "#9D57D6", "S2" = "#142556", "S3" = "#669DC4")) +
    labs(
      x = "Information density (N claims)",
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
  
  
  # --- Figure 4 · Panel D (Info-density ↑ for information-prompt) --------------
  
  g4 <- df_list$df_estimates_DiD_ates %>% 
    filter(
      outcome_id == "convo_n_facts_total",
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
      y    = "Information\ndensity (N claims)",
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
    
    coord_cartesian(ylim = c(0, 32))
  
  # > Panel 5 ----
  
  # --- Figure 4 · Panel E (Main post-training effects) -------------------------
  temp_df <- 
    df_list$df_estimates_post_train_main_fx %>% 
    #filter(str_detect(outcome, "veracity", negate = T)) %>%
    filter(outcome %in% c(outcome_variable, "convo_n_facts_total")) %>% 
    mutate(x_facet = paste0("Study ", study, "\n", str_to_sentence(type), " models"),
           term = str_replace_all(term, pt_names),
           outcome = ifelse(str_detect(outcome, "post_average"), "Persuasion (pp)", "Information density (N claims)"))
  
  temp_df_wide <- 
    temp_df %>% 
    pivot_wider(id_cols = c(term, study, type), 
                names_from = outcome, 
                values_from = c(estimate, conf.low, conf.high)) 
  
  g5 <-
    temp_df_wide %>% 
    #mutate(labelpls = paste0("Study ", study, "\n", term, " vs. Base\n", str_to_sentence(type))) %>% 
    mutate(labelpls = paste0(term)) %>% 
    ggplot(aes(x = `estimate_Information density (N claims)`, y = `estimate_Persuasion (pp)`, 
               color = paste0("Study ", factor(study)), shape = str_to_sentence(type))) +
    geom_point(size = 5) +
    geom_errorbar(aes(ymin = `conf.low_Persuasion (pp)`, 
                      ymax = `conf.high_Persuasion (pp)`), width = 0, alpha = 0.5, linewidth = .75) +
    geom_errorbarh(aes(xmin = `conf.low_Information density (N claims)`, 
                       xmax = `conf.high_Information density (N claims)`), height = 0, alpha = 0.5, linewidth = .75) +
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
    labs(x = "Main effect of post-training on\ninformation density (N claims)",
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
  ggsave(plot = g, filename = paste0("output/figures/fig3_", outcome_variable, ".pdf"),
         height = 13, width = 13)
  # ggsave(plot = g, filename = paste0("figures/fig4_", outcome_variable, ".pdf"),
  #        height = 13, width = 13)
  
  
  
  # Figure 5 ----
  # Add model sizes
  df_temp <-
    df_list$df_estimates_veracity_infodensity_by_models %>%
    rename(model = term) %>% 
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
  
  # Add chat/dev tune
  df_temp <-
    df_temp %>% 
    mutate(tune_type = case_when(str_detect(model, "gpt|grok|chat") ~ "Developer-tuned",
                                 str_detect(model, "gpt|grok|chat", negate = T) ~ "Chat-tuned"))
  
  df_temp <-
    df_temp %>% 
    filter(dialogue %in% c(1, NA))
  
  df_temp <-
    df_temp %>% 
    mutate(model = str_replace_all(model, model_names)) %>% 
    mutate(model = str_replace_all(model, c("-1-5" = "1.5", 
                                            "-3-1" = "3.1",
                                            "0-5b" = "0.5b"))) %>% 
    mutate(model = str_replace_all(model, c("qwen1.5-1-8b" = "qwen1.5-1.8b"))) %>% 
    mutate(dataset = ifelse(is.na(dialogue), 
                            paste0("S", study), 
                            paste0("S", study, " chat ", dialogue))
    )
  
  list_outcomes_accuracy <- list("convo_mean_veracity", "convo_prop_facts_above_veracity_threshold")
  
  map(list_outcomes_accuracy,
      function(.x) {
        
        if(.x=="convo_mean_veracity") {
          y_label       <- "Avg. accuracy of\nAI messages (0-100)"
          y_label_short <- "Avg. accuracy (0-100)"
          y_text_pos    <- 85
          lab_accuracy  <- "Avg. accuracy (pp)"
        } else {
          y_label <- "Mean proportion of\naccurate claims"
          y_label_short <- "Mean proportion of\naccurate claims"
          y_text_pos    <- 0.95
          lab_accuracy  <- "Claims >50/100 accuracy (pp)"
        }
        
        # > Panel 1 ----  (DROP-IN REPLACEMENT FOR g1)
        
        temp_cond_means <-
          df_list$df_estimates_scaling_curve_accuracy %>% 
          filter(outcome_y == .x) %>% 
          select(data_label, linear_cond_means) %>% 
          unnest(linear_cond_means)
        
        temp_ols_dialogue <- df_temp %>% 
          filter(outcome_id == .x) %>% 
          ## --- NEW: single-letter labels -------------------------------------------
        mutate(
          label_short = str_replace_all(
            model,
            c(
              "qwen1\\.5"  = "Q",
              "llama3\\.1" = "L"
            )
          )
        )
        
        g1 <-
          temp_cond_means %>%
          filter(data_label == "full") %>%
          ggplot(aes(x = flops_1e21_epoch, y = estimate)) +
          theme_bw(base_family = "CMU Serif") +
          scale_x_log10() +
          theme(
            plot.margin = margin(5.5, 5.5, 5.5, 5.5),
            text                = element_text(family = "CMU Serif"),
            panel.border        = element_blank(),
            axis.line.x         = element_line(colour = "black", linewidth = 0.3),
            axis.line.y         = element_line(colour = "black", linewidth = 0.3),
            axis.title.x        = element_text(vjust = 0,  size = 18, face = "bold",
                                               margin = margin(10.5, 5.5, 5.5, 5.5)),
            axis.title.y        = element_text(vjust = 0.5, hjust = 0.5,
                                               margin = margin(r = 10),
                                               size = 18, face = "bold"),
            legend.position     = c(0.7, 0.2),
            legend.direction    = "horizontal",
            legend.box.background = element_blank(),
            legend.text         = element_text(size = 16),
            legend.title         = element_text(size = 16),
            axis.text           = element_text(size = 16),
            axis.ticks          = element_line(linewidth = 0.25),
            plot.title          = element_text(hjust = 0.5, face = "bold"),
            panel.grid          = element_blank()
          ) +
          
          geom_point(
            data  = temp_ols_dialogue,
            size  = 5,
            aes(x = flops_1e21_epoch, y = estimate,
                color = tune_type, shape = factor(study))
          ) +
          
          geom_errorbar(
            data  = temp_ols_dialogue,
            aes(ymin = conf.low, ymax = conf.high, color = tune_type),
            width = 0, alpha = 0.5
          ) +
          
          geom_line(
            data      = temp_cond_means %>% filter(data_label == "dev_pt"),
            linewidth = 1, color = "#6a994e", alpha = 0.5, linetype = "dashed"
          ) +
          geom_ribbon(
            data   = temp_cond_means %>% filter(data_label == "dev_pt"),
            aes(ymin = .lower, ymax = .upper),
            alpha  = 0.05, color = NA, fill = "#6a994e"
          ) +
          
          geom_line(
            data      = temp_cond_means %>% filter(data_label == "chat_pt"),
            linewidth = 1, color = "#9D57D6", alpha = 0.5, linetype = "dashed"
          ) +
          geom_ribbon(
            data   = temp_cond_means %>% filter(data_label == "chat_pt"),
            aes(ymin = .lower, ymax = .upper),
            alpha  = 0.05, color = NA, fill = "#9D57D6"
          ) +
          
          scale_color_manual(
            labels  = c("dev_pt"  = "Developer",
                        "chat_pt" = "Chat"),
            values  = c("#9D57D6", "#6a994e")
          ) +
          
          ggrepel::geom_text_repel(
            data        = temp_ols_dialogue,
            aes(
              label = ifelse(!is.na(label_short), label_short, model),
              color = tune_type
            ),
            show.legend = FALSE,
            family      = "CMU Serif",
            fontface    = "bold",
            size        = 5
          ) +
          
          labs(
            y     = y_label,
            x     = "Effective compute (FLOPs 1e21)",
            title = NULL,
            shape = "Study:",
            color = "Post-training:",
            fill  = "Post-training:"
          ) +
          guides(
            shape = guide_legend(override.aes = list(colour = "grey40",
                                                     fill   = "grey40"))
          )
          # ---  add model key to legend ----------------------------------------------
        model_key <- tibble(
          flops_1e21_epoch = c(-Inf, -Inf),       # keeps points off–canvas
          estimate         = c(-Inf, -Inf),
          model_lab        = factor(
            c("Q = qwen1.5", "L = llama3.1"),
            levels = c("Q = qwen1.5", "L = llama3.1")
          )
        )
        
        g1 <- g1 +
          ggnewscale::new_scale("shape") +         # start a second shape scale
          geom_point(
            data  = model_key,
            aes(flops_1e21_epoch, estimate, shape = model_lab),
            size  = 0,           # invisible on plot …
            alpha = 0
          ) +
          scale_shape_manual(
            name   = "Model:",
            values = c(15, 15),  # solid squares
            guide  = guide_legend(
              order         = 3,                 # place after the study legend
              override.aes  = list(size = 5, 
                                   colour = "grey20")
            )
          )
        
        
        
# PANEL 2 ───────────────────────────────────────────────────────────────────
        
        g2 <- df_list$df_estimates_DiD_ates %>% 
          filter(
            outcome_id == .x,
            !str_detect(term, "grok")
          ) %>% 
          mutate(term = str_replace_all(term, model_names)) %>% 
          
          ## ── Plot ───────────────────────────────────────────────────────────────────
          ggplot(aes(x = term, y = estimate, fill = factor(info))) +
          
          # Columns, CIs, points
          geom_col(position = position_dodge(.9), alpha = .5) +
          geom_errorbar(
            aes(ymin = conf.low, ymax = conf.high),
            width     = 0,
            linewidth = .4,
            position  = position_dodge(.9), alpha = .75
          ) +
          geom_point(position = position_dodge(.9), size = 3, colour = "grey20", show.legend = FALSE) +
          
          # Numeric labels just above each bar
          # geom_text(
          #   aes(label = sprintf("%.2f", estimate), y = estimate + 0.08),
          #   position = position_dodge(.9),
          #   size     = 3,
          #   family   = "CMU Serif",
          #   fontface = "bold",
          #   alpha    = .75
          # ) +
          # scale_y_continuous(limits = c(0, 1)) +
          
          # Facet by study with strip labels below the panels
          facet_grid(. ~ study,            # panels in one row
                     scales = "free_x",    # each panel keeps its own x-axis
                     space  = "free_x",
                     switch = "x") +   # panel width ∝ no. of x breaks+
          scale_x_discrete(
            labels = function(x) {
              x %>% 
                stringr::str_replace("GPT-4o \\(8/24\\)", "GPT-4o\n(8/24)") %>% 
                stringr::str_replace("GPT-4o \\(3/25\\)", "GPT-4o\n(3/25)")
            }
          ) +
          
          # Scales & labels
          scale_fill_manual(
            labels = c("0" = "Other", "1" = "Information"),
            values = c("0" = "grey70", "1" = "black")
          ) +
          labs(
            x     = "Model",
            y     = y_label_short,
            fill  = "Prompt:",
            title = NULL
          ) +
          
          # Theme to match g4
          theme_bw(base_family = "CMU Serif") +
          theme(
            panel.grid       = element_blank(),
            panel.border     = element_blank(),
            axis.line        = element_line(linewidth = .3),
            axis.ticks       = element_line(linewidth = .3),
            axis.title       = element_text(size = 18, face = "bold"),
            axis.title.y     = element_text(vjust = 0.5, hjust = .5,
                                            margin = margin(r = 10.5)),
            axis.text.x        = element_text(size = 13),
            axis.text.y        = element_text(size = 15),
            strip.background = element_blank(),
            strip.text       = element_text(face = "bold", size = 14),
            strip.placement  = "outside",
            legend.position  = c(.50, 1.05),
            legend.direction = "horizontal",
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 16),
            plot.title       = element_text(hjust = .5, face = "bold", size = 11),
            plot.margin      = margin(25.5, 5.5, 5.5, 35.5)
          ) 
          

        
        # > Panel 3 ----
        
        temp_df <-
          df_list$df_estimates_post_train_main_fx %>% 
          filter(
            #str_detect(outcome, "facts", negate = T),
            outcome %in% c(outcome_variable, .x),
            # study == 2, 
            # type == "chat-tuned"
          ) %>% 
          mutate(across(c(estimate, conf.low, conf.high), ~ifelse(str_detect(outcome, "prop_facts"), .x*100, .x))) %>% 
          mutate(term = str_replace_all(term, pt_names),
                 outcome = ifelse(str_detect(outcome, "post_average"), "persuasion", "accuracy"))
        
        temp_df_wide <- 
          temp_df %>% 
          pivot_wider(id_cols = c(term, study, type), 
                      names_from = outcome, 
                      values_from = c(estimate, conf.low, conf.high))
        
        
        g3 <-
          temp_df_wide %>% 
          #mutate(labelpls = paste0("Study ", study, "\n", term, " vs. Base\n", str_to_sentence(type))) %>% 
          mutate(labelpls = paste0(term)) %>% 
          ggplot(aes(x = `estimate_accuracy`, y = `estimate_persuasion`, 
                     color = paste0("Study ", factor(study)), shape = str_to_sentence(type))) +
          theme_bw() +
          geom_point(size = 5) +
          geom_errorbar(aes(ymin = `conf.low_persuasion`, 
                            ymax = `conf.high_persuasion`), width = 0, alpha = 0.5, linewidth = .75) +
          geom_errorbarh(aes(xmin = `conf.low_accuracy`, 
                             xmax = `conf.high_accuracy`), height = 0, alpha = 0.5) +
          geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
          geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
          geom_text_repel(aes(label = labelpls), show.legend = F, fontface = "bold", size = 5) +
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
            legend.position       = c(.70, .75),
            legend.direction      = "vertical",
            legend.title          = element_blank(),
            legend.box.background = element_blank(),
            legend.text           = element_text(family = "CMU Serif", size = 16),
            plot.margin           = margin(15.5, 15.5, 5.5, 5.5)
          ) +
          labs(x = paste0("Main effect of PPT\non accuracy (pp)"),
               y = "Main effect of PPT\non persuasion (pp)") +
          scale_color_manual(values = c("#142556", "#669DC4"))
        
        g1  <- g1  + theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5))
        g2  <- g2  + theme(plot.margin = margin(15.5, 5.5, 5.5, 5.5))  
        g3  <- g3  + theme(plot.margin = margin(15.5, 5.5, 5.5, 5.5))
        
        g2 <- g2 + theme(axis.title.y = element_text(margin = margin(r = 25)))
        # or match all panels
        g1 <- g1 + theme(axis.title.y = element_text(margin = margin(r = 0)))
        g <-
          (g1 / (g2 | g3)) +               
          plot_layout(heights = c(1.5, 1)) +  
          plot_annotation(tag_levels = "A") &           
          theme(                                        
            plot.tag = element_text(size = 22, face = "bold",
                                    family = "CMU Serif"))
        
        # > Save ----
        ggsave(plot = g, filename = paste0("output/figures/fig4_", outcome_variable, "__", .x, ".pdf"),
               height = 11, width = 13)
      })
}



