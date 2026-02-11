#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
})

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  out <- list(
    contrast = "outputs/paragraph_theme_contrast_panethnic_minus_ethnic.csv",
    monthly = "outputs/paragraph_theme_monthly_by_group.csv",
    out_trend = "outputs/fig_paragraph_theme_trends_facet.png",
    x_break_every = 4L,
    top_n = 8L,
    normalize = "max100"
  )
  if (length(args) == 0) return(out)
  for (a in args) {
    if (!str_detect(a, "^--")) next
    kv <- str_split_fixed(str_remove(a, "^--"), "=", 2)
    key <- kv[, 1]
    val <- kv[, 2]
    if (!(key %in% names(out)) || !nzchar(val)) next
    if (key %in% c("contrast", "monthly", "out_trend", "normalize")) {
      out[[key]] <- val
    } else {
      out[[key]] <- as.integer(val)
    }
  }
  out
}

cfg <- parse_args()
if (!file.exists(cfg$contrast)) stop(paste("Missing", cfg$contrast), call. = FALSE)
if (!file.exists(cfg$monthly)) stop(paste("Missing", cfg$monthly), call. = FALSE)

contrast <- readr::read_csv(cfg$contrast, show_col_types = FALSE)
monthly <- readr::read_csv(cfg$monthly, show_col_types = FALSE)

if (nrow(contrast) == 0) stop("No rows in contrast table.", call. = FALSE)

# 1) Monthly trend facets only
value_cols <- grep("_per100$", names(monthly), value = TRUE)
long <- monthly %>%
  filter(group %in% c("panethnic_appeared", "ethnic_appeared")) %>%
  pivot_longer(cols = all_of(value_cols), names_to = "metric", values_to = "rate") %>%
  mutate(theme = str_remove(metric, "_per100$"))

key_themes <- c(
  "theme_invisibility",
  "policy_frame_funding",
  "theme_anti_discrimination",
  "theme_equal_opportunity",
  "domain_social_policy",
  "domain_civil_rights_voting"
)
key_themes <- key_themes[key_themes %in% unique(long$theme)]

if (length(key_themes) > 0) {
  tr_long <- long %>%
    filter(theme %in% key_themes) %>%
    mutate(
      theme_pretty = case_when(
        theme == "theme_invisibility" ~ "Invisibility",
        theme == "policy_frame_funding" ~ "Funding",
        theme == "theme_anti_discrimination" ~ "Anti-Discrimination",
        theme == "theme_equal_opportunity" ~ "Equal Opportunity",
        theme == "domain_social_policy" ~ "Social Policy",
        theme == "domain_civil_rights_voting" ~ "Civil Rights / Voting",
        TRUE ~ str_to_title(str_replace_all(theme, "_", " "))
      ),
      group_pretty = recode(group, panethnic_appeared = "Panethnic Appeared", ethnic_appeared = "Ethnic Appeared")
    )

  month_levels <- sort(unique(tr_long$year_month))
  theme_levels <- sort(unique(tr_long$theme_pretty))
  tr_long <- tr_long %>%
    tidyr::complete(
      year_month = month_levels,
      theme_pretty = theme_levels,
      group_pretty = c("Panethnic Appeared", "Ethnic Appeared"),
      fill = list(rate = 0)
    )

  tr_gap <- tr_long %>%
    select(year_month, theme_pretty, group_pretty, rate) %>%
    pivot_wider(names_from = group_pretty, values_from = rate, values_fill = 0) %>%
    mutate(
      y_min = pmin(`Panethnic Appeared`, `Ethnic Appeared`, na.rm = TRUE),
      y_max = pmax(`Panethnic Appeared`, `Ethnic Appeared`, na.rm = TRUE)
    )

  if (cfg$normalize == "max100") {
    tr_long <- tr_long %>%
      group_by(theme_pretty, group_pretty) %>%
      mutate(
        mx = max(rate, na.rm = TRUE),
        rate_plot = if_else(mx > 0, 100 * rate / mx, 0)
      ) %>%
      ungroup()

    tr_gap <- tr_long %>%
      select(year_month, theme_pretty, group_pretty, rate_plot) %>%
      pivot_wider(names_from = group_pretty, values_from = rate_plot, values_fill = 0) %>%
      mutate(
        y_min = pmin(`Panethnic Appeared`, `Ethnic Appeared`, na.rm = TRUE),
        y_max = pmax(`Panethnic Appeared`, `Ethnic Appeared`, na.rm = TRUE)
      )
  } else {
    tr_long <- tr_long %>% mutate(rate_plot = rate)
  }

  tr_long$year_month <- factor(tr_long$year_month, levels = month_levels)
  tr_gap$year_month <- factor(tr_gap$year_month, levels = month_levels)
  break_idx <- seq(1, length(month_levels), by = max(1L, cfg$x_break_every))
  month_breaks <- month_levels[break_idx]
  period_txt <- paste0(min(month_levels), " to ", max(month_levels))

  p_trend <- ggplot() +
    geom_ribbon(
      data = tr_gap,
      aes(x = year_month, ymin = y_min, ymax = y_max, group = 1),
      fill = "gray75",
      alpha = 0.55
    ) +
    geom_line(
      data = tr_long,
      aes(x = year_month, y = rate_plot, color = group_pretty, group = group_pretty),
      linewidth = 0.95
    ) +
    geom_point(
      data = tr_long,
      aes(x = year_month, y = rate_plot, color = group_pretty, group = group_pretty),
      size = 1.1
    ) +
    geom_hline(yintercept = 0, linewidth = 0.25, color = "gray60") +
    facet_wrap(~ theme_pretty, ncol = 2, scales = "free_y") +
    scale_x_discrete(breaks = month_breaks) +
    scale_color_manual(values = c("Panethnic Appeared" = "black", "Ethnic Appeared" = "gray35")) +
    labs(
      title = "Theme Trends in Panethnic- vs Ethnic-Appeared Paragraphs",
      subtitle = paste0(
        ifelse(cfg$normalize == "max100", "Gray band = gap; lines normalized to each group's max = 100", "Gray band = magnitude of gap between group lines"),
        ". Source: Agenda, NCLR newsletter (", period_txt, ")"
      ),
      x = "Issue Month",
      y = ifelse(cfg$normalize == "max100", "Normalized index (group max = 100)", "Mentions per 100 group paragraphs"),
      color = NULL
    ) +
    theme_minimal(base_size = 10.5) +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
      strip.text = element_text(face = "bold"),
      legend.position = "top",
      panel.grid.minor = element_blank()
    )

  ggsave(cfg$out_trend, p_trend, width = 12, height = 9, dpi = 320)
}

message("Wrote: ", cfg$out_trend)
