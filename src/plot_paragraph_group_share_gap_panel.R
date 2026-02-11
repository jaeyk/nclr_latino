#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(stringr)
  library(gridExtra)
})

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  out <- list(
    monthly = "outputs/analysis/paragraph_theme_monthly_by_group.csv",
    out_plot = "outputs/analysis/fig_span_group_share_over_time_gap_panel.png",
    x_break_every = 4L
  )
  if (length(args) == 0) return(out)
  for (a in args) {
    if (!str_detect(a, "^--")) next
    kv <- str_split_fixed(str_remove(a, "^--"), "=", 2)
    key <- kv[, 1]
    val <- kv[, 2]
    if (!(key %in% names(out)) || !nzchar(val)) next
    if (key %in% c("monthly", "out_plot")) out[[key]] <- val else out[[key]] <- as.integer(val)
  }
  out
}

cfg <- parse_args()
if (!file.exists(cfg$monthly)) stop(paste("Missing", cfg$monthly), call. = FALSE)

monthly <- readr::read_csv(cfg$monthly, show_col_types = FALSE) %>%
  filter(group %in% c("panethnic_appeared", "ethnic_appeared")) %>%
  mutate(year_month = as.character(year_month))

share <- monthly %>%
  select(year_month, group, n_paragraphs) %>%
  group_by(year_month, group) %>%
  summarise(n = sum(n_paragraphs, na.rm = TRUE), .groups = "drop") %>%
  group_by(year_month) %>%
  mutate(total = sum(n), pct = if_else(total > 0, 100 * n / total, 0)) %>%
  ungroup() %>%
  mutate(group_pretty = recode(group, panethnic_appeared = "Panethnic", ethnic_appeared = "Ethnic"))
share$group_pretty <- factor(share$group_pretty, levels = c("Panethnic", "Ethnic"))

gap <- share %>%
  select(year_month, group_pretty, pct) %>%
  tidyr::pivot_wider(names_from = group_pretty, values_from = pct, values_fill = 0) %>%
  mutate(gap_pan_minus_eth = Panethnic - Ethnic)

month_levels <- sort(unique(share$year_month))
share$year_month <- factor(share$year_month, levels = month_levels)
gap$year_month <- factor(gap$year_month, levels = month_levels)
break_idx <- seq(1, length(month_levels), by = max(1L, cfg$x_break_every))
month_breaks <- month_levels[break_idx]
period_txt <- paste0(min(month_levels), " to ", max(month_levels))

p_top <- ggplot(gap, aes(x = year_month, y = gap_pan_minus_eth, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30", linewidth = 0.45) +
  geom_line(color = "black", linewidth = 1.0) +
  geom_point(color = "black", size = 1.3) +
  scale_x_discrete(breaks = month_breaks) +
  scale_y_continuous(labels = function(x) round(x, 0)) +
  labs(
    title = "Panethnic Minus Ethnic Share Gap (Paragraph Unit)",
    subtitle = paste0("Both panels use paragraph-unit shares. Source: Agenda, NCLR newsletter (", period_txt, ")"),
    x = NULL,
    y = "Gap (percentage points)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank()
  )

p_bottom <- ggplot(
  share,
  aes(
    x = year_month,
    y = pct,
    color = group_pretty,
    linetype = group_pretty,
    shape = group_pretty,
    group = group_pretty
  )
) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.8, stroke = 0.2) +
  scale_color_manual(values = c("Panethnic" = "black", "Ethnic" = "gray35")) +
  scale_linetype_manual(values = c("Panethnic" = "solid", "Ethnic" = "longdash")) +
  scale_shape_manual(values = c("Panethnic" = 16, "Ethnic" = 17)) +
  scale_x_discrete(breaks = month_breaks) +
  scale_y_continuous(labels = function(x) paste0(round(x, 0), "%")) +
  labs(
    title = "Raw Group Shares (Paragraph Unit)",
    subtitle = paste0("Source: Agenda, NCLR newsletter (", period_txt, ")"),
    x = "Issue Month",
    y = "Percent share",
    color = NULL,
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
    legend.position = "top",
    panel.grid.minor = element_blank()
  ) +
  guides(
    color = guide_legend(
      override.aes = list(
        linetype = c("solid", "longdash"),
        shape = c(16, 17),
        linewidth = 1.1
      )
    ),
    linetype = "none",
    shape = "none"
  )

png(cfg$out_plot, width = 12, height = 8.5, units = "in", res = 320)
gridExtra::grid.arrange(p_top, p_bottom, ncol = 1, heights = c(0.9, 1.1))
dev.off()

message("Wrote: ", cfg$out_plot)
