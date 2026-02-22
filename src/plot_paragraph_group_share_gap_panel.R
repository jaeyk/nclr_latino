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
    panel = "outputs/paragraph_panel_filtered.csv",
    out_plot = "outputs/fig_span_group_share_over_time_gap_panel.png",
    x_break_every = 4L
  )
  if (length(args) == 0) return(out)
  for (a in args) {
    if (!str_detect(a, "^--")) next
    kv <- str_split_fixed(str_remove(a, "^--"), "=", 2)
    key <- kv[, 1]
    val <- kv[, 2]
    if (!(key %in% names(out)) || !nzchar(val)) next
    if (key %in% c("panel", "out_plot")) out[[key]] <- val else out[[key]] <- as.integer(val)
  }
  out
}

cfg <- parse_args()
if (!file.exists(cfg$panel)) stop(paste("Missing", cfg$panel), call. = FALSE)

panel <- readr::read_csv(cfg$panel, show_col_types = FALSE) %>%
  mutate(
    canonical_year = suppressWarnings(as.integer(canonical_year)),
    canonical_month = suppressWarnings(as.integer(canonical_month)),
    has_panethnic_label = suppressWarnings(as.integer(has_panethnic_label)),
    has_ethnic_label = suppressWarnings(as.integer(has_ethnic_label))
  ) %>%
  filter(!is.na(canonical_year), !is.na(canonical_month), canonical_month >= 1, canonical_month <= 12) %>%
  mutate(year_month = sprintf("%04d-%02d", canonical_year, canonical_month))

monthly_rates <- panel %>%
  group_by(year_month) %>%
  summarise(
    n_total = n(),
    n_panethnic = sum(has_panethnic_label == 1, na.rm = TRUE),
    n_ethnic = sum(has_ethnic_label == 1, na.rm = TRUE),
    pct_panethnic = if_else(n_total > 0, 100 * n_panethnic / n_total, 0),
    pct_ethnic = if_else(n_total > 0, 100 * n_ethnic / n_total, 0),
    .groups = "drop"
  )

share <- monthly_rates %>%
  select(year_month, pct_panethnic, pct_ethnic) %>%
  tidyr::pivot_longer(
    cols = c(pct_panethnic, pct_ethnic),
    names_to = "group",
    values_to = "pct"
  ) %>%
  mutate(group_pretty = recode(group, pct_panethnic = "Panethnic", pct_ethnic = "Ethnic"))
share$group_pretty <- factor(share$group_pretty, levels = c("Panethnic", "Ethnic"))

gap <- monthly_rates %>%
  transmute(year_month, gap_pan_minus_eth = pct_panethnic - pct_ethnic)

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
    title = "Panethnic Minus Ethnic Mention-Rate Gap",
    subtitle = "Denominator: all paragraphs in each month.",
    x = NULL,
    y = "Gap (percentage points)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 12)
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
    title = "Monthly Mention Rates",
    subtitle = NULL,
    x = "Issue Month",
    y = "Percent of all paragraphs",
    color = NULL,
    linetype = NULL,
    shape = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 11),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    legend.position = "top",
    legend.text = element_text(size = 12),
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

caption_grob <- grid::textGrob(
  paste0("Source: Agenda, NCLR newsletter (", period_txt, ")"),
  x = 0.99,
  hjust = 1,
  gp = grid::gpar(col = "gray30", cex = 1.0)
)

combined_plot <- gridExtra::arrangeGrob(
  p_top,
  p_bottom,
  ncol = 1,
  heights = c(0.9, 1.1),
  bottom = caption_grob
)

png(cfg$out_plot, width = 12, height = 8.5, units = "in", res = 320)
grid::grid.newpage()
grid::grid.draw(combined_plot)
dev.off()

message("Wrote: ", cfg$out_plot)
