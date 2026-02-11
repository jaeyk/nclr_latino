#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(igraph)
})

parse_args <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  out <- list(
    nodes = "outputs/panethnic_phrase_nodes.csv",
    edges = "outputs/panethnic_phrase_edges.csv",
    out_plot = "outputs/fig_panethnic_phrase_web.png",
    out_plot_all_labels = "outputs/fig_panethnic_phrase_web_all_labels.png",
    out_plot_labeled_only = "outputs/fig_panethnic_phrase_web_labeled_only.png",
    label_top_n = 8L,
    seed = 42L
  )
  if (length(args) == 0) return(out)
  for (a in args) {
    if (!str_detect(a, "^--")) next
    kv <- str_split_fixed(str_remove(a, "^--"), "=", 2)
    key <- kv[, 1]
    val <- kv[, 2]
    if (!(key %in% names(out)) || !nzchar(val)) next
    if (key %in% c("nodes", "edges", "out_plot", "out_plot_all_labels", "out_plot_labeled_only")) {
      out[[key]] <- val
    } else {
      out[[key]] <- as.integer(val)
    }
  }
  out
}

cfg <- parse_args()
if (!file.exists(cfg$nodes)) stop(paste("Missing", cfg$nodes), call. = FALSE)
if (!file.exists(cfg$edges)) stop(paste("Missing", cfg$edges), call. = FALSE)

nodes <- readr::read_csv(cfg$nodes, show_col_types = FALSE)
edges <- readr::read_csv(cfg$edges, show_col_types = FALSE)
if (nrow(nodes) == 0 || nrow(edges) == 0) stop("No nodes/edges to plot.", call. = FALSE)

nodes <- nodes %>%
  mutate(
    pan_n = as.numeric(pan_n),
    pan_rate = as.numeric(pan_rate),
    node = as.character(node)
  )

hub_row <- tibble::tibble(node = "panethnic_hub", pan_n = max(nodes$pan_n, na.rm = TRUE) * 1.35, pan_rate = 1)
all_nodes <- bind_rows(nodes %>% select(node, pan_n, pan_rate), hub_row) %>%
  distinct(node, .keep_all = TRUE)

edge_tbl <- edges %>%
  transmute(
    from = as.character(from),
    to = as.character(to),
    edge_group = as.character(edge_group),
    weight_n = as.numeric(weight_n)
  ) %>%
  filter(from %in% all_nodes$node, to %in% all_nodes$node)

if (nrow(edge_tbl) == 0) stop("No valid edges after filtering.", call. = FALSE)

phrase_edges <- edge_tbl %>% filter(edge_group %in% c("Phrase-Phrase", "Term-Term"))
core_phrase_names <- sort(unique(c(phrase_edges$from, phrase_edges$to)))
if (length(core_phrase_names) == 0) {
  core_phrase_names <- nodes %>% arrange(desc(pan_n)) %>% slice_head(n = min(20, n())) %>% pull(node)
}
keep_names <- c(core_phrase_names, "panethnic_hub")
plot_nodes <- all_nodes %>% filter(node %in% keep_names)
edge_tbl <- edge_tbl %>% filter(from %in% keep_names, to %in% keep_names)
phrase_edges <- edge_tbl %>% filter(edge_group %in% c("Phrase-Phrase", "Term-Term"))

phrase_vertices <- nodes %>%
  filter(node %in% core_phrase_names) %>%
  select(node, pan_n, pan_rate)
g_phrase <- graph_from_data_frame(d = phrase_edges, vertices = phrase_vertices, directed = FALSE)

set.seed(cfg$seed)
if (ecount(g_phrase) > 0) {
  lay_phrase <- layout_with_fr(g_phrase, niter = 2800, weights = E(g_phrase)$weight_n)
} else {
  lay_phrase <- layout_nicely(g_phrase)
}
rownames(lay_phrase) <- V(g_phrase)$name

g <- graph_from_data_frame(d = edge_tbl, vertices = plot_nodes, directed = FALSE)

E(g)$is_hub_edge <- ifelse(E(g)$edge_group == "Panethnic Hub", 1, 0)
E(g)$weight_plot <- ifelse(
  E(g)$is_hub_edge == 1,
  0.35 + 2.2 * (E(g)$weight_n - min(E(g)$weight_n)) / max(1, max(E(g)$weight_n) - min(E(g)$weight_n)),
  0.15 + 1.0 * (E(g)$weight_n - min(E(g)$weight_n)) / max(1, max(E(g)$weight_n) - min(E(g)$weight_n))
)
E(g)$col_plot <- ifelse(E(g)$is_hub_edge == 1, adjustcolor("#7a7a7a", alpha.f = 0.32), adjustcolor("#b5b5b5", alpha.f = 0.25))

V(g)$is_hub <- ifelse(V(g)$name == "panethnic_hub", 1, 0)
V(g)$size_plot <- ifelse(
  V(g)$is_hub == 1,
  34,
  4 + 34 * ((V(g)$pan_n - min(V(g)$pan_n[V(g)$is_hub == 0])) / max(1, max(V(g)$pan_n[V(g)$is_hub == 0]) - min(V(g)$pan_n[V(g)$is_hub == 0])))^0.72
)
V(g)$col_plot <- ifelse(V(g)$is_hub == 1, "#a9b6b6", "#d4dbdb")
V(g)$frame_plot <- "#1f1f1f"

lay <- matrix(0, nrow = vcount(g), ncol = 2)
rownames(lay) <- V(g)$name
non_hub_names <- V(g)$name[V(g)$name != "panethnic_hub"]
lay[non_hub_names, ] <- lay_phrase[non_hub_names, , drop = FALSE]

xr <- range(lay[non_hub_names, 1], na.rm = TRUE)
yr <- range(lay[non_hub_names, 2], na.rm = TRUE)
x_span <- max(0.8, diff(xr))
lay["panethnic_hub", ] <- c(xr[1] - 0.42 * x_span, mean(yr))

# Label only salient outer nodes with greedy spacing to prevent overlap.
hub_idx <- which(V(g)$name == "panethnic_hub")
non_hub_idx <- which(V(g)$name != "panethnic_hub")
center_xy <- colMeans(lay[non_hub_idx, , drop = FALSE], na.rm = TRUE)
rad <- sqrt((lay[, 1] - center_xy[1])^2 + (lay[, 2] - center_xy[2])^2)
outer_cut <- as.numeric(stats::quantile(rad[non_hub_idx], probs = 0.45, na.rm = TRUE))
candidate_idx <- non_hub_idx[rad[non_hub_idx] >= outer_cut]

ord <- candidate_idx[order(V(g)$pan_n[candidate_idx], rad[candidate_idx], decreasing = TRUE)]
pick <- integer(0)
min_base <- 0.16
for (idx in ord) {
  if (length(pick) >= cfg$label_top_n) break
  ok <- TRUE
  for (j in pick) {
    dx <- lay[idx, 1] - lay[j, 1]
    dy <- lay[idx, 2] - lay[j, 2]
    d <- sqrt(dx * dx + dy * dy)
    need <- min_base + 0.004 * (V(g)$size_plot[idx] + V(g)$size_plot[j])
    if (d < need) {
      ok <- FALSE
      break
    }
  }
  if (ok) pick <- c(pick, idx)
}

V(g)$label_plot <- ""
V(g)$label_plot[hub_idx] <- "panethnic"
if (length(pick) > 0) {
  V(g)$label_plot[pick] <- V(g)$name[pick]
}

mx <- max(V(g)$pan_n, na.rm = TRUE)

render_plot <- function(gp, layp, labels, label_cex, label_dist, outfile, subtitle_text, width_px = 1400, height_px = 900, use_rescale = TRUE, pointsize_px = 14, label_deg = NULL) {
  png(outfile, width = width_px, height = height_px, res = 130, bg = "#ececec", pointsize = pointsize_px)
  par(mar = c(1, 1, 1, 1), xpd = NA)
  deg <- if (is.null(label_deg)) atan2(layp[, 2], layp[, 1]) else label_deg
  xr2 <- range(layp[, 1], na.rm = TRUE)
  yr2 <- range(layp[, 2], na.rm = TRUE)
  pad_x <- 0.10 * max(1e-6, diff(xr2))
  pad_y <- 0.10 * max(1e-6, diff(yr2))
  plot(
    gp,
    layout = layp,
    rescale = use_rescale,
    asp = 0,
    xlim = c(xr2[1] - pad_x, xr2[2] + pad_x),
    ylim = c(yr2[1] - pad_y, yr2[2] + pad_y),
    vertex.shape = "circle",
    vertex.size = V(gp)$size_plot,
    vertex.color = V(gp)$col_plot,
    vertex.frame.color = V(gp)$frame_plot,
    vertex.label = labels,
    vertex.label.cex = label_cex,
    vertex.label.color = "#101010",
    vertex.label.family = "sans",
    vertex.label.degree = deg,
    vertex.label.dist = label_dist,
    edge.width = E(gp)$weight_plot,
    edge.color = E(gp)$col_plot,
    edge.curved = 0.14,
    main = ""
  )
  mtext("Policy Context Network Around Panethnic Labeling", side = 3, line = -0.3, adj = 0.01, cex = 1.4, font = 2, col = "#111111")
  mtext(subtitle_text, side = 3, line = -1.4, adj = 0.01, cex = 0.9, col = "#222222")
  mtext("Source: Agenda, NCLR newsletter (1971-06 to 1981-09)", side = 1, line = -0.2, adj = 0.99, cex = 0.78, col = "#4a4a4a")
  dev.off()
  message("Wrote: ", outfile)
}

# Version 1: all labels (larger text + anti-overlap spacing).
g_all <- g
V(g_all)$size_plot <- ifelse(V(g_all)$is_hub == 1, 28, V(g_all)$size_plot * 0.82)
labels_all <- ifelse(V(g_all)$is_hub == 1, "panethnic", V(g_all)$name)
label_cex_all <- ifelse(V(g_all)$is_hub == 1, 1.35, 0.46 + 0.48 * sqrt(V(g_all)$pan_n / mx))

dmat <- as.matrix(dist(lay))
diag(dmat) <- Inf
nn <- apply(dmat, 1, min)
label_dist_all <- ifelse(
  V(g_all)$is_hub == 1,
  0.22,
  pmin(1.8, pmax(0.85, 1.55 - 2.6 * nn))
)

# Spread coordinates for the all-label view to reduce collisions.
lay_all <- lay
ctr <- colMeans(lay_all, na.rm = TRUE)
lay_all[, 1] <- ctr[1] + 1.45 * (lay_all[, 1] - ctr[1])
lay_all[, 2] <- ctr[2] + 1.35 * (lay_all[, 2] - ctr[2])

render_plot(
  gp = g_all,
  layp = lay_all,
  labels = labels_all,
  label_cex = label_cex_all,
  label_dist = label_dist_all,
  outfile = cfg$out_plot_all_labels,
  subtitle_text = "Two-word policy phrases in panethnic-labeled paragraphs; node size reflects paragraph co-occurrence frequency",
  width_px = 1800,
  height_px = 1100,
  use_rescale = FALSE,
  pointsize_px = 22
)

# Version 2: labeled-only nodes (clean view).
keep_vids <- which(V(g)$label_plot != "")
g_clean <- induced_subgraph(g, vids = keep_vids)
lay_clean <- lay[V(g_clean)$name, , drop = FALSE]
labels_clean <- ifelse(V(g_clean)$is_hub == 1, "panethnic", V(g_clean)$name)
mx_clean <- max(V(g_clean)$pan_n, na.rm = TRUE)
label_cex_clean <- ifelse(V(g_clean)$is_hub == 1, 1.65, 0.58 + 0.65 * sqrt(V(g_clean)$pan_n / mx_clean))
label_dist_clean <- ifelse(V(g_clean)$is_hub == 1, 0.22, 0.95)
render_plot(
  gp = g_clean,
  layp = lay_clean,
  labels = labels_clean,
  label_cex = label_cex_clean,
  label_dist = label_dist_clean,
  outfile = cfg$out_plot_labeled_only,
  subtitle_text = "Top policy context phrases (clean view)"
)

# Backward-compatible default output now points to all-label version.
file.copy(cfg$out_plot_all_labels, cfg$out_plot, overwrite = TRUE)
message("Wrote: ", cfg$out_plot)
