#!/usr/bin/env python3
"""Render panethnic phrase web using Python (networkx + matplotlib).

Inputs:
- outputs/panethnic_phrase_nodes.csv
- outputs/panethnic_phrase_edges.csv

Output:
- outputs/fig_panethnic_phrase_web_python.png
"""

from __future__ import annotations

import argparse
import csv
import math
import random
from pathlib import Path

import matplotlib.pyplot as plt
import networkx as nx
import numpy as np


def read_csv(path: Path) -> list[dict]:
    with path.open(newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def scale(values: np.ndarray, lo: float, hi: float, power: float = 1.0) -> np.ndarray:
    if len(values) == 0:
        return values
    v = values.astype(float)
    vmin, vmax = float(np.min(v)), float(np.max(v))
    if vmax <= vmin:
        return np.full_like(v, (lo + hi) / 2.0)
    t = (v - vmin) / (vmax - vmin)
    if power != 1.0:
        t = np.power(t, power)
    return lo + (hi - lo) * t


def resolve_label_overlaps(ax, texts, max_iter: int = 220, px_step: float = 1.8) -> None:
    """Repel text artists in display space (pixel-based)."""
    if not texts:
        return
    fig = ax.figure
    inv = ax.transData.inverted()

    for _ in range(max_iter):
        fig.canvas.draw()
        renderer = fig.canvas.get_renderer()
        bboxes = [t.get_window_extent(renderer).expanded(1.03, 1.10) for t in texts]
        moved = False

        for i in range(len(texts)):
            bi = bboxes[i]
            cix, ciy = (bi.x0 + bi.x1) / 2.0, (bi.y0 + bi.y1) / 2.0
            for j in range(i + 1, len(texts)):
                bj = bboxes[j]
                if not bi.overlaps(bj):
                    continue
                cjx, cjy = (bj.x0 + bj.x1) / 2.0, (bj.y0 + bj.y1) / 2.0
                dx, dy = cix - cjx, ciy - cjy
                if dx == 0 and dy == 0:
                    dx = random.uniform(-1, 1)
                    dy = random.uniform(-1, 1)
                norm = math.hypot(dx, dy)
                ux, uy = dx / norm, dy / norm

                for k, sign in ((i, 1.0), (j, -1.0)):
                    tx, ty = texts[k].get_position()
                    px, py = ax.transData.transform((tx, ty))
                    px += sign * ux * px_step
                    py += sign * uy * px_step
                    nxp, nyp = inv.transform((px, py))
                    texts[k].set_position((nxp, nyp))
                moved = True

        if not moved:
            break


def main() -> None:
    p = argparse.ArgumentParser(description="Plot panethnic phrase web (Python)")
    p.add_argument("--nodes", default="outputs/panethnic_phrase_nodes.csv")
    p.add_argument("--edges", default="outputs/panethnic_phrase_edges.csv")
    p.add_argument("--out", default="outputs/fig_panethnic_phrase_web_python.png")
    p.add_argument("--seed", type=int, default=42)
    p.add_argument("--width", type=float, default=18.0)
    p.add_argument("--height", type=float, default=11.0)
    p.add_argument("--dpi", type=int, default=170)
    args = p.parse_args()

    nodes = read_csv(Path(args.nodes))
    edges = read_csv(Path(args.edges))
    if not nodes or not edges:
        raise ValueError("Missing nodes or edges.")

    phrase_nodes = {r["node"]: float(r["pan_n"]) for r in nodes}

    g_phrase = nx.Graph()
    for n, w in phrase_nodes.items():
        g_phrase.add_node(n, pan_n=w)

    hub_edges = []
    for e in edges:
        a, b = e["from"], e["to"]
        w = float(e["weight_n"])
        if e["edge_group"] == "Phrase-Phrase":
            if a in phrase_nodes and b in phrase_nodes:
                g_phrase.add_edge(a, b, weight=w, edge_group="Phrase-Phrase")
        elif e["edge_group"] == "Panethnic Hub":
            hub_edges.append((a, b, w))

    pos_phrase = nx.spring_layout(g_phrase, seed=args.seed, weight="weight", k=0.9 / math.sqrt(max(1, g_phrase.number_of_nodes())), iterations=500)
    xs = np.array([p[0] for p in pos_phrase.values()])
    ys = np.array([p[1] for p in pos_phrase.values()])
    xmin, xmax = float(xs.min()), float(xs.max())
    ymin, ymax = float(ys.min()), float(ys.max())
    xspan = max(1e-6, xmax - xmin)
    ymid = (ymin + ymax) / 2.0
    hub_pos = (xmin - 0.42 * xspan, ymid)

    fig, ax = plt.subplots(figsize=(args.width, args.height), dpi=args.dpi)
    bg = "#ececec"
    fig.patch.set_facecolor(bg)
    ax.set_facecolor(bg)

    # phrase-phrase edges
    pp_edges = [(u, v) for u, v, d in g_phrase.edges(data=True)]
    if pp_edges:
        nx.draw_networkx_edges(
            g_phrase,
            pos_phrase,
            edgelist=pp_edges,
            width=0.8,
            alpha=0.22,
            edge_color="#b5b5b5",
            ax=ax,
        )

    # hub->phrase edges
    for a, b, w in hub_edges:
        ph = b if a == "panethnic_hub" else a
        if ph not in pos_phrase:
            continue
        x1, y1 = hub_pos
        x2, y2 = pos_phrase[ph]
        lw = 0.6 + 2.0 * (w / max(1.0, max(phrase_nodes.values())))
        ax.plot([x1, x2], [y1, y2], color="#7a7a7a", alpha=0.26, linewidth=lw, solid_capstyle="round")

    # nodes
    ph_names = list(g_phrase.nodes())
    ph_weights = np.array([phrase_nodes[n] for n in ph_names], dtype=float)
    ph_sizes = scale(ph_weights, lo=180, hi=3200, power=0.72)
    xvals = np.array([pos_phrase[n][0] for n in ph_names])
    yvals = np.array([pos_phrase[n][1] for n in ph_names])
    ax.scatter(xvals, yvals, s=ph_sizes, facecolor="#d4dbdb", edgecolor="#1f1f1f", linewidth=1.3, zorder=3)
    ax.scatter([hub_pos[0]], [hub_pos[1]], s=[23000], facecolor="#a9b6b6", edgecolor="#1f1f1f", linewidth=1.5, zorder=4)

    # labels (all nodes + hub)
    cx, cy = np.mean(xvals), np.mean(yvals)
    label_sizes = scale(ph_weights, lo=11.5, hi=20.0, power=0.55)
    texts = []
    for n, fs in zip(ph_names, label_sizes):
        x, y = pos_phrase[n]
        ang = math.atan2(y - cy, x - cx)
        offset = 0.07
        tx, ty = x + offset * math.cos(ang), y + offset * math.sin(ang)
        t = ax.text(tx, ty, n, fontsize=float(fs), color="#101010", ha="center", va="center", zorder=6)
        texts.append(t)
    resolve_label_overlaps(ax, texts=texts, max_iter=260, px_step=2.2)

    ax.text(hub_pos[0], hub_pos[1], "panethnic", fontsize=35, color="#101010", ha="center", va="center", zorder=7)

    # Title / subtitle / source
    fig.text(
        0.012,
        0.965,
        "Policy Context Network Around Panethnic Labeling",
        fontsize=30,
        fontweight="bold",
        ha="left",
        va="top",
        color="#111111",
    )
    fig.text(
        0.012,
        0.935,
        "Two-word policy phrases in panethnic-labeled paragraphs; node size reflects paragraph co-occurrence frequency",
        fontsize=18,
        ha="left",
        va="top",
        color="#222222",
    )
    fig.text(
        0.988,
        0.012,
        "Source: Agenda, NCLR newsletter (1971-06 to 1981-09)",
        fontsize=13,
        ha="right",
        va="bottom",
        color="#4a4a4a",
    )

    ax.set_axis_off()
    ax.set_aspect("equal")
    xall = np.append(xvals, hub_pos[0])
    yall = np.append(yvals, hub_pos[1])
    pad_x = 0.14 * (xall.max() - xall.min())
    pad_y = 0.14 * (yall.max() - yall.min())
    ax.set_xlim(float(xall.min() - pad_x), float(xall.max() + pad_x))
    ax.set_ylim(float(yall.min() - pad_y), float(yall.max() + pad_y))

    Path(args.out).parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(args.out, dpi=args.dpi, facecolor=bg, bbox_inches="tight")
    plt.close(fig)
    print(f"Wrote: {args.out}")


if __name__ == "__main__":
    main()
