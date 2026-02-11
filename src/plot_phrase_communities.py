#!/usr/bin/env python3
"""Plot panethnic phrase network colored by community."""

from __future__ import annotations

import argparse
import csv
import math
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


def main() -> None:
    p = argparse.ArgumentParser(description="Plot phrase network communities")
    p.add_argument("--nodes", default="outputs/panethnic_phrase_nodes.csv")
    p.add_argument("--edges", default="outputs/panethnic_phrase_edges.csv")
    p.add_argument("--membership", default="outputs/panethnic_phrase_community_membership.csv")
    p.add_argument("--summary", default="outputs/panethnic_phrase_community_summary.csv")
    p.add_argument("--out", default="outputs/fig_panethnic_phrase_communities.png")
    p.add_argument("--seed", type=int, default=42)
    p.add_argument("--labels-per-community", type=int, default=5)
    args = p.parse_args()

    node_rows = read_csv(Path(args.nodes))
    edge_rows = read_csv(Path(args.edges))
    mem_rows = read_csv(Path(args.membership))
    sum_rows = read_csv(Path(args.summary))
    if not node_rows or not edge_rows or not mem_rows:
        raise ValueError("Missing required inputs.")

    node_pan_n = {r["node"]: float(r["pan_n"]) for r in node_rows}
    node_comm = {r["node"]: int(r["community_id"]) for r in mem_rows}

    g = nx.Graph()
    for n, pan_n in node_pan_n.items():
        if n in node_comm:
            g.add_node(n, pan_n=pan_n, community_id=node_comm[n])

    for e in edge_rows:
        if e["edge_group"] != "Term-Term":
            continue
        a, b = e["from"], e["to"]
        if a in g and b in g:
            g.add_edge(a, b, weight=float(e["weight_n"]))

    if g.number_of_nodes() == 0:
        raise ValueError("No nodes in filtered graph.")

    # Keep largest connected component for clean visualization.
    cc = max(nx.connected_components(g), key=len)
    g = g.subgraph(cc).copy()

    pos = nx.spring_layout(g, seed=args.seed, weight="weight", k=1.0 / math.sqrt(max(1, g.number_of_nodes())), iterations=600)
    names = list(g.nodes())
    weights = np.array([g.nodes[n]["pan_n"] for n in names])
    sizes = scale(weights, lo=170, hi=2100, power=0.72)

    comm_ids = sorted(set(g.nodes[n]["community_id"] for n in names))
    cmap = plt.get_cmap("tab20")
    comm_color = {cid: cmap((i * 2) % 20) for i, cid in enumerate(comm_ids)}
    node_colors = [comm_color[g.nodes[n]["community_id"]] for n in names]

    fig, ax = plt.subplots(figsize=(16, 10), dpi=180)
    bg = "#f2f2f2"
    fig.patch.set_facecolor(bg)
    ax.set_facecolor(bg)

    nx.draw_networkx_edges(
        g,
        pos,
        width=0.8,
        alpha=0.18,
        edge_color="#9f9f9f",
        ax=ax,
    )
    nx.draw_networkx_nodes(
        g,
        pos,
        nodelist=names,
        node_size=sizes,
        node_color=node_colors,
        edgecolors="#3a3a3a",
        linewidths=0.9,
        ax=ax,
    )

    # Label top terms per community.
    by_comm: dict[int, list[tuple[str, float]]] = {}
    for n in names:
        cid = g.nodes[n]["community_id"]
        by_comm.setdefault(cid, []).append((n, g.nodes[n]["pan_n"]))
    label_nodes = set()
    for cid, rows in by_comm.items():
        rows.sort(key=lambda x: x[1], reverse=True)
        for n, _ in rows[: args.labels_per_community]:
            label_nodes.add(n)

    for n in label_nodes:
        x, y = pos[n]
        fs = 9 + 6 * math.sqrt(g.nodes[n]["pan_n"] / max(1.0, float(np.max(weights))))
        ax.text(x, y, n, fontsize=fs, color="#101010", ha="center", va="center", zorder=5)

    ax.text(
        0.01,
        0.98,
        "Panethnic Policy Context Network by Community",
        transform=ax.transAxes,
        ha="left",
        va="top",
        fontsize=20,
        fontweight="bold",
        color="#111111",
    )
    ax.text(
        0.01,
        0.945,
        "Nodes are selected policy context terms (n-grams); colors indicate detected communities.",
        transform=ax.transAxes,
        ha="left",
        va="top",
        fontsize=12,
        color="#222222",
    )
    ax.text(
        0.99,
        0.01,
        "Source: Agenda, NCLR newsletter (1971-06 to 1981-09)",
        transform=ax.transAxes,
        ha="right",
        va="bottom",
        fontsize=10,
        color="#444444",
    )

    # Compact legend from summary labels.
    legend_rows = []
    for r in sum_rows:
        cid = int(r["community_id"])
        if cid in comm_color:
            legend_rows.append((cid, r["community_label"]))
    legend_rows.sort(key=lambda x: x[0])
    for i, (cid, label) in enumerate(legend_rows[:8]):
        ax.text(
            0.01,
            0.89 - i * 0.03,
            f"C{cid}: {label}",
            transform=ax.transAxes,
            ha="left",
            va="top",
            fontsize=10,
            color=comm_color[cid],
            fontweight="bold",
        )

    ax.set_axis_off()
    Path(args.out).parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(args.out, dpi=180, facecolor=bg, bbox_inches="tight")
    plt.close(fig)
    print(f"Wrote: {args.out}")


if __name__ == "__main__":
    main()
