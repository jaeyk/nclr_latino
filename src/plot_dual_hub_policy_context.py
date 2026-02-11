#!/usr/bin/env python3
"""Plot dual-hub policy context network (panethnic vs ethnic)."""

from __future__ import annotations

import argparse
import csv
import math
from pathlib import Path

import matplotlib.pyplot as plt
import networkx as nx
import numpy as np

DROP_TERMS = {
    "defense educational",
}


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


def choose_nodes(
    names: list[str],
    attrs: dict[str, dict],
    max_nodes: int,
    min_per_tilt: int,
) -> list[str]:
    """Select a readable set of terms while preserving tilt balance."""
    if len(names) <= max_nodes:
        return names

    by_tilt: dict[str, list[str]] = {
        "panethnic_tilt": [],
        "shared": [],
        "ethnic_tilt": [],
    }
    for n in names:
        tilt = attrs[n]["tilt"]
        by_tilt.setdefault(tilt, []).append(n)

    for tilt in by_tilt:
        by_tilt[tilt].sort(
            key=lambda n: (
                attrs[n]["pooled_n"],
                abs(attrs[n]["gap_pan_minus_eth"]),
            ),
            reverse=True,
        )

    picked: list[str] = []
    for tilt in ("panethnic_tilt", "shared", "ethnic_tilt"):
        picked.extend(by_tilt.get(tilt, [])[:min_per_tilt])
    picked = list(dict.fromkeys(picked))

    remaining = [n for n in names if n not in picked]
    remaining.sort(
        key=lambda n: (
            attrs[n]["pooled_n"] * (1.0 + 5.0 * abs(attrs[n]["gap_pan_minus_eth"])),
            attrs[n]["pooled_n"],
        ),
        reverse=True,
    )
    picked.extend(remaining[: max(0, max_nodes - len(picked))])
    return picked[:max_nodes]


def grouped_layout(g_terms: nx.Graph) -> dict[str, tuple[float, float]]:
    """Readable fixed layout by tilt groups with non-overlapping y slots."""
    groups: dict[str, list[str]] = {
        "panethnic_tilt": [],
        "shared": [],
        "ethnic_tilt": [],
    }
    for n, data in g_terms.nodes(data=True):
        groups.setdefault(data["tilt"], []).append(n)

    for k in groups:
        groups[k].sort(key=lambda n: g_terms.nodes[n]["pooled_n"], reverse=True)

    x_center = {
        "panethnic_tilt": -0.64,
        "shared": -0.02,
        "ethnic_tilt": 0.74,
    }
    pos: dict[str, tuple[float, float]] = {}
    max_n = max([len(v) for v in groups.values()] + [1])
    y_span = 1.7
    y_shift = -0.18
    for tilt in ("panethnic_tilt", "shared", "ethnic_tilt"):
        terms = groups.get(tilt, [])
        n = len(terms)
        if n == 0:
            continue
        # Evenly-spaced slots with slight deterministic x jitter by rank.
        ys = np.linspace(y_span, -y_span, n)
        for i, (term, y) in enumerate(zip(terms, ys)):
            jitter = (0.024 * ((i % 2) * 2 - 1)) if n > 5 else 0.0
            pos[term] = (x_center[tilt] + jitter, float(y + y_shift))

    return pos


def main() -> None:
    p = argparse.ArgumentParser(description="Plot dual-hub policy context network")
    p.add_argument("--nodes", default="outputs/dual_hub_nodes.csv")
    p.add_argument("--edges", default="outputs/dual_hub_edges.csv")
    p.add_argument("--out", default="outputs/fig_dual_hub_policy_context.png")
    p.add_argument("--max-nodes", type=int, default=28)
    p.add_argument("--min-per-tilt", type=int, default=6)
    args = p.parse_args()

    node_rows = read_csv(Path(args.nodes))
    edge_rows = read_csv(Path(args.edges))
    if not node_rows or not edge_rows:
        raise ValueError("Missing nodes/edges.")

    node_info = {}
    for r in node_rows:
        node_info[r["node"]] = {
            "pooled_n": float(r["pooled_n"]),
            "gap_pan_minus_eth": float(r["gap_pan_minus_eth"]),
            "tilt": r["tilt"],
            "pan_rate": float(r["pan_rate"]),
            "eth_rate": float(r["eth_rate"]),
        }

    selected_names = choose_nodes(
        names=[r["node"] for r in node_rows],
        attrs=node_info,
        max_nodes=max(8, args.max_nodes),
        min_per_tilt=max(1, args.min_per_tilt),
    )
    selected_names = [n for n in selected_names if n not in DROP_TERMS]
    selected = set(selected_names)

    g_terms = nx.Graph()
    for r in node_rows:
        if r["node"] not in selected:
            continue
        g_terms.add_node(
            r["node"],
            pooled_n=float(r["pooled_n"]),
            gap=float(r["gap_pan_minus_eth"]),
            tilt=r["tilt"],
        )

    pan_hub_edges = []
    eth_hub_edges = []
    for e in edge_rows:
        a, b = e["from"], e["to"]
        if e["edge_group"] == "Term-Term":
            if a in selected and b in selected:
                g_terms.add_edge(a, b, weight=float(e["weight_n"]))
        elif e["edge_group"] == "Panethnic Hub":
            term = b if a == "panethnic_hub" else a
            if term not in selected:
                continue
            pan_hub_edges.append((a, b, float(e["weight_rate"])))
        elif e["edge_group"] == "Ethnic Hub":
            term = b if a == "ethnic_hub" else a
            if term not in selected:
                continue
            eth_hub_edges.append((a, b, float(e["weight_rate"])))

    if g_terms.number_of_nodes() == 0:
        raise ValueError("No term nodes.")

    pan_hub_pos = (-1.55, 0.0)
    eth_hub_pos = (1.75, 0.0)
    pos_all = grouped_layout(g_terms=g_terms)
    names = list(g_terms.nodes())
    pos_terms = {n: pos_all[n] for n in names}
    xs = np.array([pos_terms[n][0] for n in names], dtype=float)
    ys = np.array([pos_terms[n][1] for n in names], dtype=float)

    weights = np.array([g_terms.nodes[n]["pooled_n"] for n in names], dtype=float)
    node_sizes = scale(weights, lo=180, hi=1200, power=0.82)

    color_map = {
        "panethnic_tilt": "#2b7bba",
        "ethnic_tilt": "#d95f02",
        "shared": "#bdbdbd",
    }
    node_colors = [color_map.get(g_terms.nodes[n]["tilt"], "#bdbdbd") for n in names]

    fig, ax = plt.subplots(figsize=(20, 12), dpi=170)
    bg = "#f1f1f1"
    fig.patch.set_facecolor(bg)
    ax.set_facecolor(bg)

    # Term-term edges
    nx.draw_networkx_edges(
        g_terms,
        pos_terms,
        width=0.65,
        alpha=0.09,
        edge_color="#9e9e9e",
        ax=ax,
    )

    # Hub edges
    max_pan_rate = max([w for _, _, w in pan_hub_edges] + [1e-6])
    max_eth_rate = max([w for _, _, w in eth_hub_edges] + [1e-6])
    for a, b, w in pan_hub_edges:
        term = b if a == "panethnic_hub" else a
        if term not in pos_terms:
            continue
        x2, y2 = pos_terms[term]
        ax.plot(
            [pan_hub_pos[0], x2],
            [pan_hub_pos[1], y2],
            color="#2b7bba",
            alpha=0.2,
            linewidth=0.4 + 2.2 * (w / max_pan_rate),
            solid_capstyle="round",
            zorder=1,
        )
    for a, b, w in eth_hub_edges:
        term = b if a == "ethnic_hub" else a
        if term not in pos_terms:
            continue
        x2, y2 = pos_terms[term]
        ax.plot(
            [eth_hub_pos[0], x2],
            [eth_hub_pos[1], y2],
            color="#d95f02",
            alpha=0.2,
            linewidth=0.4 + 2.2 * (w / max_eth_rate),
            solid_capstyle="round",
            zorder=1,
        )

    # Term nodes
    ax.scatter(
        [pos_terms[n][0] for n in names],
        [pos_terms[n][1] for n in names],
        s=node_sizes,
        c=node_colors,
        edgecolors="#353535",
        linewidths=1.0,
        zorder=3,
    )

    # Hub nodes
    ax.scatter([pan_hub_pos[0]], [pan_hub_pos[1]], s=[7000], c=["#2b7bba"], edgecolors="#222", linewidths=1.4, zorder=4)
    ax.scatter([eth_hub_pos[0]], [eth_hub_pos[1]], s=[6800], c=["#d95f02"], edgecolors="#222", linewidths=1.4, zorder=4)
    ax.text(pan_hub_pos[0] + 0.01, pan_hub_pos[1], "panethnic", fontsize=15, color="white", ha="center", va="center", zorder=5, fontweight="bold")
    ax.text(eth_hub_pos[0], eth_hub_pos[1], "ethnic", fontsize=20, color="white", ha="center", va="center", zorder=5, fontweight="bold")

    label_offsets = {
        "employment opportunity": (0.065, 0.0, "left"),
    }
    for n in sorted(names, key=lambda n: g_terms.nodes[n]["pooled_n"], reverse=True):
        x, y = pos_terms[n]
        fs = 10.0 + 2.2 * math.sqrt(g_terms.nodes[n]["pooled_n"] / max(1.0, float(np.max(weights))))
        tilt = g_terms.nodes[n]["tilt"]
        if tilt == "panethnic_tilt":
            dx = -0.038
            ha = "right"
        elif tilt == "ethnic_tilt":
            dx = 0.038
            ha = "left"
        else:
            dx = 0.034
            ha = "left"
        if n in label_offsets:
            dx, dy, ha = label_offsets[n]
        else:
            dy = 0.0
        ax.text(x + dx, y + dy, n, fontsize=fs, color="#101010", ha=ha, va="center", zorder=6)

    # Titles and legend text.
    ax.text(
        0.01,
        0.98,
        "Policy Contexts: Panethnic vs Ethnic",
        transform=ax.transAxes,
        ha="left",
        va="top",
        fontsize=20,
        fontweight="bold",
        color="#111111",
    )
    ax.text(
        0.01,
        0.948,
        "Top policy-context terms linked to both hubs; colors show whether terms are relatively panethnic-tilted, ethnic-tilted, or shared.",
        transform=ax.transAxes,
        ha="left",
        va="top",
        fontsize=10.5,
        color="#222222",
    )
    ax.text(0.01, 0.924, "Blue = panethnic-tilt | Orange = ethnic-tilt | Gray = shared", transform=ax.transAxes, ha="left", va="top", fontsize=10.5, color="#333")
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

    ax.set_axis_off()
    xall = np.array([*xs, pan_hub_pos[0], eth_hub_pos[0]])
    yall = np.array([*ys, pan_hub_pos[1], eth_hub_pos[1]])
    padx = 0.2 * (xall.max() - xall.min())
    pady = 0.18 * (yall.max() - yall.min())
    ax.set_xlim(float(xall.min() - padx), float(xall.max() + padx))
    ax.set_ylim(float(yall.min() - pady), float(yall.max() + pady))
    ax.set_aspect("equal")

    Path(args.out).parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(args.out, dpi=170, facecolor=bg, bbox_inches="tight")
    plt.close(fig)
    print(f"Wrote: {args.out}")


if __name__ == "__main__":
    main()
