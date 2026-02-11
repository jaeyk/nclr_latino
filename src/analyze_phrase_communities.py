#!/usr/bin/env python3
"""Detect communities in the panethnic phrase network.

Inputs:
- outputs/panethnic_phrase_nodes.csv
- outputs/panethnic_phrase_edges.csv

Outputs:
- outputs/panethnic_phrase_community_membership.csv
- outputs/panethnic_phrase_community_summary.csv
"""

from __future__ import annotations

import argparse
import csv
from collections import defaultdict
from pathlib import Path

import networkx as nx


def read_csv(path: Path) -> list[dict]:
    with path.open(newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def write_csv(path: Path, rows: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    if not rows:
        return
    with path.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)


def main() -> None:
    p = argparse.ArgumentParser(description="Community detection on panethnic phrase graph")
    p.add_argument("--nodes", default="outputs/panethnic_phrase_nodes.csv")
    p.add_argument("--edges", default="outputs/panethnic_phrase_edges.csv")
    p.add_argument("--out-membership", default="outputs/panethnic_phrase_community_membership.csv")
    p.add_argument("--out-summary", default="outputs/panethnic_phrase_community_summary.csv")
    p.add_argument("--seed", type=int, default=42)
    p.add_argument("--min-community-size", type=int, default=2)
    args = p.parse_args()

    node_rows = read_csv(Path(args.nodes))
    edge_rows = read_csv(Path(args.edges))
    if not node_rows or not edge_rows:
        raise ValueError("Missing nodes or edges.")

    node_info = {r["node"]: r for r in node_rows}

    g = nx.Graph()
    for r in node_rows:
        g.add_node(r["node"], pan_n=float(r["pan_n"]), pan_rate=float(r["pan_rate"]))

    for e in edge_rows:
        if e["edge_group"] != "Term-Term":
            continue
        a, b = e["from"], e["to"]
        if a not in node_info or b not in node_info:
            continue
        w = float(e["weight_n"])
        if g.has_edge(a, b):
            g[a][b]["weight"] += w
        else:
            g.add_edge(a, b, weight=w)

    if g.number_of_nodes() == 0:
        raise ValueError("No phrase nodes available for community detection.")

    if g.number_of_edges() == 0:
        # fallback: each node is its own community
        communities = [{n} for n in g.nodes()]
    else:
        communities = list(
            nx.algorithms.community.louvain_communities(
                g,
                weight="weight",
                seed=args.seed,
                resolution=1.0,
            )
        )

    # Sort communities by aggregate pan_n descending for stable IDs.
    comm_scored = []
    for c in communities:
        score = sum(float(node_info[n]["pan_n"]) for n in c)
        comm_scored.append((c, score))
    comm_scored.sort(key=lambda x: x[1], reverse=True)

    membership_rows: list[dict] = []
    summary_rows: list[dict] = []

    for cid, (comm, score) in enumerate(comm_scored, start=1):
        if len(comm) < args.min_community_size:
            continue
        terms = sorted(comm, key=lambda n: float(node_info[n]["pan_n"]), reverse=True)
        top_terms = terms[:8]
        top_label = ", ".join(top_terms[:4])
        total_pan_n = int(sum(float(node_info[n]["pan_n"]) for n in comm))

        summary_rows.append(
            {
                "community_id": cid,
                "n_terms": len(comm),
                "total_pan_n": total_pan_n,
                "top_terms": "|".join(top_terms),
                "community_label": top_label,
            }
        )

        for rank, term in enumerate(terms, start=1):
            membership_rows.append(
                {
                    "node": term,
                    "community_id": cid,
                    "community_rank_in_pan_n": rank,
                    "pan_n": int(float(node_info[term]["pan_n"])),
                    "pan_rate": float(node_info[term]["pan_rate"]),
                }
            )

    membership_rows.sort(key=lambda r: (r["community_id"], r["community_rank_in_pan_n"]))
    summary_rows.sort(key=lambda r: r["community_id"])

    write_csv(Path(args.out_membership), membership_rows)
    write_csv(Path(args.out_summary), summary_rows)

    print(f"Phrase nodes: {g.number_of_nodes()}")
    print(f"Phrase edges: {g.number_of_edges()}")
    print(f"Detected communities (raw): {len(communities)}")
    print(f"Communities kept (size >= {args.min_community_size}): {len(summary_rows)}")
    print(f"Wrote: {args.out_membership}")
    print(f"Wrote: {args.out_summary}")


if __name__ == "__main__":
    main()
