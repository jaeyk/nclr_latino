#!/usr/bin/env python3
"""Build dual-hub (panethnic vs ethnic) policy context network tables.

Input:
- outputs/paragraph_panel_filtered.csv

Outputs:
- outputs/dual_hub_nodes.csv
- outputs/dual_hub_edges.csv
- outputs/dual_hub_community_membership.csv
- outputs/dual_hub_community_summary.csv
"""

from __future__ import annotations

import argparse
import csv
import itertools
import math
import pathlib
import re
from collections import Counter
from typing import List, Tuple

import networkx as nx


TOKEN_RE = re.compile(r"[a-z][a-z'-]{1,}")

STOPWORDS = {
    "a", "about", "above", "after", "again", "against", "all", "am", "an", "and", "any", "are", "as", "at",
    "be", "because", "been", "before", "being", "below", "between", "both", "but", "by", "can", "could", "did",
    "do", "does", "doing", "down", "during", "each", "few", "for", "from", "further", "had", "has", "have",
    "having", "he", "her", "here", "hers", "herself", "him", "himself", "his", "how", "i", "if", "in", "into",
    "is", "it", "its", "itself", "just", "me", "more", "most", "my", "myself", "no", "nor", "not", "now",
    "of", "off", "on", "once", "only", "or", "other", "our", "ours", "ourselves", "out", "over", "own", "same",
    "she", "should", "so", "some", "such", "than", "that", "the", "their", "theirs", "them", "themselves", "then",
    "there", "these", "they", "this", "those", "through", "to", "too", "under", "until", "up", "very", "was", "we",
    "were", "what", "when", "where", "which", "while", "who", "whom", "why", "will", "with", "you", "your", "yours",
    "yourself", "yourselves", "agenda", "council", "issue", "volume", "number", "page", "pages", "newsletter", "washington",
    "raz", "raza", "nclr", "also", "one", "two", "three", "first", "second", "third", "fourth", "year", "years", "time",
    "new", "state", "states", "united", "national", "american", "americans", "said", "say", "says", "would", "must", "many",
    "much", "among", "however", "including", "within", "across", "based", "used", "use", "using", "well", "make", "made",
    "still", "since", "every", "another", "week", "month", "months", "government", "office", "program", "programs", "group",
    "groups", "work", "works", "worked", "working", "people", "person", "percent", "per", "via", "etc", "mr", "mrs", "ms",
    "dr", "january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december",
    "throughout", "country", "city", "york",
}

IDENTITY_TERMS = {
    "hispanic", "hispanics", "latino", "latinos", "latina", "latinas", "spanish", "speaking", "surname",
    "chicano", "chicanos", "mexican", "mexicans", "puerto", "rican", "ricans", "cuban", "dominican", "salvadoran",
    "guatemalan", "nicaraguan",
}

NOISE_TOKENS = {"tion", "ment", "com", "con", "ity", "ities", "ence", "ences", "ings", "ing", "pro"}

POLICY_TERMS = {
    "civil", "rights", "voting", "legal", "defense", "affirmative", "equal", "employment", "discrimination",
    "education", "bilingual", "school", "schools", "student", "students", "teacher", "teachers", "curriculum",
    "health", "care", "mental", "welfare", "labor", "workers", "worker", "farm", "housing", "rent", "tenant",
    "immigration", "immigrant", "border", "deportation", "undocumented", "refugee", "visa", "social", "economic",
    "development", "poverty", "services", "public", "federal", "department", "business", "minority", "opportunity",
    "justice", "criminal", "police", "prison", "jail", "court", "policy", "funding", "budget", "grant",
}

TOKEN_ALIASES = {
    "broadcasting": "broadcast",
    "departments": "department",
    "workers": "worker",
    "schools": "school",
    "services": "service",
    "agencies": "agency",
}

MODIFIER_TERMS = {
    "urban", "mental", "social", "economic", "public", "civil", "equal", "minority", "federal", "legal",
    "criminal", "affirmative", "bilingual", "high", "small",
}

HEAD_TERMS = {
    "department", "housing", "health", "education", "employment", "service", "policy", "right", "worker", "labor",
    "development", "business", "opportunity", "court", "force", "market", "center", "welfare", "commerce", "statistic",
}

BIGRAM_LABEL_OVERRIDES = {
    ("administration", "business"): "business administration",
    ("business", "enterprise"): "business enterprise",
    ("care", "health"): "health care",
    ("department", "labor"): "labor department",
    ("department", "justice"): "justice department",
    ("economic", "social"): "social economic",
    ("housing", "urban"): "urban housing",
    ("opportunity", "equal"): "equal opportunity",
}

LABEL_CANONICAL_OVERRIDES = {
    "health education": "health education welfare",
    "education welfare": "health education welfare",
}


def as_int(v: str) -> int:
    try:
        return 1 if int(v) == 1 else 0
    except Exception:
        return 0


def normalize_token(tok: str) -> str:
    t = tok.lower().strip("'")
    if t.endswith("'s"):
        t = t[:-2]
    elif t.endswith("s'"):
        t = t[:-1]
    if t in POLICY_TERMS:
        return t
    t = TOKEN_ALIASES.get(t, t)
    if t in POLICY_TERMS:
        return t
    if len(t) >= 6 and t.endswith("ies"):
        cand = t[:-3] + "y"
        if cand in POLICY_TERMS:
            t = cand
    elif len(t) >= 6 and t.endswith("sses"):
        cand = t[:-2]
        if cand in POLICY_TERMS:
            t = cand
    elif len(t) >= 6 and t.endswith("s") and not t.endswith("ss"):
        cand = t[:-1]
        if cand in POLICY_TERMS:
            t = cand
    return t


def token_sequence(text: str) -> list[str]:
    txt = (text or "").lower().replace("\u2019", "'").replace("\u00ad", "")
    raw = TOKEN_RE.findall(txt)
    out: list[str] = []
    for t in raw:
        t = normalize_token(t)
        if len(t) < 4:
            continue
        if t in STOPWORDS or t in IDENTITY_TERMS or t in NOISE_TOKENS:
            continue
        out.append(t)
    return out


def make_key(tokens: Tuple[str, ...]) -> str:
    n = len(tokens)
    if n == 2:
        a, b = sorted(tokens)
        return f"2||{a}|||{b}"
    return f"{n}||" + "|||".join(tokens)


def parse_key(key: str) -> Tuple[int, Tuple[str, ...]]:
    n_str, body = key.split("||", 1)
    toks = tuple(body.split("|||")) if body else tuple()
    return int(n_str), toks


def contains_policy(tokens: Tuple[str, ...]) -> bool:
    return any(t in POLICY_TERMS for t in tokens)


def generate_candidates(seq: List[str], min_n: int, max_n: int) -> Tuple[set[str], Counter[Tuple[str, str]]]:
    keys: set[str] = set()
    directional_bigram: Counter[Tuple[str, str]] = Counter()
    n_tokens = len(seq)
    for n in range(min_n, max_n + 1):
        if n <= 0 or n > n_tokens:
            continue
        for i in range(0, n_tokens - n + 1):
            toks = tuple(seq[i : i + n])
            if not contains_policy(toks):
                continue
            if n == 2 and toks[0] == toks[1]:
                continue
            keys.add(make_key(toks))
            if n == 2:
                directional_bigram[toks] += 1
    return keys, directional_bigram


def label_for_key(key: str, directional_bigram: Counter[Tuple[str, str]]) -> str:
    n, toks = parse_key(key)
    if n == 2 and len(toks) == 2:
        a, b = toks
        override = BIGRAM_LABEL_OVERRIDES.get(tuple(sorted((a, b))))
        if override:
            return override
        if a in MODIFIER_TERMS and b not in MODIFIER_TERMS:
            return f"{a} {b}"
        if b in MODIFIER_TERMS and a not in MODIFIER_TERMS:
            return f"{b} {a}"
        if a in HEAD_TERMS and b not in HEAD_TERMS:
            return f"{b} {a}"
        if b in HEAD_TERMS and a not in HEAD_TERMS:
            return f"{a} {b}"
        ab = directional_bigram.get((a, b), 0)
        ba = directional_bigram.get((b, a), 0)
        return f"{b} {a}" if ba > ab else f"{a} {b}"
    return " ".join(toks)


def canonicalize_label(label: str) -> str:
    return LABEL_CANONICAL_OVERRIDES.get(label, label)


def pmi_score(key: str, df: Counter[str], unigram_df: Counter[str], n_docs: int) -> float:
    n, toks = parse_key(key)
    if n <= 1:
        return 0.0
    p_ng = max(1e-12, df[key] / max(1, n_docs))
    p_parts = 1.0
    for t in toks:
        p_parts *= max(1e-12, unigram_df[f"1||{t}"] / max(1, n_docs))
    return math.log(p_ng / p_parts)


def score_key(key: str, df: Counter[str], unigram_df: Counter[str], n_docs: int, method: str) -> float:
    n, _ = parse_key(key)
    freq = float(df[key])
    if method == "freq":
        if n == 1:
            return 0.55 * freq
        return freq * (1.0 + 0.18 * (n - 2))
    pmi = pmi_score(key, df=df, unigram_df=unigram_df, n_docs=n_docs)
    if method == "pmi":
        return pmi
    pmi_boost = min(2.5, max(0.0, pmi))
    length_weight = 0.45 if n == 1 else (1.0 + 0.22 * (n - 2))
    return freq * (1.0 + pmi_boost) * length_weight


def build_variant_merge_map(selected_meta: dict[str, dict]) -> dict[str, str]:
    keys = list(selected_meta.keys())
    rep = {k: k for k in keys}
    keys_by_len = sorted(keys, key=lambda k: parse_key(k)[0], reverse=True)
    for k in keys_by_len:
        n_k, toks_k = parse_key(k)
        if n_k < 2:
            continue
        cnt_k = float(selected_meta[k]["cnt"])
        set_k = set(toks_k)
        best = None
        best_len = 10**9
        best_cnt = -1.0
        for c in keys:
            if c == k:
                continue
            n_c, toks_c = parse_key(c)
            if n_c >= n_k or n_c < 2:
                continue
            set_c = set(toks_c)
            if not set_c.issubset(set_k):
                continue
            cnt_c = float(selected_meta[c]["cnt"])
            if cnt_c < max(3.0, 0.45 * cnt_k):
                continue
            if (n_c < best_len) or (n_c == best_len and cnt_c > best_cnt):
                best = c
                best_len = n_c
                best_cnt = cnt_c
        if best is not None:
            rep[k] = best
    for k in keys:
        while rep[k] != rep[rep[k]]:
            rep[k] = rep[rep[k]]
    return rep


def write_csv(path: pathlib.Path, rows: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    if not rows:
        return
    with path.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)


def main() -> None:
    p = argparse.ArgumentParser(description="Dual-hub context-term web with flexible n-grams")
    p.add_argument("--panel", default="outputs/paragraph_panel_filtered.csv")
    p.add_argument("--out-nodes", default="outputs/dual_hub_nodes.csv")
    p.add_argument("--out-edges", default="outputs/dual_hub_edges.csv")
    p.add_argument("--out-membership", default="outputs/dual_hub_community_membership.csv")
    p.add_argument("--out-community-summary", default="outputs/dual_hub_community_summary.csv")
    p.add_argument("--min-n", type=int, default=2)
    p.add_argument("--max-n", type=int, default=4)
    p.add_argument("--score-method", choices=["freq", "pmi", "hybrid"], default="hybrid")
    p.add_argument("--min-any-paragraphs", type=int, default=8)
    p.add_argument("--max-terms", type=int, default=90)
    p.add_argument("--max-term-edges", type=int, default=220)
    p.add_argument("--seed", type=int, default=42)
    p.add_argument("--min-community-size", type=int, default=2)
    args = p.parse_args()

    if args.min_n < 1 or args.max_n < args.min_n:
        raise ValueError("Invalid n-gram range.")

    csv.field_size_limit(10**9)
    with pathlib.Path(args.panel).open(newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f))

    pan_docs = 0
    eth_docs = 0
    pooled_docs = 0

    pan_df: Counter[str] = Counter()
    eth_df: Counter[str] = Counter()
    pooled_df: Counter[str] = Counter()
    unigram_df: Counter[str] = Counter()
    directional_bigram: Counter[Tuple[str, str]] = Counter()

    pan_doc_sets: list[set[str]] = []
    eth_doc_sets: list[set[str]] = []
    pooled_doc_sets: list[set[str]] = []

    for r in rows:
        has_pan = as_int(r.get("has_panethnic_label", "0")) == 1
        has_eth = as_int(r.get("has_ethnic_label", "0")) == 1
        if not (has_pan or has_eth):
            continue

        seq = token_sequence(r.get("paragraph_text", ""))
        keys, dir_bg = generate_candidates(seq, min_n=args.min_n, max_n=args.max_n)
        if not keys:
            continue
        uni_keys, _ = generate_candidates(seq, min_n=1, max_n=1)
        unigram_df.update(uni_keys)
        directional_bigram.update(dir_bg)

        pooled_docs += 1
        pooled_df.update(keys)
        pooled_doc_sets.append(keys)

        if has_pan:
            pan_docs += 1
            pan_df.update(keys)
            pan_doc_sets.append(keys)
        if has_eth:
            eth_docs += 1
            eth_df.update(keys)
            eth_doc_sets.append(keys)

    if pan_docs == 0 or eth_docs == 0:
        raise ValueError("Need both panethnic and ethnic paragraphs.")

    scored = []
    for key in pooled_df.keys():
        pan_n = int(pan_df.get(key, 0))
        eth_n = int(eth_df.get(key, 0))
        any_n = max(pan_n, eth_n)
        if any_n < args.min_any_paragraphs:
            continue

        n, _ = parse_key(key)
        label = label_for_key(key, directional_bigram)
        pooled_score = score_key(key, df=pooled_df, unigram_df=unigram_df, n_docs=pooled_docs, method=args.score_method)
        pan_rate = pan_n / max(1, pan_docs)
        eth_rate = eth_n / max(1, eth_docs)
        gap = pan_rate - eth_rate
        select_score = pooled_score * (1.0 + min(2.0, 12.0 * abs(gap)))
        scored.append((key, label, n, pan_n, eth_n, pan_rate, eth_rate, gap, select_score))

    scored.sort(key=lambda x: x[8], reverse=True)
    scored = scored[: args.max_terms]

    selected_meta = {
        k: {
            "label": label,
            "n": n,
            "pan_n": pan_n,
            "eth_n": eth_n,
            "select_score": sel,
        }
        for (k, label, n, pan_n, eth_n, _, _, _, sel) in scored
    }
    merge_map = build_variant_merge_map({k: {"cnt": m["pan_n"] + m["eth_n"]} for k, m in selected_meta.items()})
    rep_keys = sorted(set(merge_map.values()))

    # Recount representative term dfs.
    pan_rep_df: Counter[str] = Counter()
    eth_rep_df: Counter[str] = Counter()
    pooled_rep_df: Counter[str] = Counter()
    pan_rep_sets: list[set[str]] = []
    eth_rep_sets: list[set[str]] = []
    pooled_rep_sets: list[set[str]] = []

    selected_keys = set(selected_meta.keys())
    for keys in pan_doc_sets:
        reps = {merge_map[k] for k in keys if k in selected_keys}
        if reps:
            pan_rep_df.update(reps)
            pan_rep_sets.append(reps)
    for keys in eth_doc_sets:
        reps = {merge_map[k] for k in keys if k in selected_keys}
        if reps:
            eth_rep_df.update(reps)
            eth_rep_sets.append(reps)
    for keys in pooled_doc_sets:
        reps = {merge_map[k] for k in keys if k in selected_keys}
        if reps:
            pooled_rep_df.update(reps)
            pooled_rep_sets.append(reps)

    key_to_label = {k: canonicalize_label(label_for_key(k, directional_bigram)) for k in rep_keys}

    node_accum: dict[str, dict] = {}
    for k in rep_keys:
        pan_n = int(pan_rep_df.get(k, 0))
        eth_n = int(eth_rep_df.get(k, 0))
        if max(pan_n, eth_n) < args.min_any_paragraphs:
            continue
        n, _ = parse_key(k)
        lbl = key_to_label[k]
        if lbl not in node_accum:
            node_accum[lbl] = {
                "node": lbl,
                "node_keys": set(),
                "ngram_n": n,
                "pan_n": 0,
                "eth_n": 0,
            }
        node_accum[lbl]["node_keys"].add(k)
        node_accum[lbl]["ngram_n"] = min(node_accum[lbl]["ngram_n"], n)
        node_accum[lbl]["pan_n"] += pan_n
        node_accum[lbl]["eth_n"] += eth_n

    node_rows: list[dict] = []
    for lbl, v in node_accum.items():
        pan_n = int(v["pan_n"])
        eth_n = int(v["eth_n"])
        pan_rate = pan_n / max(1, pan_docs)
        eth_rate = eth_n / max(1, eth_docs)
        gap = pan_rate - eth_rate
        pooled_n = pan_n + eth_n
        sel_score = pooled_n * (1.0 + min(2.0, 12.0 * abs(gap)))
        tilt = "panethnic_tilt" if gap > 0.01 else ("ethnic_tilt" if gap < -0.01 else "shared")
        node_rows.append(
            {
                "node": lbl,
                "node_key": ";;".join(sorted(v["node_keys"])),
                "ngram_n": int(v["ngram_n"]),
                "pan_n": pan_n,
                "eth_n": eth_n,
                "pooled_n": pooled_n,
                "pan_rate": round(pan_rate, 6),
                "eth_rate": round(eth_rate, 6),
                "gap_pan_minus_eth": round(gap, 6),
                "selection_score": round(sel_score, 6),
                "score_method": args.score_method,
                "tilt": tilt,
            }
        )

    node_rows.sort(key=lambda r: float(r["selection_score"]), reverse=True)
    kept_rep_keys = set()
    for r in node_rows:
        kept_rep_keys.update(k for k in r["node_key"].split(";;") if k)
    key_to_label = {k: key_to_label[k] for k in kept_rep_keys}

    pair_counts: Counter[Tuple[str, str]] = Counter()
    for reps in pooled_rep_sets:
        keep_labels = sorted({key_to_label[k] for k in reps if k in kept_rep_keys})
        if len(keep_labels) < 2:
            continue
        for a, b in itertools.combinations(keep_labels, 2):
            pair_counts[(a, b)] += 1

    edges: list[dict] = []
    for r in node_rows:
        edges.append(
            {
                "from": "panethnic_hub",
                "to": r["node"],
                "edge_type": "hub_term",
                "edge_group": "Panethnic Hub",
                "weight_n": int(r["pan_n"]),
                "weight_rate": r["pan_rate"],
            }
        )
        edges.append(
            {
                "from": "ethnic_hub",
                "to": r["node"],
                "edge_type": "hub_term",
                "edge_group": "Ethnic Hub",
                "weight_n": int(r["eth_n"]),
                "weight_rate": r["eth_rate"],
            }
        )

    pair_items = sorted(pair_counts.items(), key=lambda kv: kv[1], reverse=True)
    pair_items = [(k, v) for k, v in pair_items if v >= 3][: args.max_term_edges]
    for (a, b), n in pair_items:
        edges.append(
            {
                "from": a,
                "to": b,
                "edge_type": "term_term",
                "edge_group": "Term-Term",
                "weight_n": int(n),
                "weight_rate": "",
            }
        )

    # Community detection on term-term graph.
    g = nx.Graph()
    for r in node_rows:
        g.add_node(r["node"], gap=float(r["gap_pan_minus_eth"]), pooled_n=float(r["pooled_n"]))
    for (a, b), n in pair_items:
        if a != b:
            g.add_edge(a, b, weight=float(n))

    communities = []
    if g.number_of_nodes() > 0:
        if g.number_of_edges() == 0:
            communities = [{n} for n in g.nodes()]
        else:
            communities = list(nx.algorithms.community.louvain_communities(g, weight="weight", seed=args.seed))

    comm_scored = []
    for c in communities:
        score = sum(g.nodes[n]["pooled_n"] for n in c)
        comm_scored.append((c, score))
    comm_scored.sort(key=lambda x: x[1], reverse=True)

    membership_rows = []
    summary_rows = []
    for cid, (comm, _) in enumerate(comm_scored, start=1):
        if len(comm) < args.min_community_size:
            continue
        terms = sorted(comm, key=lambda n: g.nodes[n]["pooled_n"], reverse=True)
        total_n = int(sum(g.nodes[n]["pooled_n"] for n in comm))
        mean_gap = sum(g.nodes[n]["gap"] for n in comm) / max(1, len(comm))
        label = ", ".join(terms[:4])
        summary_rows.append(
            {
                "community_id": cid,
                "n_terms": len(comm),
                "total_pooled_n": total_n,
                "mean_gap_pan_minus_eth": round(mean_gap, 6),
                "community_tilt": "panethnic_tilt" if mean_gap > 0.01 else ("ethnic_tilt" if mean_gap < -0.01 else "shared"),
                "community_label": label,
                "top_terms": "|".join(terms[:8]),
            }
        )
        for rank, t in enumerate(terms, start=1):
            membership_rows.append(
                {
                    "node": t,
                    "community_id": cid,
                    "community_rank_in_pooled_n": rank,
                    "pooled_n": int(g.nodes[t]["pooled_n"]),
                    "gap_pan_minus_eth": round(float(g.nodes[t]["gap"]), 6),
                }
            )

    write_csv(pathlib.Path(args.out_nodes), node_rows)
    write_csv(pathlib.Path(args.out_edges), edges)
    write_csv(pathlib.Path(args.out_membership), membership_rows)
    write_csv(pathlib.Path(args.out_community_summary), summary_rows)

    n_counts = Counter(r["ngram_n"] for r in node_rows)
    n_merged = sum(1 for k, v in merge_map.items() if k != v)
    print(f"Input paragraphs: {len(rows)}")
    print(f"Panethnic paragraphs: {pan_docs}")
    print(f"Ethnic paragraphs: {eth_docs}")
    print(f"Terms retained: {len(node_rows)}")
    print(f"Variants merged: {n_merged}")
    print(f"Retained n-gram mix: {dict(sorted(n_counts.items()))}")
    print(f"Edges written: {len(edges)}")
    print(f"Communities kept: {len(summary_rows)}")
    print(f"Wrote: {args.out_nodes}")
    print(f"Wrote: {args.out_edges}")
    print(f"Wrote: {args.out_membership}")
    print(f"Wrote: {args.out_community_summary}")


if __name__ == "__main__":
    main()
