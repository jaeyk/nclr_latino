#!/usr/bin/env python3
"""Build panethnic context-term network with flexible n-grams.

Input:
- outputs/paragraph_panel_filtered.csv

Outputs:
- outputs/panethnic_phrase_nodes.csv
- outputs/panethnic_phrase_edges.csv
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

# Policy salience lexicon: candidate must include >=1 of these to be retained.
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
    "urban",
    "mental",
    "social",
    "economic",
    "public",
    "civil",
    "equal",
    "minority",
    "federal",
    "legal",
    "criminal",
    "affirmative",
    "bilingual",
    "high",
    "small",
}

HEAD_TERMS = {
    "department",
    "housing",
    "health",
    "education",
    "employment",
    "service",
    "policy",
    "right",
    "worker",
    "labor",
    "development",
    "business",
    "opportunity",
    "court",
    "force",
    "market",
    "center",
    "welfare",
    "commerce",
    "statistic",
}

# Force natural phrase order for known policy compounds.
# Keys must be alphabetically sorted token pairs.
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

    # Preserve policy lexicon surface forms (e.g., rights, services, workers).
    if t in POLICY_TERMS:
        return t

    t = TOKEN_ALIASES.get(t, t)
    if t in POLICY_TERMS:
        return t

    # Light singularization for common OCR/plural drift.
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
        if ba > ab:
            return f"{b} {a}"
        return f"{a} {b}"
    return " ".join(toks)


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
    # hybrid: favors frequency while capping PMI boost so rare n-grams do not dominate.
    pmi_boost = min(2.5, max(0.0, pmi))
    length_weight = 0.45 if n == 1 else (1.0 + 0.22 * (n - 2))
    return freq * (1.0 + pmi_boost) * length_weight


def build_variant_merge_map(selected_meta: dict[str, dict]) -> dict[str, str]:
    """Map near-duplicate terms to a canonical representative key."""
    keys = list(selected_meta.keys())
    rep = {k: k for k in keys}

    # Process longer terms first, allow merge into shorter subset terms.
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
            if n_c >= n_k:
                continue
            if n_c < 2:
                continue
            set_c = set(toks_c)
            if not set_c.issubset(set_k):
                continue

            cnt_c = float(selected_meta[c]["cnt"])
            # Merge only when shorter term is reasonably supported.
            if cnt_c < max(3.0, 0.45 * cnt_k):
                continue

            # Prefer shortest canonical candidate, then highest support.
            if (n_c < best_len) or (n_c == best_len and cnt_c > best_cnt):
                best = c
                best_len = n_c
                best_cnt = cnt_c

        if best is not None:
            rep[k] = best

    # Path compression.
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
    p = argparse.ArgumentParser(description="Panethnic context-term web with flexible n-grams")
    p.add_argument("--panel", default="outputs/paragraph_panel_filtered.csv")
    p.add_argument("--out-nodes", default="outputs/panethnic_phrase_nodes.csv")
    p.add_argument("--out-edges", default="outputs/panethnic_phrase_edges.csv")
    p.add_argument("--min-n", type=int, default=2)
    p.add_argument("--max-n", type=int, default=4)
    p.add_argument("--score-method", choices=["freq", "pmi", "hybrid"], default="hybrid")
    p.add_argument("--min-panethnic-paragraphs", type=int, default=8)
    p.add_argument("--max-terms", type=int, default=70)
    p.add_argument("--max-term-edges", type=int, default=140)
    args = p.parse_args()

    if args.min_n < 1 or args.max_n < args.min_n:
        raise ValueError("Invalid n-gram range.")

    csv.field_size_limit(10**9)
    with pathlib.Path(args.panel).open(newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f))

    pan_docs = 0
    term_df: Counter[str] = Counter()
    unigram_df: Counter[str] = Counter()
    directional_bigram: Counter[Tuple[str, str]] = Counter()
    doc_term_sets: list[set[str]] = []

    for r in rows:
        if as_int(r.get("has_panethnic_label", "0")) != 1:
            continue
        seq = token_sequence(r.get("paragraph_text", ""))
        keys, dir_bg = generate_candidates(seq, min_n=args.min_n, max_n=args.max_n)
        if not keys:
            continue

        pan_docs += 1
        term_df.update(keys)
        doc_term_sets.append(keys)
        directional_bigram.update(dir_bg)

        # unigram df for PMI support
        uni_keys, _ = generate_candidates(seq, min_n=1, max_n=1)
        unigram_df.update(uni_keys)

    if pan_docs == 0:
        raise ValueError("No panethnic paragraphs found after filtering.")

    scored = []
    for key, cnt in term_df.items():
        if cnt < args.min_panethnic_paragraphs:
            continue
        n, toks = parse_key(key)
        label = label_for_key(key, directional_bigram)
        score = score_key(key, df=term_df, unigram_df=unigram_df, n_docs=pan_docs, method=args.score_method)
        scored.append((key, label, n, cnt, score))

    scored.sort(key=lambda x: (x[4], x[3]), reverse=True)
    scored = scored[: args.max_terms]

    selected_meta = {
        k: {"label": label, "n": n, "cnt": cnt, "score": score}
        for (k, label, n, cnt, score) in scored
    }
    merge_map = build_variant_merge_map(selected_meta)
    selected_keys = set(selected_meta.keys())
    rep_keys = sorted(set(merge_map.values()))

    # Recompute representative df from document sets (dedup within paragraph).
    rep_df: Counter[str] = Counter()
    doc_rep_sets: list[set[str]] = []
    for keys in doc_term_sets:
        keep = selected_keys.intersection(keys)
        reps = {merge_map[k] for k in keep}
        if reps:
            rep_df.update(reps)
            doc_rep_sets.append(reps)

    key_to_label = {k: label_for_key(k, directional_bigram) for k in rep_keys}

    rep_rows = []
    for k in rep_keys:
        n, _ = parse_key(k)
        rep_rows.append(
            (
                k,
                key_to_label[k],
                n,
                int(rep_df[k]),
                float(selected_meta[k]["score"]),
            )
        )
    rep_rows.sort(key=lambda x: (x[4], x[3]), reverse=True)

    node_rows: list[dict] = []
    for key, label, n, cnt, score in rep_rows:
        if cnt < args.min_panethnic_paragraphs:
            continue
        node_rows.append(
            {
                "node": label,
                "node_key": key,
                "node_type": "term",
                "ngram_n": n,
                "pan_n": int(cnt),
                "pan_rate": round(cnt / pan_docs, 6),
                "selection_score": round(score, 6),
                "score_method": args.score_method,
            }
        )

    # Keep only representatives retained after recount threshold.
    kept_rep_keys = {r["node_key"] for r in node_rows}
    key_to_label = {k: key_to_label[k] for k in kept_rep_keys}

    pair_counts: Counter[Tuple[str, str]] = Counter()
    for reps in doc_rep_sets:
        keep = sorted(kept_rep_keys.intersection(reps))
        if len(keep) < 2:
            continue
        for a, b in itertools.combinations(keep, 2):
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
            }
        )

    pair_items = sorted(pair_counts.items(), key=lambda kv: kv[1], reverse=True)
    pair_items = [(k, v) for k, v in pair_items if v >= 3][: args.max_term_edges]
    for (a, b), n in pair_items:
        edges.append(
            {
                "from": key_to_label[a],
                "to": key_to_label[b],
                "edge_type": "term_term",
                "edge_group": "Term-Term",
                "weight_n": int(n),
            }
        )

    write_csv(pathlib.Path(args.out_nodes), node_rows)
    write_csv(pathlib.Path(args.out_edges), edges)

    n_counts = Counter(r["ngram_n"] for r in node_rows)
    n_merged = sum(1 for k, v in merge_map.items() if k != v)
    print(f"Input paragraphs: {len(rows)}")
    print(f"Panethnic hub paragraphs: {pan_docs}")
    print(f"Terms retained: {len(node_rows)}")
    print(f"Variants merged: {n_merged}")
    print(f"Retained n-gram mix: {dict(sorted(n_counts.items()))}")
    print(f"Edges written: {len(edges)}")
    print(f"Wrote: {args.out_nodes}")
    print(f"Wrote: {args.out_edges}")


if __name__ == "__main__":
    main()
