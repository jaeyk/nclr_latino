#!/usr/bin/env python3
"""Build paragraph-level phrase web for panethnic hub.

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
import pathlib
import re
from collections import Counter


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

# Keep only policy-relevant phrase nodes.
POLICY_TERMS = {
    "civil", "rights", "voting", "legal", "defense", "affirmative", "equal", "employment", "discrimination",
    "education", "bilingual", "school", "schools", "student", "students", "teacher", "teachers", "curriculum",
    "health", "care", "mental", "welfare", "labor", "workers", "worker", "farm", "housing", "rent", "tenant",
    "immigration", "immigrant", "border", "deportation", "undocumented", "refugee", "visa", "social", "economic",
    "development", "poverty", "services", "public", "federal", "department", "business", "minority", "opportunity",
    "justice", "criminal", "police", "prison", "jail", "court", "policy", "funding", "budget", "grant",
}


def as_int(v: str) -> int:
    try:
        return 1 if int(v) == 1 else 0
    except Exception:
        return 0


def token_sequence(text: str) -> list[str]:
    txt = (text or "").lower().replace("\u2019", "'").replace("\u00ad", "")
    raw = TOKEN_RE.findall(txt)
    out: list[str] = []
    for t in raw:
        if len(t) < 4:
            continue
        if t in STOPWORDS or t in IDENTITY_TERMS or t in NOISE_TOKENS:
            continue
        out.append(t)
    return out


def phrase_set(seq: list[str]) -> set[str]:
    if len(seq) < 2:
        return set()
    phrases: set[str] = set()
    for a, b in zip(seq[:-1], seq[1:]):
        if a == b:
            continue
        if a in POLICY_TERMS and b in POLICY_TERMS:
            phrases.add(f"{a} {b}")
    return phrases


def write_csv(path: pathlib.Path, rows: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    if not rows:
        return
    with path.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)


def main() -> None:
    p = argparse.ArgumentParser(description="Panethnic hub phrase web")
    p.add_argument("--panel", default="outputs/paragraph_panel_filtered.csv")
    p.add_argument("--out-nodes", default="outputs/panethnic_phrase_nodes.csv")
    p.add_argument("--out-edges", default="outputs/panethnic_phrase_edges.csv")
    p.add_argument("--min-panethnic-paragraphs", type=int, default=8)
    p.add_argument("--max-phrases", type=int, default=70)
    p.add_argument("--max-phrase-edges", type=int, default=140)
    args = p.parse_args()

    csv.field_size_limit(10**9)
    with pathlib.Path(args.panel).open(newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f))

    pan_docs = 0
    pan_counts: Counter[str] = Counter()
    pan_doc_phrases: list[set[str]] = []

    for r in rows:
        has_pan = as_int(r.get("has_panethnic_label", "0")) == 1
        if not has_pan:
            continue

        phrases = phrase_set(token_sequence(r.get("paragraph_text", "")))
        if not phrases:
            continue

        pan_docs += 1
        pan_counts.update(phrases)
        pan_doc_phrases.append(phrases)

    if pan_docs == 0:
        raise ValueError("No panethnic paragraphs found after filtering.")

    node_rows: list[dict] = []
    for ph, n in pan_counts.items():
        if n < args.min_panethnic_paragraphs:
            continue
        node_rows.append(
            {
                "node": ph,
                "node_type": "phrase",
                "pan_n": int(n),
                "pan_rate": round(n / pan_docs, 6),
            }
        )

    node_rows.sort(key=lambda r: r["pan_n"], reverse=True)
    node_rows = node_rows[: args.max_phrases]
    selected = {r["node"] for r in node_rows}

    phrase_pair_counts: Counter[tuple[str, str]] = Counter()
    for phs in pan_doc_phrases:
        keep = sorted(selected.intersection(phs))
        if len(keep) < 2:
            continue
        for a, b in itertools.combinations(keep, 2):
            phrase_pair_counts[(a, b)] += 1

    edges: list[dict] = []
    for r in node_rows:
        edges.append(
            {
                "from": "panethnic_hub",
                "to": r["node"],
                "edge_type": "hub_phrase",
                "edge_group": "Panethnic Hub",
                "weight_n": int(r["pan_n"]),
            }
        )

    pp_items = sorted(phrase_pair_counts.items(), key=lambda kv: kv[1], reverse=True)
    pp_items = [(k, v) for k, v in pp_items if v >= 3][: args.max_phrase_edges]
    for (a, b), n in pp_items:
        edges.append(
            {
                "from": a,
                "to": b,
                "edge_type": "phrase_phrase",
                "edge_group": "Phrase-Phrase",
                "weight_n": int(n),
            }
        )

    write_csv(pathlib.Path(args.out_nodes), node_rows)
    write_csv(pathlib.Path(args.out_edges), edges)

    print(f"Input paragraphs: {len(rows)}")
    print(f"Panethnic hub paragraphs: {pan_docs}")
    print(f"Phrases retained: {len(node_rows)}")
    print(f"Edges written: {len(edges)}")
    print(f"Wrote: {args.out_nodes}")
    print(f"Wrote: {args.out_edges}")


if __name__ == "__main__":
    main()
