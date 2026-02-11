#!/usr/bin/env python3
"""Build reader-facing corpus summary tables.

Inputs:
- outputs/paragraph_panel.csv
- outputs/paragraph_panel_filtered.csv
- outputs/issue_metadata.csv

Outputs:
- outputs/corpus_summary_table.csv
- outputs/corpus_summary_table.md
- outputs/corpus_issue_level_summary.csv
"""

from __future__ import annotations

import argparse
import csv
import pathlib
import statistics
from collections import defaultdict


def read_csv(path: pathlib.Path) -> list[dict]:
    with path.open(newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def write_csv(path: pathlib.Path, rows: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    if not rows:
        return
    with path.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)


def as_int(v: str) -> int:
    try:
        return int(v)
    except Exception:
        return 0


def pct(num: float, den: float) -> float:
    return (100.0 * num / den) if den else 0.0


def q(v: float) -> str:
    return f"{v:.2f}"


def main() -> None:
    p = argparse.ArgumentParser(description="Summarize corpus at issue and corpus levels")
    p.add_argument("--raw", default="outputs/paragraph_panel.csv")
    p.add_argument("--filtered", default="outputs/paragraph_panel_filtered.csv")
    p.add_argument("--metadata", default="outputs/issue_metadata.csv")
    p.add_argument("--out-summary-csv", default="outputs/corpus_summary_table.csv")
    p.add_argument("--out-summary-md", default="outputs/corpus_summary_table.md")
    p.add_argument("--out-issue-csv", default="outputs/corpus_issue_level_summary.csv")
    args = p.parse_args()

    raw_rows = read_csv(pathlib.Path(args.raw))
    filt_rows = read_csv(pathlib.Path(args.filtered))
    meta_rows = read_csv(pathlib.Path(args.metadata))

    meta_by = {r.get("file_name", ""): r for r in meta_rows}

    raw_by_issue = defaultdict(int)
    for r in raw_rows:
        raw_by_issue[r.get("file_name", "")] += 1

    issue_stats: dict[str, dict] = defaultdict(lambda: {
        "n_paragraphs_filtered": 0,
        "n_panethnic_appeared": 0,
        "n_ethnic_appeared": 0,
        "n_both_labels": 0,
        "n_identity_labeled": 0,
        "n_invisibility": 0,
        "n_funding": 0,
    })

    for r in filt_rows:
        fn = r.get("file_name", "")
        s = issue_stats[fn]
        s["n_paragraphs_filtered"] += 1

        pan = as_int(r.get("has_panethnic_label", "0"))
        eth = as_int(r.get("has_ethnic_label", "0"))
        inv = as_int(r.get("theme_invisibility", "0"))
        fund = as_int(r.get("policy_frame_funding", "0"))

        s["n_panethnic_appeared"] += pan
        s["n_ethnic_appeared"] += eth
        s["n_both_labels"] += 1 if pan and eth else 0
        s["n_identity_labeled"] += 1 if pan or eth else 0
        s["n_invisibility"] += inv
        s["n_funding"] += fund

    issue_rows: list[dict] = []
    all_files = sorted(set(raw_by_issue.keys()) | set(issue_stats.keys()))
    for fn in all_files:
        m = meta_by.get(fn, {})
        r_n = raw_by_issue.get(fn, 0)
        s = issue_stats.get(fn, {})
        f_n = s.get("n_paragraphs_filtered", 0)
        pan_n = s.get("n_panethnic_appeared", 0)
        eth_n = s.get("n_ethnic_appeared", 0)
        id_n = s.get("n_identity_labeled", 0)
        inv_n = s.get("n_invisibility", 0)
        fund_n = s.get("n_funding", 0)

        issue_rows.append(
            {
                "file_name": fn,
                "canonical_issue_date": m.get("canonical_issue_date", ""),
                "canonical_year": m.get("canonical_year", ""),
                "canonical_month": m.get("canonical_month", ""),
                "n_paragraphs_raw": r_n,
                "n_paragraphs_filtered": f_n,
                "keep_rate_pct": round(pct(f_n, r_n), 4),
                "n_identity_labeled": id_n,
                "identity_labeled_pct": round(pct(id_n, f_n), 4),
                "n_panethnic_appeared": pan_n,
                "panethnic_appeared_pct": round(pct(pan_n, f_n), 4),
                "n_ethnic_appeared": eth_n,
                "ethnic_appeared_pct": round(pct(eth_n, f_n), 4),
                "n_both_labels": s.get("n_both_labels", 0),
                "both_labels_pct": round(pct(s.get("n_both_labels", 0), f_n), 4),
                "n_invisibility": inv_n,
                "invisibility_pct": round(pct(inv_n, f_n), 4),
                "n_funding": fund_n,
                "funding_pct": round(pct(fund_n, f_n), 4),
            }
        )

    issue_rows.sort(key=lambda r: (r["canonical_issue_date"], r["file_name"]))
    write_csv(pathlib.Path(args.out_issue_csv), issue_rows)

    raw_counts = [r["n_paragraphs_raw"] for r in issue_rows]
    filt_counts = [r["n_paragraphs_filtered"] for r in issue_rows]
    id_counts = [r["n_identity_labeled"] for r in issue_rows]
    pan_counts = [r["n_panethnic_appeared"] for r in issue_rows]
    eth_counts = [r["n_ethnic_appeared"] for r in issue_rows]
    inv_counts = [r["n_invisibility"] for r in issue_rows]
    fund_counts = [r["n_funding"] for r in issue_rows]

    dates = [r["canonical_issue_date"] for r in issue_rows if r["canonical_issue_date"]]
    period = f"{min(dates)} to {max(dates)}" if dates else ""

    summary_rows = [
        {"metric": "issues_n", "value": str(len(issue_rows))},
        {"metric": "issue_period", "value": period},
        {"metric": "paragraphs_raw_total", "value": str(sum(raw_counts))},
        {"metric": "paragraphs_filtered_total", "value": str(sum(filt_counts))},
        {"metric": "filter_keep_rate_pct", "value": q(pct(sum(filt_counts), sum(raw_counts)))},
        {"metric": "paragraphs_per_issue_raw_mean", "value": q(statistics.mean(raw_counts) if raw_counts else 0)},
        {"metric": "paragraphs_per_issue_raw_median", "value": q(statistics.median(raw_counts) if raw_counts else 0)},
        {"metric": "paragraphs_per_issue_raw_min", "value": str(min(raw_counts) if raw_counts else 0)},
        {"metric": "paragraphs_per_issue_raw_max", "value": str(max(raw_counts) if raw_counts else 0)},
        {"metric": "paragraphs_per_issue_filtered_mean", "value": q(statistics.mean(filt_counts) if filt_counts else 0)},
        {"metric": "paragraphs_per_issue_filtered_median", "value": q(statistics.median(filt_counts) if filt_counts else 0)},
        {"metric": "identity_labeled_per_issue_mean", "value": q(statistics.mean(id_counts) if id_counts else 0)},
        {"metric": "panethnic_appeared_per_issue_mean", "value": q(statistics.mean(pan_counts) if pan_counts else 0)},
        {"metric": "ethnic_appeared_per_issue_mean", "value": q(statistics.mean(eth_counts) if eth_counts else 0)},
        {"metric": "invisibility_paragraphs_per_issue_mean", "value": q(statistics.mean(inv_counts) if inv_counts else 0)},
        {"metric": "funding_paragraphs_per_issue_mean", "value": q(statistics.mean(fund_counts) if fund_counts else 0)},
        {"metric": "identity_labeled_share_in_filtered_pct", "value": q(pct(sum(id_counts), sum(filt_counts)))},
        {"metric": "invisibility_share_in_filtered_pct", "value": q(pct(sum(inv_counts), sum(filt_counts)))},
        {"metric": "funding_share_in_filtered_pct", "value": q(pct(sum(fund_counts), sum(filt_counts)))},
    ]
    write_csv(pathlib.Path(args.out_summary_csv), summary_rows)

    # reader-friendly markdown table
    md_lines = ["| Metric | Value |", "|---|---:|"]
    for r in summary_rows:
        md_lines.append(f"| `{r['metric']}` | {r['value']} |")
    out_md = pathlib.Path(args.out_summary_md)
    out_md.parent.mkdir(parents=True, exist_ok=True)
    out_md.write_text("\n".join(md_lines) + "\n", encoding="utf-8")

    print(f"Issues summarized: {len(issue_rows)}")
    print(f"Wrote: {args.out_issue_csv}")
    print(f"Wrote: {args.out_summary_csv}")
    print(f"Wrote: {args.out_summary_md}")


if __name__ == "__main__":
    main()

