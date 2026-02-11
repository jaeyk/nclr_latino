#!/usr/bin/env python3
"""Quality-control filter for paragraph_panel.csv.

Input:
- outputs/paragraph_panel.csv

Outputs:
- outputs/paragraph_panel_qc.csv
- outputs/paragraph_panel_filtered.csv
"""

from __future__ import annotations

import argparse
import csv
import pathlib
import re


def write_csv(path: pathlib.Path, rows: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    if not rows:
        return
    with path.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)


def text_metrics(text: str) -> dict[str, float]:
    txt = text or ""
    n_chars = len(txt)

    alpha_chars = sum(1 for c in txt if c.isalpha())
    digit_chars = sum(1 for c in txt if c.isdigit())
    space_chars = sum(1 for c in txt if c.isspace())
    punct_chars = max(0, n_chars - alpha_chars - digit_chars - space_chars)

    tokens = re.findall(r"\S+", txt)
    word_tokens = re.findall(r"\b[a-zA-Z][a-zA-Z'-]*\b", txt)

    def has_vowel(tok: str) -> bool:
        return bool(re.search(r"[aeiouAEIOU]", tok))

    alpha_like = [t for t in tokens if re.search(r"[A-Za-z]", t)]
    no_vowel = [t for t in alpha_like if not has_vowel(t)]

    return {
        "n_chars": n_chars,
        "n_tokens": len(tokens),
        "n_word_tokens": len(word_tokens),
        "alpha_ratio": (alpha_chars / n_chars) if n_chars else 0.0,
        "digit_ratio": (digit_chars / n_chars) if n_chars else 0.0,
        "punct_ratio": (punct_chars / n_chars) if n_chars else 0.0,
        "nonvowel_token_ratio": (len(no_vowel) / len(alpha_like)) if alpha_like else 0.0,
    }


def keep_row(row: dict, m: dict[str, float]) -> tuple[bool, str]:
    reasons: list[str] = []

    # Keep threshold tuned to remove heavy OCR debris but retain short policy paragraphs.
    if m["n_tokens"] < 8:
        reasons.append("too_few_tokens")
    if m["n_word_tokens"] < 5:
        reasons.append("too_few_word_tokens")
    if m["alpha_ratio"] < 0.55:
        reasons.append("low_alpha_ratio")
    if m["punct_ratio"] > 0.22:
        reasons.append("high_punct_ratio")
    if m["nonvowel_token_ratio"] > 0.45:
        reasons.append("high_nonvowel_token_ratio")

    # Rescue rows that are clearly substantively relevant even if OCR is noisy.
    has_substantive_signal = any(
        row.get(k, "0") == "1"
        for k in [
            "has_panethnic_label",
            "has_ethnic_label",
            "policy_frame_funding",
            "domain_education",
            "domain_employment_labor",
            "domain_health",
            "domain_housing",
            "domain_immigration",
            "domain_civil_rights_voting",
            "domain_anti_discrimination_equal_opportunity",
            "domain_social_policy",
            "domain_criminal_justice",
            "domain_business_development",
            "theme_invisibility",
        ]
    )

    if reasons and has_substantive_signal and m["n_word_tokens"] >= 5 and m["alpha_ratio"] >= 0.45:
        return True, "rescued_substantive_signal"

    return (len(reasons) == 0), "|".join(reasons) if reasons else "pass"


def main() -> None:
    parser = argparse.ArgumentParser(description="Filter paragraph panel by OCR/text quality")
    parser.add_argument("--panel", default="outputs/paragraph_panel.csv")
    parser.add_argument("--qc-out", default="outputs/paragraph_panel_qc.csv")
    parser.add_argument("--filtered-out", default="outputs/paragraph_panel_filtered.csv")
    args = parser.parse_args()

    csv.field_size_limit(10**9)

    with pathlib.Path(args.panel).open(newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f))

    qc_rows: list[dict] = []
    filtered_rows: list[dict] = []

    for r in rows:
        m = text_metrics(r.get("paragraph_text", ""))
        keep, reason = keep_row(r, m)

        row_qc = dict(r)
        row_qc.update(
            {
                "quality_keep": "1" if keep else "0",
                "quality_reason": reason,
                "n_chars": str(m["n_chars"]),
                "n_tokens": str(m["n_tokens"]),
                "n_word_tokens": str(m["n_word_tokens"]),
                "alpha_ratio": f"{m['alpha_ratio']:.4f}",
                "digit_ratio": f"{m['digit_ratio']:.4f}",
                "punct_ratio": f"{m['punct_ratio']:.4f}",
                "nonvowel_token_ratio": f"{m['nonvowel_token_ratio']:.4f}",
            }
        )
        qc_rows.append(row_qc)

        if keep:
            filtered_rows.append(r)

    write_csv(pathlib.Path(args.qc_out), qc_rows)
    write_csv(pathlib.Path(args.filtered_out), filtered_rows)

    print(f"Input rows: {len(rows)}")
    print(f"Kept rows: {len(filtered_rows)}")
    print(f"Dropped rows: {len(rows) - len(filtered_rows)}")
    print(f"Wrote: {args.qc_out}")
    print(f"Wrote: {args.filtered_out}")


if __name__ == "__main__":
    main()
