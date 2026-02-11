#!/usr/bin/env python3
"""Compare theme prevalence in panethnic-appeared vs ethnic-appeared paragraphs.

Input:
- outputs/paragraph_panel_filtered.csv

Outputs:
- outputs/paragraph_theme_prevalence_by_group.csv
- outputs/paragraph_theme_contrast_panethnic_minus_ethnic.csv
- outputs/paragraph_theme_monthly_by_group.csv
"""

from __future__ import annotations

import argparse
import csv
import math
import pathlib
from collections import Counter, defaultdict


def as_int(v: str) -> int:
    try:
        return 1 if int(v) == 1 else 0
    except Exception:
        return 0


def write_csv(path: pathlib.Path, rows: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    if not rows:
        return
    with path.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)


def pretty_theme(col: str) -> str:
    x = col
    x = x.replace("policy_frame_", "")
    x = x.replace("domain_", "")
    x = x.replace("theme_", "")
    x = x.replace("_", " ")
    return x.title()


def split_anti_eqo(hit_str: str) -> tuple[int, int]:
    s = (hit_str or "").lower()
    anti = 1 if ("discriminat" in s or "nondiscriminat" in s or "anti-discrimin" in s) else 0
    eqo = 1 if ("equal opportunity" in s or "equal employment opportunity" in s or "equal access" in s) else 0
    return anti, eqo


def two_prop_pvalue(x1: int, n1: int, x2: int, n2: int) -> float:
    if min(n1, n2) <= 0:
        return 1.0
    p_pool = (x1 + x2) / (n1 + n2)
    se = math.sqrt(p_pool * (1 - p_pool) * (1 / n1 + 1 / n2))
    if se == 0:
        return 1.0
    z = ((x1 / n1) - (x2 / n2)) / se
    # two-sided p-value from normal approximation
    cdf = 0.5 * (1 + math.erf(abs(z) / math.sqrt(2)))
    return max(0.0, min(1.0, 2 * (1 - cdf)))


def odds_ratio(x1: int, n1: int, x2: int, n2: int) -> tuple[float, float]:
    # Haldane-Anscombe correction to avoid division by zero.
    a = x1 + 0.5
    b = (n1 - x1) + 0.5
    c = x2 + 0.5
    d = (n2 - x2) + 0.5
    orv = (a / b) / (c / d)
    return orv, math.log(orv)


def main() -> None:
    p = argparse.ArgumentParser(description="Paragraph-level panethnic vs ethnic theme comparison")
    p.add_argument("--panel", default="outputs/paragraph_panel_filtered.csv")
    p.add_argument("--out-prevalence", default="outputs/paragraph_theme_prevalence_by_group.csv")
    p.add_argument("--out-contrast", default="outputs/paragraph_theme_contrast_panethnic_minus_ethnic.csv")
    p.add_argument("--out-monthly", default="outputs/paragraph_theme_monthly_by_group.csv")
    args = p.parse_args()

    csv.field_size_limit(10**9)
    in_path = pathlib.Path(args.panel)
    if not in_path.exists():
        raise FileNotFoundError(f"Missing panel: {in_path}")

    with in_path.open(newline="", encoding="utf-8") as f:
        rows = list(csv.DictReader(f))

    if not rows:
        raise ValueError("No rows in panel.")

    all_cols = rows[0].keys()
    theme_cols = []
    for c in all_cols:
        if c in {"policy_frame_data", "domain_other"}:
            continue
        if c.startswith("policy_frame_") or c.startswith("domain_") or c.startswith("theme_"):
            if c.endswith("_hits"):
                continue
            theme_cols.append(c)
    # Replace combined anti-discrimination/equal-opportunity with split themes.
    combined_col = "domain_anti_discrimination_equal_opportunity"
    if combined_col in theme_cols:
        theme_cols.remove(combined_col)
    theme_cols.extend(["theme_anti_discrimination", "theme_equal_opportunity"])
    theme_cols = sorted(theme_cols)

    groups = ["panethnic_appeared", "ethnic_appeared"]
    denom = Counter()
    counts = {g: Counter() for g in groups}

    monthly = defaultdict(Counter)

    for r in rows:
        pan = as_int(r.get("has_panethnic_label", "0"))
        eth = as_int(r.get("has_ethnic_label", "0"))
        ym = f"{r.get('canonical_year','')}-{str(r.get('canonical_month','')).zfill(2)}"

        in_group = {
            "panethnic_appeared": pan == 1,
            "ethnic_appeared": eth == 1,
        }

        for g in groups:
            if not in_group[g]:
                continue
            denom[g] += 1
            monthly[(ym, g)]["n_paragraphs"] += 1
            anti, eqo = split_anti_eqo(r.get("domain_anti_discrimination_equal_opportunity_hits", ""))
            derived = {
                "theme_anti_discrimination": anti,
                "theme_equal_opportunity": eqo,
            }
            for t in theme_cols:
                v = derived.get(t, as_int(r.get(t, "0")))
                counts[g][t] += v
                monthly[(ym, g)][t] += v

    prevalence_rows: list[dict] = []
    for g in groups:
        n = max(denom[g], 1)
        for t in theme_cols:
            x = counts[g][t]
            prevalence_rows.append(
                {
                    "group": g,
                    "theme": t,
                    "theme_pretty": pretty_theme(t),
                    "n_paragraphs_group": denom[g],
                    "n_theme": x,
                    "pct_theme": round(100.0 * x / n, 4),
                }
            )

    contrast_rows: list[dict] = []
    n_pan = max(denom["panethnic_appeared"], 1)
    n_eth = max(denom["ethnic_appeared"], 1)
    for t in theme_cols:
        x_pan = counts["panethnic_appeared"][t]
        x_eth = counts["ethnic_appeared"][t]
        pct_pan = 100.0 * x_pan / n_pan
        pct_eth = 100.0 * x_eth / n_eth
        pval = two_prop_pvalue(x_pan, n_pan, x_eth, n_eth)
        orv, lor = odds_ratio(x_pan, n_pan, x_eth, n_eth)
        contrast_rows.append(
            {
                "theme": t,
                "theme_pretty": pretty_theme(t),
                "n_panethnic_paragraphs": denom["panethnic_appeared"],
                "n_ethnic_paragraphs": denom["ethnic_appeared"],
                "n_theme_panethnic": x_pan,
                "n_theme_ethnic": x_eth,
                "pct_panethnic": round(pct_pan, 4),
                "pct_ethnic": round(pct_eth, 4),
                "gap_pct_pan_minus_eth": round(pct_pan - pct_eth, 4),
                "odds_ratio_pan_vs_eth": round(orv, 6),
                "log_odds_ratio_pan_vs_eth": round(lor, 6),
                "p_value_two_prop_z": round(pval, 8),
            }
        )

    contrast_rows.sort(key=lambda r: abs(r["gap_pct_pan_minus_eth"]), reverse=True)

    monthly_rows: list[dict] = []
    for (ym, g), c in sorted(monthly.items(), key=lambda x: x[0]):
        n = max(c["n_paragraphs"], 1)
        row = {
            "year_month": ym,
            "group": g,
            "n_paragraphs": c["n_paragraphs"],
        }
        for t in theme_cols:
            val = c[t]
            row[f"{t}_n"] = val
            row[f"{t}_per100"] = round(100.0 * val / n, 4)
        monthly_rows.append(row)

    write_csv(pathlib.Path(args.out_prevalence), prevalence_rows)
    write_csv(pathlib.Path(args.out_contrast), contrast_rows)
    write_csv(pathlib.Path(args.out_monthly), monthly_rows)

    print(f"Input paragraphs: {len(rows)}")
    print(f"Panethnic-appeared paragraphs: {denom['panethnic_appeared']}")
    print(f"Ethnic-appeared paragraphs: {denom['ethnic_appeared']}")
    print(f"Wrote: {args.out_prevalence}")
    print(f"Wrote: {args.out_contrast}")
    print(f"Wrote: {args.out_monthly}")


if __name__ == "__main__":
    main()
