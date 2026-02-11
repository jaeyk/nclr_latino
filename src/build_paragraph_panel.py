#!/usr/bin/env python3
"""Build paragraph-level analysis panel with policy and identity labels.

Input:
- outputs/parsed/line_text.csv (from parse_multicolumn_pdfs.py)
- outputs/metadata/issue_metadata.csv (from extract_issue_metadata.py)

Output:
- outputs/analysis/paragraph_panel.csv
"""

from __future__ import annotations

import argparse
import csv
import pathlib
import re
import statistics
from collections import defaultdict


def bool_int(x: bool) -> int:
    return 1 if x else 0


def normalize_text(text: str) -> str:
    t = text.lower()
    t = t.replace("\u00ad", "")
    t = t.replace("\u2019", "'")
    t = re.sub(r"\s+", " ", t)
    return t.strip()


IDENTITY_PATTERNS = {
    "panethnic": re.compile(
        r"\b(hispanic(?:s)?|latino(?:s|a|as)?|spanish[- ]speaking|spanish[- ]surname)\b",
        re.IGNORECASE,
    ),
    "ethnic": re.compile(
        r"\b(chicano(?:s|a|as)?|mexican[- ]american(?:s)?|puerto rican(?:s)?|"
        r"cuban[- ]american(?:s)?|dominican(?:s)?|salvadoran(?:s)?|"
        r"guatemalan(?:s)?|nicaraguan(?:s)?)\b",
        re.IGNORECASE,
    ),
}

FRAME_PATTERNS = {
    "policy_frame_data": re.compile(
        r"\b(data|dataset|statistics?|statistical|census|survey|surveys|percent(?:age)?s?|"
        r"rate(?:s)?|estimate(?:s)?|count(?:s|ed|ing)?|undercount|report(?:ed|s)?|"
        r"evidence|numbers?)\b",
        re.IGNORECASE,
    ),
    "policy_frame_funding": re.compile(
        r"\b(fund(?:s|ed|ing)?|funding|grant(?:s)?|appropriation(?:s)?|budget(?:s)?|"
        r"allocat(?:e|ed|ion|ions)|federal dollars?|subsid(?:y|ies)|revenue(?:s)?|"
        r"financ(?:e|ed|ing)|endow(?:ed|ment)?)\b",
        re.IGNORECASE,
    ),
}

DOMAIN_PATTERNS = {
    "domain_education": re.compile(
        r"\b(education|school(?:s)?|student(?:s)?|teacher(?:s)?|classroom|curriculum|"
        r"bilingual education|head start|college|university|dropout)\b",
        re.IGNORECASE,
    ),
    "domain_employment_labor": re.compile(
        r"\b(employ(?:ment|ed|er|ers)?|labor|worker(?:s)?|wage(?:s)?|unemployment|"
        r"job(?:s)?|farm ?worker(?:s)?|union(?:s)?)\b",
        re.IGNORECASE,
    ),
    "domain_health": re.compile(
        r"\b(health|healthcare|medical|medicaid|medicare|clinic(?:s)?|hospital(?:s)?|"
        r"mental health|public health|nutrition)\b",
        re.IGNORECASE,
    ),
    "domain_housing": re.compile(
        r"\b(housing|rent(?:al)?|tenant(?:s)?|mortgage(?:s)?|home(?:s|ownership)?|"
        r"homeless(?:ness)?)\b",
        re.IGNORECASE,
    ),
    "domain_immigration": re.compile(
        r"\b(immigration|immigrant(?:s)?|migrant(?:s)?|border|deport(?:ation|ed)?|"
        r"undocumented|refugee(?:s)?|visa(?:s)?)\b",
        re.IGNORECASE,
    ),
    "domain_civil_rights_voting": re.compile(
        r"\b(civil rights?|vot(?:e|ing|er|ers)|elect(?:ion|oral)?|redistrict(?:ing)?|"
        r"representation|political participation)\b",
        re.IGNORECASE,
    ),
    "domain_anti_discrimination_equal_opportunity": re.compile(
        r"\b(anti[- ]discriminat(?:ion|ory)|discriminat(?:ion|e|ed|ory)|equal opportunity|"
        r"equal employment opportunity|affirmative action|equal access|fair treatment|"
        r"racial equality|nondiscriminat(?:ion|ory)|eeoc)\b",
        re.IGNORECASE,
    ),
    "domain_social_policy": re.compile(
        r"\b(social policy|social services?|public assistance|welfare|poverty|poor\b|"
        r"low[- ]income|income inequality|economic inequality|inequalit(?:y|ies)|inequit(?:y|ies)|"
        r"opportunity gap|social mobility|safety net|food stamps?|family services?|child care)\b",
        re.IGNORECASE,
    ),
    "domain_criminal_justice": re.compile(
        r"\b(police|policing|prison(?:s)?|jail(?:s)?|incarcerat(?:ion|ed)|crime|"
        r"criminal justice|court(?:s)?|sentenc(?:e|ing))\b",
        re.IGNORECASE,
    ),
    "domain_business_development": re.compile(
        r"\b(small business(?:es)?|business development|entrepreneur(?:s|ship)?|"
        r"mesbic|loan(?:s)?|investment|contract(?:s|ing)?|procurement)\b",
        re.IGNORECASE,
    ),
    "theme_invisibility": re.compile(
        r"\b(invisible|invisibility|ignored|overlooked|unseen|forgotten|voiceless|silenced|"
        r"marginali[sz]ed|excluded|left out|underrepresented|hidden population(?:s)?|"
        r"undercount(?:ed|ing)?|undercount(?:s)?|not counted|not count(?:ed)?|"
        r"not captured|uncaptured|miscount(?:ed|ing)?|statistically invisible|"
        r"ignored by (?:the )?(?:data|statistics)|missing from (?:the )?(?:data|statistics)|"
        r"not reflected in (?:the )?(?:data|statistics)|not in (?:the )?census)\b",
        re.IGNORECASE,
    ),
}


def identity_type(text: str) -> str:
    has_pan = bool(IDENTITY_PATTERNS["panethnic"].search(text))
    has_eth = bool(IDENTITY_PATTERNS["ethnic"].search(text))
    if has_pan and has_eth:
        return "both"
    if has_pan:
        return "panethnic"
    if has_eth:
        return "ethnic"
    return "none"


def extract_matches(pattern: re.Pattern, text: str) -> str:
    found = sorted(set(m.group(0).lower() for m in pattern.finditer(text)))
    return "|".join(found)


def read_metadata(path: pathlib.Path) -> dict[str, dict[str, str]]:
    out: dict[str, dict[str, str]] = {}
    with path.open(newline="", encoding="utf-8") as f:
        r = csv.DictReader(f)
        for row in r:
            out[row["file_name"]] = row
    return out


def build_paragraphs(line_rows: list[dict]) -> list[dict]:
    # Group lines by issue/page/column then stitch paragraphs by vertical gap.
    grouped: dict[tuple[str, str, str], list[dict]] = defaultdict(list)
    for row in line_rows:
        grouped[(row["file_name"], row["page_number"], row["column_index"])].append(row)

    out: list[dict] = []
    for (file_name, page_number, column_index), rows in grouped.items():
        rows_sorted = sorted(rows, key=lambda r: (float(r["y0"]), float(r["x0"])))
        heights = [max(1.0, float(r["y1"]) - float(r["y0"])) for r in rows_sorted]
        median_h = statistics.median(heights) if heights else 9.0
        para_gap = max(8.0, median_h * 1.25)

        para_idx = 1
        para_lines: list[str] = []
        prev_y1 = None

        def flush_paragraph(idx: int, lines: list[str]) -> None:
            if not lines:
                return
            raw = "\n".join(lines).strip()
            raw_norm = normalize_text(raw)
            # Filter extreme debris-only segments.
            if len(re.sub(r"[^a-z0-9]", "", raw_norm)) < 8:
                return
            out.append(
                {
                    "file_name": file_name,
                    "page_number": int(page_number),
                    "column_index": int(column_index),
                    "paragraph_in_column": idx,
                    "paragraph_text": raw,
                    "paragraph_text_norm": raw_norm,
                }
            )

        for row in rows_sorted:
            y0 = float(row["y0"])
            y1 = float(row["y1"])
            txt = (row.get("text") or "").strip()
            if not txt:
                continue

            if prev_y1 is not None and (y0 - prev_y1) > para_gap:
                flush_paragraph(para_idx, para_lines)
                para_idx += 1
                para_lines = [txt]
            else:
                para_lines.append(txt)
            prev_y1 = y1

        flush_paragraph(para_idx, para_lines)

    # Deterministic row ordering.
    out.sort(key=lambda r: (r["file_name"], r["page_number"], r["column_index"], r["paragraph_in_column"]))

    # Add within-page paragraph sequence.
    seq = defaultdict(int)
    for r in out:
        key = (r["file_name"], r["page_number"])
        seq[key] += 1
        r["paragraph_in_page"] = seq[key]

    return out


def main() -> None:
    parser = argparse.ArgumentParser(description="Build paragraph-level panel with policy + identity labels")
    parser.add_argument("--line-csv", default="outputs/parsed/line_text.csv")
    parser.add_argument("--metadata-csv", default="outputs/metadata/issue_metadata.csv")
    parser.add_argument("--out", default="outputs/analysis/paragraph_panel.csv")
    args = parser.parse_args()

    csv.field_size_limit(10**9)

    metadata = read_metadata(pathlib.Path(args.metadata_csv))

    with pathlib.Path(args.line_csv).open(newline="", encoding="utf-8") as f:
        line_rows = list(csv.DictReader(f))

    paragraphs = build_paragraphs(line_rows)

    rows_out: list[dict] = []
    for p in paragraphs:
        txt = p["paragraph_text_norm"]

        id_type = identity_type(txt)
        pan_hits = extract_matches(IDENTITY_PATTERNS["panethnic"], txt)
        eth_hits = extract_matches(IDENTITY_PATTERNS["ethnic"], txt)

        frame_data_hits = extract_matches(FRAME_PATTERNS["policy_frame_data"], txt)
        frame_fund_hits = extract_matches(FRAME_PATTERNS["policy_frame_funding"], txt)

        domain_hits = {}
        for k, pat in DOMAIN_PATTERNS.items():
            domain_hits[k] = extract_matches(pat, txt)

        has_any_domain = any(bool(domain_hits[k]) for k in DOMAIN_PATTERNS if k.startswith("domain_"))

        meta = metadata.get(p["file_name"], {})
        para_id = f"{p['file_name'].replace('.pdf','')}_p{p['page_number']}_c{p['column_index']}_n{p['paragraph_in_column']}"

        row = {
            "paragraph_id": para_id,
            "file_name": p["file_name"],
            "canonical_year": meta.get("canonical_year", ""),
            "canonical_month": meta.get("canonical_month", ""),
            "canonical_issue_date": meta.get("canonical_issue_date", ""),
            "validation_status": meta.get("validation_status", ""),
            "page_number": p["page_number"],
            "column_index": p["column_index"],
            "paragraph_in_column": p["paragraph_in_column"],
            "paragraph_in_page": p["paragraph_in_page"],
            "identity_type": id_type,
            "has_panethnic_label": bool_int(bool(pan_hits)),
            "has_ethnic_label": bool_int(bool(eth_hits)),
            "panethnic_hits": pan_hits,
            "ethnic_hits": eth_hits,
            "policy_frame_data": bool_int(bool(frame_data_hits)),
            "policy_frame_data_hits": frame_data_hits,
            "policy_frame_funding": bool_int(bool(frame_fund_hits)),
            "policy_frame_funding_hits": frame_fund_hits,
        }

        for k in DOMAIN_PATTERNS:
            row[k] = bool_int(bool(domain_hits[k]))
            row[f"{k}_hits"] = domain_hits[k]

        row["domain_other"] = bool_int(not has_any_domain)
        row["paragraph_text"] = p["paragraph_text"]
        rows_out.append(row)

    out_path = pathlib.Path(args.out)
    out_path.parent.mkdir(parents=True, exist_ok=True)

    if not rows_out:
        print("No paragraph rows generated.")
        return

    with out_path.open("w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=list(rows_out[0].keys()))
        w.writeheader()
        w.writerows(rows_out)

    print(f"Parsed line rows: {len(line_rows)}")
    print(f"Paragraph rows: {len(rows_out)}")
    print(f"Wrote: {args.out}")


if __name__ == "__main__":
    main()
