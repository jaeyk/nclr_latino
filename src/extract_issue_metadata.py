#!/usr/bin/env python3
"""Extract issue-level temporal metadata from NCLR PDF newsletters.

Pipeline:
1) Parse primary date from filename (YYYY_MM.pdf)
2) Extract first-page text with pdftotext -layout
3) Parse month/year + volume/issue metadata from first page
4) If needed, OCR first page (pdftoppm + tesseract) and retry parsing
5) Compare filename date vs front-page date and flag conflicts

Outputs:
- outputs/issue_metadata.csv
- outputs/date_conflicts.csv
"""

from __future__ import annotations

import argparse
import csv
import pathlib
import re
import subprocess
import tempfile
from dataclasses import dataclass
from typing import Optional


MONTH_ALIASES = {
    "JAN": 1,
    "JANUARY": 1,
    "FEB": 2,
    "FEBRUARY": 2,
    "MAR": 3,
    "MARCH": 3,
    "APR": 4,
    "APRIL": 4,
    "MAY": 5,
    "JUN": 6,
    "JUNE": 6,
    "JUL": 7,
    "JULY": 7,
    "AUG": 8,
    "AUGUST": 8,
    "SEP": 9,
    "SEPT": 9,
    "SEPTEMBER": 9,
    "OCT": 10,
    "OCTOBER": 10,
    "NOV": 11,
    "NOVEMBER": 11,
    "DEC": 12,
    "DECEMBER": 12,
}

QUARTER_ALIASES = {
    "FIRST": 3,
    "SECOND": 6,
    "THIRD": 9,
    "FOURTH": 12,
}


@dataclass
class FrontParse:
    year: Optional[int]
    month: Optional[int]
    volume: Optional[str]
    issue_no: Optional[str]
    period_label: Optional[str]
    parse_method: str
    confidence: str


MONTH_PATTERN = re.compile(
    r"\b(JAN(?:UARY)?|FEB(?:RUARY)?|MAR(?:CH)?|APR(?:IL)?|MAY|JUN(?:E)?|"
    r"JUL(?:Y)?|AUG(?:UST)?|SEP(?:T(?:EMBER)?)?|OCT(?:OBER)?|"
    r"NOV(?:EMBER)?|DEC(?:EMBER)?)\b",
    re.IGNORECASE,
)

QUARTER_PATTERN = re.compile(r"\b(FIRST|SECOND|THIRD|FOURTH)\s+QUARTER\b", re.IGNORECASE)
YEAR_PATTERN = re.compile(r"\b(19\d{2}|20\d{2})\b")

VOL_PATTERN = re.compile(r"\bV[O0][LLI1\.]?\s*([0-9]{1,2}|[IVX]{1,5})\b", re.IGNORECASE)
ISSUE_PATTERN = re.compile(r"\b[MN]?[O0][\.]?\s*([0-9]{1,2}|[IVX]{1,5})\b", re.IGNORECASE)


def run_cmd(cmd: list[str]) -> str:
    p = subprocess.run(cmd, check=False, capture_output=True, text=True)
    if p.returncode != 0:
        return ""
    return p.stdout or ""


def first_page_text_pdftotext(pdf_path: pathlib.Path) -> str:
    return run_cmd(["pdftotext", "-f", "1", "-l", "1", "-layout", str(pdf_path), "-"])


def first_page_text_ocr(pdf_path: pathlib.Path, dpi: int = 250) -> str:
    with tempfile.TemporaryDirectory() as tdir:
        base = pathlib.Path(tdir) / "front"
        subprocess.run(
            [
                "pdftoppm",
                "-f",
                "1",
                "-singlefile",
                "-r",
                str(dpi),
                "-gray",
                "-png",
                str(pdf_path),
                str(base),
            ],
            check=False,
            capture_output=True,
            text=True,
        )
        png = f"{base}.png"
        return run_cmd(["tesseract", png, "stdout", "--psm", "6"])


def normalize_ocr_text(text: str) -> str:
    # Helps with common OCR confusions in headers like V0L/M0.
    return (
        text.upper()
        .replace("V0L", "VOL")
        .replace("VOI", "VOL")
        .replace("M0", "NO")
        .replace("N0", "NO")
        .replace("ISSISS", "ISSUES")
    )


def parse_year(text: str, anchor_idx: Optional[int] = None) -> Optional[int]:
    years = [(m.start(), int(m.group(1))) for m in YEAR_PATTERN.finditer(text)]
    if not years:
        return None
    if anchor_idx is None:
        return years[0][1]
    return min(years, key=lambda t: abs(t[0] - anchor_idx))[1]


def parse_front_metadata(text: str, parse_method: str) -> FrontParse:
    clean = normalize_ocr_text(text)

    month = None
    year = None
    confidence = "low"

    month_match = MONTH_PATTERN.search(clean)
    if month_match:
        month_key = month_match.group(1).upper().rstrip(".")
        month = MONTH_ALIASES.get(month_key)
        year = parse_year(clean, anchor_idx=month_match.start())
        confidence = "high" if month and year else "medium"
    else:
        q_match = QUARTER_PATTERN.search(clean)
        if q_match:
            q = q_match.group(1).upper()
            month = QUARTER_ALIASES.get(q)
            year = parse_year(clean, anchor_idx=q_match.start())
            confidence = "medium" if year else "low"
        else:
            year = parse_year(clean)

    vol = None
    issue = None

    vol_match = VOL_PATTERN.search(clean)
    if vol_match:
        vol = vol_match.group(1)

    issue_match = ISSUE_PATTERN.search(clean)
    if issue_match:
        issue = issue_match.group(1)

    period_label = None
    q_match = QUARTER_PATTERN.search(clean)
    if q_match:
        period_label = f"{q_match.group(1).title()} Quarter"

    return FrontParse(
        year=year,
        month=month,
        volume=vol,
        issue_no=issue,
        period_label=period_label,
        parse_method=parse_method,
        confidence=confidence,
    )


def iso_date(year: Optional[int], month: Optional[int]) -> Optional[str]:
    if year is None or month is None:
        return None
    return f"{year:04d}-{month:02d}-01"


def parse_filename_date(pdf_path: pathlib.Path) -> tuple[Optional[int], Optional[int]]:
    m = re.match(r"^(\d{4})_(\d{2})\.pdf$", pdf_path.name)
    if not m:
        return None, None
    return int(m.group(1)), int(m.group(2))


def resolve_date_source(file_year: Optional[int], file_month: Optional[int], front: FrontParse) -> tuple[str, bool]:
    if front.year is None and front.month is None:
        return "filename", False
    if front.year is not None and front.month is not None and file_year is not None and file_month is not None:
        if front.year == file_year and front.month == file_month:
            return "both", False
        return "conflict", True
    if front.year is not None and file_year is not None and front.year != file_year:
        return "conflict", True
    return "filename_with_front_partial", False


def validation_status(date_source: str) -> str:
    if date_source == "both":
        return "validated_match"
    if date_source == "conflict":
        return "review_needed"
    if date_source == "filename_with_front_partial":
        return "front_partial"
    return "front_missing"


def process_one(pdf_path: pathlib.Path, use_ocr_fallback: bool = False) -> dict:
    file_year, file_month = parse_filename_date(pdf_path)

    text_pdf = first_page_text_pdftotext(pdf_path)
    front_pdf = parse_front_metadata(text_pdf, parse_method="pdftotext_layout")

    front = front_pdf
    if use_ocr_fallback and (front.year is None or front.month is None):
        text_ocr = first_page_text_ocr(pdf_path)
        front_ocr = parse_front_metadata(text_ocr, parse_method="ocr_tesseract")

        # Prefer OCR only if it adds additional date information.
        pdf_score = int(front_pdf.year is not None) + int(front_pdf.month is not None)
        ocr_score = int(front_ocr.year is not None) + int(front_ocr.month is not None)
        if ocr_score > pdf_score:
            front = front_ocr

    date_source, conflict = resolve_date_source(file_year, file_month, front)
    status = validation_status(date_source)

    file_issue_date = iso_date(file_year, file_month)
    front_issue_date = iso_date(front.year, front.month)

    return {
        "file_name": pdf_path.name,
        "file_year": file_year,
        "file_month": file_month,
        "file_issue_date": file_issue_date,
        "canonical_year": file_year,
        "canonical_month": file_month,
        "canonical_issue_date": file_issue_date,
        "front_year": front.year,
        "front_month": front.month,
        "front_issue_date": front_issue_date,
        "front_volume": front.volume,
        "front_issue_no": front.issue_no,
        "front_period_label": front.period_label,
        "front_parse_method": front.parse_method,
        "front_confidence": front.confidence,
        "date_source": date_source,
        "date_conflict": conflict,
        "validation_status": status,
        "date_review_needed": conflict,
    }


def write_csv(path: pathlib.Path, rows: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    if not rows:
        return
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def main() -> None:
    parser = argparse.ArgumentParser(description="Extract issue metadata from NCLR PDFs")
    parser.add_argument("--raw-dir", default="raw_data", help="Directory containing issue PDFs")
    parser.add_argument("--out", default="outputs/issue_metadata.csv", help="Output CSV path")
    parser.add_argument(
        "--conflicts-out",
        default="outputs/date_conflicts.csv",
        help="Output CSV path for date conflicts",
    )
    parser.add_argument(
        "--ocr-fallback",
        action="store_true",
        help="Use OCR for first page when pdftotext cannot parse month/year",
    )
    args = parser.parse_args()

    raw_dir = pathlib.Path(args.raw_dir)
    pdfs = sorted(raw_dir.glob("*.pdf"))

    rows = [process_one(pdf, use_ocr_fallback=args.ocr_fallback) for pdf in pdfs]
    write_csv(pathlib.Path(args.out), rows)

    conflicts = [r for r in rows if r["date_conflict"]]
    write_csv(pathlib.Path(args.conflicts_out), conflicts)

    total = len(rows)
    n_both = sum(1 for r in rows if r["date_source"] == "both")
    n_file = sum(1 for r in rows if r["date_source"] == "filename")
    n_partial = sum(1 for r in rows if r["date_source"] == "filename_with_front_partial")
    n_conf = len(conflicts)

    print(f"Processed {total} PDFs")
    print(f"date_source=both: {n_both}")
    print(f"date_source=filename: {n_file}")
    print(f"date_source=filename_with_front_partial: {n_partial}")
    print(f"date_conflicts: {n_conf}")
    print(f"Wrote: {args.out}")
    print(f"Wrote: {args.conflicts_out}")


if __name__ == "__main__":
    main()
