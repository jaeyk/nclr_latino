#!/usr/bin/env python3
"""Parse NCLR PDFs with layout-aware multi-column reading order.

Uses `pdftotext -bbox-layout` to extract line coordinates, detects columns per
page, and reconstructs reading order as:
- column 1 top->bottom
- column 2 top->bottom
- ...

Outputs:
- outputs/page_text.csv
- outputs/issue_text.csv
- outputs/line_text.csv
"""

from __future__ import annotations

import argparse
import csv
import pathlib
import re
import statistics
import subprocess
import xml.etree.ElementTree as ET
from dataclasses import dataclass
from typing import Optional


NS = "{http://www.w3.org/1999/xhtml}"


@dataclass
class LineUnit:
    file_name: str
    page_number: int
    line_idx: int
    x0: float
    y0: float
    x1: float
    y1: float
    text: str


def run_cmd(cmd: list[str]) -> str:
    p = subprocess.run(cmd, check=False, capture_output=True, text=True)
    if p.returncode != 0:
        raise RuntimeError(f"Command failed: {' '.join(cmd)}\n{p.stderr}")
    return p.stdout


def parse_filename_date(file_name: str) -> tuple[Optional[int], Optional[int], Optional[str]]:
    m = re.match(r"^(\d{4})_(\d{2})\.pdf$", file_name)
    if not m:
        return None, None, None
    y = int(m.group(1))
    mth = int(m.group(2))
    return y, mth, f"{y:04d}-{mth:02d}-01"


def clean_text(text: str) -> str:
    text = text.replace("\u00ad", "")
    text = text.replace("\ufb01", "fi").replace("\ufb02", "fl")
    text = text.replace("\u2019", "'")
    text = re.sub(r"[ \t]+", " ", text)
    text = re.sub(r"\n{3,}", "\n\n", text)
    return text.strip()


def line_from_xml(line_el: ET.Element, file_name: str, page_number: int, line_idx: int) -> Optional[LineUnit]:
    x0 = float(line_el.attrib.get("xMin", "0"))
    y0 = float(line_el.attrib.get("yMin", "0"))
    x1 = float(line_el.attrib.get("xMax", "0"))
    y1 = float(line_el.attrib.get("yMax", "0"))

    words = [w.text or "" for w in line_el.findall(f"{NS}word")]
    words = [w for w in words if w]
    if not words:
        return None

    text = clean_text(" ".join(words))
    if not text:
        return None

    # Drop visual debris with too little signal.
    alnum = re.sub(r"[^A-Za-z0-9]", "", text)
    if len(alnum) < 2:
        return None

    return LineUnit(
        file_name=file_name,
        page_number=page_number,
        line_idx=line_idx,
        x0=x0,
        y0=y0,
        x1=x1,
        y1=y1,
        text=text,
    )


def detect_column_centers(lines: list[LineUnit], page_width: float) -> list[float]:
    if not lines:
        return []

    substantial = [
        l
        for l in lines
        if len(re.sub(r"[^A-Za-z]", "", l.text)) >= 20 and (l.y1 - l.y0) >= 6
    ]
    candidates = substantial if len(substantial) >= 8 else [l for l in lines if len(l.text) >= 5]
    if not candidates:
        candidates = lines

    centers = sorted(((l.x0 + l.x1) / 2.0) for l in candidates)
    if len(centers) < 4:
        return [sum(centers) / len(centers)]

    gap_threshold = page_width * 0.18
    groups: list[list[float]] = [[centers[0]]]
    for c in centers[1:]:
        if c - groups[-1][-1] > gap_threshold:
            groups.append([c])
        else:
            groups[-1].append(c)

    centers_out = [sum(g) / len(g) for g in groups]
    if len(centers_out) > 3:
        centers_out = centers_out[:3]
    return centers_out


def assign_column(x0: float, x1: float, centers: list[float]) -> int:
    if not centers:
        return 0
    cx = (x0 + x1) / 2.0
    return min(range(len(centers)), key=lambda i: abs(cx - centers[i]))


def parse_pdf_lines(pdf_path: pathlib.Path) -> tuple[list[LineUnit], dict[int, float]]:
    xml_text = run_cmd(["pdftotext", "-bbox-layout", str(pdf_path), "-"])
    root = ET.fromstring(xml_text)

    lines: list[LineUnit] = []
    page_widths: dict[int, float] = {}

    pages = root.findall(f".//{NS}page")
    for p_idx, page in enumerate(pages, start=1):
        page_widths[p_idx] = float(page.attrib.get("width", "0"))
        line_idx = 0
        for flow in page.findall(f"{NS}flow"):
            for block in flow.findall(f"{NS}block"):
                for line_el in block.findall(f"{NS}line"):
                    lu = line_from_xml(line_el, pdf_path.name, p_idx, line_idx)
                    line_idx += 1
                    if lu is not None:
                        lines.append(lu)

    return lines, page_widths


def join_lines_into_paragraphs(lines_sorted: list[LineUnit]) -> str:
    if not lines_sorted:
        return ""

    heights = [max(1.0, l.y1 - l.y0) for l in lines_sorted]
    median_h = statistics.median(heights) if heights else 9.0
    para_gap = max(8.0, median_h * 1.25)

    out_parts: list[str] = [lines_sorted[0].text]
    prev = lines_sorted[0]
    for cur in lines_sorted[1:]:
        y_gap = cur.y0 - prev.y1
        if y_gap > para_gap:
            out_parts.append("\n\n" + cur.text)
        else:
            out_parts.append("\n" + cur.text)
        prev = cur

    return clean_text("".join(out_parts))


def build_rows_for_pdf(pdf_path: pathlib.Path) -> tuple[list[dict], list[dict], dict]:
    lines, page_widths = parse_pdf_lines(pdf_path)

    by_page: dict[int, list[LineUnit]] = {}
    for l in lines:
        by_page.setdefault(l.page_number, []).append(l)

    line_rows: list[dict] = []
    page_rows: list[dict] = []
    page_texts: list[str] = []

    for page_number in sorted(page_widths.keys()):
        page_lines = by_page.get(page_number, [])
        width = page_widths[page_number]

        centers = detect_column_centers(page_lines, page_width=width)
        col_order = sorted(range(len(centers)), key=lambda i: centers[i])

        cols: dict[int, list[LineUnit]] = {i: [] for i in range(len(centers))}
        if not centers:
            cols = {0: list(page_lines)}
            col_order = [0]

        for l in page_lines:
            cidx = assign_column(l.x0, l.x1, centers) if centers else 0
            cols.setdefault(cidx, []).append(l)
            line_rows.append(
                {
                    "file_name": pdf_path.name,
                    "page_number": page_number,
                    "column_index": cidx,
                    "x0": f"{l.x0:.3f}",
                    "y0": f"{l.y0:.3f}",
                    "x1": f"{l.x1:.3f}",
                    "y1": f"{l.y1:.3f}",
                    "text": l.text,
                }
            )

        column_texts: list[str] = []
        for cidx in col_order:
            col_lines = sorted(cols.get(cidx, []), key=lambda u: (u.y0, u.x0))
            col_text = join_lines_into_paragraphs(col_lines)
            if col_text:
                column_texts.append(col_text)

        page_text = clean_text("\n\n".join(column_texts))
        page_rows.append(
            {
                "file_name": pdf_path.name,
                "page_number": page_number,
                "column_count": len(centers) if centers else 1,
                "line_count": len(page_lines),
                "page_text": page_text,
            }
        )
        page_texts.append(page_text)

    year, month, issue_date = parse_filename_date(pdf_path.name)
    issue_row = {
        "file_name": pdf_path.name,
        "canonical_year": year,
        "canonical_month": month,
        "canonical_issue_date": issue_date,
        "page_count": len(page_widths),
        "issue_text": clean_text("\n\n".join(page_texts)),
    }

    return line_rows, page_rows, issue_row


def write_csv(path: pathlib.Path, rows: list[dict]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    if not rows:
        return
    with path.open("w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)


def main() -> None:
    parser = argparse.ArgumentParser(description="Parse multi-column PDFs into ordered text")
    parser.add_argument("--raw-dir", default="raw_data", help="Directory containing issue PDFs")
    parser.add_argument("--line-out", default="outputs/line_text.csv", help="Output line-level CSV")
    parser.add_argument("--page-out", default="outputs/page_text.csv", help="Output page-level CSV")
    parser.add_argument("--issue-out", default="outputs/issue_text.csv", help="Output issue-level CSV")
    parser.add_argument("--limit", type=int, default=0, help="Optional cap for testing")
    args = parser.parse_args()

    pdfs = sorted(pathlib.Path(args.raw_dir).glob("*.pdf"))
    if args.limit and args.limit > 0:
        pdfs = pdfs[: args.limit]

    all_lines: list[dict] = []
    all_pages: list[dict] = []
    all_issues: list[dict] = []

    for pdf in pdfs:
        line_rows, page_rows, issue_row = build_rows_for_pdf(pdf)
        all_lines.extend(line_rows)
        all_pages.extend(page_rows)
        all_issues.append(issue_row)

    write_csv(pathlib.Path(args.line_out), all_lines)
    write_csv(pathlib.Path(args.page_out), all_pages)
    write_csv(pathlib.Path(args.issue_out), all_issues)

    print(f"Processed {len(pdfs)} PDFs")
    print(f"Wrote: {args.line_out} ({len(all_lines)} rows)")
    print(f"Wrote: {args.page_out} ({len(all_pages)} rows)")
    print(f"Wrote: {args.issue_out} ({len(all_issues)} rows)")


if __name__ == "__main__":
    main()
