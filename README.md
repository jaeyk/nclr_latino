# nclr_latino

Minimal pipeline for paragraph-level comparison of panethnic vs ethnic contexts, ending in two core figures.

## 1) Extract Metadata

```bash
python3 src/extract_issue_metadata.py
```

Outputs:
- `outputs/issue_metadata.csv`
- `outputs/date_conflicts.csv`

## 2) Parse Multi-Column PDFs

```bash
python3 src/parse_multicolumn_pdfs.py
```

Outputs:
- `outputs/line_text.csv`
- `outputs/page_text.csv`
- `outputs/issue_text.csv`

## 3) Build Paragraph Panel

```bash
python3 src/build_paragraph_panel.py
```

Output:
- `outputs/paragraph_panel.csv`

## 4) Quality Filter

```bash
python3 src/filter_paragraph_panel.py
```

Outputs:
- `outputs/paragraph_panel_qc.csv`
- `outputs/paragraph_panel_filtered.csv`

## 5) Compare Themes in Panethnic-Appeared vs Ethnic-Appeared Paragraphs

```bash
python3 src/analyze_paragraph_ethnic_panethnic_themes.py
```

Outputs:
- `outputs/paragraph_theme_prevalence_by_group.csv`
- `outputs/paragraph_theme_contrast_panethnic_minus_ethnic.csv`
- `outputs/paragraph_theme_monthly_by_group.csv`

## 6) Corpus Summary Table (Reader-Facing)

Create corpus-level and issue-level descriptive summaries (e.g., average paragraphs per issue):

```bash
python3 src/summarize_corpus.py
```

Outputs:
- `outputs/corpus_summary_table.csv`
- `outputs/corpus_summary_table.md`
- `outputs/corpus_issue_level_summary.csv`

## 7) Figure A: Group Share Gap + Raw Shares (Paragraph Unit)

```bash
Rscript src/plot_paragraph_group_share_gap_panel.R
```

Output:
- `outputs/fig_span_group_share_over_time_gap_panel.png`

Notes:
- Top panel: panethnic minus ethnic mention-rate gap (percentage points).
- Bottom panel: monthly mention rates.
- Denominator for both rates is all paragraphs in each month.
- Panethnic and ethnic rates do not need to sum to 100% because a paragraph can match both labels.

## 8) Figure B: Theme Trend Facets (Paragraph Unit)

```bash
Rscript src/plot_paragraph_ethnic_panethnic_themes.R --normalize=max100
```

Output:
- `outputs/fig_paragraph_theme_trends_facet.png`

Notes:
- Grayscale panethnic vs ethnic lines.
- Gray band indicates the gap between lines.
- `--normalize=max100` normalizes each group line within each facet for shape comparison.

## Regex Definitions For Identity Labels

Defined in `src/build_paragraph_panel.py`:

- Panethnic regex:
  - `\b(hispanic(?:s)?|latino(?:s|a|as)?|spanish[- ]speaking|spanish[- ]surname)\b`
- Ethnic regex:
  - `\b(chicano(?:s|a|as)?|mexican[- ]american(?:s)?|puerto rican(?:s)?|cuban[- ]american(?:s)?|dominican(?:s)?|salvadoran(?:s)?|guatemalan(?:s)?|nicaraguan(?:s)?)\b`

Label behavior:
- `has_panethnic_label=1` when Panethnic regex matches.
- `has_ethnic_label=1` when Ethnic regex matches.
- Labels are non-exclusive (a paragraph may match both).
