# nclr_latino

Minimal pipeline for paragraph-level comparison of panethnic vs ethnic contexts, ending in two core figures.

## 1) Extract Metadata

```bash
python3 src/extract_issue_metadata.py
```

Outputs:
- `outputs/metadata/issue_metadata.csv`
- `outputs/metadata/date_conflicts.csv`

## 2) Parse Multi-Column PDFs

```bash
python3 src/parse_multicolumn_pdfs.py
```

Outputs:
- `outputs/parsed/line_text.csv`
- `outputs/parsed/page_text.csv`
- `outputs/parsed/issue_text.csv`

## 3) Build Paragraph Panel

```bash
python3 src/build_paragraph_panel.py
```

Output:
- `outputs/analysis/paragraph_panel.csv`

## 4) Quality Filter

```bash
python3 src/filter_paragraph_panel.py
```

Outputs:
- `outputs/analysis/paragraph_panel_qc.csv`
- `outputs/analysis/paragraph_panel_filtered.csv`

## 5) Compare Themes in Panethnic-Appeared vs Ethnic-Appeared Paragraphs

```bash
python3 src/analyze_paragraph_ethnic_panethnic_themes.py
```

Outputs:
- `outputs/analysis/paragraph_theme_prevalence_by_group.csv`
- `outputs/analysis/paragraph_theme_contrast_panethnic_minus_ethnic.csv`
- `outputs/analysis/paragraph_theme_monthly_by_group.csv`

## 6) Corpus Summary Table (Reader-Facing)

Create corpus-level and issue-level descriptive summaries (e.g., average paragraphs per issue):

```bash
python3 src/summarize_corpus.py
```

Outputs:
- `outputs/analysis/corpus_summary_table.csv`
- `outputs/analysis/corpus_summary_table.md`
- `outputs/analysis/corpus_issue_level_summary.csv`

## 7) Figure A: Group Share Gap + Raw Shares (Paragraph Unit)

```bash
Rscript src/plot_paragraph_group_share_gap_panel.R
```

Output:
- `outputs/analysis/fig_span_group_share_over_time_gap_panel.png`

Notes:
- Top panel: panethnic minus ethnic gap (percentage points).
- Bottom panel: raw percent shares.
- Both panels use paragraph unit.

## 8) Figure B: Theme Trend Facets (Paragraph Unit)

```bash
Rscript src/plot_paragraph_ethnic_panethnic_themes.R --normalize=max100
```

Output:
- `outputs/analysis/fig_paragraph_theme_trends_facet.png`

Notes:
- Grayscale panethnic vs ethnic lines.
- Gray band indicates the gap between lines.
- `--normalize=max100` normalizes each group line within each facet for shape comparison.
