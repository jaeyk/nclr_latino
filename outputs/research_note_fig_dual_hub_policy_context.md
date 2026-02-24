# Research Note: How `fig_dual_hub_policy_context` Was Analyzed and Visualized

## Purpose
This figure compares policy-context language in two sets of newsletter paragraphs:
- paragraphs tagged with a **panethnic** label
- paragraphs tagged with an **ethnic** label

The goal is to show which policy terms are shared, which lean more panethnic, and which lean more ethnic.

## Data Used
- Input file: `outputs/paragraph_panel_filtered.csv`
- Unit of analysis: one paragraph
- A paragraph enters this analysis if it has either:
  - `has_panethnic_label = 1`, or
  - `has_ethnic_label = 1`

Because labels are non-exclusive, one paragraph can contribute to both groups.

Original data scope (from corpus summary outputs):
- time period: 1971-06-01 to 1981-09-01
- number of issues: 57
- total raw paragraphs: 8,366
- total filtered paragraphs: 6,316 (75.50% kept after filtering)
- identity-labeled paragraphs in filtered corpus (panethnic or ethnic): 2,561
- panethnic-labeled paragraphs: 1,509
- ethnic-labeled paragraphs: 1,481
- paragraphs with both labels: 429

## What Filtering Did
Before this figure was built, `src/filter_paragraph_panel.py` quality-filtered paragraphs to reduce OCR debris.

Main drop rules (paragraph is flagged low quality if any apply):
- fewer than 8 tokens
- fewer than 5 word-like tokens
- alphabetic character ratio below 0.55
- punctuation ratio above 0.22
- too many non-vowel alpha-like tokens (ratio above 0.45), which often indicates OCR corruption

Rescue rule:
- even if a paragraph fails quality checks, it is kept when it still shows substantive signal (identity label or policy/theme/domain flags), with minimum safeguards (`n_word_tokens >= 5` and `alpha_ratio >= 0.45`)

Net effect in this corpus:
- 8,366 raw paragraphs -> 6,316 kept
- 2,050 paragraphs dropped

## How Terms Were Built
The analysis script (`src/analyze_dual_hub_policy_context.py`) tokenizes paragraph text, removes common stopwords and identity words, and keeps policy-relevant language.

It then creates policy-context candidate terms as:
- bigrams (2-word terms)
- trigrams (3-word terms)
- 4-grams (4-word terms)

Only candidates containing at least one policy term are retained. Terms are scored using a hybrid method (frequency + PMI signal), then boosted if their panethnic-vs-ethnic rate gap is larger.

Default selection settings:
- minimum evidence: term appears in at least 8 paragraphs in either group (`--min-any-paragraphs 8`)
- keep top terms: up to 90 (`--max-terms 90`)

## How the Network Was Built
Two kinds of edges are written to `outputs/dual_hub_edges.csv`:

1. Hub-term edges
- `panethnic_hub -> term` with weight based on panethnic paragraph rate
- `ethnic_hub -> term` with weight based on ethnic paragraph rate

2. Term-term edges
- two terms are linked if they co-occur in the same paragraph
- edge weight is number of co-occurring paragraphs (`weight_n`)
- only stronger links are kept (at least 3 co-occurrences; capped by `--max-term-edges 220`)

Node-level term statistics are written to `outputs/dual_hub_nodes.csv`.

## Tilt Classification
Each term receives a tilt from its rate gap:
- `panethnic_tilt` if `(pan_rate - eth_rate) > 0.01`
- `ethnic_tilt` if `(pan_rate - eth_rate) < -0.01`
- `shared` otherwise

This tilt drives node color in the final figure.

## Community Analysis (Tabular Output)
A Louvain community detection step is run on the term-term graph to summarize clusters:
- membership file: `outputs/dual_hub_community_membership.csv`
- summary file: `outputs/dual_hub_community_summary.csv`

Important: community IDs are for interpretation tables. The plotted figure is **not** arranged by community ID.

## How the Figure Was Drawn
The plotting script (`src/plot_dual_hub_policy_context.py`) creates `outputs/fig_dual_hub_policy_context.png`.

Main visual choices:
- left large node = panethnic hub (blue)
- right large node = ethnic hub (orange)
- term nodes are placed in three columns:
  - panethnic-tilted terms (left column)
  - shared terms (center)
  - ethnic-tilted terms (right column)
- node size scales with pooled frequency (`pooled_n`)
- thin gray lines show term-term co-occurrence structure
- blue/orange hub links vary in width by group-specific rate

For readability, only a bounded number of terms are plotted (default up to 28), while preserving representation from each tilt group.

## How To Read the Figure
- Bigger term node: appears in more labeled paragraphs overall.
- Bluer/oranger term node: stronger relative tilt toward panethnic or ethnic usage.
- Thicker hub link: higher within-group mention rate for that term.
- More gray links around a term: that term co-occurs with many other policy contexts.

## Reproducible Commands
```bash
python3 src/analyze_dual_hub_policy_context.py
python3 src/plot_dual_hub_policy_context.py
```
