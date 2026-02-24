# Scoring Economic Gains: The Effect of Winning a FIFA World Cup

**Master's Thesis — LMU Munich, February 2026**

This thesis investigates whether winning the FIFA World Cup has a measurable effect on the winner's GDP growth. It replicates and extends Mello (2024, *Oxford Bulletin of Economics and Statistics*), who finds a short-lived positive effect on GDP in the quarters following a World Cup victory. The analysis uses event study regressions and synthetic difference-in-differences (SDiD) on quarterly OECD national accounts data (1962–2021).

Beyond the GDP replication, the thesis extends the analysis in three directions:
- **GDP components** — private consumption, government consumption, capital formation, exports, and imports
- **Tournament performance gradient** — separate analyses for finalists, semi-finalists, and Elo-based underperformers
- **Argentina 2022** — a preliminary pre/post case study of the most recent World Cup winner

All data collection was done programmatically through API calls (OECD and World Cup results) and can be fully replicated by running the scripts in `Data/scripts/`.

---

## Repository Structure

```
├── thesis/                          # LaTeX source for the full thesis
│   ├── thesis.tex                   #   main document
│   ├── chapters/                    #   chapter .tex files
│   ├── tables/                      #   generated LaTeX tables
│   ├── figures/                     #   figures referenced in the text
│   └── bibliography.bib             #   references
│
├── Data/
│   ├── scripts/                     # data collection & processing scripts
│   │   ├── get_data_OECD_population.R
│   │   ├── get_OECD_6_features_test.R
│   │   ├── get_describe_WC_winner_data.R
│   │   ├── get_wc_underperformance_elo.R
│   │   ├── join_and_calculate_metrics_gdp_pop_wc.R
│   │   └── create_paper_replication_csv.R
│   ├── oecd_source/                 # raw OECD quarterly national accounts
│   ├── oecd_processed/              # cleaned & merged panel datasets
│   ├── oecd_metadata/               # feature dictionaries, coverage info
│   ├── world_cup/                   # World Cup results & Elo ratings
│   └── mello_paper_replication/     # estimation-ready samples
│
├── mello_paper_replication/         # core replication of Mello (2024)
│   ├── r_scripts/                   #   event study & SDiD estimation
│   ├── event_study_plots/           #   event study regression figures
│   ├── descriptive_plots/           #   winner/host descriptive plots
│   ├── sdid_plots/                  #   SDiD figures
│   ├── sdid_results/                #   SDiD ATT estimates
│   ├── results/                     #   coefficient tables, comparison CSVs
│   └── notebooks/                   #   exploratory Python notebooks
│
├── finalist_analysis/               # extension: World Cup finalists
│   ├── r_scripts/
│   ├── plots/
│   └── results/
│
├── semi_finalist_analysis/          # extension: semi-finalists
│   ├── r_scripts/
│   ├── plots/
│   └── results/
│
├── underperformer_analysis/         # extension: Elo-based underperformers
│   ├── r_scripts/
│   ├── plots/
│   └── results/
│
├── argentina_2022/                  # Argentina 2022 case study
│   ├── notebooks/
│   ├── plots/
│   └── results/
│
├── archive/                         # earlier drafts and data experiments
└── literature/                      # reference material
```

## Replication

1. Clone the repository
2. Run the scripts in `Data/scripts/` to pull data from the OECD API and assemble the panel datasets
3. Run the R scripts in each analysis folder to produce estimation results and figures
4. Compile `thesis/thesis.tex` with LaTeX (pdflatex + bibtex)

R packages used: `fixest`, `synthdid`, `lfe`, `ggplot2`, `dplyr`, `readr`, `tidyr`, `patchwork`.
