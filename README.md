# Scoring Economic Gains: The Effect of Winning a FIFA World Cup

**Master's Thesis — LMU Munich, February 2026**

This thesis replicates and extends the analysis of Mello (2024), who examines whether winning the FIFA World Cup boosts GDP growth. Using quarterly data from OECD countries and implementing both event-study and synthetic difference-in-differences (SDiD) methodologies, this study confirms that winning the World Cup increases year-over-year GDP growth by approximately 0.5 percentage points in the two subsequent quarters, driven primarily by enhanced export growth. Beyond replication, this thesis extends the analysis to finalists, semi-finalists, and underperformers — top-10 Elo-rated countries eliminated in the group stage. The finalist and semi-finalist extensions show positive but insignificant effects that attenuate as the treatment group broadens, confirming the premium is specific to winning. The underperformer analysis finds no evidence that early elimination depresses GDP; most SDiD estimates are positive, with the notable exception of exports, where the ATT turns negative which is consistent with the export channel operating in reverse. The findings reinforce the idea that international visibility and trade are the primary mechanism for economic growth after a victory, and suggest that investigating more detailed economic components (e.g. tourism or employment) as well as the effect for unexpected overperformers would be a valuable direction for future research.

All data collection was done programmatically through API calls (OECD and World Cup results) and can be fully replicated by running the scripts in `Data/scripts/`.

---

## Repository Structure

```
├── thesis/
├── Data/
├── mello_paper_replication/
├── finalist_analysis/
├── semi_finalist_analysis/
├── underperformer_analysis/
├── argentina_2022/
├── archive/
└── literature/
```

`Data/` contains all raw and processed datasets as well as the scripts that pull data from the OECD API and assemble the estimation panels. `mello_paper_replication/` holds the core replication of Mello (2024) — event study regressions and SDiD estimation for GDP and its components. The three extension folders (`finalist_analysis/`, `semi_finalist_analysis/`, `underperformer_analysis/`) each follow the same layout and apply the same methods to their respective treatment groups. `argentina_2022/` contains the preliminary case study. `thesis/` is the full LaTeX source. Each analysis folder has subfolders for R scripts, plots, and results.

## Replication

1. Clone the repository
2. Run the scripts in `Data/scripts/` to pull data from the OECD API and assemble the panel datasets
3. Run the R scripts in each analysis folder to produce estimation results and figures
4. Compile `thesis/thesis.tex` with LaTeX (pdflatex + bibtex)

R packages used: `fixest`, `synthdid`, `lfe`, `ggplot2`, `dplyr`, `readr`, `tidyr`, `patchwork`.
