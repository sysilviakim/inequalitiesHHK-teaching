# inequalitiesHHK-teaching

This repository is a teaching version of the replication archive for:

Horn, Alexander, Martin Haselmayer, and K. Jonathan Klueser.
"Why Inequalities Persist: Parties' (Non)Responses to Economic Inequality,
1970-2020." *American Political Science Review* 120, no. 1 (2026): 346-64.
[https://doi.org/10.1017/S0003055425100907](https://doi.org/10.1017/S0003055425100907)

The goal of this repo is to give students a much leaner, R-based workflow than
the original archive. The current version focuses on reproducing the paper's
main mixed-effects models, a recreation of Figure 2, and a simple
paper-versus-reproduction comparison.

## Folder structure

```text
inequalitiesHHK-teaching/
|-- R/
|-- data/
|   |-- raw/
|   |-- derived/
|   `-- results/
`-- README.md
```

Only the `R/` scripts are currently tracked in the GitHub repo. The `data/`
folder is expected locally.

## Required input data

To run the workflow, place these two files in `data/raw/`:

- `whyinequalitiespersist_replication.dta`
- `whyinequalitiespersist_validation_replication.dta`

These are the final analysis-ready files from the original replication archive.
The teaching workflow does not require the full raw-data construction stack.

## Scripts

The workflow is organized into four steps plus a runner:

1. `R/01_prepare_data.R`
   Imports the Stata replication files and saves local R objects.
2. `R/02_estimate_models.R`
   Estimates the ten main mixed-effects models from the article.
3. `R/03_visualize_results.R`
   Recreates the interaction-style figure for the main models.
4. `R/04_reproducibility_check.R`
   Compares reproduced estimates with values discussed in the article text.
5. `R/run_all.R`
   Runs the full pipeline from start to finish.

## How to run

From R, run:

```r
source("R/run_all.R")
```

The scripts use the `here` package for project-root paths and will install
missing R packages if needed.

## Outputs

Running the full workflow creates:

- `data/derived/analysis_data.rds`
- `data/derived/main_models.rds`
- `data/results/analysis_sample_summary.csv`
- `data/results/table_1_main_models.csv`
- `data/results/figure_2_reproduction.png`
- `data/results/reproducibility_check.csv`
- `data/results/reproducibility_check.md`

## Reproducibility note

The R translation closely matches the paper's reported main coefficients.
For example, the reproduced estimates recover the key patterns for:

- Bottom 50 (L1): positive main effect, negative right-party interaction
- 90-50 (L1): negative main effect, positive right-party interaction
- Bottom 50 (D): negative main effect, positive right-party interaction
- Top 1 (D): no meaningful response

One prediction-level comparison in the prose summary is somewhat less exact than
the printed article values, but the qualitative pattern remains the same.

## Teaching use

This repo is meant for advanced undergraduate or graduate methods teaching. It
is intentionally narrower than the original replication archive:

- no Stata dependency
- no `renv`
- no raw-data assembly pipeline
- no large auxiliary source files

That tradeoff keeps the archive easier to teach, inspect, and rerun.
