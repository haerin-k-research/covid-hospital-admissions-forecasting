# COVID-19 Hospital Admissions Forecasting
Code for "The Value of Hospital Admission Data for Forecasting Healthcare Burden During the COVID-19 Pandemic"
## Files

- `analysis_data.rds` – processed analysis dataset
- `functions_modeling.R` – modeling functions
- `modeling_github.Rmd` – model fitting and evaluation
- `bootstrap_github.Rmd` – probabilistic forecasting and uncertainty analysis
- `figures_github.Rmd` – manuscript figures
- `tables_github.Rmd` – manuscript tables

## Run order

Run the notebooks from the repository root in the order below, using the same R session.

1. `modeling_github.Rmd`
2. `bootstrap_github.Rmd`
3. `figures_github.Rmd`
4. `tables_github.Rmd`

Model fitting, bootstrap generation, and rolling calibration are retained in chunks marked `eval = FALSE`; run these explicitly (or set `eval = TRUE`) to create the files under `results/`, which the loading chunks otherwise reuse.

Data sources and documentation are available in [README_data_sources.md](README_data_sources.md).
