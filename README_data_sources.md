## Data sources

`analysis_data.rds` contains the processed weekly, state-level dataset used in the analysis
(50 states × 70 MMWR weeks, February 7, 2021 to June 5, 2022). The table below links the
dataset columns to the data sources cited in the manuscript. Variable definitions and
preprocessing methods are described in Table 1 and the Methods. Column names are retained
as used in the analysis files.

| Column in `analysis_data.rds` | Variable (manuscript Table 1) | Source | Source data and documentation |
|---|---|---|---|
| `Wearing.Masks` | Wearing masks | COVID-19 Trends and Impact Survey (CTIS), Delphi / Facebook | [CTIS data and COVIDcast documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/fb-survey.html) |
| `Symtoms` | Symptoms | CTIS | [CTIS data and COVIDcast documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/fb-survey.html) |
| `Symtoms.in.community` | Symptoms in community | CTIS | [CTIS data and COVIDcast documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/fb-survey.html) |
| `Doctor.Visits` | Outpatient visits | Delphi Doctor Visits | [Doctor Visits data and documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/doctor-visits.html) |
| `Test.Positivity` | Test positivity | CDC COVID-19 Electronic Laboratory Reporting (CELR), PCR testing time series | [COVID-19 Diagnostic Laboratory Testing (PCR Testing) Time Series](https://healthdata.gov/dataset/COVID-19-Diagnostic-Laboratory-Testing-PCR-Testing/j8mb-icvb/about_data), dataset `j8mb-icvb` |
| `Cases` | Cases | Johns Hopkins University CSSE, accessed through COVIDcast | [JHU cases and deaths data and documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html) |
| `Deaths` | Deaths | Johns Hopkins University CSSE, accessed through COVIDcast | [JHU cases and deaths data and documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html) |
| `Distributed` | Vaccinations | CDC COVID-19 Vaccinations in the United States, Jurisdiction | [CDC vaccination dataset](https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-Jurisdiction/unsk-b7fc), dataset `unsk-b7fc` |
| `index` | Policy index | Oxford COVID-19 Government Response Tracker (OxCGRT), Stringency Index | [Historical OxCGRT data repository](https://github.com/OxCGRT/covid-policy-tracker). Reference: [Hale et al. (2021)](https://doi.org/10.1038/s41562-021-01079-8) |
| `climate` | Climate-driven relative R0 | NOAA/NCEI Integrated Surface Database (ISD), accessed using `stationaRy` | [NOAA ISD data and access links](https://www.ncei.noaa.gov/products/land-based-station/integrated-surface-database). Software: [stationaRy](https://cran.r-project.org/package=stationaRy). Climate model reference: [Yang and Shaman (2021)](https://doi.org/10.1038/s41467-021-25913-9) |
| `Hospital.Admissions` | Hospital admissions (forecast target and predictor) | HHS Protect, accessed through COVIDcast | [HHS hospital admissions data and documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/hhs.html#department-of-health--human-services-hospitalizations) |
| `ED.visits` | ED visits | CDC National Syndromic Surveillance Program (NSSP) | [CDC COVID-19 surveillance page cited in the manuscript](https://www.cdc.gov/covid/php/surveillance/index.html) |

Additional COVIDcast resources are available from the [Delphi COVIDcast website](https://delphi.cmu.edu/covidcast/)
and the [Delphi Epidata documentation](https://cmu-delphi.github.io/delphi-epidata/api/covidcast_signals.html).

The links above identify the source datasets, providers, and documentation. Some source
feeds have been retired or reorganized since the study period. The processed dataset used
for the reported analyses is provided in `analysis_data.rds`.
