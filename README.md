# Differential Changes in Non-durable Spending across U.S. States after the Russia-Ukraine War

**An Empirical Study Using Continuous-exposure Difference-in-Differences**

In this project, I examine whether changes in non-durable consumer spending after the outbreak of the Russia-Ukraine war in 2022 differed across U.S. states according to their pre-war petroleum consumption exposure. I combine spending, petroleum consumption, and energy price data and use a continuous-exposure difference-in-differences (DID) model to investigate these differences. I assess the findings through trend tests and alternative model specifications.

This repository presents my work in data cleaning, state-week panel construction, time-series forecasting, econometric analysis, and visualization. The main analysis corresponds to `final work(10).R`. The inventories below document the current script and accompanying export routines; the listed files are generated after the analysis and export code run successfully.

## 1. Research Approach

My central question is: **Did states with greater pre-war petroleum consumption exposure experience a larger relative weakening of non-durable spending after the war began?**

I first fit ARIMA models to pre-war WTI prices and compare their forecasting performance with a random-walk benchmark using holdout and rolling-origin validation. I then extend the pre-war model into the post-war period and examine actual prices relative to the forecast path. This provides supporting evidence for the choice of observation window.

For the panel analysis, I match spending and exposure data using state FIPS codes. I conduct a pre-war Stute nonparametric test and a supplementary Wald test before estimating the continuous DID models. Additional specifications use 2019 exposure, include regional gasoline prices, and compare high- and low-exposure states using binary DID.

## 2. Data Sources

| Data | Source | Role |
|---|---|---|
| State-level non-durable spending index | Opportunity Insights Economic Tracker / Affinity | State-week outcome variable |
| Petroleum product consumption per capita by state | U.S. Energy Information Administration (EIA) | Continuous exposure and high/low exposure classification |
| Weekly WTI crude oil prices | FRED; input variable `DCOILWTICO` | ARIMA estimation, validation, and forecasting |
| Weekly regional retail gasoline prices | EIA | Supplementary control matched to states by PADD region |

I use the following input files:

| Filename | Import details |
|---|---|
| `2020.1.3-2024.1.5 weekly.csv` | Weekly WTI prices |
| `use_pet_capita.xlsx` | Sheet `Barrels per capita`; skip the first two rows |
| `Affinity - State - Daily.csv` | State-level spending input data |
| `PET_PRI_GND_A_EPM0_PTE_DPGAL_W.xls` | Sheet `Data 2`; skip the first three rows |

Baseline exposure is the average of each state's petroleum product consumption per capita in 2019, 2020, and 2021, standardized using the cross-state mean and standard deviation. Exposure measured in 2019 alone provides an alternative specification.

The spending index measures changes relative to a reference period. It is neither dollar spending nor week-on-week growth. Regressions retain the original index scale; selected tables and figures multiply it by 100 to report percentages or percentage-point differences. The reference period and precise definitions follow the data dictionary for the input version used.

## 3. Sample and Model

| Component | Specification |
|---|---|
| Pre-war window | 27 weeks ending from 22 August 2021 to 20 February 2022 |
| Post-war window | 27 weeks ending from 6 March 2022 to 4 September 2022 |
| Transition week | Exclude the week ending 27 February 2022 |
| Panel unit | State-week; available spending observations are averaged within weeks ending on Sunday |
| Fixed effects | State and week fixed effects |
| Standard errors | Clustered at the state level |
| Geographic coverage | Matched using abbreviations and FIPS codes for the 50 states; the effective sample size appears in the sample summary |

My baseline model is:

$$
Y_{it}=\alpha_i+\lambda_t+\beta(E_i^*\times Post_t)+\varepsilon_{it}.
$$

Here, Y is the non-durable spending index, E* is standardized pre-war exposure, and Post identifies the post-war period. The coefficient beta measures the difference in post-minus-pre spending changes associated with a one-standard-deviation increase in baseline exposure. It does not directly measure the war's overall average effect on U.S. consumption.

For the 2019 exposure specification, I continue to use the standard deviation of baseline 2019-2021 exposure as the scaling unit. The binary DID specification compares the 18 highest-exposure states with the 18 lowest-exposure states; its coefficient has a different interpretation from the continuous DID coefficient.

## 4. File Organization

| Path | Contents |
|---|---|
| `README.md` | Project overview and reproduction instructions |
| `final work(10).R` | Current main analysis script |
| `data/raw/` | Archive location for original input files |
| `data/interim/` | Rebuildable cleaning and matching outputs |
| `data/derived/` | Data used directly for regressions, tests, and supporting analysis |
| `outputs/tables/` | Descriptive statistics, model comparisons, regression results, and tests |
| `outputs/figures/` | PNG and PDF figures |
| `report/` | Research report presenting my empirical results and their interpretation |

These paths describe the project's intended organization. The original script contains a few export commands; the complete inventory requires the accompanying batch export routines.

## 5. Interim Data

| Filename | R object | Contents |
|---|---|---|
| `affinity_clean.csv` | `affinity` | Spending data with constructed dates and converted spending values |
| `state_lookup.csv` | `state_lookup` | State abbreviation-to-FIPS mapping |
| `oil_exposure_cont.csv` | `oil_exposure_cont` | Average 2019-2021 exposure and its standardized measure |
| `gas_padd.csv` | `gas_padd` | Cleaned regional gasoline prices restricted to the study window |
| `state_padd.csv` | `state_padd` | State-to-PADD mapping |
| `gas_state_weekly.csv` | `gas_state_weekly` | Regional gasoline prices matched to state-weeks |
| `oil_2019.csv` | `oil_2019` | Petroleum consumption per capita in 2019 and state identifiers |

I export these objects after the relevant transformations are complete. For example, `gas_padd` contains the version after date cleaning, conversion to long format, and study-window filtering.

## 6. Derived Data

| Filename | R object | Purpose |
|---|---|---|
| `did_cont_27.csv` | `did_cont_27` | Main continuous DID panel with 27 pre-war and 27 post-war weeks |
| `did_gas.csv` | `did_gas` | Continuous DID panel matched with regional gasoline prices |
| `did_gas_2019.csv` | `did_gas_2019` | Panel containing 2019 exposure and its consistently scaled measure |
| `stute_data.csv` | `stute_data` | Pre-war Stute data with 26 tested weeks after excluding the reference week |
| `stute_post_data.csv` | `stute_post_data` | Post-war Stute data for 27 weeks relative to the same pre-war reference week |
| `pre_wald.csv` | `pre_wald` | Data for pre-war exposure interactions and the Wald test |
| `did_weekly_27.csv` | `did_weekly_27` | Binary DID data for high- and low-exposure states |
| `state_change.csv` | `state_change` | State-level post-minus-pre average spending changes and exposure |
| `weekly_forecast.csv` | `weekly_forecast` | Actual prices, forecasts, 95% prediction intervals, and forecast gaps |

`weekly_forecast` is an output dataset from the supporting oil-price analysis. The other files primarily support panel estimation and testing.

## 7. Tables

Tables are saved in `outputs/tables/`. CSV files retain numerical results for further processing, while TXT files preserve the printed format of model comparisons and test objects.

| Filename | Source object | Contents |
|---|---|---|
| `table01_sample_summary.csv` | `sample_summary` | States, weeks, observations, and panel completeness summary |
| `table02_descriptive_statistics.csv` | `descriptive_table` | Sample size, mean, standard deviation, minimum, median, and maximum |
| `table03_state_exposure.csv` | `exposure_table` | Original and standardized state exposure |
| `table04_panel_coverage.csv` | `state_coverage` | Pre-war, post-war, and total weeks by state |
| `table05_arima_model_comparison.csv` | `model_summary` | Candidate model orders, AIC, AICc, and BIC |
| `table06_arima_residual_diagnostics.csv` | `residual_summary` | Ljung-Box tests and diagnostic summaries |
| `table07_rolling_validation_summary.csv` | `cv_summary` | Average rolling-validation RMSE and MAE |
| `table08_wald_rank_check.csv` | `rank_result` | States, restrictions, covariance rank, and full-rank status |
| `table09_did_coefficients.csv` | Five DID models | Coefficients, clustered standard errors, statistics, p-values, 95% confidence intervals, and observations |
| `table10_did_comparison.csv` / `.txt` | `fixest::etable()` | Side-by-side comparison of the five models |
| `table11_stute_prewar.txt` | `stute_pre` | Pre-war Stute test results |
| `table11_stute_prewar_joint.txt` | `stute_pre$joint` | Pre-war joint test |
| `table12_stute_postwar.txt` | `stute_post` | Post-war Stute test results |
| `table12_stute_postwar_joint.txt` | `stute_post$joint` | Post-war joint test |
| `table13_wald_prewar.txt` | `wald_pre_result` | Pre-war joint Wald test when the covariance matrix has full rank |

The five DID models are:

| Model object | Specification |
|---|---|
| `model_baseline` | Continuous DID using 2019-2021 exposure |
| `model_2019` | Continuous DID using 2019 exposure |
| `model_gas` | Baseline exposure with regional gasoline prices |
| `model_2019_gas` | 2019 exposure with regional gasoline prices |
| `model_did_27` | Binary DID comparing high- and low-exposure states |

Regression tables retain the original outcome scale. Multiplying a coefficient by 100 expresses the corresponding comparison in percentage points. The export routine saves conventional joint Wald results only when the covariance matrix has full rank; otherwise, the rank-check table documents the issue.

## 8. Figures

Figures are saved in `outputs/figures/`. Each figure is exported as a 300 dpi PNG and a vector PDF. Standard dimensions are 10 by 6 inches; the state exposure ranking uses 10 by 11 inches.

| Filename prefix (both `.png` and `.pdf`) | R object | Contents |
|---|---|---|
| `fig01_oil_forecast` | `p_oil_forecast` | Actual WTI prices, forecasts from the pre-war model, and 95% prediction intervals |
| `fig02_exposure_distribution` | `p_exposure_distribution` | Distribution of pre-war exposure and its cross-state mean |
| `fig03_exposure_by_state` | `p_exposure_states` | State exposure ranking |
| `fig04_spending_trend` | `p_spending_trend` | State spending, the equal-weighted mean, war onset, and the transition week |
| `fig05_prewar_changes` | `p_all_states` | State spending changes relative to the last pre-war reference week |
| `fig06_postwar_changes` | `p_post_states` | Post-war state spending changes relative to the same pre-war reference week |
| `fig07_exposure_spending_change` | `p_exposure_change` | Exposure versus post-minus-pre mean spending changes, with a fitted line |

The accompanying export code creates the oil-price chart as `p_oil_forecast` and the scatterplot as `p_exposure_change`. The scatterplot multiplies `spending_change` by 100, so its vertical axis is measured in percentage points.

The pre-war and post-war charts describe variation across states and over time. Statistical conclusions from the Stute tests come from the corresponding test tables. The fitted scatterplot line does not replace formal DID estimation and clustered inference.

## 9. Main Variables

| Variable | Definition |
|---|---|
| `statefips` | State FIPS identifier |
| `State` | State abbreviation; available in selected datasets |
| `week_end` | Sunday ending the corresponding week |
| `spend_nondurables_mean` | State-week mean of available spending index observations, in the original scale |
| `exposure` | Average annual petroleum product consumption per capita in 2019-2021 |
| `exposure_std` | Baseline exposure standardized across states |
| `post` | 0 before the war; 1 in the post-war window |
| `gas_price` | Retail gasoline price in the state's PADD region, USD per gallon; shared by states in the same region |
| `exposure_2019` | Petroleum product consumption per capita in 2019 |
| `exposure_2019_scaled` | Centered 2019 exposure divided by the baseline exposure standard deviation |
| `pre_change` / `post_change` | Spending index differences relative to the last pre-war reference week |
| `high` | 1 for the high-exposure group and 0 for the low-exposure group in binary DID |
| `spending_change` | A state's post-war mean spending index minus its pre-war mean |

## 10. Running the Analysis

### Dependencies

I use R for the analysis. The script requires the following packages:

```r
install.packages(c(
  "forecast", "ggplot2", "lubridate", "readr", "dplyr",
  "fixest", "readxl", "tidyr", "tibble", "Matrix"
))
```

The Stute tests additionally require `StuteTest`. Its exact installation source and version are not yet documented here and remain necessary for complete environment reproduction.

### Execution Order

1. Prepare the four input files listed above and check their sheets and variable names.
2. Update the two `setwd()` calls and input paths in the main script. The current script retains absolute paths from my computer and therefore requires adjustment on another machine.
3. Run the main script from the beginning in a fresh R session to complete oil-price analysis, data processing, trend tests, regressions, and robustness analysis.
4. Run the data export routine at the end of the script to generate CSV files in `data/interim/` and `data/derived/`.
5. Run the tables and figures export routine to generate the result files.

The data export routine uses relative paths. The figure and table export routine uses `project_dir <- getwd()` as its output root. To save all outputs under the same repository, set the working directory to the repository root before exporting and ensure `project_dir` points there. If input files are archived under `data/raw/`, update the script's import paths accordingly.

After execution, output locations and the software environment can be recorded with:

```r
normalizePath("data")
normalizePath("outputs/tables")
normalizePath("outputs/figures")

capture.output(
  sessionInfo(),
  file = "outputs/sessionInfo.txt"
)
```

Export routines depend on objects created earlier; they do not independently re-estimate every model. Errors in preceding steps need to be resolved before exporting to avoid using objects left over from an earlier session.

### Reloading Data

CSV does not fully preserve R types such as dates. Restore date variables after importing:

```r
did_cont_27 <- read.csv("data/derived/did_cont_27.csv")
did_cont_27$week_end <- as.Date(did_cont_27$week_end)
```

RDS can additionally preserve R data types:

```r
saveRDS(did_cont_27, "data/derived/did_cont_27.rds")
did_cont_27 <- readRDS("data/derived/did_cont_27.rds")
```

## 11. Interpretation and Reproducibility Status

My continuous DID interpretation depends on the relevant parallel-trends and other identification assumptions. Failure to reject a pre-war null indicates insufficient evidence against it in the observed sample; it does not establish post-war counterfactual trends. The post-war Stute test examines the relationship between exposure and changes relative to the reference week and does not independently establish the full causal mechanism.

Forecasts from the pre-war oil-price model depend on its specification. Actual-minus-forecast price differences cannot be interpreted directly as the war's entire causal effect. Gasoline prices may also lie on the pathway through which the war affects consumption, so including them can change the economic interpretation of the estimated coefficient.

This README documents the current script and accompanying export plan. It does not establish that every listed output has been generated or that end-to-end reproduction has passed. Numerical results and conclusions follow the executed analysis and research report. Full reproduction on another computer still requires path adjustments, precise data acquisition details, and the installation source and version of `StuteTest`.

