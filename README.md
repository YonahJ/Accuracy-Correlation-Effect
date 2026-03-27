# Crowdsourced versus LLM Forecasting: Evidence for the Accuracy–Correlation Effect

**Jeddi Y., Segovia-Martin J., & Servan-Schreiber E. (2026)**

---

## About

This repository contains the data and code for our study on the **accuracy–correlation effect (ACE)** in LLM forecasting: as LLMs become more accurate, they also become more correlated with human predictions. Crucially, more accurate models do not just track the truth better; the mistakes they still make become increasingly similar to the mistakes made by human forecasters.

We analyze 76 model x prompt forecast sets from 16 LLMs on 580 resolved questions from the [ForecastBench](https://forecastbench.org/) 21 July 2024 release, benchmarked against superforecasters and general public forecasters.
 
## Repository Structure

```
ForecastBench Forecasts/   # Raw LLM and human forecast data from ForecastBench 21 July 2024 release
FittedObjects/             # Saved model objects from mixed-effects analyses
ME Diagnostics/            # Mixed-effects model diagnostic outputs
Typicality/                # LLM-rated typicality validation (supplementary §ii)
Notebooks/                 # Data cleaning and preparation of raw ForecastBench data
ACE_SCRIPT.R               # Main R script reproducing all analyses and figures
ace_data_me.csv            # Processed data for mixed-effects models
all_h_m.csv                # All human and model forecasts
```
