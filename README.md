# Crowdsourced versus LLM Forecasting: Evidence for the Accuracy–Correlation Effect

**Jeddi Y., Segovia-Martin J., & Servan-Schreiber E. (2026)**

---

## About

This repository contains the data and code for our study on the **accuracy–correlation effect (ACE)** in LLM forecasting: as LLMs become more accurate, they also become more correlated with human predictions. Crucially, more accurate models do not just track the truth better; the mistakes they still make become increasingly similar to the mistakes made by human forecasters.

We analyze 76 model x prompt forecast sets from 16 LLMs on 580 resolved questions from the [ForecastBench](https://forecastbench.org/) 21 July 2024 release, benchmarked against superforecasters and general public forecasters.

## Citation

> Jeddi Y., Segovia-Martin J., Servan-Schreiber E. (2026). Crowdsourced versus large language models forecasting: evidence for the accuracy–correlation effect. *Philosophical Transactions of the Royal Society B: Biological Sciences*, 381(1948): 20240456. https://doi.org/10.1098/rstb.2024.0456
 
## Repository Structure

```
ForecastBench Forecasts/   # Raw LLM and human forecast data from ForecastBench 21 July 2024 release (full raw data available at https://osf.io/vya6d/)
FittedObjects/             # Saved model objects from mixed-effects analyses
ME Diagnostics/            # Mixed-effects model diagnostic outputs
Typicality/                # LLM-rated typicality validation (supplementary §ii)
Notebooks/                 # Data cleaning and preparation of raw ForecastBench data
ACE_SCRIPT.R               # Main R script reproducing all analyses and figures
ace_data_me.csv            # Processed data for mixed-effects models
all_h_m.csv                # All human and model forecasts
```
## License
 
Code released under the [MIT License](LICENSE). Data from ForecastBench is used under its original license.
