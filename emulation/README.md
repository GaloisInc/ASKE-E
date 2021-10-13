# Summary

This folder contains code artifacts for work performed on model emulation and model skill assessment, during the summer of 2021.  Descriptions of the Jupyter notebooks can be found in specific sections of the [SuperMaaS Interim ARO Report](https://drive.google.com/file/d/1JVX3J-7ga_1df8wYFMf45tP0Grnw9Kbm/view), as specified in the table below.

| Jupyter Notebook           | Report Section                              |
| -------------------------- | ------------------------------------------- |
| ModelEmulation.ipynb       | 11.6: Application to Epidemiological Models |
| BaselineForecasts.ipynb    | 13.1: Skill Assessment Baselines            |
| ModelSkillAssessment.ipynb | 13.2: Skill Assessment Metrics              |
| ModelFitting.ipynb         | 13.4: Forecasting via the SIR Model         |

# Notes

* ModelSkillAssessment.ipynb uses an implementation of the Weighted Interval Score (WIS) metric, provided by Yerim Lee [here](https://github.com/GaloisInc/ASKE-E/blob/main/docs/metrics/weighted_interval_score.py), and copied, with minor modifications, to `weighted_interval_score.py`.  The notebook implements some of the methods described in ["E. Cramer et al, Evaluation of individual and ensemble probabilistic forecasts of COVID-19 mortality in the US, medRxiv 2021.02.03.21250974, 2021"](https://www.medrxiv.org/content/10.1101/2021.02.03.21250974v1.full#F2).
* ModelFitting.ipynb is based on code that was originally written by Eric Davis, and subsequently modified.  It makes use of the file `ctp_state_data.tsv`, which was provided by Geometric Data Analytics, Inc. (GDA).  It can also optionally display forecasts made by the `COVIDhub-ensemble` model for Georgia; these were extracted from the [`covid19-forecast-hub`](https://github.com/reichlab/covid19-forecast-hub) repository, and saved in `COVIDhub-ensemble_13_1.csv`, `COVIDhub-ensemble_13_2.csv`, `COVIDhub-ensemble_13_3.csv` and `COVIDhub-ensemble_13_4.csv`.
* BaselineForecasts.ipynb and ModelSkillAssessment.ipynb both assume that the `covid19-forecast-hub` repository exists at the same level as the ASKE-E repository.  The Jupyter notebooks were executed with data from commit `6b06c16c0e42bb6253b6739b42fc6adb50b58ab4 ` of the `covid19-forecast-hub` repository, created on July 14, 2021.  Since then, more data has been added to the repository.
* ModelEmulation.ipynb implements a standalone version of the global emulation algorithm that is also available as part of SuperMaaS, on the [`model-emulation`](https://gitlab-ext.galois.com/world-modelers/galois-internal/supermaas/-/tree/model-emulation/emulation) branch.
* ModelEmulation.ipynb and ModelFitting.ipynb share some duplicate code (such as the implementation of the SIR model, provided by Eric Davis).
* BaselineForecasts.ipynb and ModelSkillAssessment.ipynb share some duplicate code (such implementations of the Constant and Linear models, and code for converting daily cases data to weekly cases data).
* In addition to the quantitative model assessment offered by the above notebooks, we also performed a qualitive assessment of some models; results can be found [here](https://docs.google.com/spreadsheets/d/13TSKmgIWCwyXDzwMVRayQymcpjoexPByFRTZRjEpWno/edit#gid=0), and are summarized in the section of the SuperMaaS Interim ARO Report titled "Reproducibility".
* For questions, please contact Alex Grushin (agrushin@galois.com).





