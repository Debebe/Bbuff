### README

Code and data for:

Shaweno D, Mafirakureva N, Lee A, Wong TS, Dodd PJ. Bacillus Calmette-Guérin (BCG) vaccination: a global analysis of cost-effectiveness and optimal safety stock


### Data processing

- The script `01.TBtx_unit.costs.R` calls `00.TB_care_costs.R` that models TB care costs and produces unit costs 
 for non-TB and TBM care. The output is the a data.frame called `tbtx_unit_costs.Rds`. No need to run `00.TB_care_costs.R` separately.
 
- The script `process_data.R` processes data from different sources `(WHO, WUENIC,UN(UN pop & LE),IDCC, GDP`) 
and merges with the above TB treatment unit costs. Generates a data.table with incidence, notification, LE,GDP,vaccine delivery unit costs etc, 
TB treatment and vaccination costs etc- The file it produces is called `gdp_inc_le.rds`

- The `inflation_adjustment.R` reads inflation data, processes and then inflates costs in `gdp_inc_le.rds`.It produces a version
with inflated cost - `gdp_inc_le_cost.rds`


### Modelling
 
 - The`panalysis.R` takes data from `indata`, runs the model for different PSA draws and creates an output for individual PSA samples and stores in a data.table `D`. 
 - The `get_model_outputs.R` script generates relevant outputs related to health and cost reported in the manuscript
 - `summary_statistic.R` creates summary results reported in the manuscript

### Regression 
 
 Regression and its variance decomposition is handled by `regression.R` based on data from `panalysis.R`. If there 
 
### Sensitivity analysis
 
 Sensitivity of cost-effectiveness to changes in post-TB parameters and TBM parameters is handled by `sensitivity_analysis.R` script. 
 
### Meta-analysis
 
The meta-analysis and the forest plots can be executed by `meta_analysis.R` script. It runs 1). meta-analysis on RR of TBM, 2). Sequelae of TBM

### Plots and tables

Tables and plots reported in the manuscript can be reproduced by running `create_tables.R` and `create_plots.R` scripts respectively


## Software dependencies

These analyses used R version 4.5.0 and the following packages:

| Package | Version |
|---------|---------|
| countrycode	| 1.6.1 | 
| cowplot	| 1.2.0 |
| data.table	| 1.18.0 |
| dplyr	| 1.1.4 |
| flextable	| 0.9.9 |
| forcats	| 1.0.1 |
| ggplot2	| 4.0.1 |
| ggrepel	| 0.9.6 |
| ggthemes	| 5.1.0 |
| glue	| 1.8.0 |
| googlesheets4	| 1.1.1 |
| here	| 1.0.1 |
| httr	| 1.4.7 |
| kableExtra	| 1.4.0 |
| knitr	| 1.50 |
| magick	| 2.9.0 |
| meta	| 8.1-0 |
| officer	| 0.6.10 |
| pacman	| 0.5.1 |
| patchwork	| 1.3.2 |
| RColorBrewer | 	1.1-3 |
| readr	| 2.1.5 |
| readxl	| 1.4.5 |
| rlang	| 1.1.7 |
| scales	| 1.4.0 |
| sf	| 1.0-21 |
| sp	| 2.2-0 |
| stringr	| 1.6.0 |
| tidyr	| 1.3.2 |
| truncnorm	| 1.0-9 |
| viridis	| 0.6.5 |
| wbmapdata	| 0.0.0.9000 |
