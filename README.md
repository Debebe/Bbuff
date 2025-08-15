### README

This repo is for BCG buffer analysis

### Data processing



- The script `01.TBtx_unit.costs.R` calls `00.TB_care_costs.R` that models TB care costs and produces unit costs 
 for non-TB and TB care. The output is the a data.frame called `tbtx_unit_costs.Rds`. No need to run `00.TB_care_costs.R` separately.
 
- The script `process_data.R` processes data from different sources `(WHO, WUENIC,UN(UN pop & LE),IDCC, GDP`) 
and merges with the above TB treatment unit costs. Generates a data.table with incidence, notification, LE,GDP,vaccine delivery unit costs etc, 
TB treatment and vaccination costs)- The file it produces is called `gdp_inc_le.rds`

- The `inflation_adjustment.R` reads inflation data, processes and then inflates costs in `gdp_inc_le.rds`.It produces a version
with inflated cost - `gdp_inc_le_cost.rds`


 ### Modelling
 
 - The model is executed using `analysis.R`. It works with the final data - `gdp_inc_le_cost.rds`
 
