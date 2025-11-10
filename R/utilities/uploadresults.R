## =======================================
## LOADING TO GOOGLE SHEETS (authors only)
## =======================================
rm(list = ls())

library(here)
library(data.table)
library(glue)
library(googlesheets4)

## setup - only accessible to those with access to this sheet NOTE new
yourl <- "https://docs.google.com/spreadsheets/d/1-byoo8axKl_G-t4trsUP8gpuhy63akx379egbOVJSPM/edit?gid=0#gid=0"
shid <- as.character(as_sheets_id(yourl))


## utility function
upload.to.sheets <- function(filename, sheetid) {
  fn <- glue(here("outputs/{filename}"))
  tmp <- fread(file = fn)
  sht <- gsub("\\.csv", "", filename)
  write_sheet(tmp, sheetid, sheet = sht)
}


## read & upload relevant data
upload.to.sheets("output_table.csv", shid)
upload.to.sheets("output_table_who.csv", shid)
upload.to.sheets("top10_cntrs.csv", shid)
upload.to.sheets("regression_ENB.csv", shid)
upload.to.sheets("regression_icer.csv", shid)
upload.to.sheets("statistics.csv", shid)
upload.to.sheets("Rsq.csv", shid)
upload.to.sheets("sens.csv", shid)
upload.to.sheets("sumary_sen.csv", shid)




