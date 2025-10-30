# 1 - check if i have the packages that i will need

library(BioAge)
library(devtools)
library(dplyr)
library(tidyverse)

# 2 - Validation of the algorithms (run the tutorial)

  # HD using NHANES (separate training for men and women)
hd = hd_nhanes(biomarkers=c("albumin","alp","lncrp","totchol","lncreat","hba1c","sbp","bun","uap","lymph","mcv","wbc"))

  # KDM bioage using NHANES (separate training for men and women)
kdm = kdm_nhanes(biomarkers=c("albumin","alp","lncrp","totchol","lncreat","hba1c","sbp","bun","uap","lymph","mcv","wbc"))

  # phenoage using NHANES
phenoage = phenoage_nhanes(biomarkers=c("albumin_gL","alp","lncrp","totchol", "lncreat_umol","hba1c","sbp","bun","uap","lymph","mcv","wbc"))

  # assemble NHANES IV dataset with projected biological aging measures for analysis
data = merge(hd$data, kdm$data) %>% merge(., phenoage$data)

  # select biological age variables
agevar = c("kdm0","phenoage0","kdm","phenoage","hd","hd_log")

  # prepare labels
label = c("KDM\nBiological Age",
          "Levine\nPhenotypic Age",
          "Modified-KDM\nBiological Age",
          "Modified-Levine\nPhenotypic Age",
          "Homeostatic\nDysregulation",
          "Log\nHomeostatic\nDysregulation")

  # plot age vs bioage
plot_ba(data, agevar, label)

  # select biological age variables
agevar = c("kdm_advance0","phenoage_advance0","kdm_advance","phenoage_advance","hd","hd_log")

  # prepare labels
  # values should be formatted for displaying along diagonal of the plot
  # names should be used to match variables and order is preserved
label = c(
  "kdm_advance0"="KDM\nBiological Age\nAdvancement",
  "phenoage_advance0"="Levine\nPhenotypic Age\nAdvancement",
  "kdm_advance"="Modified-KDM\nBiological Age\nAdvancement",
  "phenoage_advance"="Modified-Levine\nPhenotypic Age\nAdvancement",
  "hd" = "Homeostatic\nDysregulation",
  "hd_log" = "Log\nHomeostatic\nDysregulation")

  # use variable name to define the axis type ("int" or "float")
axis_type = c(
  "kdm_advance0"="float",
  "phenoage_advance0"="float",
  "kdm_advance"="float",
  "phenoage_advance"="flot",
  "hd"="flot",
  "hd_log"="float")

  # plot BAA corplot
plot_baa(data,agevar,label,axis_type)

table_surv(data, agevar, label)

table2 = table_health(data,agevar,outcome = c("health","adl","lnwalk","grip_scaled"), label)

 # pull table
table2$table

  # pull sample sizes
table2$n

table3 = table_ses(data,agevar,exposure = c("edu","annual_income","poverty_ratio"), label)

  # pull table
table3$table

  # pull sample sizes
table3$n

# EVERYTHING IS GOOD TO GO!