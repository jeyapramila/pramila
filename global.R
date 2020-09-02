
library(googleVis)
library(knitr)
library(shiny)

#download.file("http://www.openintro.org/stat/data/mlb11.RData", destfile = "mlb11.RData")
load("ENT1_ORG_PULSES.RData")
load("ENT2_ORG_VEG.RData")
load("SERVICE_WELL_RENOVATION1.RData")
load("SERVICE_CROP_LOAN1.RData")
load("SERVICE_CATTLE_PURCHASE1.RData")
load("SERVICE_POULTRY_SHED.RData")

rownames(mlb11) <- mlb11$team
mlb11$team <- NULL

