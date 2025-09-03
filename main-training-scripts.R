

## Useful when sourcing script from Quarto doc.
if (!require(here)) install.packages("here"); library(here)

## Load packages
source(here("R/00-load-packages.R"))

source(here("R/fct-nfi-aggregate3-new.R"))

## Training sessions
source(here("R/SOL-05-data-preparation.R"))

source(here("R/SOL-06-data-joins.R"))

source(here("R/SOL-07a-tree-weight.R"))

source(here("R/SOL-07b-tree-basal-area.R"))

source(here("R/SOL-08a-tree-agb.R"))

#source(here("R/SOL-08b-tree-bgb.R"))

#source(here("R/SOL-08c-tree-carbon.R"))

source(here("R/SOL-09a-aggregation-plotequal.R"))

