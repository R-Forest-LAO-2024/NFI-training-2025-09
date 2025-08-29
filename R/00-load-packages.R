## Simple function
use_package <- function(.pkg_name) {
  pkg_name <- as.character(substitute(.pkg_name))
  if (!require(pkg_name, character.only = T,  quietly = TRUE)) install.packages(pkg_name, dep =TRUE)
  library(pkg_name, character.only = T, quietly = TRUE)
}


use_package(usethis)
use_package(here)
use_package(readr)
use_package(stringr)
use_package(tidyr)
use_package(lubridate)
use_package(ggplot2)
use_package(ggrepel)
use_package(ggpubr)
use_package(purrr)
use_package(dplyr)
use_package(sf)
use_package(terra)
use_package(writexl)

## Default theme for ggplot
theme_set(theme_bw())