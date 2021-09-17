library(plotly)
library(flexdashboard)
library(leaflet)
library(crosstalk)

library(gt)

library(openxlsx)
library(readxl)
library(magrittr)

library(FedData)
library(tigris)
library(tidycensus)
library(tidygeocoder)
library(PL94171)

library(raster)
library(rmapshaper)
library(sf)
library(binsmooth)
library(kdensity)

library(tidyverse)
options(tigris_use_cache = TRUE)
source("./acs helpers.R")
acs2019 <- load_variables(cache = TRUE, year = 2019, dataset = "acs5")