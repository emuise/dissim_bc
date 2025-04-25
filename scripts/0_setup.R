library(tidyverse)
library(terra)
library(tidyterra)
library(reticulate)
library(arrow)
library(budR)
library(patchwork)
library(ggExtra)
library(exactextractr)
library(sf)

data_loc <- here::here("data")
dir.create(data_loc, showWarnings = F)
shp_loc <- here::here(data_loc, "shapefiles")
dir.create(shp_loc, showWarnings = F)
rst_loc <- here::here(data_loc, "rasters")
dir.create(rst_loc, showWarnings = F)
fig_loc <- here::here("figures")
dir.create(fig_loc, showWarnings = F)
input_loc <- here::here(data_loc, "inputs")
dir.create(input_loc, showWarnings = F)
scratch <- here::here(data_loc, "scratch")
dir.create(scratch, showWarnings = F)
pq_loc <- here::here(data_loc, "pq2")
dir.create(pq_loc, showWarnings = F)
output_loc <- here::here(data_loc, "outputs")
dir.create(output_loc, showWarnings = F)


terraOptions(memfrac = 0.9)

my_theme <- theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom")

theme_set(my_theme)
