library(tidyverse)
library(wdpar)
library(terra)
library(tidyterra)
library(sf)
# remotes::install_github("https://github.com/emuise/budR")
library(budR) # has my keys in it

terraOptions(memfrac = 0.9,
             tempdir = "F:\\scratch2")

# bc boundary
bcb_loc <- here::here(shp_loc, "bcb.shp")

if (!file.exists(bcb_loc)) {
  # clean and validate bc bounds
  bcb <- bcmaps::bc_bound_hres() %>%
    vect()
  
  writeVector(bcb, here::here(shp_loc, "bcb.shp"), overwrite = T)
}

bcb <- vect(bcb_loc)

# forests
forests_loc <- here::here(input_loc, "forests.dat")

if (!file.exists(forests_loc)) {
  vlce_template <- rast("F:/mosaiced/VLCE2.0/LC_Class_terr_bc.dat")
  
  forest_rcl <- keys$vlce %>%
    mutate(class_name = ifelse(forest == "Forest", class_val, NA)) %>%
    select(class_val, class_name) %>%
    as.matrix(ncol = 2)
  
  forests <- classify(vlce_template, forest_rcl) %>%
    trim() %>%
    extend(bcb)
  
  writeRaster(
    forests,
    forests_loc,
    filetype = "envi",
    datatype = "INT1U",
    overwrite = T
  )
}

forests <- rast(forests_loc)

bcb_wgs84 <- bcb %>%
  project("epsg:4326")

# some are in wgs84 and thus need to have a different cropping method
# trying cropping to bcb_wgs84 ext plus a boundary, also used to determine
# which provinces/states have large cities for the distance raster

bcb_wgs84_buff <- terra::buffer(bcb_wgs84, 25000)

# bc protected areas
pa_loc <- here::here(shp_loc, "bc_pa_filt.shp")

if (!file.exists(pa_loc)) {
  # get canadian PAs from wdpar
  # this is currently broken due to a chromote update
  # cad_pa <-
  #   wdpa_fetch("CAN",
  #              wait = T,
  #              download_dir = here::here("data"))
  
  all_pa <- here::here("data", "shapefiles") %>%
    list.dirs() %>%
    str_subset("WDPA") %>%
    list.files(pattern = "polygons.shp$", full.names = T) %>%
    map(vect) %>%
    vect()
  
  cad_pa <- all_pa %>%
    filter(ISO3 == "CAN")
  
  # get bc terrestrial PA
  # currentl broken due to chromote but
  # bc_pa <- cad_pa %>%
  #   dplyr::filter(SUB_LOC == "CA-BC") %>%
  #   st_transform(3005) %>%
  #   filter(st_geometry_type(.) == "MULTIPOLYGON")
  # remove point based PAs
  
  # get bc terrestrial PA
  bc_pa <- cad_pa %>%
    filter(SUB_LOC == "CA-BC") %>%
    project("epsg:3005")
  # remove point based PAs
  
  # bc_terr_pa <- bc_pa %>%
  #   st_make_valid() %>%
  #   st_intersection(bcb %>%
  #                     st_as_sf())
  
  pa_terr <- bc_pa %>%
    crop(bcb)
  
  writeVector(pa_terr, here::here(shp_loc, "bc_pa.gpkg"), overwrite = T)
  writeVector(pa_terr, here::here(shp_loc, "bc_pa.shp"), overwrite = T)
  
  # filter pa based off bolton et al. (2018)
  # less than 100 ha; in IUCN class Ia Ib II and IV
  bc_pa_filt <- pa_terr %>%
    mutate(area = expanse(., unit = "ha") %>%
             as.numeric() %>%
             round(digits = 2)) %>%
    filter(IUCN_CAT %in% c("Ia", "Ib", "II", "IV")) %>%
    filter(area > 100) 
  
  bc_pa_filt %>%
    writeVector(here::here(shp_loc, "bc_pa_filt.shp"), overwrite = T)
  
  
  # this rasterization is mangled. done manually in arcgis
  # it ends up missing values and values are just completely wrong
  # use feature to raster tool on bc_pa_filt.shp
  # then align it manually in R. all it needs is extend(., forests)
  # sorry this isn't reproducible!!
  
  # https://github.com/rspatial/terra/issues/1797
  # i made this issue which can hopefully solve it
  
  
  # bc_pa_filt %>%
  #   # vect() %>%
  #   rasterize(forests, field = "WDPAID", touches = T) %>%
  #   writeRaster(here::here(input_loc, "bc_pa_wpdaid2.dat"),
  #               filetype = "envi",
  #               overwrite = T)
  
  write_sf(bc_pa_filt, pa_loc)
  rm(cad_pa, bc_pa, bc_terr_pa, bc_pa_filt)
}

bc_pa_filt <- vect(pa_loc)

# bc bec zones dissolved
bec_loc <- here::here(shp_loc, "bec_terr_agg.shp")
bec_agg_loc <- here::here(shp_loc, "bec_agg.shp")

if (!file.exists(bec_loc)) {
  bec <- bcmaps::bec() %>%
    vect()
  
  bec_agg <- aggregate(bec,
                       by = "ZONE",
                       dissolve = T)
  
  writeVector(bec_agg, bec_agg_loc, overwrite = T)
  
  bec_terr <- intersect(bec_agg, bcb)
  writeVector(bec_terr, bec_loc, overwrite = T)
  rm(bec, bec_agg)
}

bec_terr <- vect(bec_loc)

bec_rst_loc <- here::here(input_loc, "bec.dat") 

if(!file.exists(bec_rst_loc)) {
  bec_agg <- vect(bec_agg_loc)
  bec_rst <- rasterize(bec_agg, forests, field = "ZONE")
  writeRaster(bec_rst, bec_rst_loc, filetype = "envi", overwrite = T)
}

# iter expand function
iter_expand <- function(raster) {
  # project and crop to bcb twice
  # crops to bcb twice because of projection issues
  if (crs(raster) == crs("epsg:4326")) {
    raster <- raster %>%
      crop(bcb_wgs84_buff) %>%
      project(
        "epsg:3005",
        threads = T,
        gdal = T,
        by_util = T
      )
  }
  
  bcb_rast <- bcb %>%
    rasterize(raster, touches = T)
  
  projed <- raster %>%
    crop(bcb_rast, mask = T)
  
  # count how many pixels there should be
  old <- bcb_rast %>%
    freq() %>%
    pull(count)
  
  print("value to match")
  print(old)
  print("----")
  
  # set a base to expand from as the OG raster
  expanded <- projed
  
  
  # loop the focal analysis until the number of pixels in bcb is the same
  # focal, then mask to bcb, then classify and count number of cells
  # if they are the same, break the loop and return the masked output
  for (i in 1:50) {
    expanded <-
      focal(
        expanded,
        w = 5,
        fun = "mean",
        na.rm = F,
        na.policy = "only"
      )
    
    # mask to bcb, classify, then count number of pixels
    masked <- mask(expanded, bcb_rast)
    
    classed <-
      classify(masked, cbind(minmax(projed)[1], minmax(projed)[2], 1), include.lowest = T)
    
    new <- freq(classed) %>%
      pull(count)
    
    print(new)
    
    # if num pixels same, break the loop, return the final masked raster
    if (unique(old == new)) {
      print(paste0("done after ", i, " loops"))
      break
    }
    
    
  }
  masked
}

# climate data; from chris mulverhill derived from climate NA
# mean annual precipitation, mean annual temperature
# mean coldest month temperature
# mean warmest month temperature
# all climate normals (1990-2020)

clim_locs <-
  list.files(path = input_loc,
             pattern = "^clim.*\\.dat$",
             full.names = T)

if (length(clim_locs) != 4) {
  clim_rasts <- list.files(path = "F:/mosaiced/climatena_1k",
                           pattern = ".dat$",
                           full.names = T) %>%
    map(rast)
  
  names <- map(clim_rasts, sources) %>%
    map(basename) %>%
    map(str_replace, pattern = "BC", replacement = "clim") %>%
    unlist() %>%
    here::here(input_loc, .)
  
  clim_rasts <- clim_rasts %>%
    map(iter_expand)
  
  clim_rasts <- rast(clim_rasts)
  
  names(clim_rasts) <- names %>%
    basename() %>%
    tools::file_path_sans_ext()
  
  clim_res <- clim_rasts %>%
    resample(forests, method = "cubicspline", threads = T)
  
  map2(
    writeRaster,
    .x = clim_res %>% as.list(),
    .y = names,
    filetype = "envi",
    overwrite = T,
    .progress = "Save"
  )
}

clim_locs <-
  list.files(path = input_loc,
             pattern = "^clim.*\\.dat$",
             full.names = T)

clim_rasts <- rast(clim_locs)
names(clim_rasts) <- sources(clim_rasts) %>%
  basename() %>%
  tools::file_path_sans_ext()

footprint_loc <-
  here::here(input_loc, "bc_albers_footprint.dat")

# human footprint cropping projecting etc
if (!file.exists(footprint_loc)) {
  footprint <-
    rast("Z:/_CanadaLayers/Rasters/canada_human_footprint/cum_threat2020.02.18.tif")
  
  bcb_lamb <- bcb %>%
    project(footprint)
  
  crop_footprint <- footprint %>%
    crop(bcb_lamb) %>%
    project("epsg:3005",
            threads = T,
            gdal = T,
            by_util = T)
  
  footprint_albers <- iter_expand(crop_footprint) %>%
    resample(forests, method = "cubicspline", threads = T)
  
  names(footprint_albers) = "cad_footprint"
  
  writeRaster(footprint_albers,
              footprint_loc,
              filetype = "envi",
              overwrite = T)
}

footprint <- rast(footprint_loc)

#### structure and dhi

struct_locs <- here::here("F://", "mosaiced", "structure")

struct_varnames <- list.files(struct_locs)

struct_rasts <- map(struct_varnames, \(x) {
  sname <- here::here(input_loc, glue::glue("{x}.dat"))
  print(sname)
  if (!file.exists(sname)) {
    r <- rast(here::here(struct_locs, x, glue::glue("{x}_2015.dat")))
    masked <- r %>%
      crop(y = forests, mask = T)
    
    if(x == "elev_cv") {
      masked[masked > 1000] = 1000
    }
    
    writeRaster(masked, sname, filetype = "envi", overwrite = T)
  }
  rast(sname)
}, .progress = "Structure Masking")

dhi_loc <- here::here("F://", "mosaiced", "DHI_nomask")

dhi_files <-
  list.files(dhi_loc, pattern = ".tif$", full.names = T)

dhi_rasts <- map(dhi_files, \(file) {
  x <- basename(file) %>%
    tools::file_path_sans_ext() %>%
    str_split("-") %>%
    unlist() %>%
    str_subset("DHI")
  
  sname <- here::here(input_loc, glue::glue("{x}.dat"))
  print(sname)
  if (!file.exists(sname)) {
    r <- rast(here::here(file))
    masked <- r %>%
      crop(y = forests, mask = T) %>%
      writeRaster(., sname, filetype = "envi", overwrite = T)
  }
  rast(sname)
}, .progress = "DHI Masking")

# disturbance
dist_loc <- here::here(input_loc, "disturbance.dat")

if(!file.exists(dist_loc)) {
  dist <- rast("F:/mosaiced/change_attribution/Attribution_v2.dat") %>%
    crop(forests, mask = T)
  writeRaster(dist, dist_loc, filetype = "envi")
}
dist <- rast(dist_loc)

# topography
topo <-
  list.files("F:/mosaiced/topo",
             full.names = T,
             pattern = ".dat$") %>%
  map(rast) %>%
  map(crop, forests, .progress = "crop") %>%
  rast()

names(topo) <- list.files("F:/mosaiced/topo", pattern = ".dat$")

map2(as.list(topo), varnames(topo), \(x, y) {
  savename <- here::here(input_loc, glue::glue("{y}.dat"))
  
  writeRaster(x, savename, overwrite = T, filetype = "envi")
}, .progress = "saving topo")
