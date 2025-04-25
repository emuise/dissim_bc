library(terra)
library(tidyverse)
library(tidyterra)
source(here::here("scripts", "0_setup.R"))

url <- "https://www.sciencebase.gov/catalog/file/get/530f8a0ee4b0e7e46bd300dd"

dest <- here::here(scratch, "great_lakes.zip")

if (!file.exists(dest)) {
  download.file(url, destfile = dest, mode = "wb")
}

unzip_dir <- here::here(scratch, "great_lakes")

unzip(dest, exdir = unzip_dir)

great_lakes <- list.files(unzip_dir, pattern = "hydro.*\\.shp$", recursive = T, full.names = T) %>%
  map(vect) %>%
  map(aggregate) %>%
  vect() %>%
  project("epsg:3347")

bcb_hres <- bcmaps::bc_bound_hres() %>%
  vect()

bcb_ext <- ext(bcb_hres)

bec <- here::here(shp_loc, "bec_terr_agg.shp") %>%
  vect() %>%
  select(ZONE) %>%
  left_join(keys$bec, by = c("ZONE" = "zone"))

# download vectors of NA coutnries for the inset map
cad <- geodata::gadm("Canada", level = 1, path = scratch)

usa <- geodata::gadm("USA", level = 0, path = scratch)

world <- geodata::world(path = scratch, resolution = 1) %>%
  project("epsg:3347")

cad_ext <- cad %>%
  project("epsg:3347") %>% 
  ext()

bc_box <- bcb_hres %>% 
  minRect()

water_colour <- "lightblue"

inset <- ggplot() +
  geom_spatvector(data = world, fill = "#7f7f7f") +
  geom_spatvector(data = cad %>% project("epsg:3347"), fill = "#b3b3b3") +
  geom_spatvector(data = great_lakes, fill = water_colour) +
  geom_spatvector(data = bcb_hres, fill = "#e5e5e5") +
  #geom_spatvector(data = bc_box, col = "red", fill = "#00000000", linewidth = 1) +
  coord_sf(xlim = c(cad_ext$xmin, cad_ext$xmax),
           ylim = c(cad_ext$ymin, cad_ext$ymax)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = water_colour, colour = "#595959"),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        plot.background = element_blank())

main <- ggplot() +

  geom_spatvector(data = usa %>% project(bec), fill = "#7f7f7f") +
  geom_spatvector(data = cad %>% project(bec) %>%
                    filter(NAME_1 != "British Columbia"), fill = "#b3b3b3") +
  geom_spatvector(data = bec,
                  aes(fill = fct_reorder(zone_nm, order),
                      col = after_scale(fill))) +
  geom_spatvector(data = bcb_hres,
                  fill = "#00000000") +
  ggsci::scale_fill_igv() +
  # scico::scale_fill_scico_d(palette = "batlow") +
  # scale_fill_manual(values = scico::scico(n = 16, palette = "batlow", categorical = T)) +
  theme_bw() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.675),
    panel.background = element_rect(fill = water_colour),
    legend.background = element_rect(fill = "white", colour = "#595959"),
    legend.text = element_text(hjust = 0.5),
    legend.title = element_text(hjust = 0.5)
  ) +
  coord_sf(
    xlim = c(bcb_ext$xmin, bcb_ext$xmax + 320000),
    ylim = c(bcb_ext$ymin, bcb_ext$ymax)
  ) +
  labs(fill = NULL)

study_area <- main +
  inset_element(inset, 0, 0, 0.33, 0.33)

ggsave(here::here(fig_loc, "study_area.png"), plot = study_area, height = 10, width = 10, dpi = 300)
