library(tidyverse)
library(terra)
library(tidyterra)
library(reticulate)
library(arrow)
library(budR)
library(patchwork)

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
pq_loc <- here::here(data_loc, "pq")
dir.create(pq_loc, showWarnings = F)
output_loc <- here::here(data_loc, "outputs")
dir.create(output_loc, showWarnings = T)

bc_pa <- here::here(shp_loc, "bc_pa.gpkg") %>%
  vect()

bc_pa_filt <- bc_pa %>%
  mutate(area = expanse(., unit = "ha") %>%
           as.numeric() %>%
           round(digits = 2)) %>%
  filter(IUCN_CAT %in% c("Ia", "Ib", "II", "IV")) %>%
  filter(area > 100) %>%
  crop(bcmaps::bc_bound_hres())

plot <- bc_pa_filt %>%
  as_tibble() %>%
  #group_by(seq(min(STATUS_YR, na.rm = T), max(STATUS_YR, na.rm = T), by = 1)) %>%
  #summarize(area = sum(area)) %>%
  ggplot(aes(x = STATUS_YR, y = area / 1000)) +
  geom_point(alpha = 0.1) +
  theme_bw() +
  geom_smooth(method = "lm", col = "red", lty = "dashed") +
  labs(x = "Year of Establishment",
       y = "Area (1000 ha)")

area_year <- ggMarginal(plot, type = "histogram")

ggsave("area_year.png", plot <- area_year, device = "png", dpi = 300, height = 6, width = 6)

bec <- vect("E:/Sync/Masters/analysis_02_sem/data/shapefiles/bec_dissolve.shp") %>%
  crop(bcmaps::bc_bound_hres())

prop_pro <- union(bec, bc_pa_filt)

prop_pro %>% 
  mutate(area = expanse(.)) %>%
  as_tibble() %>%
  group_by(ZONE, pa = !is.na(WDPA_PID)) %>%
  summarize(area = sum(area))

bec_pa <- bc_pa_filt %>%
  intersect(bec)

bec_pa %>%
  mutate(area = expanse(., unit = "ha") %>%
           as.numeric() %>%
           round(digits = 2)) %>%
  as_tibble() %>%
  ggplot(aes(x = STATUS_YR, y = area, col = ZONE)) +
  geom_point(alpha = 0.1) +
  theme_bw() +
  #geom_smooth(method = "lm") +
  labs(x = "Year of Establishment",
       y = "Area (ha)")

dat <- bec_pa %>%
  as_tibble() %>%
  select(ZONE, STATUS_YR, IUCN_CAT, WDPAID, area) %>%
  group_by(ZONE, STATUS_YR) %>%
  summarize(area = sum(area)) %>%
  ungroup() %>%
  complete(ZONE, STATUS_YR = full_seq(STATUS_YR, 1), fill = list(area = 0)) %>%
  arrange(ZONE, STATUS_YR) %>%
  group_by(ZONE) %>%
  mutate(cs_area = cumsum(area),
         per_area = cs_area / max(cs_area)) %>%
  pivot_longer(ends_with("_area")) %>%
  mutate(name = case_when(name == "cs_area" ~ "Cumulative Area\n(ha)",
                          name == "per_area" ~ "Cumulative Area\n(% of Zonal Total)"))


dat %>%
  ggplot(aes(x = STATUS_YR, y = value, col = ZONE)) +
  geom_line() +
  theme_bw() +
  # Add text labels at the last year for each zone
  geom_text_repel(data = . %>% group_by(ZONE) %>% slice_max(STATUS_YR, n = 1),
                  aes(label = ZONE), 
                  hjust = 0, # Align text to the right
                  nudge_x = 5, # Push text slightly to the right
                  direction = "y", # Avoid overlapping vertically
                  segment.color = NA) + # Remove connecting lines
  theme_bw() +
  theme(legend.position = "none") +# Hide legend since labels replace it 
  labs(x = "Established Year",
       y = "Cumulative Area (ha)") +
  scale_colour_igv() +
  facet_wrap(~fct_rev(name), scales = "free_y", nrow = 2)