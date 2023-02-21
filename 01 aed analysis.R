

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(rnaturalearthdata, rnaturalearth, ggspatial, RColorbrewer, ctpcity, showtext, extrafont, terra, fs, tidyverse, sf, glue, gtools, crayon, geodata)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Fonts
windowsFonts(georg = windowsFont('Georgia'))

# Load data ---------------------------------------------------------------
tble <- read_csv('data/tbl/Croppie_Line_base_Peru_20220920_fixed_commented.csv', skip = 1)
per1 <- gadm(country = 'PER', level = 1, path = 'tmpr')
per2 <- gadm(country = 'PER', level = 2, path = 'tmpr')

# Check NA ----------------------------------------------------------------
sum(is.na(pull(tble, GPS_longitude)))
nrow(tble) # 677
tble <- tble[!is.na(pull(tble, GPS_longitude)),]
nrow(tble) # 645

# Table to shapefile (sf) -------------------------------------------------
pnts <- st_as_sf(x = tble, coords = c('GPS_longitude', 'GPS_latitude'))

# Check the position of each point ----------------------------------------
plot(per1, border = 'grey30')
plot(st_geometry(pnts), add = TRUE, col = 'red', pch = 16)

# Add the location of each point ------------------------------------------
adm1 <- terra::extract(per1, vect(pnts))[,'NAME_1']
adm2 <- terra::extract(per2, vect(pnts))[,'NAME_2']
pnts <- mutate(pnts, name_1 = adm1, name_2 = adm2)

'Cosecha QQ/ha parcela'
sort(colnames(pnts))

colnames(pnts)[which(colnames(pnts) == 'Cosecha QQ/ha parcela')] <- 'cosecha_ha_parcela'
'Fecha_de_la_posible_cosecha'

# Relocate columns --------------------------------------------------------
pnts <- relocate(pnts, cosecha_ha_parcela, Fecha_de_la_posible_cosecha, name_1, name_2)
table(pnts$name_1); table(pnts$name_2)
pnts <- mutate(pnts, cosecha_ha_parcela = as.numeric(cosecha_ha_parcela))

# Aggregate to polygon  ---------------------------------------------------
poly <- pnts %>% 
  st_drop_geometry %>% 
  dplyr::select(cosecha_ha_parcela, Fecha_de_la_posible_cosecha, name_1, name_2) %>% 
  group_by(name_1, name_2) %>% 
  dplyr::summarise(cosecha_ha_parcela = mean(cosecha_ha_parcela, na.rm = T)) %>% 
  ungroup() %>% 
  inner_join(st_as_sf(per2), ., by = c('NAME_2' = 'name_2'))

# To make the map ---------------------------------------------------------
wrld <- ne_countries(returnclass = 'sf', scale = 50)

gmap <- ggplot() + 
  geom_sf(data = wrld, fill = NA, col = 'grey70') + 
  geom_sf(data = st_as_sf(per1), fill = NA, col = 'grey70') +
  geom_sf(data = poly, aes(fill = cosecha_ha_parcela), col = 'grey70') +
  scale_fill_gradientn(colors = brewer.pal(n = 5, name = 'Spectral')) +
  coord_sf(xlim = ext(poly)[1:2], ylim = ext(poly)[3:4]) +
  labs(x = 'Lon', y = 'Lat', fill = 'QQ / ha') +
  theme_minimal() + 
  theme(legend.position = 'bottom', 
        legend.key.width = unit(3, 'line'), 
        text = element_text(family = 'georg'),
        legend.key.height = unit(0.5, 'line')) +# text = element.text(family = 'serif')
  guides(fill = guide_legend( 
    # direction = 'horizontal',
    keyheight = unit(5.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  )) +
  annotation_scale(location =  "bl", width_hint = 0.5, text_family = 'georg', text_col = 'grey60', bar_cols = c('grey60', 'grey99'), line_width = 0.2) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering(text_family = 'georg', text_col = 'grey40', line_col = 'grey60', fill = c('grey60', 'grey99'))) 

ggsave(plot = gmap, filename = 'figures/graphs/polygon_yields_per.png', units = 'in', width = 7, height = 6, dpi = 300)
