

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(terra, fs, tidyverse, sf, glue, gtools, crayon, geodata)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
tble <- read_csv('data/tbl/Croppie_Line_base_Peru_20220920_fixed_commented.csv', skip = 1)
per1 <- gadm(country = 'PER', level = 1, path = 'tmpr')

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
pnts <- mutate(pnts, name_1 = adm1)

'Cosecha QQ/ha parcela'
sort(colnames(pnts))

colnames(pnts)[which(colnames(pnts) == 'Cosecha QQ/ha parcela')] <- 'cosecha_ha_parcela'
'Fecha_de_la_posible_cosecha'

# Relocate columns --------------------------------------------------------
pnts <- relocate(pnts, cosecha_ha_parcela, Fecha_de_la_posible_cosecha, name_1)








