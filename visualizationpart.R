# ==================================================
# INSTALL PACKAGES IF NEEDED
# ==================================================

packages <- c(
  "tidyverse",
  "sf",
  "ggthemes",
  "leaflet", 
  "tmap", 
  "osmdata", 
  "terra"
)

installed <- packages %in% installed.packages()

if(any(!installed)) {
  install.packages(packages[!installed])
}

# ==================================================
# LOAD LIBRARIES
# ==================================================

library(tidyverse)
library(sf)
library(ggthemes)
library(osmdata)
library(leaflet)
library(tmap)
library(terra)

# ==================================================
# DEFINING PATHES FROM OSM FILES
# ==================================================


# READ MAIN LAYERS
adminareas <- st_read("gis_osm_adminareas_a_free_1.shp")
landuse <- st_read("gis_osm_landuse_a_free_1.shp")
roads <- st_read("gis_osm_roads_free_1.shp")
water <- st_read("gis_osm_water_a_free_1.shp")

#=====================================
# CHECK düsseldorf area in data
#=====================================

st_crs(adminareas)
names(adminareas)
head(adminareas)

adminareas |>
  st_drop_geometry() |>
  distinct(name, fclass) |>
  View()
adminareas |>
  st_drop_geometry() |>
  filter(str_detect(name, "D"))

#=====================================
#landscape in düsseldorf
#=====================================

dusseldorf_boundary <- adminareas |>
  filter(name == "Düsseldorf")
plot(st_geometry(dusseldorf_boundary))
ggplot() +
  geom_sf(data = dusseldorf_boundary,
          fill = "lightblue",
          color = "black") +
  theme_minimal()
parks <- landuse |>
  filter(fclass %in% c(
    "park",
    "forest",
    "grass",
    "recreation_ground"
  ))
parks_clipped <- st_intersection(
  parks,
  dusseldorf_boundary
)
ggplot() +
  geom_sf(data = dusseldorf_boundary,
          fill = "gray95",
          color = "black") +
  
  geom_sf(data = parks_clipped,
          fill = "darkgreen",
          color = NA,
          alpha = 0.7) +
  
  theme_void()

#===========================================
#Check for residental areas in data
#=====================================

landuse |>
  st_drop_geometry() |>
  distinct(fclass) |>
  View()

#=====================================
#Adding Residental areas to map
#=====================================

residential <- landuse |>
  filter(fclass == "residential")

residential_clipped <- st_intersection(
  residential,
  dusseldorf_boundary
)

ggplot() +
  geom_sf(data = dusseldorf_boundary,
          fill = "gray95",
          color = "black",
          linewidth = 0.3) +
  
  geom_sf(data = residential_clipped,
          fill = "gray35",
          color = NA,
          alpha = 0.8) +
  
  geom_sf(data = parks_clipped,
          fill = "darkgreen",
          color = NA,
          alpha = 0.75) +
  
  theme_void()

#=====================================
#Adding Rhein River  to map
#=====================================

water_clipped <- st_intersection(
  water,
  dusseldorf_boundary
)

ggplot() +
  geom_sf(data = dusseldorf_boundary,
          fill = "gray95",
          color = "black",
          linewidth = 0.3) +
  geom_sf(data = water_clipped,
          fill = "lightblue",
          color = NA) +
  geom_sf(data = residential_clipped,
          fill = "gray40",
          color = NA,
          alpha = 0.8) +
  geom_sf(data = parks_clipped,
          fill = "darkgreen",
          color = NA,
          alpha = 0.75) +
  theme_void()


#=====================================
#Adding Roads to map
#=====================================

roads_clipped <- st_intersection(
  roads,
  dusseldorf_boundary
)

ggplot() +
  geom_sf(data = dusseldorf_boundary,
          fill = "gray95",
          color = "black",
          linewidth = 0.3) +
  geom_sf(data = water_clipped,
          fill = "lightblue",
          color = NA) +
  geom_sf(data = residential_clipped,
          fill = "gray40",
          color = NA,
          alpha = 0.8) +
  geom_sf(data = parks_clipped,
          fill = "darkgreen",
          color = NA,
          alpha = 0.75) +
  geom_sf(data = roads_clipped,
          color = "grey35",
          linewidth = 0.08,
          alpha = 1)+
  theme_void() +
    theme(
      plot.background = element_rect(fill = "white",
                                   color = NA)
  )

#=====================================
#Try to Analaysis 1
#=====================================

residential_points <- residential_clipped |>
  st_centroid()

distance_to_parks <- st_distance(
  residential_points,
  parks_clipped
)

min_distance <- apply(
  distance_to_parks,
  1,
  min
)

residential_points$park_distance <- as.numeric(min_distance)

ggplot() +
  geom_sf(data = dusseldorf_boundary,
          fill = "gray95",
          color = "black") +
  
  geom_sf(data = parks_clipped,
          fill = "#9da78f",
          color = NA) +
  
  geom_sf(data = residential_points,
          aes(color = park_distance),
          size = 0.35,
          alpha = 0.65) +
  
  scale_color_gradient(
  low = "#6D2E46",
  high = "#D8A48F"
) +
  labs(color = "Distance to\nGreen Areas (m)")+
  
  theme_void()

#=====================================
#Try other packages and visual styles
#=====================================

#leaflet

leaflet() |>
  addTiles() |>
  addPolygons(
    data = dusseldorf_boundary,
    fillColor = "gray95",
    color = "black",
    weight = 1,
    fillOpacity = 0.3
  ) |>
  addPolygons(
    data = parks_clipped,
    fillColor = "darkgreen",
    color = NA,
    fillOpacity = 1,
    popup = ~fclass
  )

#tmap

tmap_mode("plot")

tm_shape(dusseldorf_boundary) +
  tm_polygons(fill = "gray95", col = "black") +
  tm_shape(parks_clipped) +
  tm_polygons(fill = "#9da78f", col = NA) +
  tm_shape(residential_points) +
  tm_dots(
    col = "park_distance",
    palette = c("#6D2E46", "#D8A48F"),
    size = 0.05,
    title = "Distance to\nGreen Areas (m)"
  ) +
  tm_layout(frame = FALSE)

#Terra

parks_vect <- vect(parks_clipped)

r <- rast(
  ext(parks_vect),
  resolution = 0.001,
  crs = crs(parks_vect)
)

parks_raster <- rasterize(
  parks_vect,
  r,
  field = 1
)
plot(parks_raster)

#ggthemes

ggplot() +
  geom_sf(data = dusseldorf_boundary,
          fill = "gray95",
          color = "black",
          linewidth = 0.2) +
  geom_sf(data = parks_clipped,
          fill = "#9da78f",
          color = NA,
          alpha = 0.8) +
  geom_sf(data = residential_points,
          aes(color = park_distance),
          size = 0.35,
          alpha = 0.65) +
  scale_color_gradient(
    low = "#6D2E46",
    high = "#D8A48F"
  ) +
  labs(
    title = "Accessibility to Green Areas in Düsseldorf",
    subtitle = "Residential proximity analysis using OSM spatial data",
    color = "Distance to\nGreen Areas (m)"
  ) +
  ggthemes::theme_map() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )
