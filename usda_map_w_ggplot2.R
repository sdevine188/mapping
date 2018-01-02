library(maptools)
library(mapproj)
library(rgeos)
library(rgdal)
library(RColorBrewer)
library(ggplot2)
library(spdplyr)
library(geojsonio)
library(rmapshaper)
library(lawn)
library(readr)
library(dplyr)
library(stringr)


# create us choropleth with AK and HI inset
# https://github.com/hrbrmstr/rd3albers

# setwd
setwd("C:/Users/Stephen/Desktop/R/mapping/usda_map/county_shapefiles")
list.files()

# load census shapefile
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
county <- readOGR(dsn = ".", layer = "cb_2016_us_county_500k")
head(county)
glimpse(county)


##########################################3


# load census state fips and filter to just us states
# https://www.census.gov/geo/reference/ansi_statetables.html
setwd("C:/Users/Stephen/Desktop/R/mapping/usda_map")
list.files()
state_fips <- read_csv("census_state_fips.csv")
head(state_fips)
nrow(state_fips)
# pad state_fips
state_fips <- state_fips %>% mutate(fips_state_pad = str_pad(fips_state, width = 2, side = "left", pad = "0"))
head(state_fips)

# inspect
names(county)
county %>% distinct(STATEFP)
county %>% distinct(STATEFP) %>% nrow(.)
county_filtered <- county %>% filter(STATEFP %in% state_fips$fips_state_pad)
county_filtered %>% distinct(STATEFP)
county_filtered %>% distinct(STATEFP) %>% nrow(.)

discarded_state_index <- !(unique(county$STATEFP) %in% unique(county_filtered$STATEFP))
unique(county$STATEFP)[discarded_state_index]


#############################################


# convert shapefile to geoJSON
# https://blog.exploratory.io/creating-geojson-out-of-shapefile-in-r-40bc0005857d
county_json <- geojson_json(county_filtered)
head(county_json)


###########################################


# simplify json to reduce file size (optional, but effective)
county_json_simplified <- ms_simplify(county_json)
head(county_json_simplified)
str(county_json_simplified)

##############################################


# save geoJSON file
getwd()
geojson_write(county_json_simplified, file = "county.geojson")


###############################################


# follow tutorial on creating ggplot map with AK and HI inset
# https://github.com/hrbrmstr/rd3albers

# read in geojson
us <- readOGR(dsn = "county.geojson", layer = "OGRGeoJSON")


###############################################


# convert it to Albers equal area
us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)
head(us_aea)

################################################


# extract, then rotate, shrink & move alaska (and reset projection)
# need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html
alaska <- us_aea[us_aea$STATEFP == "02", ]
alaska <- elide(alaska, rotate = -50)
alaska <- elide(alaska, scale = max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift = c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us_aea)

# extract, then rotate & shift hawaii
hawaii <- us_aea[us_aea$STATEFP == "15", ]
hawaii <- elide(hawaii, rotate = -35)
hawaii <- elide(hawaii, shift = c(5400000, -1400000))
proj4string(hawaii) <- proj4string(us_aea)


###############################################


# remove old states and put new ones back in; note the different order
# between texas and florida via similar methods to the ones we just used
us_aea <- us_aea[!us_aea$STATEFP %in% c("02", "15"), ]
us_aea <- rbind(us_aea, alaska, hawaii)
head(us_aea)

################################################


# get ready for ggplotting it... this takes a few seconds
map <- fortify(us_aea, region="GEOID")
str(map)
head(map)


##############################################


# load and clean usda data for plotting

# load usda data
getwd()
list.files()
usda_data <- read_csv("DraftLatLonData.csv")

# make LONG negative
usda_data <- usda_data %>% mutate(LONG = LONG * -1)

head(usda_data)
glimpse(usda_data)

# create fips_state_county to match map id variable
usda_data <- usda_data %>% mutate(fips_state_county = str_c(STATE, COUNTY, sep = ""))
head(usda_data$fips_state_county)

# combine usda data with full census list of counties to avoid blank spots on map
census_counties <- us_aea@data
census_counties <- census_counties %>% mutate(GEOID = as.character(GEOID))
head(census_counties)
glimpse(census_counties)

usda_data <- left_join(census_counties, usda_data, by = c("GEOID" = "fips_state_county"))
glimpse(usda_data)

# convert NA values of DummyVar123 to zero
usda_data %>% summarize(na_count = sum(is.na(DummyVar123)), total_rows = n())
usda_data %>% distinct(DummyVar123)
usda_data <- usda_data %>% mutate(DummyVar123 = case_when(is.na(DummyVar123) ~ 0, TRUE ~ as.numeric(DummyVar123)))
usda_data %>% distinct(DummyVar123)
glimpse(usda_data)


##############################################
#############################################
#################################################


# create map of DummyVar123

# create ggplot object
dummy_var_map <- ggplot()

# add county outlines
dummy_var_map <- dummy_var_map + geom_map(data = map, map = map,
                    aes(x = long, y = lat, map_id = id, group = group),
                    fill = "white", color = "white", size = 0.15)

# add data to fill in counties
dummy_var_map <- dummy_var_map + geom_map(data = usda_data, map = map, 
                    aes(map_id = GEOID, fill = factor(DummyVar123)), color = "white", size = 0.15)

# add title
dummy_var_map <- dummy_var_map + labs(title = "USDA DummyVar123 by County")

# fix coordinates
dummy_var_map <- dummy_var_map + coord_equal()

# create discrete color scale 
display.brewer.all()
brewer.pal(9, "Blues")
discrete_blues <- brewer.pal(9, "Blues")[c(9, 5)]
dummy_var_map <- dummy_var_map + scale_fill_manual(values = discrete_blues, name = "DummyVar123")

# add theme options to clean appearance
dummy_var_map <- dummy_var_map + theme_bw() +
        theme(plot.background = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), panel.border = element_blank(), 
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              plot.title = element_text(size = 12, face = "bold", hjust = .5), legend.position = "right",
              legend.key.size = unit(2, "mm"), legend.title = element_text(size = 7),
              legend.text = element_text(size = 7))

dummy_var_map

# save map
ggsave(filename = "dummy_var_map.pdf", plot = dummy_var_map, width = 5, height = 5, 
       units = "in", dpi = 300)


##########################################
#################################################
#################################################


# create map of pct_water
usda_data <- usda_data %>% 
        mutate(log_pct_water = log(as.numeric(as.character(AWATER)) / as.numeric(as.character(ALAND)) * 100))
glimpse(usda_data)

# create ggplot object
pct_water_map <- ggplot()

# add county outlines
pct_water_map <- pct_water_map + geom_map(data = map, map = map,
                    aes(x = long, y = lat, map_id = id, group = group),
                    fill = "white", color = "white", size = 0.15)

# add data to fill in counties
pct_water_map <- pct_water_map + geom_map(data = usda_data, map = map, 
                    aes(map_id = GEOID, fill = log_pct_water), color = "white", size = 0.15)

# add title
pct_water_map <- pct_water_map + labs(title = "Percent Water by County (log scale)")

# fix coordinates
pct_water_map <- pct_water_map + coord_equal()
 cx 
# create color scale 
pct_water_map <- pct_water_map + scale_fill_gradientn(colours = viridis(256, alpha = 1), name = "Percent Water (log)")

# add theme options to clean appearance
pct_water_map <- pct_water_map + theme_bw() +
        theme(plot.background = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), panel.border = element_blank(), 
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              plot.title = element_text(size = 12, face = "bold", hjust = .5), legend.position = "right",
              legend.key.size = unit(2, "mm"), legend.title = element_text(size = 7),
              legend.text = element_text(size = 7))

pct_water_map

# save map
ggsave(filename = "pct_water_map.pdf", plot = pct_water_map, width = 5, height = 5, 
       units = "in", dpi = 300)


######################################################
#####################################################
######################################################


# plot points using albers equal area projection instead of lat/lon
# https://github.com/hrbrmstr/rd3albers
# http://rfunctions.blogspot.com/2016/06/how-to-project-coordinates-rasters-and.html
# https://mgimond.github.io/Spatial/coordinate-systems-in-r.html

# create county map
# setwd
setwd("C:/Users/Stephen/Desktop/R/mapping/usda_map/county_shapefiles")
list.files()

# load census shapefile
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
county <- readOGR(dsn = ".", layer = "cb_2016_us_county_500k")
summary(county)

# transform lat/lon to aea
county_aea <- spTransform(county, 
                          CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
summary(county_aea)

# fortify map to prepare it for ggplot
map2 <- fortify(county_aea, region="GEOID")
glimpse(map2)
str(map2)


###################################


# load usda data
setwd("C:/Users/Stephen/Desktop/R/mapping/usda_map")
list.files()
usda_data <- read_csv("DraftLatLonData.csv")
head(usda_data)
glimpse(usda_data)

# make LONG negative
usda_data <- usda_data %>% mutate(LONG = LONG * -1)

# create usda_data subsets for conus, ak, hi
# usda_data_conus <- usda_data %>% filter(!is.na(LAT), !(STATE %in% c("02", "15"))) %>% select(LONG, LAT)
usda_data_conus <- usda_data %>% filter(!is.na(LAT), !(STATE %in% c("02", "15"))) %>% select(LONG, LAT, DummyVar123)
usda_data_ak <- usda_data %>% filter(STATE == "02") %>% select(LONG, LAT, DummyVar123)
usda_data_hi <- usda_data %>% filter(STATE == "15")  %>% select(LONG, LAT, DummyVar123)


####################################################333


# convert points from lat/lon to aea for use in ggplot2

# convert coordinates into a "SpatialPoints" object.
coordinates(usda_data_conus) <- usda_data_conus
coordinates(usda_data_ak) <- usda_data_ak
coordinates(usda_data_hi) <- usda_data_hi 

str(usda_data_conus)
summary(usda_points)
proj4string(usda_points)

# add projection string to sp object to denote what projection it is currently using
proj4string(usda_data_conus) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
proj4string(usda_data_ak) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
proj4string(usda_data_hi) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

# transform lat/long to aea coordinates
usda_data_conus_aea <- spTransform(usda_data_conus, 
                          CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
usda_data_ak_aea <- spTransform(usda_data_ak, 
                                   CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
usda_data_hi_aea <- spTransform(usda_data_hi, 
                                   CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))

str(aea_coords)
aea_coords@data

# convert aea sp object to coordinates for use in ggplot
usda_data_conus_aea <- data.frame(coordinates(usda_data_conus_aea))
usda_data_ak_aea <- data.frame(coordinates(usda_data_ak_aea))
usda_data_hi_aea <- data.frame(coordinates(usda_data_hi_aea))

head(aea_coords2)
str(aea_coords2)


####################################3


# create maps for conus, ak, and hi
map2 <- map2 %>% mutate(fips_state = str_sub(id, start = 1, end = 2))
conus_map <- map2 %>% filter(fips_state %in% state_fips$fips_state_pad, !(fips_state %in% c("02", "15")))
# note alaska's island chain stretching west actually crosses 180th meridian
# this means is goes from very negative long, to very postive long at tail end
# ggplot plots this as east-west split instead of continuation, so just drop few points with long > 100
# https://en.wikipedia.org/wiki/180th_meridian
ak_map <- map2 %>% filter(fips_state == "02", long < 100)
hi_map <- map2 %>% filter(fips_state == "15")


##################################################


# create maps

# create conus map
points_map_conus <- ggplot() + geom_map(data = conus_map, map = conus_map,
         aes(x = long, y = lat, map_id = id, group = group),
         fill = "white", color = "black", size = 0.15) + coord_equal()

# create ak map
points_map_ak <- ggplot() + geom_map(data = ak_map, map = ak_map,
                                        aes(x = long, y = lat, map_id = id, group = group),
                                        fill = "white", color = "black", size = 0.15) + coord_equal()

# create hi map
points_map_hi <- ggplot() + geom_map(data = hi_map, map = hi_map,
                                     aes(x = long, y = lat, map_id = id, group = group),
                                     fill = "white", color = "black", size = 0.15) + coord_equal()


##########################################################


# create discrete color scale 
display.brewer.all()
brewer.pal(9, "Blues")
discrete_blues <- brewer.pal(9, "Blues")[c(9, 5)]


######################################################


# add usda points

# add conus usda
points_map_conus <- points_map_conus + 
        geom_point(data = usda_data_conus_aea, aes(x = LONG, y = LAT, color = factor(DummyVar123)), 
                                alpha = 1, size = 1) + scale_color_manual(values = discrete_blues)

# add ak usda
points_map_ak <- points_map_ak + 
        geom_point(data = usda_data_ak_aea, aes(x = LONG, y = LAT, color = factor(DummyVar123)), 
                   alpha = 1, size = 1) + scale_color_manual(values = discrete_blues)

# add hi usda
points_map_hi <- points_map_hi + 
        geom_point(data = usda_data_hi_aea, aes(x = LONG, y = LAT, color = factor(DummyVar123)), 
                   alpha = 1, size = 1) + scale_color_manual(values = discrete_blues)


#####################################################


# style map

# style conus map
points_map_conus <- points_map_conus + theme_bw() +
        theme(plot.background = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), panel.border = element_blank(), 
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              plot.title = element_text(size = 12, face = "bold", hjust = .5), legend.position = "right",
              legend.key.size = unit(2, "mm"), legend.title = element_text(size = 7),
              legend.text = element_text(size = 7)) + labs(color = "DummyVar123") + ggtitle("USDA DummyVar123")

points_map_conus

# style ak map
points_map_ak <- points_map_ak + theme_bw() +
        theme(plot.background = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), panel.border = element_blank(), 
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              plot.title = element_text(size = 12, face = "bold", hjust = .5), legend.position = "right",
              legend.key.size = unit(2, "mm"), legend.title = element_text(size = 7),
              legend.text = element_text(size = 7)) + labs(color = "DummyVar123")

points_map_ak

# style hi map
points_map_hi <- points_map_hi + theme_bw() +
        theme(plot.background = element_blank(), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), panel.border = element_blank(), 
              axis.ticks.y = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank(),
              axis.ticks.x = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank(),
              plot.title = element_text(size = 12, face = "bold", hjust = .5), legend.position = "right",
              legend.key.size = unit(2, "mm"), legend.title = element_text(size = 7),
              legend.text = element_text(size = 7)) + labs(color = "DummyVar123")

points_map_hi


################################################


# save maps
setwd("C:/Users/Stephen/Desktop/R/mapping/usda_map")

# save conus map
ggsave(filename = "points_map_conus.pdf", plot = points_map_conus, width = 5, height = 5, 
       units = "in", dpi = 300)

# save ak map
ggsave(filename = "points_map_ak.pdf", plot = points_map_ak, width = 5, height = 5, 
       units = "in", dpi = 300)

# save hi map
ggsave(filename = "points_map_hi.pdf", plot = points_map_hi, width = 5, height = 5, 
       units = "in", dpi = 300)

