library(dplyr)
library(leaflet)
library(RColorBrewer)
library(mapview)
library(geojsonio)
library(readr)
library(stringr)
library(ggmap)
library(viridis)
library(scales)
library(htmltools)
library(lawn)

# setwd
setwd("C:/Users/Stephen/Desktop/R/geocoding")

# read in data
list.files()
usda_data <- read_csv("DraftLatLonData.csv")

head(usda_data)
glimpse(usda_data)


###############################


# create map icon popup text
usda_data <- usda_data %>% mutate(popup_text = str_c("ID: ", usda_data$ID, "<br>", 
                         "Number of vacancies: ", usda_data$numVacancies, "<br>",
                         "Dummy variable: ", usda_data$DummyVar123, "<br>",
                         "Hard to fill?: ", usda_data$Hard_To_Fill_Location_, "<br>",
                         "Population: ", comma(usda_data$POP), "<br>",
                         usda_data$CITY, ", ", usda_data$STATECODE))

usda_data$popup_text[1]


#######################################


# create map icon label to display on hover
usda_data <- usda_data %>% mutate(usda_label = str_c(CITY, ", ", STATECODE))
usda_data$usda_label[1]


#####################################


# long coordinates seem to be missing negative sign
# test gadsden, AL in google maps (takes lat/lon)
usda_data %>% select(CITY, STATECODE, LONG, LAT)
geocode("Gadsden, AL")

# make LONG negative
usda_data <- usda_data %>% mutate(LONG = LONG * -1)


##########################################


# create color palette
usda_palette <- colorNumeric(palette = "viridis", domain = usda_data$POP)


############################################


# plot points
names(providers)
# view fitBounds
view(lawn_bbox_polygon(c(-170, 15, -55, 72)))

usda_map <- leaflet(usda_data) %>% addTiles() %>% 
        addCircleMarkers(lng = ~LONG, lat = ~LAT, popup = ~popup_text, label = ~usda_label,
                         color = ~usda_palette(POP)) %>%
        addProviderTiles(providers$OpenStreetMap) %>% 
        # setView(-115, 37.8, 3) %>%
        fitBounds(~min(LONG), ~min(LAT), ~max(LONG), ~max(LAT)) %>%
        addLegend("bottomright", pal = usda_palette, values = ~POP,
                title = "Population", labFormat = labelFormat(big.mark = ","), opacity = 1)

usda_map


##########################################


# save png image of map
mapshot(usda_map, file = "usda_map.png")










