##Generate data and maps for USDOL Prisoner Reentry Grant (April 2017)

##Load packages
library(tigris)
library(stringr)
library(xlsx)
library(readxl)
library(leaflet)
library(maptools)
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(rgdal)

##Set working directory to Shapefile folder
setwd("~/R Projects/USDOL_ReentryProject/Shapefiles")

##Read in saved shapefiles
####Michigan cities
cities <- readOGR(dsn = "Michigan_Cities", 
                  layer = "tl_2010_26_place00")
####DPD precincts
precincts <- readOGR(dsn = "DPD Precincts", 
                     layer = "geo_export_7b552edd-0682-452c-b491-7e6a16b1c277")
####Detroit neighborhoods
neighborhoods <- readOGR(dsn = "Detroit Neighborhoods", 
                         layer = "geo_export_9b67ef6c-9a29-4277-87eb-e2d8eafc5186")

##Read in shapefiles from tigris package
####Southeast MI census tracts
tracts <- tracts(state="MI", county=c(99, 125, 163), cb=TRUE)
####Michigan counties
counties <- counties(state = "MI", cb = TRUE)
####ZCTA starting with "48"
zipcodes <- zctas(cb=TRUE, starts_with=c("48"))

##Set working directory to Raw Data folder
setwd("~/R Projects/USDOL_ReentryProject/")

##Read in raw data files
####ACS 2014 5-year poverty data by census tract
tractfile <- read_excel("Raw Data/Poverty_CensusTracts_492017.xlsx")
####UWSEM school partner names and geocoordinates
schoolfile <- read.csv("Raw Data/UWSEM School Coordinates.csv", header=TRUE)
####DPD 2015 crime incidents
crimefile <- read.csv("Raw Data/Crime_2015.csv", header=TRUE)


##Clean data and shapefiles
####Keep only SE Michigan counties in counties shapefile
counties <- counties[(counties$NAME == "Macomb" | 
                        counties$NAME == "Oakland" | 
                        counties$NAME == "Wayne"),]
####Remove non-violent crime data
crimefile <- crimefile[which(crimefile$CATEGORY == "AGGRAVATED ASSAULT" |
                               crimefile$CATEGORY == "HOMICIDE" |
                               crimefile$CATEGORY =="ROBBERY"),]
####Change neighborhood names in shapefile to match crime data
neighborhoods$name <- toupper(neighborhoods$name)
neighborhoods$name[neighborhoods$name=="BARTON-MCFARLAND"] <- 
  "BARTON MCFARLANE"
neighborhoods$name[neighborhoods$name=="SOUTHWEST DETROIT"] <- 
  "SOUTHWEST DETROIT / MEXICANTOWN"

##Merge data to shapefiles
####Merge ACS poverty rate data to census tract shapefile
merged <- geo_join(tracts, tractfile, "AFFGEOID", "AFFGEOID")
##Duplicate census tract shapefile and remove high poverty census tracts
mergedpoverty <- merged
mergedpoverty <- mergedpoverty[!(is.na(mergedpoverty$poverty)),]
mergedpoverty <- mergedpoverty[!(mergedpoverty$poverty > 30),]
####Merge crime data to census tract shapefile 
crimetract <- table(crimefile$CENSUSTRACT)
crimetract <- as.data.frame(crimetract)
crimetract$tract <-crimetract$Var1
mergedtract <- geo_join(merged, crimetract, "NAME", "tract")

##Calculate violent crime rate/50000 residents
mergedtract$crimerate <- (mergedtract$Freq/mergedtract$population)*50000

##Merge crime data to DPD precincts shapefile
crimeprecinct <- table(crimefile$PRECINCT)
crimeprecinct <- as.data.frame(crimeprecinct)
crimeprecinct$PRECINCT <-crimeprecinct$Var1
####Clean precinct names in crime data file to match shapefile
crimeprecinct$PRECINCT <- as.character(crimeprecinct$PRECINCT)
crimeprecinct$PRECINCT <- str_pad(crimeprecinct$PRECINCT, width=2,
                                  side="left", pad="0")
####Merge files
mergedprecinct <- geo_join(precincts, crimeprecinct, "name", "PRECINCT")

##Add population data to precinct shapefile - source: 2010 Census
####Create data frame with population data
population <- c("54499", "43701", "66398", "49435", "63701", "46342", 
                "86006", "74348", "56894", "56596", "75052")
precinct <- c("02", "03", "04", "05", "06", "07", "08", "09", "10", "11",
              "12")
precinctpop <- cbind(population, precinct)
precinctpop <- as.data.frame(precinctpop)
####Merge files
mergedprecinctcrime <- geo_join(mergedprecinct, precinctpop, "name", "precinct")
####Calculate violent crime rate/50000 residents for each precinct
mergedprecinctcrime$Freq <- as.numeric(mergedprecinctcrime$Freq)
mergedprecinctcrime$population <- 
  as.numeric(as.character(mergedprecinctcrime$population))
mergedprecinctcrime$crimerate <- 
  (mergedprecinctcrime$Freq/mergedprecinctcrime$population)*50000

##Create labels and popups
popupschool <-paste0(schoolfile$school)
zipcodelabel <- paste0(zipcodes$ZCTA5CE10)
popup1 <- paste0("Census Tract: ", merged$NAME, "<br>", 
                 "Population: ", merged$population, "<br>",
                 "Poverty Rate: ", round(merged$poverty, 2), "%")
popupcity <-paste0(cities$NAME00)
precinctlabel <- paste0("DPD Precinct ", precincts$name)
popuplabel <- paste0(neighborhoods$name)
popup2 <- paste0("Census Tract: ", mergedtract$NAME, "<br>",
                 "Population: ", mergedtract$population, "<br>",
                 "Violent Crimes: ", mergedtract$Freq, "<br>",
                 "Violent Crime Rate: ", round(mergedtract$crimerate,0))
popupprecinct <- paste0("Precinct ", mergedprecinctcrime$name, "<br>",
                        "Violent Crime Rate: ", 
                        round(mergedprecinctcrime$crimerate, 0))

##Create color palettes for crime and poverty rates
pal <- colorNumeric(palette="Reds",
                    domain = merged$poverty, 
                    na.color="transparent")
####Transform crime rates to sqrt(crimerate) to improve color range
qpal <- colorBin(palette="Reds",
                 domain = sqrt(mergedtract$crimerate), 
                 na.color="transparent", 
                 bins=8)
palprecinct <- colorBin("Reds", 
                        domain=mergedprecinctcrime$crimerate)

##Create leaflet map
map<-leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  setView(lng=-83.170268, lat=42.373568, zoom=10) %>%
  addPolygons(data=counties, group="Census Tract Poverty Rates", 
              fillColor="Gray", color="black", 
              fillOpacity = 0, weight=5) %>%
  addPolygons(data = merged, group="Census Tract Poverty Rates", 
              fillColor = ~pal(poverty), 
              weight=0.3, fillOpacity = 0.7, color="Gray",
              popup = popup1) %>%
  addPolygons(data=counties, group="Census Tract Violent Crime Rates", 
              fillColor="Gray", color="black", 
              fillOpacity = 0, weight=5) %>%
  addPolygons(data = mergedtract, group="Census Tract Violent Crime Rates", 
              fillColor = ~qpal(sqrt(crimerate)), 
              weight=0.3, fillOpacity = 0.7, color="Gray",
              popup = popup2) %>%
  addPolygons(data=counties, group="DPD Precinct Violent Crime Rates", 
              fillColor="Gray", color="black", 
              fillOpacity = 0, weight=5) %>%
  addPolygons(data = mergedprecinctcrime, 
              group="DPD Precinct Violent Crime Rates", 
              fillColor = ~palprecinct(crimerate), 
              weight=0.3, fillOpacity = 0.7, color="Gray",
              popup = popupprecinct) %>%
  addLayersControl(baseGroups = c("Census Tract Poverty Rates", 
                                  "Census Tract Violent Crime Rates",
                                  "DPD Precinct Violent Crime Rates"),
                   overlayGroups = c("City Borders", 
                                     "Zip Codes",
                                     "DPD Precincts",
                                     "Detroit Neighborhoods",
                                     "High Poverty Census Tracts Only",
                                     "Returning Residents Center",
                                     "UWSEM Schools"),
                   position="bottomleft",
                   options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addPolygons(data=cities, group="City Borders", fillColor="Gray",
              color="Black", fillOpacity=0, weight=2,
              label=popupcity) %>%
  addPolygons(data=zipcodes, group="Zip Codes", fillColor="Gray",
              color="Black", fillOpacity=0, weight=1, dashArray="3",
              label=zipcodelabel) %>%
  addPolygons(data=precincts, group="DPD Precincts", fillColor="Gray",
              color="Black", fillOpacity=0, weight=1.5,
              label=precinctlabel) %>%
  addPolygons(data=neighborhoods, group="Detroit Neighborhoods",
              fillColor="Gray", color="Black", fillOpacity=0, 
              weight=0.75, dashArray="3",
              label=popuplabel) %>%
  addPolygons(data=mergedpoverty, group="High Poverty Census Tracts Only",
              fillColor="Gray", color="Gray", fillOpacity=0.7, weight=1) %>%
  addMarkers(lng=-83.170268, lat=42.373568, group="Returning Residents Center",
             label="Returning Residents Resource Center") %>%
  addCircles(group="UWSEM Schools", lng=schoolfile$lng, lat=schoolfile$lat, 
             label=popupschool) %>%
  hideGroup("Zip Codes") %>%
  hideGroup("City Borders") %>%
  hideGroup("DPD Precincts") %>%
  hideGroup("Detroit Neighborhoods") %>%
  hideGroup("High Poverty Census Tracts Only") %>%
  hideGroup("UWSEM Schools")

##View map
map
