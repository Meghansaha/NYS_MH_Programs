#====NYS MH Data Processing #1====#

# Library load-in====
reqpackages <- c("readr","ggplot2", "tidyverse","ggmap","crosstalk","DT","leaflet","tidygeocoder") 
newpackages <- reqpackages[!(reqpackages %in% installed.packages()[,"Package"])]
if(length(newpackages)) install.packages(newpackages)
invisible(suppressPackageStartupMessages(lapply(reqpackages, require, character.only=T)))

# Initial data load in====
#Main goal is to map all data points with an address and display this information within a map and data explorer. "Skipping" columns that are irrelevant to that task.#

MHprograms <- read_csv("Data/Local_Mental_Health_Programs.csv", 
                    col_types = cols(`Row Created Date Time` = col_skip(), 
                                     `Sponsor Name` = col_skip(), `Sponsor Code` = col_skip(), 
                                     `Agency Code` = col_skip(), `Facility Name` = col_skip(), 
                                     `Facility Code` = col_skip(), `Program Code` = col_skip(), 
                                     `Program Address 2` = col_skip(), 
                                     `Operating Certificate Required?` = col_skip(), 
                                     `Program Tier` = col_skip(), `Operating Certificate Duration` = col_skip()))

# Data cleaning and carpentry====
#Recoding all NAs to "Not Reported" for easier front-end clarity.#
MHprograms[is.na(MHprograms)] = "Not Reported"

#The "location" Column includes complete addresses and geocoded data for some observations, but are obviously incorrect. All of the latitude and longitude points are identical. In order to map the data on to a leaflet map, an accurate estimate of latitude and longitude points will need to be acquired. Will proceed to use Free Nominatim and Census APIs in an attempt to get geodata. This has limitations as it may not be possible for every address to be geocoded through these means#

#Will create a new variable "Complete Address" that pastes together all location data in the data set. Dropping the location column as it won't be used.#
MHprograms <- MHprograms %>%
  mutate(`Complete Address` = paste(`Program Address 1`,`Program City`,`Program County`, `Program State`,"US",`Program Zip`)) %>%
  select(-Location)

#Storing the addresses for the geocoding function#
addresses <- MHprograms$`Complete Address`

#Creating an empty vector to store the geocoded data from Nominatim (OSM)#
adddata <- c()

# Add a check to compare to the master file.- Revisit Later??#

nominatim_osm <- function(address = NULL)
{
  if(suppressWarnings(is.null(address)))
    return(data.frame("NA"))
  tryCatch(
    adddata <- jsonlite::fromJSON(
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', address),
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame("NA"))
  )
  if(length(adddata) == 0) return(data.frame("NA"))
  return(data.frame(lon = as.numeric(adddata$lon), lat = as.numeric(adddata$lat)))
}

#Execution of the geocoding and coercing into a dataframe.#
adddata <- suppressWarnings(lapply(addresses, function(address) {
  #calling the nominatim OSM API
  api_output <- nominatim_osm(address)
  
#return a data frame with the input addresses.#
  return(data.frame(address = address, api_output))
}) %>%

  #stack the list outputs into data frame together.#
  bind_rows() %>% data.frame())
adddata <- adddata %>%
  select(-X.NA.) %>%
  mutate(lonlookup = address) %>%
  mutate(latlookup = address)

#Attempting a second sweep of geocoding for NAs.#
#Pulling out and isolating NAs.#
NAadddata <- adddata %>%
  filter(is.na(lat & lon))

#Second sweep of Geocoding for empty lats and lons# 
NAadddata  <- geo_census(as.vector(NAadddata$address))

#Merging found geocodes for NAs into the data set#
lonlookup <- setNames(as.character(NAadddata$long),NAadddata$address)
adddata$lonlookup <- as.character(lapply(adddata$lonlookup , function(i) lonlookup[i]))

latlookup <- setNames(as.character(NAadddata$lat),NAadddata$address)
adddata$latlookup <- as.character(lapply(adddata$latlookup , function(i) latlookup[i]))

#Filling in the NAs for lat and long that were found#
adddata <- adddata %>%
  mutate(lon = ifelse(is.na(lon),paste(lonlookup),adddata$lon)) %>%
  mutate(lat = ifelse(is.na(lat),paste(latlookup),adddata$lat)) %>%
  select(-c(latlookup,lonlookup))

#Placing geodata back into main set while removing duplicates, setting the column type for lat and long, and reorganizing the lat and long columns#
MHprograms <- left_join(MHprograms,adddata, by = c("Complete Address" = "address")) %>%
  distinct() %>%
  suppressWarnings(mutate(lat = lat)) %>%
  suppressWarnings(mutate(lon = lon)) %>%
  relocate(lat, .before = lon)

#Isolating points that could not be geocoded#
MHprograms_nogeo <- MHprograms %>%
  filter(suppressWarnings(is.na(as.numeric(lat) & as.numeric(lon))))

#Removing observations that do not have any geocodes associated with them - These will be the final points plotted on the map.#
MHprograms <- MHprograms %>% 
  filter(!is.na(as.numeric(lat) & as.numeric(lon)))

#Writing CSVs out into the data folder.#
write_csv(MHprograms,"data/MHprograms.csv")
write_csv(MHprograms_nogeo,"data/MHprograms_nogeo.csv")

