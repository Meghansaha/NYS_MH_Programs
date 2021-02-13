#====NYS MH Data Processing #2====#

# Library load-in====
reqpackages <- c("readr","ggplot2", "tidyverse","ggmap","crosstalk","DT","leaflet","tidygeocoder","leafem","here") 
newpackages <- reqpackages[!(reqpackages %in% installed.packages()[,"Package"])]
if(length(newpackages)) install.packages(newpackages)
invisible(suppressPackageStartupMessages(lapply(reqpackages, require, character.only=T)))

# Initial data load in====
MHprograms <- read_csv("Data/MHprograms.csv")

#We have alot of observations. This may slow down our example. Let's filter for locations only in NYC, Brooklyn (Kings County)===
MHprogramsNYC <- MHprograms %>%
  filter(`Program Region` == "New York City") %>%
  filter(`Program County` == "Kings") 

#Cleaning up the zip code column a bit by pulling the first 5 digits in the column for easier readability===
MHprogramsNYC <- MHprogramsNYC %>%
  mutate(`Program Zip` = substr(`Program Zip`,1,5))

#Cleaning up age categories==
MHprogramsNYC <- MHprogramsNYC %>%
  mutate(`Populations Served` = ifelse(`Populations Served` == "Children Adolescents","Children and Teens",
                                       ifelse(`Populations Served` == "Children Adolescents Adults","All Ages",
                                              ifelse(`Populations Served` == "Adolescents","Teens",
                                                     ifelse(`Populations Served` == "Adolescents Adults","Teens and Adults",
                                                            ifelse(`Populations Served` == "Children Adults","Children and Adults",`Populations Served`))))))
  
#...And writing the data frame out as an RDS object===
saveRDS(MHprogramsNYC, "Data/MHprogramsNYC.RDS")
