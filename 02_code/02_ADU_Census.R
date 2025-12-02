## 

library(sf)
library(dplyr)
library(tidyverse)
library(tidycensus)
library(tigris)
library(raster)
library(tmap)
library(rmapshaper)
library(tidygeocoder)
library(flextable)
library(webshot)
library(spdep)
library(peacesciencer)
library(readr)
library(ggplot2)
library(texreg)
library(BSDA)
library(patchwork)

library(osmdata)
library(geosphere)
library(tidyverse)

# Read in data ------------------------------------------------------------

adu_primary_add = read_csv("01_data/01_ADU_census/Sac_Primary_Addresses.csv") # this has the ADU address and the primary home address 

sac_join_table = read_csv("01_data/01_ADU_census/Sac_SpatialJoin_Table.csv") # this has all the information on ADUs (from where)

housing_type = read_csv("01_data/01_ADU_census/ACSDP5Y2021.DP04-Data.csv") # census data for single occupancy homes

snap_data = read_csv("01_data/01_ADU_census/SNAP Retailer Location data.csv") # SNAP EBT data from 

parksshp <- st_read("01_data/01_ADU_census/01_parks/Parks.shp") # Shape file of parks in Sacramento from Sac Open Data Portal

overture_sac = read_csv("01_data/01_ADU_census/sacramento_full_dataset.csv") # Overture data for Sacramento

# Cleaning data ------------------------------------------------------------


  #### ADU DATA ####

#Cleaning Data (Adding Unique Object ID in line with sac_join_table)
primariy_add <- adu_primary_add %>%
  mutate(OID_ = row_number())

# Join the two data frames to put ADU info and their Primary Address info in one table
sac_ADUs <- sac_join_table %>%
  inner_join(primariy_add, by = "OID_") 

# Clean col names and select relevant variables
sac_ADUs = sac_ADUs %>%
  rename(Full = Match_addr, # ADU full 
         Street = StAddr,   # ADU street 
         State = RegionAbbr) %>%
  mutate(Longitude = X,
         Latitude = Y) %>%
  dplyr::select(c(OID_,Street,Primary_Address, City,State,Postal, Full, GEOID, X, Y, Longitude, Latitude))


  ##### PARKS + SNAP EBT DATA #####

# Adding geometry to SNAP data (now can be read as points in ArcGIS)
snap.sf <- st_as_sf(snap_data, coords = c("Longitude", "Latitude"),
                    crs = "+proj=longlat +datum=NAD83 +ellps=WGS84")

# cleaning park data to exclude non-greenspace items
parks.shp <- parksshp %>%
  dplyr::filter(DESCRIPTIO %in% c("PARK", "PARKWAY", "MARINA", "NATURE AREA")) %>%
  dplyr::select(PARK_ID, DESCRIPTIO, PARK, geometry)


  ### OVERTURE & OPEN STREET MAP DATA ###

# Look only at grocery stores and supermarkets
overture_grocery = overture_sac %>%
  dplyr::filter(category %in% c("grocery_store","supermarket")) %>%
  dplyr::select(id, name, address, category, X, Y)

overture_grocery_sf = st_as_sf(overture_grocery, coords = c("X", "Y"),
                           crs = "+proj=longlat +datum=NAD83 +ellps=WGS84")

# Adding geometry to SNAP data (now can be read as points in ArcGIS)
snap_sf  <- st_as_sf(snap_data, coords = c("Longitude", "Latitude"),
                    crs = "+proj=longlat +datum=NAD83 +ellps=WGS84")


# Function to find the nearest SNAP-accepting store (geospatial)
get_nearest_snap_store <- function(store, ref_df) {
  distances <- st_distance(store, ref_df)  # Compute distances
  nearest_index <- which.min(distances)  # Get index of nearest store
  
  if (length(nearest_index) == 0) {
    return(data.frame(
      nearest_snap_store = NA, 
      nearest_snap_address = NA,
      snap_distance_meters = NA,
      name_match_score = NA
    ))  # Handle missing matches
  }
  
  # Extract nearest store info
  nearest_store_name <- ref_df$`Store Name`[nearest_index]
  nearest_store_address <- ref_df$`Store Street Address`[nearest_index]
  distance_meters <- as.numeric(distances[nearest_index])
  
  # Perform fuzzy name matching (Jaro-Winkler distance)
  name_score <- stringdist::stringdist(store$name, nearest_store_name, method = "jw")
  
  return(data.frame(
    nearest_snap_store = nearest_store_name,  
    nearest_snap_address = nearest_store_address,
    snap_distance_meters = distance_meters,
    name_match_score = name_score
  ))
}

# Apply function to all rows in overture_grocery
nearest_snap_stores <- do.call(rbind, lapply(1:nrow(overture_grocery_sf), function(i) {
  get_nearest_snap_store(overture_grocery_sf[i, ], snap_sf)
}))

# Merge with overture_grocery dataset
overture_grocery <- cbind(overture_grocery, nearest_snap_stores)

# Define a name matching threshold (lower = better match)
name_match_threshold <- 0.2  # Adjust as needed

# Create a final match decision column
overture_grocery <- overture_grocery %>%
  mutate(
    final_match = ifelse(name_match_score < name_match_threshold, 
                         nearest_snap_store, 
                         ifelse(snap_distance_meters < 500, nearest_snap_store, NA)),
    check_match = final_match, 
    ebt = ifelse(!is.na(final_match), 1, 0)
  )

overture_grocery.sf = st_as_sf(overture_grocery, coords = c("X", "Y"),
                               crs = "+proj=longlat +datum=NAD83 +ellps=WGS84")

  ### CENSUS DATA ###

single_oc = housing_type %>%
  dplyr::slice(-1) %>%
  dplyr::select(c(GEO_ID, NAME, DP04_0007E))

# Standardize GEOID by removing the prefix (what country)
single_oc <- single_oc %>%
  mutate(GEOID = sub("1400000US", "", GEO_ID)) %>%
  dplyr::select(c(GEOID, DP04_0007E))


# Census Tract Data 2021 add 2024
v21 <- load_variables(2021, "acs5", cache = TRUE)

ca.tracts <- get_acs(geography = "tract", 
                     year = 2021,
                     variables = c(
                       # Total population
                       tpop = "B01003_001", 
                       # Total population by race
                       tpopr = "B03002_001", 
                       # Non-Hispanic white population
                       nhwhite = "B03002_003", 
                       # Non-Hispanic black population
                       nhblk = "B03002_004",
                       #American Indian and Alaska Native
                       nhai_an = "B03002_005",
                       #Asian population
                       nhasn = "B03002_006", 
                       #Native Hawaiian/Pacific Islander
                       nhnhopi = "B03002_007",
                       # Non-Hispanic Some Other Race population
                       nhother = "B03002_008",
                       # Hispanic or Latino population
                       hisp = "B03002_012",
                       
                       # Median household income
                       mehinc = "B19013_001",
                       # Owner-occupied housing units
                       own_occ = "B25003_002",
                       # Renter-occupied housing units
                       rent_occ = "B25003_003",
                       # Total occupied housing units
                       total_occ = "B25002_002",
                       # Average household size
                       avg_hh_size = "B25010_001",
                       # Median age
                       med_age = "B01002_001",
                       # Median gross rent
                       rent = "B25064_001",
                       
                       # Household auto ownership - This needs to be redone :/ 
                       # No vehicle available
                       no_veh = "B25044_003",
                       # 1 vehicle available
                       one_veh = "B25044_004",
                       # 2 vehicles available
                       two_veh = "B25044_005",
                       # 3 or more vehicles available
                       three_plus_veh = "B25044_006"),
                     state = "CA",
                     survey = "acs5",
                     output = "wide",
                     geometry = TRUE)


# Do the same with 2024

# Census Tract Data 2024
v24 <- load_variables(2024, "acs5", cache = TRUE)

ca.tracts <- get_acs(geography = "tract", 
                     year = 2024,
                     variables = c(
                       # Total population
                       tpop = "B01003_001", 
                       # Total population by race
                       tpopr = "B03002_001", 
                       # Non-Hispanic white population
                       nhwhite = "B03002_003", 
                       # Non-Hispanic black population
                       nhblk = "B03002_004",
                       #American Indian and Alaska Native
                       nhai_an = "B03002_005",
                       #Asian population
                       nhasn = "B03002_006", 
                       #Native Hawaiian/Pacific Islander
                       nhnhopi = "B03002_007",
                       # Non-Hispanic Some Other Race population
                       nhother = "B03002_008",
                       # Hispanic or Latino population
                       hisp = "B03002_012",
                       
                       # Median household income
                       mehinc = "B19013_001",
                       # Owner-occupied housing units
                       own_occ = "B25003_002",
                       # Renter-occupied housing units
                       rent_occ = "B25003_003",
                       # Total occupied housing units
                       total_occ = "B25002_002",
                       # Average household size
                       avg_hh_size = "B25010_001",
                       # Median age
                       med_age = "B01002_001",
                       # Median gross rent
                       rent = "B25064_001",
                       
                       # Household auto ownership - This needs to be redone :/ 
                       # No vehicle available
                       no_veh = "B25044_003",
                       # 1 vehicle available
                       one_veh = "B25044_004",
                       # 2 vehicles available
                       two_veh = "B25044_005",
                       # 3 or more vehicles available
                       three_plus_veh = "B25044_006"),
                     state = "CA",
                     survey = "acs5",
                     output = "wide",
                     geometry = TRUE)



#Cleaning the data to do race % and remove margin of error
ca.tracts <- ca.tracts %>%
  mutate(
    pnhwhite = nhwhiteE / tpoprE, 
    pnhblk = nhblkE / tpoprE, 
    pnhai_an = nhai_anE / tpoprE,
    pnhasn = nhasnE / tpoprE, 
    pnhnhopi = nhnhopiE / tpoprE,
    pnhother = nhotherE / tpoprE,
    phisp = hispE / tpoprE,
    own_occ = own_occE,
    rent_occ = rent_occE,
    avg_hh_size = avg_hh_sizeE,
    med_age = med_ageE,
    med_rent = rentE,
    total_occ = total_occE,
    no_veh = no_vehE,
    one_veh = one_vehE,
    two_veh = two_vehE,
    three_plus_veh = three_plus_vehE) %>%
  rename(tpop = tpopE, mhinc = mehincE) %>%
  dplyr::select(GEOID, NAME, tpop, pnhwhite, pnhblk, pnhai_an,
                pnhasn, pnhnhopi, pnhother, phisp,
                mhinc, own_occ, rent_occ, total_occ,
                avg_hh_size, med_age, med_rent, no_veh, one_veh, two_veh, three_plus_veh)


#Loading California tracts
pl <- places(state = "CA", cb = TRUE)

#Bring in City boundary 
sac <- pl %>%
  filter(NAME == "Sacramento")

#joining that Location data to data
sac.tracts <- ms_clip(target = ca.tracts, 
                      clip = sac, 
                      remove_slivers = TRUE)

# Joining the rest of the census data (single occupancy) to the tract data
sac.tracts <- sac.tracts %>%
  inner_join(single_oc, by = "GEOID")

#make sure that it is an integer not char
sac.tracts$DP04_0007E <- as.integer(sac.tracts$DP04_0007E)

# Perform the join of census data with ADU data
sac_ADUs <- sac_ADUs %>%
  left_join(sac.tracts, by = "GEOID")

#turning csv into sf (giving geometry) using Lat Long from Sac Spatial Join
sacADU.sf <- st_as_sf(sac_ADUs, coords = c("X", "Y"),
                      crs = "+proj=longlat +datum=NAD83 +ellps=WGS84")

# Counting the number of ADUs pertract
adu_agg <- aggregate(sacADU.sf["OID_"], 
                     sac.tracts, 
                     FUN = "length")

adu_agg <- adu_agg %>%
  mutate(OID_ = replace_na(OID_,0))

#Putting info with Sac Tract Data 
sac.tracts <- sac.tracts %>%
  mutate(ADUs = adu_agg$OID_)

# Export Data ----------------------------------------------------

st_write(sac.tracts, "sacTracts.shp", delete_layer = TRUE)
st_write(sacADU.sf, "sacADUs.shp", delete_layer = TRUE)

# Export CSVs
sac_tracts = st_drop_geometry(sac.tracts)
write_csv(sac_tracts, "sac_tracts.csv")

## sac_ADUs = st_drop_geometry(sac_ADUs)
## write_csv(sac_ADUs, "01_data/05_output/sac_ADUs.csv")

  #### SNAP & PARK DATA ####

st_write(snap.sf,"snap_data.shp", delete_layer = TRUE)
st_write(parks.shp, "parks_data.shp", delete_layer = TRUE)
st_write(overture_grocery_sf, "overture_grocery.shp", delete_layer = TRUE)
st_write(overture_grocery.sf, "overture_grocery_snap.shp", delete_layer = TRUE)


