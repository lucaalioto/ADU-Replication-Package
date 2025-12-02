## This R script takes all the suvrey responses and joins them into the master spreadsheet of ADU data
## It also takes the duplicates and removes them from the final df.
## There is also an attempt at making a codebook for the master


library(tidyverse)
library(dplyr)
library(stringr)
library(dplyr)
library(readxl)
library(writexl)
library(labelled)

setwd("C:/Users/lucaa/OneDrive/NCST Project/01_ReplecationPackage/ADU Replication Package/")

# Read in data ------------------------------------------------------------

adu_survey = read_csv("01_data/04_survey/survey71.csv")

contact_download = read_xlsx("01_data/04_survey/respondent_contact_info.xlsx", 
                             sheet = "contactDownload")

links_337 = read_xlsx("01_data/04_survey/respondent_contact_info.xlsx", 
                      sheet = "links337")

sac_adu = read_xlsx("01_data/03_figures/Sac_ADUs_Export.xlsx") %>%
  dplyr::select(-FID)

dupe <- readxl::read_xlsx("01_data/04_survey/duplicate_info.xlsx") %>%
  dplyr::select(ShortLabel)

# Clean & Join Data  --------------------------------------------------------------

adu_survey = adu_survey %>%
  slice(-2) %>%
  dplyr::select(-c(1:8), -c(10:17))

# Extract the first row as a named vector of metadata
question_text <- as.character(adu_survey[1, ])
names(question_text) <- names(adu_survey)

adu_survey <- adu_survey[-1, ]

for (var in names(question_text)) {
  var_label(adu_survey[[var]]) <- question_text[[var]]
}

contact_download = contact_download %>%
  dplyr::select(`Response Id`, `Lookup Id`)

links_337 = left_join(links_337, 
                      contact_download, by = c("Lookup Id" = "Lookup Id")) %>%
          rename(ResponseId = `Response Id`)

adu_survey_address = left_join(links_337, 
                               adu_survey, by = c("ResponseId" = "ResponseId")) %>%
          filter(!is.na(`ResponseId`)) # Save this separate from master sheet 


adu_survey_address_join = adu_survey_address %>%
  dplyr::select(-c(1:6),-c(8:12))


sac_adu = left_join(sac_adu,
                     adu_survey_address_join, by = c("Street" = "ADU_Address")) 

# Note this brings the survey responses up to 75 because of duplicates 

# View Duplicates 
survey_check = sac_adu %>%
  filter(!is.na(ResponseId))

duplicates_survey <- survey_check %>%
  filter(duplicated(ResponseId) | duplicated(ResponseId, fromLast = TRUE))

# Note here that only one person responded that they have multiple ADUs

sac_adu = sac_adu %>%
  mutate(of_concern = ifelse(sac_adu$Street %in% dupe$ShortLabel, 1, 0)) # Just means there is a duplicate 

## sac_adu_unique <- sac_adu[!duplicated(sac_adu$Street), ] moving this down to apply labels

# Clean Variable Names ------------------------------------------------------------------

names(sac_adu)

sac_adu = sac_adu %>%
  rename(
    `OID` = OID_,
    `ADU Street` = Street,
    `Primary Address` = Prmry_A,
    `single_fam` = DP04_00,
    `Census Tract` = NAME
  )

# Add variable labels (metadata)
var_label(sac_adu$OID) <- "ObjectID"
var_label(sac_adu$GEOID) <- "Census Tract ID"
var_label(sac_adu$`Census Tract`) <- "Census Tract Name"

var_label(sac_adu$tpop) <- "Total population"
var_label(sac_adu$pnhwhit) <- "Percent non-Hispanic white"
var_label(sac_adu$pnhblk) <- "Percent non-Hispanic Black"
var_label(sac_adu$pnhai_n) <- "Percent non-Hispanic American Indian/Alaska Native"
var_label(sac_adu$pnhasn) <- "Percent non-Hispanic Asian"
var_label(sac_adu$pnhnhop) <- "Percent non-Hispanic Native Hawaiian/Pacific Islander"
var_label(sac_adu$pnhothr) <- "Percent non-Hispanic Other Race"
var_label(sac_adu$phisp) <- "Percent Hispanic or Latino"

var_label(sac_adu$mhinc) <- "Median household income (in dollars)"
var_label(sac_adu$own_occ) <- "Number of owner-occupied housing units"
var_label(sac_adu$rent_cc) <- "Number of renter-occupied housing units"
var_label(sac_adu$totl_cc) <- "Total occupied housing units"
var_label(sac_adu$avg_hh_) <- "Average household size"
var_label(sac_adu$med_age) <- "Median age"
var_label(sac_adu$med_rnt) <- "Median gross rent (in dollars)"

var_label(sac_adu$no_veh) <- "Number of households with no vehicle"
var_label(sac_adu$one_veh) <- "Number of households with 1 vehicle"
var_label(sac_adu$two_veh) <- "Number of households with 2 vehicles"
var_label(sac_adu$thr_pl_) <- "Number of households with 3 or more vehicles"

var_label(sac_adu$single_fam) <- "Number of residents living in single-occupancy housing"

# Grocery store access (all stores)
var_label(sac_adu$ov5)     <- "Grocery stores within 0–5 min transit + walking distance"
var_label(sac_adu$ov10)    <- "Grocery stores within 5–10 min transit + walking distance"
var_label(sac_adu$ov15)    <- "Grocery stores within 10–15 min transit + walking distance"
var_label(sac_adu$ov20)    <- "Grocery stores within 15–20 min transit + walking distance"
var_label(sac_adu$ov25)    <- "Grocery stores within 20–25 min transit + walking distance"
var_label(sac_adu$ov30)    <- "Grocery stores within 25–30 min transit + walking distance"

# Grocery store access (EBT-accepting only)
var_label(sac_adu$ovebt5)  <- "EBT-accepting grocery stores within 0–5 min transit + walking distance"
var_label(sac_adu$ovebt10) <- "EBT-accepting grocery stores within 5–10 min transit + walking distance"
var_label(sac_adu$ovebt15) <- "EBT-accepting grocery stores within 10–15 min transit + walking distance"
var_label(sac_adu$ovebt20) <- "EBT-accepting grocery stores within 15–20 min transit + walking distance"
var_label(sac_adu$ovebt25) <- "EBT-accepting grocery stores within 20–25 min transit + walking distance"
var_label(sac_adu$ovebt30) <- "EBT-accepting grocery stores within 25–30 min transit + walking distance"

# Parks and transit stops
var_label(sac_adu$park0.4) <- "Parks within 0.4 km walking distance"
var_label(sac_adu$stops0.4) <- "Transit stops within 0.4 km walking distance"

var_label(sac_adu$ResponseId) <- "Survey Response ID"

# got lazy and didnt want to fix this but then chat fixed for me so wtvr
sac_adu <- sac_adu %>% 
  rename(
    three_plus_veh = thr_pl_,
    med_rent = med_rnt,
    total_occ = totl_cc,
    rent_occ = rent_cc,
    avg_hh_size = avg_hh_, 
    pnhwhite = pnhwhit,
    pnhai_an = pnhai_n,
    pnhnhopi = pnhnhop,
    pnhother = pnhothr
  )


# Desired column order (cleaned)
new_order <- c(
  "OID", "ADU Street", "Primary Address", "City", "State", "Postal", "Full", 
  "Longitd", "Latitud", "GEOID", "Census Tract", 
  "no_veh", "one_veh", "two_veh", "three_plus_veh", "mhinc", "med_rent", "total_occ", 
  "own_occ", "rent_occ", "single_fam", "avg_hh_size", "tpop", 
  "pnhwhite", "phisp", "pnhblk", "pnhasn", "pnhai_an", "pnhnhopi", "pnhother", "med_age",
  "ov5", "ov10", "ov15", "ov20", "ov25", "ov30", 
  "ovebt5", "ovebt10", "ovebt15", "ovebt20", "ovebt25", "ovebt30", 
  "park0.4", "stops0.4", 
  "ResponseId", "Q1", "Q2", "Q2_3_TEXT", "Q3", "Q4", "Q33...23", "Q33_1_TEXT", 
  "Q34...25", "Q6", "Q7", "Q7_3_TEXT", "Q8", "Q9_1", "Q5", "Q5_6_TEXT", 
  "Q10", "Q11", "Q12", "Q13", "Q13_4_TEXT", "Q14", "Q14_7_TEXT", "Q15", 
  "15.1_1", "15.1_2", "15.2", "Q16", "Q16_11_TEXT", 
  "Q17_1", "Q18_1", "Q19_1", "Q20_1", "Q20_2", "Q20_3", "Q20_4", "Q20_4_TEXT", 
  "Q21...54", "Q22...55", "Q22_8_TEXT", "Q23", "Q23_9_TEXT", 
  "Q21...59", "Q21.1_1", "Q21.1_2", "21.2", "Q22...63", "Q22_11_TEXT", 
  "Q26", "Q27_1", "Q28_1", "Q29_1", "Q29_2", "Q29_3", "Q29_4", "Q29_5", "Q29_6", "Q29_7", 
  "Q30", "Q31", 
  "Q32_1", "Q32_2", "Q32_3", "Q32_4", "Q32_5", "Q32_6", "Q32_7", "Q32_8", 
  "Q33...85", "Q34...86", "Q35", "Q35_7_TEXT", "Q36", "ContactID", "of_concern"
)

# Reorder dataframe
master435_sac_adu <- sac_adu[, new_order]

master413_sac_adu_unique <- sac_adu[!duplicated(sac_adu$`ADU Street`), ]

#Remove ADUs with sac address but outside of tracts. Only to create 408 df. No change in 435
master413_sac_adu_unique <- master413_sac_adu_unique %>%
  mutate(GEOID_flag = ifelse(is.na(GEOID), 0, 1))

# Create a new dataframe with only rows where GEOID_flag is 1
# 408 or 429 (408 for this analysis since we are using the unique ADU data) 
master408_adu_unique <- master413_sac_adu_unique %>%
  filter(GEOID_flag == 1)

# Now lets check to make sure we didnt lose any survey responses
survey_check2 = master408_adu_unique %>%
  filter(!is.na(ResponseId)) # Here we have 71 which is good. No less no more :) 

# Codebook ------------------------------------------------------------------

# Function to generate a codebook
generate_codebook <- function(df, df_name = "dataset") {
  variable_names <- names(df)
  variable_classes <- sapply(df, class)
  
  # Fix: convert NULL labels to NA
  variable_labels <- sapply(df, function(x) {
    lbl <- attr(x, "label")
    if (is.null(lbl)) NA else lbl
  })
  
  value_labels <- lapply(df, function(x) attr(x, "labels"))  # for labelled variables
  
  codebook <- tibble(
    Variable = variable_names,
    Label = variable_labels,
    Type = sapply(variable_classes, function(x) paste(x, collapse = ", "))
  ) %>%
    mutate(
      Values = sapply(value_labels, function(x) {
        if (is.null(x)) return(NA)
        paste(names(x), "=", x, collapse = "; ")
      })
    )
  
  return(codebook)
}

codebook <- generate_codebook(sac_adu, df_name = "sac_adu")

# Export Data ------------------------------------------------------------------


write_csv(master435_sac_adu, "01_data/05_output/master435_adu.csv")
write_csv(master408_adu_unique, "01_data/05_output/master408_adu_unique.csv")
write_csv(adu_survey_address, "01_data/05_output/survey71_address.csv")

saveRDS(master435_sac_adu, "01_data/05_output/master435_adu.rds") # Save as RDS for R users like me :)
saveRDS(master408_adu_unique, "01_data/05_output/master408_adu_unique.rds") # Save as RDS for R users
write_csv(codebook, "01_data/05_output/codebook_survey_questions.csv")

