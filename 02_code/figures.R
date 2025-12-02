## This code uses the ADU data from Sac County and creates the figures for the paper. 
## Here the data is trimmed down to the ADUs without duplicates

# Load libraries
library(tidyverse)
library(dplyr)
library(readxl)
library(patchwork)

# Load data

setwd("C:/Users/lucaa/OneDrive/NCST Project/01_ReplecationPackage/ADU Replication Package/")

sac_adu435 = readRDS("01_data/05_output/master435_adu.rds") # with duplicates
sac_adu = readRDS("01_data/05_output/master408_adu_unique.rds") # without duplicates (and survey data)
sac_tracts = read_csv("01_data/03_figures/sac_tracts.csv")


# Creating two separate df for tract vs no tract

Tracts_without_ADUS <- sac_tracts %>%
  filter(sac_tracts$ADUs == 0)

Tracts_with_ADUS <- sac_tracts %>%
  filter(sac_tracts$ADUs >= 1)

  ## MEDIAN HOUSEHOLD INCOME ##

MHINC_summary_data <- sac_tracts %>%
  group_by(ADUs) %>%
  summarize(mean_mhinc = mean(mhinc, na.rm = TRUE))

# mean with 0 adus
MHINC_summary_data[1,2]

# mean with

# Bar plot 
MHINC_bar_plot <- ggplot(data = MHINC_summary_data, aes(x = as.factor(ADUs), y = mean_mhinc)) +
  geom_bar(stat = "identity", fill = "#66CDAA") +
  scale_y_continuous(labels = scales::dollar_format(),expand = c(0, 0)) +
  labs(title = "Bar Plot of Average Median Household Income vs. Number of ADUs",
       x = "Number of ADUs",
       y = "Average Median Household Income") +
  theme_classic(base_size = 13, base_family = "serif")

MHINC_bar_plot

ggsave("04_figures/02_manuscript/mhinc_bar_plot.png",
       plot = MHINC_bar_plot,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)

  ## MEDIAN RENT ##

# Summarize data for bar and line plots
RENT_summary_data <- sac_tracts %>%
  group_by(ADUs) %>%
  summarize(mean_rent = mean(med_rent, na.rm = TRUE))

# Bar plot
RENT_bar_plot <- ggplot(data = RENT_summary_data, aes(x = as.factor(ADUs), y = mean_rent)) +
  geom_bar(stat = "identity", fill = "#66CDAA") +  # Set fill color to aquamarine
  scale_y_continuous(labels = scales::dollar_format(), expand = c(0, 0)) +  # Format y-axis as dollar amounts
  labs(title = "Bar Plot of Average Median Rent vs. Number of ADUs",
       x = "Number of ADUs",
       y = "Average Median Rent") +
  theme_classic(base_size = 13, base_family = "serif")

print(RENT_bar_plot)

ggsave("04_figures/02_manuscript/rent_bar_plot.png",
       plot = RENT_bar_plot,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)

# Census tract rent of respondents 
resp_rent = sac_adu %>%
  filter(!is.na(Q18_1)) %>%  # Remove missing responses
  mutate(Q18 = as.numeric(Q18_1))

# Calculate the average rent paid by respondents in each tract
mean(resp_rent$med_rent, na.rm = TRUE)

# aggregate to tract level median rent
resp_rent2 = resp_rent %>%
  group_by(GEOID) %>%
  summarize(med_rent = first(med_rent))  # Assuming med_rent is the same for all ADUs in the same tract

mean(resp_rent2$med_rent, na.rm = TRUE)

# Create the kernel density plot for ADU weighted data

density_plot3 <- ggplot() +
  geom_density(data = resp_rent2, aes(x = med_rent, fill = "Tracts with ADU respondents"), alpha = 0.5) +
  labs(title = "Tracts with ADUs",
       x = "Median Rent (Tract Level)",
       y = "Kernel Density",
       fill = "Tracts with ADU respondents") +
  scale_fill_manual(values = c("Tracts with ADU respondents" = "#ADD8E6")) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 13, base_family = "serif")

density_plot4 <- ggplot() +
  geom_density(data = resp_rent, aes(x = med_rent, fill = "ADU weighted - respondents Only"), alpha = 0.5) +
  labs(title = "ADU Rent Data",
       x = "Median Rent (Tract Level)",
       y = "Kernel Density",
       fill = "ADU weighted - respondents Only") +
  scale_fill_manual(values = c("ADU weighted - respondents Only" = "#66CDAA")) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 13, base_family = "serif")


combined_plot2 = density_plot3 / density_plot4

ggsave("04_figures/02_manuscript/rent_density_plot.png",
       plot = combined_plot2,
       width = 1700,
       height = 1260,
       units = "px",
       dpi = 192)

#Kernel Density

# Create the kernel density plot for tracts with and without ADUs
density_plot1 <- ggplot() +
  geom_density(data = Tracts_with_ADUS, aes(x = med_rent, fill = "With ADUs"), alpha = 0.5) +
  geom_density(data = Tracts_without_ADUS, aes(x = med_rent, fill = "Without ADUs"), alpha = 0.5) +
  labs(title = "Tracts With and Without ADUs",
       x = "Median Rent (Tract Level)",
       y = "Kernel Density",
       fill = "ADUs") +
  scale_fill_manual(values = c("With ADUs" = "#ADD8E6", "Without ADUs" = "#FFA07A")) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 13, base_family = "serif")

# Create the kernel density plot for ADU weighted data
density_plot2 <- ggplot() +
  geom_density(data = sac_adu, aes(x = med_rent, fill = "ADU weighted"), alpha = 0.5) +
  labs(title = "ADU Weighted Data",
       x = "Median Rent (Tract Level)",
       y = "Kernel Density",
       fill = "ADU Weighted") +
  scale_fill_manual(values = c("ADU weighted" = "#66CDAA")) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 13, base_family = "serif")

# Combine the two plots
combined_plot <- density_plot1 / density_plot2

# Print the combined plot
print(combined_plot)

  ## Car Ownership ## 

sac_tracts <- sac_tracts %>%
  dplyr::mutate(adus_present = ifelse(ADUs > 0, "With ADUs", "Without ADUs"))

# Reshape the merged_sac data to long format for easier plotting
long_data <- sac_tracts %>%
  dplyr::select(GEOID, adus_present, no_veh, one_veh, two_veh, three_plus_veh) %>%
  tidyr::pivot_longer(cols = c(no_veh, one_veh, two_veh, three_plus_veh),
                      names_to = "vehicle_count",
                      values_to = "count")

# Rename vehicle_count levels
long_data <- long_data %>%
  dplyr::mutate(vehicle_count = dplyr::recode(vehicle_count,
                                               no_veh = "0",
                                               one_veh = "1",
                                               two_veh = "2",
                                               three_plus_veh = "3+"))

# Reorder the levels of vehicle_count
long_data$vehicle_count <- factor(long_data$vehicle_count, 
                                  levels = c("0", "1", "2", "3+"))

# Calculate percentages for With and Without ADUs
long_data <- long_data %>%
  dplyr::group_by(adus_present, vehicle_count) %>%
  dplyr::summarize(total_count = sum(count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(adus_present) %>%
  dplyr::mutate(percentage = total_count / sum(total_count) * 100)

# Calculate weighted average percentages for ADU weighted using sac_adu
weighted_data <- sac_adu %>%
  dplyr::select(GEOID, no_veh, one_veh, two_veh, three_plus_veh) %>%
  dplyr::summarize(dplyr::across(c(no_veh, one_veh, two_veh, three_plus_veh), ~ sum(.x, na.rm = TRUE))) %>%
  tidyr::pivot_longer(cols = c(no_veh, one_veh, two_veh, three_plus_veh),
                      names_to = "vehicle_count",
                      values_to = "count") %>%
  dplyr::mutate(vehicle_count = dplyr::recode(vehicle_count,
                                               no_veh = "0",
                                               one_veh = "1",
                                               two_veh = "2",
                                               three_plus_veh = "3+")) %>%
  dplyr::mutate(adus_present = "ADU weighted",
                percentage = count / sum(count) * 100) %>%
  dplyr::select(adus_present, vehicle_count, percentage)

# Combine the weighted data with the long_data
combined_data <- dplyr::bind_rows(long_data, weighted_data)

# Define custom colors including the new "ADU weighted"
custom_colors <- c("With ADUs" = "#ADD8E6", "Without ADUs" = "#FFA07A", "ADU weighted" = "#66CDAA")

# Create the bar graph with percentages
bar_graph <- ggplot2::ggplot(data = combined_data, ggplot2::aes(x = vehicle_count, y = percentage, fill = adus_present)) +
  ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge(width = 0.9)) +
  ggplot2::scale_fill_manual(values = custom_colors) +
  ggplot2::labs(title = "Car Ownership in Tracts With and Without ADUs",
                x = "Vehicle Count",
                y = NULL,
                fill = "ADUs Presence") +
  ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1), expand = c(0, 0)) +
  ggplot2::theme_classic(base_size = 13, base_family = "serif") +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

print(bar_graph)

ggsave("04_figures/02_manuscript/car_ownership_bar_plot.png",
       plot = bar_graph,
       width = 10,
       height = 6,
       units = "in",
       dpi = 300)


  ## Grocery Stores ##

ov5 = ggplot(sac_adu, aes(x = ov5)) +
  geom_bar(fill = "#609FCA") +
  labs(title = "Stores within 5 min",
       x = "Number of Grocery Stores",
       y = "Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 13, base_family = "serif")

ov10 = ggplot(sac_adu, aes(x = ov10)) +
  geom_bar(fill = "#4C93C3") +
  labs(title = "Stores within 10 min",
       x = "Number of Grocery Stores",
       y = "Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 13, base_family = "serif")

ov15 = ggplot(sac_adu, aes(x = ov15)) +
  geom_bar(fill = "#99D594") +
  labs(title = "Stores within 15 min",
       x = "Number of Grocery Stores",
       y = "Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 13, base_family = "serif")

ov20 = ggplot(sac_adu, aes(x = ov20)) +
  geom_bar(fill = "#FED08B") +
  labs(title = "Stores within 20 min",
       x = "Number of Grocery Stores",
       y = "Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 13, base_family = "serif")

ov25 = ggplot(sac_adu, aes(x = ov25)) +
  geom_bar(fill = "#FC8D59") +
  labs(title = "Stores within 25 min",
       x = "Number of Grocery Stores",
       y = "Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 13, base_family = "serif")

ov30 = ggplot(sac_adu, aes(x = ov30)) +
  geom_bar(fill = "#D7191C") +
  labs(title = "Stores within 30",
       x = "Number of Grocery Stores",
       y = "Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 13, base_family = "serif")

library(patchwork)

ov5 + ov10 + ov15 + ov20 + ov25 + ov30

# Define function to assign nearest grocery store dummy variables
assign_nearest_grocery <- function(df) {
  df <- df %>%
    mutate(
      nearest_grocery_5 = ifelse(ov5 > 0, 1, 0),
      nearest_grocery_10 = ifelse(ov10 > 0 & nearest_grocery_5 == 0, 1, 0),
      nearest_grocery_15 = ifelse(ov15 > 0 & nearest_grocery_5 == 0 & nearest_grocery_10 == 0, 1, 0),
      nearest_grocery_20 = ifelse(ov20 > 0 & nearest_grocery_5 == 0 & nearest_grocery_10 == 0 & nearest_grocery_15 == 0, 1, 0),
      nearest_grocery_25 = ifelse(ov25 > 0 & nearest_grocery_5 == 0 & nearest_grocery_10 == 0 & nearest_grocery_15 == 0 & nearest_grocery_20 == 0, 1, 0),
      nearest_grocery_30 = ifelse(ov30 > 0 & nearest_grocery_5 == 0 & nearest_grocery_10 == 0 & nearest_grocery_15 == 0 & nearest_grocery_20 == 0 & nearest_grocery_25 == 0, 1, 0),
      nearest_grocery_35 = ifelse(ov5 == 0 & ov10 == 0 & ov15 == 0 & ov20 == 0 & ov25 == 0 & ov30 == 0, 1, 0)  # New column for no grocery access
    )
  
  return(df)
}

# Apply the function
sac_adu <- assign_nearest_grocery(sac_adu)

# Reshape the data to long format for easy plotting
nearest_long <- sac_adu %>%
  dplyr::select(OID, starts_with("nearest_grocery_")) %>%
  pivot_longer(cols = starts_with("nearest_grocery_"), 
               names_to = "Time", values_to = "Nearest") %>%
  filter(Nearest == 1)  # Keep only the first non-zero value

# Convert time variable to numeric, then replace 35 with ">30"
nearest_long$Time <- gsub("nearest_grocery_", "", nearest_long$Time)
nearest_long$Time <- as.character(ifelse(nearest_long$Time == "35", ">30", nearest_long$Time))  # Convert 35 to ">30"

# Define custom colors based on the given scheme
custom_colors <- c("5" = "#609FCA",
                   "10" = "#4C93C3",
                   "15" = "#99D594",
                   "20" = "#FED08B",
                   "25" = "#FC8D59",
                   "30" = "#D7191C",
                   ">30" = "#8B0000")  # Dark Red for >30 min

# Ensure time labels are in correct order
nearest_long$Time <- factor(nearest_long$Time, levels = c("5", "10", "15", "20", "25", "30", ">30"))

# Plot histogram with legend
ggplot(nearest_long, aes(x = Time, fill = Time)) +
  geom_bar() +
  scale_fill_manual(values = custom_colors, name = "Nearest Grocery Time (min)") +
  labs(title = "Distribution of Nearest Grocery Store Travel Time",
       x = "Nearest Grocery Store Travel Time (minutes)",
       y = "Number of ADUs") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 13, base_family = "serif") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )


ggsave("04_figures/02_manuscript/grocery_bar_plot.png",
       plot = last_plot(),
       width = 8,
       height = 6,
       units = "in",
       dpi = 300)

# Numbers wrong
sum(sac_adu$nearest_grocery_5) # 83 
sum(sac_adu$nearest_grocery_10) # 112
sum(sac_adu$nearest_grocery_15) # 118 
sum20 = sum(sac_adu$nearest_grocery_20) 
sun25 = sum(sac_adu$nearest_grocery_25) 
sum30 = sum(sac_adu$nearest_grocery_30) 
sum35 = sum(sac_adu$nearest_grocery_35) 

desert = sum20 + sun25 + sum30 + sum35 # 103 
desert_con = sun25 + sum30 + sum35 # 26



  ## Parks && Stops ##

ggplot(sac_adu, aes(x = park0.4)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Parks within 0.25 miles of ADUs",
       x = "Number of AUDs",
       y = "Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 13, base_family = "serif")

sum(sac_adu$park0.4 == 0) # 103 ADUs with no parks within 0.25 miles = 247

ggsave("04_figures/02_manuscript/parks_bar_plot.png",
       plot = last_plot(),
       width = 8,
       height = 6,
       units = "in",
       dpi = 300)

ggplot(sac_adu, aes(x = stops0.4)) +
  geom_bar(fill = "#4B0082") +
  labs(title = "Transit Stops within 0.25 miles of ADUs",
       x = "Number of AUDs",
       y = "Frequency") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 13, base_family = "serif")

sum(sac_adu$stops0.4 == 0) # 35 ADUs with no transit stops within 0.25 miles = 97

ggsave("04_figures/02_manuscript/stops_bar_plot.png",
       plot = last_plot(),
       width = 8,
       height = 6,
       units = "in",
       dpi = 300)




