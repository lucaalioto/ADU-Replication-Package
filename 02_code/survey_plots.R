# survey_plots.R
# Generates all plots from survey.Rmd and saves to 04_figures/03_test
# Author: Luca Alioto Aldana
# Date: 2025-04-02

# Load libraries
library(tidyverse)
library(stringr)
library(dplyr)
library(readxl)
library(ggthemes)
library(scales)
library(here)

# Create output directory
dir.create("04_figures/03_test", recursive = TRUE, showWarnings = FALSE)

# Load data
adu_survey <- read_csv(here("01_data/04_survey/survey71.csv"))
scrape <- read_xlsx(here("05_affordability_scrape/scrape.xlsx"))

# Clean data
qualtrics_df <- adu_survey %>%
  slice(-c(1:2)) %>%
  dplyr::select(-c(1:8), -c(10:17))

# Helper function to wrap labels
wrap_labels <- function(labels, width = 50) {
  str_wrap(labels, width = width)
}

# ==============================================================================
# Q14 - ADU Location Plot
# ==============================================================================

adu_location <- qualtrics_df %>%
  filter(!is.na(Q14)) %>%
  group_by(Q14) %>%
  summarise(Count = n()) %>%
  mutate(percent = Count / sum(Count) * 100)

custom_colors <- c(
  "#609",
  "#4C93C3",
  "#99D594",
  "#FED08B",
  "#FC8D59",
  "#D7191C"
)
adu_location$Q14_wrapped <- wrap_labels(adu_location$Q14, width = 50)

adu_location_plot <- ggplot(
  adu_location,
  aes(x = reorder(Q14_wrapped, -percent), y = percent, fill = Q14_wrapped)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors, name = "ADU Location") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Where Was Your ADU Built?",
    x = NULL,
    y = "Percent of Responses (n = 61)"
  ) +
  theme_classic(base_size = 13, base_family = "serif") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15, face = "bold"),
    legend.key.height = unit(1.75, "cm")
  )

ggsave(
  "04_figures/03_test/adu_location_plot.png",
  plot = adu_location_plot,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)

# ==============================================================================
# Q16 - ADU Use Plots (Rental and Non-Rental)
# ==============================================================================

adu_use <- qualtrics_df %>%
  filter(!is.na(Q16)) %>%
  separate_rows(Q16, sep = "\\.,") %>%
  mutate(Q16 = str_trim(Q16)) %>%
  count(Q16, name = "Count") %>%
  arrange(desc(Count))

adu_use <- adu_use %>%
  mutate(Q16 = str_remove(Q16, "\\.$")) %>%
  mutate(Q16 = str_trim(Q16))

adu_use_grouped <- adu_use %>%
  group_by(Q16) %>%
  summarise(Count = sum(Count), .groups = "drop")

adu_use <- adu_use_grouped %>%
  mutate(
    Rental_Category = ifelse(str_detect(Q16, "Rent"), "Rental", "Non-Rental")
  )

adu_use$Q16_wrapped <- wrap_labels(adu_use$Q16, width = 50)

adu_rental <- adu_use %>% filter(Rental_Category == "Rental")
adu_non_rental <- adu_use %>% filter(Rental_Category == "Non-Rental")

adu_non_rental <- adu_non_rental %>%
  mutate(Percent = Count / sum(Count) * 100)

a <- c(
  "ADU is not rented",
  sum(adu_non_rental$Count),
  "All Non-Rental Uses",
  NA
)
adu_rental <- adu_rental %>%
  add_row(
    Q16 = a[1],
    Count = as.numeric(a[2]),
    Q16_wrapped = a[3],
    Rental_Category = a[4]
  ) %>%
  mutate(Percent = Count / sum(Count) * 100)

custom_colors <- c("#609FCA", "#99D594", "#FED08B", "#FC8D59", "#D7191C")

rental_plot <- ggplot(
  adu_rental,
  aes(x = reorder(Q16_wrapped, -Percent), y = Percent, fill = Q16_wrapped)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors, name = "Rental ADU Use") +
  scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
  labs(
    title = "How Are ADUs Used for Rentals?",
    x = NULL,
    y = "Percentage of Responses (n = 62)"
  ) +
  geom_text(
    aes(label = paste0("n = ", Count)),
    vjust = -0.5,
    size = 4,
    family = "serif"
  ) +
  theme_classic(base_size = 13, base_family = "serif") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15, face = "bold"),
    legend.key.height = unit(1.75, "cm")
  )

ggsave(
  "04_figures/03_test/rental_use_plot.png",
  plot = rental_plot,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)

custom_colors <- c(
  "#6BAED6",
  "#D7191C",
  "#99D594",
  "#50858bff",
  "#FED08B",
  "#FC8D59"
)

non_rental_plot <- ggplot(
  adu_non_rental,
  aes(x = reorder(Q16_wrapped, -Percent), y = Percent, fill = Q16_wrapped)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors, name = "Non-Rental ADU Use") +
  scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
  labs(
    title = "How Are ADUs Used for Non-Rental Purposes?",
    x = NULL,
    y = "Percentage of Responses (n = 25)"
  ) +
  geom_text(
    aes(label = paste0("n = ", Count)),
    vjust = -0.5,
    size = 4,
    family = "serif"
  ) +
  theme_classic(base_size = 13, base_family = "serif") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    legend.position = "right",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15, face = "bold"),
    legend.key.height = unit(1.75, "cm")
  )

ggsave(
  "04_figures/03_test/non_rental_use_plot.png",
  plot = non_rental_plot,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)

# ==============================================================================
# Q18 - ADU Rent Distribution Histogram
# ==============================================================================

adu_rent <- qualtrics_df %>%
  filter(!is.na(Q18_1)) %>%
  mutate(Q18 = as.numeric(Q18_1))

adu_med_rent <- median(adu_rent$Q18, na.rm = TRUE)
city_med_rent <- 1250

histogram_plot <- ggplot(adu_rent, aes(x = Q18)) +
  geom_histogram(
    aes(y = (..count..) / sum(..count..) * 100),
    binwidth = 250,
    fill = "#66CDAA",
    color = "black",
    alpha = 0.7
  ) +
  geom_vline(
    aes(xintercept = adu_med_rent, color = "Median ADU Rent (Survey)"),
    linetype = "dashed",
    size = 1
  ) +
  geom_vline(
    aes(xintercept = city_med_rent, color = "Median Sacramento Rent (2021)"),
    linetype = "dashed",
    size = 1
  ) +
  scale_color_manual(
    name = "Legend",
    values = c(
      "Median ADU Rent (Survey)" = "blue",
      "Median Sacramento Rent (2021)" = "red"
    )
  ) +
  scale_y_continuous(expand = c(0, 0), labels = function(x) paste0(x, "%")) +
  labs(
    title = "Distribution of ADU Monthly Rent Prices",
    x = "Monthly Rent ($)",
    y = "Percentage of ADUs (n = 29)"
  ) +
  theme_classic(base_size = 13, base_family = "serif") +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15, face = "bold"),
    legend.key.height = unit(1, "cm")
  )

ggsave(
  "04_figures/03_test/adu_rent_histogram_percent.png",
  plot = histogram_plot,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)

# ==============================================================================
# Mean ADU Rent by Number of Bedrooms
# ==============================================================================

# --- ADU Survey Data ---

exclude_zero_rent <- FALSE # May change this in the future. For now can turn on and off but may want both

# For now replacing other with studio.
# This seems to be the case but need to do some more investigating
adu_rent_bed <- qualtrics_df %>%
  filter(!is.na(Q18_1)) %>%
  mutate(
    rent = as.numeric(Q18_1),
    beds = ifelse(Q13 == "Other. Please specify:", "Studio", Q13),
    sqft = as.numeric(str_replace_all(str_extract(Q12, "[0-9,]+"), ",", ""))
  ) %>%
  select(ResponseId, Q1, rent, beds, sqft, Q13_4_TEXT, Q16)

bed_rent_summary_adu <- adu_rent_bed %>%
  filter(!is.na(beds), !is.na(rent)) %>%
  {
    if (exclude_zero_rent) dplyr::filter(., rent > 0) else .
  } %>%
  group_by(beds) %>%
  summarise(
    mean_rent = mean(rent, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(beds_f = factor(beds, levels = c("Studio", "1", "2", "3")))

p_mean_rent_beds_adu <- ggplot(
  bed_rent_summary_adu,
  aes(x = beds_f, y = mean_rent)
) +
  geom_col(fill = "#66CDAA", alpha = 0.9) +
  geom_text(
    aes(label = paste0("n = ", n)),
    vjust = -0.2,
    size = 4,
    color = "black"
  ) +
  scale_y_continuous(
    limits = c(0, 2500),
    expand = c(0, 0)
  ) +
  labs(
    title = "Mean ADU Rent by Number of Bedrooms",
    x = "Number of Bedrooms",
    y = paste0("Average Monthly Rent (n = ", sum(bed_rent_summary_adu$n), ")")
  ) +
  theme_classic(base_size = 13, base_family = "serif")

p_mean_rent_beds_adu

ggsave(
  "04_figures/03_test/mean_adu_rent_by_bedrooms.png",
  plot = p_mean_rent_beds_adu,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

# --- Web Scrape Data ---

scrape$beds <- factor(scrape$beds, levels = c("Studio", "1", "2"))

rent_summary_scrape <- scrape %>%
  group_by(beds) %>%
  summarise(
    mean_rent = mean(rent, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(beds_f = factor(beds, levels = c("Studio", "1", "2")))

p_mean_rent_beds_scrape <- ggplot(
  rent_summary_scrape,
  aes(x = beds_f, y = mean_rent)
) +
  geom_col(fill = "#66CDAA", alpha = 0.9) +
  geom_text(
    aes(label = paste0("n = ", n)),
    vjust = -0.2,
    size = 4,
    color = "black"
  ) +
  scale_y_continuous(
    limits = c(0, max(rent_summary_scrape$mean_rent) * 1.15),
    expand = c(0, 0)
  ) +
  labs(
    title = "Mean Rent by Number of Bedrooms (Web Scrape)",
    x = "Number of Bedrooms",
    y = paste0("Average Monthly Rent (n = ", sum(rent_summary_scrape$n), ")")
  ) +
  theme_classic(base_size = 13, base_family = "serif")

p_mean_rent_beds_scrape

ggsave(
  "04_figures/03_test/scrape_mean_rent_by_bedrooms.png",
  plot = p_mean_rent_beds_scrape,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

# --- Combined Data ---

scrape_clean <- scrape %>%
  select(rent, beds, sqft) %>%
  mutate(
    source = "Web Scrape",
    sqft = as.numeric(sqft)
  )

adu_clean <- adu_rent_bed %>%
  select(rent, beds, sqft) %>%
  mutate(source = "Survey Responses")

combined <- bind_rows(scrape_clean, adu_clean) %>%
  filter(!is.na(rent), !is.na(beds))

# --- Histogram ---

p_hist <- ggplot(combined, aes(x = rent)) +
  geom_histogram(fill = "#66CDAA", alpha = 0.9, bins = 20) +
  labs(
    title = "Distribution of Rent (Combined Data)",
    x = "Monthly Rent",
    y = "Count"
  ) +
  theme_classic(base_size = 13, base_family = "serif")

p_hist

ggsave(
  "04_figures/03_test/combined_rent_histogram.png",
  plot = p_hist,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

# --- Summary Table by Source ---

summary_by_source <- combined %>%
  group_by(source) %>%
  summarise(
    mean_rent = mean(rent, na.rm = TRUE),
    median_rent = median(rent, na.rm = TRUE),
    mean_sqft = mean(sqft, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  bind_rows(
    combined %>%
      summarise(
        source = "Combined",
        mean_rent = mean(rent, na.rm = TRUE),
        median_rent = median(rent, na.rm = TRUE),
        mean_sqft = mean(sqft, na.rm = TRUE),
        n = n()
      )
  )

gt_table_source <- summary_by_source %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Rent and Square Footage Summary",
    subtitle = "By Data Source"
  ) %>%
  gt::cols_label(
    source = "Source",
    mean_rent = "Mean Rent",
    median_rent = "Median Rent",
    mean_sqft = "Mean Sq Ft",
    n = "N"
  ) %>%
  gt::fmt_currency(
    columns = c(mean_rent, median_rent),
    currency = "USD",
    decimals = 0
  ) %>%
  gt::fmt_number(
    columns = mean_sqft,
    decimals = 0
  ) %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_body(rows = source == "Combined")
  )

gt_table_source

gt::gtsave(gt_table_source, "04_figures/03_test/rent_summary_by_source.png")

# --- Summary Table by Bedrooms ---

summary_by_beds <- combined %>%
  group_by(beds) %>%
  summarise(
    mean_rent = mean(rent, na.rm = TRUE),
    median_rent = median(rent, na.rm = TRUE),
    mean_sqft = mean(sqft, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(beds = factor(beds, levels = c("Studio", "1", "2", "3"))) %>%
  arrange(beds) %>%
  bind_rows(
    combined %>%
      summarise(
        beds = "Overall",
        mean_rent = mean(rent, na.rm = TRUE),
        median_rent = median(rent, na.rm = TRUE),
        mean_sqft = mean(sqft, na.rm = TRUE),
        n = n()
      )
  )

gt_table_beds <- summary_by_beds %>%
  gt::gt() %>%
  gt::tab_header(
    title = "Rent and Square Footage by Number of Bedrooms",
    subtitle = "Combined Data (2025)"
  ) %>%
  gt::cols_label(
    beds = "Bedrooms",
    mean_rent = "Mean Rent",
    median_rent = "Median Rent",
    mean_sqft = "Mean Sq Ft",
    n = "N"
  ) %>%
  gt::fmt_currency(
    columns = c(mean_rent, median_rent),
    currency = "USD",
    decimals = 0
  ) %>%
  gt::fmt_number(
    columns = mean_sqft,
    decimals = 0
  ) %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_body(rows = beds == "Overall")
  )

gt_table_beds

gt::gtsave(gt_table_beds, "04_figures/03_test/rent_sqft_by_bedrooms.png")

# --- Comparison Table: Source × Bedrooms ---

summary_comparison <- combined %>%
  group_by(source, beds) %>%
  summarise(
    mean_rent = mean(rent, na.rm = TRUE),
    mean_sqft = mean(sqft, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(beds = factor(beds, levels = c("Studio", "1", "2", "3"))) %>%
  arrange(source, beds)

gt_table_comparison <- summary_comparison %>%
  gt::gt(groupname_col = "source") %>%
  gt::tab_header(
    title = "Rent and Square Footage Comparison",
    subtitle = "By Data Source and Number of Bedrooms (2025)"
  ) %>%
  gt::cols_label(
    beds = "Bedrooms",
    mean_rent = "Mean Rent",
    mean_sqft = "Mean Sq Ft",
    n = "N"
  ) %>%
  gt::fmt_currency(
    columns = mean_rent,
    currency = "USD",
    decimals = 0
  ) %>%
  gt::fmt_number(
    columns = mean_sqft,
    decimals = 0
  )

gt_table_comparison

gt::gtsave(gt_table_comparison, "04_figures/03_test/rent_sqft_comparison.png")

# ==============================================================================
# Q9 - Off-Street Parking Spaces Histogram
# ==============================================================================

parking_spaces <- qualtrics_df %>%
  filter(!is.na(Q9_1)) %>%
  mutate(Q9 = as.numeric(Q9_1))

parking_histogram <- ggplot(parking_spaces, aes(x = Q9)) +
  geom_histogram(
    aes(y = ..count.. / sum(..count..)),
    binwidth = 1,
    fill = "#1B9E77",
    color = "black",
    alpha = 0.7
  ) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(
    expand = c(0, 0),
    labels = scales::percent_format(accuracy = 1)
  ) +
  labs(
    title = "Distribution of Off-Street Parking Spaces",
    x = "Number of Off-Street Parking Spaces",
    y = "Percent of Respondents (n = 64)"
  ) +
  theme_classic(base_size = 13, base_family = "serif")

ggsave(
  "04_figures/03_test/parking_histogram.png",
  plot = parking_histogram,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

# ==============================================================================
# Q15.1 - Net Parking Change Histogram
# ==============================================================================

parking_data <- qualtrics_df %>%
  filter(!is.na(`15.1_1`) | !is.na(`15.1_2`)) %>%
  mutate(
    `15.1_1` = ifelse(is.na(`15.1_1`), 0, `15.1_1`),
    `15.1_2` = ifelse(is.na(`15.1_2`), 0, `15.1_2`)
  ) %>%
  mutate(
    spaces_removed = as.numeric(`15.1_1`),
    spaces_added = as.numeric(`15.1_2`),
    net_change = spaces_added - spaces_removed
  ) %>%
  select(`15.1_1`, `15.1_2`, spaces_removed, spaces_added, net_change)

histogram_parking <- parking_data %>%
  filter(!is.na(net_change)) %>%
  ggplot(aes(x = net_change)) +
  geom_histogram(
    aes(y = (..count..) / sum(..count..) * 100),
    binwidth = 1,
    fill = "#E72",
    color = "black",
    alpha = 0.7,
    boundary = 0
  ) +
  scale_x_continuous(
    breaks = seq(
      floor(min(parking_data$net_change, na.rm = TRUE)),
      ceiling(max(parking_data$net_change, na.rm = TRUE)),
      1
    )
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  labs(
    title = "Distribution of Net Parking Spaces Added/Removed",
    x = "Net Change in Parking Spaces",
    y = "Percentage of ADUs (n = 15)"
  ) +
  theme_classic(base_size = 13, base_family = "serif")

ggsave(
  "04_figures/03_test/net_parking_histogram_percent.png",
  plot = histogram_parking,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

# ==============================================================================
# Q20 - Where ADU Tenants Park
# ==============================================================================

parking_summary <- qualtrics_df %>%
  dplyr::select(Q20_1, Q20_2, Q20_3) %>%
  filter(rowSums(is.na(.)) < ncol(.)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Parking_Location",
    values_to = "Frequency"
  ) %>%
  filter(!is.na(Frequency)) %>%
  group_by(Parking_Location, Frequency) %>%
  summarise(Count = n(), .groups = "drop")

parking_summary$Parking_Location <- recode(
  parking_summary$Parking_Location,
  "Q20_1" = "On the Street",
  "Q20_2" = "Driveway/Carport (Primary House)",
  "Q20_3" = "Driveway/Carport (ADU)"
)

custom_colors <- c(
  "Never" = "#FED08B",
  "Sometimes" = "#1B9E77",
  "Always" = "#6BAED6"
)

parking_plot <- ggplot(
  parking_summary,
  aes(x = Parking_Location, y = Count, fill = Frequency)
) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = custom_colors, name = "Parking Frequency") +
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  labs(
    title = "Where Do ADU Tenants Park Their Cars?",
    x = "Parking Location",
    y = "Proportion of Responses (n = 13)"
  ) +
  theme_classic(base_size = 13, base_family = "serif") +
  theme(
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    legend.key.height = unit(1.5, "cm")
  )

ggsave(
  "04_figures/03_test/adu_tenant_parking.png",
  plot = parking_plot,
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

# ==============================================================================
# Q27 - Car Ownership Histogram
# ==============================================================================

parking_spaces_q27 <- qualtrics_df %>%
  filter(!is.na(Q27_1)) %>%
  mutate(Q27 = as.numeric(Q27_1))

car_ownership_histogram <- ggplot(parking_spaces_q27, aes(x = Q27)) +
  geom_histogram(
    aes(y = (..count..) / sum(..count..) * 100),
    binwidth = 1,
    fill = "#1B9E77",
    color = "black",
    alpha = 0.7
  ) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  labs(
    title = "Number of Automobiles Currently Owned or Leased",
    x = "Number of Automobiles",
    y = "Percentage of Responses (n = 45)"
  ) +
  theme_classic(base_size = 12)

ggsave(
  "04_figures/03_test/car_ownership_histogram.png",
  plot = car_ownership_histogram,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

# ==============================================================================
# Q19_1 - ADU Tenant Car Ownership
# ==============================================================================

adu_tenant_car_plot <- qualtrics_df %>%
  filter(!is.na(Q19_1)) %>%
  ggplot(aes(x = as.factor(Q19_1))) +
  geom_bar(aes(y = (..count..) / sum(..count..) * 100), fill = "#3182bd") +
  labs(
    title = "Number of Automobiles Owned or Leased by ADU Tenants",
    x = "Number of Cars",
    y = "Percentage of Respondents (n = 27)"
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    labels = function(x) paste0(x, "%")
  ) +
  theme_classic(base_size = 12)

ggsave(
  "04_figures/03_test/adu_tenant_car_ownership.png",
  plot = adu_tenant_car_plot,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

# ==============================================================================
# Q29 - Travel Behavior Changes
# ==============================================================================

travel_cols <- c("Q29_3", "Q29_4", "Q29_5", "Q29_6", "Q29_7")
question_labels <- c(
  Q29_3 = "Use ridesharing",
  Q29_4 = "Take public transportation",
  Q29_5 = "Use shared micromobility",
  Q29_6 = "Bicycle",
  Q29_7 = "Walk"
)

custom_colors <- c(
  "Less" = "#FC8D59",
  "The Same" = "#FED08B",
  "More" = "#99D594"
)

travel_summary <- qualtrics_df %>%
  select(all_of(travel_cols)) %>%
  filter(rowSums(is.na(.)) < ncol(.)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "mode",
    values_to = "response"
  ) %>%
  filter(!is.na(response)) %>%
  mutate(
    mode = recode(mode, !!!question_labels),
    response = factor(response, levels = c("Less", "The Same", "More"))
  ) %>%
  group_by(mode, response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(mode) %>%
  mutate(Percent = Count / sum(Count) * 100) %>%
  ungroup()

travel_plot <- ggplot(
  travel_summary,
  aes(x = mode, y = Percent, fill = response)
) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = custom_colors, name = "Change") +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Changes in Travel Behavior After ADU Construction",
    x = "Mode of Travel",
    y = "Percent of Respondents (n = 40)"
  ) +
  theme_classic(base_size = 13, base_family = "serif") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggsave(
  "04_figures/03_test/travel_behavior_change.png",
  plot = travel_plot,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)

# ==============================================================================
# Q35 - Race/Ethnicity Distribution
# ==============================================================================

col_q <- "Q35"
df_q <- qualtrics_df %>% mutate(resp_id = row_number())

answered <- df_q %>%
  filter(!is.na(.data[[col_q]]), .data[[col_q]] != "")

split_q <- answered %>%
  separate_rows(!!sym(col_q), sep = "\\.,") %>%
  mutate(
    option = str_squish(str_remove(!!sym(col_q), "\\.$"))
  ) %>%
  filter(option != "")

n_respondents <- n_distinct(answered$resp_id)

q35_summary <- split_q %>%
  group_by(option) %>%
  summarise(
    n_selected = n_distinct(resp_id),
    selections = n(),
    .groups = "drop"
  ) %>%
  mutate(
    percent_resp = 100 * n_selected / n_respondents
  ) %>%
  arrange(desc(percent_resp)) %>%
  mutate(option = factor(option, levels = option))

race_plot <- ggplot(
  q35_summary,
  aes(x = option, y = percent_resp, fill = option)
) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distribution of Respondent Race/Ethnicity",
    x = NULL,
    y = "Percent of Respondents (n = 64)",
    fill = "Option"
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 13, base_family = "serif") +
  theme(
    legend.position = "right",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ggsave(
  "04_figures/03_test/ethnicity.png",
  plot = race_plot,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)

# ==============================================================================
# Q32 - Age Group Distribution
# ==============================================================================

df_long <- qualtrics_df %>%
  select(Q32_1:Q32_8) %>%
  filter(rowSums(is.na(.)) < ncol(.)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "age_group",
    values_to = "count"
  )

age_labels <- c(
  "Q32_1" = "<15",
  "Q32_2" = "15-24",
  "Q32_3" = "25-34",
  "Q32_4" = "35-44",
  "Q32_5" = "45-54",
  "Q32_6" = "55-64",
  "Q32_7" = "65-74",
  "Q32_8" = "75+"
)

df_long <- df_long %>%
  mutate(age_group = recode(age_group, !!!age_labels))

age_totals <- df_long %>%
  mutate(count = as.numeric(count)) %>%
  group_by(age_group) %>%
  summarise(total = sum(count, na.rm = TRUE)) %>%
  mutate(percent = total / sum(total) * 100)

age_group_plot <- ggplot(
  age_totals,
  aes(x = age_group, y = percent, fill = age_group)
) +
  geom_bar(stat = "identity") +
  labs(
    title = "Household Members by Age Group",
    x = "Age Group",
    y = "Percentage of Household Members (n = 62)"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  theme_classic(base_size = 13, base_family = "serif") +
  theme(legend.position = "none")

ggsave(
  "04_figures/03_test/age_group_distribution_percent.png",
  plot = age_group_plot,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)

# ==============================================================================
# Q34 - Education Distribution
# ==============================================================================

edu_labels <- c(
  "No formal education.",
  "High school diploma or equivalent.",
  "Associate's degree or technical school certificate(s).",
  "Four-year bachelor's degree(s).",
  "Graduate or professional degree(s)."
)

df_edu <- qualtrics_df %>%
  rename(Q34 = Q34...86) %>%
  mutate(Q34 = factor(Q34, levels = edu_labels))

edu_summary <- df_edu %>%
  filter(!is.na(Q34)) %>%
  group_by(Q34) %>%
  summarise(n = n()) %>%
  mutate(percent = n / sum(n) * 100)

edu_plot <- ggplot(edu_summary, aes(x = Q34, y = percent, fill = Q34)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Highest Level of Education Completed",
    x = NULL,
    y = "Percent of Respondents (n = 64)",
    fill = "Education Level"
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 13, base_family = "serif") +
  theme(
    legend.position = "right",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ggsave(
  "04_figures/03_test/education_distribution.png",
  plot = edu_plot,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)

# ==============================================================================
# Q33 - Income Distribution
# ==============================================================================

income_labels <- c(
  "<$10,000",
  "$10,000 - $24,999",
  "$25,000 - $49,999",
  "$50,000 - $74,999",
  "$75,000 - $99,999",
  "$100,000 - $124,999",
  "$125,000 - $149,999",
  "$150,000 - $174,999",
  "$175,000 - $199,999",
  "$200,000 or more"
)

df_income <- qualtrics_df %>%
  rename(Q33 = Q33...85) %>%
  mutate(Q33 = factor(Q33, levels = income_labels)) %>%
  select(ResponseId, Q33)

income_summary <- df_income %>%
  filter(!is.na(Q33)) %>%
  group_by(Q33) %>%
  summarise(n = n()) %>%
  mutate(percent = n / sum(n) * 100)

income_plot <- ggplot(income_summary, aes(x = Q33, y = percent, fill = Q33)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Household Income Distribution (Percent)",
    x = "Income Category",
    y = "Percent of Respondents (n = 62)",
    fill = "Income Level"
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic(base_size = 13, base_family = "serif") +
  theme(
    legend.position = "right",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

ggsave(
  "04_figures/03_test/income_distribution.png",
  plot = income_plot,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300
)
