# Name: Lance Lowell
# Date: 2/24/25
# Purpose: COVID-19 cases & deaths by USA region

# Load necessary libraries
library(tidyverse)
library(lubridate)

# Read in the COVID-19 data from the NY Times GitHub repository
url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
covid_data <- read_csv(url)

# Create a state-region mapping data.frame
state_regions <- data.frame(
  state = state.name,
  abbreviation = state.abb,
  region = state.region
)

# Join the COVID-19 data with the state-region mapping
covid_data_joined <- covid_data %>%
  left_join(state_regions, by = c("state" = "state")) %>%
  filter(!is.na(region))  # Exclude rows where region is NA

# Group and summarize the data by region and date
regional_covid_data <- covid_data_joined %>%
  group_by(region, date) %>%
  summarise(
    Cases = sum(cases, na.rm = TRUE),
    Deaths = sum(deaths, na.rm = TRUE),
    .groups = "drop"
  )

# Pivot data to long format for easier plotting
regional_covid_long <- regional_covid_data %>%
  pivot_longer(
    cols = c(Cases, Deaths),
    names_to = "metric",
    values_to = "count"
  )

# Create the plot with separate panels for cases and deaths
plot_covid_regions <- ggplot(regional_covid_long, aes(x = ymd(date), y = count, color = metric)) +
  geom_line() +
  facet_grid(metric ~ region, scales = "free_y") +  # Cases and deaths are on separate rows per region
  labs(
    title = "Cumulative COVID-19 Cases and Deaths by Region",
    x = "Date",
    y = "Count",
    color = "Metric"
  ) +
  scale_color_manual(values = c("Cases" = "lightblue", "Deaths" = "pink")) +
  theme_dark(base_size = 14) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())  
print(plot_covid_regions)

# Save the plot
ggsave("cumulative_cases_deaths_by_region.png", plot = plot_covid_regions, width = 12, height = 8, dpi = 300)
