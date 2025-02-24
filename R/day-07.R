# Name:Lance Lowell
# Date: 2/19/25
# Purpose: COVID data set visualization exercise

# Load stuff
library(tidyverse)

url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv"
covid_data <- read_csv(url)

glimpse(covid_data)

#Identify the six states with the most current cases
latest_date <- max(covid_data$date)
top_states <- covid_data %>%
  filter(date == latest_date) %>%  
  group_by(state) %>%
  summarise(total_cases = sum(cases), .groups = 'drop') %>% 
  arrange(desc(total_cases)) %>%
  slice_head(n = 6)%>%
  glimpse()
#"California", "Texas", "Florida", "New York", "Illinois", "Pennsylvana" <- different from example


#Filter the raw data to those 6 states
top_states <- latest_cases_by_state$state
filtered_data <- covid_data %>%
  filter(state %in% top_states) %>%
  group_by(state, date)%>%
  summarise(plswork = sum(cases), .groups = 'drop')%>%
  glimpse()
#IT works!

#Set up a ggplot
plot1 <- ggplot(filtered_data, aes(x = date, y = plswork, group = state)) +
  geom_smooth(aes(color = state), method = "loess", se = FALSE, span = 0.2) +
  facet_wrap(~ state, scales = "free_y", nrow = 6)  +
  labs(title = "Cumulative Case Counts: COVID-19 by Lance", 
       x = "Sate",
       y = "Cumulative Cases")
theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot1)

# Save the plot to a file
ggsave("smooth_cumulative_cases_top_states.png", plot, width = 10, height = 20, dpi = 300)


# Identify the total cases each day in the whole country
covid_data <- covid_data %>%
  mutate(date = as.Date(date))
total_cumulative_cases_by_date <- covid_data %>%
  group_by(date) %>%
  summarise(total_cumulative_cases = sum(cases), .groups = 'drop') %>%
  arrange(date)

#Set up a ggplot –> add layers –> add labels –> add a theme

plot2 <- ggplot(total_cumulative_cases_by_date, aes(x = date, y = total_cumulative_cases)) +
  geom_line(color = "blue") + 
  labs(title = "Cumulative COVID-19 Cases in the USA by Lance",
       x = "Date",
       y = "Total Cumulative Cases") +
  theme_minimal()

# Display the plot
print(plot2)

# Save the plot
ggsave("cumulative_cases_usa_filled.png")

