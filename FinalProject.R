library(tidyverse)
library(tidytext)
library(dplyr)
library(shiny)
library(ggplot2)

setwd('~/Documents/DATA332/Final Project')
ev_df <- read.csv("Electric_Vehicle_Population_Data.csv")
othercars_df <- read.csv("Electric_Vehicle_Population_Size_History_By_County.csv")
tax_exemp <- read.csv("WA_Tax_Exemptions_-_Potential_Eligibility_by_Make_Model_Excluding_Vehicle_Price_Criteria.csv")

colnames(ev_df) <- tolower(colnames(ev_df))
colnames(othercars_df) <- tolower(colnames(othercars_df))
colnames(tax_exemp) <- tolower(colnames(tax_exemp))

#Analysis-----------------------------------------------------------------------

#1
ev_filter_2015 <- ev_df %>%
  filter(model.year > 2015 & model.year < 2024)

top_five_brands <- ev_filter_2015 %>%
  filter(make != "TESLA") %>%
  count(make, sort = TRUE) %>%
  top_n(5, n) %>%
  pull(make)

ev_filter_2015 <- ev_filter_2015 %>%
  mutate(brand_category = ifelse(make == "TESLA", "Tesla",
                                 ifelse(make %in% top_five_brands, "Top Five Other Brands", "Other")))

summary_data <- ev_filter_2015 %>%
  filter(brand_category %in% c("Tesla", "Top Five Other Brands")) %>%
  group_by(model.year, brand_category) %>%
  summarise(count = n()) %>%
  ungroup()

pivot_ev_other <- summary_data %>%
  pivot_wider(names_from = brand_category, values_from = count, values_fill = list(count = 0))

ggplot(pivot_ev_other, aes(x = model.year)) +
  geom_line(aes(y = Tesla, color = "Tesla")) +
  geom_line(aes(y = `Top Five Other Brands`, color = "Top Five Other Brands")) +
  labs(title = "Electric Vehicle Counts by Year",
       x = "Year",
       y = "Count",
       color = "Brand Category") +
  theme_minimal()


#2 
city_summary <- ev_df %>%
  group_by(city) %>%
  summarise(total_ev_count = n()) %>%
  arrange(desc(total_ev_count))

top_10_cities_summary <- city_summary %>%
  top_n(10, total_ev_count)

ggplot(top_10_cities_summary, aes(x = reorder(city, total_ev_count), y = total_ev_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Cities by Electric Vehicle Count",
       x = "City",
       y = "Total EV Count") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))


#3
ev_df_selected <- ev_df %>%
  select(county, make, model, model.year, electric.vehicle.type, electric.range)

top10_percentage_ev <- othercars_df %>%
  filter(total.vehicles > 10000) %>%
  group_by(county, state) %>%
  summarise(max_percent_electric = max(percent.electric.vehicles, na.rm = TRUE)) %>%
  arrange(desc(max_percent_electric)) %>%
  slice_head(n = 10) %>% 
  ungroup()

top10_percentage_ev <- data.frame(
  county = c("King", "San Juan", "Jefferson", "Snohomish", "Clark", "Kitsap", "Island", "Thurston", "Whatcom", "Pierce"),
  state = rep("WA", 10),
  max_percent_electric = c(6.67, 6.37, 3.95, 3.93, 3.30, 3.25, 3.03, 2.91, 2.83, 2.77)
)

joint_df3 <- top10_percentage_ev %>%
  left_join(ev_df_selected, by = "county")

ev_counts_all_counties <- joint_df3 %>%
  group_by(county, max_percent_electric, electric.vehicle.type) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count))

ggplot(joint_df3, aes(x = county, y = max_percent_electric, fill = electric.vehicle.type)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Max Percent Electric by County and Electric Vehicle Type",
       x = "County", y = "Max Percent Electric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::number_format(scale = 1e-10, suffix = "%"))


#4
tax_exemp_clean <- tax_exemp
tax_exemp_clean$vehicle.model.description <- gsub("\\s*\\([^\\)]+\\)", "", tax_exemp_clean$vehicle.model.description)
tax_exemp_clean$vehicle.model.description <- sub(",.*", "", tax_exemp_clean$vehicle.model.description)
tax_exemp_clean$vehicle.model.description <- sub(" .*$", "", tax_exemp_clean$vehicle.model.description)
tax_exemp_clean$vehicle.model.description <- tolower(tax_exemp_clean$vehicle.model.description)
colnames(tax_exemp_clean)[colnames(tax_exemp_clean) == "vehicle.model.description"] <- "model"

ev_df_clean <- ev_df
ev_df_clean$model <- sub(" .*$", "", ev_df_clean$model)
ev_df_clean$model <- tolower(ev_df_clean$model)
ev_df_clean <- ev_df_clean[, -c((ncol(ev_df_clean)-1):ncol(ev_df_clean))]
ev_df_clean <- ev_df_clean[, -c((ncol(ev_df_clean)-3):ncol(ev_df_clean))]

exemp_cars <- tax_exemp_clean %>%
  group_by(model, make) %>%
  summarise()

unique_models <- unique(ev_df_clean$model)
models_with_exemption <- unique_models[unique_models %in% tax_exemp_clean$model]

tax_exemption_status <- data.frame(
  model = unique_models,
  tax.exemption = ifelse(unique_models %in% tax_exemp_clean$model, "yes", "no")
)

pivot_table <- pivot_wider(tax_exemption_status, names_from = model, values_from = tax.exemption)

ev_df_with_tax_exemption <- ev_df_clean %>%
  left_join(tax_exemption_status, by = "model")

count_tax_exemp <- ev_df_with_tax_exemption %>%
  group_by(tax.exemption) %>%
  summarise(count = n())

filtered_data <- ev_df_with_tax_exemption %>%
  filter(model.year >= 2010 & model.year <= 2023)

ggplot(filtered_data, aes(x = model.year, color = tax.exemption)) +
  geom_line(stat = "count", aes(group = tax.exemption)) +
  labs(x = "Year", y = "Number of Cars", color = "Tax Exemption") +
  ggtitle("Number of Cars with and without Tax Exemption (2010-2023)")


#5 
ev_filtered <- ev_df %>%
  filter(model.year >= 2010 & model.year <= 2023)

ev_count_by_year <- ev_filtered %>%
  group_by(model.year) %>%
  summarise(count = n())

ggplot(ev_count_by_year, aes(x = model.year, y = count)) +
  geom_line() +
  geom_point() +
  labs(title = "Electric Vehicles Population by Year",
       x = "Year",
       y = "Number of Electric Vehicles") +
  theme_minimal()

model <- lm(count ~ model.year, data = ev_count_by_year)

future_years <- data.frame(model.year = c(2019, 2020, 2021, 2022, 2023, 2024, 2025))
predictions <- predict(model, newdata = future_years)

predicted_data <- rbind(
  data.frame(model.year = ev_count_by_year$model.year, count = ev_count_by_year$count, type = "Historical"),
  data.frame(model.year = future_years$model.year, count = predictions, type = "Predicted")
)

ggplot(predicted_data, aes(x = model.year, y = count, color = type)) +
  geom_line() +
  geom_point() +
  labs(title = "Electric Vehicles Population by Year",
       x = "Year",
       y = "Number of Electric Vehicles",
       color = "Data Type") +
  theme_minimal()


  



