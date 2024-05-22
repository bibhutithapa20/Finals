# Final Project: Electric Vehicle Data Analysis
## Contributors 
- Arnav
- Bibhuti

## Introduction 
This project analyzes electric vehicle (EV) data to provide insights into trends, geographical distribution, tax exemptions, and population growth over time. The Shiny application allows users to explore various aspects of the EV data through interactive visualizations.

## Data Dictionary
### Main Columns
- model.year: The year of the vehicle model.
- make: The manufacturer of the vehicle.
- model: The specific model of the vehicle.
- electric.vehicle.type: The type of electric vehicle (e.g., BEV, PHEV).
- electric.range: The electric range of the vehicle.
- city: The city where the vehicle is registered.
- county: The county where the vehicle is registered.

## Install Packages and Library
```
library(shiny)
library(bslib)
library(tidyverse)
library(dplyr)
library(DT)

```
## Data Loading and Preparation
### Loading the Data
The data is read from CSV files into R data frames. The setwd function sets the working directory where the data files are stored. The read.csv function reads the CSV files into data frames.
```
setwd('~/Documents/Data Final')
ev_df <- read.csv("Electric_Vehicle_Population_Data.csv")
othercars_df <- read.csv("Electric_Vehicle_Population_Size_History_By_County.csv")
tax_exemp <- read.csv("WA_Tax_Exemptions_-_Potential_Eligibility_by_Make_Model_Excluding_Vehicle_Price_Criteria.csv")

```
### Preprocessing the Data
Column names are converted to lowercase to maintain consistency and avoid case sensitivity issues during data manipulation.
```
colnames(ev_df) <- tolower(colnames(ev_df))
colnames(othercars_df) <- tolower(colnames(othercars_df))
colnames(tax_exemp) <- tolower(colnames(tax_exemp))

```
# Data Analysis 
## Filtering and Summarizing Data
### EV Data (2015-2023)
Filter data to include only vehicle models from 2015 to 2023.

```
ev_filter_2015 <- ev_df %>%
  filter(model.year > 2015 & model.year < 2024)

```
### Top 5 Brands 
dentify the top five non-Tesla EV manufacturers based on the number of vehicles.
```
top_five_brands <- ev_filter_2015 %>%
  filter(make != "TESLA") %>%
  count(make, sort = TRUE) %>%
  top_n(5, n) %>%
  pull(make)

```
Categorize brands into Tesla, top five other brands, and others.

```
ev_filter_2015 <- ev_filter_2015 %>%
  mutate(brand_category = ifelse(make == "TESLA", "Tesla",
                                 ifelse(make %in% top_five_brands, "Top Five Other Brands", "Other")))


```
Summarize the number of vehicles by model year and brand category.
```
summary_data <- ev_filter_2015 %>%
  filter(brand_category %in% c("Tesla", "Top Five Other Brands")) %>%
  group_by(model.year, brand_category) %>%
  summarise(count = n()) %>%
  ungroup()
```
Create a pivot table for visualization.
```
pivot_ev_other <- summary_data %>%
  pivot_wider(names_from = brand_category, values_from = count, values_fill = list(count = 0))
```
### City Summary
Summarize the total count of EVs by city.
```
city_summary <- ev_df %>%
  group_by(city) %>%
  summarise(total_ev_count = n()) %>%
  arrange(desc(total_ev_count))

top_10_cities_summary <- city_summary %>%
  top_n(10, total_ev_count)
```
### County Summary
Summarize EV data by county and vehicle type.

```
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

```
### Tax Exemption Analysis
Clean the tax exemption data and join it with the EV data to analyze vehicles with and without tax exemptions.
```
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

```
### Electric Vehicle Population Over Time
Analyze the EV population over time using a linear regression model and make future projections.
```
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

```
### Shiny App UI
## User Interface Layout
The user interface layout of the Shiny application, including a title panel, sidebar layout, and main panel with tabs for different visualizations.

```
ui <- fluidPage(
  theme = bs_theme(bootswatch = "sandstone"),
  tags$head(
    tags$style(HTML("
      .custom-title-bar {
        background-color: #006400;
        color: white;
        padding: 30px; 
        font-size: 36px; 
        text-align: center;
        font-weight: bold;
      }
      .custom-sidebar {
        background-color: #e0d8c3;
        padding: 15px;
      }
      .custom-sidebar h3 {
        font-weight: bold;
      }
    "))
  ),
  div(class = "custom-title-bar", "Exploration Of Electric Vehicle Data"),
  sidebarLayout(
    sidebarPanel(
      class = "custom-sidebar",
      width = 3,
      h3("Summary Statistics"),
      h4("Total Tesla Count"),
      tableOutput("teslaCount"),
      h4("Top 5 EV Manufacturers (Besides Tesla)"),
      tableOutput("top5Manufacturers"),
      h4("Total BEV Count"),
      tableOutput("bevCount"),
      h4("Total PHEV Count"),
      tableOutput("phevCount")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Introduction",
                 fluidRow(
                   column(12,
                          h3("Introduction"),
                          p("This dashboard provides an analysis of electric vehicle data, 
                            including trends over time, geographical distribution, and tax exemption statuses. 
                            Use the tabs to navigate through different visualizations and summaries."),
                          h4("Datasets Overview"),
                          p("The analysis is based on three main datasets:"),
                          h5("1. Electric Vehicle Population Data (ev_df)"),
                          p("This dataset contains information on the population of electric vehicles, including their make, model, year, and type."),
                          dataTableOutput("evDfSnippet"),
                          h5("2. Electric Vehicle Population Size History By County (othercars_df)"),
                          p("This dataset provides historical data on the population size of electric vehicles by county."),
                          dataTableOutput("othercarsDfSnippet"),
                          h5("3. WA Tax Exemptions Potential Eligibility by Make Model (tax_exemp)"),
                          p("This dataset contains information on potential eligibility for tax exemptions by vehicle make and model."),
                          dataTableOutput("taxExempSnippet")
                   )
                 )
        ),
        tabPanel("Tesla Vs Other brands",
                 fluidRow(
                   column(12, 
                          h3("EV Count by Year"),
                          p("This tab shows a plot of electric vehicle counts by year, differentiating between Tesla and the top five other brands."),
                          plotOutput("evPlot")
                   )
                 )
        ),
        tabPanel("City Summary",
                 fluidRow(
                   column(12,
                          h3("Top 10 Cities"),
                          p("This tab displays a bar plot of the top 10 cities by electric vehicle count."),
                          plotOutput("topCitiesPlot"),
                          h3("Electric Vehicle Count by City"),
                          p("This tab provides a summary of the total count of electric vehicles by city, highlighting the top cities with the highest counts."),
                          dataTableOutput("cityTable")
                   )
                 )
        ),
        tabPanel("Tax Exemption Analysis",
                 fluidRow(
                   column(12,
                          h3("Tax Exemption Analysis"),
                          p("This tab analyzes the number of cars with and without tax exemption status over the years 2010 to 2023."),
                          plotOutput("taxExemptionPlot"),
                          h4("Tax Exemption Summary"),
                          tableOutput("taxExemptionTable")
                   )
                 )
        ),
        tabPanel("Electric Vehicle Population Over Time",
                 fluidRow(
                   column(12,
                          h3("Electric Vehicle Population Over Time"),
                          p("This tab presents a plot showing the historical and predicted population of electric vehicles over the years."),
                          plotOutput("evPopulationPlot")
                   )
                 )
        ),
        tabPanel("Research Paper",
                 fluidRow(
                   column(12,
                          h3("Research Paper: Comparison of Findings from Electric Vehicle Population Data and Tax Exemption Analysis"),
                          h4("Introduction"),
                          p("This research paper aims to compare the findings derived from an analysis of electric vehicle (EV) data and tax exemption analysis presented in a Shiny application with the insights from a working paper by the Congressional Budget Office (CBO) on modeling the demand for electric vehicles and the supply of charging stations. The analysis focuses on key aspects such as tax exemption impacts on EV adoption and trends in EV population over time."),
                          h4("Data Overview"),
                          p("The Shiny application utilizes three primary datasets:"),
                          tags$ul(
                            tags$li("Electric Vehicle Population Data (ev_df): Contains information on EVs, including make, model, year, and type."),
                            tags$li("Electric Vehicle Population Size History by County (othercars_df): Provides historical data on EV population size by county."),
                            tags$li("WA Tax Exemptions Potential Eligibility by Make Model (tax_exemp): Details potential eligibility for tax exemptions based on vehicle make and model.")
                          ),
                          p("The CBO working paper (2023-06) presents a simulation model analyzing the EV market and charging infrastructure, with projections considering federal subsidies and tax credits."),
                          h4("Analysis and Findings"),
                          h5("Tax Exemption Analysis"),
                          p("Shiny Application Findings:"),
                          p("The analysis cleaned and matched the EV population data with tax exemption eligibility data. Created a summary showing the number of EVs with and without tax exemptions from 2010 to 2023. Highlighted that a significant portion of EV models are eligible for tax exemptions, which likely influences the overall adoption rates."),
                          p("CBO Working Paper Insights:"),
                          p("The paper discusses the impact of federal tax credits on EV adoption. It predicts that tax credits, along with subsidies for charging infrastructure, will significantly boost EV sales. The working paper estimates that about 64% of EV sales will qualify for tax credits, with an additional 18% possibly benefiting through leasing options to claim the credits."),
                          p("Comparison:"),
                          p("Both analyses emphasize the importance of tax incentives in promoting EV adoption. The Shiny application provides a more localized view, focusing on Washington state's tax exemptions, while the CBO paper offers a broader federal perspective. Both agree that tax incentives are crucial in driving EV sales."),
                          h5("Electric Vehicle Population Over Time"),
                          p("Shiny Application Findings:"),
                          p("A linear regression model was used to analyze the EV population from 2010 to 2023. Projections indicated a steady increase in EV population, influenced by both increased model availability and tax incentives. The analysis presented historical data and future predictions, showing a clear upward trend."),
                          p("CBO Working Paper Insights:"),
                          p("The CBO's model projects EV sales and charger supply through 2050, considering federal policies. It highlights that federal tax credits and subsidies will accelerate EV adoption, with projections showing EVs could make up between 27% and 60% of new vehicle sales by 2032. The model also predicts the growth in charging infrastructure, which supports the increased adoption of EVs."),
                          p("Comparison:"),
                          p("Both analyses predict a significant increase in EV adoption over the next decade. The Shiny application's model is limited to a shorter timeframe and a specific geographic area, whereas the CBO provides a long-term, nationwide forecast. The Shiny application's findings align with the CBO's projections, reinforcing the trend of growing EV populations supported by policy measures."),
                          h4("Conclusion"),
                          p("The findings from the Shiny application and the CBO working paper collectively underscore the critical role of tax incentives and federal policies in driving the adoption of electric vehicles. Both analyses show that tax exemptions and credits are effective tools for encouraging EV purchases, and the expansion of the EV population is expected to continue robustly. While the Shiny application offers detailed insights specific to Washington state, the CBO working paper provides a comprehensive national outlook, predicting substantial growth in EV market share and charging infrastructure over the coming decades."),
                          h5("Citation"),
                          p("Austin, D. (2023). *The demand for electric vehicles and the supply of electric vehicle charging stations*. Congressional Budget Office. https://www.cbo.gov/system/files/2023-09/58964-EV.pdf")
                   )
                 )
        ),
        tabPanel("Scatter Plot Analysis",
                 fluidRow(
                   column(12,
                          h3("Scatter Plot: Model Year vs. Electric Range"),
                          plotOutput("scatterPlot"),
                          h4("Findings"),
                          p("The scatter plot shows the relationship between the model year and the electric range of the vehicles. 
                            It helps to identify trends in vehicle range improvements over time. 
                            We can see that newer models tend to have a higher electric range, indicating advancements in battery technology and efficiency.")
                   )
                 )
        )
      )
    )
  )
)

```
### Shiny App Server
## Server-Side Logic
The server-side logic of the Shiny application processes the data and generates the visualizations.
```
server <- function(input, output) {
  output$teslaCount <- renderTable({
    tesla_count <- ev_df %>%
      filter(make == "TESLA") %>%
      summarise(count = n())
    data.frame(Tesla_Count = tesla_count$count)
  })
  
  output$top5Manufacturers <- renderTable({
    top_five_manufacturers <- c("CHEVROLET", "NISSAN", "FORD", "KIA", "BMW")
    data.frame(Manufacturers = top_five_manufacturers)
  })
  
  output$bevCount <- renderTable({
    bev_count <- ev_df %>%
      filter(electric.vehicle.type == "Battery Electric Vehicle (BEV)") %>%
      summarise(count = n())
    data.frame(BEV_Count = bev_count$count)
  })
  
  output$phevCount <- renderTable({
    phev_count <- ev_df %>%
      filter(electric.vehicle.type == "Plug-in Hybrid Electric Vehicle (PHEV)") %>%
      summarise(count = n())
    data.frame(PHEV_Count = phev_count$count)
  })
  
  output$evDfSnippet <- renderDataTable({
    datatable(head(ev_df, 5), options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$othercarsDfSnippet <- renderDataTable({
    datatable(head(othercars_df, 5), options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$taxExempSnippet <- renderDataTable({
    datatable(head(tax_exemp, 5), options = list(pageLength = 5, scrollX = TRUE))
  })
  
  # Existing render functions
  output$evPlot <- renderPlot({
    ggplot(pivot_ev_other, aes(x = model.year)) +
      geom_line(aes(y = Tesla, color = "Tesla")) +
      geom_line(aes(y = `Top Five Other Brands`, color = "Top Five Other Brands")) +
      labs(title = "Electric Vehicle Counts by Year",
           x = "Year",
           y = "Count",
           color = "Brand Category") +
      theme_minimal()
  })
  
  output$cityTable <- renderDataTable({
    datatable(city_summary)
  })
  
  output$topCitiesPlot <- renderPlot({
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
  })
  
  output$taxExemptionPlot <- renderPlot({
    ggplot(filtered_data, aes(x = model.year, color = tax.exemption)) +
      geom_line(stat = "count", aes(group = tax.exemption)) +
      labs(x = "Year", y = "Number of Cars", color = "Tax Exemption") +
      ggtitle("Number of Cars with and without Tax Exemption (2010-2023)")
  })
  
  output$taxExemptionTable <- renderTable({
    tax_exempt_summary <- ev_df_with_tax_exemption %>%
      group_by(tax.exemption) %>%
      summarise(count = n())
    tax_exempt_summary
  })
  
  output$evPopulationPlot <- renderPlot({
    ggplot(predicted_data, aes(x = model.year, y = count, color = type)) +
      geom_line() +
      geom_point() +
      labs(title = "Electric Vehicles Population by Year",
           x = "Year",
           y = "Number of Electric Vehicles",
           color = "Data Type") +
      theme_minimal()
  })

  output$scatterPlot <- renderPlot({
    ggplot(ev_df, aes(x = model.year, y = electric.range)) +
      geom_point(alpha = 0.5) +
      labs(title = "Scatter Plot: Model Year vs. Electric Range",
           x = "Model Year",
           y = "Electric Range") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

```
# Conclusion
This Shiny application serves as a comprehensive tool for analyzing and visualizing electric vehicle (EV) data. The application facilitates an in-depth exploration of trends, geographical distribution, tax exemption statuses, and population growth over time, providing valuable insights for various stakeholders, including policymakers, automotive manufacturers, and consumers. Below are the key conclusions drawn from each aspect of the analysis:

### Tesla Vs Other Brands
The analysis of EV counts by year highlights the significant dominance of Tesla in the electric vehicle market. However, the inclusion of the top five other brands demonstrates the increasing competition and diversification in the EV market. The steady rise in the number of non-Tesla EVs indicates that consumers are becoming more open to alternatives, driven by technological advancements and expanding market options.

### City Summary
The city summary provides a clear picture of the urban areas with the highest concentration of electric vehicles. This information is crucial for local governments and urban planners to understand where the adoption of EVs is highest and where infrastructure investments, such as charging stations, are most needed. The top 10 cities with the highest EV counts reflect regions that are likely more environmentally conscious or have better support for EV adoption through policies and incentives.

### Tax Exemption Analysis
The tax exemption analysis underscores the importance of financial incentives in promoting the adoption of electric vehicles. The significant proportion of EVs that qualify for tax exemptions suggests that these incentives play a crucial role in consumer decisions to purchase electric vehicles. This finding aligns with broader economic theories that suggest tax incentives lower the effective cost of ownership, thereby increasing demand.

### Electric Vehicle Population Over Time
The linear regression model used to analyze the EV population from 2010 to 2023 provides valuable insights into the growth trajectory of electric vehicles. The model predicts a continued increase in the EV population, influenced by factors such as technological advancements, increased model availability, and supportive policies. The projected growth aligns with global trends towards sustainable transportation solutions and highlights the importance of continued support for the EV market.

### Scatter Plot Analysis: Model Year vs. Electric Range
The scatter plot showing the relationship between model year and electric range reveals a clear trend of improvement in EV battery technology over time. Newer models tend to have a higher electric range, reflecting advancements in battery efficiency and capacity. This trend is encouraging for the future of electric vehicles, as increased range addresses one of the primary concerns of potential EV buyers: range anxiety.

### Research Paper Comparison
The comparison with the Congressional Budget Office (CBO) working paper provides a broader context for understanding the impact of federal policies on EV adoption. Both the Shiny application and the CBO paper emphasize the critical role of tax incentives and federal subsidies in driving the adoption of electric vehicles. The alignment of findings between the localized analysis (Washington state) and the national projections reinforces the validity of the conclusions drawn.

## Link to our Kanban 
https://github.com/users/WhyIsArnav/projects/1

## Link to ShinyApp
https://whyisarnav.shinyapps.io/EV_app/















