# ShinyApp to display and compare the 2022 population forecast

#install.packages("circlize")
library(circlize)
library(shiny)
library(ggplot2)
library(magrittr)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(data.table)
library(plotly)
library(DT)
if(!exists("Population_data")){
  Population_data <- readRDS("./Version EN/Population_data.RDS")
}
if(!exists("Births_data")){
  Births_data <- readRDS("./Version EN/Births_data.RDS")
}
if(!exists("Deaths_data")){
  Deaths_data <- readRDS("./Version EN/Deaths_data.RDS")
}
if(!exists("Immigration_data")){
  Immigration_data <- readRDS("./Version EN/Immigration_data.RDS")
}
if(!exists("Emigration_data")){
  Emigration_data <- readRDS("./Version EN/Emigration_data.RDS")
}
if(!exists("Internal_migration_data")){
  Internal_migration_data <- readRDS("./Version EN/Internal_migration_data.RDS")
}


# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Population Forecast"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      # Choose forecasted years
      sliderInput("forecast_years", "Choose forecast years:",
                  min = 2022, max = 2100, value = c(2022, 2080)),
      
      conditionalPanel( # separate for internal migration
        condition = "input.category == 'Migration between Provinces'",
        sliderInput("single_forecast_year", "Choose a Forecast Year for the Circle Plot:",
                    min = 2022, max = 2100, value = 2022)),

      # Choose Category
      selectInput("category", "Choose a Category:",
                  choices = c("Population", "Births", "Deaths", "Immigration", "Emigration", "Migration between Provinces"),
                  selected = "Population"),
      
      
      # Choose Variant
      checkboxGroupInput("variant", "Choose a Forecast Variant:",
                         choices = unique(Population_data$Variant), 
                         selected = unique(Population_data$Variant)[1]),
      
      # Choose Province
      conditionalPanel(
        condition = "input.category != 'Migration between Provinces'",
        selectInput("province", "Choose a Province", selectize = T,
                     choices = unique(Population_data$Province), 
                     selected = rev(unique(Population_data$Province))[1])),
      
      conditionalPanel( # Select Province for internal migration
        condition = "input.category == 'Migration between Provinces'",
        selectInput("province_of_origin", "Choose Province of Origin:", multiple = T, selectize = T,
                     choices = unique(Internal_migration_data$Province_Origin), 
                     selected = rev(unique(Internal_migration_data$Province_Origin))[1]),
        selectInput("province_of_destination", "Choose Province of Destination", multiple = T, selectize = T,
                    choices = unique(Internal_migration_data$Province_Origin), 
                    selected = unique(Internal_migration_data$Province_Origin)[3])),
      
      
      
      # Choose Sex
      conditionalPanel(
        condition = "input.category != 'Births'",
        radioButtons("sex", "Choose Sex:",
                     choices = unique(Population_data$Sex), 
                     selected = rev(unique(Population_data$Sex))[1]) 
      ),
      
      conditionalPanel(
        condition = "input.category == 'Births'",
        sliderInput("age_mother", "Choose the Age of the Mother:",
                     min = 10, max = 49, value = c(10, 49))),

      # Choose Age
      conditionalPanel(
        condition = "input.category != 'Births' & input.category != 'Migration between Provinces'",
        radioButtons("age_grouped", "Choose Age:",
                     choices = c("All Age groups together", "Specific Age Group"),
                     selected = "All Age groups together")
      ),
      
      conditionalPanel(
        condition = "input.age_grouped == 'Specific Age Group'",
        conditionalPanel(
          condition = "input.category != 'Births' & input.category != 'Migration between Provinces'",
          numericInput("age_lower_limit", "Choose lower Age Limit", 
                       min = 0, max = 100, value = 0),
          numericInput("age_upper_limit", "Choose upper Age Limit", 
                       min = 0, max = 100, value = 100) 
          
        )
      ),
      
      
      # Choose Country of birth
      conditionalPanel(
        condition = "input.category != 'Deaths' & input.category != 'Emigration' & input.category != 'Births'",
        radioButtons("country_of_birth", "Choose Country of Birth:",
                     choices = unique(Population_data$Country_of_Birth), 
                     selected = rev(unique(Population_data$Country_of_Birth))[1])
      ),
      conditionalPanel(
        condition = "input.category == 'Births'",
        radioButtons("country_of_birth", "Choose Mother's Country of Birth:",
                     choices = unique(Population_data$Country_of_Birth), 
                     selected = rev(unique(Population_data$Country_of_Birth))[1])
      )
      
      
      
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      plotlyOutput("plot"),
      
      conditionalPanel(
        condition = "input.category == 'Migration between Provinces'",
        plotOutput("int_migra_circle")
        ),
      
      DTOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  data_selected <- reactive({input$category})
  
  alter_selected <- reactive({input$age_grouped})
  
  data_table <- reactive({
    
    if(alter_selected() == "All Age groups together"){
      age_lower_limit = 0
      age_upper_limit = 100
    }
    if(alter_selected() == "Specific Age Group"){
      age_lower_limit = input$age_lower_limit
      age_upper_limit = input$age_upper_limit
    }
    
    
    if(data_selected() == "Population"){
      
      Population_data %>%
        filter(Variant %in% input$variant &
                 Year %in% c(input$forecast_years[1]:input$forecast_years[2]) &
                 Sex == input$sex &
                 Province %in% input$province &
                 Age >= age_lower_limit & Age <= age_upper_limit &
                 Country_of_Birth == input$country_of_birth) %>% 
        group_by(Year, Country_of_Birth, Province, Sex, Variant) %>% 
        summarise(Number = sum(Number))
      
    } else if(data_selected() == "Births"){
      
      Births_data %>% 
        filter(Variant %in% input$variant &
                 Year %in% c(input$forecast_years[1]:input$forecast_years[2]) &
                 Province %in% input$province &
                 Age >= input$age_mother[1] & Age <= input$age_mother[2] &
                 Country_of_Birth_Mother == input$country_of_birth) %>% 
        group_by(Year, Province, Country_of_Birth_Mother, Variant) %>% 
        summarise(Number = sum(Number))
      
    } else if(data_selected() == "Deaths"){
      
      Deaths_data %>% 
        filter(Variant %in% input$variant &
                 Year %in% c(input$forecast_years[1]:input$forecast_years[2]) &
                 Province %in% input$province &
                 Age >= age_lower_limit & Age <= age_upper_limit &
                 Sex == input$sex) %>% 
        group_by(Year, Province, Sex, Variant) %>% 
        summarise(Number = sum(Number))
      
    } else if(data_selected() == "Immigration"){
      
      Immigration_data %>% 
        filter(Variant %in% input$variant &
                 Year %in% c(input$forecast_years[1]:input$forecast_years[2]) &
                 Province %in% input$province &
                 Age >= age_lower_limit & Age <= age_upper_limit &
                 Sex == input$sex & 
                 Country_of_Birth == input$country_of_birth) %>% 
        group_by(Year, Province, Sex, Variant, Country_of_Birth) %>% 
        summarise(Number = sum(Number))
      
    } else if(data_selected() == "Emigration"){
      
      Emigration_data %>% 
        filter(Variant %in% input$variant &
                 Year %in% c(input$forecast_years[1]:input$forecast_years[2]) &
                 Province %in% input$province &
                 Age >= age_lower_limit & Age <= age_upper_limit &
                 Sex == input$sex) %>% 
        group_by(Year, Province, Sex, Variant) %>% 
        summarise(Number = sum(Number))
      
    } else if(data_selected() == "Migration between Provinces"){
      
      data_int_migra <- Internal_migration_data %>% 
        filter(Variant %in% input$variant &
                 Year %in% c(input$forecast_years[1]:input$forecast_years[2]) &
                 Sex == input$sex & 
                 Country_of_Birth == input$country_of_birth &
                 Province_Origin %in% input$province_of_origin & 
                 Province_Destination %in% input$province_of_destination) %>% 
        group_by(Variant, Year, Sex, Country_of_Birth) %>% 
        summarise(Number = sum(Number))
      
    }
    
  })
  
  int_migra_circle_data <- reactive({
    
    data_int_migra_circle <- Internal_migration_data %>% 
      filter(Variant %in% input$variant &
               Year == input$single_forecast_year &
               Sex == input$sex & 
               Country_of_Birth == input$country_of_birth) %>% 
      pivot_wider(names_from = Province_Destination, values_from = Number) %>% 
      as.data.frame()
    
    rownames(data_int_migra_circle) <- data_int_migra_circle$Province_Origin
    
    data_int_migra_circle %<>% 
      select(Burgenland:Vienna) %>% 
      as.matrix()
  })
  
  
  
  output$table <- renderDT({
    
    data_table()
    
  })
  
  output$plot <- renderPlotly({
    
    validate(
      need(input$variant, "Please select at least one variant")
    )
    
    ggplot(data_table(), aes(x = Year, y = Number, color = Variant)) +
      geom_line() +
      ylim(0, NA)
    

  })
  
  output$int_migra_circle <- renderPlot({

    validate(
      need(length(input$variant) == 1, "Circleplot ist available only for one variant at a time.")
    )
    # Circular plot
    chordDiagram(int_migra_circle_data(), transparency = 0.5)

  })
  
 
  variant_selected <- reactive(input$variant) %>% debounce(1000)
  
  # Warnung wenn hauptvariant vorjahr ausgewählt
  observeEvent(variant_selected(), {
    if("Main variant from previous year's forecast" %in% variant_selected()){
      showNotification("For the main variant forecast from the previous year, only the total population summed up by country of birth can be displayed.")
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
