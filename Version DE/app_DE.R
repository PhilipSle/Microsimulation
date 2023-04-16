
# TODO 
# Plotbezeichnungen
# bundesländer, geschlchter, geburtsländer parallel anzeigen


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

if(!exists("Bevoelkerung_variantenvergleich")){
  Bevoelkerung_variantenvergleich <- fread("./Version DE/Bevoelkerung_variantenvergleich.csv")
}
if(!exists("Geburten_variantenvergleich")){
  Geburten_variantenvergleich <- fread("./Version DE/Geburten_variantenvergleich.csv")
}
if(!exists("Sterbefaelle_variantenvergleich")){
  Sterbefaelle_variantenvergleich <- fread("./Version DE/Sterbefaelle_variantenvergleich.csv")
}
if(!exists("Zuwanderung_variantenvergleich")){
  Zuwanderung_variantenvergleich <- fread("./Version DE/Zuwanderung_variantenvergleich.csv")
}
if(!exists("Abwanderung_variantenvergleich")){
  Abwanderung_variantenvergleich <- fread("./Version DE/Abwanderung_variantenvergleich.csv")
}
if(!exists("Binnenwanderung_variantenvergleich")){
  Binnenwanderung_variantenvergleich <- fread("./Version DE/Binnenwanderung_variantenvergleich.csv")
}


# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Modellvergleich Bevölkerungsprognose"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Wähle Prognosezeitraum
      sliderInput("prognosejahre", "Wähle die Prognosejahre, die angezeigt werden sollen:",
                  min = 2022, max = 2100, value = c(2022, 2080)),
      
      conditionalPanel( # separat für binnenwanderung 
        condition = "input.kategorie == 'Migration zwischen Bundesländern'",
        sliderInput("prognosejahr_einzeln", "Wähle ein Prognosejahr für den Circle Plot:",
                    min = 2022, max = 2100, value = 2022)),

      # Wähle Kategorie
      selectInput("kategorie", "Wähle Kategorie",
                  choices = c("Bevölkerung", "Geburten", "Sterbefälle", "Immigration", "Emigration", "Migration zwischen Bundesländern"),
                  selected = "Bevölkerung"),
      
      
      # Wähle Variante
      checkboxGroupInput("variante", "Wähle Prognosevariante",
                         choices = unique(Bevoelkerung_variantenvergleich$Variante), 
                         selected = unique(Bevoelkerung_variantenvergleich$Variante)[1]),
      
      # Wähle Bundesland
      conditionalPanel(
        condition = "input.kategorie != 'Migration zwischen Bundesländern'",
        selectInput("bundesland", "Wähle Bundesland", selectize = T,
                     choices = unique(Bevoelkerung_variantenvergleich$Bundesland), 
                     selected = rev(unique(Bevoelkerung_variantenvergleich$Bundesland))[1])),
      
      conditionalPanel( # Bundesländerwahl für Binnenwanderung
        condition = "input.kategorie == 'Migration zwischen Bundesländern'",
        selectInput("herkunftsbundesland", "Wähle Herkunftsbundesland", multiple = T, selectize = T,
                     choices = unique(Binnenwanderung_variantenvergleich$Herkunftsbundesland), 
                     selected = rev(unique(Binnenwanderung_variantenvergleich$Herkunftsbundesland))[1]),
        selectInput("zielbundesland", "Wähle Zielbundesland", multiple = T, selectize = T,
                    choices = unique(Binnenwanderung_variantenvergleich$Herkunftsbundesland), 
                    selected = unique(Binnenwanderung_variantenvergleich$Herkunftsbundesland)[3])),
      
      
      
      # Wähle Geschlecht
      conditionalPanel(
        condition = "input.kategorie != 'Geburten'",
        radioButtons("geschlecht", "Wähle Geschlecht",
                     choices = unique(Bevoelkerung_variantenvergleich$Geschlecht), 
                     selected = rev(unique(Bevoelkerung_variantenvergleich$Geschlecht))[1]) 
      ),
      
      conditionalPanel(
        condition = "input.kategorie == 'Geburten'",
        sliderInput("alter_der_mutter", "Wähle das Alter der Mutter",
                     min = 10, max = 49, value = c(10, 49))),

      # Wähle Alter
      radioButtons("alter_gruppiert", "Wähle Alter",
                   choices = c("Alle Altersgruppen zusammen", "Altersgruppe wählen"),
                   selected = "Alle Altersgruppen zusammen"),
      
      conditionalPanel(
        condition = "input.alter_gruppiert == 'Altersgruppe wählen'",
        conditionalPanel(
          condition = "input.kategorie != 'Geburten' & input.kategorie != 'Migration zwischen Bundesländern'",
          numericInput("alter_untergrenze", "Wähle untere Altersgrenze", 
                       min = 0, max = 100, value = 0),
          numericInput("alter_obergrenze", "Wähle obere Altersgrenze", 
                       min = 0, max = 100, value = 100) 
          
        )
      ),
      
      
      # Wähle Geburtsland
      conditionalPanel(
        condition = "input.kategorie != 'Sterbefälle' & input.kategorie != 'Emigration'",
        radioButtons("geburtsland", "Wähle Geburtsland",
                     choices = unique(Bevoelkerung_variantenvergleich$Geburtsland), 
                     selected = rev(unique(Bevoelkerung_variantenvergleich$Geburtsland))[1]) 
      )
      
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      plotlyOutput("prognoseplot"),
      
      conditionalPanel(
        condition = "input.kategorie == 'Migration zwischen Bundesländern'",
        plotOutput("biwa_circle")
        ),
      
      DTOutput("table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  data_selected <- reactive({input$kategorie})
  
  alter_selected <- reactive({input$alter_gruppiert})
  
  data_table <- reactive({
    
    if(alter_selected() == "Alle Altersgruppen zusammen"){
      alter_untergrenze = 0
      alter_obergrenze = 100
    }
    if(alter_selected() == "Altersgruppe wählen"){
      alter_untergrenze = input$alter_untergrenze
      alter_obergrenze = input$alter_obergrenze
    }
    
    
    if(data_selected() == "Bevölkerung"){
      
      Bevoelkerung_variantenvergleich[Variante %in% input$variante &
                                        Jahr %in% c(input$prognosejahre[1]:input$prognosejahre[2]) &
                                        Geschlecht == input$geschlecht &
                                        Bundesland %in% input$bundesland &
                                        Alter >= alter_untergrenze & Alter <= alter_obergrenze &
                                        Geburtsland == input$geburtsland, 
                                      .(Anzahl = sum(Anzahl)), 
                                      by = c("Jahr", "Geburtsland", "Bundesland", "Geschlecht", "Variante")]
      
    } else if(data_selected() == "Geburten"){
      
      
      Geburten_variantenvergleich[Variante %in% input$variante &
                                    Jahr %in% c(input$prognosejahre[1]:input$prognosejahre[2]) &
                                    Bundesland %in% input$bundesland &
                                    Alter >= input$alter_der_mutter[1] & Alter <= input$alter_der_mutter[2] &
                                    Geburtsland_Mutter == input$geburtsland,
                                  .(Anzahl = sum(Anzahl)),
                                  by = c("Jahr", "Bundesland", "Geburtsland_Mutter", "Variante")
                                  ]
      
      
    } else if(data_selected() == "Sterbefälle"){
      
      
      Sterbefaelle_variantenvergleich[Variante %in% input$variante &
                                        Jahr %in% c(input$prognosejahre[1]:input$prognosejahre[2]) &
                                        Bundesland %in% input$bundesland &
                                        Alter >= alter_untergrenze & Alter <= alter_obergrenze &
                                        Geschlecht == input$geschlecht,
                                      .(Anzahl = sum(Anzahl)),
                                      by = c("Jahr", "Bundesland", "Geschlecht", "Variante")]
      
      
    } else if(data_selected() == "Immigration"){
      
      
      Zuwanderung_variantenvergleich[Variante %in% input$variante &
                                       Jahr %in% c(input$prognosejahre[1]:input$prognosejahre[2]) &
                                       Bundesland %in% input$bundesland &
                                       Alter >= alter_untergrenze & Alter <= alter_obergrenze &
                                       Geschlecht == input$geschlecht & 
                                       Geburtsland == input$geburtsland,
                                     .(Anzahl = sum(Anzahl)),
                                     by = c("Jahr", "Bundesland", "Geschlecht", "Variante", "Geburtsland")]
      
      
    } else if(data_selected() == "Emigration"){
      
      
      Abwanderung_variantenvergleich[Variante %in% input$variante &
                                       Jahr %in% c(input$prognosejahre[1]:input$prognosejahre[2]) &
                                       Bundesland %in% input$bundesland &
                                       Alter >= alter_untergrenze & Alter <= alter_obergrenze &
                                       Geschlecht == input$geschlecht,
                                     .(Anzahl = sum(Anzahl)),
                                     by = c("Jahr", "Bundesland", "Geschlecht", "Variante")]
      

    } else if(data_selected() == "Migration zwischen Bundesländern"){
      
      
      data_biwa <- Binnenwanderung_variantenvergleich[Variante %in% input$variante &
                                                        Jahr %in% c(input$prognosejahre[1]:input$prognosejahre[2]) &
                                                        Geschlecht == input$geschlecht & 
                                                        Geburtsland == input$geburtsland &
                                                        Herkunftsbundesland %in% input$herkunftsbundesland & 
                                                        Zielbundesland %in% input$zielbundesland,
                                                      .(Anzahl = sum(Anzahl)),
                                                      by = c("Variante", "Jahr", "Geschlecht", "Geburtsland")]

    }
    
  })
  
  biwa_circle_data <- reactive({
    
    data_biwa_circle <- Binnenwanderung_variantenvergleich %>% 
      filter(Variante %in% input$variante &
               Jahr == input$prognosejahr_einzeln &
               Geschlecht == input$geschlecht & 
               Geburtsland == input$geburtsland) %>% 
      pivot_wider(names_from = Zielbundesland, values_from = Anzahl) %>% 
      as.data.frame()
    
    rownames(data_biwa_circle) <- data_biwa_circle$Herkunftsbundesland
    
    data_biwa_circle %<>% 
      select(Burgenland:Wien) %>% 
      as.matrix()
  })
  
  
  
  output$table <- renderDT({
    
    data_table()
    
  })
  
  output$prognoseplot <- renderPlotly({
    
    validate(
      need(input$variante, "Bitte zumindest eine Variante wählen.")
    )
    
    ggplot(data_table(), aes(x = Jahr, y = Anzahl, color = Variante)) +
      geom_line() +
      ylim(0, NA)
    

  })
  
  output$biwa_circle <- renderPlot({

    validate(
      need(length(input$variante) == 1, "Circleplot ist nur für einzelne Varianten verfügbar.")
    )
    # Circular plot
    chordDiagram(biwa_circle_data(), transparency = 0.5)

  })
  
 
  variante_gewaehlt <- reactive(input$variante) %>% debounce(1000)
  
  # Warnung wenn hauptvariante vorjahr ausgewählt
  observeEvent(variante_gewaehlt(), {
    if("Hauptvariante Vorjahresprognose" %in% variante_gewaehlt()){
      showNotification("Für die Hauptvariante des Vorjahres kann nur die Bevölkerungszahl aggregiert nach Geburtsland angezeigt werden.")
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
