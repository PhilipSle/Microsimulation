library(shiny)
library(ggplot2)
library(magrittr)
library(haven)
library(foreign)
library(plyr)
library(dplyr)
library(tidyr)
#library(xlsx)
library(stringr)
library(lubridate)
library(tibble)
library(data.table)
library(plotly)
library(DT)


if(!exists("Bevoelkerung_vergleich")){
  Bevoelkerung_vergleich <- fread("Bevoelkerung_vergleich.csv.gz")
}
if(!exists("Abwanderung_vergleich")){
  Abwanderung_vergleich <- fread("Abwanderung_vergleich.csv.gz")
}
if(!exists("Geburten_vergleich")){
  Geburten_vergleich <- fread("Geburten_vergleich.csv.gz")
}
if(!exists("Sterbefaelle_vergleich")){
  Sterbefaelle_vergleich <- fread("Sterbefaelle_vergleich.csv.gz")
}
if(!exists("Zuwanderung_vergleich")){
  Zuwanderung_vergleich <- fread("Zuwanderung_vergleich.csv.gz")
}
if(!exists("Binnenwanderung_vergleich")){
  Binnenwanderung_vergleich <- fread("Binnenwanderung_vergleich.csv.gz")
}



# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Vergleich der Bevölkerungsprognose mit der Vorjahresprognose"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Wähle Prognosezeitraum
      conditionalPanel(
        condition = "input.kategorie != 'Geburten Altersverteilung' & input.kategorie != 'Emigration Altersverteilung' & 
        input.kategorie != 'Sterbefälle Altersverteilung'",
        sliderInput("prognosejahre", "Wähle die Prognosejahre, die angezeigt werden sollen:",
                  min = 2023, max = 2100, value = c(2023, 2080))),
      
      # Wähle Referenzjahr
      sliderInput("referenzjahr", "Wähle ein Referenzjahr, auf Basis dessen die Differenz betrachtet werden soll:",
                  min = 2023, max = 2100, value = c(2030)),
      
     
      # Wähle Kategorie
      selectInput("kategorie", "Wähle Kategorie",
                  choices = c("Bevölkerung", "Emigration", "Emigration Altersverteilung", "Geburten", "Geburten Altersverteilung", 
                              "Sterbefälle", "Sterbefälle Altersverteilung", "Immigration", "Immigration Altersverteilung", 
                              "Migration zwischen Bundesländern"),
                  selected = "Bevölkerung"),
      
      
      # Wähle Variante
      checkboxGroupInput("variante", "Wähle Prognosevariante",
                         choices = unique(Bevoelkerung_vergleich$Variante), 
                         selected = unique(Bevoelkerung_vergleich$Variante)[1]),
      
      # Wähle Bundesland
      conditionalPanel(
        condition = "input.kategorie != 'Migration zwischen Bundesländern'",
        selectInput("bundesland", "Wähle Bundesland", selectize = T,
                    choices = unique(Bevoelkerung_vergleich$Bundesland), 
                    selected = rev(unique(Bevoelkerung_vergleich$Bundesland))[1])),
      
      # Wähle Herkunftsbundesland bei Binnenwanderung
      conditionalPanel(
        condition = "input.kategorie == 'Migration zwischen Bundesländern'",
        selectInput("bundesland_herkunft", "Wähle Herkunftsbundesland", selectize = T,
                    choices = unique(Binnenwanderung_vergleich$Herkunftsbundesland), 
                    selected = rev(unique(Binnenwanderung_vergleich$Herkunftsbundesland))[1])),
      
      # Wähle Zielbundesland bei Binnenwanderung
      conditionalPanel(
        condition = "input.kategorie == 'Migration zwischen Bundesländern'",
        selectInput("bundesland_ziel", "Wähle Zielbundesland", selectize = T,
                    choices = unique(Binnenwanderung_vergleich$Zielbundesland), 
                    selected = rev(unique(Binnenwanderung_vergleich$Zielbundesland))[7])),
      
      
      
      
      
      # Wähle Geschlecht
      conditionalPanel(
        condition = "input.kategorie != 'Geburten' & input.kategorie != 'Geburten Altersverteilung'",
        radioButtons("geschlecht", "Wähle Geschlecht",
                     choices = unique(Bevoelkerung_vergleich$Geschlecht), 
                     selected = rev(unique(Bevoelkerung_vergleich$Geschlecht))[1]) 
      ),
      

      
      # Wähle Alter
      conditionalPanel(
        condition = "input.kategorie != 'Geburten Altersverteilung' & input.kategorie != 'Emigration Altersverteilung' & 
        input.kategorie != 'Sterbefälle Altersverteilung' & input.kategorie != 'Immigration Altersverteilung' & input.kategorie != 'Migration zwischen Bundesländern'",
        radioButtons("alter_gruppiert", "Wähle Alter",
                   choices = c("Alle Altersgruppen zusammen", "Altersgruppe wählen"),
                   selected = "Alle Altersgruppen zusammen")),
      
      conditionalPanel(
        condition = "input.alter_gruppiert == 'Altersgruppe wählen' & input.kategorie != 'Sterbefälle Altersverteilung' & 
        input.kategorie != 'Geburten' & input.kategorie != 'Migration zwischen Bundesländern'",
          numericInput("alter_untergrenze", "Wähle untere Altersgrenze", 
                       min = 0, max = 100, value = 0),
          numericInput("alter_obergrenze", "Wähle obere Altersgrenze", 
                       min = 0, max = 100, value = 100) 
      ),
      conditionalPanel(
        condition = "input.kategorie == 'Geburten'",
        sliderInput("alter_der_mutter", "Wähle das Alter der Mutter",
                    min = 10, max = 49, value = c(10, 49))),
      
      
      
      # Wähle Geburtsland
      conditionalPanel(
        condition = "input.kategorie == 'Bevölkerung' | input.kategorie == 'Immigration'",
        radioButtons("geburtsland", "Wähle Geburtsland",
                     choices = unique(Bevoelkerung_vergleich$Geburtsland), 
                     selected = rev(unique(Bevoelkerung_vergleich$Geburtsland))[1]) 
      ),
      conditionalPanel(
        condition = "input.kategorie == 'Geburten' | input.kategorie == 'Geburten Altersverteilung'",
        radioButtons("geburtsland_mutter", "Wähle Geburtsland der Mutter",
                     choices = unique(Geburten_vergleich$Geburtsland_Mutter ), 
                     selected = rev(unique(Geburten_vergleich$Geburtsland_Mutter ))[1]) 
      )
      
      
      
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
      plotlyOutput("prognoseplot"),
      br(),
      br(),
      p(strong("Differenz zwischen den Prognosegenerationen:")),
      tableOutput("table_referenzjahr"),
      br(),
      br(),
      p(strong("Plotdaten:")),
      br(),
      DTOutput("table")
      
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  data_selected <- reactive({input$kategorie})

  alter_selected <- reactive({input$alter_gruppiert})

  referenzjahr_selected <- reactive({input$referenzjahr})
  
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

      Bevoelkerung_vergleich[Variante %in% input$variante &
                               Jahr %in% c(input$prognosejahre[1]:input$prognosejahre[2]) &
                               Geschlecht == input$geschlecht &
                               Bundesland %in% input$bundesland &
                               Alter >= alter_untergrenze & Alter <= alter_obergrenze &
                               Geburtsland == input$geburtsland,
                             .(`BPR 2023` = round(sum(`BPR 2023`)),
                               `BPR 2022` = round(sum(`BPR 2022`))),
                             by = c("Jahr", "Geburtsland", "Bundesland", "Geschlecht", "Variante")] %>% 
        mutate("Differenz Absolut" = round(`BPR 2023` - `BPR 2022`, 1),
               "Differenz in Prozent" = round(100*((`BPR 2023` / `BPR 2022`)-1), 3)) %>% 
        pivot_longer(names_to = "BPR", values_to = "Anzahl", cols = `BPR 2023`:`BPR 2022`) %>% 
        mutate(BPR = paste0(Variante, " ", BPR)) %>% 
        arrange(desc("Differenz in Prozent"))
    }
    else if(data_selected() == "Emigration"){
      
      Abwanderung_vergleich[Variante %in% input$variante &
                              Jahr %in% c(input$prognosejahre[1]:input$prognosejahre[2]) &
                              Bundesland %in% input$bundesland &
                              Alter >= alter_untergrenze & Alter <= alter_obergrenze &
                              Geschlecht == input$geschlecht,
                            .(`BPR 2023` = round(sum(`BPR 2023`)),
                              `BPR 2022` = round(sum(`BPR 2022`))),
                            by = c("Jahr", "Bundesland", "Geschlecht", "Variante")] %>% 
        mutate("Differenz Absolut" = round(`BPR 2023` - `BPR 2022`, 1),
               "Differenz in Prozent" = round(100*((`BPR 2023` / `BPR 2022`)-1), 3)) %>% 
        pivot_longer(names_to = "BPR", values_to = "Anzahl", cols = `BPR 2023`:`BPR 2022`) %>% 
        mutate(BPR = paste0(Variante, " ", BPR)) %>% 
        arrange(desc("Differenz in Prozent"))
      
    }
    else if(data_selected() == "Emigration Altersverteilung"){
      
      Abwanderung_vergleich[Variante %in% input$variante &
                              Jahr == input$referenzjahr &
                              Bundesland %in% input$bundesland &
                              Geschlecht == input$geschlecht,
                            .(`BPR 2023` = round(sum(`BPR 2023`)),
                              `BPR 2022` = round(sum(`BPR 2022`))),
                            by = c("Variante", "Jahr", "Bundesland", "Geschlecht", "Alter")]%>% 
        pivot_longer(names_to = "BPR", values_to = "Anzahl", cols = `BPR 2023`:`BPR 2022`) %>% 
        mutate(BPR = paste0(Variante, " ", BPR)) %>% 
        arrange(BPR, Alter)
      
      
    }
    
    
    else if(data_selected() == "Geburten"){
      
      Geburten_vergleich[Variante %in% input$variante &
                           Jahr %in% c(input$prognosejahre[1]:input$prognosejahre[2]) &
                           Bundesland %in% input$bundesland &
                           Alter >= input$alter_der_mutter[1] & Alter <= input$alter_der_mutter[2] &
                           Geburtsland_Mutter == input$geburtsland_mutter,
                         .(`BPR 2023` = round(sum(`BPR 2023`)),
                           `BPR 2022` = round(sum(`BPR 2022`))),
                         by = c("Jahr", "Bundesland", "Geburtsland_Mutter", "Variante")] %>% 
        mutate("Differenz Absolut" = round(`BPR 2023` - `BPR 2022`, 1),
               "Differenz in Prozent" = round(100*((`BPR 2023` / `BPR 2022`)-1), 3)) %>%
        pivot_longer(names_to = "BPR", values_to = "Anzahl", cols = `BPR 2023`:`BPR 2022`) %>% 
        mutate(BPR = paste0(Variante, " ", BPR)) %>% 
        arrange(desc("Differenz in Prozent"))
      
    }
    else if(data_selected() == "Geburten Altersverteilung"){
      
      Geburten_vergleich[Variante %in% input$variante &
                           Jahr == input$referenzjahr &
                           Bundesland %in% input$bundesland &
                           #Alter >= alter_der_mutter[1] & Alter <= alter_der_mutter[2] &
                           Geburtsland_Mutter == input$geburtsland_mutter,
                         .(`BPR 2023` = round(sum(`BPR 2023`)),
                           `BPR 2022` = round(sum(`BPR 2022`))),
                         by = c("Variante", "Jahr", "Bundesland", "Geburtsland_Mutter",  "Alter")] %>% 
        pivot_longer(names_to = "BPR", values_to = "Anzahl", cols = `BPR 2023`:`BPR 2022`) %>% 
        mutate(BPR = paste0(Variante, " ", BPR)) %>% 
        arrange(BPR, Alter)
    }
    else if(data_selected() == "Sterbefälle"){
      
      Sterbefaelle_vergleich[Variante %in% input$variante &
                               Jahr %in% c(input$prognosejahre[1]:input$prognosejahre[2]) &
                               Bundesland %in% input$bundesland &
                               Alter >= alter_untergrenze & Alter <= alter_obergrenze &
                               Geschlecht == input$geschlecht,
                             .(`BPR 2023` = round(sum(`BPR 2023`)),
                               `BPR 2022` = round(sum(`BPR 2022`))),
                             by = c("Jahr", "Bundesland", "Geschlecht", "Variante")] %>% 
        mutate("Differenz Absolut" = round(`BPR 2023` - `BPR 2022`, 1),
               "Differenz in Prozent" = round(100*((`BPR 2023` / `BPR 2022`)-1), 3)) %>% 
        pivot_longer(names_to = "BPR", values_to = "Anzahl", cols = `BPR 2023`:`BPR 2022`) %>% 
        mutate(BPR = paste0(Variante, " ", BPR)) %>% 
        arrange(desc("Differenz in Prozent"))
    }
    
    else if(data_selected() == "Sterbefälle Altersverteilung"){
      
      Sterbefaelle_vergleich[Variante %in% input$variante &
                               Jahr == input$referenzjahr &
                               Bundesland %in% input$bundesland &
                               Geschlecht == input$geschlecht,
                             .(`BPR 2023` = round(sum(`BPR 2023`)),
                               `BPR 2022` = round(sum(`BPR 2022`))),
                             by = c("Variante", "Jahr", "Bundesland", "Geschlecht", "Alter")] %>% 
        pivot_longer(names_to = "BPR", values_to = "Anzahl", cols = `BPR 2023`:`BPR 2022`) %>% 
        mutate(BPR = paste0(Variante, " ", BPR)) %>% 
        arrange(BPR, Alter)
    }
    
    else if(data_selected() == "Immigration"){
      
      Zuwanderung_vergleich[Variante %in% input$variante &
                              Jahr %in% c(input$prognosejahre[1]:input$prognosejahre[2]) &
                              Geschlecht == input$geschlecht &
                              Bundesland %in% input$bundesland &
                              Alter >= alter_untergrenze & Alter <= alter_obergrenze &
                              Geburtsland == input$geburtsland,
                            .(`BPR 2023` = round(sum(`BPR 2023`)),
                              `BPR 2022` = round(sum(`BPR 2022`))),
                            by = c("Jahr", "Geburtsland", "Bundesland", "Geschlecht", "Variante")] %>% 
        pivot_longer(names_to = "BPR", values_to = "Anzahl", cols = `BPR 2023`:`BPR 2022`) %>% 
        mutate(BPR = paste0(Variante, " ", BPR)) %>% 
        arrange(BPR)
    }
    
    else if(data_selected() == "Immigration Altersverteilung"){
      
      Zuwanderung_vergleich[Variante %in% input$variante &
                              Jahr == input$referenzjahr &
                              Bundesland %in% input$bundesland &
                              Geschlecht == input$geschlecht &
                              Geburtsland == input$geburtsland,
                            .(`BPR 2023` = round(sum(`BPR 2023`)),
                              `BPR 2022` = round(sum(`BPR 2022`))),
                            by = c("Variante", "Jahr", "Geburtsland", "Bundesland", "Geschlecht", "Alter")]%>% 
        pivot_longer(names_to = "BPR", values_to = "Anzahl", cols = `BPR 2023`:`BPR 2022`) %>% 
        mutate(BPR = paste0(Variante, " ", BPR)) %>% 
        arrange(BPR, Alter)
    }
    
    else if(data_selected() == "Migration zwischen Bundesländern"){
      
      Binnenwanderung_vergleich[Variante %in% input$variante &
                                  Jahr %in% c(input$prognosejahre[1]:input$prognosejahre[2]) &
                                  Geschlecht == input$geschlecht &
                                  Herkunftsbundesland %in% input$bundesland_herkunft &
                                  Zielbundesland %in% input$bundesland_ziel &
                                  Geburtsland == input$geburtsland,
                                .(`BPR 2023` = round(sum(`BPR 2023`)),
                                  `BPR 2022` = round(sum(`BPR 2022`))),
                                by = c("Jahr", "Geburtsland", "Herkunftsbundesland",
                                       "Zielbundesland", "Geschlecht", "Variante")] %>%
        pivot_longer(names_to = "BPR", values_to = "Anzahl", cols = `BPR 2023`:`BPR 2022`) %>% 
        mutate(BPR = paste0(Variante, " ", BPR)) %>% 
        arrange(BPR)
    }
    
  

  })
  
  
  data_table_referenzjahr <- reactive({
    
    if(alter_selected() == "Alle Altersgruppen zusammen"){
      alter_untergrenze = 0
      alter_obergrenze = 100
    }
    if(alter_selected() == "Altersgruppe wählen"){
      alter_untergrenze = input$alter_untergrenze
      alter_obergrenze = input$alter_obergrenze
    }
    
    
    if(data_selected() == "Bevölkerung"){
      
      Bevoelkerung_vergleich[Variante %in% input$variante &
                               Jahr == input$referenzjahr &
                               Geschlecht == input$geschlecht &
                               Bundesland %in% input$bundesland &
                               Alter >= alter_untergrenze & Alter <= alter_obergrenze &
                               Geburtsland == input$geburtsland,
                             .(`BPR 2023` = round(sum(`BPR 2023`)),
                               `BPR 2022` = round(sum(`BPR 2022`))),
                             by = c("Jahr", "Geburtsland", "Bundesland", "Geschlecht", "Variante")] %>% 
        mutate("Differenz Absolut" = round(`BPR 2023` - `BPR 2022`, 1),
               "Differenz in Prozent" = round(100*((`BPR 2023` / `BPR 2022`)-1), 3)) %>% 
        arrange(desc("Differenz in Prozent"))
    }
    
    else if(data_selected() == "Emigration"){
      
      Abwanderung_vergleich[Variante %in% input$variante &
                              Jahr == input$referenzjahr &
                              Bundesland %in% input$bundesland &
                              Alter >= alter_untergrenze & Alter <= alter_obergrenze &
                              Geschlecht == input$geschlecht,
                            .(`BPR 2023` = round(sum(`BPR 2023`)),
                              `BPR 2022` = round(sum(`BPR 2022`))),
                            by = c("Jahr", "Bundesland", "Geschlecht", "Variante")] %>% 
        mutate("Differenz Absolut" = round(`BPR 2023` - `BPR 2022`, 1),
               "Differenz in Prozent" = round(100*((`BPR 2023` / `BPR 2022`)-1), 3)) %>% 
        arrange(desc("Differenz in Prozent"))
    }
    else if(data_selected() == "Geburten"){
      
      Geburten_vergleich[Variante %in% input$variante &
                           Jahr  == input$referenzjahr &
                           Bundesland %in% input$bundesland &
                           Alter >= input$alter_der_mutter[1] & Alter <= input$alter_der_mutter[2] &
                           Geburtsland_Mutter == input$geburtsland_mutter,
                         .(`BPR 2023` = round(sum(`BPR 2023`)),
                           `BPR 2022` = round(sum(`BPR 2022`))),
                         by = c("Jahr", "Bundesland", "Geburtsland_Mutter", "Variante")] %>% 
        mutate("Differenz Absolut" = round(`BPR 2023` - `BPR 2022`, 1),
               "Differenz in Prozent" = round(100*((`BPR 2023` / `BPR 2022`)-1), 3)) %>% 
        arrange(desc("Differenz in Prozent"))
    }
    
    else if(data_selected() == "Geburten Altersverteilung"){data.table("Keine jährliche Differenz zwischen Prognosegenerationen bei Betrachten von Altersverteilungen") %>% 
        rename("Keine Differenz:" = V1)}
    else if(data_selected() == "Emigration Altersverteilung"){data.table("Keine jährliche Differenz zwischen Prognosegenerationen bei Betrachten von Altersverteilungen") %>% 
        rename("Keine Differenz:" = V1)}
    
    else if(data_selected() == "Sterbefälle"){
      
      Sterbefaelle_vergleich[Variante %in% input$variante &
                               Jahr == input$referenzjahr &
                               Bundesland %in% input$bundesland &
                               Alter >= alter_untergrenze & Alter <= alter_obergrenze &
                               Geschlecht == input$geschlecht,
                             .(`BPR 2023` = round(sum(`BPR 2023`)),
                               `BPR 2022` = round(sum(`BPR 2022`))),
                             by = c("Jahr", "Bundesland", "Geschlecht", "Variante")] %>% 
        mutate("Differenz Absolut" = round(`BPR 2023` - `BPR 2022`, 1),
               "Differenz in Prozent" = round(100*((`BPR 2023` / `BPR 2022`)-1), 3)) %>% 
        arrange(desc("Differenz in Prozent"))
    }

    else if(data_selected() == "Sterbefälle Altersverteilung"){data.table("Keine jährliche Differenz zwischen Prognosegenerationen bei Betrachten von Altersverteilungen") %>% 
        rename("Keine Differenz:" = V1)}
    
    else if(data_selected() == "Immigration"){
      
      Zuwanderung_vergleich[Variante %in% input$variante &
                              Jahr == input$referenzjahr &
                              Geschlecht == input$geschlecht &
                              Bundesland %in% input$bundesland &
                              Alter >= alter_untergrenze & Alter <= alter_obergrenze &
                              Geburtsland == input$geburtsland,
                            .(`BPR 2023` = round(sum(`BPR 2023`)),
                              `BPR 2022` = round(sum(`BPR 2022`))),
                            by = c("Jahr", "Geburtsland", "Bundesland", "Geschlecht", "Variante")] %>% 
        mutate("Differenz Absolut" = round(`BPR 2023` - `BPR 2022`, 1),
               "Differenz in Prozent" = round(100*((`BPR 2023` / `BPR 2022`)-1), 3)) %>% 
        arrange(desc("Differenz in Prozent"))
    }
    else if(data_selected() == "Immigration Altersverteilung"){data.table("Keine jährliche Differenz zwischen Prognosegenerationen bei Betrachten von Altersverteilungen") %>% 
        rename("Keine Differenz:" = V1)}
    
    else if(data_selected() == "Migration zwischen Bundesländern"){
      
      Binnenwanderung_vergleich[Variante %in% input$variante &
                                  Jahr == input$referenzjahr &
                                  Geschlecht == input$geschlecht &
                                  Geburtsland == input$geburtsland & 
                                  Herkunftsbundesland == input$bundesland_herkunft &
                                  Zielbundesland == input$bundesland_ziel,
                                .(`BPR 2023` = round(sum(`BPR 2023`)),
                                  `BPR 2022` = round(sum(`BPR 2022`))),
                                by = c("Jahr", "Geburtsland", "Herkunftsbundesland",
                                       "Zielbundesland", "Geschlecht", "Variante")] %>%
        mutate("Differenz Absolut" = round(`BPR 2023` - `BPR 2022`, 1),
               "Differenz in Prozent" = round(100*((`BPR 2023` / `BPR 2022`)-1), 3)) %>% 
        arrange(desc("Differenz in Prozent"))
      
      
      
    }
    
  })
  
  
  
  
  
  
  
  output$table_referenzjahr <- renderTable({
    
    data_table_referenzjahr()
    
  })
  
  
  output$table <- renderDT({

    data_table()

  })

  output$prognoseplot <- renderPlotly({

    validate(
      need(input$variante, "Bitte zumindest eine Variante wählen.")
    )
    validate(
      need(input$bundesland_herkunft != input$bundesland_ziel, "Das Herkunftsbundesland darf nicht gleich dem Zielbundesland sein.")
    )
    validate(
      need(input$kategorie == "Geburten" | input$alter_untergrenze <= input$alter_obergrenze, "Die Untergrenze des Alters darf nicht über der Obergrenze liegen.")
    )

    if(data_selected() != "Geburten Altersverteilung" & data_selected() != "Emigration Altersverteilung" & 
       data_selected() != "Sterbefälle Altersverteilung" & data_selected() != "Immigration Altersverteilung"){
      
      ggplot(data_table(),
             aes(x = Jahr, y = Anzahl, color = BPR)) +
        geom_line() +
        geom_vline(xintercept = referenzjahr_selected(), color = "darkblue")
      
    }
    else if(data_selected() == "Geburten Altersverteilung"){
      
      ggplot(data_table(), aes(x = Alter, y = Anzahl, color = BPR)) +
        geom_line()+
        ggtitle("Verteilung der Geburten nach dem Alter der Mutter")
    }
    else if(data_selected() == "Emigration Altersverteilung"){
      
      ggplot(data_table(), aes(x = Alter, y = Anzahl, color = BPR)) +
        geom_line()+
        ggtitle("Alterverteilung der Emigranten")
    }
    else if(data_selected() == "Sterbefälle Altersverteilung"){
      
      ggplot(data_table(), aes(x = Alter, y = Anzahl, color = BPR)) +
        geom_line()+
        ggtitle("Alterverteilung der Gestorbenen")
    }
    else if(data_selected() == "Immigration Altersverteilung"){
      
      ggplot(data_table(), aes(x = Alter, y = Anzahl, color = BPR)) +
        geom_line()+
        ggtitle("Alterverteilung der Immigranten")
      
    }

  })

 
 
  variante_gewaehlt <- reactive(input$variante) %>% debounce(1000)


  
  
  # Warnung wenn hauptvariante vorjahr ausgewählt
  observeEvent(variante_gewaehlt(), {
    if(data_selected() == "Immigration" | data_selected() == "Immigration Altersverteilung"){
      showNotification("Für die Immigration der BPR 2022 gibt es nur Modellergebnisse mit 1-fach simulierter Bevölkerungsgröße.")
    }
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
