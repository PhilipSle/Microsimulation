# Create data for the App


library(mountSTAT)
library(magrittr)
library(haven)
library(foreign)
library(plyr)
library(dplyr)
library(tidyr)
library(xlsx)
library(lubridate)
library(tibble)
library(data.table)
library(readxl)
library(readr)
library(eha)
library(rio)

idrive <- mountWinShare(server = "DatenB", share = "B_BPR")
setwd("./Ergebnisse 2022/ShinyApp/Version EN")



varianten_liste <- data.frame(variantenname = c("Hauptvariante", "Alterungsszenario", "Hohe Fertilität", "Hohe Lebenserwartung", "Hohe Wanderung", "Keine Wanderung",
                                                "Konstante Variante", "Niedrige Fertilität", "Niedrige Lebenserwartung", "Niedrige Wanderung", "Wachstumsszenario"),
                              dateiname = c("Hauptvariante.xlsx", "Alterungsszenario.xlsx", "Hohe_Fertilitätsvariante.xlsx", "Hohe_Lebenserwartungsvariante.xlsx", "Hohe_Wanderungsvariante.xlsx",
                                            "Variante_ohne_Wanderung.xlsx", "Konstante_Variante.xlsx", "Niedrige_Fertilitätsvariante.xlsx", "Niedrige_Lebenserwartungsvariante.xlsx", 
                                            "Niedrige_Wanderungsvariante.xlsx", "Wachstumsszenario.xlsx"))

start_year = 2022
end_year = 2100
path = "BPR2022/Ergebnisse"
output_path = "/Ergebnisse 2022"


Population <- list()
Births <- list()
Deaths <- list()
Immigration <- list()
Emigration <- list()
Internal_migration <- list()

for(i in 1:nrow(varianten_liste)){
  
  print(i)
  variante = varianten_liste$variantenname[i]
  dateiname = varianten_liste$dateiname[i]
  
  file_path <- file.path(idrive, path, variante, dateiname)
  
  Population[[i]] <- read_xlsx(file_path, sheet = "tabTotalPopProvince_countrybirt") %>% 
    select(-"Table Description") %>%
    pivot_longer(names_to = "Sex", values_to = "Number", cols = c("Female", "Male", "All")) %>% 
    mutate(Variant = varianten_liste$variantenname[i]) %>% 
    rename(Country_of_Birth = `Place of birth domestic or abroad`, Province = `Place of residence`)
  
  Births[[i]] <- read_xlsx(file_path, sheet = "tabFertilityByOriginDichotom") %>% 
    select(-"Table Description") %>% 
    rename(Province = `Place of residence`) %>% 
    pivot_longer(names_to = "Country_of_Birth_Mother", values_to = "Number", cols = Domestic:All) %>% 
    relocate(Year, Country_of_Birth_Mother, Province, Age, Number) %>% 
    mutate(Variant = varianten_liste$variantenname[i])
  
  Deaths[[i]] <- read_xlsx(file_path, sheet = "tabMortalityTablesGeoNatEvents") %>% 
    select(-"Table Description") %>% 
    pivot_longer(names_to = "Year", values_to = "Number", cols = starts_with("2")) %>% 
    rename(Province = `Place of residence`) %>% 
    mutate(Variant = varianten_liste$variantenname[i],
           Year = as.numeric(Year))
  
  Immigration[[i]] <-  read_xlsx(paste0(substr(file_path, 1, nchar(file_path)-5), "_Immigration.xlsx"), sheet = "tabNumberImmigrations") %>% 
    select(-"Table Description") %>% 
    pivot_longer(names_to = "Country_of_Birth", values_to = "Number", cols = Domestic:All) %>% 
    rename(Province = `Place of residence`) %>% 
    mutate(Variant = varianten_liste$variantenname[i])
  
  Emigration[[i]] <- read_xlsx(file_path, sheet = "tabEmigrationNumberGeoNatCluste") %>% 
    select(-"Table Description") %>% 
    pivot_longer(names_to = "Year", values_to = "Number", cols = starts_with("2")) %>% 
    rename(Province = `Place of residence`) %>% 
    mutate(Variant = varianten_liste$variantenname[i],
           Year = as.numeric(Year))
    
  Internal_migration[[i]] <-  read_xlsx(file_path, sheet = "tabMigrationNumberEvents") %>% 
    select(-"Table Description") %>% 
    pivot_longer(names_to = "Province_Destination", values_to = "Number", cols = Burgenland:Wien) %>% 
    rename(Province_Origin = `Place of residence`, Country_of_Birth = `Place of birth domestic or abroad`) %>% 
    mutate(Variant = varianten_liste$variantenname[i])
  
  
}
Population_data <- bind_rows(Population)
Births_data <- bind_rows(Births)
Deaths_data <- bind_rows(Deaths)
Immigration_data <- bind_rows(Immigration)
Emigration_data <- bind_rows(Emigration)
Internal_migration_data <- bind_rows(Internal_migration)





# Population data of the previous year
Population_previous <- list()

folder_path <- file.path(idrive, "BPR2021/Output/Varianten/HPT")
dateien <- list.files(path = folder_path)
dateien <- dateien[-c(1,2,9)]
sheetname <- "Bev_1j_Jahresmitte"
Provinces <- unique(Population_data$Bundesland)

for(i in 1:length(dateien)){
  print(i)
  
  Population_previous[[i]] <- file.path(folder_path, dateien[i]) %>% 
    read_xlsx(sheet = sheetname, skip = 2) %>% 
    filter(!is.na(`Alter in Jahren`)) %>% 
    rename(Age = `Alter in Jahren`, Sex = Geschlecht) %>% 
    mutate(Sex = rep(c("All","Male", "Female"), each = 102),
           Age = if_else(Age == "100+", "100", Age),
           Age = if_else(Age == "Insg.", "All", Age),
           `2020` = as.numeric(`2020`),
           Country_of_Birth = "All", 
           Province = dateien[i],
           Variant = "Hauptvariante Vorjahresprognose") %>% 
    pivot_longer(names_to = "Year", values_to = "Number", cols = starts_with("2")) %>% 
    mutate(Year = as.numeric(Year)) %>% 
    relocate(Year, Country_of_Birth, Province, Age, Sex, Number)
  
}

Population_previous %<>% bind_rows()
Population_previous %<>% 
  mutate(Province = case_when(grepl("Bgld", Province) ~ "Burgenland",
                                grepl("Ktn", Province) ~ "Kaernten",
                                grepl("NÖ", Province) ~ "Niederoesterreich",
                                grepl("OÖ", Province) ~ "Oberoesterreich",
                                grepl("Österr", Province) ~ "All",
                                grepl("Sbg", Province) ~ "Salzburg",
                                grepl("Stmk", Province) ~ "Steiermark",
                                grepl("Tir", Province) ~ "Tirol",
                                grepl("Vbg", Province) ~ "Vorarlberg",
                                grepl("Wien", Province) ~ "Wien"))



Population_data$Number <- round(Population_data$Number, 1)
Births_data$Number <- round(Births_data$Number, 1)
Deaths_data$Number <- round(Deaths_data$Number, 1)
Immigration_data$Number <- round(Immigration_data$Number, 1)
Emigration_data$Number <- round(Emigration_data$Number, 1)
Internal_migration_data$Number <- round(Internal_migration_data$Number, 1)

Population_data %<>% rbind(Population_previous)



translate_column_names <- function(data_input, column_province, column_variant){
  
  data_input %<>% rename(province_tmp = column_province, variant_tmp = column_variant)
  
  data_input %<>%
    mutate(province_tmp = case_when(province_tmp == "Burgenland" ~ "Burgenland",
                                    province_tmp == "Kaernten" ~ "Carinthia",
                                    province_tmp == "Niederoesterreich" ~ "Lower Austria",
                                    province_tmp == "Oberoesterreich" ~ "Upper Austria",
                                    province_tmp == "Salzburg" ~ "Salzburg",
                                    province_tmp == "Steiermark" ~ "Styria",
                                    province_tmp == "Tirol" ~ "Tirol",
                                    province_tmp == "Vorarlberg" ~ "Vorarlberg",
                                    province_tmp == "Wien" ~ "Vienna",
                                    TRUE ~ as.character(province_tmp)),
           variant_tmp = case_when(variant_tmp == "Hauptvariante" ~ "Main Scenario",
                                   variant_tmp == "Alterungsszenario" ~ "Ageing scenario",
                                   variant_tmp == "Hohe Fertilität" ~ "High Fertility",
                                   variant_tmp == "Hohe Lebenserwartung" ~ "High Life expectancy",
                                   variant_tmp == "Hohe Wanderung" ~ "High Immigration",
                                   variant_tmp == "Keine Wanderung" ~ "No Migration",
                                   variant_tmp == "Konstante Variante" ~ "Constant Variant",
                                   variant_tmp == "Niedrige Fertilität" ~ "Low Fertility",
                                   variant_tmp == "Niedrige Lebenserwartung" ~ "Low Life expectancy",
                                   variant_tmp == "Niedrige Wanderung" ~ "Low Immigration",
                                   variant_tmp == "Wachstumsszenario" ~ "Growth Scenario",
                                   variant_tmp == "Hauptvariante Vorjahresprognose" ~ "Main variant from previous year's forecast")) %>% 
    

  return(as.data.frame(data_input))
}


Population_data <- translate_column_names(Population_data, "Province", "Variant") %>% rename(Province = province_tmp, Variant = variant_tmp)
Births_data <- translate_column_names(Births_data, "Province", "Variant") %>% rename(Province = province_tmp, Variant = variant_tmp)
Deaths_data <- translate_column_names(Deaths_data, "Province", "Variant") %>% rename(Province = province_tmp, Variant = variant_tmp)
Immigration_data <- translate_column_names(Immigration_data, "Province", "Variant") %>% rename(Province = province_tmp, Variant = variant_tmp)
Emigration_data <- translate_column_names(Emigration_data, "Province", "Variant") %>% rename(Province = province_tmp, Variant = variant_tmp)
Internal_migration_data <- translate_column_names(Internal_migration_data, "Province_Origin", "Variant") %>% rename(Province_Origin = province_tmp, Variant = variant_tmp)
Internal_migration_data %<>% mutate(Province_Destination = case_when(Province_Destination == "Burgenland" ~ "Burgenland",
                                                                     Province_Destination == "Kaernten" ~ "Carinthia",
                                                                     Province_Destination == "Niederoesterreich" ~ "Lower Austria",
                                                                     Province_Destination == "Oberoesterreich" ~ "Upper Austria",
                                                                     Province_Destination == "Salzburg" ~ "Salzburg",
                                                                     Province_Destination == "Steiermark" ~ "Styria",
                                                                     Province_Destination == "Tirol" ~ "Tirol",
                                                                     Province_Destination == "Vorarlberg" ~ "Vorarlberg",
                                                                     Province_Destination == "Wien" ~ "Vienna",
                                                                     TRUE ~ as.character(Province_Destination)))



Population_data %<>% 
  filter(Age != "All") %>% mutate(Age = as.numeric(Age))

Births_data %<>% 
  filter(Age != "All") %>% mutate(Age = as.numeric(Age))

Deaths_data %<>% 
  filter(Age != "All") %>% mutate(Age = as.numeric(Age))

Immigration_data %<>% 
  filter(Age != "All") %>% mutate(Age = as.numeric(Age))

Emigration_data %<>% 
  filter(Age != "All") %>% mutate(Age = as.numeric(Age))


saveRDS(Population_data, "Population_data.RDS")
saveRDS(Births_data, "Births_data.RDS")
saveRDS(Deaths_data, "Deaths_data.RDS")
saveRDS(Immigration_data, "Immigration_data.RDS")
saveRDS(Emigration_data, "Emigration_data.RDS")
saveRDS(Internal_migration_data, "Internal_migration_data.RDS")
