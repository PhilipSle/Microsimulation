# Erstelle daten für die App mit dem Modellvergleich


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
setwd("./Ergebnisse 2022/ShinyApp/Version DE")



varianten_liste <- data.frame(variantenname = c("Hauptvariante", "Alterungsszenario", "Hohe Fertilität", "Hohe Lebenserwartung", "Hohe Wanderung", "Keine Wanderung",
                                                "Konstante Variante", "Niedrige Fertilität", "Niedrige Lebenserwartung", "Niedrige Wanderung", "Wachstumsszenario"),
                              dateiname = c("Hauptvariante.xlsx", "Alterungsszenario.xlsx", "Hohe_Fertilitätsvariante.xlsx", "Hohe_Lebenserwartungsvariante.xlsx", "Hohe_Wanderungsvariante.xlsx",
                                            "Variante_ohne_Wanderung.xlsx", "Konstante_Variante.xlsx", "Niedrige_Fertilitätsvariante.xlsx", "Niedrige_Lebenserwartungsvariante.xlsx", 
                                            "Niedrige_Wanderungsvariante.xlsx", "Wachstumsszenario.xlsx"))

startjahr = 2022
endjahr = 2100
path = "BPR2022/Ergebnisse"
output_path = "/Ergebnisse 2022"


Bevoelkerung <- list()
Geburten <- list()
Sterbefaelle <- list()
Zuwanderung <- list()
Abwanderung <- list()
Binnenwanderung <- list()

for(i in 1:nrow(varianten_liste)){
  
  print(i)
  variante = varianten_liste$variantenname[i]
  dateiname = varianten_liste$dateiname[i]
  
  file_path <- file.path(idrive, path, variante, dateiname)
  
  Bevoelkerung[[i]] <- read_xlsx(file_path, sheet = "tabTotalPopProvince_countrybirt") %>% 
    select(-"Table Description") %>%
    pivot_longer(names_to = "Geschlecht", values_to = "Anzahl", cols = c("Female", "Male", "All")) %>% 
    mutate(Variante = varianten_liste$variantenname[i]) %>% 
    rename(Jahr = Year, Geburtsland = `Place of birth domestic or abroad`, Bundesland = `Place of residence`, Alter = Age)
  
  Geburten[[i]] <- read_xlsx(file_path, sheet = "tabFertilityByOriginDichotom") %>% 
    select(-"Table Description") %>% 
    rename(Bundesland = `Place of residence`, Alter = Age, Jahr = Year) %>% 
    pivot_longer(names_to = "Geburtsland_Mutter", values_to = "Anzahl", cols = Domestic:All) %>% 
    relocate(Jahr, Geburtsland_Mutter, Bundesland, Alter, Anzahl) %>% 
    mutate(Variante = varianten_liste$variantenname[i])
  
  Sterbefaelle[[i]] <- read_xlsx(file_path, sheet = "tabMortalityTablesGeoNatEvents") %>% 
    select(-"Table Description") %>% 
    pivot_longer(names_to = "Jahr", values_to = "Anzahl", cols = starts_with("2")) %>% 
    filter(Jahr %in% startjahr:endjahr) %>% 
    rename(Bundesland = `Place of residence`, Geschlecht = Sex, Alter = Age) %>% 
    mutate(Variante = varianten_liste$variantenname[i],
           Jahr = as.numeric(Jahr))
  
  Zuwanderung[[i]] <-  read_xlsx(paste0(substr(file_path, 1, nchar(file_path)-5), "_Immigration.xlsx"), sheet = "tabNumberImmigrations") %>% 
    select(-"Table Description") %>% 
    pivot_longer(names_to = "Geburtsland", values_to = "Anzahl", cols = Domestic:All) %>% 
    rename(Bundesland = `Place of residence`, Geschlecht = Sex, Alter = Age, Jahr = Year) %>% 
    filter(Jahr %in% startjahr:endjahr) %>% 
    mutate(Variante = varianten_liste$variantenname[i])
  
  Abwanderung[[i]] <- read_xlsx(file_path, sheet = "tabEmigrationNumberGeoNatCluste") %>% 
    select(-"Table Description") %>% 
    pivot_longer(names_to = "Jahr", values_to = "Anzahl", cols = starts_with("2")) %>% 
    rename(Bundesland = `Place of residence`, Geschlecht = Sex, Alter = Age) %>% 
    filter(Jahr %in% startjahr:endjahr) %>% 
    mutate(Variante = varianten_liste$variantenname[i],
           Jahr = as.numeric(Jahr))
    
  Binnenwanderung[[i]] <-  read_xlsx(file_path, sheet = "tabMigrationNumberEvents") %>% 
    select(-"Table Description") %>% 
    pivot_longer(names_to = "Zielbundesland", values_to = "Anzahl", cols = Burgenland:Wien) %>% 
    rename(Herkunftsbundesland = `Place of residence`, Geschlecht = Sex, Jahr = Year, Geburtsland = `Place of birth domestic or abroad`) %>% 
    filter(Jahr %in% startjahr:endjahr) %>% 
    mutate(Variante = varianten_liste$variantenname[i])
  
  
}
Bevoelkerung_variantenvergleich <- bind_rows(Bevoelkerung)
Geburten_variantenvergleich <- bind_rows(Geburten)
Sterbefaelle_variantenvergleich <- bind_rows(Sterbefaelle)
Zuwanderung_variantenvergleich <- bind_rows(Zuwanderung)
Abwanderung_variantenvergleich <- bind_rows(Abwanderung)
Binnenwanderung_variantenvergleich <- bind_rows(Binnenwanderung)





# hauptvariante des vorjahres anhängen:
Bev_hpt_vorjahr <- list()

folder_path <- file.path(idrive, "BPR2021/Output/Varianten/HPT")
dateien <- list.files(path = folder_path)
dateien <- dateien[-c(1,2,9)]
sheetname <- "Bev_1j_Jahresmitte"
Bundeslaender <- unique(Bevoelkerung_variantenvergleich$Bundesland)

for(i in 1:length(dateien)){
  print(i)
  
  Bev_hpt_vorjahr[[i]] <- file.path(folder_path, dateien[i]) %>% 
    read_xlsx(sheet = sheetname, skip = 2) %>% 
    filter(!is.na(`Alter in Jahren`)) %>% 
    rename(Alter = `Alter in Jahren`) %>% 
    mutate(Geschlecht = rep(c("All","Male", "Female"), each = 102),
           Alter = if_else(Alter == "100+", "100", Alter),
           Alter = if_else(Alter == "Insg.", "All", Alter),
           `2020` = as.numeric(`2020`),
           Geburtsland = "All", 
           Bundesland = dateien[i],
           Variante = "Hauptvariante Vorjahresprognose") %>% 
    pivot_longer(names_to = "Jahr", values_to = "Anzahl", cols = starts_with("2")) %>% 
    mutate(Jahr = as.numeric(Jahr)) %>% 
    relocate(Jahr, Geburtsland, Bundesland, Alter, Geschlecht, Anzahl)
  
}

Bev_hpt_vorjahr %<>% bind_rows()
Bev_hpt_vorjahr %<>% 
  mutate(Bundesland = case_when(grepl("Bgld", Bundesland) ~ "Burgenland",
                                grepl("Ktn", Bundesland) ~ "Kaernten",
                                grepl("NÖ", Bundesland) ~ "Niederoesterreich",
                                grepl("OÖ", Bundesland) ~ "Oberoesterreich",
                                grepl("Österr", Bundesland) ~ "All",
                                grepl("Sbg", Bundesland) ~ "Salzburg",
                                grepl("Stmk", Bundesland) ~ "Steiermark",
                                grepl("Tir", Bundesland) ~ "Tirol",
                                grepl("Vbg", Bundesland) ~ "Vorarlberg",
                                grepl("Wien", Bundesland) ~ "Wien"))



Bevoelkerung_variantenvergleich$Anzahl <- round(Bevoelkerung_variantenvergleich$Anzahl, 1)
Geburten_variantenvergleich$Anzahl <- round(Geburten_variantenvergleich$Anzahl, 1)
Sterbefaelle_variantenvergleich$Anzahl <- round(Sterbefaelle_variantenvergleich$Anzahl, 1)
Zuwanderung_variantenvergleich$Anzahl <- round(Zuwanderung_variantenvergleich$Anzahl, 1)
Abwanderung_variantenvergleich$Anzahl <- round(Abwanderung_variantenvergleich$Anzahl, 1)
Binnenwanderung_variantenvergleich$Anzahl <- round(Binnenwanderung_variantenvergleich$Anzahl, 1)



Bevoelkerung_variantenvergleich %<>% rbind(Bev_hpt_vorjahr)
Bevoelkerung_variantenvergleich %<>% 
  filter(Alter != "All") %>% mutate(Alter = as.numeric(Alter))

Geburten_variantenvergleich %<>% 
  filter(Alter != "All") %>% mutate(Alter = as.numeric(Alter))

Sterbefaelle_variantenvergleich %<>% 
  filter(Alter != "All") %>% mutate(Alter = as.numeric(Alter))

Zuwanderung_variantenvergleich %<>% 
  filter(Alter != "All") %>% mutate(Alter = as.numeric(Alter))

Abwanderung_variantenvergleich %<>% 
  filter(Alter != "All") %>% mutate(Alter = as.numeric(Alter))


saveRDS(Bevoelkerung_variantenvergleich, "Bevoelkerung_variantenvergleich.RDS")
saveRDS(Geburten_variantenvergleich, "Geburten_variantenvergleich.RDS")
saveRDS(Sterbefaelle_variantenvergleich, "Sterbefaelle_variantenvergleich.RDS")
saveRDS(Zuwanderung_variantenvergleich, "Zuwanderung_variantenvergleich.RDS")
saveRDS(Abwanderung_variantenvergleich, "Abwanderung_variantenvergleich.RDS")
saveRDS(Binnenwanderung_variantenvergleich, "Binnenwanderung_variantenvergleich.RDS")
