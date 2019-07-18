library(httr)
library(jsonlite)
library(rvest)
library(tidyverse)
library(XML)
library(xml2)


url <- "https://rest.survey-xact.dk/"

# Namen der Liste
IP <- c("10a", "10b", "9a", "8b", "8c", "8d", "8e")
dlType <- c(".data.v1", ".val.labels.v3", ".structure.v2", ".var.labels.v1")

apiNames <- cross(list(IP, dlType)) %>%
  map_chr(lift(paste0))


# Links für die Liste
surveyID <- c("10a" = "rest/surveys/1077059", 
              "10b" = "rest/surveys/1077060",
              "9a" = "rest/surveys/1077065",
              "8b" = "rest/surveys/1077061",
              "8c" = "rest/surveys/1077062",
              "8d" = "rest/surveys/1077064",
              "8e" = "rest/surveys/1078326") 

pathsEnd <- c("data" = "/export/dataset?format=EU",
              "labels" = "/export/labels?format=EU",
              "strcuture" = "/export/structure?format=EU",
              "var.labels" = "/export/variables?format=EU")

# Zusammensetzung der finalen Liste für die gesamte API Abfrage
SXlist <- cross(list(surveyID, pathsEnd)) %>%
  map_chr(lift(paste0)) %>%
  set_names(., apiNames) %>%
  as.list()


# Helper für die Lösung des Problems bei der Structure Datei 
# aufgrund einer fehlerhaften Tabellenkonstruktion (ein Delimiter ";" zu viel, 
# Problem von SX Seite)
oldnames.structure <- paste0("V", seq(1:7))
newnames.structure <- c("questionName", "variableName", "questionType", "subType", "questionText", "choiceValue", "choiceText")

# API Abfrage aller Befragungen
SXdl <- SXlist %>%
  map(~GET(url = url, path = .x, authenticate("username", "password")))

# Kontrolle des Status Codes!! Muss 200 sein oder mit 2 beginnen

# Übersetzung der XML Abfrage und erste Aufbereitung aller Objekttypen
SXdf <- SXdl %>%
  map(~rawToChar(x = .$content)) %>%
  map_at(vars(matches(".v1")), ~read.table(text = paste(.), header = T, sep = ";")) %>% 
  map_at(vars(matches(".v2")), ~read.table(text = paste(.), header = F, sep = ";", fill = TRUE, row.names = NULL)) %>% 
  map_at(vars(matches(".v3")), ~read.table(text = paste(.), header = F, sep = ";", col.names = c("V.Name", "V.Nummer", "V.Label"))) %>% 
  # map_at(vars(matches(".v3")), ~filter(., !V.Name %in% c("survey", "organiza"))) %>% 
  map_at(vars(matches(".v2")), ~select_at(., vars(oldnames.structure), ~ newnames.structure)) %>% 
  map_at(vars(matches(".v2")), ~filter(., !questionName == "questionName"))

saveRDS(SXdf, file = "SXACT API/SXdf.VE.rds")
  

