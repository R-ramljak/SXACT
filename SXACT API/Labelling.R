library(tidyverse) 
library(sjlabelled)


## Verfügbare Dateien aus der API Datei einlesen und umbenennen
SXdf <- readRDS("SXdf.VE.rds")

## Beispiel für 10a 

data <- SXdf$`10a.data.v1` %>%
  select(matches("survey|stats|s_|b_|stats|stato|closeTime"))
labels <- SXdf$`10a.labels.v3` %>%
  filter(str_detect(V.Name, "survey|stats|s_|b_|stats|stato|closeTime"))
var.labels <- SXdf$`10a.var.labels.v1` %>%
  filter(str_detect(variableName, "survey|stats|s_|b_|stats|stato|closeTime"))
structure <- SXdf$`10a.structure.v2` %>%
  filter(str_detect(variableName, "survey|stats|s_|b_|stats|stato|closeTime"))


# ## Fragebezeichnungen für das Excel Sheet
# label_auswertung <- var.labels %>%
#   mutate(Item = sub("(\\?).*", "\\1", .$variableDescription)) %>%
#   select(Item) %>%
#   distinct() %>%
#   filter(grepl('\\?', Item)) %>% # alle Fragen nur mit Fragezeichen übernehmen (Hintergrund FB wird ausgeschlossen)
#   mutate(Item = paste("Frage", Item, sep = " "))
# 
# saveRDS(label_auswertung, "Entwickelte Datensätze/Fragebezeichnungen_PSZ_Beratung.rds")


## Struktur Datei aus SX verändern
structure_int <- structure %>%
  mutate(type = case_when(subType == "String" ~ "St",
                          subType == "Multiple" ~ "Mc",
                          subType == "Single" ~ "Si",
                          subType == "Double" ~ "Nu")) %>%
  mutate(questionPrefix = case_when(str_detect(questionName, "s_") ~ "Vordergrund",
                                    str_detect(questionName, "b_") ~ "Hintergrund",
                                    TRUE ~ "System"))
# Gruppierte Fragenummern
structure_helper <- structure_int %>%
  select(questionName, questionPrefix) %>%
  left_join(unique(.) %>%
              group_by(questionPrefix) %>%
              mutate(Fragenummer = 1:n()))

# Fragenummer dranhängen
structure_final <- structure_int %>%
  mutate(Fragenummer = structure_helper$Fragenummer) %>%
  mutate(Fragenummer_final = case_when(questionPrefix == "Vordergrund" ~ paste0("F", .$Fragenummer),
                                       questionPrefix == "Hintergrund" ~ paste0("H", .$Fragenummer),
                                       TRUE ~ paste0("S", .$Fragenummer))) %>% # Systemvariablen
  group_by(Fragenummer_final, type) %>%
  mutate(Nummer = row_number()) %>%
  ungroup() %>%
  mutate(Nummer_final = paste0(.$Fragenummer_final, ".", .$Nummer, "_", .$type)) %>%
  mutate(Nummer_final = str_replace(Nummer_final, " ", ""))



## Variablen Label 
# Auf Item spzeifischen Titel verkürzen (Problem mit Expressions)
var.labels <- var.labels %>%
  mutate(short = sub('.* - ', '', .$variableDescription))

# Value Labels müssen als Vector für jede Variable entsprechend in eine Liste und auf die Variablen-
# spezifische Länge gebracht werden. Nu, und St haben keine Labels.

labels_spread <- labels %>%
  left_join(structure_final, by = c("V.Name" = "variableName")) %>%
  select(Nummer_final, V.Nummer, V.Label) %>%
  split(factor(.$Nummer_final, levels = unique(.$Nummer_final))) %>%
  map(~set_names(.$V.Nummer, .$V.Label))


## Variablen neu im Datensatz benennen und labeln

dataset_Nu <- data %>%
  rename_at(vars(names(.)), ~ structure_final$Nummer_final) %>%
  select(contains("_Nu"))

dataset_St <- data %>%
  rename_at(vars(names(.)), ~ structure_final$Nummer_final) %>%
  select(contains("_St")) %>%
  mutate_all(~as.character(.))

not_any_na <- function(x) all(is.na(x))

dataset_Si_Mc <- data %>%
  rename_at(vars(names(.)), ~ structure_final$Nummer_final) %>%
  select(-contains("_St"), -contains("_Nu")) %>%
  mutate_all(~as_labelled(.)) %>%
  mutate(helper = 0) %>%
  add_row(helper = 1) %>%
  mutate_if(not_any_na, ~ifelse(helper == 1, 1, .)) %>%
  select(-helper) %>%
  set_labels(labels = labels_spread) %>%
  slice(1:(n()-1))

# Die vier Fragetypen in Form der drei erstellten Datensätze zu einem finalen gelabelten Datensatz vereinen

dataset_gelabeled <- dataset_Si_Mc %>%
  bind_cols(dataset_Nu, dataset_St) %>%
  select(structure_final$Nummer_final) %>%  # in die ursprüngliche Form bringen
  set_label(label = var.labels$short)

saveRDS(dataset_gelabeled, "")

# write_spss(x = dataset_Si_Mc, path = "Entwicklungsergebnisse/AMM_gelabeled_real_sj.sav")

