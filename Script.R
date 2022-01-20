#library (readxl)  #Alternative 1, ops readxl does NOT read xlx files from an url....
#library(openxlsx) #Alternative 2, ops... even openxlx throws an error [Error in file(con, "r") : argomento 'description' non valido] and my Excel was CLOSED,...

# Alternative 3, Download and THEN open the file (with readxl) does not work, file donwloaded cannot be opened...

# Alternative 4 works, use HTTR, GET + READXL
library(readxl)
library(httr)
library(tidyverse)

# 01. Load the Data (Accidents 2020)
Year2020 <- 'https://www.aci.it/fileadmin/documenti/studi_e_ricerche/dati_statistiche/incidenti/Localizzazione_Strade_Provinciali_2020.xls'
GET(Year2020, write_disk(file_2020 <- tempfile(fileext = ".xls")))
data_2020 <- data.frame(read_excel(file_2020, sheet = "IMF_COMUNE_SP"))

# 02. Load the Data (Abitanti per Provincia)
Abitanti <- 'https://github.com/MatteoHenryChinaski/Comuni-Italiani-2018-Sql-Json-excel/blob/master/italy_provincies.xlsx?raw=true'
GET(Abitanti, write_disk(file_abitanti_2018 <- tempfile(fileext = ".xlsx")))
abitanti_2018 <- data.frame(read_excel(file_abitanti_2018, sheet = "italy_provincies"))

# 03. Join the inhabitants into the main data table
dialogues_1 <- merge(dialogues, characters[ , c("Character.ID", "Character.Name", "Gender", "House")], by = "Character.ID") #OK
names (abitanti_2018)[2] <- "PROVINCIA"

names (data_2020)[1] <- "REG COD"
names (data_2020)[2] <- "REGIONE"
names (data_2020)[3] <- "PROV COD"
names (data_2020)[4] <- "PROVINCIA"
names (data_2020)[5] <- "STRADA"
names (data_2020)[6] <- "COM COD"
names (data_2020)[7] <- "COMUNE"

data2020_a <- merge(data_2020, abitanti_2018[,c("PROVINCIA","superficie", "residenti")], by = "PROVINCIA")

#tail (data2020_a)
#head (abitanti_2018)

data2020_a %>%
  #filter (REGIONE == "Piemonte" ) %>%
  group_by (REGIONE) %>%
  summarise(two_wheels = sum (Inc.2.ruote.moto), death_rate = (sum(Morti)* 10^5 )/ max (residenti), accident_rate = (sum(Incidenti)* 10^5 )/ max (residenti))%>%
  ggplot() +
  #geom_point(aes(x=REGIONE,y=death_rate), color = "black") +
  #geom_point(aes(x=REGIONE,y=injury_rate), color = "red") +
  geom_point(aes(x=accident_rate,y=death_rate, color = REGIONE)) +
  geom_text (aes(x=accident_rate,y=death_rate+0.2,label = REGIONE)) +
  xlim (0,250) +
  labs(x="Incidenti per 100 mila abitanti", y = "Morti per 100 mila abitanti", title = "Incidenti Mortali (2020) su Strade Provinciali nelle Regioni Italiane (dati ACI)", subtitle = "Morti e Numero di Incidenti hanno una relazione lineare, con Regioni molto più sicure di altre" ) +
  theme_light(base_size = 10) + 
  theme(legend.position = "")
  #theme(axis.text.x=element_text(angle = -90, hjust = 0))


#still under costruction!