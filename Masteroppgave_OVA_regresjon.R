
# Regresjoner 


# Laster inn pakker
library(tidyverse)
library(scales)
library(stargazer) #denne er for å lage regresjonstabeller
library(openxlsx) # Pakke for å lese inn Excel-fil
library(janitor)

# Laster inn data  
setwd("~/Documents/master")

data <- read.xlsx("11.05-ny-pets-ny versjon-dommer-kodeskjema.xlsx", sheet=2)

# Lager nødvendige variabler 
# Opprett en ny variabel for alvorlighetsgrad
data$alvorlighetsgrad_kategorisert <- ifelse(
  as.numeric(data$antall_hendelser) > 5 & as.numeric(data$samlet_vurdering_varighet_av_vold) > 5, 
  "Høy", 
  ifelse(
    as.numeric(data$antall_hendelser) > 2 & as.numeric(data$samlet_vurdering_varighet_av_vold) > 2, 
    "Moderat", 
    "Lav"
  )
)

#setter referanse for alvorlighetsgrad til lav
data <- data %>% 
  mutate(alvorlighetsgrad_kategorisert = relevel(as.factor(alvorlighetsgrad_kategorisert), 
                                                 ref = "Lav"))


table(data$alvorlighetsgrad_kategorisert)


data <- data %>%
  mutate(kategori = case_when(
    fornærmet_kjønn == "0" ~ "Kun kvinne",
    fornærmet_kjønn == "1" ~ "Kun mann",
    grepl(",", fornærmet_kjønn) ~ "Kombinasjon",
    TRUE ~ "Ukjent"
  )) 

table(data$kategori)

#Dobbelsjekker at det blir riktig
data <- data %>% 
  mutate(rettsinnstans_label = case_when(rettsinnstans == 1 ~ "Tingrett", 
                                         rettsinnstans == 2 ~ "Lagmannsrett"),
         brudd_status = case_when(
             kontakt_besøksforbud_brudd == 0 ~ "Ikke brudd på kontakt/besøksforbud",
             kontakt_besøksforbud_brudd >= 1 ~ "Brudd på kontakt/besøksforbud"
           ))

table(data$brudd_status)

data <- data %>%
  mutate(vold_type = case_when(
    straffeutmåling_fysisk_vold == 1 & straffeutmåling_seksuell_v == 1 ~ "Begge",
    straffeutmåling_fysisk_vold == 1 & straffeutmåling_seksuell_v == 0 ~ "Kun fysisk vold",
    straffeutmåling_fysisk_vold == 0 & straffeutmåling_seksuell_v == 1 ~ "Kun seksuell vold",
    TRUE ~ "Ingen"
  ))



# Bygger modellene 

#starter med kontaktbrudd og retts

model_1 <- lm(as.numeric(idømt_ova) ~ 
                brudd_status , data = data)
summary(model_1)

stargazer(model_1, type = "text")

# Legger til variabler om tiltalte, år og rettsinstans

model_2 <- lm(as.numeric(idømt_ova) ~ brudd_status +
                tiltalt_tilknytet_til_forbudssonen +
                barn_involvert +
                tidligere_voldsdom + 
                tiltalt_tidligere_dømt + 
                as.numeric(årstall) + 
                rettsinnstans_label, data = data)

stargazer(model_1, model_2, type = "text")

# Legger til din variabel om alvorlighetsgrad

model_3 <- lm(as.numeric(idømt_ova) ~ brudd_status +
                tiltalt_tilknytet_til_forbudssonen + 
                barn_involvert +
                tidligere_voldsdom + 
                tiltalt_tidligere_dømt +
                alvorlighetsgrad_kategorisert +
                as.numeric(årstall) + 
                rettsinnstans_label, data = data)

stargazer(model_1, model_2, model_3, type = "text")


# Legger til flere variabler om tiltalte & voldstyper

model_4 <- lm(as.numeric(idømt_ova) ~ brudd_status +
                tiltalt_tilknytet_til_forbudssonen + 
                barn_involvert +
                tidligere_voldsdom + 
                tiltalt_tidligere_dømt +
                alvorlighetsgrad_kategorisert +
                rus_spiller_en_rolle +
                refererer_til_psykiske_plager_hos_tiltalte +
                as.numeric(årstall) + 
                rettsinnstans_label, data = data)

stargazer(model_3, model_4, type = "text")



# Legger til voldstyper og fjerner alvorlighetsgrad


model_5 <- lm(as.numeric(idømt_ova) ~ brudd_status +
                tiltalt_tilknytet_til_forbudssonen + 
                barn_involvert +
                tidligere_voldsdom + 
                tiltalt_tidligere_dømt +
                #alvorlighetsgrad_kategorisert +
                rus_spiller_en_rolle +
                refererer_til_psykiske_plager_hos_tiltalte +
                relevel(as.factor(vold_type), ref = "Ingen" )  +
                as.numeric(årstall) + 
                rettsinnstans_label, data = data)

stargazer(model_4, model_5, type = "text")




# Legger til argumentasjon - altså vektlegger_teknologiske_begreninser_ved_ova 

model_6 <- lm(as.numeric(idømt_ova) ~ brudd_status +
                tiltalt_tilknytet_til_forbudssonen + 
                barn_involvert +
                tidligere_voldsdom + 
                tiltalt_tidligere_dømt +
                alvorlighetsgrad_kategorisert +
                rus_spiller_en_rolle +
                refererer_til_psykiske_plager_hos_tiltalte +
                vektlegger_teknologiske_begreninser_ved_ova +
                as.numeric(årstall) + 
                rettsinnstans_label, data = data)

stargazer(model_4, model_6, type = "text")



stargazer(model_1,
          model_5,
          model_6, 
          type = "html",
          out = "TheasRegresjoner.html")
