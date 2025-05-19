
#deskriptiv statistiskk masteroppgave omvendt voldslarm
# Laster inn pakker -------------------------------------------------------
library(tidyverse)
library(scales)
library(stargazer) #denne er for å lage regresjonstabeller
library(openxlsx) # Pakke for å lese inn Excel-fil
library(janitor)
library(gt)
library(ggplot2)
library(dplyr)


#Laster inn datasett
setwd("~/Documents/master")

data <- read.xlsx("11.05-ny-pets-ny versjon-dommer-kodeskjema.xlsx", sheet=2)


#deskriptiv statisikk OVADATA
################

label_df <- read.xlsx("11.05-ny-pets-ny versjon-dommer-kodeskjema.xlsx", sheet=4)

############# 
#lager firgur 1- andel dommer per år. 
# Tell antall dommer per år
antall_per_år <- table(data$årstall)






# Lag data frame fra tabellen
antall_per_år_linje <- as.data.frame(table(data$årstall))
colnames(antall_per_år_linje) <- c("årstall", "antall")

# Sørg for at årstall er numerisk
antall_per_år_linje$årstall <- as.numeric(as.character(antall_per_år_linje$årstall))

# Plott med linje og punkter
ggplot(antall_per_år_linje, aes(x = årstall, y = antall)) +
  geom_line(color = "#86A4F7", size = 1.2) +      # Mørkeblå linje
  geom_point(color = "#3E31D6", size = 3) +       # Lyseblå punkter
  labs(title = "Antall dommer per år",
       x = "Årstall",
       y = "Antall dommer") +
  theme_bw()

#legger til linje mellom punktene
ggplot(antall_per_år_linje, aes(x = årstall, y = antall, group = 1)) +
  geom_line(color = "#86A4F7", size = 1.2) +   # Linje i ønsket farge
  geom_point(color = "#3E31D6", size = 3) +    # Punkter
  labs(
    title = "Antall dommer per år",
    x = "Årstall",
    y = "Antall dommer"
  ) +
  theme_bw()


antall_per_år_linje %>%
  gt() %>%
  tab_header(title = "Antall dommer per år") %>%
  cols_label(`årstall` = "Årstall", `antall` = "Antall dommer") %>%
  fmt_number(columns = "antall", decimals = 0)




#firgur 2
frekvens_domstol_navn <- table(data$domstol_tall)


# Rens navnene: fjern mellomrom og linjeskift før og etter domstolsnavn
navn <- names(frekvens_domstol_navn)
navn_renset <- trimws(gsub("[\r\n]", "", navn))
names(frekvens_domstol_navn) <- navn_renset

# Konverter til data.frame for ggplot
frekvens_df <- as.data.frame(frekvens_domstol_navn)
colnames(frekvens_df) <- c("Domstol", "Frekvens")
label_df$Number <- as.factor(label_df$Number)
frekvens_df <- frekvens_df %>%
  left_join(label_df, by = c("Domstol" = "Number"))

# Fjern domstoler med frekvens 0 (hvis noen)
frekvens_df <- frekvens_df[frekvens_df$Frekvens > 0, ]


# Lag stolpediagram
ggplot(frekvens_df, aes(x = reorder(Name, Frekvens), y = Frekvens)) +
  geom_bar(stat = "identity", fill = "#86A4F7") +
  coord_flip() +
  theme_bw() +
  labs(
    title = "Frekvensfordeling av domstoler",
    x = "Domstol",
    y = "Frekvens"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.y = element_text(size = 8)
  )






###kontekstuelle forhold tiltalte
# Figur 3: Kjønnsfordeling blant tiltalte
kjonn_frekvens <- data %>%
  filter(tiltalt_kjønn %in% c(0, 1)) %>%
  mutate(Kjønn = factor(tiltalt_kjønn, levels = c(1, 0), labels = c("Mann 97%", "Kvinne 3%"))) %>%
  count(Kjønn)

# Lag kakediagram
ggplot(kjonn_frekvens, aes(x = "", y = n, fill = Kjønn)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  theme_bw() +
  scale_fill_manual(values = c("Mann 97%" = "#86A4F7", "Kvinne 3%" = "#FB6666")) +
  labs(title = "Kjønnsfordeling blant tiltalte", fill = "Kjønn", y = "", x = "") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

################################
#figur 4- andel ova som idømmes
# Omkode idømt_ova til en faktor med labels og ikke verdiene 0 og 1
data <- data %>%
  mutate(idømt_ova_label = factor(idømt_ova, labels = c("Ikke idømt OVA", "Idømt OVA")))

# Oppdatert visualisering uten feilkoder. 
# Oppdatert visualisering uten feilkoder. 
ggplot(data, aes(x = idømt_ova_label, fill = idømt_ova_label)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Ikke idømt OVA" = "#E6ECFF", "Idømt OVA" = "#86A4F7")) +
  labs(title = "Frekvensfordeling av idømt OVA",
       x = "Idømt OVA-status",
       y = "Antall") +
  theme_bw() +
  theme(legend.position = "none")  # Skjuler legend for å unngå redundans

#ønsker også å lage et tilhørende kakediagram#
# Omkode idømt_ova til en faktor med labels
data <- data %>%
  mutate(idømt_ova_label = factor(idømt_ova, labels = c("Ikke idømt OVA", "Idømt OVA")))

# Beregn frekvensfordeling
freq_data <- data %>%
  count(idømt_ova_label) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

# Kakediagram
ggplot(freq_data, aes(x = "", y = n, fill = idømt_ova_label)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar("y", start = 0) +  # Gjør den til et kakediagram
  scale_fill_manual(values = c("Ikke idømt OVA" = "#E6ECFF", "Idømt OVA" = "#86A4F7")) +
  labs(title = "Fordeling av idømt OVA", fill = "Idømt OVA-status") +
  theme_void() +  # Fjerner akser for et rent kakediagram
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5), size = 5)  # Legger til prosentandel i sektorer



#Demografiske forhold blant fornærmede 
#figur 5 
#kjønn, kombinasjoner av kjønn
data %>%
  mutate(fornærmet_kjønn = factor(fornærmet_kjønn)) %>%
  count(fornærmet_kjønn)


data %>%
  mutate(fornærmet_kjønn = case_when(
    str_detect(fornærmet_kjønn, "^1$") ~ "Mann",  #legger på labels
    str_detect(fornærmet_kjønn, "^0$") ~ "Kvinne",  
    str_detect(fornærmet_kjønn, "1,.*0") | str_detect(fornærmet_kjønn, "0,.*1") ~ "Kombinasjon",  
    TRUE ~ "Kombinasjon"  # For alle andre tilfeller, sett til "Kombinasjon"
  )) %>%
  count(fornærmet_kjønn) %>%
  mutate(prosent = n / sum(n) * 100) %>%  # Beregn prosentandel
  ggplot(aes(x = fornærmet_kjønn, y = n, fill = fornærmet_kjønn)) +
  geom_bar(stat = "identity", color = "black") +  # Legg til svart kant rundt stolpene
  scale_fill_manual(values = c("Mann" = "#86A4F7", "Kvinne" = "#FB6666", "Kombinasjon" = "#FDCB87")) +
  theme_bw() +  # Bruker temaet theme_bw
  labs(x = "Fornærmet Kjønn", y = "Antall", title = "Kjønnsfordeling blant fornærmede") +
  geom_text(aes(label = paste0(round(prosent, 1), "%")), vjust = -0.5) +  # Legg til prosentandel på stolpene
  theme(legend.position = "none")  # For å fjerne legenden

#variabel for barn
data %>%
  count(barn_involvert)
# Beregn frekvenser som prosent
prop.table(table(data$barn_involvert)) * 100


#relasjoner mellom partene 
# Frekvenstabell for 'relasjon_mellom_partene'
frekvens_relasjon <- table(data$relasjon_mellom_partene)

# Prosentvis fordeling
prosent_relasjon <- prop.table(frekvens_relasjon) * 100

# Kombinert tabell
frekvens_tabell_relasjon <- data.frame(
  Relasjon = names(frekvens_relasjon),
  Frekvens = as.vector(frekvens_relasjon),
  Prosent = round(as.vector(prosent_relasjon), 1)
)



############har fornærmede hatt mobil voldsalarm
# Frekvenstabell for 'voldsalarm_fornærmede'
frekvens_voldsalarm <- table(data$voldsalarm_fornærmede)


###############
# voldstypene- fysisk vold, psykisk vold og seksuell vold
#ønsker først å se på fordelingen av voldstypene i datasettet. 
# Samler voldstypene i langt format for enklere telling
data_long <- data %>%
  select(straffeutmåling_fysisk_vold, straffeutmåling_psykisk_vold, straffeutmåling_seksuell_v) %>%
  pivot_longer(cols = everything(), names_to = "voldstype", values_to = "forekomst")

# Teller hvor mange som har hver voldstype
frekvens_vold <- data_long %>%
  filter(forekomst == 1) %>%  # Bare de tilfellene hvor voldstypen er til stede
  group_by(voldstype) %>%
  summarise(antall = n(), .groups = "drop")

print(frekvens_vold)




######5.2.3 De tiltalte
#
# Frekvenstabell for 'tiltalt_kjønn'
frekvens_kjønn <- table(data$tiltalt_kjønn)

# Prosentvis fordeling
prosent_kjønn <- prop.table(frekvens_kjønn) * 100

# Kombinert frekvenstabell
frekvens_tabell_kjønn <- data.frame(
  Kjønn = names(frekvens_kjønn),
  Frekvens = as.vector(frekvens_kjønn),
  Prosent = round(as.vector(prosent_kjønn), 1)
)


########årstall tiltalte
# Fjern eventuelle NA-verdier først
fødselsår <- na.omit(data$tiltalt_fødselsår)

# Beregn gjennomsnitt og median
gjennomsnitt <- mean(fødselsår)
medianverdi <- median(fødselsår)

# Skriv ut resultatene
cat("Gjennomsnittlig fødselsår:", gjennomsnitt, "\n")
cat("Median fødselsår:", medianverdi, "\n")



######tiltaltes straffehistorikk
## Frekvenstabell for 'tiltalt_tidligere_dømt'
frekvens_tidligere_dømt <- table(data$tiltalt_tidligere_dømt)

# Prosentvis fordeling
prosent_tidligere_dømt <- prop.table(frekvens_tidligere_dømt) * 100

# Kombinert tabell
frekvens_tabell_tidligere_dømt <- data.frame(
  TidligereDømt = names(frekvens_tidligere_dømt),
  Frekvens = as.vector(frekvens_tidligere_dømt),
  Prosent = round(as.vector(prosent_tidligere_dømt), 1)
)


######
# Frekvenstabell for 'tidligere_voldsdom'
frekvens_voldsdom <- table(data$tidligere_voldsdom)

# Prosentvis fordeling
prosent_voldsdom <- prop.table(frekvens_voldsdom) * 100

# Kombinert tabell
frekvens_tabell_voldsdom <- data.frame(
  TidligereVoldsdom = names(frekvens_voldsdom),
  Frekvens = as.vector(frekvens_voldsdom),
  Prosent = round(as.vector(prosent_voldsdom), 1)
)



##########Relasjonelle forhold tiltalte
#tilknytning til forbudssonen
# Frekvenstabell for 'tiltalt_tilknytet_til_forbudssonen'
frekvens_forbudssone <- table(data$tiltalt_tilknytet_til_forbudssonen)

# Prosentvis fordeling
prosent_forbudssone <- prop.table(frekvens_forbudssone) * 100

# Kombinert tabell
frekvens_tabell_forbudssone <- data.frame(
  Tilknytning = names(frekvens_forbudssone),
  Frekvens = as.vector(frekvens_forbudssone),
  Prosent = round(as.vector(prosent_forbudssone), 1)
)



#######################
#ser på frekvens av rus
# Frekvenstabell for 'rus_spiller_en_rolle'
frekvens_rus <- table(data$rus_spiller_en_rolle)

# Prosentvis fordeling
prosent_rus <- prop.table(frekvens_rus) * 100

# Kombinert tabell
frekvens_tabell_rus <- data.frame(
  RusSpillerEnRolle = names(frekvens_rus),
  Frekvens = as.vector(frekvens_rus),
  Prosent = round(as.vector(prosent_rus), 1)
)



################# 
# Frekvenstabell for 'alkohol_spiller_en_rolle'
frekvens_alkohol <- table(data$alkohol_spiller_en_rolle)

# Prosentvis fordeling
prosent_alkohol <- prop.table(frekvens_alkohol) * 100

# Kombinert tabell
frekvens_tabell_alkohol <- data.frame(
  AlkoholSpillerEnRolle = names(frekvens_alkohol),
  Frekvens = as.vector(frekvens_alkohol),
  Prosent = round(as.vector(prosent_alkohol), 1)
)



########psykiske lidelser
# Frekvenstabell for 'refererer_til_psykiske_plager_hos_tiltalte'
frekvens_psykiske_plager <- table(data$refererer_til_psykiske_plager_hos_tiltalte)

# Prosentvis fordeling
prosent_psykiske_plager <- prop.table(frekvens_psykiske_plager) * 100

# Kombinert tabell
frekvens_tabell_psykiske_plager <- data.frame(
  PsykiskePlager = names(frekvens_psykiske_plager),
  Frekvens = as.vector(frekvens_psykiske_plager),
  Prosent = round(as.vector(prosent_psykiske_plager), 1)
)


############Utfordringer knyttet til rus og psykiatri
#sammenheng mellom rus og psykiatri

data %>%
  count(rus_spiller_en_rolle)

data %>%
  count(refererer_til_psykiske_plager_hos_tiltalte)

#lager krysstabell for å se sammenheng mellom de: 
table_rus_psyk <- table(data$rus_spiller_en_rolle, data$refererer_til_psykiske_plager_hos_tiltalte)



################# 5.3 Når vold blir sak: Innhold og reaksjoner
###figur 7- bygger på voldstyåer
# voldstypene- fysisk vold, psykisk vold og seksuell vold

#ønsker først å se på fordelingen av voldstypene i datasettet. 
# Samler voldstypene i langt format for enklere telling
data_long <- data %>%
  select(straffeutmåling_fysisk_vold, straffeutmåling_psykisk_vold, straffeutmåling_seksuell_v) %>%
  pivot_longer(cols = everything(), names_to = "voldstype", values_to = "forekomst")

#Teller hvor mange som har hver voldstype
frekvens_vold <- data_long %>%
  filter(forekomst == 1) %>%  # Bare de tilfellene hvor voldstypen er til stede
  group_by(voldstype) %>%
  summarise(antall = n(), .groups = "drop")




# Oppdater etiketter og rekkefølge
frekvens_vold_plot <- frekvens_vold %>%
  mutate(
    voldstype_label = case_when(
      voldstype == "straffeutmåling_fysisk_vold" ~ "Fysisk vold",
      voldstype == "straffeutmåling_psykisk_vold" ~ "Psykisk vold",
      voldstype == "straffeutmåling_seksuell_v" ~ "Seksuell vold"
    ),
    farge = case_when(
      voldstype_label == "Psykisk vold" ~ "#86A4F7",
      voldstype_label == "Fysisk vold" ~ "#3E31D6",
      voldstype_label == "Seksuell vold" ~ "#E6ECFF"
    )
  ) %>%
  mutate(
    voldstype_label = factor(
      voldstype_label,
      levels = c("Psykisk vold", "Fysisk vold", "Seksuell vold")  # rekkefølge
    )
  )

# Lag stolpediagram med riktig farge og rekkefølge
ggplot(frekvens_vold_plot, aes(x = voldstype_label, y = antall, fill = voldstype_label)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c(
    "Psykisk vold" = "#86A4F7",
    "Fysisk vold" = "#3E31D6",
    "Seksuell vold" = "#E6ECFF"
  )) +
  labs(
    title = "Forekomst av voldstyper i straffeutmålingen",
    x = "Voldstype",
    y = "Antall"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )


#######digital vold virtuell_trakasering evt evt_virituelle_brudd
## Frekvenstabell
frekvens_virtual <- table(data$virtuell_trakasering_spiller_en_rolle_evt_virituelle_brudd)

# Prosentvis fordeling
prosent_virtual <- prop.table(frekvens_virtual) * 100

# Kombinert tabell
frekvenstabell_virtual <- data.frame(
  Verdi = names(frekvens_virtual),
  Frekvens = as.vector(frekvens_virtual),
  Prosent = round(as.vector(prosent_virtual), 1)
)




#############
#ser på hvor volden utøves
# Frekvenstabell for vold i hjemmet
frekvens_hjemme <- table(data$voldutøvelse_i_fornærmedes_hjem)

# Prosentvis fordeling
prosent_hjemme <- prop.table(frekvens_hjemme) * 100

# Kombinert tabell
frekvenstabell_hjemme <- data.frame(
  Verdi = names(frekvens_hjemme),
  Frekvens = as.vector(frekvens_hjemme),
  Prosent = round(as.vector(prosent_hjemme), 1)
)




##################
#ser på hvor volden utøves
# Frekvenstabell for vold i offentligheten
frekvens_offentlighet <- table(data$voldutøvelse_i_offentlighet, useNA = "ifany")

# Prosentvis fordeling
prosent_offentlighet <- prop.table(frekvens_offentlighet) * 100

# Kombinert tabell
frekvenstabell_offentlighet <- data.frame(
  Verdi = names(frekvens_offentlighet),
  Frekvens = as.vector(frekvens_offentlighet),
  Prosent = round(as.vector(prosent_offentlighet), 1)
)




###########
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


######regner prosent
# Frekvenstabell
frekvenser <- table(data$alvorlighetsgrad_kategorisert)

# Prosentvis fordeling
prosent <- prop.table(frekvenser) * 100

# Kombinert tabell
alvorlighet_tabell <- data.frame(
  Alvorlighetsgrad = names(frekvenser),
  Frekvens = as.vector(frekvenser),
  Prosent = round(as.vector(prosent), 1)
)



#######senskader 
frekvens_senskader <- table(data$senskader, useNA = "ifany")

# Prosentvis fordeling
prosent_senskader <- prop.table(frekvens_senskader) * 100

# Kombinert tabell
frekvenstabell_senskader <- data.frame(
  Verdi = names(frekvens_senskader),
  Frekvens = as.vector(frekvens_senskader),
  Prosent = round(as.vector(prosent_senskader), 1)
)



########figur 8
###brudd på besøksforbud 
# Frekvenstabell (antall)
frekvens_besøksforbud <- table(data$kontakt_besøksforbud_brudd, useNA = "ifany")

# Prosentvis fordeling
prosent_besoksforbud <- prop.table(frekvens_besøksforbud) * 100

# Kombinert tabell
frekvenstabell_besøksforbud <- data.frame(
  Verdi = names(frekvens_besøksforbud),
  Frekvens = as.vector(frekvens_besøksforbud),
  Prosent = round(as.vector(prosent_besoksforbud), 1)
)


# Opprett en frekvenstabell for 'kontakt_besøksforbud_brudd'
frekvens_besoksforbud <- table(data$kontakt_besøksforbud_brudd)

# Opprett en data.frame for plotting
frekvens_besoksforbud_df <- data.frame(
  Verdi = names(frekvens_besoksforbud),
  Frekvens = as.vector(frekvens_besøksforbud)
)

# Sett fargene
frekvens_besoksforbud_df$farge <- ifelse(frekvens_besoksforbud_df$Verdi == "1", "#3E31D6", "#86A4F7")

# Endre verdiene i 'kontakt_besøksforbud_brudd' til 1 for alle >= 1
data$kontakt_besøksforbud_brudd_bivariat <- ifelse(data$kontakt_besøksforbud_brudd >= 1, 1, 0)

# Opprett en frekvenstabell for 'kontakt_besøksforbud_brudd_bivariat'
frekvens_bivariat <- table(data$kontakt_besøksforbud_brudd_bivariat)

# Opprett en data.frame for plotting
frekvens_bivariat_df <- data.frame(
  Verdi = factor(names(frekvens_bivariat), levels = c("0", "1")),
  Frekvens = as.vector(frekvens_bivariat)
)

# Sett fargene for 0 og 1
frekvens_bivariat_df$farge <- ifelse(frekvens_bivariat_df$Verdi == "1", "#86A4F7", "#E6ECFF")

# Endre etikettene fra 0/1 til beskrivende tekst
frekvens_bivariat_df$Verdi <- recode(frekvens_bivariat_df$Verdi,
                                     `0` = "Ikke brudd på kontakt/besøksforbud",
                                     `1` = "Brudd på kontakt/besøksforbud")

# Lag stolpediagram
ggplot(frekvens_bivariat_df, aes(x = Verdi, y = Frekvens, fill = farge)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_identity() +  # Bruk de angitte fargene
  labs(
    title = "Frekvensfordeling av brudd på kontakt/besøksforbud",
    x = "Brudd status",
    y = "Antall"
  ) +
  theme_bw() +  # Bruk temaet
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  )

# Filtrer dataene for de som har 1 på 'kontakt_besøksforbud_brudd_bivariat' og får OVA
filtered_data_besøksforbud <- data[data$kontakt_besøksforbud_brudd_bivariat == 1 & data$idømt_ova == 1, ]

# Beregn antall personer med denne kombinasjonen
antall_kombinasjon <- nrow(filtered_data_besøksforbud)

# Beregn total antall personer med OVA
total_ova <- sum(data$idømt_ova == 1)

# Beregn prosentandelen
prosentandel <- (antall_kombinasjon / total_ova) * 100


####ser på de som ikke får OVA og de som har brudd
# Filtrer dataene for de som ikke får OVA og har 1 på 'kontakt_besøksforbud_brudd_bivariat'
filtered_data_no_ova <- data[data$kontakt_besøksforbud_brudd_bivariat == 1 & data$idømt_ova == 0, ]

# Beregn antall personer med denne kombinasjonen
antall_no_ova <- nrow(filtered_data_no_ova)

# Beregn total antall personer som ikke får OVA
total_no_ova <- sum(data$idømt_ova == 0)

# Beregn prosentandelen
prosentandel_no_ova <- (antall_no_ova / total_no_ova) * 100


#########figur 9
#ser på fordeling av straffereaksjoner
data <- data %>% 
  #Lager en variabel som har verdien 1 hvis dom kodet er kodet som 1 og eller 0 . Kan også ha andre koder, men så lenge 1 er der så får den verdien 1.
  mutate(dom_1 = case_when(str_detect(dom, "1") ~ 1 , 
                           .default = 0),
         #Lager en variabel som har verdien 2 hvis dom kodet er kodet som 1 og eller 0 .
         dom_2 = case_when(str_detect(dom, "2") ~ 1 , 
                           .default = 0),
         #Lager en variabel som har verdien 3 hvis dom kodet er kodet som 1 og eller 0 .
         dom_3 = case_when(str_detect(dom, "3") ~ 1 , 
                           .default = 0),
         #Lager en variabel som har verdien 4 hvis dom kodet er kodet som 1 og eller 0 .
         dom_4 = case_when(str_detect(dom, "4") ~ 1 , 
                           .default = 0))

# Lag frekvenstabell for hver av de fire bivariate variablene
frekvens_dom <- data.frame(
  domtype = c("dom_1", "dom_2", "dom_3", "dom_4"),
  antall = c(
    sum(data$dom_1, na.rm = TRUE),
    sum(data$dom_2, na.rm = TRUE),
    sum(data$dom_3, na.rm = TRUE),
    sum(data$dom_4, na.rm = TRUE)
  )
)


#####
# omkode labels
frekvens_dom$domtype <- recode(frekvens_dom$domtype,
                               "dom_1" = "Fengsel",
                               "dom_2" = "OVA",
                               "dom_3" = "Kontaktforbud",
                               "dom_4" = "Bot")

# Sett rekkefølge eksplisitt (fra høyre til venstre i plottet)
frekvens_dom$domtype <- factor(frekvens_dom$domtype,
                               levels = c("Bot", "Kontaktforbud", "OVA", "Fengsel"))

# Angi fargene
farger <- c("Fengsel" = "#3E31D6",
            "OVA" = "#86A4F7",
            "Kontaktforbud" = "#E6ECFF",
            "Bot" = "#B2B3B7")

# Lag stolpediagram
ggplot(frekvens_dom, aes(x = domtype, y = antall, fill = domtype)) +
 geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = farger) +
 labs(title = "Fordeling av straffereaksjoner",
       x = "Straffereaksjon",
       y = "Antall saker") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "none"
  ) +
  scale_x_discrete(limits = rev(levels(frekvens_dom$domtype)))# Reverserer for høyre-venstre


############
# Frekvenstabell for dissens
table(data$enstemmig_avgjørelse_om_ova)
# med prosent
prop.table(table(data$enstemmig_avgjørelse_om_ova)) * 100


#################for å se sammenheng mellom rettsinnstans og dissens
krysstabell <- table(data$enstemmig_avgjørelse_om_ova, data$rettsinnstans)






