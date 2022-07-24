###Achtung, für MA habe ich Muhe falsch berechnet!!! Ich müsste ein Skript VOR 24.6.22 verwenden, um die Resultate zu replizieren

# 2020 & 2021 zusammenfassen
library(tidyverse)
#library(Matrix)
load("DatenTot_final.RData")

data_final_2020 <- DatenTot_final

# Variablen umbenennen damit einheitlich
data_final_2020 <- data_final_2020 %>% 
  rename(VWTheorie = VW11,
         VWBerechnung = VW12,
         VWIndividual = VW13,
         Pflichtlit = pflichtlit,
         Probetest=Probetest_sum,
         ProbetestMuhe=Muhe7)

# Variablen Frage und Relevanz berechnen
data_final_2020 <- data_final_2020 %>% 
  mutate(Relevanz = rowMeans(select(., Relevanz1, Relevanz2, Relevanz3), na.rm = TRUE),
         #Fragen = rowMeans(select(., starts_with("Frage")), na.rm = T),
         Vorlesung = rowMeans(select(., starts_with("vl")), na.rm = TRUE),
         Muhe = rowMeans(select(., starts_with("Muhe")), na.rm = TRUE),
         rechtzeit= rowMeans(select(., starts_with("rechtzeit")), na.rm = TRUE))



A<-cbind(data_final_2020$Muhe,data_final_2020$Vorlesung)

# suffix an 2021 anpassen
data_final_2020 <- data_final_2020 %>% 
  rename_with(~str_replace(., "_A$", "_sb"), .cols = ends_with("_A")) %>% # $ indicates only end of string matching
  rename_with(~str_replace(., "_B$", "_irt12"), .cols = ends_with("_B")) %>% 
  rename_with(~str_replace(., "_C$", "_irt34"), .cols = ends_with("_C")) %>% 
  rename_with(~str_replace(., "_D$", "_cfa12"), .cols = ends_with("_D")) %>% 
  rename_with(~str_replace(., "_E$", "_cfa34"), .cols = ends_with("_E")) %>% 
  rename_with(~str_replace(., "_F$", "_Krit"), .cols = ends_with("_F"))


# Variablen Umbenennen
data_final_2020 <- data_final_2020 %>% 
  rename(F17i_Page_Submit_sb = F17_1_Page_Submit_sb,
         F17i_Click_Count_sb = F17_1_Click_Count_sb,
         F17i_sb = F17_1_sb,
         FF17i_Page_Submit_sb = FF17_1_Page_Submit_sb,
         FF17i_Click_Count_sb = FF17_1_Click_Count_sb,
         )

# rest variable (irt12) erstellen
data_final_2020 <- data_final_2020 %>% 
  mutate(rest = rowSums(select(., itemcharakter, parameterschaetz, spezobjektivi)),
         restRZ = rowMeans(select(., itemcharakterRZ, parameterschaetzRZ, spezobjektiviRZ)),
         restKlick = rowMeans(select(., itemcharakterKlick, parameterschaetzKlick, spezobjektiviKlick)),
         restFRZ = rowMeans(select(., itemcharakterFRZ, parameterschaetzFRZ, spezobjektiviFRZ)),
         restFKlick = rowMeans(select(., itemcharakterFKlick, parameterschaetzFKlick, spezobjektiviFKlick)))

data_final_2020 <- data_final_2020 %>% 
  mutate(Punktezahl = as.numeric(Punktezahl))

# summe oft
data_final_2020 <- data_final_2020 %>% 
  mutate(sum_oft = rowSums(select(., num_sb, num_irt12, num_irt34, num_cfa12, num_cfa34, num_Krit),na.rm=TRUE))

# summe oft
data_final_2020 <- data_final_2020 %>% 
  mutate(Ub1 = rowSums(!is.na(select(.,num_irt12, num_irt34, num_cfa12, num_cfa34, num_Krit))))



#Noten z-standardisieren

#data_final_2020$Note
data_final_2020["Note"][data_final_2020["Note"] ==1] <-NA


#data_final_2020["Note"]<-scale(data_final_2020["Note"])


#import data
data_final_2021 <- read_csv("data_final_alle_vars_2021.csv")
table(data_final_2021$Q55...1119)
a<-data_final_2021$Q55...1119==8
data_final_2021$Q55...1119[a]<-6
data_final_2021$StUB<-7-data_final_2021$Q55...1119

data_final_2020 <- data_final_2020 %>% 
  rename(Pflichtlit_irt12=pflichtlit2,
         Pflichtlit_irt34=pflichtlit3,
         Pflichtlit_cfa12=pflichtlit4,
         Pflichtlit_cfa34=pflichtlit5,
         Pflichtlit_Krit=pflichtlit6,
         Vorlesung_irt12=vl2,
         Vorlesung_irt34=vl3,
         Vorlesung_cfa12=vl4,
         Vorlesung_cfa34=vl5,
         Vorlesung_Krit=vl6,
         num_fragen_sb=FrageA,
         num_fragen_irt12=FrageB,
         num_fragen_irt34=FrageC,
         num_fragen_cfa12=FrageD,
         num_fragen_cfa34=FrageE,
         num_fragen_Krit=FrageF,
         rechtzeitig_sb=rechtzeitig1,
         rechtzeitig_irt12=rechtzeitig2,
         rechtzeitig_irt34=rechtzeitig3,
         rechtzeitig_cfa12=rechtzeitig4,
         rechtzeitig_cfa34=rechtzeitig5,
         rechtzeitig_Krit=rechtzeitig6,
         Muhe_sb=Muhe1,
         Muhe_irt12=Muhe2,
         Muhe_irt34=Muhe3,
         Muhe_cfa12=Muhe4,
         Muhe_cfa34=Muhe5,
         Muhe_Krit=Muhe6)

# nur 1. Termine behalten
data_final_1_termine_2021 <- data_final_2021 %>% 
  select(Pseudo,
         StUB,
         ends_with("_1"),
         Relevanz,
         Punktzahl,
         Forum_lesen,
         Stunden,
         Probetest_sum, F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21,F22,F23,F24,F25)

# Mittelwerte über Vorlesung, Pflichtlit, Fragen & Muhe Variablen
data_final_1_termine_2021 <- data_final_1_termine_2021 %>% 
  mutate(Vorlesung = rowMeans(select(., starts_with("Vorlesung")), na.rm = TRUE),
         Pflichtlit = rowMeans(select(., starts_with("Pflichtlit")), na.rm = TRUE),
         Muhe = rowMeans(select(., starts_with("Muhe")), na.rm = TRUE),
         Fragen = rowMeans(select(., starts_with("num_fragen")), na.rm = TRUE),
         rechtzeit = rowMeans(select(.,starts_with("rechtzeit")),na.rm=TRUE)
         
         )


# remove _1 suffix
data_final_1_termine_2021 <- data_final_1_termine_2021 %>% 
  rename_with(~str_replace(., "_1", ""), .cols = everything())

data_final_1_termine_2021 <- data_final_1_termine_2021 %>% 
  rename(VWTheorie = theory,
         VWBerechnung = practice,
         VWIndividual = indi,
         Punktezahl = Punktzahl,
         Probetest=Probetest_sum)

# summe oft
data_final_1_termine_2021 <- data_final_1_termine_2021 %>% 
  mutate(sum_oft = rowSums(select(., num_pseudo_sb, num_pseudo_irt12, num_pseudo_irt34,
                                  num_pseudo_cfa12, num_pseudo_cfa34, num_pseudo_Krit),na.rm=TRUE))


data_final_1_termine_2021 <- data_final_1_termine_2021 %>% 
  mutate(Ub1 = rowSums(!is.na(select(.,num_pseudo_irt12, num_pseudo_irt34,num_pseudo_cfa12, num_pseudo_cfa34, num_pseudo_Krit))))




data_final_1_termine_2021 <- data_final_1_termine_2021 %>% 
  select(-starts_with("EndDate"),
         -starts_with("Duration"),
         -starts_with("oft"))
  


# get Variable "Note"
library(readxl)
Schlussfrage2022_richtig_March_1_2022_13_02 <- read_excel("Schlussfrage2022 - richtig_March 1, 2022_13.02.xlsx")

Schlussfrage2022_richtig_March_1_2022_13_02 <- Schlussfrage2022_richtig_March_1_2022_13_02 %>%
  select(Q174, Note = Q128, Progress) %>% 
  mutate(Note = tolower(Note),
         Note = as.numeric(Note))


Schlussfrage2022_richtig_March_1_2022_13_02<-Schlussfrage2022_richtig_March_1_2022_13_02  %>% filter(Progress >10)



Schlussfrage2022_richtig_March_1_2022_13_02$Pseudo<-tolower(Schlussfrage2022_richtig_March_1_2022_13_02$Q174)


table(duplicated(Schlussfrage2022_richtig_March_1_2022_13_02$Pseudo))

###was soll ich mit duplikaten machen??? Alle ohne Note Mal raus..
is.na(Schlussfrage2022_richtig_March_1_2022_13_02$Note)

Schlussfrage2022_richtig_March_1_2022_13_02[duplicated(Schlussfrage2022_richtig_March_1_2022_13_02$Pseudo),]
Schlussfrage2022_richtig_March_1_2022_13_02[Schlussfrage2022_richtig_March_1_2022_13_02$Pseudo=="ster02be",]
Schlussfrage2022_richtig_March_1_2022_13_02[Schlussfrage2022_richtig_March_1_2022_13_02$Pseudo=="maan10se",]
Schlussfrage2022_richtig_March_1_2022_13_02[Schlussfrage2022_richtig_March_1_2022_13_02$Pseudo=="maod25",]
Schlussfrage2022_richtig_March_1_2022_13_02[Schlussfrage2022_richtig_March_1_2022_13_02$Pseudo=="ermo11wa",]
Schlussfrage2022_richtig_March_1_2022_13_02[Schlussfrage2022_richtig_March_1_2022_13_02$Pseudo=="magu28ka",]
Schlussfrage2022_richtig_March_1_2022_13_02[Schlussfrage2022_richtig_March_1_2022_13_02$Pseudo=="edru23me",]
Schlussfrage2022_richtig_March_1_2022_13_02[Schlussfrage2022_richtig_March_1_2022_13_02$Pseudo=="kaag22st",]

##ausser in einem Fall stimmen die Noten immer überein, ich nehme also einfach die nicht duplikate, also 6 raus wobei jeweils 1. behalten wird
Schlussfrage2022_richtig_March_1_2022_13_02<-Schlussfrage2022_richtig_March_1_2022_13_02[!duplicated(Schlussfrage2022_richtig_March_1_2022_13_02$Pseudo),]


# zu 2021 Datensatz hinzufügen
data_final_1_termine_2021 <- left_join(data_final_1_termine_2021, Schlussfrage2022_richtig_March_1_2022_13_02)
data_final_1_termine_2021["Note"][data_final_1_termine_2021["Note"] ==1] <-NA
sum(duplicated(data_final_1_termine_2021$Pseudo))
#data_final_1_termine_2021["Note"]<-scale(data_final_1_termine_2021["Note"])
sum(duplicated(data_final_2020$Pseudo))

sum(duplicated(data_final_1_termine_2021$Pseudo))

# Datensätze zusammenfügen,

data_final_2020$StUB<-NA

data_final_2020 <- data_final_2020 %>% select(dplyr::intersect(names(data_final_1_termine_2021), names(data_final_2020)))
data_final_1_termine_2021 <- data_final_1_termine_2021 %>% select(dplyr::intersect(names(data_final_1_termine_2021), names(data_final_2020)))

data_final_2020$Stunden<-as.numeric(data_final_2020$Stunden)
##Hier gibt es duplikate weil auch noch "ungülitge Pseudonyme drin sind, z.B. 1234 wurde evtl in beiden Jahren verwendet
data_final_2020_2021_kombi <- bind_rows(data_final_1_termine_2021, data_final_2020, .id = "Jahr") %>% 
  mutate(Jahr = if_else(Jahr==1, 2021, 2020))


#Exclude Pseudonyms with only numbers > Students dont want to be part of the study
char <- as.character(data_final_2020_2021_kombi$Pseudo)
b <- as.numeric(char)
data_final_2020_2021_kombi <- data_final_2020_2021_kombi[is.na(b),]
sum(duplicated(data_final_1_termine_2021$Pseudo))


#Alle müssen Vorwissen und Probetest haben, bei allen anderen Terminen darf 1 fehlen
a<-is.na(data_final_2020_2021_kombi$VWIndividual)
data_final_2020_2021_kombi<-data_final_2020_2021_kombi[!a,]
b<-is.na(data_final_2020_2021_kombi$Probetest)
data_final_2020_2021_kombi<-data_final_2020_2021_kombi[!b,]

Daten<-data_final_2020_2021_kombi[,c("Jahr","Pseudo","VWIndividual","lokalestoch","adaptiv_testen","r_modelvergleich","invarianz","cutoff","Probetest")]#noch Note und Probetest rein

Daten$na<-rowSums(is.na(Daten))
table(Daten$na)
a<-Daten$na<2

data_final_2020_2021_kombi<-data_final_2020_2021_kombi[a,]

table(data_final_2020_2021_kombi$Jahr)



library("readr")
write_csv(data_final_2020_2021_kombi, "data_final_2020_2021_kombi.csv")

sum(duplicated(data_final_2020_2021_kombi$Pseudo))

