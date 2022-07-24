# Auswertung der Learning Analytics Psychologische Diagnostik 2020

# Bibliotheken laden
library(tidyverse)
library(readxl)
library(dplyr)

# Daten einlesen
Daten1 <- read_excel("Hilfe50Prozent.xlsx",sheet="Standortbestimmung",col_names=TRUE)
Daten2 <- read_excel("Hilfe50Prozent.xlsx",sheet="IRT1",col_names=TRUE)
Daten3 <- read_excel("Hilfe50Prozent.xlsx",sheet="IRT2",col_names=TRUE)
Daten4 <- read_excel("Hilfe50Prozent.xlsx",sheet="CFA1",col_names=TRUE)
Daten5 <- read_excel("Hilfe50Prozent.xlsx",sheet="CFA2",col_names=TRUE)
Daten6 <- read_excel("Hilfe50Prozent.xlsx",sheet="Kriteriumsorientiert",col_names=TRUE)
Daten7 <- read_excel("Hilfe50Prozent.xlsx",sheet="Probeprufung",col_names=TRUE)
Daten8 <- read_excel("Hilfe50Prozent.xlsx",sheet="Schlussfrage",col_names=TRUE)

Daten1 <- Daten1 %>% filter(Progress > 91)
Daten2 <- Daten2 %>% filter(Progress > 91)
Daten3 <- Daten3 %>% filter(Progress > 90)
Daten4 <- Daten4 %>% filter(Progress > 95)
Daten5 <- Daten5 %>% filter(Progress > 93)
Daten6 <- Daten6 %>% filter(Progress > 91)
Daten7<-Daten7  %>% filter(Progress >81)


Daten8<-Daten8  %>% filter(!is.na(Note)) #hier kann man alle rausschmeissen bei denen keine note steht

table(duplicated(Daten1$Duration1,Daten1$EndDate1))
##exakte Dublikate ausschliessen
Daten1<-Daten1[!duplicated(Daten1$Duration1,Daten1$EndDate1),]
Daten2<-Daten2[!duplicated(Daten2$Duration2,Daten2$EndDate2),]
Daten3<-Daten3[!duplicated(Daten3$Duration3,Daten3$EndDate3),]
Daten4<-Daten4[!duplicated(Daten4$Duration4,Daten4$EndDate4),]
Daten5<-Daten5[!duplicated(Daten5$Duration5,Daten5$EndDate5),]
Daten6<-Daten6[!duplicated(Daten6$Duration6,Daten6$EndDate6),]
Daten7<-Daten7[!duplicated(Daten7$Duration7,Daten7$EndDate7),]
Daten8<-Daten8[!duplicated(Daten8),]


# Daten 1 - Standortbestimmung -------------------------------------------------
# Variablen umbenennen:
colnames(Daten1) <- gsub(" ", "_", colnames(Daten1)) # Leerschläge in "Click Count" und "Page Submit" durch "_" ersetzen

# Im Datensatz der Standordbestimmung heisst die variable `FF17_1_Click Count` Count noch
# `Q185_Click Count` dehlab umbenennen!
#@Jonas: war glaub umgekehrt, habs korrieirt
Daten1 <- Daten1 %>% rename(`FF17_1_Click_Count`=`Q185_Click_Count`)


# Variablen in R/F codieren & Summenscore bei k Prim
# F1 (2 ist korrekt) dichotomisieren
Daten1$F1 <- ifelse(Daten1$F1 == 2, 1, 0)

# F2 k Prim Score
Daten1$F2_2 <- ifelse(Daten1$F2_2 == 4, 0.25, 0)
Daten1$F2_3 <- ifelse(Daten1$F2_3 == 3, 0.25, 0)
Daten1$F2_4 <- ifelse(Daten1$F2_4 == 2, 0.25, 0)
Daten1$F2_5 <- ifelse(Daten1$F2_5 == 1, 0.25, 0)
# F2 Summenscore
Daten1$F2 <- rowSums(select(Daten1, F2_2:F2_5))
Daten1 <- relocate(Daten1, F2, .after = F2_5)

# F3 k Prim Score
Daten1$F3_1 <- ifelse(Daten1$F3_1 == 1, 0.25, 0)
Daten1$F3_2 <- ifelse(Daten1$F3_2 == 2, 0.25, 0)
Daten1$F3_3 <- ifelse(Daten1$F3_3 == 1, 0.25, 0)
Daten1$F3_4 <- ifelse(Daten1$F3_4 == 1, 0.25, 0)
# F3 Summenscore
Daten1$F3 <- rowSums(select(Daten1, F3_1:F3_4))
Daten1 <- relocate(Daten1, F3, .after = F3_4)

# F3i k Prim Score
Daten1$F3i_1 <- ifelse(Daten1$F3i_1 == 2, 0.25, 0)
Daten1$F3i_2 <- ifelse(Daten1$F3i_2 == 1, 0.25, 0)
Daten1$F3i_3 <- ifelse(Daten1$F3i_3 == 1, 0.25, 0)
Daten1$F3i_4 <- ifelse(Daten1$F3i_4 == 2, 0.25, 0)
# F3i Summenscore
Daten1$F3i <- rowSums(select(Daten1, F3i_1:F3i_4))
Daten1 <- relocate(Daten1, F3i, .after = F3i_4)

# F3ii k Prim Score
Daten1$F3ii_1 <- ifelse(Daten1$F3ii_1 == 1, 0.25, 0)
Daten1$F3ii_2 <- ifelse(Daten1$F3ii_2 == 1, 0.25, 0)
Daten1$F3ii_3 <- ifelse(Daten1$F3ii_3 == 1, 0.25, 0)
Daten1$F3ii_4 <- ifelse(Daten1$F3ii_4 == 1, 0.25, 0)
# F3ii Summenscore
Daten1$F3ii <- rowSums(select(Daten1, F3ii_1:F3ii_4))
Daten1 <- relocate(Daten1, F3ii, .after = F3ii_4)

# F4 k Prim Score
Daten1$F4_1 <- ifelse(Daten1$F4_1 == 1, 0.25, 0)
Daten1$F4_2 <- ifelse(Daten1$F4_2 == 2, 0.25, 0)
Daten1$F4_3 <- ifelse(Daten1$F4_3 == 1, 0.25, 0)
Daten1$F4_4 <- ifelse(Daten1$F4_4 == 1, 0.25, 0)
# F4 Summenscore
Daten1$F4 <- rowSums(select(Daten1, F4_1:F4_4))
Daten1 <- relocate(Daten1, F4, .after = F4_4)

# F5 k Prim Score
Daten1$F5_1 <- ifelse(Daten1$F5_1 == 1, 0.25, 0)
Daten1$F5_2 <- ifelse(Daten1$F5_2 == 2, 0.25, 0)
Daten1$F5_3 <- ifelse(Daten1$F5_3 == 1, 0.25, 0)
Daten1$F5_4 <- ifelse(Daten1$F5_4 == 2, 0.25, 0)
# F5 Summenscore
Daten1$F5 <- rowSums(select(Daten1, F5_1:F5_4))
Daten1 <- relocate(Daten1, F5, .after = F5_4)

# F6 (2 ist korrekt) dichotomisieren
Daten1$F6 <- ifelse(Daten1$F6 == 2, 1, 0)

# F7 (2 ist korrekt) dichotomisieren
Daten1$F7 <- ifelse(Daten1$F7 == 4, 1, 0)

# F8 k Prim Score
Daten1$F8_1 <- ifelse(Daten1$F8_1 == 2, 0.25, 0)
Daten1$F8_2 <- ifelse(Daten1$F8_2 == 1, 0.25, 0)
Daten1$F8_3 <- ifelse(Daten1$F8_3 == 1, 0.25, 0)
Daten1$F8_4 <- ifelse(Daten1$F8_4 == 1, 0.25, 0)
# F8 Summenscore
Daten1$F8 <- rowSums(select(Daten1, F8_1:F8_4))
Daten1 <- relocate(Daten1, F8, .after = F8_4)

# F9 k Prim Score
Daten1$F9_1 <- ifelse(Daten1$F9_1 == 1, 0.2, 0)
Daten1$F9_2 <- ifelse(Daten1$F9_2 == 1, 0.2, 0)
Daten1$F9_3 <- ifelse(Daten1$F9_3 == 2, 0.2, 0)
Daten1$F9_4 <- ifelse(Daten1$F9_4 == 2, 0.2, 0)
Daten1$F9_5 <- ifelse(Daten1$F9_5 == 2, 0.2, 0)
# F9 Summenscore
Daten1$F9 <- rowSums(select(Daten1, F9_1:F9_5))
Daten1 <- relocate(Daten1, F9, .after = F9_5)

# F10 k Prim Score
Daten1$F10_1 <- ifelse(Daten1$F10_1 == 1, 0.25, 0)
Daten1$F10_2 <- ifelse(Daten1$F10_2 == 2, 0.25, 0)
Daten1$F10_3 <- ifelse(Daten1$F10_3 == 1, 0.25, 0)
Daten1$F10_4 <- ifelse(Daten1$F10_4 == 2, 0.25, 0)
# F10 Summenscore
Daten1$F10 <- rowSums(select(Daten1, F10_1:F10_4))
Daten1 <- relocate(Daten1, F10, .after = F10_4)

# F11 k Prim Score
Daten1$F11_1 <- ifelse(Daten1$F11_1 == 1, (1/3), 0)
Daten1$F11_2 <- ifelse(Daten1$F11_2 == 1, (1/3), 0)
Daten1$F11_3 <- ifelse(Daten1$F11_3 == 2, (1/3), 0)
# F11 Summenscore
Daten1$F11 <- rowSums(select(Daten1, F11_1:F11_3))
Daten1 <- relocate(Daten1, F11, .after = F11_3)

# F12 (6 ist korrekt) dichotomisieren
Daten1$F12 <- ifelse(Daten1$F12 == 6, 1, 0)

# F13 (3 ist korrekt) dichotomisieren
Daten1$F13 <- ifelse(Daten1$F13 == 3, 1, 0)


# F14 (6 ist korrekt) dichotomisieren
Daten1$F14 <- ifelse(Daten1$F14 == 6, 1, 0)

# Keine F15

# F16 k Prim Score
Daten1$F16_1 <- ifelse(Daten1$F16_1 == 2, 0.25, 0)
Daten1$F16_2 <- ifelse(Daten1$F16_2 == 1, 0.25, 0)
Daten1$F16_3 <- ifelse(Daten1$F16_3 == 1, 0.25, 0)
Daten1$F16_4 <- ifelse(Daten1$F16_4 == 2, 0.25, 0)
# F16 Summenscore
Daten1$F16 <- rowSums(select(Daten1, F16_1:F16_4))
Daten1 <- relocate(Daten1, F16, .after = F16_4)

# F17 (3 ist korrekt) dichotomisieren
Daten1$F17 <- ifelse(Daten1$F17 == 3, 1, 0)

# F17_1 (2 ist korrekt) dichotomisieren
Daten1$F17_1 <- ifelse(Daten1$F17_1 == 2, 1, 0)

# F18 (3 ist korrekt) dichotomisieren
Daten1$F18 <- ifelse(Daten1$F18 == 3, 1, 0)

# F19 (1 ist korrekt) dichotomisieren
Daten1$F19 <- ifelse(Daten1$F19 == 1, 1, 0)

# F20 (7 ist korrekt) dichotomisieren
Daten1$F20 <- ifelse(Daten1$F20 == 7, 1, 0)

# F21 (2 ist korrekt) dichotomisieren
Daten1$F21 <- ifelse(Daten1$F21 == 2, 1, 0)

# Keine F22

# F23 (3 ist korrekt) dichotomisieren
Daten1$F23 <- ifelse(Daten1$F23 == 3, 1, 0)

# F24 (2 ist korrekt) dichotomisieren
Daten1$F24 <- ifelse(Daten1$F24 == 2, 1, 0)

# F25 (1 ist korrekt) dichotomisieren
Daten1$F25 <- ifelse(Daten1$F25 == 1, 1, 0)

# F26 (2 ist korrekt) dichotomisieren
Daten1$F26 <- ifelse(Daten1$F26 == 2, 1, 0)

# F27 (1 ist korrekt) dichotomisieren
Daten1$F27 <- ifelse(Daten1$F27 == 1, 1, 0)

# Daten 2 - IRT 1/2 ------------------------------------------------------------
Daten2 <- Daten2 %>%
  rename(F1_Page_Submit = `Q38_Page Submit`,
         F1_Click_Count = `Q38_Click Count`,
         FF1_Page_Submit = `Q21_Page Submit`,
         FF1_Click_Count = `Q21_Click Count`,
         
         F2_Page_Submit = `Q39_Page Submit`,
         F2_Click_Count = `Q39_Click Count`,
         FF2_Page_Submit = `Q22_Page Submit`,
         FF2_Click_Count = `Q22_Click Count`,
         
         #F3_Page_Submit = -> in Qualtrics hier Q84 Timingvariable welche in diesem Datensatz nicht enthalten ist
         #F3_Click_Count = -> in Qualtrics hier Q84 Timingvariable welche in diesem Datensatz nicht enthalten ist
         FF3_Page_Submit = `Q40_Page Submit`,
         FF3_Click_Count = `Q40_Click Count`,
         
         F4_Page_Submit = `Q70_Page Submit`,
         F4_Click_Count = `Q70_Click Count`,
         FF4_Page_Submit = `Q71_Page Submit`,
         FF4_Click_Count = `Q71_Click Count`,
         
         F5_Page_Submit = `Q72_Page Submit`,
         F5_Click_Count = `Q72_Click Count`,
         FF5_Page_Submit = `Q30_Page Submit`,
         FF5_Click_Count = `Q30_Click Count`,
         
         F6_Page_Submit = `Q40_Page Submit_3`,
         F6_Click_Count = `Q40_Click Count_4`,
         FF6_Page_Submit = `Q36_Page Submit`,
         FF6_Click_Count = `Q36_Click Count`,
         
         F7_Page_Submit = `Q37_Page Submit`,
         F7_Click_Count = `Q37_Click Count`,
         FF7_Page_Submit = `Q41_Page Submit`,
         FF7_Click_Count = `Q41_Click Count`,
         
         F8_Page_Submit = `Q81_Page Submit`,
         F8_Click_Count = `Q81_Click Count`,
         # F8 und F9 sind eine Frage, Antwort erst nach F9 deshalb kein FF8_Page_Submit & FF8_Click_Count
         
         F9_Page_Submit = `Q43_Page Submit`,
         F9_Click_Count = `Q43_Click Count`,
         FF9_Page_Submit = `Q50_Page Submit`,
         FF9_Click_Count = `Q50_Click Count`,
         
         F10_Page_Submit = `Q51_Page Submit`,
         F10_Click_Count = `Q51_Click Count`,
         FF10_Page_Submit = `Q52_Page Submit`,
         FF10_Click_Count = `Q52_Click Count`,
         
         F11_Page_Submit = `Q54_Page Submit`,
         F11_Click_Count = `Q54_Click Count`,
         FF11_Page_Submit = `Q56_Page Submit`,
         FF11_Click_Count = `Q56_Click Count`,
         
         F12_Page_Submit = `Q61_Page Submit`,
         F12_Click_Count = `Q61_Click Count`,
         FF12_Page_Submit = `Q62_Page Submit`,
         FF12_Click_Count = `Q62_Click Count`,
         
         F13_Page_Submit = `Q63_Page Submit`,
         F13_Click_Count = `Q63_Click Count`,
         FF13_Page_Submit = `Q64_Page Submit`,
         FF13_Click_Count = `Q64_Click Count`,
         
         F14_Page_Submit = `Q66_Page Submit`,
         F14_Click_Count = `Q66_Click Count`,
         FF14_Page_Submit = `Q68_Page Submit`,
         FF14_Click_Count = `Q68_Click Count`)


# Variablen in R/F codieren & Summenscore bei k Prim
# F1 k Prim Score
Daten2$F1_1 <- ifelse(Daten2$F1_1 == 1, 0.25, 0)
Daten2$F1_2 <- ifelse(Daten2$F1_2 == 2, 0.25, 0)
Daten2$F1_3 <- ifelse(Daten2$F1_3 == 2, 0.25, 0)
Daten2$F1_4 <- ifelse(Daten2$F1_4 == 2, 0.25, 0)
# F1 Summenscore
Daten2$F1 <- rowSums(select(Daten2, F1_1, F1_2, F1_3, F1_4))
Daten2 <- relocate(Daten2, F1, .after = F1_4)

# F2 (1 ist korrekt) dichotomisieren
Daten2$F2 <- ifelse(Daten2$F2 == 1, 1, 0)

# F3 k Prim Score
Daten2$F3_1 <- ifelse(Daten2$F3_1 == 1, 0.25, 0)
Daten2$F3_2 <- ifelse(Daten2$F3_2 == 2, 0.25, 0)
Daten2$F3_3 <- ifelse(Daten2$F3_3 == 2, 0.25, 0)
Daten2$F3_4 <- ifelse(Daten2$F3_4 == 2, 0.25, 0)
# F3 Summenscore
Daten2$F3 <- rowSums(select(Daten2, F3_1, F3_2, F3_3, F3_4))
Daten2 <- relocate(Daten2, F3, .after = F3_4)

# F4 (1 ist korrekt) dichotomisieren
Daten2$F4 <- ifelse(Daten2$F4 == 1, 1, 0)

# F5 k Prim Score
Daten2$F5_1 <- ifelse(Daten2$F5_1 == 1, 0.2, 0)
Daten2$F5_2 <- ifelse(Daten2$F5_2 == 2, 0.2, 0)
Daten2$F5_3 <- ifelse(Daten2$F5_3 == 1, 0.2, 0)
Daten2$F5_4 <- ifelse(Daten2$F5_4 == 1, 0.2, 0)
Daten2$F5_5 <- ifelse(Daten2$F5_5 == 1, 0.2, 0)
# F5 Summenscore
Daten2$F5 <- rowSums(select(Daten2, F5_1, F5_2, F5_3, F5_4, F5_5))
Daten2 <- relocate(Daten2, F5, .after = F5_5)

# F6 k Prim Score
Daten2$F6_1 <- ifelse(Daten2$F6_1 == 1, (1/3), 0)
Daten2$F6_2 <- ifelse(Daten2$F6_2 == 2, (1/3), 0)
Daten2$F6_3 <- ifelse(Daten2$F6_3 == 1, (1/3), 0)
# F6 Summenscore
Daten2$F6 <- rowSums(select(Daten2, F6_1, F6_2, F6_3))
Daten2 <- relocate(Daten2, F6, .after = F6_3)

# F7 k Prim Score
Daten2$F7_1 <- ifelse(Daten2$F7_1 == 1, (1/3), 0)
Daten2$F7_2 <- ifelse(Daten2$F7_2 == 1, (1/3), 0)
Daten2$F7_3 <- ifelse(Daten2$F7_3 == 1, (1/3), 0)
# F7 Summenscore
Daten2$F7 <- rowSums(select(Daten2, F7_1, F7_2, F7_3))
Daten2 <- relocate(Daten2, F7, .after = F7_3)

# F8 k Prim Score
Daten2$F8_1 <- ifelse(Daten2$F8_1 == 2, (1/3), 0)
Daten2$F8_2 <- ifelse(Daten2$F8_2 == 1, (1/3), 0)
Daten2$F8_3 <- ifelse(Daten2$F8_3 == 1, (1/3), 0)
# F8 Summenscore
#Daten2$F8 <- rowSums(select(Daten2, F8_1, F8_2, F8_3))
#Daten2 <- relocate(Daten2, F8, .after = F8_3)

# F9 k Prim Score
Daten2$F9_1 <- ifelse(Daten2$F9_1 == 1, (1/3), 0)
Daten2$F9_2 <- ifelse(Daten2$F9_2 == 2, (1/3), 0)
Daten2$F9_3 <- ifelse(Daten2$F9_3 == 1, (1/3), 0)
# F9 Summenscore
Daten2$F9 <- rowSums(select(Daten2,F8_1, F8_2, F8_3, F9_1, F9_2, F9_3))
Daten2 <- relocate(Daten2, F9, .after = F9_3)

# F10 k Prim Score
Daten2$F10_1 <- ifelse(Daten2$F10_1 == 1, 0.25, 0)
Daten2$F10_2 <- ifelse(Daten2$F10_2 == 2, 0.25, 0)
Daten2$F10_3 <- ifelse(Daten2$F10_3 == 2, 0.25, 0)
Daten2$F10_4 <- ifelse(Daten2$F10_4 == 1, 0.25, 0)
# F10 Summenscore
Daten2$F10 <- rowSums(select(Daten2, F10_1, F10_2, F10_3, F10_4))
Daten2 <- relocate(Daten2, F10, .after = F10_4)

# F11 (2 ist korrekt) dichotomisieren
Daten2$F11 <- ifelse(Daten2$F11 == 2, 1, 0)

# F12 k Prim Score
Daten2$F12_1 <- ifelse(Daten2$F12_1 == 1, 0.25, 0)
Daten2$F12_2 <- ifelse(Daten2$F12_2 == 2, 0.25, 0)
Daten2$F12_3 <- ifelse(Daten2$F12_3 == 1, 0.25, 0)
Daten2$F12_4 <- ifelse(Daten2$F12_4 == 2, 0.25, 0)
# F12 Summenscore
Daten2$F12 <- rowSums(select(Daten2, F12_1, F12_2, F12_3, F12_4))
Daten2 <- relocate(Daten2, F12, .after = F12_4)

# F13 k Prim Score
Daten2$F13_1 <- ifelse(Daten2$F13_1 == 2, 0.25, 0)
Daten2$F13_2 <- ifelse(Daten2$F13_2 == 2, 0.25, 0)
Daten2$F13_3 <- ifelse(Daten2$F13_3 == 1, 0.25, 0)
Daten2$F13_4 <- ifelse(Daten2$F13_4 == 1, 0.25, 0)
# F13 Summenscore
Daten2$F13 <- rowSums(select(Daten2, F13_1, F13_2, F13_3, F13_4))
Daten2 <- relocate(Daten2, F13, .after = F13_4)

# F14 k Prim Score
Daten2$F14_1 <- ifelse(Daten2$F14_1 == 1, 0.25, 0)
Daten2$F14_2 <- ifelse(Daten2$F14_2 == 1, 0.25, 0)
Daten2$F14_3 <- ifelse(Daten2$F14_3 == 2, 0.25, 0)
Daten2$F14_4 <- ifelse(Daten2$F14_4 == 3, 0.25, 0)
# F14 Summenscore
Daten2$F14 <- rowSums(select(Daten2, F14_1, F14_2, F14_3, F14_4))
Daten2 <- relocate(Daten2, F14, .after = F14_4)


# Daten 3 - IRT 3/4 ------------------------------------------------------------
Daten3 <- Daten3 %>%
  rename(F1_Page_Submit = `Q55_Page Submit`,
         F1_Click_Count = `Q55_Click Count`,
         FF1_Page_Submit = `Q57_Page Submit`,
         FF1_Click_Count = `Q57_Click Count`,
         
         F2_Page_Submit = `Q58_Page Submit`,
         F2_Click_Count = `Q58_Click Count`,
         FF2_Page_Submit = `Q59_Page Submit`,#q59 fehlt teilweise und muss aus berechnung raus
         FF2_Click_Count = `Q59_Click Count`,
         
         F3_Page_Submit = `Q60_Page Submit`,
         F3_Click_Count = `Q60_Click Count`,
         FF3_Page_Submit = `Q62_Page Submit`,
         FF3_Click_Count = `Q62_Click Count`,
         
         F4_Page_Submit = `Q63_Page Submit`,
         F4_Click_Count = `Q63_Click Count`,
         FF4_Page_Submit = `Q64_Page Submit`,
         FF4_Click_Count = `Q64_Click Count`,
         
         F5_Page_Submit = `Q65_Page Submit`,
         F5_Click_Count = `Q65_Click Count`,
         FF5_Page_Submit = `Q67_Page Submit`,
         FF5_Click_Count = `Q67_Click Count`,
         
         F6_Page_Submit = `Q68_Page Submit`,
         F6_Click_Count = `Q68_Click Count`,
         FF6_Page_Submit = `Q69_Page Submit`,
         FF6_Click_Count = `Q69_Click Count`,
         
         F7_Page_Submit = `Q70_Page Submit`,
         F7_Click_Count = `Q70_Click Count`,
         FF7_Page_Submit = `Q71_Page Submit`,
         FF7_Click_Count = `Q71_Click Count`,
         
         F8_Page_Submit = `Q72_Page Submit`,
         F8_Click_Count = `Q72_Click Count`,
         FF8_Page_Submit = `Q75_Page Submit`,
         FF8_Click_Count = `Q75_Click Count`,
         
         F9_Page_Submit = `Q76_Page Submit`,
         F9_Click_Count = `Q76_Click Count`,
         FF9_Page_Submit = `Q77_Page Submit`,
         FF9_Click_Count = `Q77_Click Count`,
         
         F10_Page_Submit = `Q78_Page Submit`,
         F10_Click_Count = `Q78_Click Count`,
         FF10_Page_Submit = `Q79_Page Submit`,
         FF10_Click_Count = `Q79_Click Count`,
         
         F11_Page_Submit = `Q80_Page Submit`,
         F11_Click_Count = `Q80_Click Count`,
         FF11_Page_Submit = `Q81_Page Submit`,
         FF11_Click_Count = `Q81_Click Count`,
         
         F12_Page_Submit = `Q82_Page Submit`,
         F12_Click_Count = `Q82_Click Count`,
         FF12_Page_Submit = `Q83_Page Submit`,
         FF12_Click_Count = `Q83_Click Count`)


# Variablen in R/F codieren & Summenscore bei k Prim
# F1 k Prim Score
Daten3$F1_1 <- ifelse(Daten3$F1_1 == 1, 0.2, 0)
Daten3$F1_2 <- ifelse(Daten3$F1_2 == 9, 0.2, 0)
Daten3$F1_3 <- ifelse(Daten3$F1_3 == 1, 0.2, 0)
Daten3$F1_4 <- ifelse(Daten3$F1_4 == 9, 0.2, 0)
Daten3$F1_5 <- ifelse(Daten3$F1_4 == 1, 0.2, 0)
# F1 Summenscore
Daten3$F1 <- rowSums(select(Daten3, F1_1, F1_2, F1_3, F1_4, F1_5))
Daten3 <- relocate(Daten3, F1, .after = F1_5)

# F2 (1 ist korrekt) dichotomisieren
Daten3$F2 <- ifelse(Daten3$F2 == 1, 1, 0)

# F3 k Prim Score
Daten3$F3_1 <- ifelse(Daten3$F3_1 == 2, 0.25, 0)
Daten3$F3_2 <- ifelse(Daten3$F3_2 == 1, 0.25, 0)
Daten3$F3_3 <- ifelse(Daten3$F3_3 == 2, 0.25, 0)
Daten3$F3_4 <- ifelse(Daten3$F3_4 == 1, 0.25, 0)
# F3 Summenscore
Daten3$F3 <- rowSums(select(Daten3, F3_1, F3_2, F3_3, F3_4))
Daten3 <- relocate(Daten3, F3, .after = F3_4)

# F4 (1 ist korrekt) dichotomisieren
Daten3$F4 <- ifelse(Daten3$F4 == 1, 1, 0)

# F5 k Prim Score
Daten3$F5_1 <- ifelse(Daten3$F5_1 == 2, 0.25, 0)
Daten3$F5_2 <- ifelse(Daten3$F5_2 == 1, 0.25, 0)
Daten3$F5_3 <- ifelse(Daten3$F5_3 == 1, 0.25, 0)
Daten3$F5_4 <- ifelse(Daten3$F5_4 == 1, 0.25, 0)

# F5 Summenscore
Daten3$F5 <- rowSums(select(Daten3, F5_1, F5_2, F5_3, F5_4))
Daten3 <- relocate(Daten3, F5, .after = F5_4)

# F6 k Prim Score
Daten3$F6_1 <- ifelse(Daten3$F6_1 == 1, 0.25, 0)
Daten3$F6_2 <- ifelse(Daten3$F6_2 == 2, 0.25, 0)
Daten3$F6_3 <- ifelse(Daten3$F6_3 == 2, 0.25, 0)
Daten3$F6_4 <- ifelse(Daten3$F6_3 == 1, 0.25, 0)
# F6 Summenscore
Daten3$F6 <- rowSums(select(Daten3, F6_1, F6_2, F6_3, F6_4))
Daten3 <- relocate(Daten3, F6, .after = F6_4)

# F7 (2 ist korrekt) dichotomisieren
Daten3$F7 <- ifelse(Daten3$F7 == 2, 1, 0)

# F8 (4 ist korrekt) dichotomisieren
Daten3$F8 <- ifelse(Daten3$F8 == 4, 1, 0)

# F9 k Prim Score
Daten3$F9_1 <- ifelse(Daten3$F9_1 == 2, 0.25, 0)
Daten3$F9_2 <- ifelse(Daten3$F9_2 == 2, 0.25, 0)
Daten3$F9_3 <- ifelse(Daten3$F9_3 == 1, 0.25, 0)
Daten3$F9_4 <- ifelse(Daten3$F9_4 == 2, 0.25, 0)
# F9 Summenscore
Daten3$F9 <- rowSums(select(Daten3, F9_1, F9_2, F9_3, F9_4))
Daten3 <- relocate(Daten3, F9, .after = F9_4)

# F10 k Prim Score
Daten3$F10_1 <- ifelse(Daten3$F10_1 == 2, 0.25, 0)
Daten3$F10_2 <- ifelse(Daten3$F10_2 == 1, 0.25, 0)
Daten3$F10_3 <- ifelse(Daten3$F10_3 == 1, 0.25, 0)
Daten3$F10_4 <- ifelse(Daten3$F10_4 == 1, 0.25, 0)
# F10 Summenscore
Daten3$F10 <- rowSums(select(Daten3, F10_1, F10_2, F10_3, F10_4))
Daten3 <- relocate(Daten3, F10, .after = F10_4)

# F11 k Prim Score
Daten3$F11_1 <- ifelse(Daten3$F11_1 == 2, 0.25, 0)
Daten3$F11_2 <- ifelse(Daten3$F11_2 == 2, 0.25, 0)
Daten3$F11_3 <- ifelse(Daten3$F11_3 == 1, 0.25, 0)
Daten3$F11_4 <- ifelse(Daten3$F11_4 == 1, 0.25, 0)
# F10 Summenscore
Daten3$F11 <- rowSums(select(Daten3, F11_1, F11_2, F11_3, F11_4))
Daten3 <- relocate(Daten3, F11, .after = F11_4)

# F12 (5 ist korrekt) dichotomisieren
Daten3$F12 <- ifelse(Daten3$F12 == 5, 1, 0)


# Daten 4 - CFA1 ---------------------------------------------------------------
Daten4 <- Daten4 %>%
  rename(F1_Page_Submit = `Q31_Page Submit`,
         F1_Click_Count = `Q31_Click Count`,
         FF1_Page_Submit = `Q95_Page Submit`,
         FF1_Click_Count = `Q95_Click Count`,
         
         F2_Page_Submit = `Q42_Page Submit`,
         F2_Click_Count = `Q42_Click Count`,
         FF2_Page_Submit = `Q96_Page Submit`,
         FF2_Click_Count = `Q96_Click Count`,
         
         F3_Page_Submit = `Q50_Page Submit`,
         F3_Click_Count = `Q50_Click Count`,
         FF3_Page_Submit = `Q97_Page Submit`,
         FF3_Click_Count = `Q97_Click Count`,
         
         F4_Page_Submit = `Q51_Page Submit`,
         F4_Click_Count = `Q51_Click Count`,
         FF4_Page_Submit = `Q98_Page Submit`,
         FF4_Click_Count = `Q98_Click Count`,
         
         # Keine F5
         
         F6_Page_Submit = `Q53_Page Submit`,
         F6_Click_Count = `Q53_Click Count`,
         FF6_Page_Submit = `Q91_Page Submit`,
         FF6_Click_Count = `Q91_Click Count`,
         
         F7_Page_Submit = `Q54_Page Submit`,
         F7_Click_Count = `Q54_Click Count`,
         FF7_Page_Submit = `Q99_Page Submit`,
         FF7_Click_Count = `Q99_Click Count`,
         
         F8_Page_Submit = `Q55_Page Submit`,
         F8_Click_Count = `Q55_Click Count`,
         FF8_Page_Submit = `Q100_Page Submit`,
         FF8_Click_Count = `Q100_Click Count`,
         
         F9_Page_Submit = `Q56_Page Submit`,
         F9_Click_Count = `Q56_Click Count`,
         FF9_Page_Submit = `Q101_Page Submit`,
         FF9_Click_Count = `Q101_Click Count`,
         
         F10_Page_Submit = `Q57_Page Submit`,
         F10_Click_Count = `Q57_Click Count`,
         FF10_Page_Submit = `Q102_Page Submit`,
         FF10_Click_Count = `Q102_Click Count`,
         
         F11_Page_Submit = `Q58_Page Submit`,
         F11_Click_Count = `Q58_Click Count`,
         FF11_Page_Submit = `Q103_Page Submit`,
         FF11_Click_Count = `Q103_Click Count`,
         
         F12_Page_Submit = `Q59_Page Submit`,
         F12_Click_Count = `Q59_Click Count`,
         FF12_Page_Submit = `Q63_Page Submit`,
         FF12_Click_Count = `Q63_Click Count`,
         
         F13_Page_Submit = `Q65_Page Submit`,
         F13_Click_Count = `Q65_Click Count`,
         FF13_Page_Submit = `Q68_Page Submit`,
         FF13_Click_Count = `Q68_Click Count`,
         
         F14_Page_Submit = `Q71_Page Submit`,
         F14_Click_Count = `Q71_Click Count`,
         FF14_Page_Submit = `Q73_Page Submit`,
         FF14_Click_Count = `Q73_Click Count`,
         
         F15_Page_Submit = `Q78_Page Submit`,
         F15_Click_Count = `Q78_Click Count`,
         FF15_Page_Submit = `Q76_Page Submit`,
         FF15_Click_Count = `Q76_Click Count`,
         
         F16_Page_Submit = `Q80_Page Submit`,
         F16_Click_Count = `Q80_Click Count`,
         FF16_Page_Submit = `Q83_Page Submit`,
         FF16_Click_Count = `Q83_Click Count`,
         
         F17R = Q86,
         F17F = Q87,
         
         F17_Page_Submit = `Q85_Page Submit`,
         F17_Click_Count = `Q85_Click Count`,
         FF17_Page_Submit = `Q88_Page Submit`,
         FF17_Click_Count = `Q88_Click Count`)


# Variablen in R/F codieren & Summenscore bei k Prim
# F1 (2 ist korrekt) dichotomisieren
Daten4$F1 <- ifelse(Daten4$F1 == 2, 1, 0)

# F2 (2 ist korrekt) dichotomisieren
Daten4$F2 <- ifelse(Daten4$F2 == 2, 1, 0)

# F3 (3 ist korrekt) dichotomisieren
Daten4$F3 <- ifelse(Daten4$F3 == 3, 1, 0)

# F4 (1 ist korrekt) dichotomisieren
Daten4$F4 <- ifelse(Daten4$F4 == 1, 1, 0)

# Keine F5

# F6 (2 ist korrekt) dichotomisieren
Daten4$F6 <- ifelse(Daten4$F6 == 2, 1, 0)

# F7 (3 ist korrekt) dichotomisieren
Daten4$F7 <- ifelse(Daten4$F7 == 3, 1, 0)

# F8 (4 ist korrekt) dichotomisieren
Daten4$F8 <- ifelse(Daten4$F8 == 4, 1, 0)

# F9 (2 ist korrekt) dichotomisieren
Daten4$F9 <- ifelse(Daten4$F9 == 2, 1, 0)

# F10 (2 ist korrekt) dichotomisieren
Daten4$F10 <- ifelse(Daten4$F10 == 1, 1, 0)

# F11 k Prim Score
Daten4$F11_1 <- ifelse(Daten4$F11_1 == 2, 0.25, 0)
Daten4$F11_2 <- ifelse(Daten4$F11_2 == 1, 0.25, 0)
Daten4$F11_3 <- ifelse(Daten4$F11_3 == 1, 0.25, 0)
Daten4$F11_4 <- ifelse(Daten4$F11_4 == 1, 0.25, 0)
# F11 Summenscore
Daten4$F11 <- rowSums(select(Daten4, F11_1, F11_2, F11_3, F11_4))
Daten4 <- relocate(Daten4, F11, .after = F11_4)

# F12 (1 ist korrekt) dichotomisieren
Daten4$F12 <- ifelse(Daten4$F12 == 1, 1, 0)

# F13 (2 ist korrekt) dichotomisieren
Daten4$F13 <- ifelse(Daten4$F13 == 2, 1, 0)

# F14 k Prim Score
Daten4$F14_1 <- ifelse(Daten4$F14_1 == 1, 0.25, 0)
Daten4$F14_2 <- ifelse(Daten4$F14_2 == 1, 0.25, 0)
Daten4$F14_3 <- ifelse(Daten4$F14_3 == 1, 0.25, 0)
Daten4$F14_4 <- ifelse(Daten4$F14_4 == 1, 0.25, 0)
# F14 Summenscore
Daten4$F14 <- rowSums(select(Daten4, F14_1, F14_2, F14_3, F14_4))
Daten4 <- relocate(Daten4, F14, .after = F14_4)

# F15 (2 ist korrekt) dichotomisieren
Daten4$F15 <- ifelse(Daten4$F15 == 2, 1, 0)

# F16 k Prim Score
Daten4$F16_1 <- ifelse(Daten4$F16_1 == 2, (1/3), 0)
Daten4$F16_2 <- ifelse(Daten4$F16_2 == 1, (1/3), 0)
Daten4$F16_3 <- ifelse(Daten4$F16_3 == 2, (1/3), 0)
# F14 Summenscore
Daten4$F16 <- rowSums(select(Daten4, F16_1, F16_2, F16_3))
Daten4 <- relocate(Daten4, F16, .after = F16_3)

# F17 (1 ist korrekt) dichotomisieren
Daten4$F17 <- ifelse(Daten4$F17 == 1, 1, 0)


# Daten 5 - CFA2 MTMM ----------------------------------------------------------
Daten5 <- Daten5 %>%
  rename(F1_Page_Submit = `Q9_Page Submit`,
         F1_Click_Count = `Q9_Click Count`,
         FF1_Page_Submit = `Q12_Page Submit`,
         FF1_Click_Count = `Q12_Click Count`,
         
         F2_Page_Submit = `Q14_Page Submit`,
         F2_Click_Count = `Q14_Click Count`,
         FF2_Page_Submit = `Q17_Page Submit`,
         FF2_Click_Count = `Q17_Click Count`,
         
         F3_Page_Submit = `Q19_Page Submit`,
         F3_Click_Count = `Q19_Click Count`,
         FF3_Page_Submit = `Q22_Page Submit`,
         FF3_Click_Count = `Q22_Click Count`,
         
         F4_Page_Submit = `Q24_Page Submit`,
         F4_Click_Count = `Q24_Click Count`,
         FF4_Page_Submit = `Q27_Page Submit`,
         FF4_Click_Count = `Q27_Click Count`,
         
         F5_Page_Submit = `Q29_Page Submit`,
         F5_Click_Count = `Q29_Click Count`,
         FF5_Page_Submit = `Q32_Page Submit`,
         FF5_Click_Count = `Q32_Click Count`,
         
         F6_Page_Submit = `Q34_Page Submit`,
         F6_Click_Count = `Q34_Click Count`,
         FF6_Page_Submit = `Q37_Page Submit`,
         FF6_Click_Count = `Q37_Click Count`,
         
         F7_Page_Submit = `Q39_Page Submit`,
         F7_Click_Count = `Q39_Click Count`,
         FF7_Page_Submit = `Q42_Page Submit`,
         FF7_Click_Count = `Q42_Click Count`,
         
         # F8 kein Page Submit bzw. Click Count
         
         F9_Page_Submit = `Q47_Page Submit`,
         F9_Click_Count = `Q47_Click Count`,
         FF9_Page_Submit = `Q50_Page Submit`,
         FF9_Click_Count = `Q50_Click Count`,
         
         F10_Page_Submit = `Q52_Page Submit`,
         F10_Click_Count = `Q52_Click Count`,
         FF10_Page_Submit = `Q55_Page Submit`,
         FF10_Click_Count = `Q55_Click Count`,
         
         F11_Page_Submit = `Q57_Page Submit`,
         F11_Click_Count = `Q57_Click Count`,
         FF11_Page_Submit = `Q60_Page Submit`,
         FF11_Click_Count = `Q60_Click Count`,
         
         F12_Page_Submit = `Q62_Page Submit`,
         F12_Click_Count = `Q62_Click Count`,
         FF12_Page_Submit = `Q65_Page Submit`,
         FF12_Click_Count = `Q65_Click Count`,
         
         F13_Page_Submit = `Q67_Page Submit`,
         F13_Click_Count = `Q67_Click Count`,
         FF13_Page_Submit = `Q70_Page Submit`,
         FF13_Click_Count = `Q70_Click Count`,
         
         F14_Page_Submit = `Q72_Page Submit`,
         F14_Click_Count = `Q72_Click Count`,
         FF14_Page_Submit = `Q75_Page Submit`,
         FF14_Click_Count = `Q75_Click Count`,
         
         F15_Page_Submit = `Q77_Page Submit`,
         F15_Click_Count = `Q77_Click Count`,
         FF15_Page_Submit = `Q80_Page Submit`,
         FF15_Click_Count = `Q80_Click Count`)


# Variablen in R/F codieren & Summenscore bei k Prim
# F1 k Prim Score
Daten5$F1_1 <- ifelse(Daten5$F1_1 == 1, 0.25, 0)
Daten5$F1_2 <- ifelse(Daten5$F1_2 == 2, 0.25, 0)
Daten5$F1_3 <- ifelse(Daten5$F1_3 == 2, 0.25, 0)
Daten5$F1_4 <- ifelse(Daten5$F1_4 == 2, 0.25, 0)
# F1 Summenscore
Daten5$F1 <- rowSums(select(Daten5, F1_1, F1_2, F1_3, F1_4))
Daten5 <- relocate(Daten5, F1, .after = F1_4)

# F2 k Prim Score
Daten5$F2_1 <- ifelse(Daten5$F2_1 == 1, 0.25, 0)
Daten5$F2_2 <- ifelse(Daten5$F2_2 == 2, 0.25, 0)
Daten5$F2_3 <- ifelse(Daten5$F2_3 == 1, 0.25, 0)
Daten5$F2_4 <- ifelse(Daten5$F2_4 == 2, 0.25, 0)
# F2 Summenscore
Daten5$F2 <- rowSums(select(Daten5, F2_1, F2_2, F2_3, F2_4))
Daten5 <- relocate(Daten5, F2, .after = F2_4)

# F3 (2 ist korrekt) dichotomisieren
Daten5$F3 <- ifelse(Daten5$F3 == 2, 1, 0)

# F4 k Prim Score
Daten5$F4_1 <- ifelse(Daten5$F4_1 == 1, 0.25, 0)
Daten5$F4_2 <- ifelse(Daten5$F4_2 == 1, 0.25, 0)
Daten5$F4_3 <- ifelse(Daten5$F4_3 == 1, 0.25, 0)
Daten5$F4_4 <- ifelse(Daten5$F4_4 == 2, 0.25, 0)
# F4 Summenscore
Daten5$F4 <- rowSums(select(Daten5, F4_1, F4_2, F4_3, F4_4))
Daten5 <- relocate(Daten5, F4, .after = F4_4)

# F5 k Prim Score
Daten5$F5_1 <- ifelse(Daten5$F5_1 == 1, 0.25, 0)
Daten5$F5_2 <- ifelse(Daten5$F5_2 == 2, 0.25, 0)
Daten5$F5_3 <- ifelse(Daten5$F5_3 == 2, 0.25, 0)
Daten5$F5_4 <- ifelse(Daten5$F5_4 == 1, 0.25, 0)
# F5 Summenscore
Daten5$F5 <- rowSums(select(Daten5, F5_1, F5_2, F5_3, F5_4))
Daten5 <- relocate(Daten5, F5, .after = F5_4)

# F6 k Prim Score
Daten5$F6_1 <- ifelse(Daten5$F6_1 == 2, 0.25, 0)
Daten5$F6_2 <- ifelse(Daten5$F6_2 == 1, 0.25, 0)
Daten5$F6_3 <- ifelse(Daten5$F6_3 == 1, 0.25, 0)
Daten5$F6_4 <- ifelse(Daten5$F6_4 == 2, 0.25, 0)
# F6 Summenscore
Daten5$F6 <- rowSums(select(Daten5, F6_1, F6_2, F6_3, F6_4))
Daten5 <- relocate(Daten5, F6, .after = F6_4)

# F7 k Prim Score
Daten5$F7_1 <- ifelse(Daten5$F7_1 == 2, 0.25, 0)
Daten5$F7_2 <- ifelse(Daten5$F7_2 == 2, 0.25, 0)
Daten5$F7_3 <- ifelse(Daten5$F7_3 == 2, 0.25, 0)
Daten5$F7_4 <- ifelse(Daten5$F7_4 == 1, 0.25, 0)
# F7 Summenscore
Daten5$F7 <- rowSums(select(Daten5, F7_1, F7_2, F7_3, F7_4))
Daten5 <- relocate(Daten5, F7, .after = F7_4)

# F8 (1 ist korrekt) dichotomisieren
Daten5$F8 <- ifelse(Daten5$F8 == 1, 1, 0)

# F9 k Prim Score
Daten5$F9_1 <- ifelse(Daten5$F9_1 == 2, 0.25, 0)
Daten5$F9_2 <- ifelse(Daten5$F9_2 == 1, 0.25, 0)
Daten5$F9_3 <- ifelse(Daten5$F9_3 == 2, 0.25, 0)
Daten5$F9_4 <- ifelse(Daten5$F9_4 == 1, 0.25, 0)
# F9 Summenscore
Daten5$F9 <- rowSums(select(Daten5, F9_1, F9_2, F9_3, F9_4))
Daten5 <- relocate(Daten5, F9, .after = F9_4)

# F10 k Prim Score
Daten5$F10_1 <- ifelse(Daten5$F10_1 == 1, 0.25, 0)
Daten5$F10_2 <- ifelse(Daten5$F10_2 == 2, 0.25, 0)
Daten5$F10_3 <- ifelse(Daten5$F10_3 == 2, 0.25, 0)
Daten5$F10_4 <- ifelse(Daten5$F10_4 == 2, 0.25, 0)
# F10 Summenscore
Daten5$F10 <- rowSums(select(Daten5, F10_1, F10_2, F10_3, F10_4))
Daten5 <- relocate(Daten5, F10, .after = F10_4)

# F11 (1 ist korrekt) dichotomisieren
Daten5$F11 <- ifelse(Daten5$F11 == 1, 1, 0)

# F12 k Prim Score
Daten5$F12_1 <- ifelse(Daten5$F12_1 == 5, 0.25, 0)
Daten5$F12_2 <- ifelse(Daten5$F12_2 == 5, 0.25, 0)
Daten5$F12_3 <- ifelse(Daten5$F12_3 == 5, 0.25, 0)
Daten5$F12_4 <- ifelse(Daten5$F12_4 == 4, 0.25, 0)
# F12 Summenscore
Daten5$F12 <- rowSums(select(Daten5, F12_1, F12_2, F12_3, F12_4))
Daten5 <- relocate(Daten5, F12, .after = F12_4)

# F13 (1 ist korrekt) dichotomisieren
Daten5$F13 <- ifelse(Daten5$F13 == 1, 1, 0)

# F14 k Prim Score
Daten5$F14_1 <- ifelse(Daten5$F14_1 == 1, 0.25, 0)
Daten5$F14_2 <- ifelse(Daten5$F14_2 == 1, 0.25, 0)
Daten5$F14_3 <- ifelse(Daten5$F14_3 == 2, 0.25, 0)
Daten5$F14_4 <- ifelse(Daten5$F14_4 == 2, 0.25, 0)
# F14 Summenscore
Daten5$F14 <- rowSums(select(Daten5, F14_1, F14_2, F14_3, F14_4))
Daten5 <- relocate(Daten5, F14, .after = F14_4)

# F15 k Prim Score
Daten5$F15_1 <- ifelse(Daten5$F15_1 == 1, 0.25, 0)
Daten5$F15_2 <- ifelse(Daten5$F15_2 == 1, 0.25, 0)
Daten5$F15_3 <- ifelse(Daten5$F15_3 == 2, 0.25, 0)
Daten5$F15_4 <- ifelse(Daten5$F15_4 == 2, 0.25, 0)
# F15 Summenscore
Daten5$F15 <- rowSums(select(Daten5, F15_1, F15_2, F15_3, F15_4))
Daten5 <- relocate(Daten5, F15, .after = F15_4)


# Daten 6 - Kriteriumsorientiertes Testen --------------------------------------
Daten6 <- Daten6 %>%
  rename(F1_Page_Submit = `Q9_Page Submit`,
         F1_Click_Count = `Q9_Click Count`,
         FF1_Page_Submit = `Q12_Page Submit`,
         FF1_Click_Count = `Q12_Click Count`,
         
         F2_Page_Submit = `Q14_Page Submit`,
         F2_Click_Count = `Q14_Click Count`,
         FF2_Page_Submit = `Q17_Page Submit`,
         FF2_Click_Count = `Q17_Click Count`,
         
         F3_Page_Submit = `Q19_Page Submit`,
         F3_Click_Count = `Q19_Click Count`,
         FF3_Page_Submit = `Q22_Page Submit`,
         FF3_Click_Count = `Q22_Click Count`,
         
         F4_Page_Submit = `Q24_Page Submit`,
         F4_Click_Count = `Q24_Click Count`,
         FF4_Page_Submit = `Q27_Page Submit`,
         FF4_Click_Count = `Q27_Click Count`,
         
         F5_Page_Submit = `Q29_Page Submit`,
         F5_Click_Count = `Q29_Click Count`,
         FF5_Page_Submit = `Q32_Page Submit`,
         FF5_Click_Count = `Q32_Click Count`,
         
         F6_Page_Submit = `Q34_Page Submit`,
         F6_Click_Count = `Q34_Click Count`,
         FF6_Page_Submit = `Q37_Page Submit`,
         FF6_Click_Count = `Q37_Click Count`,
         
         F7_Page_Submit = `Q39_Page Submit`,
         F7_Click_Count = `Q39_Click Count`,
         FF7_Page_Submit = `Q42_Page Submit`,
         FF7_Click_Count = `Q42_Click Count`,
         
         # F8 & FF8 kein Page Submit bzw. Click Count
         
         F9_Page_Submit = `Q47_Page Submit`,
         F9_Click_Count = `Q47_Click Count`,
         FF9_Page_Submit = `Q50_Page Submit`,
         FF9_Click_Count = `Q50_Click Count`,
         
         F10_Page_Submit = `Q52_Page Submit`,
         F10_Click_Count = `Q52_Click Count`,
         FF10_Page_Submit = `Q55_Page Submit`,
         FF10_Click_Count = `Q55_Click Count`,
         
         F11_Page_Submit = `Q57_Page Submit`,
         F11_Click_Count = `Q57_Click Count`
         #FF11 kein Page Submit bzw. Click Count
  )


# Variablen in R/F codieren & Summenscore bei k Prim
# F1 k Prim Score
Daten6$F1_1 <- ifelse(Daten6$F1_1 == 1, 0.25, 0)
Daten6$F1_2 <- ifelse(Daten6$F1_2 == 2, 0.25, 0)
Daten6$F1_3 <- ifelse(Daten6$F1_3 == 1, 0.25, 0)
Daten6$F1_4 <- ifelse(Daten6$F1_4 == 2, 0.25, 0)
# F1 Summenscore
Daten6$F1 <- rowSums(select(Daten6, F1_1, F1_2, F1_3, F1_4))
Daten6 <- relocate(Daten6, F1, .after = F1_4)

# F2 (4 ist korrekt) dichotomisieren
Daten6$F2 <- ifelse(Daten6$F2 == 4, 1, 0)

# F3 (4 ist korrekt) dichotomisieren
Daten6$F3 <- ifelse(Daten6$F3 == 4, 1, 0)

# F4 k Prim Score
Daten6$F4_1 <- ifelse(Daten6$F4_1 == 2, 0.25, 0)
Daten6$F4_2 <- ifelse(Daten6$F4_2 == 1, 0.25, 0)
Daten6$F4_3 <- ifelse(Daten6$F4_3 == 2, 0.25, 0)
Daten6$F4_4 <- ifelse(Daten6$F4_4 == 2, 0.25, 0)
# F4 Summenscore
Daten6$F4 <- rowSums(select(Daten6, F4_1, F4_2, F4_3, F4_4))
Daten6 <- relocate(Daten6, F4, .after = F4_4)

# F5 (2 ist korrekt) dichotomisieren
Daten6$F5 <- ifelse(Daten6$F5 == 2, 1, 0)

# F6 k Prim Score
Daten6$F6_1 <- ifelse(Daten6$F6_1 == 1, 0.25, 0)
Daten6$F6_2 <- ifelse(Daten6$F6_2 == 2, 0.25, 0)
Daten6$F6_3 <- ifelse(Daten6$F6_3 == 2, 0.25, 0)
Daten6$F6_4 <- ifelse(Daten6$F6_4 == 1, 0.25, 0)
# F6 Summenscore
Daten6$F6 <- rowSums(select(Daten6, F6_1, F6_2, F6_3, F6_4))
Daten6 <- relocate(Daten6, F6, .after = F6_4)

# F7 k Prim Score
Daten6$F7_1 <- ifelse(Daten6$F7_1 == 1, 0.25, 0)
Daten6$F7_2 <- ifelse(Daten6$F7_2 == 2, 0.25, 0)
Daten6$F7_3 <- ifelse(Daten6$F7_3 == 2, 0.25, 0)
Daten6$F7_4 <- ifelse(Daten6$F7_4 == 2, 0.25, 0)
# F7 Summenscore
Daten6$F7 <- rowSums(select(Daten6, F7_1, F7_2, F7_3, F7_4))
Daten6 <- relocate(Daten6, F7, .after = F7_4)

# F8 k Prim Score
Daten6$F8_1 <- ifelse(Daten6$F8_1 == 2, 0.25, 0)
Daten6$F8_2 <- ifelse(Daten6$F8_2 == 1, 0.25, 0)
Daten6$F8_3 <- ifelse(Daten6$F8_3 == 1, 0.25, 0)
Daten6$F8_4 <- ifelse(Daten6$F8_4 == 2, 0.25, 0)
# F8 Summenscore
Daten6$F8 <- rowSums(select(Daten6, F8_1, F8_2, F8_3, F8_4))
Daten6 <- relocate(Daten6, F8, .after = F8_4)

# F9 k Prim Score
Daten6$F9_1 <- ifelse(Daten6$F9_1 == 2, 0.25, 0)
Daten6$F9_2 <- ifelse(Daten6$F9_2 == 2, 0.25, 0)
Daten6$F9_3 <- ifelse(Daten6$F9_3 == 2, 0.25, 0)
Daten6$F9_4 <- ifelse(Daten6$F9_4 == 2, 0.25, 0)
# F9 Summenscore
Daten6$F9 <- rowSums(select(Daten6, F9_1, F9_2, F9_3, F9_4))
Daten6 <- relocate(Daten6, F9, .after = F9_4)

# F10 (1 ist korrekt) dichotomisieren
Daten6$F10 <- ifelse(Daten6$F10 == 1, 1, 0)

# F11 k Prim Score
Daten6$F11_1 <- ifelse(Daten6$F11_1 == 4, 0.25, 0)
Daten6$F11_2 <- ifelse(Daten6$F11_2 == 4, 0.25, 0)
Daten6$F11_3 <- ifelse(Daten6$F11_3 == 4, 0.25, 0)
Daten6$F11_4 <- ifelse(Daten6$F11_4 == 5, 0.25, 0)
# F11 Summenscore
Daten6$F11 <- rowSums(select(Daten6, F11_1, F11_2, F11_3, F11_4))
Daten6 <- relocate(Daten6, F11, .after = F11_4)


# Daten 7 - Probepruefung ------------------------------------------------------
# Datensatz 7 (Probepruefung) Fragen umbenennen
Daten7 <- Daten7 %>%
  rename(F1 = RF01,
         F2 = RF02,
         F3 = RF03,
         F4 = RF04,
         F5 = RF05,
         F6 = RF06,
         F7 = RF07,
         F8 = RF08,
         F9 = RF10, # hier absichtlich vertauscht -> so in Qualtrics
         F10 = RF09, # hier absichtlich vertauscht -> so in Qualtrics
         F11 = RF11,
         F12 = RF12,
         
         F13 = KF38B,
         F14 = KF38A,
         F15 = Q67,
         F16 = Q69,
         F17 = Q70,
         F18 = Q71,
         F19 = Q94,
         F20 = Q75,
         F21 = Q80,
         F22 = Q78,
         F23 = Q82,
         F24 = Q83,
         F25 = Q84)


# Datensatz 7 (Probeprüfung) Kodierung der Antworten
# F1 (2 ist korrekt) dichotomisieren
Daten7$F1 <- ifelse(Daten7$F1 == 2, 1, 0)

# F2 (1 ist korrekt) dichotomisieren
Daten7$F2 <- ifelse(Daten7$F2 == 1, 1, 0)

# F3 (1 ist korrekt) dichotomisieren
Daten7$F3 <- ifelse(Daten7$F3 == 1, 1, 0)

# F4 (2 ist korrekt) dichotomisieren
Daten7$F4 <- ifelse(Daten7$F4 == 2, 1, 0)

# F5 (2 ist korrekt) dichotomisieren
Daten7$F5 <- ifelse(Daten7$F5 == 2, 1, 0)

# F6 (1 ist korrekt) dichotomisieren
Daten7$F6 <- ifelse(Daten7$F6 == 1, 1, 0)

# F7 (2 ist korrekt) dichotomisieren
Daten7$F7 <- ifelse(Daten7$F7 == 2, 1, 0)

# F8 (1 ist korrekt) dichotomisieren
Daten7$F8 <- ifelse(Daten7$F8 == 1, 1, 0)

# F9 (2 ist korrekt) dichotomisieren
Daten7$F9 <- ifelse(Daten7$F9 == 2, 1, 0)

# F10 (1 ist korrekt) dichotomisieren
Daten7$F10 <- ifelse(Daten7$F10 == 1, 1, 0)

# F11 (1 ist korrekt) dichotomisieren
Daten7$F11 <- ifelse(Daten7$F11 == 1, 1, 0)

# F12 (2 ist korrekt) dichotomisieren
Daten7$F12 <- ifelse(Daten7$F12 == 2, 1, 0)

# F13
Daten7$F13 <- ifelse(Daten7$F13 == 1, 1, 0)

# F14
Daten7$F14 <- ifelse(Daten7$F14 %in% c(0.20, 0.2, .2, .20), 1, 0)

# F15
Daten7$F15 <- ifelse(Daten7$F15 %in% c(0.50, 0.5, .5, .50), 1, 0)

# F16
Daten7$F16 <- ifelse(Daten7$F16 == 1.811, 1, 0)

# F17
Daten7$F17 <- ifelse(Daten7$F17 == 1.5, 1, 0)

# F18
Daten7$F18 <- ifelse(Daten7$F18 == 14, 1, 0)

# F19
Daten7$F19 <- ifelse(Daten7$F19 == 109.006, 1, 0)

# F20
Daten7$F20 <- ifelse(Daten7$F20 == 0.671, 1, 0)

# F21
Daten7$F21 <- ifelse(Daten7$F21 == 91.014, 1, 0)

# F22
Daten7$F22 <- ifelse(Daten7$F22 %in% c(0.43, .43), 1, 0)

# F23
Daten7$F23 <- ifelse(Daten7$F23 == 0.006, 1, 0)

# F24
Daten7$F24 <- ifelse(Daten7$F24 %in% c(0.70, 0.7, .7, .70), 1, 0)

# F25
Daten7$F25 <- ifelse(Daten7$F25 %in% c(-0.2, -0.3, -0.4, -0.5, -0.6,
                                       -.2, -.3, -.4, -.5, -.6), 1, 0)



# Summenscore und Prop von Krit
Daten7 <- Daten7 %>% 
  mutate(Probetest_sum = rowSums(select(., F1, F2, F3, F4, F5, F6, F7, F8,
                                        F9, F10, F11, F12, F14, F15, F16, F17, F18,
                                        F19, F20, F21, F22, F24, F25), na.rm = TRUE))#F23 muss raus, war nicht dieselbe Variable, F13 raus, unterRatewahr



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Datensatzt A mit allen nötigen Var für Itemanalyse ---------------------------
DatenA<-select(Daten1, EndDate1, Pseudo, VW11, VW12, VW13, oft1, Ziel, Relevanz1 = Q183_1,
               Relevanz2 = Q183_2, Relevanz3 = Q183_3, Muhe, Duration1, Prüfung,
               
               F1_Page_Submit, F1_Click_Count, F1, FF1_Page_Submit, FF1_Click_Count,
               F2_Page_Submit, F2_Click_Count, F2, FF2_Page_Submit, FF2_Click_Count,
               F3_Page_Submit, F3_Click_Count, F3, FF3_Page_Submit, FF3_Click_Count,
               F3i_Page_Submit, F3i_Click_Count, F3i, FF3i_Page_Submit, FF3i_Click_Count,
               F3ii_Page_Submit, F3ii_Click_Count, F3ii, # kein Page Submit und Click Count nach Frage
               F4_Page_Submit, F4_Click_Count, F4, FF4_Page_Submit, FF4_Click_Count,
               F5_Page_Submit, F5_Click_Count, F5, FF5_Page_Submit, FF5_Click_Count,
               F6_Page_Submit, F6_Click_Count, F6, # kein Page Submit und Click Count nach Frage
               F7_Page_Submit, F7_Click_Count, F7, # kein Page Submit und Click Count nach Frage, sehr viele NA's in den anderen!
               F8_Page_Submit, F8_Click_Count, F8, FF8_Page_Submit, FF8_Click_Count,
               F9_Page_Submit, F9_Click_Count, F9, FF9_Page_Submit, FF9_Click_Count,
               F10_Page_Submit, F10_Click_Count, F10, FF10_Page_Submit, FF10_Click_Count,
               F11_Page_Submit, F11_Click_Count, F11, FF11_Page_Submit, FF11_Click_Count,
               F12_Page_Submit, F12_Click_Count, F12, FF12_Page_Submit, FF12_Click_Count,
               F13_Page_Submit, F13_Click_Count, F13, # kein Page Submit und Click Count nach Frage
               F14_Page_Submit, F14_Click_Count, F14, FF14_Page_Submit, FF14_Click_Count,
               F16_Page_Submit, F16_Click_Count, F16, FF16_Page_Submit, FF16_Click_Count,
               F17_Page_Submit, F17_Click_Count, F17, FF17_Page_Submit, FF17_Click_Count,
               F17_1_Page_Submit, F17_1_Click_Count, F17_1, FF17_1_Page_Submit, FF17_1_Click_Count,
               F18_Page_Submit, F18_Click_Count, F18, FF18_Page_Submit, FF18_Click_Count,
               F19_Page_Submit, F19_Click_Count, F19, FF19_Page_Submit, FF19_Click_Count,
               F20_Page_Submit, F20_Click_Count, F20, FF20_Page_Submit, FF20_Click_Count,
               F21_Page_Submit, F21_Click_Count, F21, FF21_Page_Submit, FF21_Click_Count,
               F23_Page_Submit, F23_Click_Count, F23, FF23_Page_Submit, FF23_Click_Count,
               F24_Page_Submit, F24_Click_Count, F24, FF24_Page_Submit, FF24_Click_Count,
               F25_Page_Submit, F25_Click_Count, F25, FF25_Page_Submit, FF25_Click_Count,
               F26_Page_Submit, F26_Click_Count, F26, FF26_Page_Submit, FF26_Click_Count,
               F27_Page_Submit, F27_Click_Count, F27, FF27_Page_Submit, FF27_Click_Count)

DatenA<-rename(DatenA, Muhe1 = Muhe)




DatenA$VW11<-rowSums(select(DatenA, F2, F3, F3i, F4, F5, F8,F9, F11, F14, F18, F19, F21)) # was ist mit F3ii? In Qualtrics ist F25 auch in Theorie, diese gehört aber ziemlich sicher in die Individualdiagnostik
DatenA$VW12<-rowSums(select(DatenA, F1, F6, F7, F10, F12, F13,F16, F17, F17_1, F20))
DatenA$VW13<-rowSums(select(DatenA, F23, F24, F25, F26, F27))
  

# Alle Pseudonyme zu lowercase
DatenA$Pseudo<-tolower(DatenA$Pseudo)


# Jonas, DatenA ergänzen mit allen Varialben, dann Natalie schicken
# Wie oft erscheint dasselbe Pseudonym?
DatenA <- DatenA %>%
  group_by(Pseudo) %>% # nach Pseudonym gruppieren
  mutate(num_A = n()) %>%  # Anzahl Fälle in jeder Gruppe in "num" speichern
  ungroup() # gruppierung wieder auflösen


# Ab alle Fragen-Variablen ein "_A" anfügen, damit im Gesamtdatensatz Fragenvariablen unterscheidbar bleiben
fragen_vars <- names(select(DatenA, F1_Page_Submit:FF27_Click_Count))
names(DatenA) <- ifelse(names(DatenA) %in% fragen_vars, paste0(names(DatenA), "_A"), names(DatenA))

# Wie viele Fragen hat eine Person im Fragebogen gestellt?
FrageA <- Daten1 %>% # matches("A[0-9]") wählt alle Kommentarspalten aus
  mutate(across(matches("A[0-9]"), ~ if_else(nchar(.) > 2, ., NULL))) %>% # Nur Kommentare die länger sind als 2 Zeichen behalten (zum Teil als Kommentar nur eine "1")
  mutate(across(matches("A[0-9]"), ~ as.numeric(!is.na(.)))) %>% # alle NA's zu 0 und Kommentare zu 1
  mutate(FrageA = rowSums(select(., matches("A[0-9]")))) %>%  # Variable FrageA zeigt wie oft eine Person einen Kommentar gemacht hat
  select(FrageA)
DatenA <- cbind(DatenA, FrageA)




# Datensatzt B mit allen nötigen Var für Itemanalyse ---------------------------
DatenB <- select(Daten2, EndDate2, Pseudo ,SC21, SC22, oft2, lange2, vl2, pflichtlit2,
                 Muhe2, Duration2, verstanden1_1, verstanden1_2, verstanden1_3, verstanden1_4,
                 verstanden1_5,verstanden1_6,
                 
                 F1_Page_Submit, F1_Click_Count, F1, FF1_Page_Submit, FF1_Click_Count,
                 F2_Page_Submit, F2_Click_Count, F2, FF2_Page_Submit, FF2_Click_Count,
                 F3, FF3_Page_Submit, FF3_Click_Count, # F3 kein F3_Page_Submit und F3_Click_Count
                 F4_Page_Submit, F4_Click_Count, F4, FF4_Page_Submit, FF4_Click_Count,
                 F5_Page_Submit, F5_Click_Count, F5, FF5_Page_Submit, FF5_Click_Count,
                 F6_Page_Submit, F6_Click_Count, F6, FF6_Page_Submit, FF6_Click_Count,
                 F7_Page_Submit, F7_Click_Count, F7, FF7_Page_Submit, FF7_Click_Count,
                 F8_Page_Submit, F8_Click_Count, #F8, # F8 und F9 sind eine Frage, Antwort erst nach F9 deshalb kein FF8_Page_Submit & FF8_Click_Count
                 F9_Page_Submit, F9_Click_Count, F9, FF9_Page_Submit, FF9_Click_Count,
                 F10_Page_Submit, F10_Click_Count, F10, FF10_Page_Submit, FF10_Click_Count,
                 F11_Page_Submit, F11_Click_Count, F11, FF11_Page_Submit, FF11_Click_Count,
                 F12_Page_Submit, F12_Click_Count, F12, FF12_Page_Submit, FF12_Click_Count,
                 F13_Page_Submit, F13_Click_Count, F13, FF13_Page_Submit, FF13_Click_Count,
                 F14_Page_Submit, F14_Click_Count, F14, FF14_Page_Submit, FF14_Click_Count
                 )


DatenB$Pseudo<-tolower(DatenB$Pseudo)


# Wie oft erscheint das selbe Pseudonym?
DatenB <- DatenB %>%
  group_by(Pseudo) %>% # nach Pseudonym gruppieren
  mutate(num_B = n()) %>%  # Anzahl Fälle in jeder Gruppe in "num" speichern
  ungroup() # gruppierung wieder auflösen

# Ab alle Fragen-Variablen ein "_B" anfügen, damit im Gesamtdatensatz Fragenvariablen unterscheidbar bleiben
fragen_vars <- names(select(DatenB, F1_Page_Submit:FF14_Click_Count))
names(DatenB) <- ifelse(names(DatenB) %in% fragen_vars, paste0(names(DatenB), "_B"), names(DatenB))

# Wie viele Fragen hat eine Person im Fragebogen gestellt?
FrageB <- Daten2 %>% # matches(""F[0-9]+R") wählt alle Kommentarspalten aus
  mutate(across(matches("F[0-9]+R"), ~ if_else(nchar(.) > 2, ., NULL))) %>% # Nur Kommentare die länger sind als 2 Zeichen behalten (zum Teil als Kommentar nur eine "1")
  mutate(across(matches("F[0-9]+R"), ~ as.numeric(!is.na(.)))) %>% # alle NA's zu 0 und Kommentare zu 1
  mutate(FrageB = rowSums(select(., matches("F[0-9]+R")))) %>%  # Variable FrageA zeigt wie oft eine Person einen Kommentar gemacht hat
  select(FrageB)
DatenB <- cbind(DatenB, FrageB)


# Datensatzt C mit allen nötigen Var für Itemanalyse ---------------------------
DatenC <- select(Daten3, EndDate3, Pseudo, SC31, SC32, SC33, oft3, lange3, vl3,
                 pflichtlit3, Muhe3, Duration3, verstanden2_1, verstanden2_2, verstanden2_3,
                 verstanden2_4, verstanden2_5, verstanden2_6,
                 
                 F1_Page_Submit, F1_Click_Count, F1, FF1_Page_Submit, FF1_Click_Count,
                 F2_Page_Submit, F2_Click_Count, F2, FF2_Page_Submit, FF2_Click_Count,
                 F3_Page_Submit, F3_Click_Count, F3, FF3_Page_Submit, FF3_Click_Count,
                 F4_Page_Submit, F4_Click_Count, F4, FF4_Page_Submit, FF4_Click_Count,
                 F5_Page_Submit, F5_Click_Count, F5, FF5_Page_Submit, FF5_Click_Count,
                 F6_Page_Submit, F6_Click_Count, F6, FF6_Page_Submit, FF6_Click_Count,
                 F7_Page_Submit, F7_Click_Count, F7, FF7_Page_Submit, FF7_Click_Count,
                 F8_Page_Submit, F8_Click_Count, F8, FF8_Page_Submit, FF8_Click_Count,
                 F9_Page_Submit, F9_Click_Count, F9, FF9_Page_Submit, FF9_Click_Count,
                 F10_Page_Submit, F10_Click_Count, F10, FF10_Page_Submit, FF10_Click_Count,
                 F11_Page_Submit, F11_Click_Count, F11, FF11_Page_Submit, FF11_Click_Count,
                 F12_Page_Submit, F12_Click_Count, F12, FF12_Page_Submit, FF12_Click_Count
                 )


DatenC$Pseudo<-tolower(DatenC$Pseudo)

# Wie oft erscheint das selbe Pseudonym?
DatenC <- DatenC %>%
  group_by(Pseudo) %>% # nach Pseudonym gruppieren
  mutate(num_C = n()) %>%  # Anzahl Fälle in jeder Gruppe in "num" speichern
  ungroup() # gruppierung wieder auflösen

# Ab alle Fragen-Variablen ein "_C" anfügen, damit im Gesamtdatensatz Fragenvariablen unterscheidbar bleiben
fragen_vars <- names(select(DatenC, F1_Page_Submit:FF12_Click_Count))
names(DatenC) <- ifelse(names(DatenC) %in% fragen_vars, paste0(names(DatenC), "_C"), names(DatenC))

# Wie viele Fragen hat eine Person im Fragebogen gestellt?
FrageC <- Daten3 %>% # matches(""F[0-9]+R") wählt alle Kommentarspalten aus
  mutate(across(matches("F[0-9]+R"), ~ if_else(nchar(.) > 2, ., NULL))) %>% # Nur Kommentare die länger sind als 2 Zeichen behalten (zum Teil als Kommentar nur eine "1")
  mutate(across(matches("F[0-9]+R"), ~ as.numeric(!is.na(.)))) %>% # alle NA's zu 0 und Kommentare zu 1
  mutate(FrageC = rowSums(select(., matches("F[0-9]+R")))) %>%  # Variable FrageA zeigt wie oft eine Person einen Kommentar gemacht hat
  select(FrageC)
DatenC <- cbind(DatenC, FrageC)


# Datensatzt D mit allen nötigen Var für Itemanalyse ---------------------------
DatenD <- select(Daten4, EndDate4, Pseudo, SC41, SC42, SC43, SC44, oft4, lange4,
                 vl4, pflichtlit4, Muhe4, Duration4, verstanden3_1, verstanden3_2, verstanden3_3,
                 
                 F1_Page_Submit, F1_Click_Count, F1, FF1_Page_Submit, FF1_Click_Count,
                 F2_Page_Submit, F2_Click_Count, F2, FF2_Page_Submit, FF2_Click_Count,
                 F3_Page_Submit, F3_Click_Count, F3, FF3_Page_Submit, FF3_Click_Count,
                 F4_Page_Submit, F4_Click_Count, F4, FF4_Page_Submit, FF4_Click_Count,
                 F6_Page_Submit, F6_Click_Count, F6, FF6_Page_Submit, FF6_Click_Count,
                 F7_Page_Submit, F7_Click_Count, F7, FF7_Page_Submit, FF7_Click_Count,
                 F8_Page_Submit, F8_Click_Count, F8, FF8_Page_Submit, FF8_Click_Count,
                 F9_Page_Submit, F9_Click_Count, F9, FF9_Page_Submit, FF9_Click_Count,
                 F10_Page_Submit, F10_Click_Count, F10, FF10_Page_Submit, FF10_Click_Count,
                 F11_Page_Submit, F11_Click_Count, F11, FF11_Page_Submit, FF11_Click_Count,
                 F12_Page_Submit, F12_Click_Count, F12, FF12_Page_Submit, FF12_Click_Count,
                 F13_Page_Submit, F13_Click_Count, F13, FF13_Page_Submit, FF13_Click_Count,
                 F14_Page_Submit, F14_Click_Count, F14, FF14_Page_Submit, FF14_Click_Count,
                 F15_Page_Submit, F15_Click_Count, F15, FF15_Page_Submit, FF15_Click_Count,
                 F16_Page_Submit, F16_Click_Count, F16, FF16_Page_Submit, FF16_Click_Count,
                 F17_Page_Submit, F17_Click_Count, F17, FF17_Page_Submit, FF17_Click_Count,
                 )


DatenD$Pseudo<-tolower(DatenD$Pseudo)

# Wie oft erscheint das selbe Pseudonym?
DatenD <- DatenD %>%
  group_by(Pseudo) %>% # nach Pseudonym gruppieren
  mutate(num_D = n()) %>%  # Anzahl Fälle in jeder Gruppe in "num" speichern
  ungroup() # gruppierung wieder auflösen

# Ab alle Fragen-Variablen ein "_D" anfügen, damit im Gesamtdatensatz Fragenvariablen unterscheidbar bleiben
fragen_vars <- names(select(DatenD, F1_Page_Submit:FF17_Click_Count))
names(DatenD) <- ifelse(names(DatenD) %in% fragen_vars, paste0(names(DatenD), "_D"), names(DatenD))

# Wie viele Fragen hat eine Person im Fragebogen gestellt?
FrageD <- Daten4 %>% # matches("F[0-9]+[a-zA-Z]") wählt alle Kommentarspalten aus
  mutate(across(matches("F[0-9]+[a-zA-Z]"), ~ if_else(nchar(.) > 2, ., NULL))) %>% # Nur Kommentare die länger sind als 2 Zeichen behalten (zum Teil als Kommentar nur eine "1")
  mutate(across(matches("F[0-9]+[a-zA-Z]"), ~ as.numeric(!is.na(.)))) %>% # alle NA's zu 0 und Kommentare zu 1
  mutate(FrageD = rowSums(select(., matches("F[0-9]+[a-zA-Z]")))) %>%  # Variable FrageA zeigt wie oft eine Person einen Kommentar gemacht hat
  select(FrageD)
DatenD <- cbind(DatenD, FrageD)

# Datensatzt E mit allen nötigen Var für Itemanalyse ---------------------------
DatenE <- select(Daten5, EndDate5, Pseudo, SC51, SC52, SC53, SC54, oft5, lange5, vl5,
               pflichtlit5, Muhe5, Duration5, verstanden4_1, verstanden4_2, verstanden4_3, verstanden4_4,
               
               F1_Page_Submit, F1_Click_Count, F1, FF1_Page_Submit, FF1_Click_Count,
               F2_Page_Submit, F2_Click_Count, F2, FF2_Page_Submit, FF2_Click_Count,
               F3_Page_Submit, F3_Click_Count, F3, FF3_Page_Submit, FF3_Click_Count,
               F4_Page_Submit, F4_Click_Count, F4, FF4_Page_Submit, FF4_Click_Count,
               F5_Page_Submit, F5_Click_Count, F5, FF5_Page_Submit, FF5_Click_Count,
               F6_Page_Submit, F6_Click_Count, F6, FF6_Page_Submit, FF6_Click_Count,
               F7_Page_Submit, F7_Click_Count, F7, FF7_Page_Submit, FF7_Click_Count,
               F8, # F8 kein Page_Submit bzw. Click Count
               F9_Page_Submit, F9_Click_Count, F9, FF9_Page_Submit, FF9_Click_Count,
               F10_Page_Submit, F10_Click_Count, F10, FF10_Page_Submit, FF10_Click_Count,
               F11_Page_Submit, F11_Click_Count, F11, FF11_Page_Submit, FF11_Click_Count,
               F12_Page_Submit, F12_Click_Count, F12, FF12_Page_Submit, FF12_Click_Count,
               F13_Page_Submit, F13_Click_Count, F13, FF13_Page_Submit, FF13_Click_Count,
               F14_Page_Submit, F14_Click_Count, F14, FF14_Page_Submit, FF14_Click_Count,
               F15_Page_Submit, F15_Click_Count, F15, FF15_Page_Submit, FF15_Click_Count
               )

DatenE$Pseudo<-tolower(DatenE$Pseudo)

# Wie oft erscheint das selbe Pseudonym?
DatenE <- DatenE %>%
  group_by(Pseudo) %>% # nach Pseudonym gruppieren
  mutate(num_E = n()) %>%  # Anzahl Fälle in jeder Gruppe in "num" speichern
  ungroup() # gruppierung wieder auflösen

# Ab alle Fragen-Variablen ein "_E" anfügen, damit im Gesamtdatensatz Fragenvariablen unterscheidbar bleiben
fragen_vars <- names(select(DatenE, F1_Page_Submit:FF15_Click_Count))
names(DatenE) <- ifelse(names(DatenE) %in% fragen_vars, paste0(names(DatenE), "_E"), names(DatenE))

# Wie viele Fragen hat eine Person im Fragebogen gestellt?
FrageE <- Daten5 %>% # matches("F[0-9]+\\.") wählt alle Kommentarspalten aus
  mutate(across(matches("F[0-9]+\\."), ~ if_else(nchar(.) > 2, ., NULL))) %>% # Nur Kommentare die länger sind als 2 Zeichen behalten (zum Teil als Kommentar nur eine "1")
  mutate(across(matches("F[0-9]+\\."), ~ as.numeric(!is.na(.)))) %>% # alle NA's zu 0 und Kommentare zu 1
  mutate(FrageE = rowSums(select(., matches("F[0-9]+\\.")))) %>%  # Variable FrageA zeigt wie oft eine Person einen Kommentar gemacht hat
  select(FrageE)
DatenE <- cbind(DatenE, FrageE)



# Datensatzt F mit allen nötigen Var für Itemanalyse ---------------------------
DatenF <- select(Daten6, EndDate6, Pseudo, SC61, SC62, SC63, oft6, lange6, vl6,
                 pflichtlit6, Muhe6, Duration6, verstanden5_1, verstanden5_2, verstanden5_3,
                 
                 F1_Page_Submit, F1_Click_Count, F1, FF1_Page_Submit, FF1_Click_Count,
                 F2_Page_Submit, F2_Click_Count, F2, FF2_Page_Submit, FF2_Click_Count,
                 F3_Page_Submit, F3_Click_Count, F3, FF3_Page_Submit, FF3_Click_Count,
                 F4_Page_Submit, F4_Click_Count, F4, FF4_Page_Submit, FF4_Click_Count,
                 F5_Page_Submit, F5_Click_Count, F5, FF5_Page_Submit, FF5_Click_Count,
                 F6_Page_Submit, F6_Click_Count, F6, FF6_Page_Submit, FF6_Click_Count,
                 F7_Page_Submit, F7_Click_Count, F7, FF7_Page_Submit, FF7_Click_Count,
                 F8, # F8 kein Page Submit oder Click Count
                 F9_Page_Submit, F9_Click_Count, F9, FF9_Page_Submit, FF9_Click_Count,
                 F10_Page_Submit, F10_Click_Count, F10, FF10_Page_Submit, FF10_Click_Count,
                 F11_Page_Submit, F11_Click_Count, F11 # FF11 kein Page Submit oder Click count
                 )
DatenF$Pseudo<-tolower(DatenF$Pseudo)

# Wie oft erscheint das selbe Pseudonym?
DatenF <- DatenF %>%
  group_by(Pseudo) %>% # nach Pseudonym gruppieren
  mutate(num_F = n()) %>%  # Anzahl Fälle in jeder Gruppe in "num" speichern
  ungroup() # gruppierung wieder auflösen

# Ab alle Fragen-Variablen ein "_F" anfügen, damit im Gesamtdatensatz Fragenvariablen unterscheidbar bleiben
fragen_vars <- names(select(DatenF, F1_Page_Submit:F11))
names(DatenF) <- ifelse(names(DatenF) %in% fragen_vars, paste0(names(DatenF), "_F"), names(DatenF))

# Wie viele Fragen hat eine Person im Fragebogen gestellt?
FrageF <- Daten6 %>% # matches("F[0-9]+\\.") wählt alle Kommentarspalten aus
  mutate(across(matches("F[0-9]+\\."), ~ if_else(nchar(.) > 2, ., NULL))) %>% # Nur Kommentare die länger sind als 2 Zeichen behalten (zum Teil als Kommentar nur eine "1")
  mutate(across(matches("F[0-9]+\\."), ~ as.numeric(!is.na(.)))) %>% # alle NA's zu 0 und Kommentare zu 1
  mutate(FrageF = rowSums(select(., matches("F[0-9]+\\.")))) %>%  # Variable FrageA zeigt wie oft eine Person einen Kommentar gemacht hat
  select(FrageF)
DatenF <- cbind(DatenF, FrageF)

# Datensatzt G mit allen nötigen Var für Itemanalyse ---------------------------
DatenG<-select(Daten7, EndDate7,Duration7, Pseudo, Forschung,Stunden, Vorbereitung, Stunden, Forumseintrage,
               Pflichtliteratur, Onlineubung, Vorlesunghoren, Folienanschauen, NotizenZusammenfassungen,
               Zusammenfassungschreiben, SemesterStrategien, oft7, Punktezahl, Musterlosung,
               Musterlosung2, Muhe7, Duration7,Probetest_sum,F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21,F22,F23,F24,F25)

# Rekonstruktion Pseudonym Datensatz 8 bzw. H ----------------------------------
Stand<-Daten8[,c(107:123)]
A<-Stand[,c("IRT1","IRT2","CFA1","CFA2","kritorient","Probetest","VWBerechnung")]
Cluster<-read_excel("richtigCluster.xlsx",col_names=TRUE)
Cluster<-as.data.frame(Cluster)
B<-Cluster[,c("IRT1","IRT2","CFA1","CFA2","kritorient","Probetest","VWBerechnung")]

a<-c()
b<-matrix(nrow=nrow(B),ncol=nrow(A))
for (i in 1:nrow(A)){
  for (j in 1:nrow(B)){
    a[j]<-sum(duplicated(rbind(A[i,],B[j,])))
    #a[j]<-A[i,]==B[j,] 
    }
  print(table(a))
  b[,i]<-a
  }
  
table(rowSums(b))
rownames(b)<-c(1:164)

true_cols <- apply(b, 2, function(data)
  names(which(data == T)))

true_cols<-as.numeric(true_cols)

Pseudo<-Cluster$ExternalDataReference[true_cols]
Daten8<-cbind(Daten8,Pseudo)
DatenH<-select(Daten8,Pseudo,Note)

# Alle Pseudonyme zu lowercase
DatenA$Pseudo<-tolower(DatenA$Pseudo)
DatenB$Pseudo<-tolower(DatenB$Pseudo)
DatenC$Pseudo<-tolower(DatenC$Pseudo)
DatenD$Pseudo<-tolower(DatenD$Pseudo)
DatenE$Pseudo<-tolower(DatenE$Pseudo)
DatenF$Pseudo<-tolower(DatenF$Pseudo)
DatenG$Pseudo<-tolower(DatenG$Pseudo)
DatenH$Pseudo<-tolower(DatenH$Pseudo)

# Relevante Fälle behalten -----------------------------------------------------

# Wir nehmen einfach den ersten Eintrag von beiden, ist das eine sinnvolle Annahmen?
# was wenn ich mich beim ersten nur durchklicke

# @Natalie: diese Bedenken sind berechtigt, ich habe mir die Personen welche die
# Fragebogen mehr als einmal gelöst haben mal angeschaut (nicht systematisch), und
# es ist tatsächlich so, das im 1. Durchgang oft sehr kurze Bearbeitungszeiten (z.T kürzer
# als 5 Miunten) sind! Hier ev. nochmal anschauen wie das am Besten gelöst wird (ev. längster Durchgang nehmen?)

#weniger als 10 Minuten Bearbeitunszeit unplausibel > raus

DatenA <- DatenA %>% 
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>% 
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration1/60 > 10)) ~ EndDate1 == min(EndDate1), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()
  
 



DatenB <- DatenB %>% 
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>% 
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration2/60 > 10)) ~ EndDate2 == min(EndDate2), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()


DatenC <- DatenC %>% 
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>% 
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration3/60 > 10)) ~ EndDate3 == min(EndDate3), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()



DatenD <- DatenD %>% 
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>% 
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration4/60 > 10)) ~ EndDate4 == min(EndDate4), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()



DatenE <- DatenE %>% 
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>% 
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration5/60 > 10)) ~ EndDate5 == min(EndDate5), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()



DatenF <- DatenF %>% 
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>% 
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration6/60 > 10)) ~ EndDate6 == min(EndDate6), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()




DatenG <- DatenG %>% 
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>% 
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration7/60 > 10)) ~ EndDate7 == min(EndDate7), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()





DatenH<-DatenH[!duplicated(DatenH$Pseudo),]



# Abgabezeitpunkte in DF einfügen
DatenA$rechtzeitig1 <- DatenA$EndDate1 < "2020-09-23 23:59:59"
DatenB$rechtzeitig2 <- DatenB$EndDate2 < "2020-10-07 23:59:59"
DatenC$rechtzeitig3 <- DatenC$EndDate3 < "2020-10-21 23:59:59"
DatenD$rechtzeitig4 <- DatenD$EndDate4 < "2020-11-04 23:59:59"
DatenE$rechtzeitig5 <- DatenE$EndDate5 < "2020-11-18 23:59:59"
DatenF$rechtzeitig6 <- DatenF$EndDate6 < "2020-12-02 23:59:59"

##Differenz
DatenA$Differenz1 <- as.Date(DatenA$EndDate1) - as.Date("2020-09-23 23:59:59")
DatenB$Differenz2 <- as.Date(DatenB$EndDate2) - as.Date("2020-10-07 23:59:59")
DatenC$Differenz3 <- as.Date(DatenC$EndDate3) - as.Date("2020-10-21 23:59:59")
DatenD$Differenz4 <- as.Date(DatenD$EndDate4) - as.Date("2020-11-04 23:59:59")
DatenE$Differenz5 <- as.Date(DatenE$EndDate5) - as.Date("2020-11-18 23:59:59")
DatenF$Differenz6 <- as.Date(DatenF$EndDate6) - as.Date("2020-12-02 23:59:59")


sum(duplicated(DatenH$Pseudo))

# Datensätze zu DatenTot zusammenfügen -----------------------------------------
DatenTot <- DatenA %>%
  full_join(DatenB, by='Pseudo') %>% 
  full_join(DatenC, by='Pseudo') %>% 
  full_join(DatenD, by='Pseudo') %>% 
  full_join(DatenE, by='Pseudo') %>% 
  full_join(DatenF, by='Pseudo') %>% 
  full_join(DatenG, by='Pseudo') %>% 
  full_join(DatenH, by='Pseudo') 

sum(duplicated(DatenA$Pseudo))
sum(duplicated(DatenB$Pseudo))
sum(duplicated(DatenC$Pseudo))
sum(duplicated(DatenD$Pseudo))
sum(duplicated(DatenE$Pseudo))
sum(duplicated(DatenF$Pseudo))
sum(duplicated(DatenG$Pseudo))
sum(duplicated(DatenH$Pseudo))


##!!!anschauen, was ist mit dieser Perosn? Wie kann die zur selben Zeit zwei unterschiedliche Probetests abgeben
##Ich habe den zweiten Jaev manuell herausgelöscht, im Datensaet unter herausgelöscht > keine Ahnung wie das sein kann
a<-duplicated(DatenG$Pseudo)
DatenG[a,]
a<-DatenB$Pseudo=="jaev08"
DatenB[a,]


#make pflichtlit binary > gelesen ja oder nein
DatenTot$pflichtlit2[DatenTot$pflichtlit2 == 1] <- 0 
DatenTot$pflichtlit2[DatenTot$pflichtlit2 > 1] <- 1 

DatenTot$pflichtlit3[DatenTot$pflichtlit3 == 1] <- 0 
DatenTot$pflichtlit3[DatenTot$pflichtlit3 > 1] <- 1 

DatenTot$pflichtlit4[DatenTot$pflichtlit4 == 1] <- 0 
DatenTot$pflichtlit4[DatenTot$pflichtlit4 > 1] <- 1 

DatenTot$pflichtlit5[DatenTot$pflichtlit5 == 1] <- 0 
DatenTot$pflichtlit5[DatenTot$pflichtlit5 > 1] <- 1 

DatenTot$pflichtlit6[DatenTot$pflichtlit6 == 1] <- 0 
DatenTot$pflichtlit6[DatenTot$pflichtlit6 > 1] <- 1 

DatenTot$pflichtlit<-rowMeans(cbind(DatenTot$pflichtlit2,DatenTot$pflichtlit3,DatenTot$pflichtlit4,DatenTot$pflichtlit5,DatenTot$pflichtlit6),na.rm=TRUE)


# nur vollständige Datensätze beibehalten
#Falsche Pseudonyme raushauen
#a <- nchar(DatenTot$Pseudo, type = "chars", allowNA = FALSE)==6|nchar(DatenTot$Pseudo, type = "chars", allowNA = FALSE)==5
#DatenTot <- DatenTot[a,]

# Datensätze auswählen, die nicht ausgeschlossen werden sollen (nur Ziffern)
#char <- as.character(DatenTot$Pseudo)
#b <- as.numeric(char)
#DatenTot <- DatenTot[is.na(b),]
#DatenTot$na_count <- apply(DatenTot, 1, function(x) sum(is.na(x)))

# DatenTot in RData File speichern damit nicht immer der ganze Code oben 
# gerannt werden muss
save(DatenTot, file = "DatenTot.RData")





# TODO: Ab hier könnte man ein neues Skript beginnen DateTot.RData laden

load("data/DatenTot.RData")


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ---- Funktion um Variablen zu glätten ----
# Diese Funktion verhindert zu grosse Werte: Werte die grösser sind als mean + 2SD werden auf
# ebendiesen Wert gesetzt. Als Argumente übernimmt die Funktion das Datenset und die Variablennamen
# als String die geglättet werden sollen
flaten_extreme_values <- function(dataset, variable.names) {
  dataset <- dataset %>% mutate(across(variable.names, ~ ifelse(.x > mean(., na.rm = T)+2*sd(., na.rm = T), # Testet ob  Wert grösser als mean+2SD ist
                                                                mean(., na.rm = T)+2*sd(., na.rm = T), # True: auf ebendiesen wert setzen
                                                                .x))) # False: original Wert beibehalten
  return(as_tibble(dataset))
}

# Relevante Faktor-Variablen (Summenscore) erstellen (Teil A) Standortbestimmung -----
# 1. Theoretische Grundlagen / 2. Berechnung / 3. Individualdiagnostik
DatenTot$theory <- rowSums(select(DatenTot, F2_A, F3_A, F3i_A, F4_A, F5_A, F8_A,
                                  F9_A, F11_A, F14_A, F18_A, F19_A, F21_A, F25_A))

DatenTot$practice <- rowSums(select(DatenTot, F1_A, F6_A, F7_A, F10_A, F12_A, F13_A,
                                  F16_A, F17_A, F17_1_A, F20_A))

DatenTot$indi <- rowSums(select(DatenTot, F23_A, F24_A, F26_A, F27_A))



# Vektor mit Variablen die geglättet werden sollen (A Page Submit) -------------
names_DatenA_Page_Submit <- names(select(DatenTot, F2_Page_Submit_A,F3_Page_Submit_A,F3i_Page_Submit_A,
                                                   F4_Page_Submit_A,F5_Page_Submit_A,F8_Page_Submit_A,
                                                   F9_Page_Submit_A,F11_Page_Submit_A,F14_Page_Submit_A,
                                                   F18_Page_Submit_A,F19_Page_Submit_A,F21_Page_Submit_A,
                                                   F25_Page_Submit_A,F1_Page_Submit_A,F6_Page_Submit_A,
                                                   F10_Page_Submit_A,F12_Page_Submit_A,F13_Page_Submit_A,
                                                   F16_Page_Submit_A,F17_Page_Submit_A,F17_1_Page_Submit_A,
                                                   F20_Page_Submit_A,F23_Page_Submit_A,F24_Page_Submit_A,
                                                   F26_Page_Submit_A,F27_Page_Submit_A))

# Ausgewählte Variablen (Page Submit des des "A" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(all_of(names_DatenA_Page_Submit))

DatenTot$BerechnungenRZ <- rowSums(select(DatenTot,F12_Page_Submit_A,F13_Page_Submit_A,F20_Page_Submit_A,F23_Page_Submit_A,F24_Page_Submit_A,
                                          F26_Page_Submit_A), na.rm = T)

DatenTot$TheorieRZ <- rowSums(select(DatenTot, F2_Page_Submit_A,F3_Page_Submit_A,F3i_Page_Submit_A,
                                    F4_Page_Submit_A,F5_Page_Submit_A,F8_Page_Submit_A,
                                    F9_Page_Submit_A,F11_Page_Submit_A,F14_Page_Submit_A,F18_Page_Submit_A,
                                    F19_Page_Submit_A,F21_Page_Submit_A,
                                    F25_Page_Submit_A))

DatenTot$practiceRZ <- rowSums(select(DatenTot, F1_Page_Submit_A,F6_Page_Submit_A,
                                      F10_Page_Submit_A,F12_Page_Submit_A,F13_Page_Submit_A,
                                      F16_Page_Submit_A,F17_Page_Submit_A,F17_1_Page_Submit_A,
                                      F20_Page_Submit_A), na.rm = T)



DatenTot$theoryRZ <- rowSums(select(DatenTot, F2_Page_Submit_A,F3_Page_Submit_A,F3i_Page_Submit_A,
                                    F4_Page_Submit_A,F5_Page_Submit_A,F8_Page_Submit_A,
                                    F9_Page_Submit_A,F11_Page_Submit_A,F14_Page_Submit_A,
                                    F18_Page_Submit_A,F19_Page_Submit_A,F21_Page_Submit_A,
                                    F25_Page_Submit_A))

DatenTot$practiceRZ <- rowSums(select(DatenTot, F1_Page_Submit_A,F6_Page_Submit_A,
                                      F10_Page_Submit_A,F12_Page_Submit_A,F13_Page_Submit_A,
                                      F16_Page_Submit_A,F17_Page_Submit_A,F17_1_Page_Submit_A,
                                      F20_Page_Submit_A), na.rm = T)

DatenTot$indiRZ <- rowSums(select(DatenTot, F23_Page_Submit_A,F24_Page_Submit_A,
                                  F26_Page_Submit_A,F27_Page_Submit_A))



# Vektor mit Variablen die geglättet werden sollen (A Click Count) -------------
names_DatenA_Click_Count <- names(select(DatenTot, F2_Click_Count_A,F3_Click_Count_A,F3i_Click_Count_A,
                                         F4_Click_Count_A,F5_Click_Count_A,F8_Click_Count_A,
                                         F9_Click_Count_A,F11_Click_Count_A,F14_Click_Count_A,
                                         F18_Click_Count_A,F19_Click_Count_A,F21_Click_Count_A,
                                         F25_Click_Count_A,F1_Click_Count_A,F6_Click_Count_A,
                                         F10_Click_Count_A,F12_Click_Count_A,F13_Click_Count_A,
                                         F16_Click_Count_A,F17_Click_Count_A,F17_1_Click_Count_A,
                                         F20_Click_Count_A,F23_Click_Count_A,F24_Click_Count_A,
                                         F26_Click_Count_A,F27_Click_Count_A))

# Ausgewählte Variablen (Click Count des des "A" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(all_of(names_DatenA_Click_Count))

# Relevante Variablen erstellen mit geglätteten Click Count erstllen
DatenTot$theoryKlick <- rowSums(select(DatenTot, F2_Click_Count_A,F3_Click_Count_A,F3i_Click_Count_A,
                                    F4_Click_Count_A,F5_Click_Count_A,F8_Click_Count_A,
                                    F9_Click_Count_A,F11_Click_Count_A,F14_Click_Count_A,
                                    F18_Click_Count_A,F19_Click_Count_A,F21_Click_Count_A,
                                    F25_Click_Count_A))

DatenTot$practiceKlick <- rowSums(select(DatenTot, F1_Click_Count_A,F6_Click_Count_A,
                                      F10_Click_Count_A,F12_Click_Count_A,F13_Click_Count_A,
                                      F16_Click_Count_A,F17_Click_Count_A,F17_1_Click_Count_A,
                                      F20_Click_Count_A))

DatenTot$indiKlick <- rowSums(select(DatenTot, F23_Click_Count_A,F24_Click_Count_A,
                                  F26_Click_Count_A,F27_Click_Count_A))



# Vektor mit Variablen die geglättet werden sollen (A FF Page Submit) -------------
names_DatenA_FFPage_Submit <- names(select(DatenTot, FF2_Page_Submit_A,FF3_Page_Submit_A,FF3i_Page_Submit_A,
                                         FF4_Page_Submit_A,FF5_Page_Submit_A,
                                         FF9_Page_Submit_A,FF11_Page_Submit_A,FF14_Page_Submit_A,
                                         FF18_Page_Submit_A,FF19_Page_Submit_A,FF21_Page_Submit_A,
                                         FF25_Page_Submit_A,FF1_Page_Submit_A,FF12_Page_Submit_A,
                                         FF16_Page_Submit_A,FF17_Page_Submit_A,FF17_1_Page_Submit_A,
                                         FF20_Page_Submit_A,FF23_Page_Submit_A,FF24_Page_Submit_A,
                                         FF26_Page_Submit_A,FF27_Page_Submit_A))

# Ausgewählte Variablen (FF Page Submit des des "A" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenA_FFPage_Submit)

# Relevante Variablen erstellen mit geglätteten Reaktionszeiten erstllen

DatenTot$BerechnungenFRZ <- rowSums(select(DatenTot,FF12_Page_Submit_A,FF20_Page_Submit_A,FF23_Page_Submit_A,FF24_Page_Submit_A,
                                          FF26_Page_Submit_A), na.rm = T)

DatenTot$TheorieFRZ <- rowSums(select(DatenTot, FF2_Page_Submit_A,FF3_Page_Submit_A,FF3i_Page_Submit_A,
                                     FF4_Page_Submit_A,FF5_Page_Submit_A,FF8_Page_Submit_A,
                                     FF9_Page_Submit_A,FF11_Page_Submit_A,FF14_Page_Submit_A,FF18_Page_Submit_A,
                                     FF19_Page_Submit_A,FF21_Page_Submit_A,
                                     FF25_Page_Submit_A))

DatenTot$theoryFRZ <- rowSums(select(DatenTot, FF2_Page_Submit_A,FF3_Page_Submit_A,FF3i_Page_Submit_A,
                                    FF4_Page_Submit_A,FF5_Page_Submit_A,
                                    FF9_Page_Submit_A,FF11_Page_Submit_A,FF14_Page_Submit_A,
                                    FF18_Page_Submit_A,FF19_Page_Submit_A,FF21_Page_Submit_A,
                                    FF25_Page_Submit_A))

DatenTot$practiceFRZ <- rowSums(select(DatenTot, FF1_Page_Submit_A,
                                      FF12_Page_Submit_A,
                                      FF16_Page_Submit_A,FF17_Page_Submit_A,FF17_1_Page_Submit_A,
                                      FF20_Page_Submit_A))

DatenTot$indiFRZ <- rowSums(select(DatenTot, FF23_Page_Submit_A,FF24_Page_Submit_A,
                                  FF26_Page_Submit_A,FF27_Page_Submit_A))


# Vektor mit Variablen die geglättet werden sollen (A FF Click Count) -------------
names_DatenA_FFClick_Count <- names(select(DatenTot,FF2_Click_Count_A,FF3_Click_Count_A,FF3i_Click_Count_A,
                                           FF4_Click_Count_A,FF5_Click_Count_A,
                                           FF9_Click_Count_A,FF11_Click_Count_A,FF14_Click_Count_A,
                                           FF18_Click_Count_A,FF19_Click_Count_A,FF21_Click_Count_A,
                                           FF25_Click_Count_A,FF1_Click_Count_A,
                                           FF12_Click_Count_A,
                                           FF16_Click_Count_A,FF17_Click_Count_A,FF17_1_Click_Count_A,
                                           FF20_Click_Count_A,FF23_Click_Count_A,FF24_Click_Count_A,
                                           FF26_Click_Count_A,FF27_Click_Count_A))

# Ausgewählte Variablen (FF Click Count des des "A" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenA_FFClick_Count)

# Relevante Variablen erstellen mit geglätteten Click Count erstllen
DatenTot$theoryFKlick <- rowSums(select(DatenTot, FF2_Click_Count_A,FF3_Click_Count_A,FF3i_Click_Count_A,
                                       FF4_Click_Count_A,FF5_Click_Count_A,
                                       FF9_Click_Count_A,FF11_Click_Count_A,FF14_Click_Count_A,
                                       FF18_Click_Count_A,FF19_Click_Count_A,FF21_Click_Count_A,
                                       FF25_Click_Count_A))

DatenTot$practiceFKlick <- rowSums(select(DatenTot, FF1_Click_Count_A,
                                         FF12_Click_Count_A,
                                         FF16_Click_Count_A,FF17_Click_Count_A,FF17_1_Click_Count_A,
                                         FF20_Click_Count_A))

DatenTot$indiFKlick <- rowSums(select(DatenTot, FF23_Click_Count_A,FF24_Click_Count_A,
                                     FF26_Click_Count_A,FF27_Click_Count_A))



# Relevante Faktor-Variablen (Summenscore) erstellen (Teil B) IRT1/2 -----
# 1. Lokale Stoachastische Unabh. / 2. ICC / 3. Parameterschätzung / 4. Spez. Objektivität
DatenTot <- DatenTot %>%
  mutate(lokalestoch = rowSums(select(., F1_B, F2_B, F3_B)),
         itemcharakter = rowSums(select(., F4_B, F5_B, F6_B)),
         parameterschaetz = rowSums(select(., F10_B, F11_B, F12_B, F13_B, F14_B)),
         spezobjektivi = rowSums(select(., F7_B, F9_B)),
         
         termin2 = rowSums(select(., F1_B, F2_B, F3_B, F4_B, F5_B, F6_B,
                                  F7_B, F9_B, F10_B, F11_B, F12_B, F13_B, F14_B))
  )

# Vektor mit Variablen die geglättet werden sollen (B Page Submit) -------------
names_DatenB_Page_Submit <- names(select(DatenTot,
                                         F1_Page_Submit_B, F2_Page_Submit_B, F4_Page_Submit_B,
                                         F5_Page_Submit_B, F6_Page_Submit_B, F7_Page_Submit_B,
                                         F8_Page_Submit_B, F9_Page_Submit_B, F10_Page_Submit_B,
                                         F11_Page_Submit_B, F12_Page_Submit_B, F13_Page_Submit_B,
                                         F14_Page_Submit_B)
                                  )

# Ausgewählte Variablen (Page Submit des des "B" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenB_Page_Submit)

# Variablen Page Submit für IRT1 Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(lokalestochRZ = rowSums(select(., F1_Page_Submit_B, F2_Page_Submit_B)),
         itemcharakterRZ = rowSums(select(., F4_Page_Submit_B, F5_Page_Submit_B, F6_Page_Submit_B)),
         parameterschaetzRZ = rowSums(select(., F10_Page_Submit_B, F11_Page_Submit_B,
                                             F12_Page_Submit_B, F13_Page_Submit_B, F14_Page_Submit_B)),
         spezobjektiviRZ = rowSums(select(., F7_Page_Submit_B, F8_Page_Submit_B, F9_Page_Submit_B))
  )



# Vektor mit Variablen die geglättet werden sollen (B Klick Count) -------------
names_DatenB_Click_Count <- names(select(DatenTot,
                                         F1_Click_Count_B, F2_Click_Count_B, F4_Click_Count_B,
                                         F5_Click_Count_B, F6_Click_Count_B, F7_Click_Count_B,
                                         F8_Click_Count_B, F9_Click_Count_B, F10_Click_Count_B,
                                         F11_Click_Count_B, F12_Click_Count_B, F13_Click_Count_B,
                                         F14_Click_Count_B)
)

# Ausgewählte Variablen (Klick Count des des "B" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenB_Click_Count)

# Variablen Klick Count für IRT1 Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(lokalestochKlick = rowSums(select(., F1_Click_Count_B, F2_Click_Count_B)),
         itemcharakterKlick = rowSums(select(., F4_Click_Count_B, F5_Click_Count_B, F6_Click_Count_B)),
         parameterschaetzKlick = rowSums(select(., F10_Click_Count_B,F11_Click_Count_B,
                                                F12_Click_Count_B, F13_Click_Count_B,F14_Click_Count_B)),
         spezobjektiviKlick = rowSums(select(., F7_Click_Count_B, F8_Click_Count_B, F9_Click_Count_B))
  )
# Vektor mit Variablen die geglättet werden sollen (B FF Page Submit) -------------
names_DatenB_FFPage_Submit <- names(select(DatenTot,
                                         FF1_Page_Submit_B, FF2_Page_Submit_B, FF3_Page_Submit_B,
                                         FF4_Page_Submit_B, FF5_Page_Submit_B, FF6_Page_Submit_B,
                                         FF7_Page_Submit_B, FF9_Page_Submit_B,
                                         FF10_Page_Submit_B,FF11_Page_Submit_B, FF12_Page_Submit_B,
                                         FF13_Page_Submit_B, FF14_Page_Submit_B)
)

# Ausgewählte Variablen (FF Page Submit des des "B" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenB_FFPage_Submit)

# Variablen Page Submit für IRT1 Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(lokalestochFRZ = rowSums(select(., FF1_Page_Submit_B, FF2_Page_Submit_B, FF3_Page_Submit_B)),
         itemcharakterFRZ = rowSums(select(., FF4_Page_Submit_B, FF5_Page_Submit_B, FF6_Page_Submit_B)),
         parameterschaetzFRZ = rowSums(select(., FF10_Page_Submit_B,FF11_Page_Submit_B, FF12_Page_Submit_B,
                                             FF13_Page_Submit_B, FF14_Page_Submit_B)),
         spezobjektiviFRZ = rowSums(select(., FF7_Page_Submit_B, FF9_Page_Submit_B))
  )



# Vektor mit Variablen die geglättet werden sollen (B FF Klick Count) -------------
names_DatenB_FFClick_Count <- names(select(DatenTot,
                                         FF1_Click_Count_B, FF2_Click_Count_B, FF3_Click_Count_B,
                                         FF4_Click_Count_B, FF5_Click_Count_B, FF6_Click_Count_B,
                                         FF7_Click_Count_B, FF9_Click_Count_B,
                                         FF10_Click_Count_B, FF11_Click_Count_B, FF12_Click_Count_B,
                                         FF13_Click_Count_B, FF14_Click_Count_B)
)

# Ausgewählte Variablen (FF Klick Count des des "B" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenB_FFClick_Count)

# Variablen FF Klick Count für IRT1 Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(lokalestochFKlick = rowSums(select(., FF1_Click_Count_B, FF2_Click_Count_B, FF3_Click_Count_B)),
         itemcharakterFKlick = rowSums(select(., FF4_Click_Count_B, FF5_Click_Count_B, FF6_Click_Count_B)),
         parameterschaetzFKlick = rowSums(select(., FF10_Click_Count_B, FF11_Click_Count_B, FF12_Click_Count_B,
                                                 FF13_Click_Count_B, FF14_Click_Count_B)),
         spezobjektiviFKlick = rowSums(select(., FF7_Click_Count_B, FF9_Click_Count_B))
  )

# Relevante Faktor-Variablen (Summenscore) erstellen (Teil C) IRT3/4 -----
# 1. Item/Testinformation, 2. R/Tests, 3. Adaptives Testen
DatenTot <- DatenTot %>%
  mutate(item_testinfo = rowSums(select(., F1_C,F2_C, F3_C)),
         r_tests = rowSums(select(., F4_C, F5_C, F6_C, F7_C, F8_C)),
         adaptiv_testen = rowSums(select(., F9_C, F10_C, F11_C, F12_C)),
         
         termin3 = rowSums(select(., F1_C, F2_C, F3_C, F4_C, F5_C, F6_C, F7_C, F8_C,
                                  F9_C, F10_C, F11_C, F12_C))
  )


# Vektor mit Variablen die geglättet werden sollen (C Page Submit) -------------
names_DatenC_Page_Submit <- names(select(DatenTot,
                                         F1_Page_Submit_C, F2_Page_Submit_C, F3_Page_Submit_C,
                                         F4_Page_Submit_C, F5_Page_Submit_C, F6_Page_Submit_C,
                                         F7_Page_Submit_C, F8_Page_Submit_C, F9_Page_Submit_C,
                                         F10_Page_Submit_C, F11_Page_Submit_C, F12_Page_Submit_C)
)

# Ausgewählte Variablen (Page Submit des des "C" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenC_Page_Submit)

# Variablen Page Submit für IRT3/4 Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(item_testinfoRZ = rowSums(select(., F1_Page_Submit_C,F2_Page_Submit_C, F3_Page_Submit_C)),
         r_testsRZ = rowSums(select(., F4_Page_Submit_C, F5_Page_Submit_C, F6_Page_Submit_C,
                                  F7_Page_Submit_C, F8_Page_Submit_C)),
         adaptiv_testenRZ = rowSums(select(., F9_Page_Submit_C,
                                         F10_Page_Submit_C, F11_Page_Submit_C, F12_Page_Submit_C))
  )

# Vektor mit Variablen die geglättet werden sollen (C Klick Count) -------------
names_DatenC_Click_Count <- names(select(DatenTot,
                                         F1_Click_Count_C, F2_Click_Count_C, F3_Click_Count_C,
                                         F4_Click_Count_C, F5_Click_Count_C, F6_Click_Count_C,
                                         F7_Click_Count_C, F8_Click_Count_C, F9_Click_Count_C,
                                         F10_Click_Count_C, F11_Click_Count_C, F12_Click_Count_C)
)

# Ausgewählte Variablen (Klick Count des des "C" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenC_Click_Count)

# Variablen Klick Count für IRT3/4 Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(item_testinfoKlick = rowSums(select(., F1_Click_Count_C, F2_Click_Count_C, F3_Click_Count_C)),
         r_testsKlick = rowSums(select(., F4_Click_Count_C, F5_Click_Count_C, F6_Click_Count_C,
                                       F7_Click_Count_C, F8_Click_Count_C)),
         adaptiv_testenKlick = rowSums(select(., F9_Click_Count_C,
                                              F10_Click_Count_C, F11_Click_Count_C, F12_Click_Count_C))
  )

# Vektor mit Variablen die geglättet werden sollen (C FF Page Submit) -------------
names_DatenC_FFPage_Submit <- names(select(DatenTot,
                                         FF1_Page_Submit_C, FF2_Page_Submit_C, FF3_Page_Submit_C,
                                         FF4_Page_Submit_C, FF5_Page_Submit_C, FF6_Page_Submit_C,
                                         FF7_Page_Submit_C, FF8_Page_Submit_C, FF9_Page_Submit_C,
                                         FF10_Page_Submit_C, FF11_Page_Submit_C, FF12_Page_Submit_C)
)

# Ausgewählte Variablen (FF Page Submit des des "C" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenC_FFPage_Submit)

# Variablen Page Submit für IRT3/4 Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(item_testinfoFRZ = rowSums(select(., FF1_Page_Submit_C)),
         r_testsFRZ = rowSums(select(., FF4_Page_Submit_C, FF5_Page_Submit_C, FF6_Page_Submit_C,
                                     FF7_Page_Submit_C, FF8_Page_Submit_C)),
         adaptiv_testenFRZ = rowSums(select(., FF9_Page_Submit_C,
                                            FF10_Page_Submit_C, FF11_Page_Submit_C, FF12_Page_Submit_C))
  )


# Vektor mit Variablen die geglättet werden sollen (C FF Klick Count) -------------
names_DatenC_FFClick_Count <- names(select(DatenTot,
                                         FF1_Click_Count_C, FF2_Click_Count_C, FF3_Click_Count_C,
                                         FF4_Click_Count_C, FF5_Click_Count_C, FF6_Click_Count_C,
                                         FF7_Click_Count_C, FF8_Click_Count_C, FF9_Click_Count_C,
                                         FF10_Click_Count_C, FF11_Click_Count_C, FF12_Click_Count_C)
                                  )

# Ausgewählte Variablen (Klick Count des des "C" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenC_FFClick_Count)

# Variablen Klick Count für IRT3/4 Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(item_testinfoFKlick = rowSums(select(., FF1_Click_Count_C, FF2_Click_Count_C, FF3_Click_Count_C)),
         r_testsFKlick = rowSums(select(., FF4_Click_Count_C, FF5_Click_Count_C, FF6_Click_Count_C,
                                        FF7_Click_Count_C, FF8_Click_Count_C)),
         adaptiv_testenFKlick = rowSums(select(., FF9_Click_Count_C,
                                               FF10_Click_Count_C, FF11_Click_Count_C, FF12_Click_Count_C))
         )



# Relevante Faktor-Variablen (Summenscore) erstellen (Teil D) CFA1/2 -----
# 1. Freiheitsgrade, 2. Parameterschätzung, 3. R Modellierung, 4. R Modellvergleich
DatenTot <- DatenTot %>%
  mutate(freiheitsgrade = rowSums(select(., F1_D, F2_D, F3_D, F4_D)),
         parameterschaetz_cfa = rowSums(select(., F6_D, F7_D, F8_D, F9_D)),
         r_modelierung = rowSums(select(., F10_D, F11_D, F12_D, F13_D, F14_D)),
         r_modelvergleich = rowSums(select(., F15_D, F16_D, F17_D)),
         
         termin4 = rowSums(select(., F1_D, F2_D, F3_D, F4_D, F6_D, F7_D, F8_D, F9_D,
                                  F10_D, F11_D, F12_D, F13_D, F14_D, F15_D, F16_D, F17_D))
  )

# Vektor mit Variablen die geglättet werden sollen (D Page Submit) -------------
names_DatenD_Page_Submit <- names(select(DatenTot,
                                         F1_Page_Submit_D, F2_Page_Submit_D, F3_Page_Submit_D,
                                         F4_Page_Submit_D, F6_Page_Submit_D,
                                         F7_Page_Submit_D, F8_Page_Submit_D, F9_Page_Submit_D,
                                         F10_Page_Submit_D, F11_Page_Submit_D, F12_Page_Submit_D,
                                         F13_Page_Submit_D, F14_Page_Submit_D, F15_Page_Submit_D,
                                         F16_Page_Submit_D, F17_Page_Submit_D)
)

# Ausgewählte Variablen (Page Submit des des "D" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenD_Page_Submit)

# Variablen Page Submit für CFA1/2 Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(freiheitsgradeRZ = rowSums(select(., F1_Page_Submit_D, F2_Page_Submit_D, F3_Page_Submit_D, F4_Page_Submit_D)),
         parameterschaetz_cfaRZ = rowSums(select(., F6_Page_Submit_D,F7_Page_Submit_D, F8_Page_Submit_D, F9_Page_Submit_D)),
         r_modelierungRZ = rowSums(select(., F10_Page_Submit_D, F11_Page_Submit_D, F12_Page_Submit_D,F13_Page_Submit_D, F14_Page_Submit_D)),
         r_modelvergleichRZ = rowSums(select(., F15_Page_Submit_D,F16_Page_Submit_D, F17_Page_Submit_D))
  )

# Vektor mit Variablen die geglättet werden sollen (D Klick Count) -------------
names_DatenD_Click_Count <- names(select(DatenTot,
                                         F1_Click_Count_D, F2_Click_Count_D, F3_Click_Count_D,
                                         F4_Click_Count_D, F6_Click_Count_D,
                                         F7_Click_Count_D, F8_Click_Count_D, F9_Click_Count_D,
                                         F10_Click_Count_D, F11_Click_Count_D, F12_Click_Count_D,
                                         F13_Click_Count_D, F14_Click_Count_D, F15_Click_Count_D,
                                         F16_Click_Count_D, F17_Click_Count_D)
)

# Ausgewählte Variablen (Klick Count des des "D" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenD_Click_Count)

# Variablen Page Submit für CFA1/2 Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(freiheitsgradeKlick = rowSums(select(., F1_Click_Count_D, F2_Click_Count_D, F3_Click_Count_D, F4_Click_Count_D)),
         parameterschaetz_cfaKlick = rowSums(select(., F6_Click_Count_D, F7_Click_Count_D, F8_Click_Count_D, F9_Click_Count_D)),
         r_modelierungKlick = rowSums(select(., F10_Click_Count_D, F11_Click_Count_D, F12_Click_Count_D, F13_Click_Count_D, F14_Click_Count_D)),
         r_modelvergleichKlick = rowSums(select(., F15_Click_Count_D, F16_Click_Count_D, F17_Click_Count_D))
  )


# Vektor mit Variablen die geglättet werden sollen (D FF Page Submit) -------------
names_DatenD_FFPage_Submit <- names(select(DatenTot,
                                         FF1_Page_Submit_D, FF2_Page_Submit_D, FF3_Page_Submit_D,
                                         FF4_Page_Submit_D, FF6_Page_Submit_D,
                                         FF7_Page_Submit_D, FF8_Page_Submit_D, FF9_Page_Submit_D,
                                         FF10_Page_Submit_D, FF11_Page_Submit_D, FF12_Page_Submit_D,
                                         FF13_Page_Submit_D, FF14_Page_Submit_D, FF15_Page_Submit_D,
                                         FF16_Page_Submit_D, FF17_Page_Submit_D)
)

# Ausgewählte Variablen (FF Page Submit des des "D" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenD_FFPage_Submit)

# Variablen FF Page Submit für CFA1/2 Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(freiheitsgradeFRZ = rowSums(select(., FF1_Page_Submit_D, FF2_Page_Submit_D, FF3_Page_Submit_D, FF4_Page_Submit_D)),
         parameterschaetz_cfaFRZ = rowSums(select(., FF6_Page_Submit_D, FF7_Page_Submit_D, FF8_Page_Submit_D, FF9_Page_Submit_D)),
         r_modelierungFRZ = rowSums(select(., FF10_Page_Submit_D, FF11_Page_Submit_D, FF12_Page_Submit_D, FF13_Page_Submit_D, FF14_Page_Submit_D)),
         r_modelvergleichFRZ = rowSums(select(.,FF17_Page_Submit_D))
  )


# Vektor mit Variablen die geglättet werden sollen (D FF Klick Count) -------------
names_DatenD_FFClick_Count <- names(select(DatenTot,
                                           FF1_Click_Count_D, FF2_Click_Count_D, FF3_Click_Count_D,
                                           FF4_Click_Count_D, FF6_Click_Count_D,
                                           FF7_Click_Count_D, FF8_Click_Count_D, FF9_Click_Count_D,
                                           FF10_Click_Count_D, FF11_Click_Count_D, FF12_Click_Count_D,
                                           FF13_Click_Count_D, FF14_Click_Count_D, FF15_Click_Count_D,
                                           FF16_Click_Count_D, FF17_Click_Count_D)
)

# Ausgewählte Variablen (FF Klick Count des des "D" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenD_FFClick_Count)

# Variablen Page Submit für CFA1/2 Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(freiheitsgradeFKlick = rowSums(select(., F1_Click_Count_D, F2_Click_Count_D, F3_Click_Count_D, F4_Click_Count_D)),
         parameterschaetz_cfaFKlick = rowSums(select(., F6_Click_Count_D, F7_Click_Count_D, F8_Click_Count_D, F9_Click_Count_D)),
         r_modelierungFKlick = rowSums(select(., F10_Click_Count_D, F11_Click_Count_D, F12_Click_Count_D, F13_Click_Count_D, F14_Click_Count_D)),
         r_modelvergleichFKlick = rowSums(select(., F15_Click_Count_D, F16_Click_Count_D, F17_Click_Count_D))
  )



# Relevante Faktor-Variablen (Summenscore) erstellen (Teil E) CFA3/4 -----
# 1. MTMM, 2. Fairness, 3. R Reliability, 4. R Invarianz
DatenTot <- DatenTot %>%
  mutate(mtmm = rowSums(select(., F1_E, F2_E, F3_E, F4_E)),
         fairness = rowSums(select(., F5_E, F6_E, F7_E)),
         reliability = rowSums(select(., F9_E, F10_E, F11_E, F12_E)),
         invarianz = rowSums(select(., F13_E, F14_E, F15_E)),
         
         termin5 = rowSums(select(., F1_E, F2_E, F3_E, F4_E, F5_E, F6_E, F7_E,
                                  F9_E, F10_E, F11_E, F12_E, F13_E, F14_E, F15_E))
  )


# Vektor mit Variablen die geglättet werden sollen (E Page Submit) -------------
names_DatenE_Page_Submit <- names(select(DatenTot,
                                         F1_Page_Submit_E, F2_Page_Submit_E, F2_Page_Submit_E, F4_Page_Submit_E,
                                         F5_Page_Submit_E, F6_Page_Submit_E, F7_Page_Submit_E,
                                         F9_Page_Submit_E, F10_Page_Submit_E, F11_Page_Submit_E, F12_Page_Submit_E,
                                         F13_Page_Submit_E, F14_Page_Submit_E, F15_Page_Submit_E)
                                  )

# Ausgewählte Variablen (Page Submit des des "E" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenE_Page_Submit)

# Variablen Page Submit für CFA3/4 Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(mtmmRZ = rowSums(select(., F1_Page_Submit_E, F2_Page_Submit_E, F2_Page_Submit_E, F4_Page_Submit_E)),
         fairnessRZ = rowSums(select(., F5_Page_Submit_E, F6_Page_Submit_E, F7_Page_Submit_E)),
         reliabilityRZ = rowSums(select(., F9_Page_Submit_E, F10_Page_Submit_E, F11_Page_Submit_E, F12_Page_Submit_E)),
         invarianzRZ = rowSums(select(., F13_Page_Submit_E, F14_Page_Submit_E, F15_Page_Submit_E))
         )

# Vektor mit Variablen die geglättet werden sollen (E Klick Count) -------------
names_DatenE_Click_Count <- names(select(DatenTot,
                                         F1_Click_Count_E, F2_Click_Count_E, F2_Click_Count_E, F4_Click_Count_E,
                                         F5_Click_Count_E, F6_Click_Count_E, F7_Click_Count_E,
                                         F9_Click_Count_E, F10_Click_Count_E, F11_Click_Count_E, F12_Click_Count_E,
                                         F13_Click_Count_E, F14_Click_Count_E, F15_Click_Count_E)
)

# Ausgewählte Variablen (Klick Count des des "E" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenE_Click_Count)

# Variablen Klick Count für CFA3/4 Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(mtmmKlick = rowSums(select(., F1_Click_Count_E, F2_Click_Count_E, F2_Click_Count_E, F4_Click_Count_E)),
         fairnessKlick = rowSums(select(., F5_Click_Count_E, F6_Click_Count_E, F7_Click_Count_E)),
         reliabilityKlick = rowSums(select(., F9_Click_Count_E, F10_Click_Count_E, F11_Click_Count_E, F12_Click_Count_E)),
         invarianzKlick = rowSums(select(., F13_Click_Count_E, F14_Click_Count_E, F15_Click_Count_E))
  )




# Vektor mit Variablen die geglättet werden sollen (E FF Page Submit) -------------
names_DatenE_FFPage_Submit <- names(select(DatenTot,
                                         FF1_Page_Submit_E, FF2_Page_Submit_E, FF2_Page_Submit_E, FF4_Page_Submit_E,
                                         FF5_Page_Submit_E, FF6_Page_Submit_E, FF7_Page_Submit_E,
                                         FF9_Page_Submit_E, FF10_Page_Submit_E, FF11_Page_Submit_E, FF12_Page_Submit_E,
                                         FF13_Page_Submit_E, FF14_Page_Submit_E, FF15_Page_Submit_E)
                                    )

# Ausgewählte Variablen (FF Page Submit des des "E" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenE_FFPage_Submit)

# Variablen ff Page Submit für CFA3/4 Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(mtmmFRZ = rowSums(select(., FF1_Page_Submit_E, FF2_Page_Submit_E, FF2_Page_Submit_E, FF4_Page_Submit_E)),
         fairnessFRZ = rowSums(select(., FF5_Page_Submit_E, FF6_Page_Submit_E, FF7_Page_Submit_E)),
         reliabilityFRZ = rowSums(select(., FF9_Page_Submit_E, FF10_Page_Submit_E, FF11_Page_Submit_E, FF12_Page_Submit_E)),
         invarianzFRZ = rowSums(select(., FF13_Page_Submit_E, FF14_Page_Submit_E, FF15_Page_Submit_E))
  )




# Vektor mit Variablen die geglättet werden sollen (E FF Klick Count) -------------
names_DatenE_FFClick_Count <- names(select(DatenTot,
                                         FF1_Click_Count_E, FF2_Click_Count_E, FF2_Click_Count_E, FF4_Click_Count_E,
                                         FF5_Click_Count_E, FF6_Click_Count_E, FF7_Click_Count_E,
                                         FF9_Click_Count_E, FF10_Click_Count_E, FF11_Click_Count_E, FF12_Click_Count_E,
                                         FF13_Click_Count_E,FF14_Click_Count_E, FF15_Click_Count_E)
)

# Ausgewählte Variablen (FF Klick Count des des "E" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenE_FFClick_Count)

# Variablen FF Klick Count für CFA3/4 Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(mtmmFKlick = rowSums(select(., FF1_Click_Count_E, FF2_Click_Count_E, FF2_Click_Count_E, FF4_Click_Count_E)),
         fairnessFKlick = rowSums(select(., FF5_Click_Count_E, FF6_Click_Count_E, FF7_Click_Count_E)),
         reliabilityFKlick = rowSums(select(., FF9_Click_Count_E, FF10_Click_Count_E, FF11_Click_Count_E, FF12_Click_Count_E)),
         invarianzFKlick = rowSums(select(., FF13_Click_Count_E,FF14_Click_Count_E, FF15_Click_Count_E))
  )



# Relevante Faktor-Variablen (Summenscore) erstellen (Teil F) Kriteriumsorientiert -----
# 1. Validität, 2. Cutoff, 3. Reliabilität
DatenTot <- DatenTot %>%
  mutate(validity = rowSums(select(., F2_F, F3_F, F4_F, F5_F, F6_F)),
         cutoff = rowSums(select(., F7_F, F8_F, F9_F, F10_F)),
         reliability_krit = rowSums(select(., F1_F, F11_F)),
         
         termin6 = rowSums(select(., F2_F, F3_F, F4_F, F5_F, F6_F,
                                  F7_F, F8_F, F9_F, F10_F, F1_F, F11_F))
  )

# Vektor mit Variablen die geglättet werden sollen (F Page Submit) -------------
names_DatenF_Page_Submit <- names(select(DatenTot,
                                         F2_Page_Submit_F, F3_Page_Submit_F, F4_Page_Submit_F, F5_Page_Submit_F, F6_Page_Submit_F,
                                         F7_Page_Submit_F, F9_Page_Submit_F, F10_Page_Submit_F,
                                         F1_Page_Submit_F, F11_Page_Submit_F))

# Ausgewählte Variablen (Page Submit des des "F" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenF_Page_Submit)

# Variablen Page Submit für Kriteriumsorientiert Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(validityRZ = rowSums(select(., F2_Page_Submit_F, F3_Page_Submit_F, F4_Page_Submit_F, F5_Page_Submit_F, F6_Page_Submit_F)),
         cutoffRZ = rowSums(select(., F7_Page_Submit_F, F9_Page_Submit_F, F10_Page_Submit_F)),
         reliability_kritRZ = rowSums(select(., F1_Page_Submit_F, F11_Page_Submit_F))
  )


# Vektor mit Variablen die geglättet werden sollen (F Klick Count) -------------
names_DatenF_Click_Count <- names(select(DatenTot,
                                          F2_Click_Count_F, F3_Click_Count_F, F4_Click_Count_F, F5_Click_Count_F, F6_Click_Count_F,
                                          F7_Click_Count_F, F9_Click_Count_F, F10_Click_Count_F,
                                          F1_Click_Count_F, F11_Click_Count_F))

# Ausgewählte Variablen (Klick Count des des "F" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenF_Click_Count)

# Variablen Page Submit für Kriteriumsorientiert Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(validityKlick = rowSums(select(., F2_Click_Count_F, F3_Click_Count_F, F4_Click_Count_F, F5_Click_Count_F, F6_Click_Count_F)),
         cutoffKlick = rowSums(select(., F7_Click_Count_F, F9_Click_Count_F, F10_Click_Count_F)),
         reliability_kritKlick = rowSums(select(., F1_Click_Count_F, F11_Click_Count_F))
  )

# Vektor mit Variablen die geglättet werden sollen (F FF Page Submit) -------------
names_DatenF_FFPage_Submit <- names(select(DatenTot,
                                         FF2_Page_Submit_F, FF3_Page_Submit_F, FF4_Page_Submit_F, FF5_Page_Submit_F, FF6_Page_Submit_F,
                                         FF7_Page_Submit_F, FF9_Page_Submit_F, FF10_Page_Submit_F,
                                         FF1_Page_Submit_F))

# Ausgewählte Variablen (FF Page Submit des des "F" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenF_FFPage_Submit)

# Variablen FF Page Submit für Kriteriumsorientiert Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(validityFRZ = rowSums(select(., FF2_Page_Submit_F, FF3_Page_Submit_F, FF4_Page_Submit_F, FF5_Page_Submit_F, FF6_Page_Submit_F)),
         cutoffFRZ = rowSums(select(., FF7_Page_Submit_F, FF9_Page_Submit_F, FF10_Page_Submit_F)),
         reliability_kritFRZ = rowSums(select(., FF1_Page_Submit_F))
  )

# Vektor mit Variablen die geglättet werden sollen (F FF Klick Count) -------------
names_DatenF_FFClick_Count <- names(select(DatenTot,
                                         FF2_Click_Count_F, FF3_Click_Count_F, FF4_Click_Count_F, FF5_Click_Count_F, FF6_Click_Count_F,
                                         FF7_Click_Count_F, FF9_Click_Count_F, FF10_Click_Count_F,
                                         FF1_Click_Count_F))

# Ausgewählte Variablen (FF Klick Count des des "F" Teils) glätten und in DatenTot speichern
DatenTot <- DatenTot %>% flaten_extreme_values(names_DatenF_FFClick_Count)

# Variablen Page Submit für Kriteriumsorientiert Faktoren generieren
DatenTot <- DatenTot %>%
  mutate(validityFKlick = rowSums(select(., FF2_Click_Count_F, FF3_Click_Count_F, FF4_Click_Count_F, FF5_Click_Count_F, FF6_Click_Count_F)),
         cutoffFKlick = rowSums(select(., FF7_Click_Count_F, FF9_Click_Count_F, FF10_Click_Count_F)),
         reliability_kritFKlick = rowSums(select(., FF1_Click_Count_F))
  )



# Verstanden Varialben
DatenTot <- DatenTot %>% 
  mutate(verstanden_B = rowMeans(select(., verstanden1_1, verstanden1_2, verstanden1_3,
                                       verstanden1_4, verstanden1_5, verstanden1_6),
                                na.rm = TRUE),
         
         verstanden_C = rowMeans(select(., verstanden2_1, verstanden2_2, verstanden2_3,
                                        verstanden2_4, verstanden2_5, verstanden2_6),
                                 na.rm = TRUE),
        
         verstanden_D = rowMeans(select(., verstanden3_1, verstanden3_2, verstanden3_3),
                                 na.rm = TRUE),
         
         verstanden_E = rowMeans(select(., verstanden4_1, verstanden4_2, verstanden4_3,
                                        verstanden4_4),
                                 na.rm = TRUE),
         
         verstanden_F = rowMeans(select(., verstanden5_1, verstanden5_2, verstanden5_3),
                                 na.rm = TRUE))

# Variable Forum_lesen
DatenTot <- DatenTot %>% 
  mutate(Forum_lesen = case_when(grepl("[5]", DatenTot$SemesterStrategien) ~ 2,
                                 grepl("[3 4]", DatenTot$SemesterStrategien) ~ 1,
                                 TRUE ~ 0))


Muhe<-cbind(DatenTot$Muhe1,DatenTot$Muhe2,DatenTot$Muhe3,DatenTot$Muhe4,DatenTot$Muhe5,DatenTot$Muhe6,DatenTot$Muhe7)

DatenTot$Muhe<-rowMeans(Muhe,na.rm=TRUE)

DatenTot$Fragen<-rowMeans(cbind(DatenTot$FrageA,DatenTot$FrageB,DatenTot$FrageC,DatenTot$FrageD,DatenTot$FrageE,DatenTot$FrageF),na.rm=TRUE)
# Finaler Datensatzt abspeichern ----
DatenTot_final <- DatenTot



save(DatenTot_final, file = "DatenTot_final.RData")
write_csv(DatenTot_final, "DatenTot_final.csv")

#------
Prüfung<-DatenTot$Prüfung

a<-Prüfung==6
Prüfung[a]<-NA

Relevanz1<-DatenTot$Relevanz1
Relevanz2<-DatenTot$Relevanz2
Relevanz3<-DatenTot$Relevanz3

     
Ziel<-DatenTot$Ziel
rechtzeitig1<-DatenTot$rechtzeitig1
Differenz1<-DatenTot$Differenz1
FrageA<-DatenTot$FrageA
Anzahl<-DatenTot$num_A
Note<-DatenTot$Note
Punktezahl<-DatenTot$Punktezahl


#####################reduzieren auf vollständige Datensätze
library(psych)
describe(DatenTot$na_count)
table(DatenTot$na_count)


##Personen könnte man über Missings herausfiltern, wir machen es über Note
#Daten_neu<-DatenTot[DatenTot$na_count<150,]
#b<-!is.na(Daten_neu$Note)
#table(Daten_neu$Musterlosung)
#table(Daten_neu$Musterlosung2)
#table(Daten_neu$Forschung)
b<-!is.na(DatenTot$Note)
Daten_neu<-DatenTot[b,]


Dat <- Daten_neu %>%
  select(Pseudo,theory,practice,indi,theoryKlick,practiceKlick,indiKlick,theoryRZ,practiceRZ,indiRZ,theoryFKlick,practiceFKlick,indiFKlick,theoryFRZ,practiceFRZ,indiFRZ,Relevanz1,Relevanz2,Relevanz3,Muhe,Ziel,Differenz1,FrageA,num_A,Note,Punktezahl)


# ------------------------------------------------------------------------------
# Bis hier Skript bearbeitet ---------------------------------------------------
# ------------------------------------------------------------------------------


##numerische Variablen auswählen, numerisch machen, Resultate anschauen
#for (i in 2:dim(Daten_neu)[2]) {
#  Daten_neu[,i] <- c(as.numeric(Daten_neu[[i]]))
#}


##numerische Variablen auswählen, numerisch machen, Resultate anschauen
for (i in 2:dim(Dat)[2]) {

    Dat[,i] <- c(as.numeric(Dat[[i]]))
}



library(xlsx)
write.xlsx(Dat,"Test_Ioan.xlsx")



