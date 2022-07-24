# Auswertung der Learning Analytics Psychologische Diagnostik 2021

# Bibliotheken laden
library(tidyverse)
library(readxl)

# Daten einlesen
data_sb_1 <- read_excel("data_all_2022.xlsx",sheet="SB_2021_1",col_names=TRUE)
data_sb_2 <- read_excel("data_all_2022.xlsx",sheet="SB_2021_2",col_names=TRUE)
data_irt12_1 <- read_excel("data_all_2022.xlsx",sheet="IRT12_2021_1",col_names=TRUE)
data_irt12_2 <- read_excel("data_all_2022.xlsx",sheet="IRT12_2021_2",col_names=TRUE)
data_irt34_1 <- read_excel("data_all_2022.xlsx",sheet="IRT34_2021_1",col_names=TRUE)
data_irt34_2 <- read_excel("data_all_2022.xlsx",sheet="IRT34_2021_2",col_names=TRUE)
data_cfa12_1 <- read_excel("data_all_2022.xlsx",sheet="CFA12_2021_1",col_names=TRUE)
data_cfa12_2 <- read_excel("data_all_2022.xlsx",sheet="CFA12_2021_2",col_names=TRUE)
data_cfa34_1 <- read_excel("data_all_2022.xlsx",sheet="CFA34_2021_1",col_names=TRUE)
data_cfa34_2 <- read_excel("data_all_2022.xlsx",sheet="CFA34_2021_2",col_names=TRUE)
data_Krit_1 <- read_excel("data_all_2022.xlsx",sheet="Krit_2021_1",col_names=TRUE)
data_Krit_2 <- read_excel("data_all_2022.xlsx",sheet="Krit_2021_2",col_names=TRUE)
data_Probetest <- read_excel("data_all_2022.xlsx",sheet="Probep_2021_1",col_names=TRUE)




# Nur komplette Fälle filtern ----
#das Kritierum ist so gewählt, dass alle Übungsaufgaben beantwortet werden mussten, Mühe fehlt aber teilweise. Auswahl basiert im Moment nur auf 1.Durchgang zwiete Termine nicht geschaut
data_sb_1 <- data_sb_1 %>% filter(Progress > 91)
data_sb<-data_sb_1
data_sb_2 <- data_sb_2 %>% filter(Progress > 91)
data_irt12_1 <- data_irt12_1 %>% filter(Progress > 91)
data_irt12_2 <- data_irt12_2 %>% filter(Progress > 91)
data_irt34_1 <- data_irt34_1 %>% filter(Progress > 90)
data_irt34_2 <- data_irt34_2 %>% filter(Progress > 90)
data_cfa12_1 <- data_cfa12_1 %>% filter(Progress > 95)
data_cfa12_2 <- data_cfa12_2 %>% filter(Progress > 95)
data_cfa34_1 <- data_cfa34_1 %>% filter(Progress > 93)
data_cfa34_2 <- data_cfa34_2 %>% filter(Progress > 93)
data_Krit_1 <- data_Krit_1 %>% filter(Progress > 91)
data_Krit_2 <- data_Krit_2 %>% filter(Progress >91)
data_Probetest<-data_Probetest  %>% filter(Progress >81)
#auch bei Probetest braucht es Progress angabe. Wie viel habe ich da 2020 genommen?




# 1. Termin Standortbestimmung Variablen umbenennen ----
data_sb_1 <- data_sb_1 %>%
  rename(`Q198_Page Submit` = `Q198_Page Submit...321`,
         `Q198_Click Count` = `Q198_Click Count...322`,
         `Q203_Page Submit` = `Q203_Page Submit...349`,
         `Q203_Click Count` = `Q203_Click Count...350`,
         `Q204_Page Submit` = `Q204_Page Submit...354`,
         `Q204_Click Count` = `Q204_Click Count...355`,
         F17 = F17...241,
         F17i = F17...252,
         "Duration_seconds" = `Duration (in seconds)`,
         Muhe = Muhe1)

data_sb_1 <- data_sb_1 %>%
  rename(F1_Page_Submit = `Q149_Page Submit`,
         F1_Click_Count = `Q149_Click Count`,
         FF1_Page_Submit = `Q153_Page Submit`,
         FF1_Click_Count = `Q153_Click Count`,
         
         F2_Page_Submit = `Q150_Page Submit`,
         F2_Click_Count = `Q150_Click Count`,
         FF2_Page_Submit = `Q155_Page Submit`,
         FF2_Click_Count = `Q155_Click Count`,
         
         F3_Page_Submit = `Q151_Page Submit`,
         F3_Click_Count = `Q151_Click Count`,
         FF3_Page_Submit = `Q152_Page Submit`,
         FF3_Click_Count = `Q152_Click Count`,
         
         F3i_Page_Submit = `Q156_Page Submit`,
         F3i_Click_Count = `Q156_Click Count`,
         FF3i_Page_Submit = `Q157_Page Submit`,
         FF3i_Click_Count = `Q157_Click Count`,
         
         #F3ii_Page_Submit = -> kein Page Submit
         #F3ii_Click_Count = -> kein Click Count`,
         FF3ii_Page_Submit = `Q158_Page Submit`,
         FF3ii_Click_Count = `Q158_Click Count`,
         
         F4_Page_Submit = `Q161_Page Submit`,
         F4_Click_Count = `Q161_Click Count`,
         FF4_Page_Submit = `Q160_Page Submit`,
         FF4_Click_Count = `Q160_Click Count`,
         
         F5_Page_Submit = `Q162_Page Submit`,
         F5_Click_Count = `Q162_Click Count`,
         FF5_Page_Submit = `Q163_Page Submit`,
         FF5_Click_Count = `Q163_Click Count`,
         
         #F6_Page_Submit = -> kein Page Submit
         #F6_Click_Count = -> kein Click Count`,
         FF6_Page_Submit = `Q164_Page Submit`,
         FF6_Click_Count = `Q164_Click Count`,
         
         #F7_Page_Submit = -> kein Page Submit
         #F7_Click_Count = -> kein Click Count`,
         FF7_Page_Submit = `Q165_Page Submit`,
         FF7_Click_Count = `Q165_Click Count`,
         
         F8_Page_Submit = `Q167_Page Submit`,
         F8_Click_Count = `Q167_Click Count`,
         FF8_Page_Submit = `Q166_Page Submit`,
         FF8_Click_Count = `Q166_Click Count`,
         
         F9_Page_Submit = `Q168_Page Submit`,
         F9_Click_Count = `Q168_Click Count`,
         FF9_Page_Submit = `Q169_Page Submit`,
         FF9_Click_Count = `Q169_Click Count`,
         
         F10_Page_Submit = `Q170_Page Submit`,
         F10_Click_Count = `Q170_Click Count`,
         FF10_Page_Submit = `Q171_Page Submit`,
         FF10_Click_Count = `Q171_Click Count`,
         
         F11_Page_Submit = `Q172_Page Submit`,
         F11_Click_Count = `Q172_Click Count`,
         FF11_Page_Submit = `Q173_Page Submit`,
         FF11_Click_Count = `Q173_Click Count`,
         
         F12_Page_Submit = `Q174_Page Submit`,
         F12_Click_Count = `Q174_Click Count`,
         FF12_Page_Submit = `Q175_Page Submit`,
         FF12_Click_Count = `Q175_Click Count`,
         
         F13_Page_Submit = `Q176_Page Submit`,
         F13_Click_Count = `Q176_Click Count`,
         FF13_Page_Submit = `Q177_Page Submit`,
         FF13_Click_Count = `Q177_Click Count`,
         
         F14_Page_Submit = `Q178_Page Submit`,
         F14_Click_Count = `Q178_Click Count`,
         FF14_Page_Submit = `Q179_Page Submit`,
         FF14_Click_Count = `Q179_Click Count`,
         
         # Keine F15
         
         F16_Page_Submit = `Q180_Page Submit`,
         F16_Click_Count = `Q180_Click Count`,
         FF16_Page_Submit = `Q181_Page Submit`,
         FF16_Click_Count = `Q181_Click Count`,
         
         F17_Page_Submit = `Q182_Page Submit`,
         F17_Click_Count = `Q182_Click Count`,
         FF17_Page_Submit = `Q183_Page Submit`,
         FF17_Click_Count = `Q183_Click Count`,
        
         F17i_Page_Submit = `Q184_Page Submit`,
         F17i_Click_Count = `Q184_Click Count`,
         FF17i_Page_Submit = `Q185_Page Submit`,
         FF17i_Click_Count = `Q185_Click Count`,
         
         F18_Page_Submit = `Q186_Page Submit`,
         F18_Click_Count = `Q186_Click Count`,
         FF18_Page_Submit = `Q187_Page Submit`,
         FF18_Click_Count = `Q187_Click Count`,
         
         F19_Page_Submit = `Q188_Page Submit`,
         F19_Click_Count = `Q188_Click Count`,
         FF19_Page_Submit = `Q189_Page Submit`,
         FF19_Click_Count = `Q189_Click Count`,
         
         F20_Page_Submit = `Q190_Page Submit`,
         F20_Click_Count = `Q190_Click Count`,
         FF20_Page_Submit = `Q191_Page Submit`,
         FF20_Click_Count = `Q191_Click Count`,
         
         F21_Page_Submit = `Q192_Page Submit`,
         F21_Click_Count = `Q192_Click Count`,
         FF21_Page_Submit = `Q193_Page Submit`,
         FF21_Click_Count = `Q193_Click Count`,
         
         # Keine F22
         
         F23_Page_Submit = `Q196_Page Submit`,
         F23_Click_Count = `Q196_Click Count`,
         FF23_Page_Submit = `Q197_Page Submit`,
         FF23_Click_Count = `Q197_Click Count`,
         
         F24_Page_Submit = `Q198_Page Submit`,
         F24_Click_Count = `Q198_Click Count`,
         FF24_Page_Submit = `Q199_Page Submit`,
         FF24_Click_Count = `Q199_Click Count`,
         
         F25_Page_Submit = `Q200_Page Submit`,
         F25_Click_Count = `Q200_Click Count`,
         FF25_Page_Submit = `Q201_Page Submit`,
         FF25_Click_Count = `Q201_Click Count`,
         
         F26_Page_Submit = `Q202_Page Submit`,
         F26_Click_Count = `Q202_Click Count`,
         FF26_Page_Submit = `Q203_Page Submit`,
         FF26_Click_Count = `Q203_Click Count`,
         
         F27_Page_Submit = `Q204_Page Submit`,
         F27_Click_Count = `Q204_Click Count`,
         FF27_Page_Submit = `Q205_Page Submit`,
         FF27_Click_Count = `Q205_Click Count`
         )

# 2. Termin Standortbestimmung Variablen umbenennen ----
data_sb_2 <- data_sb_2 %>%
  rename(F17 = F17...244,
         F17i = F17...255,
         "Duration_seconds" = `Duration (in seconds)`,
         Muhe = Muhe1)

data_sb_2 <- data_sb_2 %>% 
  rename(F1_Page_Submit = `Q149_Page Submit`,
         F1_Click_Count = `Q149_Click Count`,
         FF1_Page_Submit = `Q153_Page Submit`,
         FF1_Click_Count = `Q153_Click Count`,
         
         F2_Page_Submit = `Q150_Page Submit`,
         F2_Click_Count = `Q150_Click Count`,
         FF2_Page_Submit = `Q155_Page Submit`,
         FF2_Click_Count = `Q155_Click Count`,
         
         F3_Page_Submit = `Q151_Page Submit`,
         F3_Click_Count = `Q151_Click Count`,
         FF3_Page_Submit = `Q152_Page Submit`,
         FF3_Click_Count = `Q152_Click Count`,
         
         F3i_Page_Submit = `Q156_Page Submit`,
         F3i_Click_Count = `Q156_Click Count`,
         FF3i_Page_Submit = `Q157_Page Submit`,
         FF3i_Click_Count = `Q157_Click Count`,
         
         #F3ii_Page_Submit = -> kein Page Submit
         #F3ii_Click_Count = -> kein Click Count`,
         FF3ii_Page_Submit = `Q158_Page Submit`,
         FF3ii_Click_Count = `Q158_Click Count`,
         
         F4_Page_Submit = `Q161_Page Submit`,
         F4_Click_Count = `Q161_Click Count`,
         FF4_Page_Submit = `Q160_Page Submit`,
         FF4_Click_Count = `Q160_Click Count`,
         
         F5_Page_Submit = `Q162_Page Submit`,
         F5_Click_Count = `Q162_Click Count`,
         FF5_Page_Submit = `Q163_Page Submit`,
         FF5_Click_Count = `Q163_Click Count`,
         
         F6_Page_Submit = `Q209_Page Submit`,
         F6_Click_Count = `Q209_Click Count`,
         FF6_Page_Submit = `Q164_Page Submit`,
         FF6_Click_Count = `Q164_Click Count`,
         
         F7_Page_Submit = `Q211_Page Submit`,
         F7_Click_Count = `Q211_Click Count`,
         FF7_Page_Submit = `Q165_Page Submit`,
         FF7_Click_Count = `Q165_Click Count`,
         
         F8_Page_Submit = `Q167_Page Submit`,
         F8_Click_Count = `Q167_Click Count`,
         FF8_Page_Submit = `Q166_Page Submit`,
         FF8_Click_Count = `Q166_Click Count`,
         
         F9_Page_Submit = `Q168_Page Submit`,
         F9_Click_Count = `Q168_Click Count`,
         FF9_Page_Submit = `Q169_Page Submit`,
         FF9_Click_Count = `Q169_Click Count`,
         
         F10_Page_Submit = `Q170_Page Submit`,
         F10_Click_Count = `Q170_Click Count`,
         FF10_Page_Submit = `Q171_Page Submit`,
         FF10_Click_Count = `Q171_Click Count`,
         
         F11_Page_Submit = `Q172_Page Submit`,
         F11_Click_Count = `Q172_Click Count`,
         FF11_Page_Submit = `Q173_Page Submit`,
         FF11_Click_Count = `Q173_Click Count`,
         
         F12_Page_Submit = `Q174_Page Submit`,
         F12_Click_Count = `Q174_Click Count`,
         FF12_Page_Submit = `Q175_Page Submit`,
         FF12_Click_Count = `Q175_Click Count`,
         
         F13_Page_Submit = `Q176_Page Submit`,
         F13_Click_Count = `Q176_Click Count`,
         FF13_Page_Submit = `Q177_Page Submit`,
         FF13_Click_Count = `Q177_Click Count`,
         
         F14_Page_Submit = `Q178_Page Submit`,
         F14_Click_Count = `Q178_Click Count`,
         FF14_Page_Submit = `Q179_Page Submit`,
         FF14_Click_Count = `Q179_Click Count`,
         
         # Keine F15
         
         F16_Page_Submit = `Q180_Page Submit`,
         F16_Click_Count = `Q180_Click Count`,
         FF16_Page_Submit = `Q181_Page Submit`,
         FF16_Click_Count = `Q181_Click Count`,
         
         F17_Page_Submit = `Q182_Page Submit`,
         F17_Click_Count = `Q182_Click Count`,
         FF17_Page_Submit = `Q183_Page Submit`,
         FF17_Click_Count = `Q183_Click Count`,
         
         F17i_Page_Submit = `Q184_Page Submit`,
         F17i_Click_Count = `Q184_Click Count`,
         FF17i_Page_Submit = `Q185_Page Submit`,
         FF17i_Click_Count = `Q185_Click Count`,
         
         F18_Page_Submit = `Q186_Page Submit`,
         F18_Click_Count = `Q186_Click Count`,
         FF18_Page_Submit = `Q187_Page Submit`,
         FF18_Click_Count = `Q187_Click Count`,
         
         F19_Page_Submit = `Q188_Page Submit`,
         F19_Click_Count = `Q188_Click Count`,
         FF19_Page_Submit = `Q189_Page Submit`,
         FF19_Click_Count = `Q189_Click Count`,
         
         F20_Page_Submit = `Q190_Page Submit`,
         F20_Click_Count = `Q190_Click Count`,
         FF20_Page_Submit = `Q191_Page Submit`,
         FF20_Click_Count = `Q191_Click Count`,
         
         F21_Page_Submit = `Q192_Page Submit`,
         F21_Click_Count = `Q192_Click Count`,
         FF21_Page_Submit = `Q193_Page Submit`,
         FF21_Click_Count = `Q193_Click Count`,
         
         # Keine F22
         
         F23_Page_Submit = `Q196_Page Submit`,
         F23_Click_Count = `Q196_Click Count`,
         FF23_Page_Submit = `Q197_Page Submit`,
         FF23_Click_Count = `Q197_Click Count`,
         
         F24_Page_Submit = `Q198_Page Submit`,
         F24_Click_Count = `Q198_Click Count`,
         FF24_Page_Submit = `Q199_Page Submit`,
         FF24_Click_Count = `Q199_Click Count`,
         
         F25_Page_Submit = `Q200_Page Submit`,
         F25_Click_Count = `Q200_Click Count`,
         FF25_Page_Submit = `Q201_Page Submit`,
         FF25_Click_Count = `Q201_Click Count`,
         
         F26_Page_Submit = `Q202_Page Submit`,
         F26_Click_Count = `Q202_Click Count`,
         FF26_Page_Submit = `Q203_Page Submit`,
         FF26_Click_Count = `Q203_Click Count`,
         
         F27_Page_Submit = `Q204_Page Submit`,
         F27_Click_Count = `Q204_Click Count`,
         FF27_Page_Submit = `Q205_Page Submit`,
         FF27_Click_Count = `Q205_Click Count`
         )

# 1. Termin IRT12 Variablen umbenennen ----
data_irt12_1 <- data_irt12_1 %>%
  rename(`Q40_Page Submit` = `Q40_Page Submit...72`,
         `Q40_Click Count` = `Q40_Click Count...73`,
         `Q40i_Page Submit` = `Q40_Page Submit...105`,
         `Q40i_Click Count` = `Q40_Click Count...106`,
         "Duration_seconds" = `Duration (in seconds)`,
         Vorlesung = A2,
         Pflichtlit = A1,
         Muhe = Muhe2,
         verstanden_1 = A3_1,
         verstanden_2 = A3_2,
         verstanden_3 = A3_3,
         verstanden_4 = A3_4,
         verstanden_5 = A3_5,
         verstanden_6 = A3_6)

data_irt12_1 <- data_irt12_1 %>%
  rename(F1_Page_Submit = `Q38_Page Submit`,
         F1_Click_Count = `Q38_Click Count`,
         FF1_Page_Submit = `Q21_Page Submit`,
         FF1_Click_Count = `Q21_Click Count`,
         
         F2_Page_Submit = `Q39_Page Submit`,
         F2_Click_Count = `Q39_Click Count`,
         FF2_Page_Submit = `Q22_Page Submit`,
         FF2_Click_Count = `Q22_Click Count`,
         
         F3_Page_Submit = `Q84_Page Submit`,
         F3_Click_Count = `Q84_Click Count`,
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
         
         F6_Page_Submit = `Q40i_Page Submit`,
         F6_Click_Count = `Q40i_Click Count`,
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
         FF14_Click_Count = `Q68_Click Count`,
  )

# verstanden Variable generieren
data_irt12_1 <- data_irt12_1 %>% 
  mutate(verstanden = rowMeans(select(., verstanden_1, verstanden_2, verstanden_3,verstanden_4, verstanden_5, verstanden_6),na.rm = TRUE))

# 2. Termin IRT12 Variablen umbenennen ----
data_irt12_2 <- data_irt12_2 %>%
  rename("Duration_seconds" = `Duration (in seconds)`,
         Vorlesung = Boadcast,
         Pflichtlit = Pflicht,
         Muhe = Muhe2)

data_irt12_2 <- data_irt12_2 %>%
  rename(F1_Page_Submit = `Q108_Page Submit`,
         F1_Click_Count = `Q108_Click Count`,
         FF1_Page_Submit = `Q109_Page Submit`,
         FF1_Click_Count = `Q109_Click Count`,
         
         #F2_Page_Submit = -> kein Page Submit
         #F2_Click_Count = -> kein Click Count
         FF2_Page_Submit = `Q112_Page Submit`,
         FF2_Click_Count = `Q112_Click Count`,
         
         F3_Page_Submit = `Q39_Page Submit`,
         F3_Click_Count = `Q39_Click Count`,
         FF3_Page_Submit = `Q22_Page Submit`,
         FF3_Click_Count = `Q22_Click Count`,
         
         F4_Page_Submit = `Q70_Page Submit`,
         F4_Click_Count = `Q70_Click Count`,
         FF4_Page_Submit = `Q71_Page Submit`,
         FF4_Click_Count = `Q71_Click Count`,
         
         F5_Page_Submit = `Q72_Page Submit`,
         F5_Click_Count = `Q72_Click Count`,
         FF5_Page_Submit = `Q30_Page Submit`,
         FF5_Click_Count = `Q30_Click Count`,
         
         F6_Page_Submit = `Q40_Page Submit`,
         F6_Click_Count = `Q40_Click Count`,
         FF6_Page_Submit = `Q36_Page Submit`,
         FF6_Click_Count = `Q36_Click Count`,
         
         F7_Page_Submit = `Q37_Page Submit`,
         F7_Click_Count = `Q37_Click Count`,
         FF7_Page_Submit = `Q41_Page Submit`,
         FF7_Click_Count = `Q41_Click Count`,
         
         F8_Page_Submit = `Q103_Page Submit`,
         F8_Click_Count = `Q103_Click Count`,
         FF8_Page_Submit = `Q81_Page Submit`,
         FF8_Click_Count = `Q81_Click Count`,
         
         F9_Page_Submit = `Q105_Page Submit`,
         F9_Click_Count = `Q105_Click Count`,
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
         
         # Keine F13
         
         F14_Page_Submit = `Q66_Page Submit`,
         F14_Click_Count = `Q66_Click Count`,
         FF14_Page_Submit = `Q68_Page Submit`,
         FF14_Click_Count = `Q68_Click Count`,
  )

# verstanden Variable generieren
data_irt12_2 <- data_irt12_2 %>% 
  mutate(verstanden = rowMeans(select(., verstanden_1, verstanden_2, verstanden_3,
                                      verstanden_4, verstanden_5, verstanden_6),
                               na.rm = TRUE))

# 1. Termin IRT34 Variablen umbenennen ----
data_irt34_1 <- data_irt34_1 %>%
  rename(oft = Q4,
         "Duration_seconds" = `Duration (in seconds)`,
         Vorlesung = Q7,
         Pflichtlit = Q3,
         Muhe = Muhe3,
         verstanden_1 = Q9_1,
         verstanden_2 = Q9_2,
         verstanden_3 = Q9_3,
         verstanden_4 = Q9_4,
         verstanden_5 = Q9_5,
         verstanden_6 = Q9_6)

data_irt34_1 <- data_irt34_1 %>%
  rename(F1_Page_Submit = `Q55_Page Submit`,
         F1_Click_Count = `Q55_Click Count`,
         FF1_Page_Submit = `Q57_Page Submit`,
         FF1_Click_Count = `Q57_Click Count`,
         
         F2_Page_Submit = `Q58_Page Submit`,
         F2_Click_Count = `Q58_Click Count`,
         FF2_Page_Submit = `Q59_Page Submit`,
         FF2_Click_Count = `Q59_Click Count`,
         
         F3_Page_Submit = `Q60_Page Submit`,
         F3_Click_Count = `Q60_Click Count`,
         FF3_Page_Submit = `Q61_Page Submit`,
         FF3_Click_Count = `Q61_Click Count`,
         
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
         FF12_Click_Count = `Q83_Click Count`
  )

# verstanden Variable generieren
data_irt34_1 <- data_irt34_1 %>% 
  mutate(verstanden = rowMeans(select(., verstanden_1, verstanden_2, verstanden_3,
                                      verstanden_4, verstanden_5, verstanden_6),
                               na.rm = TRUE))

# 2. Termin IRT34 Variablen umbenennen ----
data_irt34_2 <- data_irt34_2 %>%
  rename(F9b_1 = `F9b _1`,
         F9b_2 = `F9b _2`,
         F9b_3 = `F9b _3`,
         "Duration_seconds" = `Duration (in seconds)`,
         Vorlesung = Q7,
         Pflichtlit = Q3,
         Muhe = Muhe3,
         verstanden_1 = Q9_1,
         verstanden_2 = Q9_2,
         verstanden_3 = Q9_3,
         verstanden_4 = Q9_4,
         verstanden_5 = Q9_5,
         verstanden_6 = Q9_6)

data_irt34_2 <- data_irt34_2 %>%
  rename(F1_Page_Submit = `Q55_Page Submit`,
         F1_Click_Count = `Q55_Click Count`,
         FF1_Page_Submit = `Q57_Page Submit`,
         FF1_Click_Count = `Q57_Click Count`,
         
         F2_Page_Submit = `Q58_Page Submit`,
         F2_Click_Count = `Q58_Click Count`,
         FF2_Page_Submit = `Q59_Page Submit`,
         FF2_Click_Count = `Q59_Click Count`,
         
         F3_Page_Submit = `Q88_Page Submit`,
         F3_Click_Count = `Q88_Click Count`,
         FF3_Page_Submit = `Q91_Page Submit`,
         FF3_Click_Count = `Q91_Click Count`,
         
         F4_Page_Submit = `Q60_Page Submit`,
         F4_Click_Count = `Q60_Click Count`,
         FF4_Page_Submit = `Q61_Page Submit`,
         FF4_Click_Count = `Q61_Click Count`,
         
         F5_Page_Submit = `Q63_Page Submit`,
         F5_Click_Count = `Q63_Click Count`,
         FF5_Page_Submit = `Q64_Page Submit`,
         FF5_Click_Count = `Q64_Click Count`,
         
         F6_Page_Submit = `Q65_Page Submit`,
         F6_Click_Count = `Q65_Click Count`,
         FF6_Page_Submit = `Q67_Page Submit`,
         FF6_Click_Count = `Q67_Click Count`,
         
         F7_Page_Submit = `Q68_Page Submit`,
         F7_Click_Count = `Q68_Click Count`,
         FF7_Page_Submit = `Q69_Page Submit`,
         FF7_Click_Count = `Q69_Click Count`,
         
         F8_Page_Submit = `Q70_Page Submit`,
         F8_Click_Count = `Q70_Click Count`,
         FF8_Page_Submit = `Q71_Page Submit`,
         FF8_Click_Count = `Q71_Click Count`,
         
         F9_Page_Submit = `Q72_Page Submit`,
         F9_Click_Count = `Q72_Click Count`,
         FF9_Page_Submit = `Q75_Page Submit`,
         FF9_Click_Count = `Q75_Click Count`,
         
         F10_Page_Submit = `Q76_Page Submit`,
         F10_Click_Count = `Q76_Click Count`,
         FF10_Page_Submit = `Q77_Page Submit`,
         FF10_Click_Count = `Q77_Click Count`,
         
         F11_Page_Submit = `Q78_Page Submit`,
         F11_Click_Count = `Q78_Click Count`,
         FF11_Page_Submit = `Q79_Page Submit`,
         FF11_Click_Count = `Q79_Click Count`,
         
         F12_Page_Submit = `Q80_Page Submit`,
         F12_Click_Count = `Q80_Click Count`,
         FF12_Page_Submit = `Q81_Page Submit`,
         FF12_Click_Count = `Q81_Click Count`,
         
         F13_Page_Submit = `Q82_Page Submit`,
         F13_Click_Count = `Q82_Click Count`,
         FF13_Page_Submit = `Q83_Page Submit`,
         FF13_Click_Count = `Q83_Click Count`
  )

data_irt34_2 <- data_irt34_2 %>% 
  mutate(verstanden = rowMeans(select(., verstanden_1, verstanden_2, verstanden_3,
                                      verstanden_4, verstanden_5, verstanden_6),
                               na.rm = TRUE))

# 1. Termin CFA12 Variablen umbenennen ----
data_cfa12_1 <- data_cfa12_1 %>%
  rename(oft = oft4,
         "Duration_seconds" = `Duration (in seconds)`,
         Vorlesung = vl4,
         Pflichtlit = pflichtlit4,
         Muhe = Muhe4,
         verstanden_1 = `6_1`,
         verstanden_2 = `6_2`,
         verstanden_3 = `6_3`,
         verstanden_4 = `6_4`)

data_cfa12_1 <- data_cfa12_1 %>%
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
         
         F17_Page_Submit = `Q85_Page Submit`,
         F17_Click_Count = `Q85_Click Count`,
         FF17_Page_Submit = `Q88_Page Submit`,
         FF17_Click_Count = `Q88_Click Count`,
  )

data_cfa12_1 <- data_cfa12_1 %>% 
  mutate(verstanden = rowMeans(select(., verstanden_1, verstanden_2, verstanden_3,
                                      verstanden_4),
                               na.rm = TRUE))

# 2. Termin CFA12 Variablen umbenennen ----
data_cfa12_2 <- data_cfa12_2 %>%
  rename(oft = oft4,
         "Duration_seconds" = `Duration (in seconds)`,
         Vorlesung = vl4,
         Pflichtlit = pflichtlit4,
         Muhe = Muhe4,
         verstanden_1 = Q6_1,
         verstanden_2 = Q6_2,
         verstanden_3 = Q6_3,
         verstanden_4 = Q6_4)

data_cfa12_2 <- data_cfa12_2 %>%
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
         
         F5_Page_Submit = `Q132_Page Submit`,
         F5_Click_Count = `Q132_Click Count`,
         FF5_Page_Submit = `Q134_Page Submit`,
         FF5_Click_Count = `Q134_Click Count`,
         
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
         FF16_Click_Count = `Q83_Click Count`
  )

data_cfa12_2 <- data_cfa12_2 %>% 
  mutate(verstanden = rowMeans(select(., verstanden_1, verstanden_2, verstanden_3,
                                      verstanden_4),
                               na.rm = TRUE))

# 1. Termin CFA34 Variablen umbenennen ----
data_cfa34_1 <- data_cfa34_1 %>%
  rename(oft = oft5,
         "Duration_seconds" = `Duration (in seconds)`,
         Vorlesung = vl5,
         Pflichtlit = pflichtlit5,
         Muhe = Muhe5,
         verstanden_1 = Intro6_1,
         verstanden_2 = Intro6_2,
         verstanden_3 = Intro6_3,
         verstanden_4 = Intro6_4)

data_cfa34_1 <- data_cfa34_1 %>%
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
         
         #F8_Page_Submit = -> Kein Page Submit
         #F8_Click_Count = -> Kein Click Count
         #FF8_Page_Submit = -> Kein Page Submit
         #FF8_Click_Count = -> Kein Click Count`
         
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
         FF15_Click_Count = `Q80_Click Count`,
         )

data_cfa34_1 <- data_cfa34_1 %>% 
  mutate(verstanden = rowMeans(select(., verstanden_1, verstanden_2, verstanden_3,
                                      verstanden_4),
                               na.rm = TRUE))

# 2. Termin CFA34 Variablen umbenennen ----
data_cfa34_2 <- data_cfa34_2 %>%
  rename(oft = oft5,
         "Duration_seconds" = `Duration (in seconds)`,
         Vorlesung = vl5,
         Pflichtlit = pflichtlit5,
         Muhe = Muhe5,
         verstanden_1 = Intro6_1,
         verstanden_2 = Intro6_2,
         verstanden_3 = Intro6_3,
         verstanden_4 = Intro6_4)

data_cfa34_2 <- data_cfa34_2 %>%
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
         
         F8_Page_Submit = `Q134_Page Submit`,
         F8_Click_Count = `Q134_Click Count`,
         FF8_Page_Submit = `Q135_Page Submit`,
         FF8_Click_Count = `Q135_Click Count`,
         
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
         FF15_Click_Count = `Q80_Click Count`,
  )

data_cfa34_2 <- data_cfa34_2 %>% 
  mutate(verstanden = rowMeans(select(., verstanden_1, verstanden_2, verstanden_3,
                                      verstanden_4),
                               na.rm = TRUE))

# 1. Termin Krit. orientiert Variablen umbenennen ----
data_Krit_1 <- data_Krit_1 %>%
  rename(oft = Intro4,
         "Duration_seconds" = `Duration (in seconds)`,
         Vorlesung = Intro5,
         Pflichtlit = Intro3,
         Muhe = Muhe6,
         verstanden_1 = Intro6_1,
         verstanden_2 = Intro6_2,
         verstanden_3 = Intro6_3)

data_Krit_1 <- data_Krit_1 %>%
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
         
         #F8_Page_Submit = -> kein Page Submit
         #F8_Click_Count = -> kein Click Count
         #FF8_Page_Submit = -> kein Page Submit
         #FF8_Click_Count = -> kein Click Count
         
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
         #FF11_Page_Submit = -> kein Page Submit
         #FF11_Click_Count = -> kein Click Count
  )

data_Krit_1 <- data_Krit_1 %>% 
  mutate(verstanden = rowMeans(select(., verstanden_1, verstanden_2, verstanden_3),
                               na.rm = TRUE))

# 2. Termin Krit. orientiert Variablen umbenennen ----
data_Krit_2 <- data_Krit_2 %>% 
  rename(oft = Intro4,
         F4a = Q129,
         F9a = F9,
         F9_1 = F9.1_1,
         F9_2 = F9.1_2,
         F11a = F11,
         F11_1 = F11.1_1,
         F11_2 = F11.1_2,
         "Duration_seconds" = `Duration (in seconds)`,
         Vorlesung = Intro5,
         Pflichtlit = Intro3,
         Muhe = Muhe6,
         verstanden_1 = Intro6_1,
         verstanden_2 = Intro6_2,
         verstanden_3 = Intro6_3)

data_Krit_2 <- data_Krit_2 %>%
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
         
         #F8_Page_Submit = -> kein Page Submit
         #F8_Click_Count = -> kein Click Count
         #FF8_Page_Submit = -> kein Page Submit
         #FF8_Click_Count = -> kein Click Count
         
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
         #FF11_Page_Submit = -> kein Page Submit
         #FF11_Click_Count = -> kein Click Count
  )

data_Krit_2 <- data_Krit_2 %>% 
  mutate(verstanden = rowMeans(select(., verstanden_1, verstanden_2, verstanden_3),
                               na.rm = TRUE))


# 1. Termin Standortbestimmung Scoring in R/F und Summenscore ----
data_sb_1$Vorlesung <- NA #leere platzhalter für die Variablen
data_sb_1$Pflichtlit <- NA
data_sb_1$verstanden <- NA
# Variablen in R/F codieren & Summenscore bei k Prim
# F1 (2 ist korrekt) dichotomisieren
data_sb_1$F1 <- ifelse(data_sb_1$F1 == 2, 1, 0)

# F2 k Prim Score
data_sb_1$F2_2 <- ifelse(data_sb_1$F2_2 == 4, 0.25, 0)
data_sb_1$F2_3 <- ifelse(data_sb_1$F2_3 == 3, 0.25, 0)
data_sb_1$F2_4 <- ifelse(data_sb_1$F2_4 == 2, 0.25, 0)
data_sb_1$F2_5 <- ifelse(data_sb_1$F2_5 == 1, 0.25, 0)
# F2 Summenscore
data_sb_1$F2 <- rowSums(select(data_sb_1, F2_2:F2_5))
data_sb_1 <- relocate(data_sb_1, F2, .after = F2_5)

# F3 k Prim Score
data_sb_1$F3_1 <- ifelse(data_sb_1$F3_1 == 1, 0.25, 0)
data_sb_1$F3_2 <- ifelse(data_sb_1$F3_2 == 2, 0.25, 0)
data_sb_1$F3_3 <- ifelse(data_sb_1$F3_3 == 1, 0.25, 0)
data_sb_1$F3_4 <- ifelse(data_sb_1$F3_4 == 1, 0.25, 0)
# F3 Summenscore
data_sb_1$F3 <- rowSums(select(data_sb_1, F3_1:F3_4))
data_sb_1 <- relocate(data_sb_1, F3, .after = F3_4)

# F3i k Prim Score
data_sb_1$F3i_1 <- ifelse(data_sb_1$F3i_1 == 2, 0.25, 0)
data_sb_1$F3i_2 <- ifelse(data_sb_1$F3i_2 == 1, 0.25, 0)
data_sb_1$F3i_3 <- ifelse(data_sb_1$F3i_3 == 1, 0.25, 0)
data_sb_1$F3i_4 <- ifelse(data_sb_1$F3i_4 == 2, 0.25, 0)
# F3i Summenscore
data_sb_1$F3i <- rowSums(select(data_sb_1, F3i_1:F3i_4))
data_sb_1 <- relocate(data_sb_1, F3i, .after = F3i_4)

# F3ii k Prim Score
data_sb_1$F3ii_1 <- ifelse(data_sb_1$F3ii_1 == 1, 0.25, 0)
data_sb_1$F3ii_2 <- ifelse(data_sb_1$F3ii_2 == 1, 0.25, 0)
data_sb_1$F3ii_3 <- ifelse(data_sb_1$F3ii_3 == 1, 0.25, 0)
data_sb_1$F3ii_4 <- ifelse(data_sb_1$F3ii_4 == 1, 0.25, 0)
# F3ii Summenscore
data_sb_1$F3ii <- rowSums(select(data_sb_1, F3ii_1:F3ii_4))
data_sb_1 <- relocate(data_sb_1, F3ii, .after = F3ii_4)

# F4 k Prim Score
data_sb_1$F4_1 <- ifelse(data_sb_1$F4_1 == 1, 0.25, 0)
data_sb_1$F4_2 <- ifelse(data_sb_1$F4_2 == 2, 0.25, 0)
data_sb_1$F4_3 <- ifelse(data_sb_1$F4_3 == 1, 0.25, 0)
data_sb_1$F4_4 <- ifelse(data_sb_1$F4_4 == 1, 0.25, 0)
# F4 Summenscore
data_sb_1$F4 <- rowSums(select(data_sb_1, F4_1:F4_4))
data_sb_1 <- relocate(data_sb_1, F4, .after = F4_4)

# F5 k Prim Score
data_sb_1$F5_1 <- ifelse(data_sb_1$F5_1 == 1, 0.25, 0)
data_sb_1$F5_2 <- ifelse(data_sb_1$F5_2 == 2, 0.25, 0)
data_sb_1$F5_3 <- ifelse(data_sb_1$F5_3 == 1, 0.25, 0)
data_sb_1$F5_4 <- ifelse(data_sb_1$F5_4 == 2, 0.25, 0)
# F5 Summenscore
data_sb_1$F5 <- rowSums(select(data_sb_1, F5_1:F5_4))
data_sb_1 <- relocate(data_sb_1, F5, .after = F5_4)

# F6 dichotomisieren
data_sb_1$F6 <- ifelse(data_sb_1$F6 == 2, 1, 0)

# F7 dichotomisieren
data_sb_1$F7 <- ifelse(data_sb_1$F7 == 4, 1, 0)

# F8 k Prim Score
data_sb_1$F8_1 <- ifelse(data_sb_1$F8_1 == 2, 0.25, 0)
data_sb_1$F8_2 <- ifelse(data_sb_1$F8_2 == 1, 0.25, 0)
data_sb_1$F8_3 <- ifelse(data_sb_1$F8_3 == 1, 0.25, 0)
data_sb_1$F8_4 <- ifelse(data_sb_1$F8_4 == 1, 0.25, 0)
# F8 Summenscore
data_sb_1$F8 <- rowSums(select(data_sb_1, F8_1:F8_4))
data_sb_1 <- relocate(data_sb_1, F8, .after = F8_4)

# F9 k Prim Score
data_sb_1$F9_1 <- ifelse(data_sb_1$F9_1 == 1, 0.2, 0)
data_sb_1$F9_2 <- ifelse(data_sb_1$F9_2 == 1, 0.2, 0)
data_sb_1$F9_3 <- ifelse(data_sb_1$F9_3 == 2, 0.2, 0)
data_sb_1$F9_4 <- ifelse(data_sb_1$F9_4 == 2, 0.2, 0)
data_sb_1$F9_5 <- ifelse(data_sb_1$F9_5 == 2, 0.2, 0)
# F9 Summenscore
data_sb_1$F9 <- rowSums(select(data_sb_1, F9_1:F9_5))
data_sb_1 <- relocate(data_sb_1, F9, .after = F9_5)

# F10 k Prim Score
data_sb_1$F10_1 <- ifelse(data_sb_1$F10_1 == 1, 0.25, 0)
data_sb_1$F10_2 <- ifelse(data_sb_1$F10_2 == 2, 0.25, 0)
data_sb_1$F10_3 <- ifelse(data_sb_1$F10_3 == 1, 0.25, 0)
data_sb_1$F10_4 <- ifelse(data_sb_1$F10_4 == 2, 0.25, 0)
# F10 Summenscore
data_sb_1$F10 <- rowSums(select(data_sb_1, F10_1:F10_4))
data_sb_1 <- relocate(data_sb_1, F10, .after = F10_4)

# F11 k Prim Score
data_sb_1$F11_1 <- ifelse(data_sb_1$F11_1 == 1, (1/3), 0)
data_sb_1$F11_2 <- ifelse(data_sb_1$F11_2 == 1, (1/3), 0)
data_sb_1$F11_3 <- ifelse(data_sb_1$F11_3 == 2, (1/3), 0)
# F11 Summenscore
data_sb_1$F11 <- rowSums(select(data_sb_1, F11_1:F11_3))
data_sb_1 <- relocate(data_sb_1, F11, .after = F11_3)

# F12 dichotomisieren
data_sb_1$F12 <- ifelse(data_sb_1$F12 == 6, 1, 0)

# F13 dichotomisieren
data_sb_1$F13 <- ifelse(data_sb_1$F13 == 3, 1, 0)


# F14 dichotomisieren
data_sb_1$F14 <- ifelse(data_sb_1$F14 == 6, 1, 0)

# Keine F15

# F16 k Prim Score
data_sb_1$F16_1 <- ifelse(data_sb_1$F16_1 == 2, 0.25, 0)
data_sb_1$F16_2 <- ifelse(data_sb_1$F16_2 == 1, 0.25, 0)
data_sb_1$F16_3 <- ifelse(data_sb_1$F16_3 == 1, 0.25, 0)
data_sb_1$F16_4 <- ifelse(data_sb_1$F16_4 == 2, 0.25, 0)
# F16 Summenscore
data_sb_1$F16 <- rowSums(select(data_sb_1, F16_1:F16_4))
data_sb_1 <- relocate(data_sb_1, F16, .after = F16_4)

# F17 dichotomisieren
data_sb_1$F17 <- ifelse(data_sb_1$F17 == 3, 1, 0)

# F17_1dichotomisieren
data_sb_1$F17i <- ifelse(data_sb_1$F17i == 2, 1, 0)

# F18 dichotomisieren
data_sb_1$F18 <- ifelse(data_sb_1$F18 == 3, 1, 0)

# F19 dichotomisieren
data_sb_1$F19 <- ifelse(data_sb_1$F19 == 1, 1, 0)

# F20 dichotomisieren
data_sb_1$F20 <- ifelse(data_sb_1$F20 == 7, 1, 0)

# F21 dichotomisieren
data_sb_1$F21 <- ifelse(data_sb_1$F21 == 2, 1, 0)

# Keine F22

# F23 dichotomisieren
data_sb_1$F23 <- ifelse(data_sb_1$F23 == 3, 1, 0)

# F24 dichotomisieren
data_sb_1$F24 <- ifelse(data_sb_1$F24 == 2, 1, 0)

# F25 dichotomisieren
data_sb_1$F25 <- ifelse(data_sb_1$F25 == 1, 1, 0)

# F26 dichotomisieren
data_sb_1$F26 <- ifelse(data_sb_1$F26 == 2, 1, 0)

# F27 dichotomisieren
data_sb_1$F27 <- ifelse(data_sb_1$F27 == 1, 1, 0)


# 2. Termin Standortbestimmung Scoring in R/F und Summenscore ----
data_sb_2$Vorlesung <- NA #leere platzhalter für die Variablen
data_sb_2$Pflichtlit <- NA
data_sb_2$verstanden <- NA
# Variablen in R/F codieren & Summenscore bei k Prim
# F1 dichotomisieren
data_sb_2$F1 <- ifelse(data_sb_2$F1 == 2, 1, 0)

# F2 k Prim Score
data_sb_2$F2_2 <- ifelse(data_sb_2$F2_2 == 3, 0.25, 0)
data_sb_2$F2_3 <- ifelse(data_sb_2$F2_3 == 4, 0.25, 0)
data_sb_2$F2_4 <- ifelse(data_sb_2$F2_4 == 2, 0.25, 0)
data_sb_2$F2_5 <- ifelse(data_sb_2$F2_5 == 1, 0.25, 0)
# F2 Summenscore
data_sb_2$F2 <- rowSums(select(data_sb_2, F2_2:F2_5))
data_sb_2 <- relocate(data_sb_2, F2, .after = F2_5)

# F3 k Prim Score
data_sb_2$F3_1 <- ifelse(data_sb_2$F3_1 == 1, 0.25, 0)
data_sb_2$F3_2 <- ifelse(data_sb_2$F3_2 == 2, 0.25, 0)
data_sb_2$F3_3 <- ifelse(data_sb_2$F3_3 == 1, 0.25, 0)
data_sb_2$F3_4 <- ifelse(data_sb_2$F3_4 == 1, 0.25, 0)
# F3 Summenscore
data_sb_2$F3 <- rowSums(select(data_sb_2, F3_1:F3_4))
data_sb_2 <- relocate(data_sb_2, F3, .after = F3_4)

# F3i k Prim Score
data_sb_2$F3i_1 <- ifelse(data_sb_2$F3i_1 == 2, 0.25, 0)
data_sb_2$F3i_2 <- ifelse(data_sb_2$F3i_2 == 2, 0.25, 0)
data_sb_2$F3i_3 <- ifelse(data_sb_2$F3i_3 == 1, 0.25, 0)
data_sb_2$F3i_4 <- ifelse(data_sb_2$F3i_4 == 1, 0.25, 0)
# F3i Summenscore
data_sb_2$F3i <- rowSums(select(data_sb_2, F3i_1:F3i_4))
data_sb_2 <- relocate(data_sb_2, F3i, .after = F3i_4)

# F3ii k Prim Score
data_sb_2$F3ii_1 <- ifelse(data_sb_2$F3ii_1 == 2, 0.25, 0)
data_sb_2$F3ii_2 <- ifelse(data_sb_2$F3ii_2 == 2, 0.25, 0)
data_sb_2$F3ii_3 <- ifelse(data_sb_2$F3ii_3 == 1, 0.25, 0)
data_sb_2$F3ii_4 <- ifelse(data_sb_2$F3ii_4 == 1, 0.25, 0)
# F3ii Summenscore
data_sb_2$F3ii <- rowSums(select(data_sb_2, F3ii_1:F3ii_4))
data_sb_2 <- relocate(data_sb_2, F3ii, .after = F3ii_4)

# F4 k Prim Score
data_sb_2$F4_1 <- ifelse(data_sb_2$F4_1 == 2, 0.25, 0)
data_sb_2$F4_2 <- ifelse(data_sb_2$F4_2 == 1, 0.25, 0)
data_sb_2$F4_3 <- ifelse(data_sb_2$F4_3 == 1, 0.25, 0)
data_sb_2$F4_4 <- ifelse(data_sb_2$F4_4 == 2, 0.25, 0)
# F4 Summenscore
data_sb_2$F4 <- rowSums(select(data_sb_2, F4_1:F4_4))
data_sb_2 <- relocate(data_sb_2, F4, .after = F4_4)

# F5 k Prim Score
data_sb_2$F5_1 <- ifelse(data_sb_2$F5_1 == 2, 0.25, 0)
data_sb_2$F5_2 <- ifelse(data_sb_2$F5_2 == 2, 0.25, 0)
data_sb_2$F5_3 <- ifelse(data_sb_2$F5_3 == 2, 0.25, 0)
data_sb_2$F5_4 <- ifelse(data_sb_2$F5_4 == 1, 0.25, 0)
# F5 Summenscore
data_sb_2$F5 <- rowSums(select(data_sb_2, F5_1:F5_4))
data_sb_2 <- relocate(data_sb_2, F5, .after = F5_4)

# F6 k Prim Score
data_sb_2$F6_1 <- ifelse(data_sb_2$F6_1 == 1, 0.25, 0)
data_sb_2$F6_2 <- ifelse(data_sb_2$F6_2 == 2, 0.25, 0)
data_sb_2$F6_3 <- ifelse(data_sb_2$F6_3 == 2, 0.25, 0)
data_sb_2$F6_4 <- ifelse(data_sb_2$F6_4 == 2, 0.25, 0)
# F5 Summenscore
data_sb_2$F6 <- rowSums(select(data_sb_2, F6_1:F6_4))
data_sb_2 <- relocate(data_sb_2, F6, .after = F6_4)

# F7 dichotomisieren
data_sb_2$F7 <- ifelse(data_sb_2$F7 == 4, 1, 0)

# F8 k Prim Score
data_sb_2$F8_1 <- ifelse(data_sb_2$F8_1 == 1, 0.25, 0)
data_sb_2$F8_2 <- ifelse(data_sb_2$F8_2 == 2, 0.25, 0)
data_sb_2$F8_3 <- ifelse(data_sb_2$F8_3 == 1, 0.25, 0)
data_sb_2$F8_4 <- ifelse(data_sb_2$F8_4 == 2, 0.25, 0)
# F8 Summenscore
data_sb_2$F8 <- rowSums(select(data_sb_2, F8_1:F8_4))
data_sb_2 <- relocate(data_sb_2, F8, .after = F8_4)

# F9 k Prim Score
data_sb_2$F9_1 <- ifelse(data_sb_2$F9_1 == 1, 0.2, 0)
data_sb_2$F9_2 <- ifelse(data_sb_2$F9_2 == 2, 0.2, 0)
data_sb_2$F9_3 <- ifelse(data_sb_2$F9_3 == 1, 0.2, 0)
data_sb_2$F9_4 <- ifelse(data_sb_2$F9_4 == 1, 0.2, 0)
data_sb_2$F9_5 <- ifelse(data_sb_2$F9_5 == 2, 0.2, 0)
# F9 Summenscore
data_sb_2$F9 <- rowSums(select(data_sb_2, F9_1:F9_5))
data_sb_2 <- relocate(data_sb_2, F9, .after = F9_5)

# F10 k Prim Score
data_sb_2$F10_1 <- ifelse(data_sb_2$F10_1 == 1, 0.25, 0)
data_sb_2$F10_2 <- ifelse(data_sb_2$F10_2 == 2, 0.25, 0)
data_sb_2$F10_3 <- ifelse(data_sb_2$F10_3 == 1, 0.25, 0)
data_sb_2$F10_4 <- ifelse(data_sb_2$F10_4 == 2, 0.25, 0)
# F10 Summenscore
data_sb_2$F10 <- rowSums(select(data_sb_2, F10_1:F10_4))
data_sb_2 <- relocate(data_sb_2, F10, .after = F10_4)

# F11 dichotomisieren
data_sb_2$F11 <- ifelse(data_sb_2$F11 == 5, 1, 0)

# F12 dichotomisieren
data_sb_2$F12 <- ifelse(data_sb_2$F12 == 4, 1, 0)

# F13 dichotomisieren
data_sb_2$F13 <- ifelse(data_sb_2$F13 == 3, 1, 0)

# F14 dichotomisieren
data_sb_2$F14 <- ifelse(data_sb_2$F14 == 6, 1, 0)


# Keine F15


# F16 k Prim Score
data_sb_2$F16_1 <- ifelse(data_sb_2$F16_1 == 2, 0.25, 0)
data_sb_2$F16_2 <- ifelse(data_sb_2$F16_2 == 2, 0.25, 0)
data_sb_2$F16_3 <- ifelse(data_sb_2$F16_3 == 2, 0.25, 0)
data_sb_2$F16_4 <- ifelse(data_sb_2$F16_4 == 1, 0.25, 0)
# F16 Summenscore
data_sb_2$F16 <- rowSums(select(data_sb_2, F16_1:F16_4))
data_sb_2 <- relocate(data_sb_2, F16, .after = F16_4)

# F17 dichotomisieren
data_sb_2$F17 <- ifelse(data_sb_2$F17 == 3, 1, 0)

# F17i dichotomisieren
data_sb_2$F17i <- ifelse(data_sb_2$F17i == 3, 1, 0)

# F18 dichotomisieren
data_sb_2$F18 <- ifelse(data_sb_2$F18 == 3, 1, 0)

# F19 dichotomisieren
data_sb_2$F19 <- ifelse(data_sb_2$F19 == 2, 1, 0)

# F20 k Prim Score
data_sb_2$F20_1 <- ifelse(data_sb_2$F20_1 == 2, 0.25, 0)
data_sb_2$F20_2 <- ifelse(data_sb_2$F20_2 == 2, 0.25, 0)
data_sb_2$F20_3 <- ifelse(data_sb_2$F20_3 == 1, 0.25, 0)
data_sb_2$F20_4 <- ifelse(data_sb_2$F20_4 == 1, 0.25, 0)
# F20 Summenscore
data_sb_2$F20 <- rowSums(select(data_sb_2, F20_1:F20_4))
data_sb_2 <- relocate(data_sb_2, F20, .after = F20_4)

# F21 dichotomisieren
data_sb_2$F21 <- ifelse(data_sb_2$F21 == 1, 1, 0)

# Keine F22

# F23 dichotomisieren
data_sb_2$F23 <- ifelse(data_sb_2$F23 == 1, 1, 0)

# F24 k Prim Score
data_sb_2$F24_1 <- ifelse(data_sb_2$F24_1 == 2, 0.25, 0)
data_sb_2$F24_2 <- ifelse(data_sb_2$F24_2 == 2, 0.25, 0)
data_sb_2$F24_3 <- ifelse(data_sb_2$F24_3 == 2, 0.25, 0)
data_sb_2$F24_4 <- ifelse(data_sb_2$F24_4 == 1, 0.25, 0)
# F24 Summenscore
data_sb_2$F24 <- rowSums(select(data_sb_2, F24_1:F24_4))
data_sb_2 <- relocate(data_sb_2, F24, .after = F24_4)

# F25 k Prim Score
data_sb_2$F25_1 <- ifelse(data_sb_2$F25_1 == 2, 0.25, 0)
data_sb_2$F25_2 <- ifelse(data_sb_2$F25_2 == 2, 0.25, 0)
data_sb_2$F25_3 <- ifelse(data_sb_2$F25_3 == 2, 0.25, 0)
data_sb_2$F25_4 <- ifelse(data_sb_2$F25_4 == 1, 0.25, 0)
# F25 Summenscore
data_sb_2$F25 <- rowSums(select(data_sb_2, F25_1:F25_4))
data_sb_2 <- relocate(data_sb_2, F25, .after = F25_4)

# F26 dichotomisieren
data_sb_2$F26 <- ifelse(data_sb_2$F26 == 1, 1, 0)

# F27 dichotomisieren
data_sb_2$F27 <- ifelse(data_sb_2$F27 == 2, 1, 0)


# 1. Termin IRT12 Scoring in R/F und Summenscore ----
# Variablen in R/F codieren & Summenscore bei k Prim

# F1 k Prim Score
data_irt12_1$F1_1 <- ifelse(data_irt12_1$F1_1 == 1, 0.25, 0)
data_irt12_1$F1_2 <- ifelse(data_irt12_1$F1_2 == 2, 0.25, 0)
data_irt12_1$F1_3 <- ifelse(data_irt12_1$F1_3 == 2, 0.25, 0)
data_irt12_1$F1_4 <- ifelse(data_irt12_1$F1_4 == 2, 0.25, 0)
# F1 Summenscore
data_irt12_1$F1 <- rowSums(select(data_irt12_1, F1_1, F1_2, F1_3, F1_4))
data_irt12_1 <- relocate(data_irt12_1, F1, .after = F1_4)

# F2  dichotomisieren
data_irt12_1$F2 <- ifelse(data_irt12_1$F2 == 1, 1, 0)

# F3 k Prim Score
data_irt12_1$F3_1 <- ifelse(data_irt12_1$F3_1 == 1, 0.25, 0)
data_irt12_1$F3_2 <- ifelse(data_irt12_1$F3_2 == 2, 0.25, 0)
data_irt12_1$F3_3 <- ifelse(data_irt12_1$F3_3 == 2, 0.25, 0)
data_irt12_1$F3_4 <- ifelse(data_irt12_1$F3_4 == 2, 0.25, 0)
# F3 Summenscore
data_irt12_1$F3 <- rowSums(select(data_irt12_1, F3_1, F3_2, F3_3, F3_4))
data_irt12_1 <- relocate(data_irt12_1, F3, .after = F3_4)

# F4 dichotomisieren
data_irt12_1$F4 <- ifelse(data_irt12_1$F4 == 1, 1, 0)

# F5 k Prim Score
data_irt12_1$F5_1 <- ifelse(data_irt12_1$F5_1 == 1, 0.2, 0)
data_irt12_1$F5_2 <- ifelse(data_irt12_1$F5_2 == 2, 0.2, 0)
data_irt12_1$F5_3 <- ifelse(data_irt12_1$F5_3 == 1, 0.2, 0)
data_irt12_1$F5_4 <- ifelse(data_irt12_1$F5_4 == 1, 0.2, 0)
data_irt12_1$F5_5 <- ifelse(data_irt12_1$F5_5 == 1, 0.2, 0)
# F5 Summenscore
data_irt12_1$F5 <- rowSums(select(data_irt12_1, F5_1, F5_2, F5_3, F5_4, F5_5))
data_irt12_1 <- relocate(data_irt12_1, F5, .after = F5_5)

# F6 k Prim Score
data_irt12_1$F6_1 <- ifelse(data_irt12_1$F6_1 == 1, (1/3), 0)
data_irt12_1$F6_2 <- ifelse(data_irt12_1$F6_2 == 2, (1/3), 0)
data_irt12_1$F6_3 <- ifelse(data_irt12_1$F6_3 == 1, (1/3), 0)
# F6 Summenscore
data_irt12_1$F6 <- rowSums(select(data_irt12_1, F6_1, F6_2, F6_3))
data_irt12_1 <- relocate(data_irt12_1, F6, .after = F6_3)

# F7 k Prim Score
data_irt12_1$F7_1 <- ifelse(data_irt12_1$F7_1 == 1, (1/3), 0)
data_irt12_1$F7_2 <- ifelse(data_irt12_1$F7_2 == 1, (1/3), 0)
data_irt12_1$F7_3 <- ifelse(data_irt12_1$F7_3 == 1, (1/3), 0)
# F7 Summenscore
data_irt12_1$F7 <- rowSums(select(data_irt12_1, F7_1, F7_2, F7_3))
data_irt12_1 <- relocate(data_irt12_1, F7, .after = F7_3)

# F8 k Prim Score
data_irt12_1$F8_1 <- ifelse(data_irt12_1$F8_1 == 2, (1/3), 0)
data_irt12_1$F8_2 <- ifelse(data_irt12_1$F8_2 == 1, (1/3), 0)
data_irt12_1$F8_3 <- ifelse(data_irt12_1$F8_3 == 1, (1/3), 0)


# F8 Summenscore
#data_irt12_1$F8 <- rowSums(select(data_irt12_1, F8_1, F8_2, F8_3))
#data_irt12_1 <- relocate(data_irt12_1, F8, .after = F8_3)

# F9 k Prim Score - gehört zu Frage 8
data_irt12_1$F9_1 <- ifelse(data_irt12_1$F9_1 == 1, (1/3), 0)
data_irt12_1$F9_2 <- ifelse(data_irt12_1$F9_2 == 2, (1/3), 0)
data_irt12_1$F9_3 <- ifelse(data_irt12_1$F9_3 == 1, (1/3), 0)
# F9 Summenscore
data_irt12_1$F9 <- rowSums(select(data_irt12_1, F8_1, F8_2, F8_3, F9_1, F9_2, F9_3))
data_irt12_1 <- relocate(data_irt12_1, F9, .after = F9_3)

# F10 k Prim Score
data_irt12_1$F10_1 <- ifelse(data_irt12_1$F10_1 == 1, 0.25, 0)
data_irt12_1$F10_2 <- ifelse(data_irt12_1$F10_2 == 2, 0.25, 0)
data_irt12_1$F10_3 <- ifelse(data_irt12_1$F10_3 == 2, 0.25, 0)
data_irt12_1$F10_4 <- ifelse(data_irt12_1$F10_4 == 1, 0.25, 0)
# F10 Summenscore
data_irt12_1$F10 <- rowSums(select(data_irt12_1, F10_1, F10_2, F10_3, F10_4))
data_irt12_1 <- relocate(data_irt12_1, F10, .after = F10_4)

# F11  dichotomisieren
data_irt12_1$F11 <- ifelse(data_irt12_1$F11 == 2, 1, 0)

# F12 k Prim Score
data_irt12_1$F12_1 <- ifelse(data_irt12_1$F12_1 == 1, 0.25, 0)
data_irt12_1$F12_2 <- ifelse(data_irt12_1$F12_2 == 2, 0.25, 0)
data_irt12_1$F12_3 <- ifelse(data_irt12_1$F12_3 == 1, 0.25, 0)
data_irt12_1$F12_4 <- ifelse(data_irt12_1$F12_4 == 2, 0.25, 0)
# F12 Summenscore
data_irt12_1$F12 <- rowSums(select(data_irt12_1, F12_1, F12_2, F12_3, F12_4))
data_irt12_1 <- relocate(data_irt12_1, F12, .after = F12_4)

# F13 k Prim Score
data_irt12_1$F13_1 <- ifelse(data_irt12_1$F13_1 == 2, 0.25, 0)
data_irt12_1$F13_2 <- ifelse(data_irt12_1$F13_2 == 2, 0.25, 0)
data_irt12_1$F13_3 <- ifelse(data_irt12_1$F13_3 == 1, 0.25, 0)
data_irt12_1$F13_4 <- ifelse(data_irt12_1$F13_4 == 1, 0.25, 0)
# F13 Summenscore
data_irt12_1$F13 <- rowSums(select(data_irt12_1, F13_1, F13_2, F13_3, F13_4))
data_irt12_1 <- relocate(data_irt12_1, F13, .after = F13_4)

# F14 k Prim Score
data_irt12_1$F14_1 <- ifelse(data_irt12_1$F14_1 == 1, 0.25, 0)
data_irt12_1$F14_2 <- ifelse(data_irt12_1$F14_2 == 1, 0.25, 0)
data_irt12_1$F14_3 <- ifelse(data_irt12_1$F14_3 == 2, 0.25, 0)
data_irt12_1$F14_4 <- ifelse(data_irt12_1$F14_4 == 3, 0.25, 0)
# F14 Summenscore
data_irt12_1$F14 <- rowSums(select(data_irt12_1, F14_1, F14_2, F14_3, F14_4))
data_irt12_1 <- relocate(data_irt12_1, F14, .after = F14_4)


# 2. Termin IRT12 Scoring in R/F und Summenscore ----
# F1 dichotomisieren
data_irt12_2$F1 <- ifelse(data_irt12_2$F1 == "0.52", 1, 0)

# F2  dichotomisieren
data_irt12_2$F2 <- ifelse(data_irt12_2$F2 == 5, 1, 0)

# F3 k Prim Score
data_irt12_2$F3_1 <- ifelse(data_irt12_2$F3_1 == 1, 0.25, 0)
data_irt12_2$F3_2 <- ifelse(data_irt12_2$F3_2 == 2, 0.25, 0)
data_irt12_2$F3_3 <- ifelse(data_irt12_2$F3_3 == 1, 0.25, 0)
data_irt12_2$F3_4 <- ifelse(data_irt12_2$F3_4 == 1, 0.25, 0)
# F3 Summenscore
data_irt12_2$F3 <- rowSums(select(data_irt12_2, F3_1, F3_2, F3_3, F3_4))
data_irt12_2 <- relocate(data_irt12_2, F3, .after = F3_4)

# F4 k Prim Score
data_irt12_2$F4a <- ifelse(data_irt12_2$F4a == "73.11", 0.5, 0)
data_irt12_2$F4b <- ifelse(data_irt12_2$F4b == "26.89", 0.5, 0)
# F4 Summenscore
data_irt12_2$F4 <- rowSums(select(data_irt12_2, F4a, F4b))
data_irt12_2 <- relocate(data_irt12_2, F4, .after = F4b)

# F5  dichotomisieren
data_irt12_2$F5 <- ifelse(data_irt12_2$F5 == 3, 1, 0)

# F6 k Prim Score
data_irt12_2$F6_1 <- ifelse(data_irt12_2$F6_1 == 2, 0.25, 0)
data_irt12_2$F6_2 <- ifelse(data_irt12_2$F6_2 == 1, 0.25, 0)
data_irt12_2$F6_3 <- ifelse(data_irt12_2$F6_3 == 2, 0.25, 0)
data_irt12_2$F6_4 <- ifelse(data_irt12_2$F6_4 == 1, 0.25, 0)
# F6 Summenscore
data_irt12_2$F6 <- rowSums(select(data_irt12_2, F6_1, F6_2, F6_3, F6_4))
data_irt12_2 <- relocate(data_irt12_2, F6, .after = F6_4)


# F7 k Prim Score
data_irt12_2$F7_1 <- ifelse(data_irt12_2$F7_1 == 1, 0.25, 0)
data_irt12_2$F7_2 <- ifelse(data_irt12_2$F7_2 == 1, 0.25, 0)
data_irt12_2$F7_3 <- ifelse(data_irt12_2$F7_3 == 2, 0.25, 0)
data_irt12_2$F7_4 <- ifelse(data_irt12_2$F7_4 == 1, 0.25, 0)
# F7 Summenscore
data_irt12_2$F7 <- rowSums(select(data_irt12_2, F7_1, F7_2, F7_3, F7_4))
data_irt12_2 <- relocate(data_irt12_2, F7, .after = F7_4)


# F8 k Prim Score
data_irt12_2$F8_1 <- ifelse(data_irt12_2$F8_1 == 1, 0.25, 0)
data_irt12_2$F8_2 <- ifelse(data_irt12_2$F8_2 == 1, 0.25, 0)
data_irt12_2$F8_3 <- ifelse(data_irt12_2$F8_3 == 1, 0.25, 0)
data_irt12_2$F8_4 <- ifelse(data_irt12_2$F8_4 == 2, 0.25, 0)
# F8 Summenscore
data_irt12_2$F8 <- rowSums(select(data_irt12_2, F8_1, F8_2, F8_3, F8_4))
data_irt12_2 <- relocate(data_irt12_2, F8, .after = F8_4)

# F9  dichotomisieren
data_irt12_2$F9 <- ifelse(data_irt12_2$F9 == 2, 1, 0)


# F10 k Prim Score
data_irt12_2$F10_1 <- ifelse(data_irt12_2$F10_1 == 2, 0.25, 0)
data_irt12_2$F10_2 <- ifelse(data_irt12_2$F10_2 == 2, 0.25, 0)
data_irt12_2$F10_3 <- ifelse(data_irt12_2$F10_3 == 1, 0.25, 0)
data_irt12_2$F10_4 <- ifelse(data_irt12_2$F10_4 == 1, 0.25, 0)
# F10 Summenscore
data_irt12_2$F10 <- rowSums(select(data_irt12_2, F10_1, F10_2, F10_3, F10_4))
data_irt12_2 <- relocate(data_irt12_2, F10, .after = F10_4)


# F11 k Prim Score
data_irt12_2$F11a <- ifelse(data_irt12_2$F11a == "0.73", 0.25, 0)
data_irt12_2$F11b <- ifelse(data_irt12_2$F11b == "0.50", 0.25, 0)
data_irt12_2$F11c <- ifelse(data_irt12_2$F11c == "0.73", 0.25, 0)
data_irt12_2$F11d <- ifelse(data_irt12_2$F11d == "0.032", 0.25, 0)
# F11 Summenscore
data_irt12_2$F11 <- rowSums(select(data_irt12_2, F11a, F11b, F11c, F11d))
data_irt12_2 <- relocate(data_irt12_2, F11, .after = F11d)


# F12 k Prim Score
data_irt12_2$F12_1 <- ifelse(data_irt12_2$F12_1 == 1, 0.25, 0)
data_irt12_2$F12_2 <- ifelse(data_irt12_2$F12_2 == 2, 0.25, 0)
data_irt12_2$F12_3 <- ifelse(data_irt12_2$F12_3 == 2, 0.25, 0)
data_irt12_2$F12_4 <- ifelse(data_irt12_2$F12_4 == 1, 0.25, 0)
# F12 Summenscore
data_irt12_2$F12 <- rowSums(select(data_irt12_2, F12_1, F12_2, F12_3, F12_4))
data_irt12_2 <- relocate(data_irt12_2, F12, .after = F12_4)

# Keine F13

# F14  dichotomisieren
data_irt12_2$F14 <- ifelse(data_irt12_2$F14 == 6, 1, 0)


# 1. Termin IRT34 Scoring in R/F und Summenscore ----

# Variablen in R/F codieren & Summenscore bei k Prim
# F1 k Prim Score
data_irt34_1$F1_1 <- ifelse(data_irt34_1$F1_1 == 1, 0.2, 0)
data_irt34_1$F1_2 <- ifelse(data_irt34_1$F1_2 == 9, 0.2, 0)
data_irt34_1$F1_3 <- ifelse(data_irt34_1$F1_3 == 1, 0.2, 0)
data_irt34_1$F1_4 <- ifelse(data_irt34_1$F1_4 == 9, 0.2, 0)
data_irt34_1$F1_5 <- ifelse(data_irt34_1$F1_4 == 1, 0.2, 0)
# F1 Summenscore
data_irt34_1$F1 <- rowSums(select(data_irt34_1, F1_1, F1_2, F1_3, F1_4, F1_5))
data_irt34_1 <- relocate(data_irt34_1, F1, .after = F1_5)

# F2 dichotomisieren
data_irt34_1$F2 <- ifelse(data_irt34_1$F2 == 1, 1, 0)

# F3 k Prim Score
data_irt34_1$F3_1 <- ifelse(data_irt34_1$F3_1 == 2, 0.25, 0)
data_irt34_1$F3_2 <- ifelse(data_irt34_1$F3_2 == 1, 0.25, 0)
data_irt34_1$F3_3 <- ifelse(data_irt34_1$F3_3 == 2, 0.25, 0)
data_irt34_1$F3_4 <- ifelse(data_irt34_1$F3_4 == 1, 0.25, 0)
# F3 Summenscore
data_irt34_1$F3 <- rowSums(select(data_irt34_1, F3_1, F3_2, F3_3, F3_4))
data_irt34_1 <- relocate(data_irt34_1, F3, .after = F3_4)

# F4 dichotomisieren
data_irt34_1$F4 <- ifelse(data_irt34_1$F4 == 1, 1, 0)

# F5 k Prim Score
data_irt34_1$F5_1 <- ifelse(data_irt34_1$F5_1 == 2, 0.25, 0)
data_irt34_1$F5_2 <- ifelse(data_irt34_1$F5_2 == 1, 0.25, 0)
data_irt34_1$F5_3 <- ifelse(data_irt34_1$F5_3 == 1, 0.25, 0)
data_irt34_1$F5_4 <- ifelse(data_irt34_1$F5_4 == 1, 0.25, 0)

# F5 Summenscore
data_irt34_1$F5 <- rowSums(select(data_irt34_1, F5_1, F5_2, F5_3, F5_4))
data_irt34_1 <- relocate(data_irt34_1, F5, .after = F5_4)

# F6 k Prim Score
data_irt34_1$F6_1 <- ifelse(data_irt34_1$F6_1 == 1, 0.25, 0)
data_irt34_1$F6_2 <- ifelse(data_irt34_1$F6_2 == 2, 0.25, 0)
data_irt34_1$F6_3 <- ifelse(data_irt34_1$F6_3 == 2, 0.25, 0)
data_irt34_1$F6_4 <- ifelse(data_irt34_1$F6_3 == 1, 0.25, 0)
# F6 Summenscore
data_irt34_1$F6 <- rowSums(select(data_irt34_1, F6_1, F6_2, F6_3, F6_4))
data_irt34_1 <- relocate(data_irt34_1, F6, .after = F6_4)

# F7 dichotomisieren
data_irt34_1$F7 <- ifelse(data_irt34_1$F7 == 2, 1, 0)

# F8 dichotomisieren
data_irt34_1$F8 <- ifelse(data_irt34_1$F8 == 4, 1, 0)

# F9 k Prim Score
data_irt34_1$F9_1 <- ifelse(data_irt34_1$F9_1 == 2, 0.25, 0)
data_irt34_1$F9_2 <- ifelse(data_irt34_1$F9_2 == 2, 0.25, 0)
data_irt34_1$F9_3 <- ifelse(data_irt34_1$F9_3 == 1, 0.25, 0)
data_irt34_1$F9_4 <- ifelse(data_irt34_1$F9_4 == 2, 0.25, 0)
# F9 Summenscore
data_irt34_1$F9 <- rowSums(select(data_irt34_1, F9_1, F9_2, F9_3, F9_4))
data_irt34_1 <- relocate(data_irt34_1, F9, .after = F9_4)

# F10 k Prim Score
data_irt34_1$F10_1 <- ifelse(data_irt34_1$F10_1 == 2, 0.25, 0)
data_irt34_1$F10_2 <- ifelse(data_irt34_1$F10_2 == 1, 0.25, 0)
data_irt34_1$F10_3 <- ifelse(data_irt34_1$F10_3 == 1, 0.25, 0)
data_irt34_1$F10_4 <- ifelse(data_irt34_1$F10_4 == 1, 0.25, 0)
# F10 Summenscore
data_irt34_1$F10 <- rowSums(select(data_irt34_1, F10_1, F10_2, F10_3, F10_4))
data_irt34_1 <- relocate(data_irt34_1, F10, .after = F10_4)

# F11 k Prim Score
data_irt34_1$F11_1 <- ifelse(data_irt34_1$F11_1 == 2, 0.25, 0)
data_irt34_1$F11_2 <- ifelse(data_irt34_1$F11_2 == 2, 0.25, 0)
data_irt34_1$F11_3 <- ifelse(data_irt34_1$F11_3 == 1, 0.25, 0)
data_irt34_1$F11_4 <- ifelse(data_irt34_1$F11_4 == 1, 0.25, 0)
# F10 Summenscore
data_irt34_1$F11 <- rowSums(select(data_irt34_1, F11_1, F11_2, F11_3, F11_4))
data_irt34_1 <- relocate(data_irt34_1, F11, .after = F11_4)

# F12 dichotomisieren
data_irt34_1$F12 <- ifelse(data_irt34_1$F12 == 5, 1, 0)


# 2. Termin IRT34 Scoring in R/F und Summenscore ----

# F1 k Prim Score
data_irt34_2$F1_1 <- ifelse(data_irt34_2$F1_1 == 9, 0.25, 0)
data_irt34_2$F1_2 <- ifelse(data_irt34_2$F1_2 == 1, 0.25, 0)
data_irt34_2$F1_3 <- ifelse(data_irt34_2$F1_3 == 9, 0.25, 0)
data_irt34_2$F1_4 <- ifelse(data_irt34_2$F1_4 == 1, 0.25, 0)
# F1 Summenscore
data_irt34_2$F1 <- rowSums(select(data_irt34_2, F1_1, F1_2, F1_3, F1_4))
data_irt34_2 <- relocate(data_irt34_2, F1, .after = F1_4)

# F2 k Prim Score
data_irt34_2$F2_1 <- ifelse(data_irt34_2$F2_1 == 1, 0.25, 0)
data_irt34_2$F2_2 <- ifelse(data_irt34_2$F2_2 == 1, 0.25, 0)
data_irt34_2$F2_3 <- ifelse(data_irt34_2$F2_3 == 2, 0.25, 0)
data_irt34_2$F2_4 <- ifelse(data_irt34_2$F2_4 == 2, 0.25, 0)
# F2 Summenscore
data_irt34_2$F2 <- rowSums(select(data_irt34_2, F2_1, F2_2, F2_3, F2_4))
data_irt34_2 <- relocate(data_irt34_2, F2, .after = F2_4)


# F3 freie Antwort Score
data_irt34_2$F3a <- ifelse(data_irt34_2$F3a == "0.6192", 0.5, 0)
data_irt34_2$F3b <- ifelse(data_irt34_2$F3b == "0.5516", 0.5, 0)
# F3 Summenscore
data_irt34_2$F3 <- rowSums(select(data_irt34_2, F3a, F3b))
data_irt34_2 <- relocate(data_irt34_2, F3, .after = F3b)


# F4 k Prim & freie Antwort Score
data_irt34_2$F4a <- ifelse(data_irt34_2$F4a == "-4.60", (1/6), 0)
data_irt34_2$F4b <- ifelse(data_irt34_2$F4b == "1.60", (1/6), 0)
data_irt34_2$F4c <- ifelse(data_irt34_2$F4c == "-1.53", (1/6), 0)
data_irt34_2$F4d <- ifelse(data_irt34_2$F4d == "3.53", (1/6), 0)
data_irt34_2$F4e_1 <- ifelse(data_irt34_2$F4e_1 == 2, (1/6), 0)
data_irt34_2$F4e_2 <- ifelse(data_irt34_2$F4e_2 == 1, (1/6), 0)
# F4 Summenscore
data_irt34_2$F4 <- rowSums(select(data_irt34_2, F4a, F4b, F4c, F4d, F4e_1, F4e_2))
data_irt34_2 <- relocate(data_irt34_2, F4, .after = F4e_2)


# F5  dichotomisieren NICHT in Summenwert einfliessen lassen
data_irt34_2$F5 <- ifelse(data_irt34_2$F5 == 1, 1, 0)


# F6 k Prim Score
data_irt34_2$F6_1 <- ifelse(data_irt34_2$F6_1 == 2, 0.25, 0)
data_irt34_2$F6_2 <- ifelse(data_irt34_2$F6_2 == 1, 0.25, 0)
data_irt34_2$F6_3 <- ifelse(data_irt34_2$F6_3 == 1, 0.25, 0)
data_irt34_2$F6_4 <- ifelse(data_irt34_2$F6_4 == 1, 0.25, 0)
# F6 Summenscore
data_irt34_2$F6 <- rowSums(select(data_irt34_2, F6_1, F6_2, F6_3, F6_4))
data_irt34_2 <- relocate(data_irt34_2, F6, .after = F6_4)

# F7 k Prim Score
data_irt34_2$F7_1 <- ifelse(data_irt34_2$F7_1 == 2, 0.25, 0)
data_irt34_2$F7_2 <- ifelse(data_irt34_2$F7_2 == 1, 0.25, 0)
data_irt34_2$F7_3 <- ifelse(data_irt34_2$F7_3 == 1, 0.25, 0)
data_irt34_2$F7_4 <- ifelse(data_irt34_2$F7_4 == 2, 0.25, 0)
# F7 Summenscore
data_irt34_2$F7 <- rowSums(select(data_irt34_2, F7_1, F7_2, F7_3, F7_4))
data_irt34_2 <- relocate(data_irt34_2, F7, .after = F7_4)

# F8  dichotomisieren
data_irt34_2$F8 <- ifelse(data_irt34_2$F8 == 3, 1, 0)

# F9 k Prim Score
data_irt34_2$F9a <- ifelse(data_irt34_2$F9a == "9", 0.25, 0)
data_irt34_2$F9b_1 <- ifelse(data_irt34_2$F9b_1 == 2, 0.25, 0)
data_irt34_2$F9b_2 <- ifelse(data_irt34_2$F9b_2 == 2, 0.25, 0)
data_irt34_2$F9b_3 <- ifelse(data_irt34_2$F9b_3 == 1, 0.25, 0)
# F9 Summenscore
data_irt34_2$F9 <- rowSums(select(data_irt34_2, F9a, F9b_1, F9b_2, F9b_3))
data_irt34_2 <- relocate(data_irt34_2, F9, .after = F9b_3)


# F10 k Prim Score
data_irt34_2$F10_1 <- ifelse(data_irt34_2$F10_1 == 2, 0.25, 0)
data_irt34_2$F10_2 <- ifelse(data_irt34_2$F10_2 == 1, 0.25, 0)
data_irt34_2$F10_3 <- ifelse(data_irt34_2$F10_3 == 2, 0.25, 0)
data_irt34_2$F10_4 <- ifelse(data_irt34_2$F10_4 == 2, 0.25, 0)
# F10 Summenscore
data_irt34_2$F10 <- rowSums(select(data_irt34_2, F10_1, F10_2, F10_3, F10_4))
data_irt34_2 <- relocate(data_irt34_2, F10, .after = F10_4)


# F11 k Prim Score
data_irt34_2$F11_1 <- ifelse(data_irt34_2$F11_1 == 2, 0.2, 0)
data_irt34_2$F11_2 <- ifelse(data_irt34_2$F11_2 == 2, 0.2, 0)
data_irt34_2$F11_3 <- ifelse(data_irt34_2$F11_3 == 1, 0.2, 0)
data_irt34_2$F11_4 <- ifelse(data_irt34_2$F11_4 == 1, 0.2, 0)
data_irt34_2$F11_5 <- ifelse(data_irt34_2$F11_5 == 1, 0.2, 0)
# F11 Summenscore
data_irt34_2$F11 <- rowSums(select(data_irt34_2, F11_1, F11_2, F11_3, F11_4, F11_5))
data_irt34_2 <- relocate(data_irt34_2, F11, .after = F11_5)

# F12 k Prim Score
data_irt34_2$F12a <- ifelse(data_irt34_2$F12a == "1.9", (1/6), 0)
data_irt34_2$F12b <- ifelse(data_irt34_2$F12b == "0.6", (1/6), 0)
data_irt34_2$F12c <- ifelse(data_irt34_2$F12c == "57", (1/6), 0)
data_irt34_2$F12d <- ifelse(data_irt34_2$F12d == "47", (1/6), 0)
data_irt34_2$F12e <- ifelse(data_irt34_2$F12e == "66", (1/6), 0)
data_irt34_2$F12f_1 <- ifelse(data_irt34_2$F12f_1 == 4, (1/6), 0)
# F12 Summenscore
data_irt34_2$F12 <- rowSums(select(data_irt34_2, F12a, F12b, F12c, F12d, F12e, F12f_1))
data_irt34_2 <- relocate(data_irt34_2, F12, .after = F12f_1)


# F13 k Prim Score
data_irt34_2$F13_1 <- ifelse(data_irt34_2$F13_1 == 1, 0.25, 0)
data_irt34_2$F13_2 <- ifelse(data_irt34_2$F13_2 == 2, 0.25, 0)
data_irt34_2$F13_3 <- ifelse(data_irt34_2$F13_3 == 2, 0.25, 0)
data_irt34_2$F13_4 <- ifelse(data_irt34_2$F13_4 == 2, 0.25, 0)
# F13 Summenscore
data_irt34_2$F13 <- rowSums(select(data_irt34_2, F13_1, F13_2, F13_3, F13_4))
data_irt34_2 <- relocate(data_irt34_2, F13, .after = F13_4)


# 1. Termin CFA12 Scoring in R/F und Summenscore ----

# Variablen in R/F codieren & Summenscore bei k Prim
# F1 dichotomisieren
data_cfa12_1$F1 <- ifelse(data_cfa12_1$F1 == 2, 1, 0)

# F2 dichotomisieren
data_cfa12_1$F2 <- ifelse(data_cfa12_1$F2 == 2, 1, 0)

# F3  dichotomisieren
data_cfa12_1$F3 <- ifelse(data_cfa12_1$F3 == 3, 1, 0)

# F4 dichotomisieren
data_cfa12_1$F4 <- ifelse(data_cfa12_1$F4 == 1, 1, 0)

# Keine F5

# F6 dichotomisieren
data_cfa12_1$F6 <- ifelse(data_cfa12_1$F6 == 2, 1, 0)

# F7  dichotomisieren
data_cfa12_1$F7 <- ifelse(data_cfa12_1$F7 == 3, 1, 0)

# F8 dichotomisieren
data_cfa12_1$F8 <- ifelse(data_cfa12_1$F8 == 4, 1, 0)

# F9 dichotomisieren
data_cfa12_1$F9 <- ifelse(data_cfa12_1$F9 == 2, 1, 0)

# F10  dichotomisieren
data_cfa12_1$F10 <- ifelse(data_cfa12_1$F10 == 1, 1, 0)

# F11 k Prim Score
data_cfa12_1$F11_1 <- ifelse(data_cfa12_1$F11_1 == 2, 0.25, 0)
data_cfa12_1$F11_2 <- ifelse(data_cfa12_1$F11_2 == 1, 0.25, 0)
data_cfa12_1$F11_3 <- ifelse(data_cfa12_1$F11_3 == 1, 0.25, 0)
data_cfa12_1$F11_4 <- ifelse(data_cfa12_1$F11_4 == 1, 0.25, 0)
# F11 Summenscore
data_cfa12_1$F11 <- rowSums(select(data_cfa12_1, F11_1, F11_2, F11_3, F11_4))
data_cfa12_1 <- relocate(data_cfa12_1, F11, .after = F11_4)

# F12  dichotomisieren
data_cfa12_1$F12 <- ifelse(data_cfa12_1$F12 == 1, 1, 0)

# F13 dichotomisieren
data_cfa12_1$F13 <- ifelse(data_cfa12_1$F13 == 2, 1, 0)

# F14 k Prim Score
data_cfa12_1$F14_1 <- ifelse(data_cfa12_1$F14_1 == 1, 0.25, 0)
data_cfa12_1$F14_2 <- ifelse(data_cfa12_1$F14_2 == 1, 0.25, 0)
data_cfa12_1$F14_3 <- ifelse(data_cfa12_1$F14_3 == 1, 0.25, 0)
data_cfa12_1$F14_4 <- ifelse(data_cfa12_1$F14_4 == 1, 0.25, 0)
# F14 Summenscore
data_cfa12_1$F14 <- rowSums(select(data_cfa12_1, F14_1, F14_2, F14_3, F14_4))
data_cfa12_1 <- relocate(data_cfa12_1, F14, .after = F14_4)

# F15  dichotomisieren
data_cfa12_1$F15 <- ifelse(data_cfa12_1$F15 == 2, 1, 0)

# F16 k Prim Score
data_cfa12_1$F16_1 <- ifelse(data_cfa12_1$F16_1 == 2, (1/3), 0)
data_cfa12_1$F16_2 <- ifelse(data_cfa12_1$F16_2 == 1, (1/3), 0)
data_cfa12_1$F16_3 <- ifelse(data_cfa12_1$F16_3 == 2, (1/3), 0)
# F14 Summenscore
data_cfa12_1$F16 <- rowSums(select(data_cfa12_1, F16_1, F16_2, F16_3))
data_cfa12_1 <- relocate(data_cfa12_1, F16, .after = F16_3)

# F17  dichotomisieren
data_cfa12_1$F17 <- ifelse(data_cfa12_1$F17 == 1, 1, 0)

# 2. Termin CFA12 Scoring in R/F und Summenscore ----

# Variablen in R/F codieren & Summenscore bei k Prim
# F1 dichotomisieren
data_cfa12_2$F1 <- ifelse(data_cfa12_2$F1 == 2, 1, 0)

# F2 k Prim Score
data_cfa12_2$F2 <- ifelse(data_cfa12_2$F2 == 4, (1/3), 0)
data_cfa12_2$F2a <- ifelse(data_cfa12_2$F2a == "28", (1/3), 0)
data_cfa12_2$F2b <- ifelse(data_cfa12_2$F2b == "14", (1/3), 0)
# F2 Summenscore
data_cfa12_2$F2 <- rowSums(select(data_cfa12_2, F2, F2a, F2b))
data_cfa12_2 <- relocate(data_cfa12_2, F2, .after = F2b)

# F3 dichotomisieren
data_cfa12_2$F3 <- ifelse(data_cfa12_2$F3 == 4, 1, 0)

# F4 k Prim Score
data_cfa12_2$F4a <- ifelse(data_cfa12_2$F4a %in% c(".766", "0.766"), (1/3), 0)
data_cfa12_2$F4b <- ifelse(data_cfa12_2$F4b %in% c(".691", "0.691"), (1/3), 0)
data_cfa12_2$F4c <- ifelse(data_cfa12_2$F4c %in% c(".572", "0.572"), (1/3), 0)
# F4 Summenscore
data_cfa12_2$F4 <- rowSums(select(data_cfa12_2, F4a, F4b, F4c))
data_cfa12_2 <- relocate(data_cfa12_2, F4, .after = F4c)

# F5 dichotomisieren
data_cfa12_2$F5 <- ifelse(data_cfa12_2$F5 == 4, 1, 0)


# F6 k Prim Score
data_cfa12_2$F6a <- ifelse(data_cfa12_2$F6a %in% c(".541", "0.541"), (1/3), 0)
data_cfa12_2$F6b <- ifelse(data_cfa12_2$F6b %in% c(".488", "0.488"), (1/3), 0)
data_cfa12_2$F6c <- ifelse(data_cfa12_2$F6c %in% c(".404", "0.404"), (1/3), 0)
# F6 Summenscore
data_cfa12_2$F6 <- rowSums(select(data_cfa12_2, F6a, F6b, F6c))
data_cfa12_2 <- relocate(data_cfa12_2, F6, .after = F6c)


# F7 k Prim Score
data_cfa12_2$F7_1 <- ifelse(data_cfa12_2$F7_1 == 1, (1/3), 0)
data_cfa12_2$F7_2 <- ifelse(data_cfa12_2$F7_2 == 2, (1/3), 0)
data_cfa12_2$F7_3 <- ifelse(data_cfa12_2$F7_3 == 2, (1/3), 0)
# F7 Summenscore
data_cfa12_2$F7 <- rowSums(select(data_cfa12_2, F7_1, F7_2, F7_3))
data_cfa12_2 <- relocate(data_cfa12_2, F7, .after = F7_3)

# F8 dichotomisieren
data_cfa12_2$F8 <- ifelse(data_cfa12_2$F8 == 1, 1, 0)


# F9 k Prim Score
data_cfa12_2$F9_1 <- ifelse(data_cfa12_2$F9_1 == 1, 0.25, 0)
data_cfa12_2$F9_2 <- ifelse(data_cfa12_2$F9_2 == 1, 0.25, 0)
data_cfa12_2$F9_3 <- ifelse(data_cfa12_2$F9_3 == 2, 0.25, 0)
data_cfa12_2$F9_4 <- ifelse(data_cfa12_2$F9_4 == 2, 0.25, 0)
# F9 Summenscore
data_cfa12_2$F9 <- rowSums(select(data_cfa12_2, F9_1, F9_2, F9_3, F9_4))
data_cfa12_2 <- relocate(data_cfa12_2, F9, .after = F9_4)

# F10 dichotomisieren
data_cfa12_2$F10 <- ifelse(data_cfa12_2$F10 == 1, 1, 0)


# F11 k Prim Score
data_cfa12_2$F11_1 <- ifelse(data_cfa12_2$F11_1 == 2, 0.2, 0)
data_cfa12_2$F11_2 <- ifelse(data_cfa12_2$F11_2 == 2, 0.2, 0)
data_cfa12_2$F11_3 <- ifelse(data_cfa12_2$F11_3 == 2, 0.2, 0)
data_cfa12_2$F11_4 <- ifelse(data_cfa12_2$F11_4 == 2, 0.2, 0)
data_cfa12_2$F11_5 <- ifelse(data_cfa12_2$F11_5 == 1, 0.2, 0)
# F11 Summenscor
data_cfa12_2$F11 <- rowSums(select(data_cfa12_2, F11_1, F11_2, F11_3, F11_4, F11_5))
data_cfa12_2 <- relocate(data_cfa12_2, F11, .after = F11_5)


# F12 k Prim Score
data_cfa12_2$F12a <- ifelse(data_cfa12_2$F12a %in% c(".32", "0.32"), 0.25, 0)
data_cfa12_2$F12b <- ifelse(data_cfa12_2$F12b %in% c("4", "Q4"), 0.25, 0)
data_cfa12_2$F12c <- ifelse(data_cfa12_2$F12c %in% c(".72", "0.72"), 0.25, 0)
data_cfa12_2$F12d <- ifelse(data_cfa12_2$F12d == "1", 0.25, 0)
# F12 Summenscore
data_cfa12_2$F12 <- rowSums(select(data_cfa12_2, F12a, F12b, F12c, F12d))
data_cfa12_2 <- relocate(data_cfa12_2, F12, .after = F12d)


# F13 k Prim Score
data_cfa12_2$F13a_1 <- ifelse(data_cfa12_2$F13a_1 == 1, (1/3), 0)
data_cfa12_2$F13a_2 <- ifelse(data_cfa12_2$F13a_2 == 2, (1/3), 0)
data_cfa12_2$F13a_3 <- ifelse(data_cfa12_2$F13a_3 == 2, (1/3), 0)
# F13 Summenscore
data_cfa12_2$F13a <- rowSums(select(data_cfa12_2, F13a_1, F13a_2, F13a_3))
data_cfa12_2 <- relocate(data_cfa12_2, F13, .after = F13a_3)


# F14 k Prim Score
data_cfa12_2$F14_1 <- ifelse(data_cfa12_2$F14_1 == 1, 0.25, 0)
data_cfa12_2$F14_2 <- ifelse(data_cfa12_2$F14_2 == 1, 0.25, 0)
data_cfa12_2$F14_3 <- ifelse(data_cfa12_2$F14_3 == 1, 0.25, 0)
data_cfa12_2$F14_4 <- ifelse(data_cfa12_2$F14_4 == 2, 0.25, 0)
# F14 Summenscore
data_cfa12_2$F14 <- rowSums(select(data_cfa12_2, F14_1, F14_2, F14_3, F14_4))
data_cfa12_2 <- relocate(data_cfa12_2, F14, .after = F14_4)

# F15 dichotomisieren
data_cfa12_2$F15 <- ifelse(data_cfa12_2$F15 == 2, 1, 0)


# F16 k Prim Score
data_cfa12_2$F16_1 <- ifelse(data_cfa12_2$F16_1 == 2, 0.25, 0)
data_cfa12_2$F16_2 <- ifelse(data_cfa12_2$F16_2 == 1, 0.25, 0)
data_cfa12_2$F16_3 <- ifelse(data_cfa12_2$F16_3 == 1, 0.25, 0)
data_cfa12_2$F16_4 <- ifelse(data_cfa12_2$F16_4 == 2, 0.25, 0)
# F16 Summenscore
data_cfa12_2$F16 <- rowSums(select(data_cfa12_2, F16_1, F16_2, F16_3, F16_4))
data_cfa12_2 <- relocate(data_cfa12_2, F16, .after = F16_4)

# 1. Termin CFA34 Scoring in R/F und Summenscore ----

# Variablen in R/F codieren & Summenscore bei k Prim
# F1 k Prim Score
data_cfa34_1$F1_1 <- ifelse(data_cfa34_1$F1_1 == 1, 0.25, 0)
data_cfa34_1$F1_2 <- ifelse(data_cfa34_1$F1_2 == 2, 0.25, 0)
data_cfa34_1$F1_3 <- ifelse(data_cfa34_1$F1_3 == 2, 0.25, 0)
data_cfa34_1$F1_4 <- ifelse(data_cfa34_1$F1_4 == 2, 0.25, 0)
# F1 Summenscore
data_cfa34_1$F1 <- rowSums(select(data_cfa34_1, F1_1, F1_2, F1_3, F1_4))
data_cfa34_1 <- relocate(data_cfa34_1, F1, .after = F1_4)

# F2 k Prim Score
data_cfa34_1$F2_1 <- ifelse(data_cfa34_1$F2_1 == 1, 0.25, 0)
data_cfa34_1$F2_2 <- ifelse(data_cfa34_1$F2_2 == 2, 0.25, 0)
data_cfa34_1$F2_3 <- ifelse(data_cfa34_1$F2_3 == 1, 0.25, 0)
data_cfa34_1$F2_4 <- ifelse(data_cfa34_1$F2_4 == 2, 0.25, 0)
# F2 Summenscore
data_cfa34_1$F2 <- rowSums(select(data_cfa34_1, F2_1, F2_2, F2_3, F2_4))
data_cfa34_1 <- relocate(data_cfa34_1, F2, .after = F2_4)

# F3 dichotomisieren
data_cfa34_1$F3 <- ifelse(data_cfa34_1$F3 == 2, 1, 0)

# F4 k Prim Score
data_cfa34_1$F4_1 <- ifelse(data_cfa34_1$F4_1 == 1, 0.25, 0)
data_cfa34_1$F4_2 <- ifelse(data_cfa34_1$F4_2 == 1, 0.25, 0)
data_cfa34_1$F4_3 <- ifelse(data_cfa34_1$F4_3 == 1, 0.25, 0)
data_cfa34_1$F4_4 <- ifelse(data_cfa34_1$F4_4 == 2, 0.25, 0)
# F4 Summenscore
data_cfa34_1$F4 <- rowSums(select(data_cfa34_1, F4_1, F4_2, F4_3, F4_4))
data_cfa34_1 <- relocate(data_cfa34_1, F4, .after = F4_4)

# F5 k Prim Score
data_cfa34_1$F5_1 <- ifelse(data_cfa34_1$F5_1 == 1, 0.25, 0)
data_cfa34_1$F5_2 <- ifelse(data_cfa34_1$F5_2 == 2, 0.25, 0)
data_cfa34_1$F5_3 <- ifelse(data_cfa34_1$F5_3 == 2, 0.25, 0)
data_cfa34_1$F5_4 <- ifelse(data_cfa34_1$F5_4 == 1, 0.25, 0)
# F5 Summenscore
data_cfa34_1$F5 <- rowSums(select(data_cfa34_1, F5_1, F5_2, F5_3, F5_4))
data_cfa34_1 <- relocate(data_cfa34_1, F5, .after = F5_4)

# F6 k Prim Score
data_cfa34_1$F6_1 <- ifelse(data_cfa34_1$F6_1 == 2, 0.25, 0)
data_cfa34_1$F6_2 <- ifelse(data_cfa34_1$F6_2 == 1, 0.25, 0)
data_cfa34_1$F6_3 <- ifelse(data_cfa34_1$F6_3 == 1, 0.25, 0)
data_cfa34_1$F6_4 <- ifelse(data_cfa34_1$F6_4 == 2, 0.25, 0)
# F6 Summenscore
data_cfa34_1$F6 <- rowSums(select(data_cfa34_1, F6_1, F6_2, F6_3, F6_4))
data_cfa34_1 <- relocate(data_cfa34_1, F6, .after = F6_4)

# F7 k Prim Score
data_cfa34_1$F7_1 <- ifelse(data_cfa34_1$F7_1 == 2, 0.25, 0)
data_cfa34_1$F7_2 <- ifelse(data_cfa34_1$F7_2 == 2, 0.25, 0)
data_cfa34_1$F7_3 <- ifelse(data_cfa34_1$F7_3 == 2, 0.25, 0)
data_cfa34_1$F7_4 <- ifelse(data_cfa34_1$F7_4 == 1, 0.25, 0)
# F7 Summenscore
data_cfa34_1$F7 <- rowSums(select(data_cfa34_1, F7_1, F7_2, F7_3, F7_4))
data_cfa34_1 <- relocate(data_cfa34_1, F7, .after = F7_4)

# F8 dichotomisieren
data_cfa34_1$F8 <- ifelse(data_cfa34_1$F8 == 1, 1, 0)

# F9 k Prim Score
data_cfa34_1$F9_1 <- ifelse(data_cfa34_1$F9_1 == 2, 0.25, 0)
data_cfa34_1$F9_2 <- ifelse(data_cfa34_1$F9_2 == 1, 0.25, 0)
data_cfa34_1$F9_3 <- ifelse(data_cfa34_1$F9_3 == 2, 0.25, 0)
data_cfa34_1$F9_4 <- ifelse(data_cfa34_1$F9_4 == 1, 0.25, 0)
# F9 Summenscore
data_cfa34_1$F9 <- rowSums(select(data_cfa34_1, F9_1, F9_2, F9_3, F9_4))
data_cfa34_1 <- relocate(data_cfa34_1, F9, .after = F9_4)

# F10 k Prim Score
data_cfa34_1$F10_1 <- ifelse(data_cfa34_1$F10_1 == 1, 0.25, 0)
data_cfa34_1$F10_2 <- ifelse(data_cfa34_1$F10_2 == 2, 0.25, 0)
data_cfa34_1$F10_3 <- ifelse(data_cfa34_1$F10_3 == 2, 0.25, 0)
data_cfa34_1$F10_4 <- ifelse(data_cfa34_1$F10_4 == 2, 0.25, 0)
# F10 Summenscore
data_cfa34_1$F10 <- rowSums(select(data_cfa34_1, F10_1, F10_2, F10_3, F10_4))
data_cfa34_1 <- relocate(data_cfa34_1, F10, .after = F10_4)

# F11  dichotomisieren
data_cfa34_1$F11 <- ifelse(data_cfa34_1$F11 == 1, 1, 0)

# F12 k Prim Score
data_cfa34_1$F12_1 <- ifelse(data_cfa34_1$F12_1 == 5, 0.25, 0)
data_cfa34_1$F12_2 <- ifelse(data_cfa34_1$F12_2 == 5, 0.25, 0)
data_cfa34_1$F12_3 <- ifelse(data_cfa34_1$F12_3 == 5, 0.25, 0)
data_cfa34_1$F12_4 <- ifelse(data_cfa34_1$F12_4 == 4, 0.25, 0)
# F12 Summenscore
data_cfa34_1$F12 <- rowSums(select(data_cfa34_1, F12_1, F12_2, F12_3, F12_4))
data_cfa34_1 <- relocate(data_cfa34_1, F12, .after = F12_4)

# F13 dichotomisieren
data_cfa34_1$F13 <- ifelse(data_cfa34_1$F13 == 1, 1, 0)

# F14 k Prim Score
data_cfa34_1$F14_1 <- ifelse(data_cfa34_1$F14_1 == 1, 0.25, 0)
data_cfa34_1$F14_2 <- ifelse(data_cfa34_1$F14_2 == 1, 0.25, 0)
data_cfa34_1$F14_3 <- ifelse(data_cfa34_1$F14_3 == 2, 0.25, 0)
data_cfa34_1$F14_4 <- ifelse(data_cfa34_1$F14_4 == 2, 0.25, 0)
# F14 Summenscore
data_cfa34_1$F14 <- rowSums(select(data_cfa34_1, F14_1, F14_2, F14_3, F14_4))
data_cfa34_1 <- relocate(data_cfa34_1, F14, .after = F14_4)

# F15 k Prim Score
data_cfa34_1$F15_1 <- ifelse(data_cfa34_1$F15_1 == 1, 0.25, 0)
data_cfa34_1$F15_2 <- ifelse(data_cfa34_1$F15_2 == 1, 0.25, 0)
data_cfa34_1$F15_3 <- ifelse(data_cfa34_1$F15_3 == 2, 0.25, 0)
data_cfa34_1$F15_4 <- ifelse(data_cfa34_1$F15_4 == 2, 0.25, 0)
# F15 Summenscore
data_cfa34_1$F15 <- rowSums(select(data_cfa34_1, F15_1, F15_2, F15_3, F15_4))
data_cfa34_1 <- relocate(data_cfa34_1, F15, .after = F15_4)

# 2. Termin CFA34 Scoring in R/F und Summenscore ----

# Variablen in R/F codieren & Summenscore bei k Prim
# F1 k Prim Score
data_cfa34_2$F1_1 <- ifelse(data_cfa34_2$F1_1 == 2, (1/6), 0)
data_cfa34_2$F1_2 <- ifelse(data_cfa34_2$F1_2 == 1, (1/6), 0)
data_cfa34_2$F1_3 <- ifelse(data_cfa34_2$F1_3 == 5, (1/6), 0)
data_cfa34_2$F1_4 <- ifelse(data_cfa34_2$F1_4 == 4, (1/6), 0)
data_cfa34_2$F1_5 <- ifelse(data_cfa34_2$F1_5 == 1, (1/6), 0)
data_cfa34_2$F1_6 <- ifelse(data_cfa34_2$F1_6 == 4, (1/6), 0)
# F1 Summenscore
data_cfa34_2$F1 <- rowSums(select(data_cfa34_2, F1_1, F1_2, F1_3, F1_4, F1_5, F1_6))
data_cfa34_2 <- relocate(data_cfa34_2, F1, .after = F1_6)

# F2 k Prim Score
data_cfa34_2$F2_1 <- ifelse(data_cfa34_2$F2_1 == 4, 0.25, 0)
data_cfa34_2$F2_2 <- ifelse(data_cfa34_2$F2_2 == 3, 0.25, 0)
data_cfa34_2$F2_3 <- ifelse(data_cfa34_2$F2_3 == 1, 0.25, 0)
data_cfa34_2$F2_4 <- ifelse(data_cfa34_2$F2_4 == 2, 0.25, 0)
# F2 Summenscore
data_cfa34_2$F2 <- rowSums(select(data_cfa34_2, F2_1, F2_2, F2_3, F2_4))
data_cfa34_2 <- relocate(data_cfa34_2, F2, .after = F2_4)


# F3 k Prim Score
data_cfa34_2$F3a <- ifelse(data_cfa34_2$F3a == "0.14", (1/3), 0)
data_cfa34_2$F3b <- ifelse(data_cfa34_2$F3b == "0.55", (1/3), 0)
data_cfa34_2$F3c <- ifelse(data_cfa34_2$F3c == "0.31", (1/3), 0)
# F3 Summenscore
data_cfa34_2$F3 <- rowSums(select(data_cfa34_2, F3a, F3b, F3c))
data_cfa34_2 <- relocate(data_cfa34_2, F3, .after = F3c)


# F4 k Prim Score
data_cfa34_2$F4_1 <- ifelse(data_cfa34_2$F4_1 == 1, 0.25, 0)
data_cfa34_2$F4_2 <- ifelse(data_cfa34_2$F4_2 == 2, 0.25, 0)
data_cfa34_2$F4_3 <- ifelse(data_cfa34_2$F4_3 == 2, 0.25, 0)
data_cfa34_2$F4_4 <- ifelse(data_cfa34_2$F4_4 == 2, 0.25, 0)
# F4 Summenscore
data_cfa34_2$F4 <- rowSums(select(data_cfa34_2, F4_1, F4_2, F4_3, F4_4))
data_cfa34_2 <- relocate(data_cfa34_2, F4, .after = F4_4)


# F5 k Prim Score
data_cfa34_2$F5a_1 <- ifelse(data_cfa34_2$F5a_1 == 2, 0.25, 0)
data_cfa34_2$F5a_2 <- ifelse(data_cfa34_2$F5a_2 == 1, 0.25, 0)
data_cfa34_2$F5a_3 <- ifelse(data_cfa34_2$F5a_3 == 1, 0.25, 0)
data_cfa34_2$F5b <- ifelse(data_cfa34_2$F5b == "5.88", 0.25, 0)
# F5 Summenscore
data_cfa34_2$F5 <- rowSums(select(data_cfa34_2, F5a_1, F5a_2, F5a_3, F5b))
data_cfa34_2 <- relocate(data_cfa34_2, F5, .after = F5b)


# F6 k Prim Score
data_cfa34_2$F6_1 <- ifelse(data_cfa34_2$F6_1 == 2, 0.25, 0)
data_cfa34_2$F6_2 <- ifelse(data_cfa34_2$F6_2 == 1, 0.25, 0)
data_cfa34_2$F6_3 <- ifelse(data_cfa34_2$F6_3 == 1, 0.25, 0)
data_cfa34_2$F6_4 <- ifelse(data_cfa34_2$F6_4 == 2, 0.25, 0)
# F6 Summenscore
data_cfa34_2$F6 <- rowSums(select(data_cfa34_2, F6_1, F6_2, F6_3, F6_4))
data_cfa34_2 <- relocate(data_cfa34_2, F6, .after = F6_4)

# F7  dichotomisieren
data_cfa34_2$F7 <- ifelse(data_cfa34_2$F7 == 4, 1, 0)

# F8  dichotomisieren
data_cfa34_2$F8 <- ifelse(data_cfa34_2$F8 == 1, 1, 0)

# F9 k Prim Score
data_cfa34_2$F9a <- ifelse(data_cfa34_2$F9a == "16.00", 0.25, 0)
data_cfa34_2$F9b <- ifelse(data_cfa34_2$F9b == "8.09", 0.25, 0)
data_cfa34_2$F9c_1 <- ifelse(data_cfa34_2$F9c_1 == 1, 0.25, 0)
data_cfa34_2$F9d <- ifelse(data_cfa34_2$F9d == "0.46", 0.25, 0)
# F9 Summenscore
data_cfa34_2$F9 <- rowSums(select(data_cfa34_2, F9a, F9b, F9c_1, F9d))
data_cfa34_2 <- relocate(data_cfa34_2, F9, .after = F9d)

# F10 k Prim Score
data_cfa34_2$F10_1 <- ifelse(data_cfa34_2$F10_1 == 1, 0.25, 0)
data_cfa34_2$F10_2 <- ifelse(data_cfa34_2$F10_2 == 2, 0.25, 0)
data_cfa34_2$F10_3 <- ifelse(data_cfa34_2$F10_3 == 2, 0.25, 0)
data_cfa34_2$F10_4 <- ifelse(data_cfa34_2$F10_4 == 2, 0.25, 0)
# F10 Summenscore
data_cfa34_2$F10 <- rowSums(select(data_cfa34_2, F10_1, F10_2, F10_3, F10_4))
data_cfa34_2 <- relocate(data_cfa34_2, F10, .after = F10_4)

# F11  dichotomisieren
data_cfa34_2$F11 <- ifelse(data_cfa34_2$F11 == 1, 1, 0)


# F12 k Prim Score
data_cfa34_2$F12_1 <- ifelse(data_cfa34_2$F12_1 == 8, 0.25, 0)
data_cfa34_2$F12_2 <- ifelse(data_cfa34_2$F12_2 == 8, 0.25, 0)
data_cfa34_2$F12a <- ifelse(data_cfa34_2$F12a == "0.72", 0.25, 0)
data_cfa34_2$F12b <- ifelse(data_cfa34_2$F12b == "0.75", 0.25, 0)
# F12 Summenscore
data_cfa34_2$F12 <- rowSums(select(data_cfa34_2, F12_1, F12_2, F12a, F12b))
data_cfa34_2 <- relocate(data_cfa34_2, F12, .after = F12b)

# F13  dichotomisieren
data_cfa34_2$F13 <- ifelse(data_cfa34_2$F13 == 1, 1, 0)


# F14 k Prim Score
data_cfa34_2$F14_1 <- ifelse(data_cfa34_2$F14_1 == 2, 0.25, 0)
data_cfa34_2$F14_2 <- ifelse(data_cfa34_2$F14_2 == 1, 0.25, 0)
data_cfa34_2$F14_3 <- ifelse(data_cfa34_2$F14_3 == 2, 0.25, 0)
data_cfa34_2$F14_4 <- ifelse(data_cfa34_2$F14_4 == 1, 0.25, 0)
# F14 Summenscore
data_cfa34_2$F14 <- rowSums(select(data_cfa34_2, F14_1, F14_2, F14_3, F14_4))
data_cfa34_2 <- relocate(data_cfa34_2, F14, .after = F14_4)


# F15 k Prim Score
data_cfa34_2$F15_1 <- ifelse(data_cfa34_2$F15_1 == 1, 0.25, 0)
data_cfa34_2$F15_2 <- ifelse(data_cfa34_2$F15_2 == 1, 0.25, 0)
data_cfa34_2$F15_3 <- ifelse(data_cfa34_2$F15_3 == 2, 0.25, 0)
data_cfa34_2$F15_4 <- ifelse(data_cfa34_2$F15_4 == 2, 0.25, 0)
# F15 Summenscore
data_cfa34_2$F15 <- rowSums(select(data_cfa34_2, F15_1, F15_2, F15_3, F15_4))
data_cfa34_2 <- relocate(data_cfa34_2, F15, .after = F15_4)

# 1. Termin Krit Scoring in R/F und Summenscore ----

# Variablen in R/F codieren & Summenscore bei k Prim
# F1 k Prim Score
data_Krit_1$F1_1 <- ifelse(data_Krit_1$F1_1 == 1, 0.25, 0)
data_Krit_1$F1_2 <- ifelse(data_Krit_1$F1_2 == 2, 0.25, 0)
data_Krit_1$F1_3 <- ifelse(data_Krit_1$F1_3 == 1, 0.25, 0)
data_Krit_1$F1_4 <- ifelse(data_Krit_1$F1_4 == 2, 0.25, 0)
# F1 Summenscore
data_Krit_1$F1 <- rowSums(select(data_Krit_1, F1_1, F1_2, F1_3, F1_4))
data_Krit_1 <- relocate(data_Krit_1, F1, .after = F1_4)

# F2 dichotomisieren
data_Krit_1$F2 <- ifelse(data_Krit_1$F2 == 4, 1, 0)

# F3  dichotomisieren
data_Krit_1$F3 <- ifelse(data_Krit_1$F3 == 4, 1, 0)

# F4 k Prim Score
data_Krit_1$F4_1 <- ifelse(data_Krit_1$F4_1 == 2, 0.25, 0)
data_Krit_1$F4_2 <- ifelse(data_Krit_1$F4_2 == 1, 0.25, 0)
data_Krit_1$F4_3 <- ifelse(data_Krit_1$F4_3 == 2, 0.25, 0)
data_Krit_1$F4_4 <- ifelse(data_Krit_1$F4_4 == 2, 0.25, 0)
# F4 Summenscore
data_Krit_1$F4 <- rowSums(select(data_Krit_1, F4_1, F4_2, F4_3, F4_4))
data_Krit_1 <- relocate(data_Krit_1, F4, .after = F4_4)

# F5 dichotomisieren
data_Krit_1$F5 <- ifelse(data_Krit_1$F5 == 2, 1, 0)

# F6 k Prim Score
data_Krit_1$F6_1 <- ifelse(data_Krit_1$F6_1 == 1, 0.25, 0)
data_Krit_1$F6_2 <- ifelse(data_Krit_1$F6_2 == 2, 0.25, 0)
data_Krit_1$F6_3 <- ifelse(data_Krit_1$F6_3 == 2, 0.25, 0)
data_Krit_1$F6_4 <- ifelse(data_Krit_1$F6_4 == 1, 0.25, 0)
# F6 Summenscore
data_Krit_1$F6 <- rowSums(select(data_Krit_1, F6_1, F6_2, F6_3, F6_4))
data_Krit_1 <- relocate(data_Krit_1, F6, .after = F6_4)

# F7 k Prim Score
data_Krit_1$F7_1 <- ifelse(data_Krit_1$F7_1 == 1, 0.25, 0)
data_Krit_1$F7_2 <- ifelse(data_Krit_1$F7_2 == 2, 0.25, 0)
data_Krit_1$F7_3 <- ifelse(data_Krit_1$F7_3 == 2, 0.25, 0)
data_Krit_1$F7_4 <- ifelse(data_Krit_1$F7_4 == 2, 0.25, 0)
# F7 Summenscore
data_Krit_1$F7 <- rowSums(select(data_Krit_1, F7_1, F7_2, F7_3, F7_4))
data_Krit_1 <- relocate(data_Krit_1, F7, .after = F7_4)

# F8 k Prim Score
data_Krit_1$F8_1 <- ifelse(data_Krit_1$F8_1 == 2, 0.25, 0)
data_Krit_1$F8_2 <- ifelse(data_Krit_1$F8_2 == 1, 0.25, 0)
data_Krit_1$F8_3 <- ifelse(data_Krit_1$F8_3 == 1, 0.25, 0)
data_Krit_1$F8_4 <- ifelse(data_Krit_1$F8_4 == 2, 0.25, 0)
# F8 Summenscore
data_Krit_1$F8 <- rowSums(select(data_Krit_1, F8_1, F8_2, F8_3, F8_4))
data_Krit_1 <- relocate(data_Krit_1, F8, .after = F8_4)

# F9 k Prim Score
data_Krit_1$F9_1 <- ifelse(data_Krit_1$F9_1 == 2, 0.25, 0)
data_Krit_1$F9_2 <- ifelse(data_Krit_1$F9_2 == 2, 0.25, 0)
data_Krit_1$F9_3 <- ifelse(data_Krit_1$F9_3 == 2, 0.25, 0)
data_Krit_1$F9_4 <- ifelse(data_Krit_1$F9_4 == 2, 0.25, 0)
# F9 Summenscore
data_Krit_1$F9 <- rowSums(select(data_Krit_1, F9_1, F9_2, F9_3, F9_4))
data_Krit_1 <- relocate(data_Krit_1, F9, .after = F9_4)

# F10  dichotomisieren
data_Krit_1$F10 <- ifelse(data_Krit_1$F10 == 1, 1, 0)

# F11 k Prim Score
data_Krit_1$F11_1 <- ifelse(data_Krit_1$F11_1 == 4, 0.25, 0)
data_Krit_1$F11_2 <- ifelse(data_Krit_1$F11_2 == 4, 0.25, 0)
data_Krit_1$F11_3 <- ifelse(data_Krit_1$F11_3 == 4, 0.25, 0)
data_Krit_1$F11_4 <- ifelse(data_Krit_1$F11_4 == 5, 0.25, 0)
# F11 Summenscore
data_Krit_1$F11 <- rowSums(select(data_Krit_1, F11_1, F11_2, F11_3, F11_4))
data_Krit_1 <- relocate(data_Krit_1, F11, .after = F11_4)


# 2. Termin Krit Scoring in R/F und Summenscore ----

# Variablen in R/F codieren & Summenscore bei k Prim
# F1 k Prim Score
data_Krit_2$F1_1 <- ifelse(data_Krit_2$F1_1 == 2, 0.25, 0)
data_Krit_2$F1_2 <- ifelse(data_Krit_2$F1_2 == 2, 0.25, 0)
data_Krit_2$F1_3 <- ifelse(data_Krit_2$F1_3 == 2, 0.25, 0)
data_Krit_2$F1_4 <- ifelse(data_Krit_2$F1_4 == 2, 0.25, 0)
# F1 Summenscore
data_Krit_2$F1 <- rowSums(select(data_Krit_2, F1_1, F1_2, F1_3, F1_4))
data_Krit_2 <- relocate(data_Krit_2, F1, .after = F1_4)

# F2 dichotomisieren
data_Krit_2$F2 <- ifelse(data_Krit_2$F2 == "1,2,3", 1, 0)

# F3 dichotomisieren
data_Krit_2$F3 <- ifelse(data_Krit_2$F3 %in% c("0.70", ".7"), 1, 0)

# F4 k Prim Score
data_Krit_2$F4_1 <- ifelse(data_Krit_2$F4_1 == 2, 0.2, 0)
data_Krit_2$F4_2 <- ifelse(data_Krit_2$F4_2 == 2, 0.2, 0)
data_Krit_2$F4_3 <- ifelse(data_Krit_2$F4_3 == 2, 0.2, 0)
data_Krit_2$F4_4 <- ifelse(data_Krit_2$F4_4 == 2, 0.2, 0)
data_Krit_2$F4a <- ifelse(data_Krit_2$F4a == "6", 0.2, 0)
# F4 Summenscore
data_Krit_2$F4 <- rowSums(select(data_Krit_2, F4_1, F4_2, F4_3, F4_4, F4a))
data_Krit_2 <- relocate(data_Krit_2, F4, .after = F4a)

# F5 k Prim Score
data_Krit_2$F5_1 <- ifelse(data_Krit_2$F5_1 == 4, (1/3), 0)
data_Krit_2$F5_2 <- ifelse(data_Krit_2$F5_2 == 4, (1/3), 0)
data_Krit_2$F5_3 <- ifelse(data_Krit_2$F5_3 == 5, (1/3), 0)
# F5 Summenscore
data_Krit_2$F5 <- rowSums(select(data_Krit_2, F5_1, F5_2, F5_3))
data_Krit_2 <- relocate(data_Krit_2, F5, .after = F5_3)

# F6 dichotomisieren
data_Krit_2$F6 <- ifelse(data_Krit_2$F6 == "0.41", 1, 0)

# F7 k Prim Score
data_Krit_2$F7_1 <- ifelse(data_Krit_2$F7_1 == 2, 0.25, 0)
data_Krit_2$F7_2 <- ifelse(data_Krit_2$F7_2 == 2, 0.25, 0)
data_Krit_2$F7_3 <- ifelse(data_Krit_2$F7_3 == 1, 0.25, 0)
data_Krit_2$F7_4 <- ifelse(data_Krit_2$F7_4 == 2, 0.25, 0)
# F7 Summenscore
data_Krit_2$F7 <- rowSums(select(data_Krit_2, F7_1, F7_2, F7_3, F7_4))
data_Krit_2 <- relocate(data_Krit_2, F7, .after = F7_4)


# F8 k Prim Score
data_Krit_2$F8_1 <- ifelse(data_Krit_2$F8_1 == 2, 0.25, 0)
data_Krit_2$F8_2 <- ifelse(data_Krit_2$F8_2 == 1, 0.25, 0)
data_Krit_2$F8_3 <- ifelse(data_Krit_2$F8_3 == 2, 0.25, 0)
data_Krit_2$F8_4 <- ifelse(data_Krit_2$F8_4 == 2, 0.25, 0)
# F8 Summenscore
data_Krit_2$F8 <- rowSums(select(data_Krit_2, F8_1, F8_2, F8_3, F8_4))
data_Krit_2 <- relocate(data_Krit_2, F8, .after = F8_4)


# F9 k Prim Score
data_Krit_2$F9a <- ifelse(data_Krit_2$F9a == "58", (1/3), 0)
data_Krit_2$F9_1 <- ifelse(data_Krit_2$F9_1 == 2, (1/3), 0)
data_Krit_2$F9_2 <- ifelse(data_Krit_2$F9_2 == 1, (1/3), 0)
# F9 Summenscore
data_Krit_2$F9 <- rowSums(select(data_Krit_2, F9a, F9_1, F9_2))
data_Krit_2 <- relocate(data_Krit_2, F9, .after = F9_2)

# F10 dichotomisieren
data_Krit_2$F10 <- ifelse(data_Krit_2$F10 == 6, 1, 0)

# F11 k Prim Score
data_Krit_2$F11a <- ifelse(data_Krit_2$F11a == "0.81", (1/3), 0)
data_Krit_2$F11_1 <- ifelse(data_Krit_2$F11_1 == 1, (1/3), 0)
data_Krit_2$F11_2 <- ifelse(data_Krit_2$F11_2 == 2, (1/3), 0)
# F11 Summenscore
data_Krit_2$F11 <- rowSums(select(data_Krit_2, F11a, F11_1, F11_2))
data_Krit_2 <- relocate(data_Krit_2, F11, .after = F11_2)





# Funktion die zählt wie eine Person eine Frage geschrieben hat ----
count_comments <- function(data, termin) {
  # num_termin <- paste0("num_fragen_", termin)
  num_termin <- "num_fragen"
  
  data <- data %>% 
    mutate(across(matches(c("A[0-9]", "F[0-9+][FR]")), ~ if_else(nchar(.) > 2, ., NULL))) %>% # Nur Kommentare die länger sind als 2 Zeichen behalten (zum Teil als Kommentar nur eine "1")
    mutate(across(matches(c("A[0-9]", "F[0-9+][FR]")), ~ as.numeric(!is.na(.)))) %>% # alle NA's zu 0 und Kommentare zu 1
    mutate({{ num_termin }} := rowSums(select(., matches(c("A[0-9]", "F[0-9+][FR]")))))  # Variable zeigt wie oft eine Person einen Kommentar gemacht hat
  
  
  # Wie viele Fragen hat eine Person im Fragebogen gestellt?
 
  return(data)
}

# Wie viele Fragen hat eine Person im Fragebogen gestellt? ----
data_sb_1 <- data_sb_1 %>% count_comments("sb_1")
data_sb_2 <- data_sb_2 %>% count_comments("sb_2")
data_irt12_1 <- data_irt12_1 %>% count_comments("irt12_1")
data_irt12_2 <- data_irt12_2 %>% count_comments("irt12_2")
data_irt34_1 <- data_irt34_1 %>% count_comments("irt34_1")
data_irt34_2 <- data_irt34_2 %>% count_comments("irt34_2")
data_cfa12_1 <- data_cfa12_1 %>% count_comments("cfa12_1")
data_cfa12_2 <- data_cfa12_2 %>% count_comments("cfa12_2")
data_cfa34_1 <- data_cfa34_1 %>% count_comments("cfa34_1")
data_cfa34_2 <- data_cfa34_2 %>% count_comments("cfa34_2")
data_Krit_1 <- data_Krit_1 %>% count_comments("Krit_1")
data_Krit_2 <- data_Krit_2 %>% count_comments("Krit_2")

# Funktion die zählt wie of Pseudonym vorkommt ----
count_pseudo <- function(data, termin) {
  #num_termin <- paste0("num_pseudo_", termin)
  num_pseudo <- "num_pseudo"
  
  data <- data %>% 
    mutate(Pseudo = tolower(Pseudo)) %>%
    group_by(Pseudo) %>% # nach Pseudonym gruppieren
    mutate({{ num_pseudo }} := n()) %>%  # Anzahl Fälle in jeder Gruppe in "num_pseudo" speichern
    ungroup() # gruppierung wieder auflösen
  
  return(data)
}

# Wie oft erscheint dasselbe Pseudonym? ---
data_sb_1 <- data_sb_1 %>% count_pseudo("sb_1")
data_sb_2 <- data_sb_2 %>% count_pseudo("sb_2")
data_irt12_1 <- data_irt12_1 %>% count_pseudo("irt12_1")
data_irt12_2 <- data_irt12_2 %>% count_pseudo("irt12_2")
data_irt34_1 <- data_irt34_1 %>% count_pseudo("irt34_1")
data_irt34_2 <- data_irt34_2 %>% count_pseudo("irt34_2")
data_cfa12_1 <- data_cfa12_1 %>% count_pseudo("cfa12_1")
data_cfa12_2 <- data_cfa12_2 %>% count_pseudo("cfa12_2")
data_cfa34_1 <- data_cfa34_1 %>% count_pseudo("cfa34_1")
data_cfa34_2 <- data_cfa34_2 %>% count_pseudo("cfa34_2")
data_Krit_1 <- data_Krit_1 %>% count_pseudo("Krit_1")
data_Krit_2 <- data_Krit_2 %>% count_pseudo("Krit_2")



#rechtzeitig
# Abgabezeitpunkte in DF einfügen
data_sb_1$rechtzeitig <- data_sb_1$EndDate < "2021-09-28 23:59:59"
data_irt12_1$rechtzeitig <- data_irt12_1$EndDate < "2021-10-14 23:59:59"
data_irt34_1$rechtzeitig <- data_irt34_1$EndDate < "2021-10-28 23:59:59"
data_cfa12_1$rechtzeitig <- data_cfa12_1$EndDate < "2021-11-11 23:59:59"
data_cfa34_1$rechtzeitig <- data_cfa34_1$EndDate < "2021-11-25 23:59:59"
data_Krit_1$rechtzeitig <- data_Krit_1$EndDate < "2021-12-09 23:59:59"


data_sb_2$rechtzeitig <- data_sb_2$EndDate < "2021-10-06 23:59:59"
data_irt12_2$rechtzeitig <- data_irt12_2$EndDate < "2021-10-20 23:59:59"
data_irt34_2$rechtzeitig <- data_irt34_2$EndDate < "2021-11-03 23:59:59"
data_cfa12_2$rechtzeitig <- data_cfa12_2$EndDate < "2021-11-17 23:59:59"
data_cfa34_2$rechtzeitig <- data_cfa34_2$EndDate < "2021-12-01 23:59:59"
data_Krit_2$rechtzeitig <- data_Krit_2$EndDate < "2021-12-15 23:59:59"





# Funktion behällt alle relevanten Variablen ----
select_relevant_vars <- function(data) {
  data <- data %>% 
    select(Pseudo,
           EndDate,
           #starts_with("StartDate"),
           #starts_with("EndDate"),
           #starts_with("Progress"),
           starts_with("Duration_seconds"),
           starts_with("oft"),
           matches("F[0-9]"), # alle Fragevariablen auswählen (Summe, Pagesubmit, Clickcount)
           -matches("_[0-9]"), # bei K-Prim werden einzelne Items nicht benötigt
           starts_with("num"), # Anzahl Pseudo und Fragen behalten
           -matches("[0-9][Rabcdef\\.]"), # Antwortvars werden nicht mehr gebraucht
           Vorlesung,
           Pflichtlit,
           Muhe,
           rechtzeitig,
           verstanden)
  
  return(data)
}


# Relevante Variablen behalten ----
data_sb_1 <- data_sb_1 %>% select_relevant_vars()
data_sb_2 <- data_sb_2 %>% select_relevant_vars()
data_irt12_1 <- data_irt12_1 %>% select_relevant_vars()
data_irt12_2 <- data_irt12_2 %>% select_relevant_vars()
data_irt34_1 <- data_irt34_1 %>% select_relevant_vars()
data_irt34_2 <- data_irt34_2 %>% select_relevant_vars()
data_cfa12_1 <- data_cfa12_1 %>% select_relevant_vars()
data_cfa12_2 <- data_cfa12_2 %>% select_relevant_vars()
data_cfa34_1 <- data_cfa34_1 %>% select_relevant_vars()
data_cfa34_2 <- data_cfa34_2 %>% select_relevant_vars()
data_Krit_1 <- data_Krit_1 %>% select_relevant_vars()
data_Krit_2 <- data_Krit_2 %>% select_relevant_vars()

# Funktion die Termin an Variablen hängt, ausser Pseudo ----
add_termin_to_vars <- function(data, termin) {
  names(data) <- if_else(names(data) == "Pseudo", "Pseudo", paste0(names(data), "_", termin))
  
  return(data)
}

# Termine an variablen hängen ----
data_sb_1 <- data_sb_1 %>% add_termin_to_vars("sb_1")
data_sb_2 <- data_sb_2 %>% add_termin_to_vars("sb_2")
data_irt12_1 <- data_irt12_1 %>% add_termin_to_vars("irt12_1")
data_irt12_2 <- data_irt12_2 %>% add_termin_to_vars("irt12_2")
data_irt34_1 <- data_irt34_1 %>% add_termin_to_vars("irt34_1")
data_irt34_2 <- data_irt34_2 %>% add_termin_to_vars("irt34_2")
data_cfa12_1 <- data_cfa12_1 %>% add_termin_to_vars("cfa12_1")
data_cfa12_2 <- data_cfa12_2 %>% add_termin_to_vars("cfa12_2")
data_cfa34_1 <- data_cfa34_1 %>% add_termin_to_vars("cfa34_1")
data_cfa34_2 <- data_cfa34_2 %>% add_termin_to_vars("cfa34_2")
data_Krit_1 <- data_Krit_1 %>% add_termin_to_vars("Krit_1")
data_Krit_2 <- data_Krit_2 %>% add_termin_to_vars("Krit_2")


# 7. Probetest ----
data_Probetest <- data_Probetest %>%
  rename(Duration_seconds_Probe = `Duration (in seconds)`,
         Muhe_Probe = Muhe7,
         Q67 = Q67...52,
         SemesterStrategien = Q65,
         Stunden=Q59
         )

data_Probetest$RF01 <- ifelse(data_Probetest$RF01 == 2, 1, 0)

data_Probetest$RF02 <- ifelse(data_Probetest$RF02 == 1, 1, 0)

data_Probetest$RF03 <- ifelse(data_Probetest$RF03 == 1, 1, 0)

data_Probetest$RF04 <- ifelse(data_Probetest$RF04 == 2, 1, 0)

data_Probetest$RF05 <- ifelse(data_Probetest$RF05 == 2, 1, 0)

data_Probetest$RF06 <- ifelse(data_Probetest$RF06 == 1, 1, 0)

data_Probetest$RF07 <- ifelse(data_Probetest$RF07 == 2, 1, 0)

data_Probetest$RF08 <- ifelse(data_Probetest$RF08 == 1, 1, 0)

data_Probetest$RF09 <- ifelse(data_Probetest$RF09 == 1, 1, 0)

data_Probetest$RF10 <- ifelse(data_Probetest$RF10 == 2, 1, 0)

data_Probetest$RF11 <- ifelse(data_Probetest$RF11 == 1, 1, 0)

data_Probetest$RF12 <- ifelse(data_Probetest$RF12 == 2, 1, 0)

data_Probetest$KF38B <- ifelse(data_Probetest$KF38B == 1, 1, 0)

data_Probetest$KF38A <- ifelse(data_Probetest$KF38A %in% c(0.20, 0.2, .2, .20), 1, 0)

data_Probetest$Q67 <- ifelse(data_Probetest$Q67 %in% c(0.50, 0.5, .5, .50), 1, 0)

data_Probetest$Q69 <- ifelse(data_Probetest$Q69 == 1.811, 1, 0)

data_Probetest$Q70 <- ifelse(data_Probetest$Q70 == 1.5, 1, 0)

data_Probetest$Q71 <- ifelse(data_Probetest$Q71 == 14, 1, 0)

data_Probetest$Q94 <- ifelse(data_Probetest$Q94 == 109.006, 1, 0)

data_Probetest$Q75 <- ifelse(data_Probetest$Q75 == 0.671, 1, 0)

data_Probetest$Q80 <- ifelse(data_Probetest$Q80 == 91.014, 1, 0)

data_Probetest$Q78 <- ifelse(data_Probetest$Q78 %in% c(0.43, .43), 1, 0)

data_Probetest$Q82 <- ifelse(data_Probetest$Q82 == 7.466, 1, 0)

data_Probetest$Q83 <- ifelse(data_Probetest$Q83 %in% c(0.70, 0.7, .7, .70), 1, 0)

data_Probetest$Q84 <- ifelse(data_Probetest$Q84 %in% c(-0.2, -0.3, -0.4, -0.5, -0.6,
                                                       -.2, -.3, -.4, -.5, -.6), 1, 0)




# Summenscore und Prop von Krit
data_Probetest <- data_Probetest %>% 
  mutate(Probetest_sum = rowSums(select(., RF01, RF02, RF03, RF04, RF05, RF06, RF07, RF08,
                                        RF09, RF10, RF11, RF12, KF38A, Q67, Q69, Q70, Q71,
                                        Q94, Q75, Q80, Q78, Q83, Q84), na.rm = TRUE)) #Q82 muss raus, ist nicht dieslebe Variable, KF38B raus, unter Ratewahr



# Datensatz 7 (Probepruefung) Fragen umbenennen
data_Probetest <- data_Probetest %>%
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




data_Probetest <- data_Probetest %>% 
  mutate(Forum = grepl("[3 4 5 ]",data_Probetest$SemesterStrategien))

data_Probetest$Q52
data_Probetest$StefanU<-data_Probetest$Q55...32
a<-data_Probetest$StefanU==8
data_Probetest$StefanU[a]<-6
data_Probetest$StefanU<-7-data_Probetest$StefanU
# Variable Forum_lesen
data_Probetest <- data_Probetest %>% 
  mutate(Forum_lesen = case_when(grepl("[5]", data_Probetest$SemesterStrategien) ~ 2,
                                 grepl("[3 4]", data_Probetest$SemesterStrategien) ~ 1,
                                 TRUE ~ 0))

data_Probetest_save <- data_Probetest 

#data_Probetest <- data_Probetest %>% 
#  filter((Duration_seconds_Probe/60) > 10) %>% 
#  mutate(Pseudo = tolower(Pseudo)) %>% 
#  group_by(Pseudo) %>% 
#  filter(EndDate == min(EndDate)) %>% 
#  select(Pseudo,
#         Probetest_sum,
#         Muhe_Probe,
#         Forum,
#         Punktzahl)


data_Probetest <- data_Probetest%>%
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>%
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration_seconds_Probe/60 > 10)) ~ EndDate == min(EndDate), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()







data_relevanz <- data_sb %>% 
  #filter(Progress == 100) %>% 
  #filter(oft == 1) %>%
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>% 
  #filter(EndDate == min(EndDate)) %>% Wir filtern weiter unten, oder? Zuerst nach Zeit und dann ersten Durchgang
  ungroup() %>% 
  mutate(Relevanz = rowMeans(select(., Q183_1, Q183_2, Q183_3),na.rm = TRUE)) %>% 
  select(Pseudo,Relevanz)









# Sumscore für jeden Termin berechnen ----
data_sb_1 <- data_sb_1 %>% 
  mutate(sb_1_score = rowSums(select(., F1_sb_1, F2_sb_1, F3_sb_1, F3i_sb_1, F3ii_sb_1, F4_sb_1, F5_sb_1, F6_sb_1, F7_sb_1, F8_sb_1, F9_sb_1,
                                     F10_sb_1, F11_sb_1, F12_sb_1, F13_sb_1, F14_sb_1, F16_sb_1, F17_sb_1, F17i_sb_1, F18_sb_1, F19_sb_1, F20_sb_1,
                                     F21_sb_1, F23_sb_1, F24_sb_1, F25_sb_1, F26_sb_1, F27_sb_1), na.rm = TRUE),
         sb_1_score_prop = sb_1_score / length(select(.,F1_sb_1, F2_sb_1, F3_sb_1, F3i_sb_1, F3ii_sb_1, F4_sb_1, F5_sb_1, F6_sb_1, F7_sb_1, F8_sb_1, F9_sb_1,
                                                      F10_sb_1, F11_sb_1, F12_sb_1, F13_sb_1, F14_sb_1, F16_sb_1, F17_sb_1, F17i_sb_1, F18_sb_1, F19_sb_1, F20_sb_1,
                                                      F21_sb_1, F23_sb_1, F24_sb_1, F25_sb_1, F26_sb_1, F27_sb_1)))

data_sb_2 <- data_sb_2 %>% 
  mutate(sb_2_score = rowSums(select(., F1_sb_2, F2_sb_2, F3_sb_2, F3i_sb_2, F3ii_sb_2, F4_sb_2, F5_sb_2, F6_sb_2, F7_sb_2, F8_sb_2, F9_sb_2,
                                     F10_sb_2, F11_sb_2, F12_sb_2, F13_sb_2, F14_sb_2, F16_sb_2, F17_sb_2, F17i_sb_2, F18_sb_2, F19_sb_2, F20_sb_2,
                                     F21_sb_2, F23_sb_2, F24_sb_2, F25_sb_2, F26_sb_2, F27_sb_2), na.rm = TRUE),
         sb_2_score_prop = sb_2_score / length(select(., F1_sb_2, F2_sb_2, F3_sb_2, F3i_sb_2, F3ii_sb_2, F4_sb_2, F5_sb_2, F6_sb_2, F7_sb_2, F8_sb_2, F9_sb_2,
                                                      F10_sb_2, F11_sb_2, F12_sb_2, F13_sb_2, F14_sb_2, F16_sb_2, F17_sb_2, F17i_sb_2, F18_sb_2, F19_sb_2, F20_sb_2,
                                                      F21_sb_2, F23_sb_2, F24_sb_2, F25_sb_2, F26_sb_2, F27_sb_2)))

data_irt12_1 <- data_irt12_1 %>% 
  mutate(irt12_1_score = rowSums(select(., F1_irt12_1, F2_irt12_1, F3_irt12_1, F4_irt12_1, F5_irt12_1, F6_irt12_1, F7_irt12_1,
                                        F9_irt12_1, F10_irt12_1, F11_irt12_1, F12_irt12_1, F13_irt12_1, F14_irt12_1), na.rm = TRUE),
         irt12_1_score_prop = irt12_1_score / length(select(., F1_irt12_1, F2_irt12_1, F3_irt12_1, F4_irt12_1, F5_irt12_1, F6_irt12_1, F7_irt12_1,
                                                            F9_irt12_1, F10_irt12_1, F11_irt12_1, F12_irt12_1, F13_irt12_1, F14_irt12_1)))

data_irt12_2 <- data_irt12_2 %>% 
  mutate(irt12_2_score = rowSums(select(., F1_irt12_2, F2_irt12_2, F3_irt12_2, F4_irt12_2, F5_irt12_2, F6_irt12_2, F7_irt12_2,
                                        F8_irt12_2, F9_irt12_2, F10_irt12_2, F11_irt12_2, F12_irt12_2, F14_irt12_2), na.rm = TRUE),
         irt12_2_score_prop = irt12_2_score / length(select(., F1_irt12_2, F2_irt12_2, F3_irt12_2, F4_irt12_2, F5_irt12_2, F6_irt12_2, F7_irt12_2,
                                                            F8_irt12_2, F9_irt12_2, F10_irt12_2, F11_irt12_2, F12_irt12_2, F14_irt12_2)))

data_irt34_1 <- data_irt34_1 %>% 
  mutate(irt34_1_score = rowSums(select(., F1_irt34_1, F2_irt34_1, F3_irt34_1, F4_irt34_1, F5_irt34_1, F6_irt34_1, F7_irt34_1, F8_irt34_1,
                                        F9_irt34_1, F10_irt34_1, F11_irt34_1, F12_irt34_1), na.rm = TRUE),
         irt34_1_score_prop = irt34_1_score / length(select(., F1_irt34_1, F2_irt34_1, F3_irt34_1, F4_irt34_1, F5_irt34_1, F6_irt34_1, F7_irt34_1, F8_irt34_1,
                                                            F9_irt34_1, F10_irt34_1, F11_irt34_1, F12_irt34_1)))

data_irt34_2 <- data_irt34_2 %>% 
  mutate(irt34_2_score = rowSums(select(., F1_irt34_2, F2_irt34_2, F3_irt34_2, F4_irt34_2, F5_irt34_2, F6_irt34_2, F7_irt34_2, F8_irt34_2,
                                        F9_irt34_2, F10_irt34_2, F11_irt34_2, F12_irt34_2, F13_irt34_2), na.rm = TRUE),
         irt34_2_score_prop = irt34_2_score / length(select(., F1_irt34_2, F2_irt34_2, F3_irt34_2, F4_irt34_2, F5_irt34_2, F6_irt34_2, F7_irt34_2, F8_irt34_2,
                                                            F9_irt34_2, F10_irt34_2, F11_irt34_2, F12_irt34_2, F13_irt34_2)))

data_cfa12_1 <- data_cfa12_1 %>% 
  mutate(cfa12_1_score = rowSums(select(., F1_cfa12_1, F2_cfa12_1, F3_cfa12_1, F4_cfa12_1, F6_cfa12_1, F7_cfa12_1, F8_cfa12_1,
                                        F9_cfa12_1, F10_cfa12_1, F11_cfa12_1, F12_cfa12_1, F13_cfa12_1, F14_cfa12_1, F15_cfa12_1, F16_cfa12_1, F17_cfa12_1), na.rm = TRUE),
         cfa12_1_score_prop = cfa12_1_score / length(select(., F1_cfa12_1, F2_cfa12_1, F3_cfa12_1, F4_cfa12_1, F6_cfa12_1, F7_cfa12_1, F8_cfa12_1,
                                                            F9_cfa12_1, F10_cfa12_1, F11_cfa12_1, F12_cfa12_1, F13_cfa12_1, F14_cfa12_1, F15_cfa12_1, F16_cfa12_1, F17_cfa12_1)))

data_cfa12_2 <- data_cfa12_2 %>% 
  mutate(cfa12_2_score = rowSums(select(., F1_cfa12_2, F2_cfa12_2, F3_cfa12_2, F4_cfa12_2, F5_cfa12_2, F6_cfa12_2, F7_cfa12_2, F8_cfa12_2,
                                        F9_cfa12_2, F10_cfa12_2, F11_cfa12_2, F12_cfa12_2, F14_cfa12_2, F15_cfa12_2, F16_cfa12_2), na.rm = TRUE),
         cfa12_2_score_prop = cfa12_2_score / length(select(., F1_cfa12_2, F2_cfa12_2, F3_cfa12_2, F4_cfa12_2, F5_cfa12_2, F6_cfa12_2, F7_cfa12_2, F8_cfa12_2,
                                                            F9_cfa12_2, F10_cfa12_2, F11_cfa12_2, F12_cfa12_2, F14_cfa12_2, F15_cfa12_2, F16_cfa12_2)))

data_cfa34_1 <- data_cfa34_1 %>% 
  mutate(cfa34_1_score = rowSums(select(., F1_cfa34_1, F2_cfa34_1, F3_cfa34_1, F4_cfa34_1, F5_cfa34_1, F6_cfa34_1, F7_cfa34_1, F8_cfa34_1,
                                        F9_cfa34_1, F10_cfa34_1, F11_cfa34_1, F12_cfa34_1, F13_cfa34_1, F14_cfa34_1, F15_cfa34_1), na.rm = TRUE),
         cfa34_1_score_prop = cfa34_1_score / length(select(., F1_cfa34_1, F2_cfa34_1, F3_cfa34_1, F4_cfa34_1, F5_cfa34_1, F6_cfa34_1, F7_cfa34_1, F8_cfa34_1,
                                                            F9_cfa34_1, F10_cfa34_1, F11_cfa34_1, F12_cfa34_1, F13_cfa34_1, F14_cfa34_1, F15_cfa34_1)))

data_cfa34_2 <- data_cfa34_2 %>% 
  mutate(cfa34_2_score = rowSums(select(., F1_cfa34_2, F2_cfa34_2, F3_cfa34_2, F4_cfa34_2, F5_cfa34_2, F6_cfa34_2, F7_cfa34_2, F8_cfa34_2,
                                        F9_cfa34_2, F10_cfa34_2, F11_cfa34_2, F12_cfa34_2, F13_cfa34_2, F14_cfa34_2, F15_cfa34_2), na.rm = TRUE),
         cfa34_2_score_prop = cfa34_2_score / length(select(., F1_cfa34_2, F2_cfa34_2, F3_cfa34_2, F4_cfa34_2, F5_cfa34_2, F6_cfa34_2, F7_cfa34_2, F8_cfa34_2,
                                                            F9_cfa34_2, F10_cfa34_2, F11_cfa34_2, F12_cfa34_2, F13_cfa34_2, F14_cfa34_2, F15_cfa34_2)))

data_Krit_1 <- data_Krit_1 %>% 
  mutate(Krit_1_score = rowSums(select(., F1_Krit_1, F2_Krit_1, F3_Krit_1, F4_Krit_1, F5_Krit_1, F6_Krit_1, F7_Krit_1, F8_Krit_1,
                                        F9_Krit_1, F10_Krit_1, F11_Krit_1), na.rm = TRUE),
         Krit_1_score_prop = Krit_1_score / length(select(., F1_Krit_1, F2_Krit_1, F3_Krit_1, F4_Krit_1, F5_Krit_1, F6_Krit_1, F7_Krit_1, F8_Krit_1,
                                                          F9_Krit_1, F10_Krit_1, F11_Krit_1)))

data_Krit_2 <- data_Krit_2 %>% 
  mutate(Krit_2_score = rowSums(select(., F1_Krit_2, F2_Krit_2, F3_Krit_2, F4_Krit_2, F5_Krit_2, F6_Krit_2, F7_Krit_2, F8_Krit_2,
                                       F9_Krit_2, F10_Krit_2, F11_Krit_2), na.rm = TRUE),
         Krit_2_score_prop = Krit_2_score / length(select(., F1_Krit_2, F2_Krit_2, F3_Krit_2, F4_Krit_2, F5_Krit_2, F6_Krit_2, F7_Krit_2, F8_Krit_2,
                                                          F9_Krit_2, F10_Krit_2, F11_Krit_2)))


# Duplikate entfernen ----
# einige Schlaumeier haben trotz zweifacher Bearbeitung bei "oft" 1 angegeben.
# Das führt zu Problemen beim Zusammenfügen der Datensätze, da dann in diesen Fällen
# das Pseudonym dieser Personen mehrfach vorhanden ist. 

# um das Problem zu lösen werden zuerste nur die Fälle länger als 10 minuten behalten
# danach werden pro Pseudo nur die Fälle der ersten Bearbeitung behalten

data_sb_1 <- data_sb_1 %>% 
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>%
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration_seconds_sb_1/60 > 10)) ~ EndDate_sb_1 == min(EndDate_sb_1), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()

 
#A<-left_join(data_sb_1,data_relevanz,by="Pseudo") 
# data_sb_1 <- data_sb_1 %>% 
#   filter((Duration_seconds_sb_1/60) > 10) %>% # versuche kürzer 10 Minuten entfernen
#   mutate(Pseudo = tolower(Pseudo)) %>%
#   group_by(Pseudo) %>% 
#   filter(EndDate_sb_1 == min(EndDate_sb_1)) %>% # dann für jede Person "1." Versuch behalten -> da alle Versuche kürzer 10 min raus sind, ev. nicht der wahre 1. Versuch
#   ungroup()


# data_sb_2 <- data_sb_2 %>% 
#   filter((Duration_seconds_sb_2/60) > 10) %>% # versuche kürzer 10 Minuten entfernen
#   mutate(Pseudo = tolower(Pseudo)) %>%
#   group_by(Pseudo) %>% 
#   filter(EndDate_sb_2 == min(EndDate_sb_2)) %>% # dann für jede Person "1." Versuch behalten -> da alle Versuche kürzer 10 min raus sind, ev. nicht der wahre 1. Versuch
#   ungroup()

data_sb_2 <- data_sb_2 %>% 
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>%
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration_seconds_sb_2/60 > 10)) ~ EndDate_sb_2 == min(EndDate_sb_2), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()


data_irt12_1 <- data_irt12_1 %>% 
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>%
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration_seconds_irt12_1/60 > 10)) ~ EndDate_irt12_1 == min(EndDate_irt12_1), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()



# data_irt12_1 <- data_irt12_1 %>% 
#   filter((Duration_seconds_irt12_1/60) > 10) %>% # versuche kürzer 10 Minuten entfernen
#   mutate(Pseudo = tolower(Pseudo)) %>%
#   group_by(Pseudo) %>% 
#   filter(EndDate_irt12_1 == min(EndDate_irt12_1)) %>% # dann für jede Person "1." Versuch behalten -> da alle Versuche kürzer 10 min raus sind, ev. nicht der wahre 1. Versuch
#   ungroup()

data_irt12_2 <- data_irt12_2 %>% 
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>%
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration_seconds_irt12_2/60 > 10)) ~ EndDate_irt12_2 == min(EndDate_irt12_2), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()


# data_irt12_2 <- data_irt12_2 %>% 
#   filter((Duration_seconds_irt12_2/60) > 10) %>% # versuche kürzer 10 Minuten entfernen
#   mutate(Pseudo = tolower(Pseudo)) %>%
#   group_by(Pseudo) %>% 
#   filter(EndDate_irt12_2 == min(EndDate_irt12_2)) %>% # dann für jede Person "1." Versuch behalten -> da alle Versuche kürzer 10 min raus sind, ev. nicht der wahre 1. Versuch
#   ungroup()


data_irt34_1 <- data_irt34_1 %>% 
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>%
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration_seconds_irt34_1/60 > 10)) ~ EndDate_irt34_1 == min(EndDate_irt34_1), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()

data_irt34_2 <- data_irt34_2 %>% 
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>%
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration_seconds_irt34_2/60 > 10)) ~ EndDate_irt34_2 == min(EndDate_irt34_2), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()


# data_irt34_1 <- data_irt34_1 %>% 
#   filter((Duration_seconds_irt34_1/60) > 10) %>% # versuche kürzer 10 Minuten entfernen
#   mutate(Pseudo = tolower(Pseudo)) %>%
#   group_by(Pseudo) %>% 
#   filter(EndDate_irt34_1 == min(EndDate_irt34_1)) %>% # dann für jede Person "1." Versuch behalten -> da alle Versuche kürzer 10 min raus sind, ev. nicht der wahre 1. Versuch
#   ungroup()
# 
# data_irt34_2 <- data_irt34_2 %>% 
#   filter((Duration_seconds_irt34_2/60) > 10) %>% # versuche kürzer 10 Minuten entfernen
#   mutate(Pseudo = tolower(Pseudo)) %>%
#   group_by(Pseudo) %>% 
#   filter(EndDate_irt34_2 == min(EndDate_irt34_2)) %>% # dann für jede Person "1." Versuch behalten -> da alle Versuche kürzer 10 min raus sind, ev. nicht der wahre 1. Versuch
#   ungroup()


data_cfa12_1 <- data_cfa12_1 %>% 
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>%
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration_seconds_cfa12_1/60 > 10)) ~ EndDate_cfa12_1 == min(EndDate_cfa12_1), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()


data_cfa12_2 <- data_cfa12_2 %>% 
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>%
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration_seconds_cfa12_2/60 > 10)) ~ EndDate_cfa12_2 == min(EndDate_cfa12_2), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()

data_cfa34_1 <- data_cfa34_1 %>% 
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>%
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration_seconds_cfa34_1/60 > 10)) ~ EndDate_cfa34_1 == min(EndDate_cfa34_1), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()

data_cfa34_2 <- data_cfa34_2 %>% 
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>%
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration_seconds_cfa34_2/60 > 10)) ~ EndDate_cfa34_2 == min(EndDate_cfa34_2), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()


# data_cfa12_1 <- data_cfa12_1 %>% 
#   filter((Duration_seconds_cfa12_1/60) > 10) %>% # versuche kürzer 10 Minuten entfernen
#   mutate(Pseudo = tolower(Pseudo)) %>%
#   group_by(Pseudo) %>% 
#   filter(EndDate_cfa12_1 == min(EndDate_cfa12_1)) %>% # dann für jede Person "1." Versuch behalten -> da alle Versuche kürzer 10 min raus sind, ev. nicht der wahre 1. Versuch
#   ungroup()
# 
# data_cfa12_2 <- data_cfa12_2 %>% 
#   filter((Duration_seconds_cfa12_2/60) > 10) %>% # versuche kürzer 10 Minuten entfernen
#   mutate(Pseudo = tolower(Pseudo)) %>%
#   group_by(Pseudo) %>% 
#   filter(EndDate_cfa12_2 == min(EndDate_cfa12_2)) %>% # dann für jede Person "1." Versuch behalten -> da alle Versuche kürzer 10 min raus sind, ev. nicht der wahre 1. Versuch
#   ungroup()
# 
# data_cfa34_1 <- data_cfa34_1 %>% 
#   filter((Duration_seconds_cfa34_1/60) > 10) %>% # versuche kürzer 10 Minuten entfernen
#   mutate(Pseudo = tolower(Pseudo)) %>%
#   group_by(Pseudo) %>% 
#   filter(EndDate_cfa34_1 == min(EndDate_cfa34_1)) %>% # dann für jede Person "1." Versuch behalten -> da alle Versuche kürzer 10 min raus sind, ev. nicht der wahre 1. Versuch
#   ungroup()
# 
# data_cfa34_2 <- data_cfa34_2 %>% 
#   filter((Duration_seconds_cfa34_2/60) > 10) %>% # versuche kürzer 10 Minuten entfernen
#   mutate(Pseudo = tolower(Pseudo)) %>%
#   group_by(Pseudo) %>% 
#   filter(EndDate_cfa34_2 == min(EndDate_cfa34_2)) %>% # dann für jede Person "1." Versuch behalten -> da alle Versuche kürzer 10 min raus sind, ev. nicht der wahre 1. Versuch
#   ungroup()





# 
# data_Krit_1 <- data_Krit_1 %>% 
#   filter((Duration_seconds_Krit_1/60) > 10) %>% # versuche kürzer 10 Minuten entfernen
#   mutate(Pseudo = tolower(Pseudo)) %>%
#   group_by(Pseudo) %>% 
#   filter(EndDate_Krit_1 == min(EndDate_Krit_1)) %>% # dann für jede Person "1." Versuch behalten -> da alle Versuche kürzer 10 min raus sind, ev. nicht der wahre 1. Versuch
#   ungroup()
# 
# data_Krit_2 <- data_Krit_2 %>% 
#   filter((Duration_seconds_Krit_2/60) > 10) %>% # versuche kürzer 10 Minuten entfernen
#   mutate(Pseudo = tolower(Pseudo)) %>%
#   group_by(Pseudo) %>% 
#   filter(EndDate_Krit_2 == min(EndDate_Krit_2)) %>% # dann für jede Person "1." Versuch behalten -> da alle Versuche kürzer 10 min raus sind, ev. nicht der wahre 1. Versuch
#   ungroup()

data_Krit_1 <- data_Krit_1 %>%
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>%
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration_seconds_Krit_1/60 > 10)) ~ EndDate_Krit_1 == min(EndDate_Krit_1), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()


data_Krit_2 <- data_Krit_2 %>%
  mutate(Pseudo = tolower(Pseudo)) %>%
  group_by(Pseudo) %>%
  mutate(n = n()) %>% # count n per pseudo
  filter(case_when(((n > 1) & (Duration_seconds_Krit_2/60 > 10)) ~ EndDate_Krit_2 == min(EndDate_Krit_2), # if count > 1 keep oldest try (furthest in the past)
                   n == 1 ~ TRUE)) %>% # else keep the only present try
  ungroup()


data_relevanz<-data_relevanz[!duplicated(data_relevanz$Pseudo),]


# Datensatzt zusammenfassen ----
data_final <- full_join(data_sb_1, data_sb_2, by = "Pseudo") %>%
  full_join(data_irt12_1, by = "Pseudo") %>% 
  full_join(data_irt12_2, by = "Pseudo") %>% 
  full_join(data_irt34_1, by = "Pseudo") %>% 
  full_join(data_irt34_2, by = "Pseudo") %>% 
  full_join(data_cfa12_1, by = "Pseudo") %>% 
  full_join(data_cfa12_2, by = "Pseudo") %>%
  full_join(data_cfa34_1, by = "Pseudo") %>%
  full_join(data_cfa34_2, by = "Pseudo") %>%
  full_join(data_Krit_1, by = "Pseudo") %>%
  full_join(data_Krit_2, by = "Pseudo") %>% 
  full_join(data_Probetest, by = "Pseudo") %>% 
  full_join(data_relevanz, by = "Pseudo") %>% ##bei relevanz muss ich noch duplikate loswerden
  ungroup()




data_final$Pflichtlit_irt12_1[data_final$Pflichtlit_irt12_1 == 1] <- 0
data_final$Pflichtlit_irt12_1[data_final$Pflichtlit_irt12_1 > 1] <- 1

data_final$Pflichtlit_irt34_1[data_final$Pflichtlit_irt34_1 == 1] <- 0
data_final$Pflichtlit_irt34_1[data_final$Pflichtlit_irt34_1 > 1] <- 1

data_final$Pflichtlit_cfa12_1[data_final$Pflichtlit_cfa12_1 == 1] <- 0
data_final$Pflichtlit_cfa12_1[data_final$Pflichtlit_cfa12_1 > 1] <- 1

data_final$Pflichtlit_cfa34_1[data_final$Pflichtlit_cfa34_1 == 1] <- 0
data_final$Pflichtlit_cfa34_1[data_final$Pflichtlit_cfa34_1 > 1] <- 1

data_final$Pflichtlit_Krit_1[data_final$Pflichtlit_Krit_1 == 1] <- 0
data_final$Pflichtlit_Krit_1[data_final$Pflichtlit_Krit_1 > 1] <- 1



write_csv(data_final, "data_final_2021.csv")

data_2021_2_termine <- data_final %>%
  select(Pseudo,
         ends_with("2_score"),
         num_pseudo_sb_2, num_pseudo_irt12_2, num_pseudo_irt34_2,
         num_pseudo_cfa12_2, num_pseudo_cfa34_2, num_pseudo_Krit_2)
write_csv(data_2021_2_termine, "data_2021_2_termine.csv")
