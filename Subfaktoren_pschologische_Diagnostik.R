library(tidyverse)
data_final <- read_csv("data_final_2021.csv")

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



# Relevante Faktor-Variablen (Summenscore) erstellen (sb_1) Standortbestimmung -----
# 1. Theoretische Grundlagen / 2. Berechnung / 3. Individualdiagnostik
data_final$theory_1 <- rowSums(select(data_final, F2_sb_1, F3_sb_1, F3i_sb_1, F4_sb_1, F5_sb_1, F8_sb_1,
                                     F9_sb_1, F11_sb_1, F14_sb_1, F18_sb_1, F19_sb_1, F21_sb_1)) # In Qualtrics ist F25 auch in Theorie, diese gehört aber ziemlich sicher in die Individualdiagnostik

data_final$practice_1 <- rowSums(select(data_final, F1_sb_1, F6_sb_1, F7_sb_1, F10_sb_1, F12_sb_1, F13_sb_1,
                                       F16_sb_1, F17_sb_1, F17i_sb_1, F20_sb_1))

data_final$indi_1 <- rowSums(select(data_final, F23_sb_1, F24_sb_1, F25_sb_1, F26_sb_1, F27_sb_1))


# Vektor mit Variablen die geglättet werden sollen (sb_1 Page Submit) -------------
names_sb_1_Page_Submit <- names(select(data_final, F2_Page_Submit_sb_1,F3_Page_Submit_sb_1,F3i_Page_Submit_sb_1,
                                       F4_Page_Submit_sb_1,F5_Page_Submit_sb_1,F8_Page_Submit_sb_1,
                                       F9_Page_Submit_sb_1,F11_Page_Submit_sb_1,F14_Page_Submit_sb_1,
                                       F18_Page_Submit_sb_1,F19_Page_Submit_sb_1,F21_Page_Submit_sb_1,
                                       F25_Page_Submit_sb_1,F1_Page_Submit_sb_1,
                                       F10_Page_Submit_sb_1,F12_Page_Submit_sb_1,F13_Page_Submit_sb_1,
                                       F16_Page_Submit_sb_1,F17_Page_Submit_sb_1,F17i_Page_Submit_sb_1,
                                       F20_Page_Submit_sb_1,F23_Page_Submit_sb_1,F24_Page_Submit_sb_1,
                                       F26_Page_Submit_sb_1,F27_Page_Submit_sb_1))

# Ausgewählte Variablen (Page Submit sb_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(all_of(names_sb_1_Page_Submit))


# Relevante Variablen erstellen mit geglätteten Reaktionszeiten
data_final$BerechnungenRZ_1 <- rowSums(select(data_final,F12_Page_Submit_sb_1,F13_Page_Submit_sb_1,F20_Page_Submit_sb_1,F23_Page_Submit_sb_1,F24_Page_Submit_sb_1,
                                          F26_Page_Submit_sb_1), na.rm = T)

data_final$TheorieRZ_1 <- rowSums(select(data_final, F2_Page_Submit_sb_1,F3_Page_Submit_sb_1,F3i_Page_Submit_sb_1,
                                     F4_Page_Submit_sb_1,F5_Page_Submit_sb_1,F8_Page_Submit_sb_1,
                                     F9_Page_Submit_sb_1,F11_Page_Submit_sb_1,F14_Page_Submit_sb_1,F18_Page_Submit_sb_1,
                                     F19_Page_Submit_sb_1,F21_Page_Submit_sb_1,
                                     F25_Page_Submit_sb_1))



data_final$theoryRZ_1 <- rowSums(select(data_final, F2_Page_Submit_sb_1,F3_Page_Submit_sb_1,F3i_Page_Submit_sb_1,
                                       F4_Page_Submit_sb_1,F5_Page_Submit_sb_1,F8_Page_Submit_sb_1,
                                       F9_Page_Submit_sb_1,F11_Page_Submit_sb_1,F14_Page_Submit_sb_1,
                                       F18_Page_Submit_sb_1,F19_Page_Submit_sb_1,F21_Page_Submit_sb_1))

data_final$practiceRZ_1 <- rowSums(select(data_final, F1_Page_Submit_sb_1,
                                         F10_Page_Submit_sb_1,F12_Page_Submit_sb_1,F13_Page_Submit_sb_1,
                                         F16_Page_Submit_sb_1,F17_Page_Submit_sb_1,F17i_Page_Submit_sb_1,
                                         F20_Page_Submit_sb_1), na.rm = T)

data_final$indiRZ_1 <- rowSums(select(data_final, F23_Page_Submit_sb_1,F24_Page_Submit_sb_1, F25_Page_Submit_sb_1,
                                     F26_Page_Submit_sb_1,F27_Page_Submit_sb_1))


# Vektor mit Variablen die geglättet werden sollen (sb_1 Click Count) -------------
names_sb_1_Click_Count <- names(select(data_final, F2_Click_Count_sb_1,F3_Click_Count_sb_1,F3i_Click_Count_sb_1,
                                       F4_Click_Count_sb_1,F5_Click_Count_sb_1,F8_Click_Count_sb_1,
                                       F9_Click_Count_sb_1,F11_Click_Count_sb_1,F14_Click_Count_sb_1,
                                       F18_Click_Count_sb_1,F19_Click_Count_sb_1,F21_Click_Count_sb_1,
                                       F25_Click_Count_sb_1,F1_Click_Count_sb_1,
                                       F10_Click_Count_sb_1,F12_Click_Count_sb_1,F13_Click_Count_sb_1,
                                       F16_Click_Count_sb_1,F17_Click_Count_sb_1,F17i_Click_Count_sb_1,
                                       F20_Click_Count_sb_1,F23_Click_Count_sb_1,F24_Click_Count_sb_1,
                                       F26_Click_Count_sb_1,F27_Click_Count_sb_1))

# Ausgewählte Variablen (Click Count sb_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(all_of(names_sb_1_Click_Count))


# Relevante Variablen erstellen mit geglätteten Click Count erstllen
data_final$theoryKlick_1 <- rowSums(select(data_final, F2_Click_Count_sb_1,F3_Click_Count_sb_1,F3i_Click_Count_sb_1,
                                          F4_Click_Count_sb_1,F5_Click_Count_sb_1,F8_Click_Count_sb_1,
                                          F9_Click_Count_sb_1,F11_Click_Count_sb_1,F14_Click_Count_sb_1,
                                          F18_Click_Count_sb_1,F19_Click_Count_sb_1,F21_Click_Count_sb_1))

data_final$practiceKlick_1 <- rowSums(select(data_final, F1_Click_Count_sb_1,
                                            F10_Click_Count_sb_1,F12_Click_Count_sb_1,F13_Click_Count_sb_1,
                                            F16_Click_Count_sb_1,F17_Click_Count_sb_1,F17i_Click_Count_sb_1,
                                            F20_Click_Count_sb_1))

data_final$indiKlick_1 <- rowSums(select(data_final, F23_Click_Count_sb_1,F24_Click_Count_sb_1, F25_Click_Count_sb_1,
                                        F26_Click_Count_sb_1,F27_Click_Count_sb_1))



# Vektor mit Variablen die geglättet werden sollen (A FF Page Submit) -------------
names_sb_1_FFPage_Submit <- names(select(data_final, FF2_Page_Submit_sb_1,FF3_Page_Submit_sb_1,FF3i_Page_Submit_sb_1,
                                         FF4_Page_Submit_sb_1,FF5_Page_Submit_sb_1,
                                         FF9_Page_Submit_sb_1,FF11_Page_Submit_sb_1,FF14_Page_Submit_sb_1,
                                         FF18_Page_Submit_sb_1,FF19_Page_Submit_sb_1,FF21_Page_Submit_sb_1,
                                         FF25_Page_Submit_sb_1,FF1_Page_Submit_sb_1,FF12_Page_Submit_sb_1,
                                         FF16_Page_Submit_sb_1,FF17_Page_Submit_sb_1,FF17i_Page_Submit_sb_1,
                                         FF20_Page_Submit_sb_1,FF23_Page_Submit_sb_1,FF24_Page_Submit_sb_1,
                                         FF26_Page_Submit_sb_1,FF27_Page_Submit_sb_1))

# Ausgewählte Variablen (FF Page Submit des des "A" Teils) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_sb_1_FFPage_Submit)

# Relevante Variablen erstellen mit geglätteten Reaktionszeiten erstllen

data_final$BerechnungenFRZ_1 <- rowSums(select(data_final,FF12_Page_Submit_sb_1,FF20_Page_Submit_sb_1,FF23_Page_Submit_sb_1,FF24_Page_Submit_sb_1,
                                              FF26_Page_Submit_sb_1), na.rm = T)

data_final$TheorieFRZ_1 <- rowSums(select(data_final, FF2_Page_Submit_sb_1,FF3_Page_Submit_sb_1,FF3i_Page_Submit_sb_1,
                                         FF4_Page_Submit_sb_1,FF5_Page_Submit_sb_1,FF8_Page_Submit_sb_1,
                                         FF9_Page_Submit_sb_1,FF11_Page_Submit_sb_1,FF14_Page_Submit_sb_1,FF18_Page_Submit_sb_1,
                                         FF19_Page_Submit_sb_1,FF21_Page_Submit_sb_1,
                                         FF25_Page_Submit_sb_1))



data_final$theoryFRZ_1 <- rowSums(select(data_final, FF2_Page_Submit_sb_1,FF3_Page_Submit_sb_1,FF3i_Page_Submit_sb_1,
                                        FF4_Page_Submit_sb_1,FF5_Page_Submit_sb_1,
                                        FF9_Page_Submit_sb_1,FF11_Page_Submit_sb_1,FF14_Page_Submit_sb_1,
                                        FF18_Page_Submit_sb_1,FF19_Page_Submit_sb_1,FF21_Page_Submit_sb_1))

data_final$practiceFRZ_1 <- rowSums(select(data_final, FF1_Page_Submit_sb_1,
                                          FF12_Page_Submit_sb_1,
                                          FF16_Page_Submit_sb_1,FF17_Page_Submit_sb_1,FF17i_Page_Submit_sb_1,
                                          FF20_Page_Submit_sb_1))

data_final$indiFRZ_1 <- rowSums(select(data_final, FF23_Page_Submit_sb_1,FF24_Page_Submit_sb_1, FF25_Page_Submit_sb_1,
                                      FF26_Page_Submit_sb_1,FF27_Page_Submit_sb_1))


# Vektor mit Variablen die geglättet werden sollen (A FF Click Count) -------------
names_sb_1_FFClick_Count <- names(select(data_final,FF2_Click_Count_sb_1,FF3_Click_Count_sb_1,FF3i_Click_Count_sb_1,
                                         FF4_Click_Count_sb_1,FF5_Click_Count_sb_1,
                                         FF9_Click_Count_sb_1,FF11_Click_Count_sb_1,FF14_Click_Count_sb_1,
                                         FF18_Click_Count_sb_1,FF19_Click_Count_sb_1,FF21_Click_Count_sb_1,
                                         FF25_Click_Count_sb_1,FF1_Click_Count_sb_1,
                                         FF12_Click_Count_sb_1,
                                         FF16_Click_Count_sb_1,FF17_Click_Count_sb_1,FF17i_Click_Count_sb_1,
                                         FF20_Click_Count_sb_1,FF23_Click_Count_sb_1,FF24_Click_Count_sb_1,
                                         FF26_Click_Count_sb_1,FF27_Click_Count_sb_1))

# Ausgewählte Variablen (FF Click Count des des "A" Teils) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_sb_1_FFClick_Count)

# Relevante Variablen erstellen mit geglätteten Click Count erstllen
data_final$theoryFKlick_1 <- rowSums(select(data_final, FF2_Click_Count_sb_1,FF3_Click_Count_sb_1,FF3i_Click_Count_sb_1,
                                           FF4_Click_Count_sb_1,FF5_Click_Count_sb_1,
                                           FF9_Click_Count_sb_1,FF11_Click_Count_sb_1,FF14_Click_Count_sb_1,
                                           FF18_Click_Count_sb_1,FF19_Click_Count_sb_1,FF21_Click_Count_sb_1))

data_final$practiceFKlick_1 <- rowSums(select(data_final, FF1_Click_Count_sb_1,
                                             FF12_Click_Count_sb_1,
                                             FF16_Click_Count_sb_1,FF17_Click_Count_sb_1,FF17i_Click_Count_sb_1,
                                             FF20_Click_Count_sb_1))

data_final$indiFKlick_1 <- rowSums(select(data_final, FF23_Click_Count_sb_1,FF24_Click_Count_sb_1, FF25_Click_Count_sb_1,
                                         FF26_Click_Count_sb_1,FF27_Click_Count_sb_1))














# Relevante Faktor-Variablen (Summenscore) erstellen (sb_2) Standortbestimmung -----
# 1. Theoretische Grundlagen / 2. Berechnung / 3. Individualdiagnostik
data_final$theory_2 <- rowSums(select(data_final, F2_sb_2, F3_sb_2, F3i_sb_2, F4_sb_2, F5_sb_2, F8_sb_2,
                                     F9_sb_2, F11_sb_2, F14_sb_2, F18_sb_2, F19_sb_2, F21_sb_2)) # In Qualtrics ist F25 auch in Theorie, diese gehört aber ziemlich sicher in die Individualdiagnostik

data_final$practice_2 <- rowSums(select(data_final, F1_sb_2, F6_sb_2, F7_sb_2, F10_sb_2, F12_sb_2, F13_sb_2,
                                       F16_sb_2, F17_sb_2, F17i_sb_2, F20_sb_2))

data_final$indi_2 <- rowSums(select(data_final, F23_sb_2, F24_sb_2, F25_sb_2, F26_sb_2, F27_sb_2))


# Vektor mit Variablen die geglättet werden sollen (sb_2 Page Submit) -------------
names_sb_2_Page_Submit <- names(select(data_final, F2_Page_Submit_sb_2,F3_Page_Submit_sb_2,F3i_Page_Submit_sb_2,
                                       F4_Page_Submit_sb_2,F5_Page_Submit_sb_2,F8_Page_Submit_sb_2,
                                       F9_Page_Submit_sb_2,F11_Page_Submit_sb_2,F14_Page_Submit_sb_2,
                                       F18_Page_Submit_sb_2,F19_Page_Submit_sb_2,F21_Page_Submit_sb_2,
                                       F25_Page_Submit_sb_2,F1_Page_Submit_sb_2,
                                       F10_Page_Submit_sb_2,F12_Page_Submit_sb_2,F13_Page_Submit_sb_2,
                                       F16_Page_Submit_sb_2,F17_Page_Submit_sb_2,F17i_Page_Submit_sb_2,
                                       F20_Page_Submit_sb_2,F23_Page_Submit_sb_2,F24_Page_Submit_sb_2,
                                       F26_Page_Submit_sb_2,F27_Page_Submit_sb_2))

# Ausgewählte Variablen (Page Submit sb_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(all_of(names_sb_2_Page_Submit))


# Relevante Variablen erstellen mit geglätteten Reaktionszeiten
data_final$theoryRZ_2 <- rowSums(select(data_final,F3_Page_Submit_sb_2,F3i_Page_Submit_sb_2,
                                       F4_Page_Submit_sb_2,F5_Page_Submit_sb_2,F8_Page_Submit_sb_2,
                                       F9_Page_Submit_sb_2,F11_Page_Submit_sb_2,F14_Page_Submit_sb_2,
                                       F18_Page_Submit_sb_2,F19_Page_Submit_sb_2,F21_Page_Submit_sb_2))

data_final$practiceRZ_2 <- rowSums(select(data_final, F1_Page_Submit_sb_2,
                                         F10_Page_Submit_sb_2,F12_Page_Submit_sb_2,F13_Page_Submit_sb_2,
                                         F16_Page_Submit_sb_2,F17_Page_Submit_sb_2,F17i_Page_Submit_sb_2,
                                         F20_Page_Submit_sb_2), na.rm = T)

data_final$indiRZ_2 <- rowSums(select(data_final, F23_Page_Submit_sb_2,F24_Page_Submit_sb_2, F25_Page_Submit_sb_2,
                                     F26_Page_Submit_sb_2,F27_Page_Submit_sb_2))


# Vektor mit Variablen die geglättet werden sollen (sb_2 Click Count) -------------
names_sb_2_Click_Count <- names(select(data_final, F2_Click_Count_sb_2,F3_Click_Count_sb_2,F3i_Click_Count_sb_2,
                                       F4_Click_Count_sb_2,F5_Click_Count_sb_2,F8_Click_Count_sb_2,
                                       F9_Click_Count_sb_2,F11_Click_Count_sb_2,F14_Click_Count_sb_2,
                                       F18_Click_Count_sb_2,F19_Click_Count_sb_2,F21_Click_Count_sb_2,
                                       F25_Click_Count_sb_2,F1_Click_Count_sb_2,
                                       F10_Click_Count_sb_2,F12_Click_Count_sb_2,F13_Click_Count_sb_2,
                                       F16_Click_Count_sb_2,F17_Click_Count_sb_2,F17i_Click_Count_sb_2,
                                       F20_Click_Count_sb_2,F23_Click_Count_sb_2,F24_Click_Count_sb_2,
                                       F26_Click_Count_sb_2,F27_Click_Count_sb_2))

# Ausgewählte Variablen (Click Count sb_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(all_of(names_sb_2_Click_Count))


# Relevante Variablen erstellen mit geglätteten Click Count erstllen
data_final$theoryKlick_2 <- rowSums(select(data_final, F2_Click_Count_sb_2,F3_Click_Count_sb_2,F3i_Click_Count_sb_2,
                                          F4_Click_Count_sb_2,F5_Click_Count_sb_2,F8_Click_Count_sb_2,
                                          F9_Click_Count_sb_2,F11_Click_Count_sb_2,F14_Click_Count_sb_2,
                                          F18_Click_Count_sb_2,F19_Click_Count_sb_2,F21_Click_Count_sb_2))

data_final$practiceKlick_2 <- rowSums(select(data_final, F1_Click_Count_sb_2,
                                            F10_Click_Count_sb_2,F12_Click_Count_sb_2,F13_Click_Count_sb_2,
                                            F16_Click_Count_sb_2,F17_Click_Count_sb_2,F17i_Click_Count_sb_2,
                                            F20_Click_Count_sb_2))

data_final$indiKlick_2 <- rowSums(select(data_final, F23_Click_Count_sb_2,F24_Click_Count_sb_2, F25_Click_Count_sb_2,
                                        F26_Click_Count_sb_2,F27_Click_Count_sb_2))



# Vektor mit Variablen die geglättet werden sollen (sb_2 FF Page Submit) -------------
names_sb_2_FFPage_Submit <- names(select(data_final, FF2_Page_Submit_sb_2,FF3_Page_Submit_sb_2,FF3i_Page_Submit_sb_2,
                                         FF4_Page_Submit_sb_2,FF5_Page_Submit_sb_2, FF8_Page_Submit_sb_2,
                                         FF9_Page_Submit_sb_2,FF11_Page_Submit_sb_2,FF14_Page_Submit_sb_2,
                                         FF18_Page_Submit_sb_2,FF19_Page_Submit_sb_2,FF21_Page_Submit_sb_2,
                                         FF25_Page_Submit_sb_2,FF1_Page_Submit_sb_2,FF12_Page_Submit_sb_2,
                                         FF16_Page_Submit_sb_2,FF17_Page_Submit_sb_2,FF17i_Page_Submit_sb_2,
                                         FF20_Page_Submit_sb_2,FF23_Page_Submit_sb_2,FF24_Page_Submit_sb_2,
                                         FF26_Page_Submit_sb_2,FF27_Page_Submit_sb_2))

# Ausgewählte Variablen (FF Page Submit sb_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_sb_2_FFPage_Submit)

# Relevante Variablen erstellen mit geglätteten Reaktionszeiten erstllen

data_final$theoryFRZ_2 <- rowSums(select(data_final, FF2_Page_Submit_sb_2,FF3_Page_Submit_sb_2,FF3i_Page_Submit_sb_2,
                                        FF4_Page_Submit_sb_2,FF5_Page_Submit_sb_2, FF8_Page_Submit_sb_2,
                                        FF9_Page_Submit_sb_2,FF11_Page_Submit_sb_2,FF14_Page_Submit_sb_2,
                                        FF18_Page_Submit_sb_2,FF19_Page_Submit_sb_2,FF21_Page_Submit_sb_2))

data_final$practiceFRZ_2 <- rowSums(select(data_final, FF1_Page_Submit_sb_2,
                                          FF12_Page_Submit_sb_2,
                                          FF16_Page_Submit_sb_2,FF17_Page_Submit_sb_2,FF17i_Page_Submit_sb_2,
                                          FF20_Page_Submit_sb_2))

data_final$indiFRZ_2 <- rowSums(select(data_final, FF23_Page_Submit_sb_2,FF24_Page_Submit_sb_2, FF25_Page_Submit_sb_2,
                                      FF26_Page_Submit_sb_2,FF27_Page_Submit_sb_2))


# Vektor mit Variablen die geglättet werden sollen (sb_2 FF Click Count) -------------
names_sb_2_FFClick_Count <- names(select(data_final,FF2_Click_Count_sb_2,FF3_Click_Count_sb_2,FF3i_Click_Count_sb_2,
                                         FF4_Click_Count_sb_2,FF5_Click_Count_sb_2, FF8_Click_Count_sb_2,
                                         FF9_Click_Count_sb_2,FF11_Click_Count_sb_2,FF14_Click_Count_sb_2,
                                         FF18_Click_Count_sb_2,FF19_Click_Count_sb_2,FF21_Click_Count_sb_2,
                                         FF25_Click_Count_sb_2,FF1_Click_Count_sb_2,
                                         FF12_Click_Count_sb_2,
                                         FF16_Click_Count_sb_2,FF17_Click_Count_sb_2,FF17i_Click_Count_sb_2,
                                         FF20_Click_Count_sb_2,FF23_Click_Count_sb_2,FF24_Click_Count_sb_2,
                                         FF26_Click_Count_sb_2,FF27_Click_Count_sb_2))

# Ausgewählte Variablen (FF Click Count sb_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_sb_2_FFClick_Count)

# Relevante Variablen erstellen mit geglätteten Click Count erstllen
data_final$theoryFKlick_2 <- rowSums(select(data_final, FF2_Click_Count_sb_2,FF3_Click_Count_sb_2,FF3i_Click_Count_sb_2,
                                           FF4_Click_Count_sb_2,FF5_Click_Count_sb_2, FF8_Click_Count_sb_2,
                                           FF9_Click_Count_sb_2,FF11_Click_Count_sb_2,FF14_Click_Count_sb_2,
                                           FF18_Click_Count_sb_2,FF19_Click_Count_sb_2,FF21_Click_Count_sb_2))

data_final$practiceFKlick_2 <- rowSums(select(data_final, FF1_Click_Count_sb_2,
                                             FF12_Click_Count_sb_2,
                                             FF16_Click_Count_sb_2,FF17_Click_Count_sb_2,FF17i_Click_Count_sb_2,
                                             FF20_Click_Count_sb_2))

data_final$indiFKlick_2 <- rowSums(select(data_final, FF23_Click_Count_sb_2,FF24_Click_Count_sb_2, FF25_Click_Count_sb_2,
                                         FF26_Click_Count_sb_2,FF27_Click_Count_sb_2))


















# Relevante Faktor-Variablen (Summenscore) erstellen (Teil irt12_1) IRT1/2 -----
# 1. Lokale Stoachastische Unabh. / 2. ICC / 3. Parameterschätzung / 4. Spez. Objektivität
data_final <- data_final %>%
  mutate(lokalestoch_1 = rowSums(select(., F1_irt12_1, F2_irt12_1, F3_irt12_1)),
         
         rest_1 = rowSums(select(., F4_irt12_1, F5_irt12_1, F6_irt12_1, F7_irt12_1, F9_irt12_1,
                                 F10_irt12_1, F11_irt12_1, F12_irt12_1, F13_irt12_1, F14_irt12_1))
         
         #itemcharakter_1 = rowSums(select(., F4_irt12_1, F5_irt12_1, F6_irt12_1)),
         #parameterschaetz_1 = rowSums(select(., F10_irt12_1, F11_irt12_1, F12_irt12_1, F13_irt12_1, F14_irt12_1)),
         #spezobjektivi_1 = rowSums(select(., F7_irt12_1, F9_irt12_1)), # F8 und F9 sind eine Frage, ev. für F9 2 Punkte vergeben
  )

# Vektor mit Variablen die geglättet werden sollen (irt12_1 Page Submit) -------------
names_irt12_1_Page_Submit <- names(select(data_final,
                                          F1_Page_Submit_irt12_1, F2_Page_Submit_irt12_1, F4_Page_Submit_irt12_1,
                                          F5_Page_Submit_irt12_1, F6_Page_Submit_irt12_1, F7_Page_Submit_irt12_1,
                                          F8_Page_Submit_irt12_1, F9_Page_Submit_irt12_1, F10_Page_Submit_irt12_1,
                                          F11_Page_Submit_irt12_1, F12_Page_Submit_irt12_1, F13_Page_Submit_irt12_1,
                                          F14_Page_Submit_irt12_1)
)

# Ausgewählte Variablen (Page Submit irt12_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_irt12_1_Page_Submit)

# Variablen Page Submit für IRT1 Faktoren generieren
data_final <- data_final %>%
  mutate(lokalestochRZ_1 = rowSums(select(., F1_Page_Submit_irt12_1, F2_Page_Submit_irt12_1, F3_Page_Submit_irt12_1)),
         
         restRZ_1 = rowSums(select(., F4_Page_Submit_irt12_1, F5_Page_Submit_irt12_1, F6_Page_Submit_irt12_1,
                                      F7_Page_Submit_irt12_1, F8_Page_Submit_irt12_1, F9_Page_Submit_irt12_1,
                                      F10_Page_Submit_irt12_1, F11_Page_Submit_irt12_1,
                                      F12_Page_Submit_irt12_1, F13_Page_Submit_irt12_1, F14_Page_Submit_irt12_1))
         
         #itemcharakterRZ_1 = rowSums(select(., F4_Page_Submit_irt12_1, F5_Page_Submit_irt12_1, F6_Page_Submit_irt12_1)),
         #parameterschaetzRZ_1 = rowSums(select(., F10_Page_Submit_irt12_1, F11_Page_Submit_irt12_1,
         #                                    F12_Page_Submit_irt12_1, F13_Page_Submit_irt12_1, F14_Page_Submit_irt12_1)),
         #spezobjektiviRZ_1 = rowSums(select(., F7_Page_Submit_irt12_1, F8_Page_Submit_irt12_1, F9_Page_Submit_irt12_1))
  )



# Vektor mit Variablen die geglättet werden sollen (irt12_1 Klick Count) -------------
names_irt12_1_Click_Count <- names(select(data_final,
                                          F1_Click_Count_irt12_1, F2_Click_Count_irt12_1, F4_Click_Count_irt12_1,
                                          F5_Click_Count_irt12_1, F6_Click_Count_irt12_1, F7_Click_Count_irt12_1,
                                          F8_Click_Count_irt12_1, F9_Click_Count_irt12_1, F10_Click_Count_irt12_1,
                                          F11_Click_Count_irt12_1, F12_Click_Count_irt12_1, F13_Click_Count_irt12_1,
                                          F14_Click_Count_irt12_1)
)

# Ausgewählte Variablen (Klick Count irt12_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_irt12_1_Click_Count)

# Variablen Klick Count für IRT1 Faktoren generieren
data_final <- data_final %>%
  mutate(lokalestochKlick_1 = rowSums(select(., F1_Click_Count_irt12_1, F2_Click_Count_irt12_1, F3_Click_Count_irt12_1)),
         
         restKlick_1 = rowSums(select(., F4_Click_Count_irt12_1, F5_Click_Count_irt12_1, F6_Click_Count_irt12_1,
                                      F7_Click_Count_irt12_1, F8_Click_Count_irt12_1, F9_Click_Count_irt12_1,
                                      F10_Click_Count_irt12_1,F11_Click_Count_irt12_1,
                                      F12_Click_Count_irt12_1, F13_Click_Count_irt12_1,F14_Click_Count_irt12_1))
         
         #itemcharakterKlick_1 = rowSums(select(., F4_Click_Count_irt12_1, F5_Click_Count_irt12_1, F6_Click_Count_irt12_1)),
         #parameterschaetzKlick_1 = rowSums(select(., F10_Click_Count_irt12_1,F11_Click_Count_irt12_1,
         #                                       F12_Click_Count_irt12_1, F13_Click_Count_irt12_1,F14_Click_Count_irt12_1)),
         #spezobjektiviKlick_1 = rowSums(select(., F7_Click_Count_irt12_1, F8_Click_Count_irt12_1, F9_Click_Count_irt12_1))
  )


# Vektor mit Variablen die geglättet werden sollen (irt12_1 FF Page Submit) -------------
names_irt12_1_FFPage_Submit <- names(select(data_final,
                                            FF1_Page_Submit_irt12_1, FF2_Page_Submit_irt12_1, FF3_Page_Submit_irt12_1,
                                            FF4_Page_Submit_irt12_1, FF5_Page_Submit_irt12_1, FF6_Page_Submit_irt12_1,
                                            FF7_Page_Submit_irt12_1, FF9_Page_Submit_irt12_1,
                                            FF10_Page_Submit_irt12_1,FF11_Page_Submit_irt12_1, FF12_Page_Submit_irt12_1,
                                            FF13_Page_Submit_irt12_1, FF14_Page_Submit_irt12_1)
)

# Ausgewählte Variablen (FF Page Submit irt12_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_irt12_1_FFPage_Submit)

# Variablen Page Submit für IRT1 Faktoren generieren
data_final <- data_final %>%
  mutate(lokalestochFRZ_1 = rowSums(select(., FF1_Page_Submit_irt12_1, FF2_Page_Submit_irt12_1, FF3_Page_Submit_irt12_1)),
         
         restFRZ_1 = rowSums(select(., FF4_Page_Submit_irt12_1, FF5_Page_Submit_irt12_1, FF6_Page_Submit_irt12_1,
                                    FF7_Page_Submit_irt12_1, FF9_Page_Submit_irt12_1,
                                    FF10_Page_Submit_irt12_1,FF11_Page_Submit_irt12_1, FF12_Page_Submit_irt12_1,
                                    FF13_Page_Submit_irt12_1, FF14_Page_Submit_irt12_1))
         
         #itemcharakterFRZ_ = rowSums(select(., FF4_Page_Submit_irt12_1, FF5_Page_Submit_irt12_1, FF6_Page_Submit_irt12_1)),
         #parameterschaetzFRZ_1 = rowSums(select(., FF10_Page_Submit_irt12_1,FF11_Page_Submit_irt12_1, FF12_Page_Submit_irt12_1,
         #                                     FF13_Page_Submit_irt12_1, FF14_Page_Submit_irt12_1)),
         #spezobjektiviFRZ_1 = rowSums(select(., FF7_Page_Submit_irt12_1, FF9_Page_Submit_irt12_1))
  )



# Vektor mit Variablen die geglättet werden sollen (irt12_1 FF Klick Count) -------------
names_irt12_1_FFClick_Count <- names(select(data_final,
                                            FF1_Click_Count_irt12_1, FF2_Click_Count_irt12_1, FF3_Click_Count_irt12_1,
                                            FF4_Click_Count_irt12_1, FF5_Click_Count_irt12_1, FF6_Click_Count_irt12_1,
                                            FF7_Click_Count_irt12_1, FF9_Click_Count_irt12_1,
                                            FF10_Click_Count_irt12_1, FF11_Click_Count_irt12_1, FF12_Click_Count_irt12_1,
                                            FF13_Click_Count_irt12_1, FF14_Click_Count_irt12_1)
)

# Ausgewählte Variablen (FF Klick Count irt12_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_irt12_1_FFClick_Count)

# Variablen FF Klick Count für IRT1 Faktoren generieren
data_final <- data_final %>%
  mutate(lokalestochFKlick_1 = rowSums(select(., FF1_Click_Count_irt12_1, FF2_Click_Count_irt12_1, FF3_Click_Count_irt12_1)),
         
         restFKlick_1 = rowSums(select(., FF4_Click_Count_irt12_1, FF5_Click_Count_irt12_1, FF6_Click_Count_irt12_1,
                                    FF7_Click_Count_irt12_1, FF9_Click_Count_irt12_1,
                                    FF10_Click_Count_irt12_1, FF11_Click_Count_irt12_1, FF12_Click_Count_irt12_1,
                                    FF13_Click_Count_irt12_1, FF14_Click_Count_irt12_1))
         
         #itemcharakterFKlick_1 = rowSums(select(., FF4_Click_Count_irt12_1, FF5_Click_Count_irt12_1, FF6_Click_Count_irt12_1)),
         #parameterschaetzFKlick_1 = rowSums(select(., FF10_Click_Count_irt12_1, FF11_Click_Count_irt12_1, FF12_Click_Count_irt12_1,
         #                                        FF13_Click_Count_irt12_1, FF14_Click_Count_irt12_1)),
         #spezobjektiviFKlick_1 = rowSums(select(., FF7_Click_Count_irt12_1, FF9_Click_Count_irt12_1))
  )




















# Relevante Faktor-Variablen (Summenscore) erstellen (Teil irt12_2) IRT1/2 -----
# 1. Lokale Stoachastische Unabh. / 2. ICC / 3. Parameterschätzung / 4. Spez. Objektivität
data_final <- data_final %>%
  mutate(lokalestoch_2 = rowSums(select(., F1_irt12_2, F2_irt12_2, F3_irt12_2)),
         
         rest_2 = rowSums(select(., F4_irt12_2, F5_irt12_2, F6_irt12_2,
                                 F7_irt12_2, F8_irt12_2, F9_irt12_2,
                                 F10_irt12_2, F11_irt12_2, F12_irt12_2, F14_irt12_2))
         
         #itemcharakter_2 = rowSums(select(., F4_irt12_2, F5_irt12_2, F6_irt12_2)),
         #parameterschaetz_2 = rowSums(select(., F10_irt12_2, F11_irt12_2, F12_irt12_2, F14_irt12_2)),
         #spezobjektivi_2 = rowSums(select(., F7_irt12_2, F8_irt12_2, F9_irt12_2)), # F8 und F9 sind eine Frage, ev. für F9 2 Punkte vergeben
  )

# Vektor mit Variablen die geglättet werden sollen (irt12_2 Page Submit) -------------
names_irt12_2_Page_Submit <- names(select(data_final,
                                          F1_Page_Submit_irt12_2, F3_Page_Submit_irt12_2, F4_Page_Submit_irt12_2, 
                                          F5_Page_Submit_irt12_2, F6_Page_Submit_irt12_2, F7_Page_Submit_irt12_2,
                                          F8_Page_Submit_irt12_2, F9_Page_Submit_irt12_2, F10_Page_Submit_irt12_2,
                                          F11_Page_Submit_irt12_2, F12_Page_Submit_irt12_2,
                                          F14_Page_Submit_irt12_2)
)

# Ausgewählte Variablen (Page Submit irt12_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_irt12_2_Page_Submit)

# Variablen Page Submit für IRT1 Faktoren generieren
data_final <- data_final %>%
  mutate(lokalestochRZ_2 = rowSums(select(., F1_Page_Submit_irt12_2, F3_Page_Submit_irt12_2)),
         
         restRZ_2 = rowSums(select(., F4_Page_Submit_irt12_2, F5_Page_Submit_irt12_2, F6_Page_Submit_irt12_2,
                                   F7_Page_Submit_irt12_2, F8_Page_Submit_irt12_2, F9_Page_Submit_irt12_2,
                                   F10_Page_Submit_irt12_2, F11_Page_Submit_irt12_2,
                                   F12_Page_Submit_irt12_2, F14_Page_Submit_irt12_2))
         
         #itemcharakterRZ_2 = rowSums(select(., F4_Page_Submit_irt12_2, F5_Page_Submit_irt12_2, F6_Page_Submit_irt12_2)),
         #parameterschaetzRZ_2 = rowSums(select(., F10_Page_Submit_irt12_2, F11_Page_Submit_irt12_2,
         #                                      F12_Page_Submit_irt12_2, F14_Page_Submit_irt12_2)),
         #spezobjektiviRZ_2 = rowSums(select(., F7_Page_Submit_irt12_2, F8_Page_Submit_irt12_2, F9_Page_Submit_irt12_2))
  )



# Vektor mit Variablen die geglättet werden sollen (irt12_2 Klick Count) -------------
names_irt12_2_Click_Count <- names(select(data_final,
                                          F1_Click_Count_irt12_2, F3_Click_Count_irt12_2, F4_Click_Count_irt12_2,
                                          F5_Click_Count_irt12_2, F6_Click_Count_irt12_2, F7_Click_Count_irt12_2,
                                          F8_Click_Count_irt12_2, F9_Click_Count_irt12_2, F10_Click_Count_irt12_2,
                                          F11_Click_Count_irt12_2, F12_Click_Count_irt12_2,
                                          F14_Click_Count_irt12_2)
)

# Ausgewählte Variablen (Klick Count irt12_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_irt12_2_Click_Count)

# Variablen Klick Count für IRT1 Faktoren generieren
data_final <- data_final %>%
  mutate(lokalestochKlick_2 = rowSums(select(., F1_Click_Count_irt12_2, F3_Click_Count_irt12_2)),
         
         restKlick_2 = rowSums(select(., F4_Click_Count_irt12_2, F5_Click_Count_irt12_2, F6_Click_Count_irt12_2,
                                   F7_Click_Count_irt12_2, F8_Click_Count_irt12_2, F9_Click_Count_irt12_2,
                                   F10_Click_Count_irt12_2,F11_Click_Count_irt12_2,
                                   F12_Click_Count_irt12_2,F14_Click_Count_irt12_2))
         
         #itemcharakterKlick_2 = rowSums(select(., F4_Click_Count_irt12_2, F5_Click_Count_irt12_2, F6_Click_Count_irt12_2)),
         #parameterschaetzKlick_2 = rowSums(select(., F10_Click_Count_irt12_2,F11_Click_Count_irt12_2,
         #                                         F12_Click_Count_irt12_2,F14_Click_Count_irt12_2)),
         #spezobjektiviKlick_2 = rowSums(select(., F7_Click_Count_irt12_2, F8_Click_Count_irt12_2, F9_Click_Count_irt12_2))
  )
# Vektor mit Variablen die geglättet werden sollen (irt12_2 FF Page Submit) -------------
names_irt12_2_FFPage_Submit <- names(select(data_final,
                                            FF1_Page_Submit_irt12_2, FF2_Page_Submit_irt12_2, FF3_Page_Submit_irt12_2,
                                            FF4_Page_Submit_irt12_2, FF5_Page_Submit_irt12_2, FF6_Page_Submit_irt12_2,
                                            FF7_Page_Submit_irt12_2, FF8_Page_Submit_irt12_2, FF9_Page_Submit_irt12_2,
                                            FF10_Page_Submit_irt12_2,FF11_Page_Submit_irt12_2, FF12_Page_Submit_irt12_2,
                                            FF14_Page_Submit_irt12_2)
)

# Ausgewählte Variablen (FF Page Submit irt12_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_irt12_2_FFPage_Submit)

# Variablen Page Submit für IRT1 Faktoren generieren
data_final <- data_final %>%
  mutate(lokalestochFRZ_2 = rowSums(select(., FF1_Page_Submit_irt12_2, FF2_Page_Submit_irt12_2, FF3_Page_Submit_irt12_2)),
         
         restFRZ_2 = rowSums(select(., FF4_Page_Submit_irt12_2, FF5_Page_Submit_irt12_2, FF6_Page_Submit_irt12_2,
                                      FF7_Page_Submit_irt12_2, FF8_Page_Submit_irt12_2, FF9_Page_Submit_irt12_2,
                                      FF10_Page_Submit_irt12_2,FF11_Page_Submit_irt12_2, FF12_Page_Submit_irt12_2,
                                      FF14_Page_Submit_irt12_2))
         
         #itemcharakterFRZ_2 = rowSums(select(., FF4_Page_Submit_irt12_2, FF5_Page_Submit_irt12_2, FF6_Page_Submit_irt12_2)),
         #parameterschaetzFRZ_2 = rowSums(select(., FF10_Page_Submit_irt12_2,FF11_Page_Submit_irt12_2, FF12_Page_Submit_irt12_2,
         #                                       FF14_Page_Submit_irt12_2)),
         #spezobjektiviFRZ_2 = rowSums(select(., FF7_Page_Submit_irt12_2, FF8_Page_Submit_irt12_2, FF9_Page_Submit_irt12_2))
  )



# Vektor mit Variablen die geglättet werden sollen (irt12_2 FF Klick Count) -------------
names_irt12_2_FFClick_Count <- names(select(data_final,
                                            FF1_Click_Count_irt12_2, FF2_Click_Count_irt12_2, FF3_Click_Count_irt12_2,
                                            FF4_Click_Count_irt12_2, FF5_Click_Count_irt12_2, FF6_Click_Count_irt12_2,
                                            FF7_Click_Count_irt12_2, FF8_Click_Count_irt12_2, FF9_Click_Count_irt12_2,
                                            FF10_Click_Count_irt12_2, FF11_Click_Count_irt12_2, FF12_Click_Count_irt12_2,
                                            FF14_Click_Count_irt12_2)
)

# Ausgewählte Variablen (FF Klick Count irt12_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_irt12_2_FFClick_Count)

# Variablen FF Klick Count für IRT1 Faktoren generieren
data_final <- data_final %>%
  mutate(lokalestochFKlick_2 = rowSums(select(., FF1_Click_Count_irt12_2, FF2_Click_Count_irt12_2, FF3_Click_Count_irt12_2)),
         
         restFKlick_2 = rowSums(select(., FF4_Click_Count_irt12_2, FF5_Click_Count_irt12_2, FF6_Click_Count_irt12_2,
                                       FF7_Click_Count_irt12_2, FF8_Click_Count_irt12_2, FF9_Click_Count_irt12_2,
                                       FF10_Click_Count_irt12_2, FF11_Click_Count_irt12_2, FF12_Click_Count_irt12_2,
                                       FF14_Click_Count_irt12_2))
         
         #itemcharakterFKlick_2 = rowSums(select(., FF4_Click_Count_irt12_2, FF5_Click_Count_irt12_2, FF6_Click_Count_irt12_2)),
         #parameterschaetzFKlick_2 = rowSums(select(., FF10_Click_Count_irt12_2, FF11_Click_Count_irt12_2, FF12_Click_Count_irt12_2,
         #                                          FF14_Click_Count_irt12_2)),
         #spezobjektiviFKlick_2 = rowSums(select(., FF7_Click_Count_irt12_2, FF8_Click_Count_irt12_2, FF9_Click_Count_irt12_2))
  )













# Relevante Faktor-Variablen (Summenscore) erstellen (irt34_1) IRT3/4 -----
# 1. Item/Testinformation, 2. R/Tests, 3. Adaptives Testen
data_final <- data_final %>%
  mutate(item_testinfo_1 = rowSums(select(., F1_irt34_1,F2_irt34_1, F3_irt34_1)),
         r_tests_1 = rowSums(select(., F4_irt34_1, F5_irt34_1, F6_irt34_1, F7_irt34_1, F8_irt34_1)),
         adaptiv_testen_1 = rowSums(select(., F9_irt34_1, F10_irt34_1, F11_irt34_1, F12_irt34_1))
  )


# Vektor mit Variablen die geglättet werden sollen (irt34_1 Page Submit) -------------
names_irt34_1_Page_Submit <- names(select(data_final,
                                         F1_Page_Submit_irt34_1, F2_Page_Submit_irt34_1, F3_Page_Submit_irt34_1,
                                         F4_Page_Submit_irt34_1, F5_Page_Submit_irt34_1, F6_Page_Submit_irt34_1,
                                         F7_Page_Submit_irt34_1, F8_Page_Submit_irt34_1, F9_Page_Submit_irt34_1,
                                         F10_Page_Submit_irt34_1, F11_Page_Submit_irt34_1, F12_Page_Submit_irt34_1)
)

# Ausgewählte Variablen (Page Submit irt34_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_irt34_1_Page_Submit)

# Variablen Page Submit für IRT3/4 Faktoren generieren
data_final <- data_final %>%
  mutate(item_testinfoRZ_1 = rowSums(select(., F1_Page_Submit_irt34_1, F2_Page_Submit_irt34_1, F3_Page_Submit_irt34_1)),
         r_testsRZ_1 = rowSums(select(., F4_Page_Submit_irt34_1, F5_Page_Submit_irt34_1, F6_Page_Submit_irt34_1,
                                    F7_Page_Submit_irt34_1, F8_Page_Submit_irt34_1)),
         adaptiv_testenRZ_1 = rowSums(select(., F9_Page_Submit_irt34_1,
                                           F10_Page_Submit_irt34_1, F11_Page_Submit_irt34_1, F12_Page_Submit_irt34_1))
  )

# Vektor mit Variablen die geglättet werden sollen (irt34_1 Klick Count) -------------
names_irt34_1_Click_Count <- names(select(data_final,
                                         F1_Click_Count_irt34_1, F2_Click_Count_irt34_1, F3_Click_Count_irt34_1,
                                         F4_Click_Count_irt34_1, F5_Click_Count_irt34_1, F6_Click_Count_irt34_1,
                                         F7_Click_Count_irt34_1, F8_Click_Count_irt34_1, F9_Click_Count_irt34_1,
                                         F10_Click_Count_irt34_1, F11_Click_Count_irt34_1, F12_Click_Count_irt34_1)
)

# Ausgewählte Variablen (Klick Count irt34_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_irt34_1_Click_Count)

# Variablen Klick Count für IRT3/4 Faktoren generieren
data_final <- data_final %>%
  mutate(item_testinfoKlick_1 = rowSums(select(., F1_Click_Count_irt34_1, F2_Click_Count_irt34_1, F3_Click_Count_irt34_1)),
         r_testsKlick_1 = rowSums(select(., F4_Click_Count_irt34_1, F5_Click_Count_irt34_1, F6_Click_Count_irt34_1,
                                       F7_Click_Count_irt34_1, F8_Click_Count_irt34_1)),
         adaptiv_testenKlick_1 = rowSums(select(., F9_Click_Count_irt34_1,
                                              F10_Click_Count_irt34_1, F11_Click_Count_irt34_1, F12_Click_Count_irt34_1))
  )

# Vektor mit Variablen die geglättet werden sollen (irt34_1 FF Page Submit) -------------
names_irt34_1_FFPage_Submit <- names(select(data_final,
                                           FF1_Page_Submit_irt34_1, FF2_Page_Submit_irt34_1, FF3_Page_Submit_irt34_1,
                                           FF4_Page_Submit_irt34_1, FF5_Page_Submit_irt34_1, FF6_Page_Submit_irt34_1,
                                           FF7_Page_Submit_irt34_1, FF8_Page_Submit_irt34_1, FF9_Page_Submit_irt34_1,
                                           FF10_Page_Submit_irt34_1, FF11_Page_Submit_irt34_1, FF12_Page_Submit_irt34_1)
)

# Ausgewählte Variablen (FF Page Submit irt34_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_irt34_1_FFPage_Submit)

# Variablen Page Submit für IRT3/4 Faktoren generieren
data_final <- data_final %>%
  mutate(item_testinfoFRZ_1 = rowSums(select(., FF1_Page_Submit_irt34_1)),
         r_testsFRZ_1 = rowSums(select(., FF4_Page_Submit_irt34_1, FF5_Page_Submit_irt34_1, FF6_Page_Submit_irt34_1,
                                     FF7_Page_Submit_irt34_1, FF8_Page_Submit_irt34_1)),
         adaptiv_testenFRZ_1 = rowSums(select(., FF9_Page_Submit_irt34_1,
                                            FF10_Page_Submit_irt34_1, FF11_Page_Submit_irt34_1, FF12_Page_Submit_irt34_1))
  )


# Vektor mit Variablen die geglättet werden sollen (irt34_1 FF Klick Count) -------------
names_irt34_1_FFClick_Count <- names(select(data_final,
                                           FF1_Click_Count_irt34_1, FF2_Click_Count_irt34_1, FF3_Click_Count_irt34_1,
                                           FF4_Click_Count_irt34_1, FF5_Click_Count_irt34_1, FF6_Click_Count_irt34_1,
                                           FF7_Click_Count_irt34_1, FF8_Click_Count_irt34_1, FF9_Click_Count_irt34_1,
                                           FF10_Click_Count_irt34_1, FF11_Click_Count_irt34_1, FF12_Click_Count_irt34_1)
)

# Ausgewählte Variablen (Klick Count irt34_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_irt34_1_FFClick_Count)

# Variablen Klick Count für IRT3/4 Faktoren generieren
data_final <- data_final %>%
  mutate(item_testinfoFKlick_1 = rowSums(select(., FF1_Click_Count_irt34_1, FF2_Click_Count_irt34_1, FF3_Click_Count_irt34_1)),
         r_testsFKlick_1 = rowSums(select(., FF4_Click_Count_irt34_1, FF5_Click_Count_irt34_1, FF6_Click_Count_irt34_1,
                                        FF7_Click_Count_irt34_1, FF8_Click_Count_irt34_1)),
         adaptiv_testenFKlick_1 = rowSums(select(., FF9_Click_Count_irt34_1,
                                               FF10_Click_Count_irt34_1, FF11_Click_Count_irt34_1, FF12_Click_Count_irt34_1))
  )

















# Relevante Faktor-Variablen (Summenscore) erstellen (irt34_2) IRT3/4 -----
# 1. Item/Testinformation, 2. R/Tests, 3. Adaptives Testen
data_final <- data_final %>%
  mutate(item_testinfo_2 = rowSums(select(., F1_irt34_2, F2_irt34_2, F3_irt34_2, F4_irt34_2)),
         r_tests_2 = rowSums(select(., F5_irt34_2, F6_irt34_2, F7_irt34_2, F8_irt34_2, F9_irt34_2)),
         adaptiv_testen_2 = rowSums(select(., F10_irt34_2, F11_irt34_2, F12_irt34_2, F13_irt34_2))
  )


# Vektor mit Variablen die geglättet werden sollen (irt34_2 Page Submit) -------------
names_irt34_2_Page_Submit <- names(select(data_final,
                                          F1_Page_Submit_irt34_2, F2_Page_Submit_irt34_2, F3_Page_Submit_irt34_2,
                                          F4_Page_Submit_irt34_2, F5_Page_Submit_irt34_2, F6_Page_Submit_irt34_2,
                                          F7_Page_Submit_irt34_2, F8_Page_Submit_irt34_2, F9_Page_Submit_irt34_2,
                                          F10_Page_Submit_irt34_2, F11_Page_Submit_irt34_2, F12_Page_Submit_irt34_2, F13_Page_Submit_irt34_2)
)

# Ausgewählte Variablen (Page Submit irt34_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_irt34_2_Page_Submit)

# Variablen Page Submit für IRT3/4 Faktoren generieren
data_final <- data_final %>%
  mutate(item_testinfoRZ_2 = rowSums(select(., F1_Page_Submit_irt34_2, F2_Page_Submit_irt34_2, F3_Page_Submit_irt34_2, F4_Page_Submit_irt34_2)),
         r_testsRZ_2 = rowSums(select(., F5_Page_Submit_irt34_2, F6_Page_Submit_irt34_2,
                                    F7_Page_Submit_irt34_2, F8_Page_Submit_irt34_2, F9_Page_Submit_irt34_2)),
         adaptiv_testenRZ_2 = rowSums(select(., F10_Page_Submit_irt34_2, F11_Page_Submit_irt34_2, F12_Page_Submit_irt34_2, F13_Page_Submit_irt34_2))
  )

# Vektor mit Variablen die geglättet werden sollen (irt34_2 Klick Count) -------------
names_irt34_2_Click_Count <- names(select(data_final,
                                          F1_Click_Count_irt34_2, F2_Click_Count_irt34_2, F3_Click_Count_irt34_2,
                                          F4_Click_Count_irt34_2, F5_Click_Count_irt34_2, F6_Click_Count_irt34_1,
                                          F7_Click_Count_irt34_2, F8_Click_Count_irt34_2, F9_Click_Count_irt34_2,
                                          F10_Click_Count_irt34_2, F11_Click_Count_irt34_2, F12_Click_Count_irt34_2, F13_Click_Count_irt34_2)
)

# Ausgewählte Variablen (Klick Count irt34_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_irt34_2_Click_Count)

# Variablen Klick Count für IRT3/4 Faktoren generieren
data_final <- data_final %>%
  mutate(item_testinfoKlick_2 = rowSums(select(., F1_Click_Count_irt34_2, F2_Click_Count_irt34_2, F3_Click_Count_irt34_2, F4_Click_Count_irt34_2)),
         r_testsKlick_2 = rowSums(select(., F5_Click_Count_irt34_2, F6_Click_Count_irt34_2,
                                       F7_Click_Count_irt34_2, F8_Click_Count_irt34_2, F9_Click_Count_irt34_2)),
         adaptiv_testenKlick_2 = rowSums(select(., F10_Click_Count_irt34_2, F11_Click_Count_irt34_2, F12_Click_Count_irt34_2, F13_Click_Count_irt34_2))
  )

# Vektor mit Variablen die geglättet werden sollen (irt34_2 FF Page Submit) -------------
names_irt34_2_FFPage_Submit <- names(select(data_final,
                                            FF1_Page_Submit_irt34_2, FF2_Page_Submit_irt34_2, FF3_Page_Submit_irt34_2,
                                            FF4_Page_Submit_irt34_2, FF5_Page_Submit_irt34_2, FF6_Page_Submit_irt34_2,
                                            FF7_Page_Submit_irt34_2, FF8_Page_Submit_irt34_2, FF9_Page_Submit_irt34_2,
                                            FF10_Page_Submit_irt34_2, FF11_Page_Submit_irt34_2, FF12_Page_Submit_irt34_2, FF13_Page_Submit_irt34_2)
)

# Ausgewählte Variablen (FF Page Submit irt34_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_irt34_2_FFPage_Submit)

# Variablen Page Submit für IRT3/4 Faktoren generieren
data_final <- data_final %>%
  mutate(item_testinfoFRZ_2 = rowSums(select(., FF1_Page_Submit_irt34_2, FF2_Page_Submit_irt34_2, FF3_Page_Submit_irt34_2, FF4_Page_Submit_irt34_2)),
         r_testsFRZ_2 = rowSums(select(., FF5_Page_Submit_irt34_2, FF6_Page_Submit_irt34_2,
                                     FF7_Page_Submit_irt34_2, FF8_Page_Submit_irt34_2, FF9_Page_Submit_irt34_2)),
         adaptiv_testenFRZ_2 = rowSums(select(., FF10_Page_Submit_irt34_2, FF11_Page_Submit_irt34_2, FF12_Page_Submit_irt34_2, FF13_Page_Submit_irt34_2))
  )


# Vektor mit Variablen die geglättet werden sollen (irt34_2 FF Klick Count) -------------
names_irt34_2_FFClick_Count <- names(select(data_final,
                                            FF1_Click_Count_irt34_2, FF2_Click_Count_irt34_2, FF3_Click_Count_irt34_2,
                                            FF4_Click_Count_irt34_2, FF5_Click_Count_irt34_2, FF6_Click_Count_irt34_2,
                                            FF7_Click_Count_irt34_2, FF8_Click_Count_irt34_2, FF9_Click_Count_irt34_2,
                                            FF10_Click_Count_irt34_2, FF11_Click_Count_irt34_2, FF12_Click_Count_irt34_2, FF13_Click_Count_irt34_2)
)

# Ausgewählte Variablen (Klick Count irt34_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_irt34_2_FFClick_Count)

# Variablen Klick Count für IRT3/4 Faktoren generieren
data_final <- data_final %>%
  mutate(item_testinfoFKlick_2 = rowSums(select(., FF1_Click_Count_irt34_2, FF2_Click_Count_irt34_2, FF3_Click_Count_irt34_2, FF4_Click_Count_irt34_2)),
         r_testsFKlick_2 = rowSums(select(., FF5_Click_Count_irt34_2, FF6_Click_Count_irt34_2,
                                        FF7_Click_Count_irt34_2, FF8_Click_Count_irt34_2, FF9_Click_Count_irt34_2)),
         adaptiv_testenFKlick_2 = rowSums(select(., FF10_Click_Count_irt34_2, FF11_Click_Count_irt34_2, FF12_Click_Count_irt34_2, FF13_Click_Count_irt34_2))
  )







write_csv(data_final, "data_final_2021_komplett.csv")









# Relevante Faktor-Variablen (Summenscore) erstellen (cfa12_1) CFA1/2 -----
# 1. Freiheitsgrade, 2. Parameterschätzung, 3. R Modellierung, 4. R Modellvergleich
data_final <- data_final %>%
  mutate(freiheitsgrade_1 = rowSums(select(., F1_cfa12_1, F2_cfa12_1, F3_cfa12_1, F4_cfa12_1)),
         parameterschaetz_cfa_1 = rowSums(select(., F6_cfa12_1, F7_cfa12_1, F8_cfa12_1, F9_cfa12_1)),
         r_modelierung_1 = rowSums(select(., F10_cfa12_1, F11_cfa12_1, F12_cfa12_1, F13_cfa12_1, F14_cfa12_1)),
         r_modelvergleich_1 = rowSums(select(., F15_cfa12_1, F16_cfa12_1, F17_cfa12_1)),
         
         termin4 = rowSums(select(., F1_cfa12_1, F2_cfa12_1, F3_cfa12_1, F4_cfa12_1, F6_cfa12_1, F7_cfa12_1, F8_cfa12_1, F9_cfa12_1,
                                  F10_cfa12_1, F11_cfa12_1, F12_cfa12_1, F13_cfa12_1, F14_cfa12_1, F15_cfa12_1, F16_cfa12_1, F17_cfa12_1))
  )

# Vektor mit Variablen die geglättet werden sollen (cfa12_1 Page Submit) -------------
names_cfa12_1_Page_Submit <- names(select(data_final,
                                         F1_Page_Submit_cfa12_1, F2_Page_Submit_cfa12_1, F3_Page_Submit_cfa12_1,
                                         F4_Page_Submit_cfa12_1, F6_Page_Submit_cfa12_1,
                                         F7_Page_Submit_cfa12_1, F8_Page_Submit_cfa12_1, F9_Page_Submit_cfa12_1,
                                         F10_Page_Submit_cfa12_1, F11_Page_Submit_cfa12_1, F12_Page_Submit_cfa12_1,
                                         F13_Page_Submit_cfa12_1, F14_Page_Submit_cfa12_1, F15_Page_Submit_cfa12_1,
                                         F16_Page_Submit_cfa12_1, F17_Page_Submit_cfa12_1)
)

# Ausgewählte Variablen (Page Submit cfa12_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_cfa12_1_Page_Submit)

# Variablen Page Submit für CFA1/2 Faktoren generieren
data_final <- data_final %>%
  mutate(freiheitsgradeRZ_1 = rowSums(select(., F1_Page_Submit_cfa12_1, F2_Page_Submit_cfa12_1, F3_Page_Submit_cfa12_1, F4_Page_Submit_cfa12_1)),
         parameterschaetz_cfaRZ_1 = rowSums(select(., F6_Page_Submit_cfa12_1,F7_Page_Submit_cfa12_1, F8_Page_Submit_cfa12_1, F9_Page_Submit_cfa12_1)),
         r_modelierungRZ_1 = rowSums(select(., F10_Page_Submit_cfa12_1, F11_Page_Submit_cfa12_1, F12_Page_Submit_cfa12_1,F13_Page_Submit_cfa12_1, F14_Page_Submit_cfa12_1)),
         r_modelvergleichRZ_1 = rowSums(select(., F15_Page_Submit_cfa12_1,F16_Page_Submit_cfa12_1, F17_Page_Submit_cfa12_1))
  )

# Vektor mit Variablen die geglättet werden sollen (cfa12_1 Klick Count) -------------
names_cfa12_1_Click_Count <- names(select(data_final,
                                         F1_Click_Count_cfa12_1, F2_Click_Count_cfa12_1, F3_Click_Count_cfa12_1,
                                         F4_Click_Count_cfa12_1, F6_Click_Count_cfa12_1,
                                         F7_Click_Count_cfa12_1, F8_Click_Count_cfa12_1, F9_Click_Count_cfa12_1,
                                         F10_Click_Count_cfa12_1, F11_Click_Count_cfa12_1, F12_Click_Count_cfa12_1,
                                         F13_Click_Count_cfa12_1, F14_Click_Count_cfa12_1, F15_Click_Count_cfa12_1,
                                         F16_Click_Count_cfa12_1, F17_Click_Count_cfa12_1)
)

# Ausgewählte Variablen (Klick Count cfa12_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_cfa12_1_Click_Count)

# Variablen Page Submit für CFA1/2 Faktoren generieren
data_final <- data_final %>%
  mutate(freiheitsgradeKlick_1 = rowSums(select(., F1_Click_Count_cfa12_1, F2_Click_Count_cfa12_1, F3_Click_Count_cfa12_1, F4_Click_Count_cfa12_1)),
         parameterschaetz_cfaKlick_1 = rowSums(select(., F6_Click_Count_cfa12_1, F7_Click_Count_cfa12_1, F8_Click_Count_cfa12_1, F9_Click_Count_cfa12_1)),
         r_modelierungKlick_1 = rowSums(select(., F10_Click_Count_cfa12_1, F11_Click_Count_cfa12_1, F12_Click_Count_cfa12_1, F13_Click_Count_cfa12_1, F14_Click_Count_cfa12_1)),
         r_modelvergleichKlick_1 = rowSums(select(., F15_Click_Count_cfa12_1, F16_Click_Count_cfa12_1, F17_Click_Count_cfa12_1))
  )


# Vektor mit Variablen die geglättet werden sollen (cfa12_1 FF Page Submit) -------------
names_cfa12_1_FFPage_Submit <- names(select(data_final,
                                           FF1_Page_Submit_cfa12_1, FF2_Page_Submit_cfa12_1, FF3_Page_Submit_cfa12_1,
                                           FF4_Page_Submit_cfa12_1, FF6_Page_Submit_cfa12_1,
                                           FF7_Page_Submit_cfa12_1, FF8_Page_Submit_cfa12_1, FF9_Page_Submit_cfa12_1,
                                           FF10_Page_Submit_cfa12_1, FF11_Page_Submit_cfa12_1, FF12_Page_Submit_cfa12_1,
                                           FF13_Page_Submit_cfa12_1, FF14_Page_Submit_cfa12_1, FF15_Page_Submit_cfa12_1,
                                           FF16_Page_Submit_cfa12_1, FF17_Page_Submit_cfa12_1)
)

# Ausgewählte Variablen (FF Page Submit cfa12_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_cfa12_1_FFPage_Submit)

# Variablen FF Page Submit für CFA1/2 Faktoren generieren
data_final <- data_final %>%
  mutate(freiheitsgradeFRZ_1 = rowSums(select(., FF1_Page_Submit_cfa12_1, FF2_Page_Submit_cfa12_1, FF3_Page_Submit_cfa12_1, FF4_Page_Submit_cfa12_1)),
         parameterschaetz_cfaFRZ_1 = rowSums(select(., FF6_Page_Submit_cfa12_1, FF7_Page_Submit_cfa12_1, FF8_Page_Submit_cfa12_1, FF9_Page_Submit_cfa12_1)),
         r_modelierungFRZ_1 = rowSums(select(., FF10_Page_Submit_cfa12_1, FF11_Page_Submit_cfa12_1, FF12_Page_Submit_cfa12_1, FF13_Page_Submit_cfa12_1, FF14_Page_Submit_cfa12_1)),
         r_modelvergleichFRZ_1 = rowSums(select(.,FF17_Page_Submit_cfa12_1))
  )


# Vektor mit Variablen die geglättet werden sollen (cfa12_1 FF Klick Count) -------------
names_cfa12_1_FFClick_Count <- names(select(data_final,
                                           FF1_Click_Count_cfa12_1, FF2_Click_Count_cfa12_1, FF3_Click_Count_cfa12_1,
                                           FF4_Click_Count_cfa12_1, FF6_Click_Count_cfa12_1,
                                           FF7_Click_Count_cfa12_1, FF8_Click_Count_cfa12_1, FF9_Click_Count_cfa12_1,
                                           FF10_Click_Count_cfa12_1, FF11_Click_Count_cfa12_1, FF12_Click_Count_cfa12_1,
                                           FF13_Click_Count_cfa12_1, FF14_Click_Count_cfa12_1, FF15_Click_Count_cfa12_1,
                                           FF16_Click_Count_cfa12_1, FF17_Click_Count_cfa12_1)
)

# Ausgewählte Variablen (FF Klick Count cfa12_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_cfa12_1_FFClick_Count)

# Variablen Page Submit für CFA1/2 Faktoren generieren
data_final <- data_final %>%
  mutate(freiheitsgradeFKlick_1 = rowSums(select(., F1_Click_Count_cfa12_1, F2_Click_Count_cfa12_1, F3_Click_Count_cfa12_1, F4_Click_Count_cfa12_1)),
         parameterschaetz_cfaFKlick_1 = rowSums(select(., F6_Click_Count_cfa12_1, F7_Click_Count_cfa12_1, F8_Click_Count_cfa12_1, F9_Click_Count_cfa12_1)),
         r_modelierungFKlick_1 = rowSums(select(., F10_Click_Count_cfa12_1, F11_Click_Count_cfa12_1, F12_Click_Count_cfa12_1, F13_Click_Count_cfa12_1, F14_Click_Count_cfa12_1)),
         r_modelvergleichFKlick_1 = rowSums(select(., F15_Click_Count_cfa12_1, F16_Click_Count_cfa12_1, F17_Click_Count_cfa12_1))
  )


















# Relevante Faktor-Variablen (Summenscore) erstellen (cfa12_2) CFA1/2 -----
# 1. Freiheitsgrade, 2. Parameterschätzung, 3. R Modellierung, 4. R Modellvergleich
data_final <- data_final %>%
  mutate(freiheitsgrade_2 = rowSums(select(., F1_cfa12_2, F2_cfa12_2, F3_cfa12_2)),
         parameterschaetz_cfa_2 = rowSums(select(., F4_cfa12_2, F5_cfa12_2, F6_cfa12_2, F7_cfa12_2)),
         r_modelierung_2 = rowSums(select(., F8_cfa12_2, F9_cfa12_2, F10_cfa12_2, F11_cfa12_2, F12_cfa12_2)),
         r_modelvergleich_2 = rowSums(select(., F14_cfa12_2, F15_cfa12_2, F16_cfa12_2))
  )

# Vektor mit Variablen die geglättet werden sollen (cfa12_2 Page Submit) -------------
names_cfa12_2_Page_Submit <- names(select(data_final,
                                          F1_Page_Submit_cfa12_2, F2_Page_Submit_cfa12_2, F3_Page_Submit_cfa12_2,
                                          F4_Page_Submit_cfa12_2, F5_Page_Submit_cfa12_2, F6_Page_Submit_cfa12_2,
                                          F7_Page_Submit_cfa12_2, F8_Page_Submit_cfa12_2, F9_Page_Submit_cfa12_2,
                                          F10_Page_Submit_cfa12_2, F11_Page_Submit_cfa12_2, F12_Page_Submit_cfa12_2,
                                          F13_Page_Submit_cfa12_2, F14_Page_Submit_cfa12_2, F15_Page_Submit_cfa12_2,
                                          F16_Page_Submit_cfa12_2)
)

# Ausgewählte Variablen (Page Submit cfa12_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_cfa12_2_Page_Submit)

# Variablen Page Submit für CFA1/2 Faktoren generieren
data_final <- data_final %>%
  mutate(freiheitsgradeRZ_2 = rowSums(select(., F1_Page_Submit_cfa12_2, F2_Page_Submit_cfa12_2, F3_Page_Submit_cfa12_2)),
         parameterschaetz_cfaRZ_2 = rowSums(select(., F4_Page_Submit_cfa12_2, F5_Page_Submit_cfa12_2, F6_Page_Submit_cfa12_2, F7_Page_Submit_cfa12_2)),
         r_modelierungRZ_2 = rowSums(select(., F8_Page_Submit_cfa12_2, F9_Page_Submit_cfa12_2, F10_Page_Submit_cfa12_2, F11_Page_Submit_cfa12_2, F12_Page_Submit_cfa12_2, F13_Page_Submit_cfa12_2)),
         r_modelvergleichRZ_2 = rowSums(select(., F14_Page_Submit_cfa12_2, F15_Page_Submit_cfa12_2, F16_Page_Submit_cfa12_2))
  )

# Vektor mit Variablen die geglättet werden sollen (cfa12_2 Klick Count) -------------
names_cfa12_2_Click_Count <- names(select(data_final,
                                          F1_Click_Count_cfa12_2, F2_Click_Count_cfa12_2, F3_Click_Count_cfa12_2,
                                          F4_Click_Count_cfa12_2, F5_Click_Count_cfa12_2, F6_Click_Count_cfa12_2,
                                          F7_Click_Count_cfa12_2, F8_Click_Count_cfa12_2, F9_Click_Count_cfa12_2,
                                          F10_Click_Count_cfa12_2, F11_Click_Count_cfa12_2, F12_Click_Count_cfa12_2,
                                          F13_Click_Count_cfa12_2, F14_Click_Count_cfa12_2, F15_Click_Count_cfa12_2,
                                          F16_Click_Count_cfa12_2)
)

# Ausgewählte Variablen (Klick Count cfa12_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_cfa12_2_Click_Count)

# Variablen Page Submit für CFA1/2 Faktoren generieren
data_final <- data_final %>%
  mutate(freiheitsgradeKlick_2 = rowSums(select(., F1_Click_Count_cfa12_2, F2_Click_Count_cfa12_2, F3_Click_Count_cfa12_2)),
         parameterschaetz_cfaKlick_2 = rowSums(select(., F4_Click_Count_cfa12_2, F5_Click_Count_cfa12_2, F6_Click_Count_cfa12_2, F7_Click_Count_cfa12_2)),
         r_modelierungKlick_2 = rowSums(select(., F8_Click_Count_cfa12_2, F9_Click_Count_cfa12_2, F10_Click_Count_cfa12_2, F11_Click_Count_cfa12_2, F12_Click_Count_cfa12_2, F13_Click_Count_cfa12_2)),
         r_modelvergleichKlick_2 = rowSums(select(., F14_Click_Count_cfa12_2, F15_Click_Count_cfa12_2, F16_Click_Count_cfa12_2))
  )


# Vektor mit Variablen die geglättet werden sollen (cfa12_2 FF Page Submit) -------------
names_cfa12_2_FFPage_Submit <- names(select(data_final,
                                            FF1_Page_Submit_cfa12_2, FF2_Page_Submit_cfa12_2, FF3_Page_Submit_cfa12_2,
                                            FF4_Page_Submit_cfa12_2, FF5_Page_Submit_cfa12_2, FF6_Page_Submit_cfa12_2,
                                            FF7_Page_Submit_cfa12_2, FF8_Page_Submit_cfa12_2, FF9_Page_Submit_cfa12_2,
                                            FF10_Page_Submit_cfa12_2, FF11_Page_Submit_cfa12_2, FF12_Page_Submit_cfa12_2,
                                            FF13_Page_Submit_cfa12_2, FF14_Page_Submit_cfa12_2, FF15_Page_Submit_cfa12_2,
                                            FF16_Page_Submit_cfa12_2)
)

# Ausgewählte Variablen (FF Page Submit cfa12_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_cfa12_2_FFPage_Submit)

# Variablen FF Page Submit für CFA1/2 Faktoren generieren
data_final <- data_final %>%
  mutate(freiheitsgradeFRZ_2 = rowSums(select(., FF1_Page_Submit_cfa12_2, FF2_Page_Submit_cfa12_2, FF3_Page_Submit_cfa12_2)),
         parameterschaetz_cfaFRZ_2 = rowSums(select(., FF4_Page_Submit_cfa12_2, FF5_Page_Submit_cfa12_2, FF6_Page_Submit_cfa12_2, FF7_Page_Submit_cfa12_2)),
         r_modelierungRZ_2 = rowSums(select(., FF8_Page_Submit_cfa12_2, FF9_Page_Submit_cfa12_2, FF10_Page_Submit_cfa12_2, FF11_Page_Submit_cfa12_2, FF12_Page_Submit_cfa12_2, FF13_Page_Submit_cfa12_2)),
         r_modelvergleichRZ_2 = rowSums(select(., FF14_Page_Submit_cfa12_2, FF15_Page_Submit_cfa12_2, FF16_Page_Submit_cfa12_2))
  )


# Vektor mit Variablen die geglättet werden sollen (cfa12_2 FF Klick Count) -------------
names_cfa12_2_FFClick_Count <- names(select(data_final,
                                            FF1_Click_Count_cfa12_2, FF2_Click_Count_cfa12_2, FF3_Click_Count_cfa12_2,
                                            FF4_Click_Count_cfa12_2, FF5_Click_Count_cfa12_2, FF6_Click_Count_cfa12_2,
                                            FF7_Click_Count_cfa12_2, FF8_Click_Count_cfa12_2, FF9_Click_Count_cfa12_2,
                                            FF10_Click_Count_cfa12_2, FF11_Click_Count_cfa12_2, FF12_Click_Count_cfa12_2,
                                            FF13_Click_Count_cfa12_2, FF14_Click_Count_cfa12_2, FF15_Click_Count_cfa12_2,
                                            FF16_Click_Count_cfa12_2)
)

# Ausgewählte Variablen (FF Klick Count cfa12_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_cfa12_2_FFClick_Count)

# Variablen Page Submit für CFA1/2 Faktoren generieren
data_final <- data_final %>%
  mutate(freiheitsgradeFKlick_2 = rowSums(select(., FF1_Click_Count_cfa12_2, FF2_Click_Count_cfa12_2, FF3_Click_Count_cfa12_2)),
         parameterschaetz_cfaFKlick_2 = rowSums(select(., FF4_Click_Count_cfa12_2, FF5_Click_Count_cfa12_2, FF6_Click_Count_cfa12_2, FF7_Click_Count_cfa12_2)),
         r_modelierungFKlick_2 = rowSums(select(., FF8_Click_Count_cfa12_2, FF9_Click_Count_cfa12_2, FF10_Click_Count_cfa12_2, FF11_Click_Count_cfa12_2, FF12_Click_Count_cfa12_2, FF13_Click_Count_cfa12_2)),
         r_modelvergleichFKlick_2 = rowSums(select(., FF14_Click_Count_cfa12_2, FF15_Click_Count_cfa12_2, FF16_Click_Count_cfa12_2))
  )


















# Relevante Faktor-Variablen (Summenscore) erstellen (cfa34_1) CFA3/4 -----
# 1. MTMM, 2. Fairness, 3. R Reliability, 4. R Invarianz
data_final <- data_final %>%
  mutate(mtmm_1 = rowSums(select(., F1_cfa34_1, F2_cfa34_1, F3_cfa34_1, F4_cfa34_1)),
         fairness_1 = rowSums(select(., F5_cfa34_1, F6_cfa34_1, F7_cfa34_1)),
         reliability_1 = rowSums(select(., F9_cfa34_1, F10_cfa34_1, F11_cfa34_1, F12_cfa34_1)),
         invarianz_1 = rowSums(select(., F13_cfa34_1, F14_cfa34_1, F15_cfa34_1))
  )


# Vektor mit Variablen die geglättet werden sollen (cfa34_1 Page Submit) -------------
names_cfa34_1_Page_Submit <- names(select(data_final,
                                         F1_Page_Submit_cfa34_1, F2_Page_Submit_cfa34_1, F2_Page_Submit_cfa34_1, F4_Page_Submit_cfa34_1,
                                         F5_Page_Submit_cfa34_1, F6_Page_Submit_cfa34_1, F7_Page_Submit_cfa34_1,
                                         F9_Page_Submit_cfa34_1, F10_Page_Submit_cfa34_1, F11_Page_Submit_cfa34_1, F12_Page_Submit_cfa34_1,
                                         F13_Page_Submit_cfa34_1, F14_Page_Submit_cfa34_1, F15_Page_Submit_cfa34_1)
)

# Ausgewählte Variablen (Page Submit cfa34_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_cfa34_1_Page_Submit)

# Variablen Page Submit für CFA3/4 Faktoren generieren
data_final <- data_final %>%
  mutate(mtmmRZ_1 = rowSums(select(., F1_Page_Submit_cfa34_1, F2_Page_Submit_cfa34_1, F2_Page_Submit_cfa34_1, F4_Page_Submit_cfa34_1)),
         fairnessRZ_1 = rowSums(select(., F5_Page_Submit_cfa34_1, F6_Page_Submit_cfa34_1, F7_Page_Submit_cfa34_1)),
         reliabilityRZ_1 = rowSums(select(., F9_Page_Submit_cfa34_1, F10_Page_Submit_cfa34_1, F11_Page_Submit_cfa34_1, F12_Page_Submit_cfa34_1)),
         invarianzRZ_1 = rowSums(select(., F13_Page_Submit_cfa34_1, F14_Page_Submit_cfa34_1, F15_Page_Submit_cfa34_1))
  )

# Vektor mit Variablen die geglättet werden sollen (cfa34_1 Klick Count) -------------
names_cfa34_1_Click_Count <- names(select(data_final,
                                         F1_Click_Count_cfa34_1, F2_Click_Count_cfa34_1, F2_Click_Count_cfa34_1, F4_Click_Count_cfa34_1,
                                         F5_Click_Count_cfa34_1, F6_Click_Count_cfa34_1, F7_Click_Count_cfa34_1,
                                         F9_Click_Count_cfa34_1, F10_Click_Count_cfa34_1, F11_Click_Count_cfa34_1, F12_Click_Count_cfa34_1,
                                         F13_Click_Count_cfa34_1, F14_Click_Count_cfa34_1, F15_Click_Count_cfa34_1)
)

# Ausgewählte Variablen (Klick Count cfa34_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_cfa34_1_Click_Count)

# Variablen Klick Count für CFA3/4 Faktoren generieren
data_final <- data_final %>%
  mutate(mtmmKlick_1 = rowSums(select(., F1_Click_Count_cfa34_1, F2_Click_Count_cfa34_1, F2_Click_Count_cfa34_1, F4_Click_Count_cfa34_1)),
         fairnessKlick_1 = rowSums(select(., F5_Click_Count_cfa34_1, F6_Click_Count_cfa34_1, F7_Click_Count_cfa34_1)),
         reliabilityKlick_1 = rowSums(select(., F9_Click_Count_cfa34_1, F10_Click_Count_cfa34_1, F11_Click_Count_cfa34_1, F12_Click_Count_cfa34_1)),
         invarianzKlick_1 = rowSums(select(., F13_Click_Count_cfa34_1, F14_Click_Count_cfa34_1, F15_Click_Count_cfa34_1))
  )




# Vektor mit Variablen die geglättet werden sollen (cfa34_1 FF Page Submit) -------------
names_cfa34_1_FFPage_Submit <- names(select(data_final,
                                           FF1_Page_Submit_cfa34_1, FF2_Page_Submit_cfa34_1, FF2_Page_Submit_cfa34_1, FF4_Page_Submit_cfa34_1,
                                           FF5_Page_Submit_cfa34_1, FF6_Page_Submit_cfa34_1, FF7_Page_Submit_cfa34_1,
                                           FF9_Page_Submit_cfa34_1, FF10_Page_Submit_cfa34_1, FF11_Page_Submit_cfa34_1, FF12_Page_Submit_cfa34_1,
                                           FF13_Page_Submit_cfa34_1, FF14_Page_Submit_cfa34_1, FF15_Page_Submit_cfa34_1)
)

# Ausgewählte Variablen (FF Page Submit cfa34_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_cfa34_1_FFPage_Submit)

# Variablen ff Page Submit für CFA3/4 Faktoren generieren
data_final <- data_final %>%
  mutate(mtmmFRZ_1 = rowSums(select(., FF1_Page_Submit_cfa34_1, FF2_Page_Submit_cfa34_1, FF2_Page_Submit_cfa34_1, FF4_Page_Submit_cfa34_1)),
         fairnessFRZ_1 = rowSums(select(., FF5_Page_Submit_cfa34_1, FF6_Page_Submit_cfa34_1, FF7_Page_Submit_cfa34_1)),
         reliabilityFRZ_1 = rowSums(select(., FF9_Page_Submit_cfa34_1, FF10_Page_Submit_cfa34_1, FF11_Page_Submit_cfa34_1, FF12_Page_Submit_cfa34_1)),
         invarianzFRZ_1 = rowSums(select(., FF13_Page_Submit_cfa34_1, FF14_Page_Submit_cfa34_1, FF15_Page_Submit_cfa34_1))
  )




# Vektor mit Variablen die geglättet werden sollen (cfa34_1 FF Klick Count) -------------
names_cfa34_1_FFClick_Count <- names(select(data_final,
                                           FF1_Click_Count_cfa34_1, FF2_Click_Count_cfa34_1, FF2_Click_Count_cfa34_1, FF4_Click_Count_cfa34_1,
                                           FF5_Click_Count_cfa34_1, FF6_Click_Count_cfa34_1, FF7_Click_Count_cfa34_1,
                                           FF9_Click_Count_cfa34_1, FF10_Click_Count_cfa34_1, FF11_Click_Count_cfa34_1, FF12_Click_Count_cfa34_1,
                                           FF13_Click_Count_cfa34_1,FF14_Click_Count_cfa34_1, FF15_Click_Count_cfa34_1)
)

# Ausgewählte Variablen (FF Klick Count cfa34_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_cfa34_1_FFClick_Count)

# Variablen FF Klick Count für CFA3/4 Faktoren generieren
data_final <- data_final %>%
  mutate(mtmmFKlick_1 = rowSums(select(., FF1_Click_Count_cfa34_1, FF2_Click_Count_cfa34_1, FF2_Click_Count_cfa34_1, FF4_Click_Count_cfa34_1)),
         fairnessFKlick_1 = rowSums(select(., FF5_Click_Count_cfa34_1, FF6_Click_Count_cfa34_1, FF7_Click_Count_cfa34_1)),
         reliabilityFKlick_1 = rowSums(select(., FF9_Click_Count_cfa34_1, FF10_Click_Count_cfa34_1, FF11_Click_Count_cfa34_1, FF12_Click_Count_cfa34_1)),
         invarianzFKlick_1 = rowSums(select(., FF13_Click_Count_cfa34_1,FF14_Click_Count_cfa34_1, FF15_Click_Count_cfa34_1))
  )

















# Relevante Faktor-Variablen (Summenscore) erstellen (cfa34_2) CFA3/4 -----
# 1. MTMM, 2. Fairness, 3. R Reliability, 4. R Invarianz
data_final <- data_final %>%
  mutate(mtmm_2 = rowSums(select(., F1_cfa34_2, F2_cfa34_2, F3_cfa34_2, F4_cfa34_2)),
         fairness_2 = rowSums(select(., F5_cfa34_2, F6_cfa34_2, F7_cfa34_2)),
         reliability_2 = rowSums(select(., F9_cfa34_2, F10_cfa34_2, F11_cfa34_2, F12_cfa34_2)),
         invarianz_2 = rowSums(select(., F13_cfa34_2, F14_cfa34_2, F15_cfa34_2))
  )


# Vektor mit Variablen die geglättet werden sollen (cfa34_2 Page Submit) -------------
names_cfa34_2_Page_Submit <- names(select(data_final,
                                          F1_Page_Submit_cfa34_2, F2_Page_Submit_cfa34_2, F2_Page_Submit_cfa34_2, F4_Page_Submit_cfa34_2,
                                          F5_Page_Submit_cfa34_2, F6_Page_Submit_cfa34_2, F7_Page_Submit_cfa34_2,
                                          F9_Page_Submit_cfa34_2, F10_Page_Submit_cfa34_2, F11_Page_Submit_cfa34_2, F12_Page_Submit_cfa34_2,
                                          F13_Page_Submit_cfa34_2, F14_Page_Submit_cfa34_2, F15_Page_Submit_cfa34_2)
)

# Ausgewählte Variablen (Page Submit cfa34_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_cfa34_2_Page_Submit)

# Variablen Page Submit für CFA3/4 Faktoren generieren
data_final <- data_final %>%
  mutate(mtmmRZ_2 = rowSums(select(., F1_Page_Submit_cfa34_2, F2_Page_Submit_cfa34_2, F2_Page_Submit_cfa34_2, F4_Page_Submit_cfa34_2)),
         fairnessRZ_2 = rowSums(select(., F5_Page_Submit_cfa34_2, F6_Page_Submit_cfa34_2, F7_Page_Submit_cfa34_2)),
         reliabilityRZ_2 = rowSums(select(., F9_Page_Submit_cfa34_2, F10_Page_Submit_cfa34_2, F11_Page_Submit_cfa34_2, F12_Page_Submit_cfa34_2)),
         invarianzRZ_2 = rowSums(select(., F13_Page_Submit_cfa34_2, F14_Page_Submit_cfa34_2, F15_Page_Submit_cfa34_2))
  )

# Vektor mit Variablen die geglättet werden sollen (cfa34_2 Klick Count) -------------
names_cfa34_2_Click_Count <- names(select(data_final,
                                          F1_Click_Count_cfa34_2, F2_Click_Count_cfa34_2, F2_Click_Count_cfa34_2, F4_Click_Count_cfa34_2,
                                          F5_Click_Count_cfa34_2, F6_Click_Count_cfa34_2, F7_Click_Count_cfa34_2,
                                          F9_Click_Count_cfa34_2, F10_Click_Count_cfa34_2, F11_Click_Count_cfa34_2, F12_Click_Count_cfa34_2,
                                          F13_Click_Count_cfa34_2, F14_Click_Count_cfa34_2, F15_Click_Count_cfa34_2)
)

# Ausgewählte Variablen (Klick Count cfa34_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_cfa34_2_Click_Count)

# Variablen Klick Count für CFA3/4 Faktoren generieren
data_final <- data_final %>%
  mutate(mtmmKlick_2 = rowSums(select(., F1_Click_Count_cfa34_2, F2_Click_Count_cfa34_2, F2_Click_Count_cfa34_2, F4_Click_Count_cfa34_2)),
         fairnessKlick_2 = rowSums(select(., F5_Click_Count_cfa34_2, F6_Click_Count_cfa34_2, F7_Click_Count_cfa34_2)),
         reliabilityKlick_2 = rowSums(select(., F9_Click_Count_cfa34_2, F10_Click_Count_cfa34_2, F11_Click_Count_cfa34_2, F12_Click_Count_cfa34_2)),
         invarianzKlick_2 = rowSums(select(., F13_Click_Count_cfa34_2, F14_Click_Count_cfa34_2, F15_Click_Count_cfa34_2))
  )




# Vektor mit Variablen die geglättet werden sollen (cfa34_2 FF Page Submit) -------------
names_cfa34_2_FFPage_Submit <- names(select(data_final,
                                            FF1_Page_Submit_cfa34_2, FF2_Page_Submit_cfa34_2, FF2_Page_Submit_cfa34_2, FF4_Page_Submit_cfa34_2,
                                            FF5_Page_Submit_cfa34_2, FF6_Page_Submit_cfa34_2, FF7_Page_Submit_cfa34_2,
                                            FF9_Page_Submit_cfa34_2, FF10_Page_Submit_cfa34_2, FF11_Page_Submit_cfa34_2, FF12_Page_Submit_cfa34_2,
                                            FF13_Page_Submit_cfa34_2, FF14_Page_Submit_cfa34_2, FF15_Page_Submit_cfa34_2)
)

# Ausgewählte Variablen (FF Page Submit cfa34_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_cfa34_2_FFPage_Submit)

# Variablen ff Page Submit für CFA3/4 Faktoren generieren
data_final <- data_final %>%
  mutate(mtmmFRZ_2 = rowSums(select(., FF1_Page_Submit_cfa34_2, FF2_Page_Submit_cfa34_2, FF2_Page_Submit_cfa34_2, FF4_Page_Submit_cfa34_2)),
         fairnessFRZ_2 = rowSums(select(., FF5_Page_Submit_cfa34_2, FF6_Page_Submit_cfa34_2, FF7_Page_Submit_cfa34_2)),
         reliabilityFRZ_2 = rowSums(select(., FF9_Page_Submit_cfa34_2, FF10_Page_Submit_cfa34_2, FF11_Page_Submit_cfa34_2, FF12_Page_Submit_cfa34_2)),
         invarianzFRZ_2 = rowSums(select(., FF13_Page_Submit_cfa34_2, FF14_Page_Submit_cfa34_2, FF15_Page_Submit_cfa34_2))
  )




# Vektor mit Variablen die geglättet werden sollen (cfa34_2 FF Klick Count) -------------
names_cfa34_2_FFClick_Count <- names(select(data_final,
                                            FF1_Click_Count_cfa34_2, FF2_Click_Count_cfa34_2, FF2_Click_Count_cfa34_2, FF4_Click_Count_cfa34_2,
                                            FF5_Click_Count_cfa34_2, FF6_Click_Count_cfa34_2, FF7_Click_Count_cfa34_2,
                                            FF9_Click_Count_cfa34_2, FF10_Click_Count_cfa34_2, FF11_Click_Count_cfa34_2, FF12_Click_Count_cfa34_2,
                                            FF13_Click_Count_cfa34_2,FF14_Click_Count_cfa34_2, FF15_Click_Count_cfa34_2)
)

# Ausgewählte Variablen (FF Klick Count cfa34_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_cfa34_2_FFClick_Count)

# Variablen FF Klick Count für CFA3/4 Faktoren generieren
data_final <- data_final %>%
  mutate(mtmmFKlick_2 = rowSums(select(., FF1_Click_Count_cfa34_2, FF2_Click_Count_cfa34_2, FF2_Click_Count_cfa34_2, FF4_Click_Count_cfa34_2)),
         fairnessFKlick_2 = rowSums(select(., FF5_Click_Count_cfa34_2, FF6_Click_Count_cfa34_2, FF7_Click_Count_cfa34_2)),
         reliabilityFKlick_2 = rowSums(select(., FF9_Click_Count_cfa34_2, FF10_Click_Count_cfa34_2, FF11_Click_Count_cfa34_2, FF12_Click_Count_cfa34_2)),
         invarianzFKlick_2 = rowSums(select(., FF13_Click_Count_cfa34_2,FF14_Click_Count_cfa34_2, FF15_Click_Count_cfa34_2))
  )



















# Relevante Faktor-Variablen (Summenscore) erstellen (Krit_1) Kriteriumsorientiert -----
# 1. Validität, 2. Cutoff, 3. Reliabilität
data_final <- data_final %>%
  mutate(validity_1 = rowSums(select(., F2_Krit_1, F3_Krit_1, F4_Krit_1, F5_Krit_1, F6_Krit_1)),
         cutoff_1 = rowSums(select(., F7_Krit_1, F8_Krit_1, F9_Krit_1, F10_Krit_1)),
         reliability_krit_1 = rowSums(select(., F1_Krit_1, F11_Krit_1))
  )

# Vektor mit Variablen die geglättet werden sollen (Krit_1 Page Submit) -------------
names_Krit_1_Page_Submit <- names(select(data_final,
                                         F2_Page_Submit_Krit_1, F3_Page_Submit_Krit_1, F4_Page_Submit_Krit_1, F5_Page_Submit_Krit_1, F6_Page_Submit_Krit_1,
                                         F7_Page_Submit_Krit_1, F9_Page_Submit_Krit_1, F10_Page_Submit_Krit_1,
                                         F1_Page_Submit_Krit_1, F11_Page_Submit_Krit_1))

# Ausgewählte Variablen (Page Submit Krit_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_Krit_1_Page_Submit)

# Variablen Page Submit für Kriteriumsorientiert Faktoren generieren
data_final <- data_final %>%
  mutate(validityRZ_1 = rowSums(select(., F2_Page_Submit_Krit_1, F3_Page_Submit_Krit_1, F4_Page_Submit_Krit_1, F5_Page_Submit_Krit_1, F6_Page_Submit_Krit_1)),
         cutoffRZ_1 = rowSums(select(., F7_Page_Submit_Krit_1, F9_Page_Submit_Krit_1, F10_Page_Submit_Krit_1)),
         reliability_kritRZ_1 = rowSums(select(., F1_Page_Submit_Krit_1, F11_Page_Submit_Krit_1))
  )


# Vektor mit Variablen die geglättet werden sollen (Krit_1 Klick Count) -------------
names_Krit_1_Click_Count <- names(select(data_final,
                                         F2_Click_Count_Krit_1, F3_Click_Count_Krit_1, F4_Click_Count_Krit_1, F5_Click_Count_Krit_1, F6_Click_Count_Krit_1,
                                         F7_Click_Count_Krit_1, F9_Click_Count_Krit_1, F10_Click_Count_Krit_1,
                                         F1_Click_Count_Krit_1, F11_Click_Count_Krit_1))

# Ausgewählte Variablen (Klick Count Krit_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_Krit_1_Click_Count)

# Variablen Page Submit für Kriteriumsorientiert Faktoren generieren
data_final <- data_final %>%
  mutate(validityKlick_1 = rowSums(select(., F2_Click_Count_Krit_1, F3_Click_Count_Krit_1, F4_Click_Count_Krit_1, F5_Click_Count_Krit_1, F6_Click_Count_Krit_1)),
         cutoffKlick_1 = rowSums(select(., F7_Click_Count_Krit_1, F9_Click_Count_Krit_1, F10_Click_Count_Krit_1)),
         reliability_kritKlick_1 = rowSums(select(., F1_Click_Count_Krit_1, F11_Click_Count_Krit_1))
  )

# Vektor mit Variablen die geglättet werden sollen (Krit_1 FF Page Submit) -------------
names_Krit_1_FFPage_Submit <- names(select(data_final,
                                           FF2_Page_Submit_Krit_1, FF3_Page_Submit_Krit_1, FF4_Page_Submit_Krit_1, FF5_Page_Submit_Krit_1, FF6_Page_Submit_Krit_1,
                                           FF7_Page_Submit_Krit_1, FF9_Page_Submit_Krit_1, FF10_Page_Submit_Krit_1,
                                           FF1_Page_Submit_Krit_1))

# Ausgewählte Variablen (FF Page Submit Krit_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_Krit_1_FFPage_Submit)

# Variablen FF Page Submit für Kriteriumsorientiert Faktoren generieren
data_final <- data_final %>%
  mutate(validityFRZ_1 = rowSums(select(., FF2_Page_Submit_Krit_1, FF3_Page_Submit_Krit_1, FF4_Page_Submit_Krit_1, FF5_Page_Submit_Krit_1, FF6_Page_Submit_Krit_1)),
         cutoffFRZ_1 = rowSums(select(., FF7_Page_Submit_Krit_1, FF9_Page_Submit_Krit_1, FF10_Page_Submit_Krit_1)),
         reliability_kritFRZ_1 = rowSums(select(., FF1_Page_Submit_Krit_1))
  )

# Vektor mit Variablen die geglättet werden sollen (Krit_1 FF Klick Count) -------------
names_Krit_1_FFClick_Count <- names(select(data_final,
                                           FF2_Click_Count_Krit_1, FF3_Click_Count_Krit_1, FF4_Click_Count_Krit_1, FF5_Click_Count_Krit_1, FF6_Click_Count_Krit_1,
                                           FF7_Click_Count_Krit_1, FF9_Click_Count_Krit_1, FF10_Click_Count_Krit_1,
                                           FF1_Click_Count_Krit_1))

# Ausgewählte Variablen (FF Klick Count Krit_1) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_Krit_1_FFClick_Count)

# Variablen Page Submit für Kriteriumsorientiert Faktoren generieren
data_final <- data_final %>%
  mutate(validityFKlick_1 = rowSums(select(., FF2_Click_Count_Krit_1, FF3_Click_Count_Krit_1, FF4_Click_Count_Krit_1, FF5_Click_Count_Krit_1, FF6_Click_Count_Krit_1)),
         cutoffFKlick_1 = rowSums(select(., FF7_Click_Count_Krit_1, FF9_Click_Count_Krit_1, FF10_Click_Count_Krit_1)),
         reliability_kritFKlick_1 = rowSums(select(., FF1_Click_Count_Krit_1))
  )















# Relevante Faktor-Variablen (Summenscore) erstellen (Krit_2) Kriteriumsorientiert -----
# 1. Validität, 2. Cutoff, 3. Reliabilität
data_final <- data_final %>%
  mutate(validity_2 = rowSums(select(., F2_Krit_2, F3_Krit_2, F4_Krit_2, F5_Krit_2, F6_Krit_2)),
         cutoff_2 = rowSums(select(., F7_Krit_2, F8_Krit_2, F9_Krit_2, F10_Krit_2)),
         reliability_krit_2 = rowSums(select(., F1_Krit_2, F11_Krit_2))
  )

# Vektor mit Variablen die geglättet werden sollen (Krit_2 Page Submit) -------------
names_Krit_2_Page_Submit <- names(select(data_final,
                                         F2_Page_Submit_Krit_2, F3_Page_Submit_Krit_2, F4_Page_Submit_Krit_2, F5_Page_Submit_Krit_2, F6_Page_Submit_Krit_2,
                                         F7_Page_Submit_Krit_2, F9_Page_Submit_Krit_2, F10_Page_Submit_Krit_2,
                                         F1_Page_Submit_Krit_2, F11_Page_Submit_Krit_2))

# Ausgewählte Variablen (Page Submit Krit_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_Krit_2_Page_Submit)

# Variablen Page Submit für Kriteriumsorientiert Faktoren generieren
data_final <- data_final %>%
  mutate(validityRZ_2 = rowSums(select(., F2_Page_Submit_Krit_2, F3_Page_Submit_Krit_2, F4_Page_Submit_Krit_2, F5_Page_Submit_Krit_2, F6_Page_Submit_Krit_2)),
         cutoffRZ_2 = rowSums(select(., F7_Page_Submit_Krit_2, F9_Page_Submit_Krit_2, F10_Page_Submit_Krit_2)),
         reliability_kritRZ_2 = rowSums(select(., F1_Page_Submit_Krit_2, F11_Page_Submit_Krit_2))
  )


# Vektor mit Variablen die geglättet werden sollen (Krit_2 Klick Count) -------------
names_Krit_2_Click_Count <- names(select(data_final,
                                         F2_Click_Count_Krit_2, F3_Click_Count_Krit_2, F4_Click_Count_Krit_2, F5_Click_Count_Krit_2, F6_Click_Count_Krit_2,
                                         F7_Click_Count_Krit_2, F9_Click_Count_Krit_2, F10_Click_Count_Krit_2,
                                         F1_Click_Count_Krit_2, F11_Click_Count_Krit_2))

# Ausgewählte Variablen (Klick Count Krit_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_Krit_2_Click_Count)

# Variablen Page Submit für Kriteriumsorientiert Faktoren generieren
data_final <- data_final %>%
  mutate(validityKlick_2 = rowSums(select(., F2_Click_Count_Krit_2, F3_Click_Count_Krit_2, F4_Click_Count_Krit_2, F5_Click_Count_Krit_2, F6_Click_Count_Krit_2)),
         cutoffKlick_2 = rowSums(select(., F7_Click_Count_Krit_2, F9_Click_Count_Krit_2, F10_Click_Count_Krit_2)),
         reliability_kritKlick_2 = rowSums(select(., F1_Click_Count_Krit_2, F11_Click_Count_Krit_2))
  )

# Vektor mit Variablen die geglättet werden sollen (Krit_2 FF Page Submit) -------------
names_Krit_2_FFPage_Submit <- names(select(data_final,
                                           FF2_Page_Submit_Krit_2, FF3_Page_Submit_Krit_2, FF4_Page_Submit_Krit_2, FF5_Page_Submit_Krit_2, FF6_Page_Submit_Krit_2,
                                           FF7_Page_Submit_Krit_2, FF9_Page_Submit_Krit_2, FF10_Page_Submit_Krit_2,
                                           FF1_Page_Submit_Krit_2))

# Ausgewählte Variablen (FF Page Submit Krit_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_Krit_2_FFPage_Submit)

# Variablen FF Page Submit für Kriteriumsorientiert Faktoren generieren
data_final <- data_final %>%
  mutate(validityFRZ_2 = rowSums(select(., FF2_Page_Submit_Krit_2, FF3_Page_Submit_Krit_2, FF4_Page_Submit_Krit_2, FF5_Page_Submit_Krit_2, FF6_Page_Submit_Krit_2)),
         cutoffFRZ_2 = rowSums(select(., FF7_Page_Submit_Krit_2, FF9_Page_Submit_Krit_2, FF10_Page_Submit_Krit_2)),
         reliability_kritFRZ_2 = rowSums(select(., FF1_Page_Submit_Krit_2))
  )

# Vektor mit Variablen die geglättet werden sollen (Krit_2 FF Klick Count) -------------
names_Krit_2_FFClick_Count <- names(select(data_final,
                                           FF2_Click_Count_Krit_2, FF3_Click_Count_Krit_2, FF4_Click_Count_Krit_2, FF5_Click_Count_Krit_2, FF6_Click_Count_Krit_2,
                                           FF7_Click_Count_Krit_2, FF9_Click_Count_Krit_2, FF10_Click_Count_Krit_2,
                                           FF1_Click_Count_Krit_2))

# Ausgewählte Variablen (FF Klick Count Krit_2) glätten und in data_final speichern
data_final <- data_final %>% flaten_extreme_values(names_Krit_2_FFClick_Count)

# Variablen Page Submit für Kriteriumsorientiert Faktoren generieren
data_final <- data_final %>%
  mutate(validityFKlick_2 = rowSums(select(., FF2_Click_Count_Krit_2, FF3_Click_Count_Krit_2, FF4_Click_Count_Krit_2, FF5_Click_Count_Krit_2, FF6_Click_Count_Krit_2)),
         cutoffFKlick_2 = rowSums(select(., FF7_Click_Count_Krit_2, FF9_Click_Count_Krit_2, FF10_Click_Count_Krit_2)),
         reliability_kritFKlick_2 = rowSums(select(., FF1_Click_Count_Krit_2))
  )

write_csv(data_final, "data_final_alle_vars_2021.csv")
