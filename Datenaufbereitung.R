# load packages
library(tidyverse)
library(readxl)
library(dplyr)
library(texreg)
library(MatchIt)
library(cobalt)

#import data
Daten <- read_csv("data_final_2020_2021_kombi.csv")

t.test(Daten$Probetest~Daten$Jahr)

t.test(Daten$verstanden_irt12~Daten$Jahr)
t.test(Daten$verstanden_irt34~Daten$Jahr)
t.test(Daten$verstanden_cfa12~Daten$Jahr)
t.test(Daten$verstanden_cfa34~Daten$Jahr)
t.test(Daten$verstanden_Krit~Daten$Jahr)

t.test(Daten$VWBerechnung~Daten$Jahr)
t.test(Daten$VWIndividual~Daten$Jahr)
t.test(Daten$VWTheorie~Daten$Jahr)

##Lernzeit während SEmester
library(psych)
describe(Daten$Stunden)
a<-order(Daten$Stunden)
Daten$Stunden[a]

Daten$Stunden[Daten$Stunden>201]<-NA
t.test(Daten$Stunden~Daten$Jahr)

#look at number of casses per year and number of repetitions in the first set of excersises
table(Daten$Jahr)
table(Daten$sum_oft,Daten$Jahr)
t.test(Daten$sum_oft~Daten$Jahr)

t.test(Daten$Relevanz~Daten$Jahr)
t.test(Daten$Vorlesung~Daten$Jahr)

t.test(Daten$Pflichtlit~Daten$Jahr)
table(Daten$Pflichtlit,Daten$Jahr)
cor.test(Daten$Pflichtlit,Daten$Probetest)

t.test(Daten$Fragen~Daten$Jahr)
t.test(Daten$Forum~Daten$Jahr)

##add data of second excersies
data_2termine <- read_csv("data_2021_2_termine.csv")

#calculate how many of the second exercises were solved
Wiederholungen<-cbind(data_2termine$num_pseudo_irt12_2,data_2termine$num_pseudo_irt34_2,data_2termine$num_pseudo_cfa12_2,data_2termine$num_pseudo_cfa34_2,data_2termine$num_pseudo_Krit_2)
#Wiederholungen2<-cbind(data_2termine$num_pseudo_sb_2,data_2termine$num_pseudo_irt12_2,data_2termine$num_pseudo_irt34_2,data_2termine$num_pseudo_cfa12_2,data_2termine$num_pseudo_cfa34_2,data_2termine$num_pseudo_Krit_2)




colnames(Wiederholungen)<-c("irt12_2","irt34_2","cfa12_2","cfa34_2","krit_2")
Rep2<-rowSums(Wiederholungen,na.rm=TRUE)
#Rep3<-rowSums(Wiederholungen2,na.rm=TRUE)
#plot(Rep2,Rep3)
#cor(Rep2,Rep3)

NAS<-rowSums(is.na(Wiederholungen))
#NAS2<-rowSums(is.na(Wiederholungen2))
data_2termine$NAS<-5-NAS
#data_2termine$NAS2<-6-NAS2
data_2termine$Rep2<-Rep2

table(data_2termine$NAS,data_2termine$NAS2)

##When more three or more of the excercises were solved, the student is a solver
data_2termine$solver<-data_2termine$NAS>3
#data_2termine$solver<-data_2termine$NAS2>4

##match data of second with data of first exercise
Total<-merge(x=Daten,y=data_2termine,by="Pseudo",all.x=TRUE)
Total[is.na(Total$Rep2),"Rep2"] <- 0
t.test(Total$Rep2~Total$Jahr)
Total$Rep<-Total$Rep2+Total$sum_oft
t.test(Total$Rep~Total$Jahr)
table(Total$Jahr,Total$Rep)

##kategorisieren der Varialbe Rep
##5-10; 11-15, 16-20,21-25,26-30,30+
Total$Rep=cut(Total$Rep,c(0,10,15,20,25,30,100),include.lowest=TRUE)
table(Total$Jahr,Total$Rep)
table(Total$Jahr)

#rechtzeitig
table(Total$Jahr,Total$rechtzeit)
t.test(Total$rechtzeit~Total$Jahr)
cor.test(Total$Probetest,Total$rechtzeit)

t.test(Total$Muhe~Total$Jahr)

###all students of 2020Rep###all students of 2020 are non-solvers, replace NA by FALSE
##and NAS (number of additional taks solved) by 0
Total[is.na(Total$solver),"solver"] <- FALSE 
Total[is.na(Total$NAS),"NAS"] <- 0 
table(Total$Jahr,Total$solver)

##nur 25 Leute im HS21 haben nicht alle zweiten Übungsdurchgänge gelöst

#Daten<-Total[,c("Pseudo","solver","NAS","Jahr","Probetest","lokalestoch","rest","item_testinfo","r_tests",
#                "adaptiv_testen","freiheitsgrade","r_modelierung","r_modelvergleich",
#                "reliability","invarianz","mtmm","fairness","reliability_krit","validity","cutoff","lokalestochRZ","restRZ","item_testinfoRZ","r_testsRZ",
#                "adaptiv_testenRZ","freiheitsgradeRZ","r_modelierungRZ","r_modelvergleichRZ",
#                "reliabilityRZ","invarianzRZ","mtmmRZ","fairnessRZ","reliability_kritRZ","validityRZ","cutoffRZ","lokalestochFRZ","restFRZ","item_testinfoFRZ","r_testsFRZ",
#                "adaptiv_testenFRZ","freiheitsgradeFRZ","r_modelierungFRZ","r_modelvergleichFRZ",
#                "reliabilityFRZ","invarianzFRZ","mtmmFRZ","fairnessFRZ","reliability_kritFRZ","validityFRZ","cutoffFRZ")]

#####Ab Pflichtlit habe ich die ganzen Variablen hinzugefügt, kann das so noch stimmen?
##alte Version
##imputation muss hier auch noch folgen,dafür muss ich bestimmen, welche Variablen ich für die erste Hypothese alle brauche
##Ausserdem war Muhe bisher falsch!! Ich habe für 2020 nur erste Messung genommen

Daten<-Total[,c("Pseudo","VWBerechnung","VWTheorie","VWIndividual","solver","NAS","Jahr","Probetest","lokalestoch","rest","item_testinfo","r_tests",
                "adaptiv_testen","freiheitsgrade","r_modelierung","r_modelvergleich",
                "reliability","invarianz","mtmm","fairness","reliability_krit","validity","cutoff","lokalestochRZ","restRZ","item_testinfoRZ","r_testsRZ",
                "adaptiv_testenRZ","freiheitsgradeRZ","r_modelierungRZ","r_modelvergleichRZ",
                "reliabilityRZ","invarianzRZ","mtmmRZ","fairnessRZ","reliability_kritRZ","validityRZ","cutoffRZ","lokalestochFRZ","restFRZ","item_testinfoFRZ","r_testsFRZ",
                "adaptiv_testenFRZ","freiheitsgradeFRZ","r_modelierungFRZ","r_modelvergleichFRZ",
                "reliabilityFRZ","invarianzFRZ","mtmmFRZ","fairnessFRZ","reliability_kritFRZ","validityFRZ","cutoffFRZ","Rep","sum_oft","F1_sb","F2_sb", "F3_sb", "F3i_sb", "F3ii_sb", "F4_sb", "F5_sb",
                "F6_sb", "F7_sb", "F8_sb", "F9_sb", "F10_sb", "F11_sb", "F12_sb",
                "F13_sb", "F14_sb", "F16_sb", "F17_sb",  "F17i_sb", "F18_sb",
                "F19_sb", "F20_sb", "F21_sb", "F23_sb", "F24_sb", "F25_sb",
                "F26_sb", "F27_sb","F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25","verstanden_irt12",
                "verstanden_irt34","verstanden_cfa12","verstanden_cfa34","verstanden_Krit","Relevanz","Stunden")]




##imputation muss hier auch noch folgen,dafür muss ich bestimmen, welche Variablen ich für die erste Hypothese alle brauche
Daten<-Total[,c("Pseudo","VWBerechnung","VWTheorie","VWIndividual","solver","NAS","Jahr","Probetest","lokalestoch","rest","item_testinfo","r_tests",
                "adaptiv_testen","freiheitsgrade","r_modelierung","r_modelvergleich",
                "reliability","invarianz","mtmm","fairness","reliability_krit","validity","cutoff","lokalestochRZ","restRZ","item_testinfoRZ","r_testsRZ",
                "adaptiv_testenRZ","freiheitsgradeRZ","r_modelierungRZ","r_modelvergleichRZ",
                "reliabilityRZ","invarianzRZ","mtmmRZ","fairnessRZ","reliability_kritRZ","validityRZ","cutoffRZ","lokalestochFRZ","restFRZ","item_testinfoFRZ","r_testsFRZ",
                "adaptiv_testenFRZ","freiheitsgradeFRZ","r_modelierungFRZ","r_modelvergleichFRZ",
                "reliabilityFRZ","invarianzFRZ","mtmmFRZ","fairnessFRZ","reliability_kritFRZ","validityFRZ","cutoffFRZ","Rep","sum_oft","F1_sb","F2_sb", "F3_sb", "F3i_sb", "F3ii_sb", "F4_sb", "F5_sb",
                "F6_sb", "F7_sb", "F8_sb", "F9_sb", "F10_sb", "F11_sb", "F12_sb",
                "F13_sb", "F14_sb", "F16_sb", "F17_sb",  "F17i_sb", "F18_sb",
                "F19_sb", "F20_sb", "F21_sb", "F23_sb", "F24_sb", "F25_sb",
                "F26_sb", "F27_sb","F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","F11","F12","F13","F14","F15","F16","F17","F18","F19","F20","F21","F22","F23","F24","F25","verstanden_irt12",
                "verstanden_irt34","verstanden_cfa12","verstanden_cfa34","verstanden_Krit","Relevanz","Stunden","Pflichtlit_irt12","Pflichtlit_irt34","Pflichtlit_cfa12","Pflichtlit_cfa34","Pflichtlit_Krit","Vorlesung_irt12","Vorlesung_irt34","Vorlesung_cfa12","Vorlesung_cfa34","Vorlesung_Krit","num_fragen_sb",
                "num_fragen_irt12","num_fragen_irt34","num_fragen_cfa12","num_fragen_cfa34","num_fragen_Krit","rechtzeitig_sb","rechtzeitig_irt12","rechtzeitig_irt34",
                "rechtzeitig_cfa12","rechtzeitig_cfa34","rechtzeitig_Krit","Muhe_sb","Muhe_irt12","Muhe_irt34","Muhe_cfa12","Muhe_cfa34","Muhe_Krit")]




max(colSums(is.na(Daten)))
Daten$na<-rowSums(is.na(Daten))
table(Daten$na)
table(Total$Stunden)

a<-is.na(Daten)
b<-!is.na(Daten)
table(Daten$na)

##impute missing values as matching needs complete datasets, die Variab
##Hier muss man noch die Note herausholen, die sollte nicht imputiert werden
library(mice)
set.seed(123)
imputed_Data <- mice(Daten, m=1, maxit = 50, method = 'pmm', seed = 500)
Dat<-complete(imputed_Data)
Kopie<-Dat

theoryRZ<-Total$theoryRZ
theoryFRZ<-Total$theoryFRZ
practiceRZ<-Total$practiceRZ
practiceFRZ<-Total$practiceFRZ

Note<-Total$Note
Muhe<-Total$Muhe
rechtzeit<-Total$rechtzeit
#Relevanz<-Total$Relevanz
Vorlesung<-Total$Vorlesung
Forum_lesen<-Total$Forum_lesen
Pflichtlit<-Total$Pflichtlit
Fragen<-Total$Fragen
sum_oft<-Total$sum_oft
Rep<-as.numeric(Total$Rep)

BerechnungenRZ<-Total$BerechnungenRZ
BerechnungenFRZ<-Total$BerechnungenFRZ
TheorieRZ<-Total$TheorieRZ
TheorieFRZ<-Total$TheorieFRZ

#Stunden<-Total$Stunden

t.test(Daten$Relevanz~Daten$Jahr)

StUB<-Total$StUB

Daten<-cbind(Dat,Note,Muhe,rechtzeit,Vorlesung,Forum_lesen,Pflichtlit,Fragen,Rep,StUB,theoryRZ,theoryFRZ,practiceRZ,practiceFRZ,BerechnungenRZ,BerechnungenFRZ,TheorieRZ,TheorieFRZ)

cor(Daten[,c(2:121)])
##latente Variablen berechnen
attach(Daten)


Daten$Termin1<-lokalestoch + rest
Daten$Termin2<-item_testinfo + r_tests+adaptiv_testen
Daten$Termin3<-freiheitsgrade+r_modelierung+r_modelvergleich
Daten$Termin4<-mtmm+fairness+reliability+invarianz
Daten$Termin5<-reliability_krit+validity+cutoff
Daten$Leistungman<-Daten$Termin1 +Daten$Termin2 + Daten$Termin3+Daten$Termin4+Daten$Termin5
Daten$Vorwissen<-Daten$VWBerechnung+Daten$VWIndividual+Daten$VWTheorie


t.test(Daten$Leistungma~Daten$Jahr)

Sicherheit<-Daten

Daten<-Sicherheit

write_csv(Sicherheit, "Sicherheit.csv")
