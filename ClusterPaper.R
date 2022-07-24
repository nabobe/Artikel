#Daten<-Dat
Daten<-data_match
table(Daten$StUB)##das ist falsch codiert, die meisten haben an der Fragerunde NIE teilgenommen, also 6 sollte 0 sein
Daten$StUB<-7-Daten$StUB
table(Daten$Vorlesung)

library(rstatix)
cohens_d(Daten,Probetest ~ solver, paired = FALSE)
cohens_d(Daten,ilu ~ solver, paired = FALSE)

Daten$Vorwissen<-Daten$Berechnungen+Daten$Theorie
t.test(Daten$Probetest~Daten$solver)
t.test(Daten$Vorwissen~Daten$solver)
t.test(Daten$Berechnungen~Daten$solver)
t.test(Daten$Theorie~Daten$solver)

aggregate(Daten$Probetest,by=list(cluster=Daten$solver,grpsKM), sd)
aggregate(Daten$Theorie,by=list(cluster=Daten$solver), sd)

Daten<-cbind(Daten,ilu)


aggregate(Daten$ilu,by=list(cluster=Daten$solver), sd)
aggregate(Daten$Theorie,by=list(cluster=Daten$solver), sd)


Daten$Leistung<-Daten$Leistungma

t.test(Daten$Leistung~Daten$solver)


#Daten<-Daten[,c("Note","Probetest","Leistung","Vorwissen","verstanden","RZ","FRZ","Muhe","rechtzeit","Relevanz","Vorlesung","Forum_lesen","Pflichtlit","Fragen","sum_oft","solver","Rep")]

Dat1<-Daten[,c("Leistungman","verstanden","RZ","FRZ","Muhe","rechtzeit","Vorlesung","Forum_lesen","Pflichtlit","Fragen","Vorwissen","Relevanz","Stunden")]

#Dat<-Daten[,c("Leistung","verstanden","RZ","Muhe","rechtzeit","Vorlesung","Forum_lesen","Pflichtlit","Fragen")]


#Leistung (Vorwissen, Leistung, verstanden, RZ , FRZ), Motivation (RZ, FRZ, Muhe, Relevanz), Zeitmanagement (rechtzeit, Stunden)

cor(Dat1,use="complete.obs")

##NA müssen aus Stunden raus

a<-Daten$Vorlesung>1.5

Daten[a,"Vorlesung"]<-1.5



#a<-Daten$Fragen>2.5
#Daten[a,"Fragen"]<-2.5

mean(Daten[,"Vorlesung"])

Daten$verstanden<-0-Daten$verstanden

table(Daten$Jahr,Daten$solver)

Daten$Rep<-as.numeric(Daten$Rep)

Dat<-Daten[,c("Leistungman","verstanden","RZ","FRZ","Muhe","rechtzeit","Vorlesung","Forum_lesen","Pflichtlit","Fragen","Vorwissen","Relevanz","Stunden")]
#Dat<-Daten[,c("LKW","verstanden","RZ","FRZ","Muhe","rechtzeit","Vorlesung","Forum_lesen","Pflichtlit","Fragen","Vorwissen","Relevanz","Stunden")]
Dat<-Daten[,c("Leistungman","verstanden","RZ","FRZ","Muhe","rechtzeit","Vorlesung","Forum_lesen","Pflichtlit","Fragen","Vorwissen","Relevanz","Stunden")]

describe(Daten$Probetest)
describe(Daten$Rep)
describe(Daten$Vorlesung)

Dat$Vorlesung<-5-Dat$Vorlesung

Dat$Fragen<-log(Dat$Fragen+1)
Dat$Stunden<-log(Dat$Stunden+1)

describe(Dat$Fragen)

t.test(Dat$Leistungman~Daten$solver)
t.test(Dat$rechtzeit~Daten$solver)

t.test(Dat$Vorlesung~Daten$solver)
t.test(Dat$Forum_lesen~Daten$solver)
t.test(Dat$Pflichtlit~Daten$solver)
t.test(Dat$Fragen~Daten$solver)

t.test(Dat$RZ~Daten$solver)
t.test(Dat$FRZ~Daten$solver)
t.test(Dat$Fragen~Daten$solver)

t.test(Dat$Muhe~Daten$solver)
t.test(Dat$Relevanz~Daten$solver)
t.test(Dat$Stunden~Daten$solver)

table(Daten$solver,Dat$verstanden)
table(Daten$solver,Dat$RZ)
table(Daten$solver,Dat$Muhe)
table(Daten$solver,Dat$rechtzeit)
table(Daten$solver,Dat$Vorlesung)
table(Daten$solver,Dat$Forum_lesen)
table(Daten$solver,Dat$Pflichtlit)
table(Daten$solver,Dat$Fragen)
table(Daten$solver,Dat$Vorwissen)
table(Daten$solver,Dat$Relevanz)
table(Daten$solver,Dat$Stunden)
table(Daten$solver,Daten$Berechnungen)
table(Daten$solver,Daten$Theorie)



table(Daten$solver,Dat$Leistungman)
table(Daten$solver,Dat$verstanden)
table(Daten$solver,Dat$RZ)
table(Daten$solver,Dat$Muhe)
table(Daten$solver,Dat$rechtzeit)
table(Daten$solver,Dat$Vorlesung)
table(Daten$solver,Dat$Forum_lesen)
table(Daten$solver,Dat$Pflichtlit)
table(Daten$solver,Dat$Fragen)
table(Daten$solver,Dat$Vorwissen)
table(Daten$solver,Dat$Relevanz)
table(Daten$solver,Dat$Stunden)
table(Daten$solver,Daten$Berechnungen)
table(Daten$solver,Daten$Theorie)

hist(Dat$RZ)
hist(Dat$FRZ)
hist(Dat$Vorwissen)
hist(Dat$verstanden)
hist(Dat$Muhe)
hist(Dat$rechtzeit)
hist(Dat$Vorlesung)
hist(Dat$Forum_lesen)
hist(Dat$Pflichtlit)
hist(Dat$Fragen)
hist(Dat$Stunden)
hist(Dat$Relevanz)
hist(Dat$Stunden)







library(psych)
describe(Dat)



#Die Verteilung der Vorlesung geht so nicht
#Dat$Vorlesung<-log(Dat$Vorlesung)



hist(Dat$Fragen)
hist(Dat$Stunden)

describe(Dat)
B<-cbind(Dat,Probetest)
corr.test(B)

####hier kommen die ganzen t-test hin (Jonas)
t.test(Dat$Leistungman~Daten$solver)
t.test(Daten$Rep~Daten$solver)
t.test(Daten$RZ~Daten$solver)
t.test(Daten$Vorwissen~Daten$solver)
t.test(Daten$Probetest~Daten$solver)
t.test(Dat$verstanden~Daten$solver)
t.test(Dat$rechtzeit~Daten$solver)
t.test(Dat$Vorlesung~Daten$solver)
t.test(Dat$Forum_lesen~Daten$solver)
t.test(Dat$Pflichtlit~Daten$solver)
t.test(Dat$Fragen~Daten$solver)
t.test(Dat$Relevanz~Daten$solver)
t.test(Dat$Stunden~Daten$solver)
t.test(RZ~Daten$solver)
t.test(MR2~Daten$solver)

m <- lm(Leistungman ~ solver)
solver<-Daten$solver

m <- lm(Daten$Leistungman ~ ilu*solver)
m <- lm(Daten$verstanden ~ ilu*solver)
m <- lm(Daten$Probetest ~ solver)
m <- lm(Daten$Probetest ~ ilu*solver)
m <- lm(Fragen ~ ilu*solver)
m <- lm(Pflichtlit ~ ilu*solver)
m <- lm(Daten$RZ ~ ilu*solver)
m <- lm(Daten$RZ ~ solver)
m <- lm(Probetest ~ GrL*solver, data = Test)
summary(m)
anova(m)


describe(Daten$Probetest)

##Dat$Stunden<-as.numeric(cut(Daten$Stunden,breaks=c(0,10,20,30,40,50,60,70,80,90,100,500)))


library(lavaan)
library(psych)
describe(Dat)
cor(Dat)

table(Daten$Jahr,Daten$solver)

library(effectsize)

library(sjPlot)
library(sjmisc)
library(ggplot2)

##Vorwissen variablen in 3 ähnlich grosse gruppen unterteilen, ANOVA rechen
GrL<-cut_number(Dat$Leistung,2)
GrL<-cut_number(Dat$Muhe,2)
GrL<-cut_number(Dat$verstanden,2)
GrL<-cut_number(Dat$Vorwissen,2)
GrL<-cut_number(Daten$Theorie,2)
GrL<-cut_number(Daten$Vorlesung,2)
GrL<-cut_number(Dat$RZ,3)
GrL<-cut_number(MR2,3)
GrL<-cut_number(Dat$FRZ,2)
GrL<-cut_number(Daten$Forum_lesen,2)
GrL<-cut_number(Daten$Pflichtlit,2)
GrL<-cut_number(Daten$Fragen,2)
GrL<-cut_number(Daten$Relevanz,2)
GrL<-cut_number(Daten$Stunden,2)
GrL<-cut_number(Daten$rechtzeit,2)

GrL<-cut_number(MR1,3)
GrL<-cut_number(MR3,3)
GrL<-cut_number(MR2,3)


table(GrL,Daten$solver)
Chi<-chisq.test(GrL,Daten$Jahr)
Chi

GrL<-as.numeric(GrL)
#GrL<-as.factor(GrL)

solver<-Daten$solver
SB1<-Daten$SB1
Probetest<-Daten$Probetest

Test<-cbind(GrL,Dat,solver,Probetest)
#Test<-cbind(Test,Daten$SB1)

Test$solver<-as.factor(Test$solver)

m <- lm(Probetest ~ as.factor(GrL)*as.factor(solver), data = Test)
m <- lm(Probetest ~ GrL*solver, data = Test)
summary(m)
anova(m)
library("rstatix")
eta_squared(m)

time_investment_group<-GrL
time_investment_group<-as.factor(time_investment_group)
solver<-Daten$solver
solver<-as.factor(solver)

levels(time_investment_group)<-c("low","average","high")

solver<-as.factor(solver)

levels(solver)<-c("non-solver","solver")

grpsKM<-as.factor(grpsKM)

levels(grpsKM)<-c("low-performance/low-investment","high-performance/medium,low-investment","high-performance/high-investment")
additional_exercises<-solver

cluster<-grpsKM

knowledge_test<-Probetest

m <- lm(knowledge_test ~ additional_exercises*cluster)
anova(m)

plot_model(m,type = "pred", terms = c("additional_exercises","cluster"))





additional_exercises<-solver


Test<-cbind(time_investment_group,Dat,solver,Probetest,knowledge_test,additional_exercises)

m <- lm(knowledge_test ~ time_investment_group*additional_exercises, data = Test)

plot_model(m,type = "pred", terms = c("additional_exercises", "time_investment_group"))



##evtl hier noch schauen ob Anstieg im Vorwissen (erster zweiter Test) Kovariate ist? Kann man das aufholen? Arbeit für Jonas. Auch Durchgänge raus die erst nach Probetest



# displaying the result table with summary()
summary(post.hoc)



#table(cut(Dat$Vorlesung,c(1,1.5,2,2.5,3,3.5,4),include.lowest=TRUE))
#A<-cut(Dat$Vorlesung,c(1,1.5,2,2.5,3,3.5,4),include.lowest=TRUE)
#A<-as.numeric(A)
#describe(A)

#Dat<-Daten[,c("Leistung","verstanden","RZ","FRZ","Muhe","rechtzeit")]

#keine binären Daten
table(Dat$rechtzeit)
table(Dat$Vorlesung)
table(Dat$Forum_lesen) #0 bedeutet gar nie angeschaut
table(Dat$Pflichtlit)
table(Dat$Fragen)



describe(Dat)

# attach(Dat)
# A<-cbind(Vorlesung,Forum_lesen,Pflichtlit,Fragen)
# 
# 
# #Vorlesung, Pflichtlit und Fragen haben extreme Verteilungen (2 gipflic z.B.)
# #Fragen und Vorlesung binär machen
# b<-Dat[,"Vorlesung"]==4
# Dat[,"Vorlesung"]<-b
# Dat[,"Vorlesung"]<-Dat[,"Vorlesung"]*1
# 
# #Pflichtlit
# b<-Dat[,"Pflichtlit"]==1
# Dat[,"Pflichtlit"]<-b
# Dat[,"Pflichtlit"]<-Dat[,"Pflichtlit"]*1
# 
# #Fragen
# b<-Dat[,"Fragen"]==0
# Dat[,"Fragen"]<-!b
# Dat[,"Fragen"]<-Dat[,"Fragen"]*1
# 
# 
# #Forum_lesen
# b<-Dat[,"Forum_lesen"]==0
# Dat[,"Forum_lesen"]<-!b
# Dat[,"Forum_lesen"]<-Dat[,"Forum_lesen"]*1
# 
# 
# Z<-cbind(Dat[,"Vorlesung"],Dat[,"Pflichtlit"],Dat[,"Fragen"],Dat[,"Forum_lesen"])
# table(rowSums(Z))
# 
# Dat$freiwillig<-rowSums(Z)

##deutliche Unterschiede zwischen 2020 un 2021, muss ich die innerhalb der Gruppen standardisierne?
Dat<-as.data.frame(Dat)

# 
# t.test(Dat$freiwillig~Dat1$Jahr)
# 
# t.test(Dat$Leistung~Dat1$Jahr)
# #t.test(Dat[,"Leistung"]~as.factor(Dat[,"Jahr"]))
# t.test(Dat$verstanden~Dat$Jahr)
# t.test(Dat1$RZ~Dat1$Jahr) ##sind diese Variablen wirklich identisch?
# t.test(Dat1$FRZ~Dat1$Jahr)
# t.test(Dat1$Muhe~Dat1$Jahr)
# t.test(Dat1$rechtzeit~Dat1$Jahr)
# t.test(Dat1$Vorlesung~Dat1$Jahr)
# t.test(Dat1$Forum_lesen~Dat1$Jahr)
# t.test(Dat1$Pflichtlit~Dat1$Jahr)
# t.test(Dat1$Fragen~Dat1$Jahr)
# t.test(Dat1$Vorwissen~Dat1$Jahr)
# t.test(Dat1$Relevanz~Dat1$Jahr)



##Zeit,Leistung,Selbstbericht


#Vorbereitung: 3 Indikatoren: verstanden, plichtlit, vorlesugn 
#Abruf: RZ,Muhe,Leistung,rechtzeit
#Nachberarbeiutn: FRZ,Forum_lesen, Fragen

##test for multivariat normal distributuion
##Vorwissen und Relevanz sind Kovariaten



#Fragen und Vorlesung binär machen
#A<-table(Dat[,"Vorlesung"])
#b<-Dat[,"Vorlesung"]==names(A)[1]
#Dat[,"Vorlesung"]<-b==TRUE
#Dat[,"Vorlesung"]<-Dat[,"Vorlesung"]*1


#Fragen und Vorlesung binär machen
##A<-table(Dat[,"Fragen"])
#b<-Dat[,"Fragen"]==0
#Dat[,"Fragen"]<-b==FALSE


#cor(Dat)
#Dat<-Dat[,c("Leistung","verstanden","RZ","FRZ","Muhe","rechtzeit","Vorelseung","freiwillig","Vorwissen")]
#cor(Dat)

## Scaled data


Dat <- scale(Dat, center = TRUE, scale = TRUE)
boxplot(Dat)
describe(Dat)

Dat<-as.data.frame(Dat)

Probetest<-Daten$Probetest
Rep<-Daten$Rep

Dat3<-cbind(Dat,Probetest,Rep)
dcor(Dat3)
#keine binären Daten
#table(Dat$rechtzeit)
#table(Dat$Vorlesung)
#table(Dat$Forum_lesen) #0 bedeutet gar nie angeschaut
#table(Dat$Pflichtlit)
#table(Dat$Fragen)

t.test(Relevanz~solver)


A<-princomp(Dat)
summary(A)
plot(A)
A$loadings

fa.parallel(Dat)


library(psych)
A<-fa(Dat,nfactors=3,rotate="oblimin")
#A<-fa(Dat,nfactors=3,rotate="oblimin",scores="Bartlett")
B<-factor.scores(Dat,A)
Da<-B$scores
Da<-as.data.frame(Da)

a<-Da$MR1>0
b<-Da$MR2>0
c<-Da$MR3>0

a<-as.factor(a)
b<-as.factor(b)
c<-as.factor(c)

levels(a) <- c("Per-", "Per+")
levels(b) <- c("time-", "time+")
levels(c) <- c("add-", "add+")

solver<-Daten$solver
mod<-lm(formula = Probetest ~ as.factor(b)*as.factor(solver))

mod<-lm(formula = Probetest ~ as.factor(a)*as.factor(b)*as.factor(c)*as.factor(solver))
mod<-lm(formula = Probetest ~ as.factor(c)*as.factor(b)*solver)

mod<-lm(formula = Da$Probetest ~ as.factor(Da$b)*as.factor(Da$solver))
summary(mod)
anova(mod)


mod<-lm(formula = Probetest ~ as.factor(a)*as.factor(b)* as.factor(c)*as.factor(solver)*as.factor(grpsKMdrei))
anova(mod)

mod<-lm(formula = Probetest ~ as.factor(b)* as.factor(solver)*as.factor(grpsKMvier))
anova(mod)

cor(RZ,Probetest)

mod<-lm(Probetest~RZ*as.factor(solver))

mod<-lm(Probetest~as.factor(solver)*as.factor(d))

mod<-lm(MR2~as.factor(solver)*as.numeric(ilu))
summary(mod)

mod<-lm(Performance~as.factor(solver)*as.numeric(RZ))
summary(mod)

MR3<-Da$MR3
MR1<-Da$MR1
MR2<-Da$MR2

mod<-lm(MR3~as.numeric(MR1)*as.numeric(MR2)*as.factor(solver))
summary(mod)

mod<-lm(MR3~as.factor(solver))
summary(mod)

leser<-grpsKM>0
leser<-Daten$Pflichtlit>0.5
leser<-Daten$RZ>0
Da<-cbind(Da,Probetest,grpsKMdrei)
Da<-cbind(Da,solver)
#DaA<-Da[leser,]
#DaB<-Da[!leser,]
cor(DaA)
cor(DaB)


a<-Performance>0
b<-verstanden>0
c<-RZ>0
d<-zus>0
e<-Fragen>0

a<-MR1>0
b<-MR2>0
c<-MR3>0



mod<-lm(as.numeric(Probetest)~as.factor(solver)*as.factor(a)*as.factor(b)*as.factor(c)*as.factor(d)*as.factor(e))
mod<-lm(as.numeric(Probetest)~as.factor(a)*as.factor(solver))

MR2<-Da$MR2

#MR1<-DaA$MR1
#MR2<-DaA$MR2
#MR3<-DaA$MR3
solver<-Daten$solver

mod<-lm(as.numeric(Probetest)~as.numeric(MR1)*as.numeric(MR2)*as.numeric(MR3)*as.factor(solver))
summary(mod)
anova(mod)
lm(mod)

anova(mod)
plot_model(mod,type = "pred", terms = c("grpsKMdrei","solver"))
plot_model(mod,type = "pred", terms = c("c","solver"))

solver<-Daten$solver
mod<-lm(as.numeric(Probetest)~factor(solver)*as.factor(b))
mod<-lm(as.numeric(Probetest)~factor(solver)*as.factor(grpsKMdrei)*as.factor(b))
mod<-lm(as.numeric(MR1)~factor(solver)*as.factor(grpsKMdrei))
mod<-lm(as.numeric(MR2)~factor(solver)*as.factor(grpsKMdrei))
mod<-lm(as.numeric(MR3)~factor(solver)*as.factor(grpsKMdrei))
mod<-lm(as.numeric(ilu)~RZ*as.factor(solver))

mod<-lm(as.numeric(Probetest)~as.numeric(RZ)*as.factor(solver))

mod<-lm(as.numeric(MR3)~factor(solver)*as.factor(grpsKMdrei))

a<-ilu>0
mod<-lm(Probetest~as.factor(c)*as.factor(solver))

mod<-lm(Probetest~as.numeric(ilu)*as.numeric(RZ)*as.numeric(Pflichtlit)*as.numeric(Fragen)*as.factor(solver))
mod<-lm(Dat$FRZ~as.factor(solver))


mod<-lm(Probetest~as.factor(c)*as.factor(grpsKM)*as.factor(solver))
mod<-lm(Probetest~as.factor(grpsKM)*as.factor(solver))

mod<-lm(Dat$FRZ~as.factor(solver))


t.test(Dat$RZ~as.factor(solver))
t.test(Dat$FRZ~as.factor(solver))
t.test(MR2~as.factor(solver))

table(grpsKMdrei,b,solver)

mod<-lm(Probetest~as.factor(f)*as.factor(grpsKM)*as.factor(solver))
summary(mod)
anova(mod)
lm(mod)

anova(mod)
plot_model(mod,type = "pred", terms = c("b","c","d"))
plot_model(mod,type = "pred", terms = c("solver", "c"))
plot_model(mod,type = "pred", terms = c("solver","c","grpsKM"))
plot_model(mod,type = "pred", terms = c("MR1","MR2"))



table(b,solver)

table(b,grpsKMdrei,solver)
Chi<-chisq.test(b,Daten$solver)
Chi

Leistung<-Dat$Leistung
Leistungman<-Dat$Leistungman
verstanden<-Dat$verstanden
Vorwissen<-Dat$Vorwissen
Relevanz<-Dat$Relevanz
RZ<-Dat$RZ
FRZ<-Dat$FRZ
Muhe<-Dat$Muhe
rechtzeit<-Dat$rechtzeit
Forum_lesen<-Dat$Forum_lesen
Pflichtlit<-Dat$Pflichtlit
Fragen<-Dat$Fragen


#attach(Dat)
Leist<-cbind(Leistungman,verstanden,Vorwissen,Relevanz)
Zeit<-cbind(RZ,FRZ,Muhe)
Gew<-cbind(rechtzeit,Forum_lesen,Pflichtlit,Fragen,Stunden)

Leist<-rowSums(Leist)
Zeit<-rowSums(Zeit)
Gew<-rowSums(Gew)

Bl<-cbind(Da,Leist,Zeit,Gew)
cor(Bl)
plot(Da[,"MR1"],Leist)

StUB<-Daten$StUB
T<-cbind(Da,StUB)
cor(T,use="complete.obs")


t.test(Da[,"MR1"]~Daten$solver)#solver etwas höhere Leistung
t.test(Da[,"MR2"]~Daten$solver) ##kein Unterschied
t.test(Da[,"MR3"]~Daten$solver) ##Leute 2020 mehr Zusatzaufwand
t.test(Da[,"MR4"]~Daten$solver) ##Nein unterschied auf zusatzaufgaben

A<-lm(Da[,"MR3"]~Daten$solver*Daten$Rep)
summary(A)

A<-lm(Da[,"MR3"]~Daten$Rep)
summary(A)

A<-lm(Da[,"MR1"]~Daten$solver*Daten$Rep)
summary(A)

A<-lm(Da[,"MR2"]~Daten$solver*Daten$Rep)
summary(A)

a<-lm(Da[,"MR3"]~as.factor(grpsKM)*as.factor(solver))
a<-lm(Da[,"MR1"]~as.factor(grpsKM)*as.factor(solver))

summary(a)
anova(a)

colnames(Da)<-c("Performance","Time investment","additional investment")
hist(Da[,"Performance"])
hist(Da[,"Time investment"])
hist(Da[,"additional investment"])

a<-lm(Dat$RZ~Da[,"Performance"]*ilu)
a<-lm(Dat$FRZ~Da[,"Performance"]*ilu)
a<-lm(Dat$Muhe~Da[,"Performance"]*ilu)
summary(a)

describe(Da)

#warum investieren die viel weniger Zeit? Ist das mit mehr Durchg?ngen zu erkl?ren?

fa.parallel(Dat)

scree(Dat)

?scree
attach(Dat)

test<-Dat[,c("Forum_lesen","Pflichtlit","Fragen","Vorlesung")]

A<-pca(Dat,nfactors=2)

#test<-Dat[,c("Muhe","Vorlesung","Forum_lesen","Pflichtlit","Fragen","rechtzeit","Leistung","verstanden","RZ","FRZ","Vorwissen","Relevanz")]

#test<-cbind(Vorlesung,Forum_lesen,Pflichtlit,Fragen,Muhe,RZ,FRZ)
A<-princomp(test)
scree(test)

#A<-princomp(Dat)

summary(A)
plot(A)
A$loadings
#multivariate 


#Investment_freiwillig<-0-A$scores[,1]
Investment<-A$scores[,1]


Leist<-Dat[,c("Leistung","verstanden","RZ","FRZ")]
A<-princomp(Leist)


summary(A)
plot(A)
A$loadings

Performancelange<-A$scores[,1]
Performancekurz<-A$scores[,2]
#multivariate 



Dat<-Dat[,c("Vorwissen","Relevanz","Leistung","RZ","FRZ","rechtzeit","Stunden","Muhe","verstanden")]
Dat$Investment<-Investment

#Dat$Investment_freiwillig<-Investment_freiwillig
#Dat$Performancelange<-Performancelange
#Dat$Performancelange<-Performance#
#Dat$Performancekurz<-Performancekurz
#Dat$Performancekurz<-Time


describe(Dat)
describe(Da)

A<-princomp(Dat)

summary(A)
plot(A)
A$loadings

scree(Dat)
#multivariate normalverteilung
library("MVN")
mvn(Dat)
mvn(Da)


Kom1<-A$scores[,1]
Kom2<-A$scores[,2]
Kom3<-A$scores[,3]
Kom4<-A$scores[,4]

#Dat<-cbind(Kom1,Kom2,Kom3,Kom4)

##Clusteranalyse
#############
## k-means ##
#############
#Daten<-Dat
Dat<-Da[,c(1:3)]


##wie viele cluster?
#set.seed(256)
set.seed(123)
set.seed(456)
library("NbClust")
NbClust(Dat,method="kmeans")


## k-means with 3 centers
##set.seed(567)
set.seed(123)
set.seed(321)
set.seed(888)
ckm <- kmeans(Dat, centers = 3, nstart = 1000)
grpsKM <- ckm$cluster
grpsKM
table(grpsKM)
grpsKM<-as.numeric(grpsKM)
table(grpsKM,solver)

A<-cbind(Dat,grpsKM,solver,Probetest,Note,Rep,Vorwissen,Leistungman,verstanden,StUB)
B<-cbind(Dat,StUB,solver)
aggregate(B, by=list(cluster=grpsKM,solver), mean)

aggregate(A, by=list(cluster=grpsKM), mean)

table(grpsKM,Daten$Jahr,Daten$solver)
table(grpsKM,Daten$solver)
Chi<-chisq.test(grpsKM,Daten$solver)
Chi
Chi$expected
Chi$observed

Chi<-chisq.test(grpsKM,solver,time_investment_group)
table(grpsKM,time_investment_group,solver)
Chi<-chisq.test(grpsKM,time_investment_group)
Chi
Chi$expected
Chi$observed

Daten<-as.data.frame(Daten)
a<-aov(as.numeric(Probetest)~as.factor(grpsKM)*as.factor(solver))
anova(a)

TukeyHSD(a)




Performance<-Da$Performance
Time_investment<-Da$`Time investment`
additional_investment<-Da$`additional investment`

a<-lm(as.numeric(Probetest)~Performance*Time_investment*additional_investment*solver*grpsKMdrei)
summary(a)
anova(a)


plot_model(a,type = "pred", terms = c("solver","grpsKM"))
plot_model(a,type = "pred", terms = c("Time_investment","additional_investment","grpsKMdrei"))

library(car)
bartlett.test(Probetest ~ interaction(as.factor(grpsKM)*as.factor(solver)))
leveneTest(Probetest ~ as.factor(grpsKM)*as.factor(solver))

aggregate(Probetest, by=list(cluster=grpsKM,solver), sd)
aggregate(Probetest, by=list(cluster=grpsKM,solver), skew)




pairwise.t.test(Daten$Probetest,a, p.adj = "bonf",var.equal=FALSE)

a<-lm(Daten$Probetest~as.factor(grpsKM)*as.factor(solver))
summary(a)
anova(a)
plot_model(a,type = "pred", terms = c("grpsKM", "solver"))





plot(Daten$Leistungman,Dat[,"additional investment"],col=grpsKM)
plot(Daten$Leistungman,Daten$verstanden,col=grpsKM)

plot(Dat[,"Time investment"],Dat[,"Performance"],col=solver+1)
plot(Dat[,"Time investment"],Dat[,"additional investment"],col=solver)
plot(Dat[,"Performance"],Dat[,"additional investment"],col=solver+1)

plot(Dat[,"Time investment"],Dat[,"Performance"],col=solver+1)
plot(Dat[,"Time investment"],Dat[,"additional investment"],col=solver+1)
plot(Dat[,"Performance"],Daten$Probetest,col=solver+1)


A<-as.data.frame(Dat)
a<-lm(Dat[,"Performance"]~as.factor(grpsKM)*as.factor(solver))
#a<-lm(A$Performance~solver)
anova(a)
plot_model(a,type = "pred", terms = c("grpsKM", "solver"))
plot(Dat[,"Performance"]~grpsKM,col=solver)

A<-as.data.frame(Dat)
a<-lm(Dat[,"Time investment"]~as.factor(grpsKM)*as.factor(solver))
#a<-lm(A$Performance~solver)
anova(a)
plot_model(a,type = "pred", terms = c("grpsKM", "solver"))
plot(Dat[,"Performance"]~grpsKM,col=solver)


A<-as.data.frame(Dat)
a<-lm(Dat[,"additional investment"]~as.factor(grpsKM)*as.factor(solver))
#a<-lm(A$Performance~solver)
anova(a)
summary(a)
a<-aov(Dat[,"additional investment"]~as.factor(grpsKM)*as.factor(solver))
plot(Dat[,"additional investment"]~as.factor(grpsKM)*as.factor(solver),col=solver+1)
plot_model(a,type = "pred", terms = c("grpsKM", "solver"))


A<-as.data.frame(Dat)
a<-aov(Dat$Probetest~as.factor(solver)*as.numeric(Rep))
#a<-lm(A$Performance~solver)
summary(a)
plot_model(a,type = "pred", terms = c("Rep", "solver"))
plot(Dat[,"Performance"]~grpsKM,col=solver)

DA<-cbind(Dat,Probetest)
aggregate(DA, by=list(cluster=grpsKM,solver), sd)
aggregate(A, by=list(cluster=grpsKM,solver), mean)

A<-as.data.frame(A)
a<-lm(A$"Time investment"~as.factor(grpsKM)*as.factor(solver))
anova(a)
plot_model(a,type = "pred", terms = c("grpsKM", "solver"))
aggregate(A, by=list(cluster=grpsKM,solver), mean)

A<-as.data.frame(A)
a<-lm(A$"additional investment"~as.factor(grpsKM)*as.factor(solver))
anova(a)
plot_model(a,type = "pred", terms = c("grpsKM", "solver"))
aggregate(A, by=list(cluster=grpsKM,solver), mean)

A<-as.data.frame(A)
a<-lm(Daten$Termin1~solver*grpsKM)
summary(a)
plot_model(a,type = "pred", terms = c("grpsKM", "solver"))
aggregate(A, by=list(cluster=grpsKM,solver), mean)


A<-as.data.frame(A)
a<-lm(Daten$Probetest~as.numeric(Rep)*as.factor(grpsKM)*as.factor(solver))
a<-lm(as.numeric(Rep)~as.factor(grpsKM)*as.factor(solver))
anova(a)
plot_model(a,type = "pred", terms = c("grpsKM", "solver"))
aggregate(A, by=list(cluster=grpsKM,solver), mean)

grpsKM<-as.factor(grpsKM,ordered=FALSE)
solver<-as.factor(solver)

ABC<-cbind(Probetest,grpsKM,solver)
ABC<-as.data.frame(ABC)

ABC <- within(ABC, grpsKM <- relevel(grpsKM, ref = 3))

aa<-aov(Daten$Probetest~as.factor(grpsKM)*as.factor(solver))

TukeyHSD(aa)
pairwise.t.test(ABC$Probetest,c(ABC$grpsKM,ABC$solver))



A<-aov(as.numeric(Rep)~as.factor(grpsKM)*as.factor(solver))
anova(A)





a<-lm(Dat[,"Performance"]~as.factor(solver)*as.factor(grpsKM))
a<-aov(a)
summary(a)
plot_model(a,type = "pred", terms = c("grpsKM", "solver"))
aggregate(A, by=list(cluster=grpsKM,solver), mean)

a<-aov(Daten$Termin1~as.factor(solver)*as.factor(grpsKM))
a<-aov(a)
summary(a)
plot_model(a,type = "pred", terms = c("grpsKM", "solver"))
aggregate(A, by=list(cluster=grpsKM,solver), mean)

a<-lm(Daten$Termin2~as.factor(solver)*as.factor(grpsKM))
a<-aov(a)
summary(a)
plot_model(a,type = "pred", terms = c("grpsKM", "solver"))
aggregate(A, by=list(cluster=grpsKM,solver), mean)
pairwise.t.test(Daten$Termin2,a, p.adj = "bonf",var.equal=FALSE)


a<-lm(Daten$Leistung~as.factor(solver)*as.factor(grpsKM))
a<-aov(a)
summary(a)
plot_model(a,type = "pred", terms = c("grpsKM", "solver"))

a<-lm(Daten$Termin3~as.factor(solver)*as.factor(grpsKM))
a<-aov(a)
summary(a)
plot_model(a,type = "pred", terms = c("grpsKM", "solver"))
aggregate(A, by=list(cluster=grpsKM,solver), mean)
pairwise.t.test(Daten$Termin2,a, p.adj = "bonf",var.equal=FALSE)

a<-lm(Daten$Termin4~as.factor(solver)*as.factor(grpsKM))
a<-aov(a)
summary(a)
plot_model(a,type = "pred", terms = c("grpsKM", "solver"))
aggregate(A, by=list(cluster=grpsKM,solver), mean)
pairwise.t.test(Daten$Termin2,a, p.adj = "bonf",var.equal=FALSE)

a<-lm(Daten$Termin5~as.factor(solver)*as.factor(grpsKM))
a<-aov(a)
summary(a)
plot_model(a,type = "pred", terms = c("grpsKM", "solver"))
aggregate(A, by=list(cluster=grpsKM,solver), mean)
pairwise.t.test(Daten$Termin2,a, p.adj = "bonf",var.equal=FALSE)

a<-lm(Dat[,"Performance"]~as.factor(grpsKM)*as.factor(solver))
#a<-anova(a)
anova(a)
plot_model(a,type = "pred", terms = c("grpsKM", "solver"))
aggregate(A, by=list(cluster=grpsKM,solver), mean)
pairwise.t.test(Daten$Termin2,a, p.adj = "bonf",var.equal=FALSE)

a<-lm(Dat[,"Time investment"]~as.factor(grpsKM)*as.factor(solver))
#a<-aov(a)
anova(a)
plot_model(a,type = "pred", terms = c("grpsKM", "solver"))
aggregate(A, by=list(cluster=grpsKM,solver), mean)
pairwise.t.test(Daten$Termin2,a, p.adj = "bonf",var.equal=FALSE)

a<-lm(Dat[,"additional investment"]~as.factor(grpsKM)*as.factor(solver))
anova(a)
summary(a)
plot_model(a,type = "pred", terms = c("grpsKM", "solver"))
aggregate(A, by=list(cluster=grpsKM,solver), mean)
pairwise.t.test(Daten$Termin2,a, p.adj = "bonf",var.equal=FALSE)

a<-lm(as.numeric(Rep)~as.factor(grpsKM)*as.factor(solver))
anova(a)


A<-as.data.frame(A)
a<-lm(Daten$Termin1~solver)
summary(a)

a<-lm(Daten$Termin1~solver)
summary(a)

a<-t.test(Daten$Termin1~solver)
a

a<-t.test(Daten$Termin2~solver)
a
a<-t.test(Daten$Termin3~solver)
a
a<-t.test(Daten$Termin4~solver)
a
a<-t.test(Daten$Termin5~solver)
a


summary(a)






table(is.na(Daten$StUB))
t<-is.na(Daten$StUB)
#t<-as.numeric(t)
Hilfe<-cbind(Daten,grpsKM)
Hilfe[t,"StUB"]<-1
table(Hilfe[,"StUB"])

#a<-Hilfe[,"StUB"]>1
#Hilfe[t,"StUB"]<-2

##nur Leute auswählen, die nie bei stefan in der übung waren
a<-Hilfe[,"StUB"]==1
Hilfe2<-Hilfe[a,]

a<-aov(Hilfe2$Probetest~Hilfe2$solver*Hilfe2$grpsKM)
summary(a)
anova(a)

table(Hilfe2$solver,Hilfe2$grpsKM)


a<-lm(Daten$Probetest~Hilfe$StUB*Daten$solver*grpsKM)
summary(a)
anova(a)

a<-aov(Daten$Probetest~Leistungman*grpsKM*solver)
anova(a)

Daten<-cbind(Daten,grpsKM,solver)
Daten<-cbind(Daten,MR2,MR3)

Dat1<-Daten[grpsKM==1,]
Dat2<-Daten[grpsKM==2,]
Dat3<-Daten[grpsKM==3,]

Dat1<-Daten[solver==0,]
cor(Dat1$MR2,Dat1$Probetest)
Dat2<-Daten[solver==1,]
cor(Dat2$MR2,Dat2$Probetest)

t.test(Dat1$Probetest~Dat1$solver)
t.test(Dat2$Probetest~Dat2$solver)
t.test(Dat3$Probetest~Dat3$solver)

Dat<-cbind(Dat,Probetest,Rep,solver,Vorwissen,StUB)

Dat<-as.data.frame(Dat)
m <- lm(Performance ~ grpsKM*solver, data = Dat2)
m <- aov(Note ~ as.numeric(ilu)*as.factor(solver)*as.factor(grpsKM), data = Dat)
m <- lm(MR1 ~ as.factor(c)*as.factor(solver)*as.factor(grpsKM), data = Dat)
m <- lm(MR3 ~ as.factor(c)*as.factor(solver), data = Dat2)
m <- lm(Probetest ~ as.factor(solver)*as.numeric(MR1)*as.numeric(MR2)*as.numeric(MR3), data = Dat)
m <- lm(Probetest ~ Rep*grpsKM, data = Dat)

m <- lm(Probetest ~ as.factor(solver)*as.factor(grpsKM), data = Daten)

summary(m)
anova(m)

library(effectsize)
eta_squared(m)



plot_model(m,type = "pred", terms = c("solver"))
plot_model(m,type = "pred", terms = c("solver","c","grpsKM"))


plot_model(m,type = "pred", terms = c("MR2","MR3","MR1"))
plot_model(m,type = "pred", terms = c("MR2","solver"))
plot_model(m,type = "pred", terms = c("MR2","grpsKM"))
plot_model(m,type = "pred", terms = c("ilu","grpsKM","solver"))
plot_model(m,type = "pred", terms = c("ilu","solver","grpsKM"))
plot_model(m,type = "pred", terms = c("ilu","solver"))

library(car)
Anova(aov(Note~as.factor(solver)*as.numeric(ilu)),type=3)
Anova(aov(Note~as.factor(solver)*as.factor(grpsKM)*as.),type=3)
A<-Anova(aov(Probetest~as.factor(solver)*as.factor(grpsKM)),type=3)
library(effectsize)

eta_squared(A, partial = FALSE)



Anova(aov(Probetest~as.factor(solver)*as.factor(grpsKM)),type=3)

Anova(aov(Probetest~as.factor(solver)*as.factor(c)),type=3)


pairwise.t.test(Probetest,g=c("c","solver"), p.adj = "bonf",var.equal=FALSE)

plot(MR2,Probetest)

m <- aov(Probetest ~ as.numeric(Rep)*as.factor(solver)*as.factor(grpsKM), data = Dat)

a<-Dat$Rep==5
Dat$Rep2<-Dat$Rep
Dat$Rep2[a]<-4

a<-Dat$Rep==6
Dat$Rep2[a]<-4

table(Dat$Rep2)
table(Dat$Rep2,Dat$solver)

m <- aov(Probetest ~ as.factor(solver)*as.numeric(MR3), data = Dat)
m <- aov(Probetest ~ as.factor(grpsKM)*as.factor(solver), data = Dat)

m <- aov(Probetest ~ as.numeric(Rep)*as.factor(solver), data = Dat)

m <- aov(Probetest ~ as.numeric(Rep2)*as.factor(solver), data = Dat)

#m <- aov(Probetest ~ as.factor(grpsKM)*as.factor(solver)*as.numeric(Rep), data = Dat)
#m <- lm(Probetest ~ GrL*solver, data = Test)
anova(m)
summary(m)


library("rstatix")
eta_squared(m)

table(grpsKM)
table(solver)

solver<-as.numeric(solver)

a<-grpsKM+3*solver

pairwise.t.test(Daten$Probetest,a, p.adj = "bonf",var.equal=FALSE)

plot_model(m,type = "pred", terms = c("MR1","MR2","MR3"))
plot_model(m,type = "pred", terms = c("solver"))
plot_model(m,type = "pred", terms = c("MR3"))
plot_model(m,type = "pred", terms = c("grpsKM", "solver"))
plot_model(m,type = "pred", terms = c("solver","c"))
plot_model(m,type = "pred", terms = c("Rep","grpsKM"))

Dat<-cbind(Dat,grpsKM)
Dat1<-Dat[solver==0,]
Dat2<-Dat[solver==1,]

m <- lm(Probetest ~ Rep*grpsKM, data = Dat)
m <- lm(Probetest ~ Rep*grpsKM, data = Dat1)
m <- lm(Probetest ~ Rep*grpsKM, data = Dat2)

m <- lm(Probetest ~ grpsKM*solver, data = Dat)

m <- lm(Probetest ~ grpsKM*solver, data = Hilfe2)

summary(m)
plot_model(m,type = "pred", terms = c("grpsKM","solver"))

TukeyHSD(m)

Dat1<-Dat[grpsKM==1,]
Dat2<-Dat[grpsKM==2,]
Dat3<-Dat[grpsKM==3,]

cor(Dat1,use=c)

table(Dat1$solver,Dat1$Rep)
table(Dat2$solver,Dat2$Rep)
table(Dat3$solver,Dat3$Rep)

cor(Dat1,use="complete.obs")
Dat1<-as.data.frame(Dat1)
plot(Dat1$Rep,Dat1$Probetest)

cor(Dat2,use="complete.obs")
Dat2<-as.data.frame(Dat2)
plot(Dat2$Rep,Dat2$Probetest)

cor(Dat3,use="complete.obs")
Dat3<-as.data.frame(Dat3)
plot(Dat2$Rep,Dat2$Probetest)


t.test(Dat1$Probetest~Dat1$solver,var.equal=FALSE)
t.test(Dat2$Probetest~Dat2$solver,var.equal=FALSE)
t.test(Dat3$Probetest~Dat3$solver,var.equal=FALSE)

aggregate(A, by=list(cluster=grpsKM), mean)
a<-aov(Daten$Leistungman~grpsKM)
a<-aov(Daten$Rep~grpsKM)
plot(Daten$Leistungman~grpsKM)

pairwise.t.test(Daten$Leistungman,grpsKM, p.adj = "bonf",var.equal=FALSE)
pairwise.t.test(Daten$Probetest,grpsKM, p.adj = "bonf",var.equal=FALSE)
pairwise.t.test(Daten$Vorwissen,grpsKM, p.adj = "bonf",var.equal=FALSE)
pairwise.t.test(Daten$Rep,grpsKM, p.adj = "bonf",var.equal=FALSE)
pairwise.t.test(Daten$verstanden,grpsKM, p.adj = "bonf",var.equal=FALSE)
pairwise.t.test(Daten$RZ,grpsKM, p.adj = "bonf",var.equal=FALSE)
pairwise.t.test(Daten$FRZ,grpsKM, p.adj = "bonf",var.equal=FALSE)
plot(Daten$Muhe~grpsKM)
pairwise.t.test(Daten$Muhe,grpsKM, p.adj = "bonf",var.equal=FALSE)
pairwise.t.test(Daten$rechtzeit,grpsKM, p.adj = "bonf",var.equal=FALSE)
pairwise.t.test(Daten$Vorlesung,grpsKM, p.adj = "bonf",var.equal=FALSE)
pairwise.t.test(Daten$Forum_lesen,grpsKM, p.adj = "bonf",var.equal=FALSE)
pairwise.t.test(Daten$Pflichtlit,grpsKM, p.adj = "bonf",var.equal=FALSE)
pairwise.t.test(Daten$Fragen,grpsKM, p.adj = "bonf",var.equal=FALSE)
pairwise.t.test(Daten$Relevanz,grpsKM, p.adj = "bonf",var.equal=FALSE)
pairwise.t.test(Daten$Stunden,grpsKM, p.adj = "bonf",var.equal=FALSE)
plot(Daten$Pflichtlit~grpsKM)

aggregate(Daten, by=list(cluster=grpsKM), mean)

summary(a)


library(factoextra)
fviz_cluster(ckm,data=Dat)

A<-cbind(Dat,Probetest)
aggregate(A, by=list(cluster=grpsKM), mean)

aggregate(Dat, by=list(cluster=grpsKM), mean)
## Choose number of clusters k with scree plot
wss <- rep(0, 13)
for (i in 1:13) wss[i] <- sum(kmeans(Dat, centers = i, nstart = 20)$withinss)
par(mfrow = c(1,1))
plot(1:13, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares") ## 3 centers was a good choice
## Note: If nstart=1 is used, the results can vary because of random starting configurations in kmeans

solver<-Daten$solver
SB1<-Daten$SB1
Probetest<-Daten$Probetest
Jahr<-Daten$Jahr
Note<-Daten$Note
Rep<-Daten$Rep

table(grpsKM,Daten$Jahr)
table(Daten$solver,Daten$Jahr)

A<-cbind(grpsKM,Daten$Jahr)
Chi<-chisq.test(grpsKM,Daten$Jahr)


A<-cbind(Dat,grpsKM,solver,Probetest)

Note<-Daten[,"Note"]

Dat<-as.data.frame(Dat)

attach(Daten)
#Anova
AN<-cbind(Dat,Jahr,solver,Probetest,grpsKM,Note,Rep)

head(AN)
AN<-as.data.frame(AN)

AN$Jahr<-as.factor(AN$Jahr)
AN$solver<-as.factor(AN$solver)
AN$grpsKM<-as.factor(AN$grpsKM)


#a<-AN$Jahr==2021&AN$solver==0

#AN<-AN[!a,]

library("rstatix")
AN %>%
  group_by(Jahr, solver, grpsKM) %>%
  get_summary_stats(Probetest, type = "mean_sd")


AN %>%
  group_by(solver, grpsKM) %>%
  get_summary_stats(Probetest, type = "mean_sd")


anova_test(AN,Probetest~solver*grpsKM)

library("ggpubr")
ggboxplot(AN, x = "grpsKM", y = "Probetest", color = "solver",
          palette = c("#00AFBB", "#E7B800"))

ggboxplot(AN, x = "grpsKM", y = "Note", color = "solver",
          palette = c("#00AFBB", "#E7B800"))

A<-lm(as.numeric(Note)~as.factor(solver)*as.factor(grpsKM),dat=AN)
summary(A)
anova(A)

summary(m)
plot_model(A,type = "pred", terms = c("grpsKM","solver"))



oneway.test(Probetest~as.factor(solver)*as.factor(grpsKM),dat=AN, var.equal=FALSE)
twoway.test(Probetest~as.factor(solver)*as.factor(grpsKM),dat=AN, var.equal=FALSE)


A<-aov(Probetest~as.factor(solver)*as.factor(grpsKM),dat=AN)
summary(A)


#A<-aov(grpsKM~Rep)

res<-A$residuals
hist(res,main="Histogram of
residuals",xlab="Residuals")

bartlett.test(AN$Probetest~as.factor(AN$solver))
bartlett.test(AN$Probetest~as.factor(AN$grpsKM))


library(car)
leveneTest(Probetest~as.factor(solver)*as.factor(grpsKM),data=AN)
leveneTest(Probetest~as.factor(solver),data=AN)
leveneTest(Probetest~as.factor(grpsKM),data=AN)
leveneTest(Note~as.factor(solver)*as.factor(grpsKM),data=AN)

fligner.test(formula=AN$Probetest~as.factor(AN$grpsKM))

###https://stackoverflow.com/questions/2933253/homoscedascity-test-for-two-way-anova
##https://stats.stackexchange.com/questions/97098/practically-speaking-how-do-people-handle-anova-when-the-data-doesnt-quite-mee
library(car)
model.lm <- lm(formula=Probetest~as.numeric(Rep)*as.factor(solver)*as.factor(grpsKM),data=AN)
Anova(model.lm,type='II',white.adjust='hc3')


library(nlme)
mod.gls = gls(Probetest~as.factor(solver)*as.factor(grpsKM), data=AN,weights=varIdent(form= ~ 1 | grpsKM))
anova(mod.gls)
summary(mod.gls)

model.matrix.gls <- function(object, ...)
  model.matrix(terms(object), data = getData(object), ...)
model.frame.gls <- function(object, ...)
  model.frame(formula(object), data = getData(object), ...)
terms.gls <- function(object, ...)
  terms(model.frame(object),...)

library(multcomp)
mod.gls.mc = glht(mod.gls, linfct = mcp(grpsKM = "Tukey"))
summary(mod.gls.mc)

#plot(Probetest~solver*grpsKM,dat=AN)



pairwise.t.test(AN$Probetest,grpsKM, p.adj = "none")
pairwise.t.test(AN$Probetest,solver, p.adj = "none")

TukeyHSD(A)


ANA <- 
  AN %>%
  group_by(solver) %>%
  mutate_at(vars(Probetest), scale)


interaction.plot(x.factor=grpsKM,trace.factor=solver,response=Probetest)

#interaction.plot(x.factor=ANA$grpsKM,trace.factor=ANA$solver,response=ANA$Probetest)



A<-lm(as.numeric(Note)~as.factor(solver)*as.factor(grpsKM),dat=gut)
A<-lm(as.numeric(Note)~as.factor(solver)*as.factor(grpsKM),dat=Dat)
summary(A)

summary(m)
plot_model(A,type = "pred", terms = c("grpsKM","solver"))




table(AN$Rep,AN$grpsKM,AN$solver)
T<-aov(AN$Rep~AN$grpsKM*AN$solver)
summary(T)


T<-aov(AN$Performance~AN$grpsKM*AN$solver)
summary(T)

interaction.plot(x.factor=AN$grpsKM,trace.factor=AN$solver,response=AN$Performance)


a<-!is.na(AN$Note)
gut<-AN[a,]


library(dplyr)
#Noten innerhalb der Gruppe standardisieren und damit rechnen
gut <- 
  gut %>%
  group_by(Jahr) %>%
  mutate_at(vars(Note), scale)


a<-gut$solver==FALSE
gut1<-gut[a,]
gut1<-as.data.frame(gut1)
plot(gut$Note~gut$grpsKM)
mod.gls = gls(Note~as.factor(grpsKM), data=gut1,weights=varIdent(form= ~ 1 | grpsKM))
anova(mod.gls)
summary(mod.gls)

a<-gut$solver==TRUE
gut1<-gut[a,]
gut1<-as.data.frame(gut1)
plot(gut$Note~gut$grpsKM)
mod.gls = gls(Note~as.factor(grpsKM), data=gut1,weights=varIdent(form= ~ 1 | grpsKM))
anova(mod.gls)
summary(mod.gls)




interaction.plot(x.factor=gut$grpsKM,trace.factor=gut$solver,response=gut$Note)
#grp<-as.factor(grpsKM)

library(nlme)
mod.gls = gls(Note~as.factor(solver)*as.factor(grpsKM), data=gut,weights=varIdent(form= ~ 1 | grpsKM))
anova(mod.gls)
summary(mod.gls)

mod.gls = aov(Note~as.factor(solver)*as.factor(grpsKM), data=gut)
anova(mod.gls)
summary(mod.gls)

mod.gls = lm(Note~as.factor(solver)*as.factor(grpsKM), data=gut)
anova(mod.gls)
summary(mod.gls)

ANA<-as.data.frame(ANA)

#a<-Note<6
#Note[a]<-6

anova_test(ANA,Probetest~solver*grpsKM)
A<-aov(Probetest~as.factor(solver)*as.factor(grpsKM),dat=ANA)
summary(A)

A<-aov(Note~as.factor(solver)*as.factor(grpsKM),dat=ANA)
summary(A)

gut$solver<-as.numeric(gut$solver)

anova_test(gut$Note~gutsolver*gut$grpsKM)


A<-aov(Probetest~as.factor(solver)*as.factor(grpsKM),dat=ANA)
summary(A)

A<-aov(Note~as.factor(solver)*as.factor(grpsKM),dat=ANA)
summary(A)




gut<-as.data.frame(gut)
aov(gut$Note~gut$solver*gut$grpsKM)
A<-aov(Note~solver*grpsKM,dat=gut)
summary(A)


A<-lm(Note~as.factor(grpsKM),dat=AN)
summary(A)

hist(Note~grpsKM)
AN<-as.data.frame(AN)
aggregate(gut, by=list(cluster=gut$grpsKM), mean)
aggregate(AN, by=list(cluster=AN$grpsKM), mean)

A<-lm(Note~as.factor(solver)*as.factor(grpsKM),dat=gut)
anova(A)
summary(A)

A<-aov(Note~as.factor(solver)*as.factor(grpsKM),dat=gut)
anova(A)
summary(A)

A<-lm(Note~solver*as.factor(grpsKM),dat=gut)
summary(A)

A<-lm(Probetest~as.factor(solver)*as.factor(grpsKM),dat=AN)
anova(A)
summary(A)

AN<-as.data.frame(AN)
aov(AN$Note~AN$solver*AN$grpsKM)
A<-aov(Note~solver*grpsKM,dat=AN)
summary(A)


#mod<-lm(Probetest~solver*grp,dat=AN)
#summary(mod)

#eta_squared(mod)

#grp<-as.numeric(grpsKM)

##ANA imputieren
#ANA<-as.data.frame(ANA)
#ANA$Probetest<-as.numeric(ANA$Probetest)
#library(mice)
#set.seed(123)
#imputed_Data <- mice(ANA, m=1, maxit = 50, method = 'pmm', seed = 500)
#ANA<-complete(imputed_Data)







mod<-lm(ANA$Note~ANA$solver*ANA$grpsKM)
summary(mod)

mod<-aov(ANA$Note~ANA$solver*ANA$grpsKM)
summary(mod)















eta_squared(mod)

sum(table(Note,solver))
sum(table(Probetest,solver))

##Note imputieren und schauen was passiert..

#anova_test(AN,SB1~solver*grpsKM)
#A<-aov(SB1~solver*grpsKM,dat=AN)
#summary(A)

mod<-lm(SB1~solver*grpsKM,dat=AN)

eta_squared(mod)







anova_test(AN,SB1~solver*grpsKM)
A<-aov(SB1~solver*grpsKM,dat=AN)
summary(A)



anova_test(AN,Probetest~solver*grpsKM)
A<-aov(Daten[,"Note"]~solver*grpsKM)
summary(A)



A<-cbind(Dat,grpsKM,solver,Probetest,Note,Rep,Vorwissen,Leistungman,verstanden)

aggregate(Dat, by=list(cluster=grpsKM), mean)




A1<-A[,"grpsKM"]==1
B<-A[A1,]
t.test(B[,"Note"]~B[,"solver"],var.equal=FALSE)
t.test(B[,"Performance"]~B[,"solver"],var.equal=FALSE)
t.test(B[,"Bearbeitungszeit"]~B[,"solver"],var.equal=FALSE)
t.test(B[,"Gewissenhaftigkeit"]~B[,"solver"],var.equal=FALSE)
t.test(B[,"Vorwissen"]~B[,"solver"],var.equal=FALSE)
t.test(B[,"Leistungman"]~B[,"solver"],var.equal=FALSE)
t.test(B[,"LKW"]~B[,"solver"],var.equal=FALSE)
t.test(B[,"verstanden"]~B[,"solver"],var.equal=FALSE)

A1<-A[,"grpsKM"]==2
B<-A[A1,]
t.test(B[,"Note"]~B[,"solver"],var.equal=FALSE)

A1<-A[,"grpsKM"]==3
B<-A[A1,]
t.test(B[,"Note"]~B[,"solver"])

cor(B$SB1,B$Note,use="complete.obs")
cor(B$SB1,B$Leistung,use="complete.obs")
cor(B$Leistung,B$Note,use="complete.obs")
cor(B$Vorwissen,B$Note,use="complete.obs")

A1<-A[,"grpsKM"]==1
B<-A[A1,]
t.test(B[,"Probetest"]~B[,"solver"])
ml<-aov(B[,"Probetest"]~B[,"solver"]+B[,"Rep"])
#ml<-lm(B[,"Probetest"]~B[,"solver"]+B[,"Rep"])
summary(ml)

A1<-A[,"grpsKM"]==2 ##müssten hier die Leute ohne repetitions im HS21 raus?
B<-A[A1,]
t.test(B[,"Probetest"]~B[,"solver"])

A1<-A[,"grpsKM"]==3
B<-A[A1,]
t.test(B[,"Probetest"]~B[,"solver"])





cor(B$SB1,B$Note,use="complete.obs")
cor(B$SB1,B$Leistung,use="complete.obs")
cor(B$Leistung,B$Note,use="complete.obs")
cor(B$Vorwissen,B$Note,use="complete.obs")


A1<-A[,"grpsKM"]==1
B<-A[A1,]
t.test(B[,"SB1"]~B[,"solver"])




A1<-A[,"grpsKM"]==2 ##müssten hier die Leute ohne repetitions im HS21 raus?
B<-A[A1,]
t.test(B[,"SB1"]~B[,"solver"])

A1<-A[,"grpsKM"]==3 ##müssten hier die Leute ohne repetitions im HS21 raus?
B<-A[A1,]
t.test(B[,"SB1"]~B[,"solver"])

#Noten innerhalb der Gruppe standardisieren und damit rechnen
Daten <- 
  Daten %>%
  group_by(Jahr) %>%
  mutate_at(vars(Note), scale)

Note<-Daten$Note

DA<-cbind(A,Note)

A<-aov(DA[,"Note"]~solver*grpsKM)
summary(A)

A1<-DA[,"grpsKM"]==1
B<-DA[A1,]
t.test(B[,"Note"]~B[,"solver"])

cor(B$SB1,B$Note,use="complete.obs")


A1<-DA[,"grpsKM"]==2 ##müssten hier die Leute ohne repetitions im HS21 raus?
B<-DA[A1,]
t.test(B[,"SB1"]~B[,"solver"])
cor(B$SB1,B$Note,use="complete.obs")


Daten$Note




A1<-A[,"grpsKM"]==3
B<-A[A1,]
t.test(B[,"SB1"]~B[,"solver"])
#A1<-A[,"grpsKM"]==4
#B<-A[A1,]
#t.test(B[,"Probetest"]~B[,"solver"])


## Silhouette Plot
library(cluster)
plot(silhouette(grpsKM, dp))

## visualize in PC 1 & 2
#pr <- princomp(Dat)$scores[,1:2]
#plot(pr, pch = grpsKM, col=grpsKM, lwd=2)
#legend("bottomright", legend = 1:3, pch = 1:3, col=1:3, bty="n")











##Nach Dimensionsreduktion passen 5 Klassen am besten

library(tidyLPA) 
library(tidyverse)




Mod<-Dat%>%
  #select(broad_interest, enjoyment, self_efficacy) %>%
  #single_imputation() %>%
  estimate_profiles(1:10,variances = c("equal","varying","varying"),covariances = c("zero","zero","varying")) %>%

  #covariances = c("zero", "varying")) %>%
  compare_solutions(statistics = c("AIC", "BIC"))



C<-estimate_profiles(Dat,3,variances = c("equal"),covariances = c("zero") )
plot_profiles(C)
B<-get_data(C)
table(B$Class)


Probetest<-Daten$Probetest
Note<-Daten$Note
Rep<-as.numeric(Daten$Rep)
solver<-Daten$solver

Alles<-cbind(B,Probetest,Note,Rep,solver)

##alle Daten (ohne matching) und drei Gruppen funktioniert sehr gut, zufällig verteilt über die Gruppen
chisq.test(B$Class,Daten$Jahr)
chisq.test(B$Class,Daten$solver)


table(B$Class,Daten$Jahr)
table(B$Class,Daten$solver,Daten$Jahr)

FIT<-get_fit(A)

B<-as.data.frame(B)
B$Class<-as.factor(B$Class)


B %>%
  group_by(Class) %>%
  summarise_at(vars(Vorwissen,Relevanz,Investment,Investment_unfreiwillig,Performancelange,Performancekurz), list(name = mean))

B %>%
  group_by(Class) %>%
  summarise_at(vars(Vorwissen,Relevanz,Leistung,verstanden,RZ,FRZ,Investment,Investment_unfreiwillig), list(name = mean))




B %>%
  group_by(Class) %>%
  summarise_at(vars(Vorwissen,Relevanz,Investment,Investment_freiwillig,Performancelange,Performancekurz), list(name = mean))


B %>%
  group_by(Class) %>%
  summarise_at(vars(Vorwissen,Relevanz,Investment,Investment_freiwillig,Leistung,RZ,FRZ,rechtzeit,Stunden,Muhe), list(name = mean))


B %>%
  group_by(Class) %>%
  summarise_at(vars(Vorwissen,Relevanz,Investment,Investment_freiwillig,Leistung,verstanden,RZ,FRZ,rechtzeit,Stunden,Muhe), list(name = mean))

B %>%
  group_by(Class) %>%
  summarise_at(vars(MR1,MR2,MR3), list(name = mean))



#dritte Hypothese

grpsKM<-B$Class


##Voraussetzungen Anova

Alles$Class<-as.factor(Alles$Class)
Alles$solver<-as.factor(Alles$solver)

#Alles$Class<-as.numeric(Alles$Class)
#Alles$solver<-as.numeric(Alles$solver)

A<-aov(Alles$Probetest~Alles$Class*Alles$solver)
summary(A)

A<-aov(Daten$SB1~Alles$Class*Alles$solver)
summary(A)

A<-lm(Daten$SB1~Alles$Class*Alles$solver)
summary(A)


plot(Daten$SB1~Daten$Vorwissen,col=Alles$Class)
plot(Daten$Note~Daten$Vorwissen,col=Alles$Class)

library(dplyr)
Alles %>%
  group_by(Class) %>%
  summarise_at(vars(Vorwissen,Relevanz,Investment,Investment_unfreiwillig,Performancelange,Performancekurz,Probetest,Note,Rep), list(name = mean))





A<-aov(Daten$SB1~Alles$solver*Alles$Class*Alles$Rep)

A<-aov(Alles$Probetest~Alles$solver*Alles$Rep*Alles$Clas)


summary(A)

A<-aov(Daten$Note~solver*grpsKM)


summary(A)


A<-as.data.frame(B)

A1<-B[,"Class"]==1
C<-Daten[A1,]
t.test(C[,"Probetest"]~C[,"solver"])

A1<-B[,"Class"]==1
C<-Daten[A1,]
t.test(C[,"SB1"]~C[,"solver"])


A1<-B[,"Class"]==2 ##müssten hier die Leute ohne repetitions im HS21 raus?
C<-Daten[A1,]
t.test(C[,"Probetest"]~C[,"solver"])


A1<-B[,"Class"]==2 ##müssten hier die Leute ohne repetitions im HS21 raus?
C<-Daten[A1,]
t.test(C[,"SB1"]~C[,"solver"])


A1<-B[,"Class"]==3
C<-Daten[A1,]
t.test(C[,"Probetest"]~C[,"solver"])

A1<-B[,"Class"]==3
C<-Daten[A1,]
t.test(C[,"SB1"]~C[,"solver"])



A1<-B[,"Class"]==4
C<-Daten[A1,]
t.test(C[,"Probetest"]~C[,"solver"])

A1<-B[,"Class"]==4
C<-Daten[A1,]
t.test(C[,"SB1"]~C[,"solver"])

A1<-B[,"Class"]==5
C<-Daten[A1,]
t.test(C[,"Probetest"]~C[,"solver"])


A1<-B[,"Class"]==5
C<-Daten[A1,]
t.test(C[,"SB1"]~C[,"solver"])

A1<-B[,"Class"]==6
C<-Daten[A1,]
t.test(C[,"Probetest"]~C[,"solver"])


A1<-B[,"Class"]==6
C<-Daten[A1,]
t.test(C[,"SB1"]~C[,"solver"])

#install.packages("ggpubr")
library("ggpubr")

ggboxplot(Alles, x = "Class", y = "Probetest", color = "solver",
          palette = c("#00AFBB", "#E7B800"))


ggboxplot(Alles, x = "Class", y = "SB1", color = "solver",
          palette = c("#00AFBB", "#E7B800"))



ggboxplot(Alles, x = "Class", y = "Note", color = "solver",
          palette = c("#00AFBB", "#E7B800"))



A<-aov(Alles$Note~Alles$solver*Alles$Class)
summary(A)


cor(Daten$Note,Daten$Probetest,use="complete")

library(dplyr)
Daten %>%
  group_by(Jahr) %>%
  summarize(COR=cor(Note,Probetest,use="complete"))


######letzte Frage, können diese Unterschiede durch Anzahl repetitions erklärt werden? ANCOVA




Dal<-Dat[,c(1:7)]
Dal <- scale(Dal, center = TRUE, scale = TRUE)
Dal<-Dal[,c(1:4,7)]


## Distance matrix (used below)
dp <- dist(t(Dal)) ## Euclidean


## Fit classical MDS (2 dimensions)
mds <- cmdscale(dp, k = 2, eig = TRUE)

## Plot results
mds_coordinates <- mds$points
plot(mds_coordinates[,1], mds_coordinates[,2], type = "n")
text(mds_coordinates[,1], mds_coordinates[,2], labels = rownames(mds_coordinates), cex = 0.7)


cor(Dal,use="complete.obs")


A<-prin(Dat)

summary(A)
plot(A)
A$loadings

