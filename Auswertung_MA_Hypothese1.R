# load packages
library(tidyverse)
library(readxl)
library(dplyr)
library(texreg)
library(MatchIt)
library(cobalt)

Daten<-Sicherheit2
#look at number of casses per year and number of repetitions in the first set of excersises
table(Daten$Jahr)
table(Daten$sum_oft)
table(Daten$Rep,Daten$Jahr)


##Preknowledge
t.test(Daten$Note~Daten$Jahr)
t.test(Daten$Probetest~Daten$Jahr)
t.test(Daten$Leistungma~Daten$Jahr)

t.test(Daten$Theorie~Daten$Jahr)
t.test(Daten$Berechnungen~Daten$Jahr)

aggregate(Daten$Berechnunge,by=list(cluster=Daten$solver), sd)
aggregate(Daten$Theorie,by=list(cluster=Daten$solver), sd)

aggregate(Probetest, by=list(cluster=grpsKM,solver), sd)
aggregate(Probetest, by=list(cluster=grpsKM,solver), skew)


Vorwissen<-Daten$Theorie+Daten$Berechnungen
Daten$Vorwissen<-Daten$Theorie+Daten$Berechnungen
t.test(Daten$Theorie~Daten$Jahr)
t.test(Daten$Berechnungen~Daten$Jahr)

t.test(Vorwissen~Daten$Jahr)


cor.test(Daten$Probetest,Daten$Note)
plot(Daten$Probetest,Daten$Note,col=Daten$Jahr-2019)

table(Daten$solver)

mod1 <- lm(Note ~ Jahr + solver, 
           data = Daten)
summary(mod1)

mod1 <- lm(Probetest ~ solver, 
           data = Daten)
summary(mod1)
###das ist irgendwie nicht zulässig, weil nämlich solver sein eine Selbstselektion ist


##make 3 groups / solver by year > after matching
b<-Daten$Jahr+Daten$solver
b<-as.factor(b)
levels(b) <- c("2020","2021_no_additional_tasks","2021_additional tasks")

mod<-aov(Daten$Probetest~b)
summary(mod)

plot(Daten$Probetest~b,main="",xlab="groups",ylab="performance")
pairwise.t.test(Daten$Probetest,b, p.adj = "none")
table(Daten$solver,Daten$Jahr)

mod<-aov(Daten$Note~b)
summary(mod)

plot(Daten$Note~b,main="",xlab="groups",ylab="performance")
pairwise.t.test(Daten$Note,b, p.adj = "none")
#group means
a<-is.na(Daten$Note)
Dat<-Daten[!a,]
b<-b[!a]
aggregate(Dat$Note, list(b), FUN=mean)



##die non-solver von 2021 müssen raus weil sie teilweise auch zweite Durchgänge gemacht haben
#exclude non-solvers from Dataset 2021
a<-Daten$Jahr==2021&Daten$solver==0
Dat<-Daten[!a,]

t.test(Dat$Theorie~Dat$Jahr)
t.test(Dat$Berechnungen~Dat$Jahr)







Dat<-as.data.frame(Dat)

table(Dat$solver,Dat$Jahr)
#######################Probetest
#Hypothesis 1
##do solver have more points in a test at the end of semester?
mod1<-lm(Dat$Probetest~Dat$solver)
summary(mod1)

mod1<-lm(Dat$Relevanz~Dat$solver)
summary(mod1)

mod1<-lm(Dat$Vorwissen~Dat$solver)
summary(mod1)

mod1<-lm(Dat$Leistungman~Dat$solver)
summary(mod1)

mod1<-lm(Dat$SB1~Dat$solver)
summary(mod1)

library("rstatix")
eta_squared(mod1)

#Hypothesis 2
##Preknowledge
cor.test(Dat$Theorie,Dat$Probetest)
cor.test(Dat$Berechnungen,Dat$Probetest)

t.test(Dat$Theorie~Dat$Jahr)
t.test(Dat$Berechnungen~Dat$Jahr)

t.test(Dat$Theorie~Dat$solver)
t.test(Dat$Berechnungen~Dat$solver)
t.test(Dat$Leistung~Dat$solver)
t.test(Dat$Leistung~Dat$Jahr)

##Preparation: Attend to the two lectures
cor.test(Dat$Muhe,Dat$Probetest)
t.test(Dat$Muhe~Dat$solver)

#Dat$Vorwissen<-Dat$Berechnungen+Dat$Theorie
t.test(Dat$Vorwissen~Dat$Jahr)

t.test(Dat$Leistungma~Dat$solver)

mod_match <- matchit(solver ~  Vorwissen,
                     data = Dat, 
                     method = 'exact', distance = 'logit', 
                     ratio=1, 
                     replace=FALSE, estimand="ATT")


mod_match <- matchit(solver ~  Berechnungen +Theorie +Leistungman+Muhe+Vorlesung+rechtzeit, 
                     data = Dat, 
                     method = 'nearest', distance = 'logit', 
                     ratio=1, 
                     replace=FALSE, estimand="ATT")


library("optmatch")
mod_match <- matchit(solver ~ Vorwissen, 
                     data = Dat, 
                     method = 'nearest', distance = 'logit', 
                     ratio=1, 
                     replace=FALSE, estimand="ATT")




##
# mod_match <- matchit(Jahr ~  Vorwissen, 
#                      data = Dat, 
#                      method = 'nearest', distance = 'logit', 
#                      ratio=1, 
#                      replace=FALSE, estimand="ATT")
# 

#Daten$Vorwissen<-Daten$Berechnungen+Daten$Theorie


Daten$Jahr<-as.factor(Daten$Jahr)
mod_match <- matchit(solver ~  Berechnungen + Theorie, 
                     data = Dat, 
                     method = 'nearest', distance = 'logit', 
                     ratio=1, 
                     replace=FALSE, estimand="ATT")





summary(mod_match)
love.plot(mod_match)

# # Step 2 - restrict data to matched sample
##wenn ich auf Vorwissen matche dann unterschied in Theorie und Berechnungen nicht signi

data_match <- match.data(mod_match,distance="prop.score")
t.test(data_match$Berechnungen~data_match$Jahr)
t.test(data_match$Theorie~data_match$Jahr)
t.test(data_match$Relevanz~data_match$Jahr)

data_match$Vorwissen<-data_match$Theorie+data_match$Berechnungen


t.test(data_match$VW~data_match$Jahr)
t.test(data_match$Vorwissen~data_match$solver)
t.test(data_match$Leistungma~data_match$solver)

t.test(data_match$SB1~data_match$Jahr)
t.test(data_match$Berechnungen~data_match$Jahr)
t.test(data_match$Theorie~data_match$solver)
t.test(data_match$Vorwissen~data_match$Jahr)
t.test(data_match$Muhe~data_match$solver)
t.test(data_match$Leistung~data_match$solver)
t.test(data_match$LKW~data_match$solver)
t.test(data_match$rechtzeit~data_match$solver)


dim(data_match)
Hmisc::describe(data_match$weights)

# # Step 3 - Estimate treatment effects
# # NOTE: We use weights here,to account for control observations that were  
# #       matched to multiple treated observations

data_match$solver<-as.numeric(data_match$solver)

reg_match <- lm(Probetest ~ solver, 
                data = data_match, 
                weights = data_match$weights)
out1 <- summary(reg_match)
out1
summary(out1)

t.test(Probetest~solver, data=data_match,var.equal=FALSE)

plot(as.numeric(data_match$Probetest) ~ as.factor(data_match$solver))
t.test(as.numeric(data_match$Probetest) ~ as.factor(data_match$solver))

reg_match <- aov(Probetest ~ solver, 
                data = data_match) 
                #weights = data_match$weights)
out1 <- summary(reg_match)
out1
summary(out1)

reg_match <- t.test(Probetest ~ solver, 
                data = data_match) 
#weights = data_match$weights)
out1 <- summary(reg_match)
out1
summary(out1)



hist(data_match$Rep)
table(data_match$Rep)
describe(data_match$Rep)
ggqqplot(data_match$Rep)
shapiro.test(data_match$Rep)


data_match$Rep<-as.numeric(data_match$Rep)
data_match$solver<-as.factor(data_match$solver)
t.test(data_match$Rep~data_match$solver)

data_match %>%
  ggplot(aes(x=Rep, 
             y=Probetest, 
             color=solver))+
  geom_point()+
  geom_smooth(method="lm")


data_match %>%  anova_test(Probetest ~ solver*Rep)



model<-aov(Probetest ~ as.factor(solver)+as.numeric(Rep), 
    data = data_match)
summary(model)
Anova(model, type="III")

model<-aov(Probetest ~as.numeric(Rep)+as.factor(solver), 
           data = data_match)
summary(model)

data_match$Rep<-as.numeric(data_match$Rep)

a<-data_match$Rep==5
data_match$Rep2<-data_match$Rep
data_match$Rep2[a]<-4

a<-data_match$Rep==6
data_match$Rep2[a]<-4



model<-aov(Probetest ~as.numeric(Rep2)*as.factor(solver), 
           data = data_match)
summary(model)
anova(model)
Anova(model, type="III")

plot_model(model,type = "pred", terms = c("Rep", "solver"))
plot_model(model,type = "pred", terms = c("solver"))

##Voraussetzungen ANCOVA
A<-as.data.frame(Dat)
a<-aov(Dat$Probetest~as.numeric(Rep)*as.factor(solver))
#a<-lm(A$Performance~solver)
anova(a)
plot_model(a,type = "pred", terms = c("Rep", "solver"))

#Normality of residuals
model<-lm(Dat$Probetest~as.numeric(Rep)*as.factor(solver))
res<-A$residuals

shapiro_test(res)


t.test(as.numeric(Rep)~as.factor(solver),var.equal=FALSE)



shapiro_test(model$residuals)
hist(model$residuals)
describe(model$residuals)

levene_test(data=data_match,formula=model$residuals ~ as.factor(solver))
levene_test(data=data_match,formula=model$residuals ~ as.factor(Rep))

levene_test(data=data_match,formula=model$residuals ~ as.factor(Rep)*as.factor(solver))

plot(model$residuals ~ as.factor(solver)*as.factor(Rep))
plot(model$residuals ~ as.factor(Rep))


table(data_match$Rep,data_match$solver)
t.test(data_match$Rep~data_match$solver)
anova(a)


data_match$Rep<-as.numeric(data_match$Rep)
a<-lm(data_match$Rep~data_match$solver)
summary(a)
anova(a)


reg_match <- aov(Probetest ~ Rep*solver, 
                data = data_match)
anova(reg_match)

reg_match <- lm(Probetest ~ Rep*solver, 
                 data = data_match)
summary(reg_match)
anova(reg_match)

reg_match <- aov(Probetest ~ solver+Rep, 
                 data = data_match)
anova(reg_match)


out1 <- summary(reg_match)
out1
summary(out1)

data_match$Rep<-as.numeric(data_match$Rep)

reg_match <- lm(SB1 ~ solver+Rep, 
                data = data_match, 
                weights = data_match$weights)
out1 <- summary(reg_match)
out1
summary(out1)


reg_match <- lm(SB1 ~ solver+Vorwissen, 
                data = data_match, 
                weights = data_match$weights)
out1 <- summary(reg_match)
out1
summary(out1)


reg_match <- lm(Probetest ~ solver+VW, 
                data = data_match, 
                weights = data_match$weights)

eta_squared(reg_match)
effectsize(reg_match)

out1 <- summary(reg_match)
out1
summary(out1)
####Probetest latent anschauen?










# # Step 3 - Estimate treatment effects
# # NOTE: We use weights here,to account for control observations that were  
# #       matched to multiple treated observations
reg_match <- lm(Probetest ~ solver*as.numeric(Rep), 
                data = data_match,
                weights = data_match$weights)
out1 <- summary(reg_match)
out1
summary(out1)


reg_match <- aov(Probetest ~ as.factor(solver)*as.numeric(Rep), 
                data = data_match,
                weights = data_match$weights)


reg_match <- aov(Probetest ~as.numeric(Rep)* as.factor(solver), 
                 data = data_match,
                 weights = data_match$weights)

anova(reg_match)


reg_match <- lm(Note ~ solver+as.numeric(Rep), 
                data = data_match, 
                weights = data_match$weights)
out1 <- summary(reg_match)
out1
summary(out1)

#######################Note
#Hypothesis 1
##do solver have more points in a test at the end of semester?
mod1<-lm(Daten$Note~Daten$solver)
summary(mod1)


#Hypothesis 2
##Preknowledge
cor.test(Daten$Vorwissen,Daten$Note)
t.test(Daten$Vorwissen~Daten$solver)

##Preparation: Attend to the two lectures
cor.test(Daten$Vorlesung,Daten$Note)
t.test(Daten$Vorlesung~Daten$solver)

##Preparation: Attend to the two lectures
cor.test(Daten$Muhe,Daten$Note)
t.test(Daten$Muhe~Daten$solver)

##Hypothesis 3
# # (iii) Matching on the propensity score 
# # Step 1 - Matching
# # Note: the R code calculates ATET with the estimand=="ATT" option

# 
# mod_match <- matchit(solver ~  Vorwissen+Vorlesung + Muhe, 
#                      data = Daten, 
#                      method = 'exact', distance = 'logit', 
#                      ratio=1, 
#                      replace=FALSE, estimand="ATT")
# 
# 
# mod_match <- matchit(solver ~  Vorwissen+Vorlesung + Muhe, 
#                      data = Daten, 
#                      method = 'nearest', distance = 'logit', 
#                      ratio=1, 
#                      replace=FALSE, estimand="ATT")
# 
# 
# 
# summary(mod_match)
# love.plot(mod_match)

# # Step 2 - restrict data to matched sample
#data_match <- match.data(mod_match,distance="prop.score")


t.test(data_match$Vorwissen~data_match$solver)
t.test(data_match$Vorlesung~data_match$solver)
t.test(data_match$Muhe~data_match$solver)


dim(data_match)
Hmisc::describe(data_match$weights)

# # Step 3 - Estimate treatment effects
# # NOTE: We use weights here,to account for control observations that were  
# #       matched to multiple treated observations
reg_match <- lm(Note ~ solver, 
                data = data_match, 
                weights = data_match$weights)
out1 <- summary(reg_match)
out1
summary(out1)


t.test(Probetest ~ solver, 
   data = data_match, 
   weights = data_match$weights)

# # Step 3 - Estimate treatment effects
# # NOTE: We use weights here,to account for control observations that were  
# #       matched to multiple treated observations
reg_match <- lm(Probetest ~ solver, 
                data = data_match,
                weights = data_match$weights)
out1 <- summary(reg_match)
out1
summary(out1)


##matchit Daten kombinieren mit Datenset für Clusteranalyse
Daten<-data_match

##
