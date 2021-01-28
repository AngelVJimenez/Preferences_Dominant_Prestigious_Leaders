## STUDY 1

# Set working directory

setwd(".........................................")


# open the file
dat<-readRDS(file = "dataset_study1.rds")
head(dat)

# MODELS TO PREDICT PRESTIGE
library(brms)
library(loo)


fit0<-brm(
  formula=PRESTIGEREV~1+(1|COUNTRY_NAME),
  family=cumulative("logit"), data=dat, chains = 4, iter = 5000, cores=4)
fit0<-add_criterion(fit0, c("loo", "waic"))
summary(fit0)
plotfit0<-plot(fit0)
loofit0<-loo(fit0)


# Control model

fit1<-brm(
  formula=PRESTIGEREV~mo(CLASSREV)+mo(INCOME)+mo(POLTICALIDEOLOGY)+GENDER+AGE+
    (1|COUNTRY_NAME),
  family=cumulative("logit"), data=dat, chains = 4, iter = 5000, cores=4)

fit1<-add_criterion(fit1, c("loo", "waic"))
summary(fit1)
plotfit1<-plot(fit1)
loofit1<-loo(fit1)



# Economic Uncertainty Model

fit2<-brm(
  formula=mo(ECOUNJOBREV)+mo(ECOUNEDUCATIONREV)+mo(ECOUNFOODREV)
          +mo(ECOUNMEDICINEREV)+mo(ECOUNCASHREV)+
            (1|COUNTRY_NAME),
          family=cumulative("logit"), data=dat, chains = 4, iter = 5000, cores=4)

fit2<-add_criterion(fit2, c("loo", "waic"))
summary(fit2)
plotfit2<-plot(fit2)
loofit2<-loo(fit2)


# Economic Uncertainty Model + Controls

fit3<-brm(
  formula=PRESTIGEREV~mo(CLASSREV)+mo(INCOME)+mo(POLTICALIDEOLOGY)+GENDER+AGE+ 
    mo(ECOUNJOBREV)+mo(ECOUNEDUCATIONREV)+mo(ECOUNFOODREV)+mo(ECOUNMEDICINEREV)+mo(ECOUNCASHREV)+
  (1|COUNTRY_NAME),
  family=cumulative("logit"), data=dat, chains = 4, iter = 5000, cores=4)

fit3<-add_criterion(fit3, c("loo", "waic"))
summary(fit3)
plotfit3<-plot(fit3)
summaryfit3<-loo(fit3)


# Lack of Control Model

fit4<-brm(formula=PRESTIGEREV~mo(LACK_CONTROLREV)+
            (1|COUNTRY_NAME),
          family=cumulative("logit"), data=dat, chains = 4, iter = 5000, cores=4)
fit4<-add_criterion(fit4, c("loo", "waic"))
summary(fit4)
plotfit4<-plot(fit4)
loofit4<-loo(fit4)


# Lack of Control + Controls

fit5<-brm(
  formula=PRESTIGEREV~mo(CLASSREV)+mo(INCOME)+mo(POLTICALIDEOLOGY)+GENDER+AGE+
    mo(LACK_CONTROLREV)+
  (1|COUNTRY_NAME),
  family=cumulative("logit"), data=dat, chains = 4, iter = 5000, cores=4)

fit5<-add_criterion(fit5, c("loo", "waic"))
summary(fit5)
plotfit5<-plot(fit5)
loofit5<-loo(fit5)


# Economic Uncertainty + Lack of Control

fit6<-brm(
  formula=mo(ECOUNJOBREV)+mo(ECOUNEDUCATIONREV)+mo(ECOUNFOODREV)+mo(ECOUNMEDICINEREV)+mo(ECOUNCASHREV)+
            mo(LACK_CONTROLREV)+
            (1|COUNTRY_NAME),
          family=cumulative("logit"), data=dat, chains = 4, iter = 5000, cores=4)

fit6<-add_criterion(fit6, c("loo", "waic"))
summary(fit6)
plotfit6<-plot(fit6)
loofit6<-loo(fit6)


# Economic Uncertainty + Lack of Control + Controls

fit7<-brm(
  formula=PRESTIGEREV~mo(CLASSREV)+mo(INCOME)+mo(POLTICALIDEOLOGY)+GENDER+AGE+ 
    mo(ECOUNJOBREV)+mo(ECOUNEDUCATIONREV)+mo(ECOUNFOODREV)+mo(ECOUNMEDICINEREV)+mo(ECOUNCASHREV)+
    (LACK_CONTROLREV)+
    (1|COUNTRY_NAME),
  family=cumulative("logit"), data=dat, chains = 5, iter = 5000, cores=4)


fit7<-add_criterion(fit7, c("loo", "waic"))
summary(fit7)
plotfit7<-plot(fit7)
loofit7<-loo(fit7)



# Intergroup Conflict

fit8<-brm(formula=PRESTIGEREV~mo(INTERCOUNTRYWARREV)+mo(CIVILWARREV)+mo(TERRORISTATTACKREV)+
            (1|COUNTRY_NAME),
          family=cumulative("logit"), data=dat, chains = 4, iter = 5000, cores=4)

fit8<-add_criterion(fit8, c("loo", "waic"))
summary(fit8)
plotfit8<-plot(fit8)
loofit8<-loo(fit8)


# Intergroup Conflict + Controls

fit9<-brm(formula=PRESTIGEREV~mo(CLASSREV)+mo(INCOME)+mo(POLTICALIDEOLOGY)+GENDER+AGE+ 
            mo(INTERCOUNTRYWARREV)+mo(CIVILWARREV)+mo(TERRORISTATTACKREV)+
            (1|COUNTRY_NAME),
          family=cumulative("logit"), data=dat, chains = 4, iter = 5000, cores=4)

fit9<-add_criterion(fit9, c("loo", "waic"))
summary(fit9)
plotfit9<-plot(fit9)
loofit9<-loo(fit9)


# Full Model


fit10<-brm(
  formula=PRESTIGEREV~mo(CLASSREV)+mo(INCOME)+mo(POLTICALIDEOLOGY)+GENDER+AGE+ 
    mo(ECOUNJOBREV)+mo(ECOUNEDUCATIONREV)+mo(ECOUNFOODREV)+mo(ECOUNMEDICINEREV)+mo(ECOUNCASHREV)+
    (LACK_CONTROLREV)+
    mo(INTERCOUNTRYWARREV)+mo(CIVILWARREV)+mo(TERRORISTATTACKREV)+
    (1|COUNTRY_NAME),
  family=cumulative("logit"), data=dat, chains = 4, iter = 5000, cores=4)

fit10<-add_criterion(fit10, c("loo", "waic"))
summary(fit10)
plotfit10<-plot(fit10)
loofit10<-loo(fit10)


# Model Comparisons
loo_list <- list(fit0, fit1, fit3, fit5, fit9, fit10)
loo_model_weights(loo_list)

