remove()

library(FactoMineR)
library(explor)
library(readxl)
library(ggplot2)
library(factoextra)
library(readr)
library(shiny)
library(plyr)
library(Factoshiny)
library(FactoInvestigate)


## importer la base de données
sociobase <- "C:/Users/abiol/OneDrive/Documents/cdsd_fecond_simp/cdsd_fecond_simp.csv"

grand_base <- read.csv2(sociobase)
names(grand_base)


notrebase <- grand_base[, c(2, 4, 9:11, 17:21, 35, 38, 44:50)]
b <-notrebase

#recoder les variables:car base de donnée var categorielle donc on les mets en facteur pour faire acm 
#1/ SEXE
b$F1<-as.factor(b$sexe)
b$sexe<-revalue(b$F1,c("1"= "Homme", "2"= "Femme"))
table(b$sexe)

#b$F100<-as.factor(b$sexe)
#b$fe<-revalue(b$F100,c("1"= NA, "2"= "Femme"))
#table(b$sexe)

#2 duree_relaltion
b$F2<-as.factor(b$duree_relation)
b$duree_relation<-revalue(b$F2,c("1"= "Moins d'un mois", "2"= "Plus d'un mois", "3"= "Plus d'un an", "6"= "Non concerné", "7"= NA))
table(b$duree_relation)

#3 SOUHAIT
b$F3<-as.factor(b$souhait_enfant)
b$souhait_enfant<-revalue(b$F3,c("1"= "Oui", "2"= "Non", "3"= "Je ne me sens pas concerné"))
table(b$souhait_enfant)

#4 PART_FE
irec(b$partenaires_femmes)
## Recodage de b$partenaires_femmes en b$F4a
b$F4a <- as.character(b$partenaires_femmes)
b$F4a[b$partenaires_femmes == "1"] <- "1 à 4"
b$F4a[b$partenaires_femmes == "2"] <- "1 à 4"
b$F4a[b$partenaires_femmes == "3"] <- "1 à 4"
b$F4a[b$partenaires_femmes == "4"] <- "1 à 4"
b$F4a[b$partenaires_femmes == "5"] <- "5 à 9"
b$F4a[b$partenaires_femmes == "6"] <- "5 à 9"
b$F4a[b$partenaires_femmes == "7"] <- "5 à 9"
b$F4a[b$partenaires_femmes == "8"] <- "5 à 9"
b$F4a[b$partenaires_femmes == "9"] <- "5 à 9"
b$F4a[b$partenaires_femmes == "10"] <- "10 à 19"
b$F4a[b$partenaires_femmes == "11"] <- "10 à 19"
b$F4a[b$partenaires_femmes == "12"] <- "10 à 19"
b$F4a[b$partenaires_femmes == "14"] <- "10 à 19"
b$F4a[b$partenaires_femmes == "15"] <- "10 à 19"
b$F4a[b$partenaires_femmes == "19"] <- "10 à 19"
b$F4a[b$partenaires_femmes == "20"] <- "20 à 26"
b$F4a[b$partenaires_femmes == "25"] <- "20 à 26"
b$F4a[b$partenaires_femmes == "26"] <- "20 à 26"
b$F4a[b$partenaires_femmes == "30"] <- "beaucoup"
b$F4a[b$partenaires_femmes == "40"] <- "beaucoup"
b$F4a[b$partenaires_femmes == "50"] <- "beaucoup"
b$F4a[b$partenaires_femmes == "57"] <- "beaucoup"
b$F4a[b$partenaires_femmes == "60"] <- "beaucoup"
b$F4a[b$partenaires_femmes == "75"] <- "beaucoup"
b$F4a[b$partenaires_femmes == "80"] <- "beaucoup"
b$F4a[b$partenaires_femmes == "90"] <- "beaucoup"
b$F4a[b$partenaires_femmes == "99"] <- "beaucoup"
b$F4a[b$partenaires_femmes == "777"] <- NA
b$F4a[b$partenaires_femmes == "888"] <- NA
b$partenaires_femmes <- factor(b$F4a)
table(b$partenaires_femmes)

#5 PART HO
irec(b$partenaires_hommes)
## Recodage de b$partenaires_hommes en b$F5a
b$F5a <- as.character(b$partenaires_hommes)
b$F5a[b$partenaires_hommes == "1"] <- "1 à 4"
b$F5a[b$partenaires_hommes == "2"] <- "1 à 4"
b$F5a[b$partenaires_hommes == "3"] <- "1 à 4"
b$F5a[b$partenaires_hommes == "4"] <- "1 à 4"
b$F5a[b$partenaires_hommes == "5"] <- "5 à 9"
b$F5a[b$partenaires_hommes == "6"] <- "5 à 9"
b$F5a[b$partenaires_hommes == "7"] <- "5 à 9"
b$F5a[b$partenaires_hommes == "8"] <- "5 à 9"
b$F5a[b$partenaires_hommes == "9"] <- "5 à 9"
b$F5a[b$partenaires_hommes == "10"] <- "10 à 19"
b$F5a[b$partenaires_hommes == "11"] <- "10 à 19"
b$F5a[b$partenaires_hommes == "12"] <- "10 à 19"
b$F5a[b$partenaires_hommes == "13"] <- "10 à 19"
b$F5a[b$partenaires_hommes == "14"] <- "10 à 19"
b$F5a[b$partenaires_hommes == "15"] <- "10 à 19"
b$F5a[b$partenaires_hommes == "17"] <- "10 à 19"
b$F5a[b$partenaires_hommes == "18"] <- "10 à 19"
b$F5a[b$partenaires_hommes == "19"] <- "10 à 19"
b$F5a[b$partenaires_hommes == "20"] <- "20 à 26"
b$F5a[b$partenaires_hommes == "23"] <- "20 à 26"
b$F5a[b$partenaires_hommes == "25"] <- "20 à 26"
b$F5a[b$partenaires_hommes == "28"] <- "20 à 26"
b$F5a[b$partenaires_hommes == "30"] <- "beaucoup"
b$F5a[b$partenaires_hommes == "40"] <- "beaucoup"
b$F5a[b$partenaires_hommes == "50"] <- "beaucoup"
b$F5a[b$partenaires_hommes == "60"] <- "beaucoup"
b$F5a[b$partenaires_hommes == "90"] <- "beaucoup"
b$F5a[b$partenaires_hommes == "99"] <- "beaucoup"
b$F5a[b$partenaires_hommes == "777"] <- NA
b$F5a[b$partenaires_hommes == "888"] <- NA
b$partenaires_hommes <- factor(b$F5a)
table(b$partenaires_hommes)

#6 CONT_PILULE
b$F6<-as.factor(b$cont_pilule)
b$cont_pilule<-revalue(b$F6,c("1"= "Choisi", "2"= "Non-choisi", "6"= NA, "7"= NA,"8"= NA))
table(b$cont_pilule)

#"9"= "N'a pas répondu"

#7 STERILET
b$F7<-as.factor(b$cont_sterilet)

b$cont_sterilet<-revalue(b$F7,c("1"= "Choisi", "2"= "Non-choisi", "6"= NA, "7"= NA, "8"= NA ))
table(b$cont_sterilet)

#8 IMPLANT
b$F8<-as.factor(b$cont_implant)
b$cont_implant<-revalue(b$F8,c("1"= "Choisi", "2"= "Non-choisi", "6"= NA, "7"= NA, "8"= NA))
table(b$cont_implant)

#9 preservatif 

b$F9<-as.factor(b$cont_preservatif_masculin)
b$cont_preservatif_masculin<-revalue(b$F9,c("1"= "Choisi", "2"= "Non-choisi", "6"= NA, "7"= NA, "8"= NA))
table(b$cont_preservatif_masculin)

#10 retrait
b$F10<-as.factor(b$cont_retrait)
b$cont_retrait<-revalue(b$F10,c("1"= "Choisi", "2"= "Non-choisi", "6"= NA, "7"= NA, "8"= NA))
table(b$cont_retrait)

#11 PROBLEME ENDOMEDRIOSE
b$F11<-as.factor(b$probleme_endometriose)
b$probleme_endometriose<-revalue(b$F11,c("1"= "Choisi", "2"= "Non-choisi", "6"= NA))
table(b$probleme_endometriose)

#12 MEDECIN GYNECO
b$F12<-as.factor(b$medecin_gyneco)
b$medecin_gyneco<-revalue(b$F12,c("1"= "Oui", "2"= "Non", "6"= NA))
table(b$medecin_gyneco)

#13 DEMO AGE
b$F13<-as.factor(b$demo_age)
irec(b$demo_age)
## Recodage de b$demo_age
b$demo_age <- as.character(b$demo_age)
b$demo_age[b$demo_age == "1"] <- "- de 29ans"
b$demo_age[b$demo_age == "2"] <- "30 à 34 ans"
b$demo_age[b$demo_age == "3"] <- "35 à 39 ans"
b$demo_age[b$demo_age == "4"] <- "40 à 44 ans"
b$demo_age[b$demo_age == "5"] <- "45 à 49 ans"
b$demo_age[b$demo_age == "6"] <- "50 à 54 ans"
b$demo_age[b$demo_age == "7"] <- "55à 59 ans"
b$demo_age[b$demo_age == "8"] <- "60 à 64 ans"
b$demo_age[b$demo_age == "9"] <- "plus de 65 ans"
table(b$demo_age)

#14 DEMO DIPLOME
b$F14<-as.factor(b$demo_diplome)
b$diplome<-revalue(b$F14,c("1"= "Niveau VI - sans diplôme ou Brevet des collèges", "2"= "Niveau V - CAP ou BEP", "3"= "niveau IV - Baccalauréat général, technologique ou professionnel", "4"= "Niveau III - diplômes de niveau Bac+2 (DUT, BTS, DEUG, écoles des formations sanitaires ou sociales,...)", "5"= "Niveau III - diplômes de niveau Bac+2 (DUT, BTS, DEUG, écoles des formations sanitaires ou sociales,...)", "96"= "Non enquêté"))
table(b$diplome)

#15 DEMO REVENUS
b$F15<-as.factor(b$demo_revenus)
b$demo_revenus<-revalue(b$F15,c("3" = "Moins de 800€", "4" = "De 800 à 1000€" , "5" = "De 1000 à 1200€","6" = "De 1200 à 1500€", "7" = "De 1500 à 1800€", "8" = "De 1800 à 2000€",  "9" = "De 2000 à 2500€", "10" = "De 2500 À 3000€", "11" = "De 3000 à 4000€", "12" = "De 4000 à 6000€", "13" =  "6000€ et plus", "77" = NA, "88" = NA, "96" = NA))
table(b$demo_revenus)

#16 religion importance
b$F16<-as.factor(b$religion_importance)
b$religion_importance<-revalue(b$F16,c("1" = "Pas du tout d'importance",  "2" = "Un peu d'importance", "3" = "Assez d'importance" , "4" = "Beaucoup d'importance", "7" = NA, "8" = NA, "96" = NA))
table(b$religion_importance)

#17 religion education
b$F17<-as.factor(b$religion_education)
b$religion_education<-revalue(b$F17,c("1" = "Oui", "2" = "Non", "7" = NA, "96" = NA))
table(b$religion_education)

#18 politiqque
b$F18<-as.factor(b$politique_gauche_droite)

irec(b$politique_gauche_droite)

  ## Recodage de b$politique_gauche_droite
b$politique_gauche_droite <- as.character(b$politique_gauche_droite)
b$politique_gauche_droite <- factor(b$politique_gauche_droite)
table(b$politique_gauche_droite)

#19 HABITAT
b$F19<-as.factor(b$habitat_TUU2014)
b$habitat_TUU2014<-revalue(b$F19,c("0" = "Commune rurale", "1" = "Commune appartenant à une unité urbaine de 2 000 à 19 999 habitants", "2" = "Commune appartenant à une unité urbaine de 20 000 à 199 999 habitants", "3" = "Commune appartenant à une unité urbaine de 200 000 à 1 999 999 habitants", "4" = "Commune apparenant à l'aire urbaine de Paris"))
table(b$F19)
irec(b$F19)

#une nouvelle base 
b1 <- b[, c(1:12,13,14, 15:18,34,36)]


# realisation des tableaux croisés

library(descr) 
#3.1 
CrossTable(b1$souhait_enfant,b1$cont_preservatif_masculin,
           prop.t = F, prop.r = F, prop.c = T,
           prop.chisq = F, chisq = T,format = "SPSS")

CrossTable(b1$souhait_enfant,b1$cont_pilule,
           prop.t = F, prop.r = F, prop.c = T,
           prop.chisq = F, chisq = T,format = "SPSS")

CrossTable(b1$souhait_enfant,b1$cont_sterilet,
           prop.t = F, prop.r = F, prop.c = T,
           prop.chisq = F, chisq = T,format = "SPSS")

CrossTable(b1$souhait_enfant,b1$cont_implant,
           prop.t = F, prop.r = F, prop.c = T,
           prop.chisq = F, chisq = T,format = "SPSS")

CrossTable(b1$souhait_enfant,b1$cont_retrait,
           prop.t = F, prop.r = F, prop.c = T,
           prop.chisq = F, chisq = T,format = "SPSS")

#3.2

CrossTable(b1$partenaires_femmes,b1$cont_preservatif_masculin,
           prop.t = F, prop.r = F, prop.c = T,
           prop.chisq = F, chisq = T,format = "SPSS")

#3.3
CrossTable(b1$souhait_enfant,b1$duree_relation,
           prop.t = F, prop.r = F, prop.c = T,
           prop.chisq = F, chisq = T,format = "SPSS")

#3.4
CrossTable(b1$cont_pilule,b1$probleme_endometriose,
           prop.t = F, prop.r = F, prop.c = T,
           prop.chisq = F, chisq = T,format = "SPSS")

#3.5
CrossTable(b1$cont_preservatif_masculin,b1$demo_diplome,
           prop.t = F, prop.r = F, prop.c = T,
           prop.chisq = F, chisq = T,format = "SPSS")

CrossTable(b1$cont_pilule,b1$demo_diplome,
           prop.t = F, prop.r = F, prop.c = T,
           prop.chisq = F, chisq = T,format = "SPSS")

CrossTable(b1$cont_sterilet,b1$demo_diplome,
           prop.t = F, prop.r = F, prop.c = T,
           prop.chisq = F, chisq = T,format = "SPSS")

#3.6
CrossTable(b1$souhait_enfant,b1$medecin_gyneco,
           prop.t = F, prop.r = F, prop.c = T,
           prop.chisq = F, chisq = T,format = "SPSS")

CrossTable(b1$cont_sterilet,b1$demo_age,
           prop.t = F, prop.r = F, prop.c = T,
           prop.chisq = F, chisq = T,format = "SPSS")


#tableau croisé supplémentaire
#
CrossTable(b1$cont_pilule,b1$demo_revenus,
           prop.t = F, prop.r = F, prop.c = T,
           prop.chisq = F, chisq = T,format = "SPSS")

CrossTable(b1$cont_pilule,b1$habitat_TUU2014,
           prop.t = F, prop.r = F, prop.c = T,
           prop.chisq = F, chisq = T,format = "SPSS")

CrossTable(b1$cont_sterilet,b1$demo_age,
           prop.t = F, prop.r = F, prop.c = T,
           prop.chisq = F, chisq = T,format = "SPSS")

CrossTable(b1$cont_sterilet,b1$demo_age,
           prop.t = F, prop.r = F, prop.c = T,
           prop.chisq = F, chisq = T,format = "SPSS")

#mesurer la longueur des variables
length(b1$cont_sterilet)
length(b1$demo_age)









#REALISER L'ACM
library(questionr)
library(ade4)

acm <- dudi.acm(b1)

acm <- dudi.acm(b1, scannf = FALSE, nf = 5)

summary(acm)

explor(acm)

#acm1 <- MCA(b1, ncp = 5, graph = FALSE)
#acm1
# On demande également d'obtenir le calcul des valeurs propres (ou "inerties") de chaque axe :
screeplot(acm)

# La fonction "summary" permet d'obtenir les valeurs propres chiffrées :
summary(acm)

library(dplyr)
b2<-b1 %>% 
  filter(b1, (as.numeric("Femme"))
enir

d4 <- d %>%
  filter(F4a != "NA")

#vous rempacez par Fa1 par le nom de variable NA par homme 

CrossTable(actif$CSER, actif$AGE3,
           prop.t = F, prop.r = F, prop.c = T,
           prop.chisq = F, chisq = T,
           format = "SPSS")




acm <- dudi.acm(b2)
acm <- dudi.acm(b2, scannf = FALSE, nf = 5)
explor(acm)


# Une première représentation graphique utile consiste en un "cercle de corrélation des modalités",
# avec la fonction "s.corcircle" :
s.corcircle(acm$co, 1, 2, clabel = 0.7)


# La représentation graphique sur laquelle on s'appuie fondamentalement est le "plan factoriel",
# ici obtenu avec la fonction "s.label" :
s.label(acm$co, clabel = 0.7)

explor(acm)


