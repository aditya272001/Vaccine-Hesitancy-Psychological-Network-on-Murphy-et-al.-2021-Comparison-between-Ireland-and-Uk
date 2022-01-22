#######################
######_Packages_#######

library(qgraph)
library(NetworkComparisonTest)
library(bootnet)
library(tidyverse)
library(mirt)
library(ltm)
library(mgm)
library(lavaan)

#######################
#####_DATA-IMPORT######

Ireland <- read.csv("D:/Vax_A/Ireland1.csv")
Uk <- read.csv("D:/Vax_A/Uk.csv")

Ireland[,c(1:91)][Ireland[,c(1:91)] == "-99"] <- NA
Ireland <- na.omit(Ireland)
Ireland$CRT1[Ireland$CRT1 == "5"] <- 0
Ireland$CRT1[Ireland$CRT1 == "6"] <- 0
Ireland$CRT1[Ireland$CRT1 == "7"] <- 0
Ireland$CRT2[Ireland$CRT2 == "5"] <- 0
Ireland$CRT2[Ireland$CRT2 == "6"] <- 0
Ireland$CRT2[Ireland$CRT2 == "7"] <- 0
Ireland$CRT3[Ireland$CRT3 == "5"] <- 0
Ireland$CRT3[Ireland$CRT3 == "6"] <- 0
Ireland$CRT3[Ireland$CRT3 == "7"] <- 0

Uk[,c(1:151)][Uk[,c(1:151)] == "-99"] <- NA
Uk <- na.omit(Uk)
write.csv(Uk, "D:/Vax_A/WithoutNAuk.csv")

#####################################
#######_Data_fit_Mirt_Ireland########

########_Authoritarians_############
Authoritarian.ireland <- 'Authoritariansm = 1-6'
Authoritarian.Model.ireland <- mirt(data = Ireland[, c(4:9)], model = Authoritarian.ireland, 
                                    itemtype = "gpcm", SE = T, verbose = F)

Coef.auth.ireland <- coef(Authoritarian.Model.ireland, IRTpars = T, simplify = T)

items.auth.ireland <- as.data.frame(Coef.auth.ireland$items)

print(items.auth.ireland)

plot(Authoritarian.Model.ireland, type = 'infotrace', which.items = c(1:6), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(Authoritarian.Model.ireland, type = 'trace', which.items = c(1:6), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))

M2(Authoritarian.Model.ireland, type = "C2", calcNull = T)

Authoritarian_Ireland <- as.data.frame(fscores(Authoritarian.Model.ireland))
#################################################################################

########_Socio-dominance_Orientation_#######
Sociodominance.ireland <- 'SDO = 1-8'
Sociodominance.Model.ireland <- mirt(data = Ireland[, c(10:17)], model = Sociodominance.ireland, 
                                     itemtype = "gpcm", SE = T, verbose = F)

Coef.Sociodominance.ireland <- coef(Sociodominance.Model.ireland, IRTpars = T, simplify = T)

items.Sociodominance.ireland <- as.data.frame(Coef.Sociodominance.ireland$items)

print(items.Sociodominance.ireland)

plot(Sociodominance.Model.ireland, type = 'infotrace', which.items = c(1:8), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(Sociodominance.Model.ireland, type = 'trace', which.items = c(1:8), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))
plot(Sociodominance.Model.ireland)

M2(Sociodominance.Model.ireland, type = "C2", calcNull = T)
Sociodominance_Ireland <- as.data.frame(fscores(Sociodominance.Model.ireland))
##################################################################################

#########_CRT_############# 
CRT.Ireland <- 'CRT = 1-3'
CRT.Model.ireland <- mirt(Ireland[, c(83:85)], model = CRT.Ireland, 
                          itemtype = '2PL', SE = T, verbose = F)
Coef.CRT.ireland <- coef(CRT.Model.ireland, IRTpars = T, simplify = T)

items.CRT.ireland <- as.data.frame(Coef.CRT.ireland$items)

print(items.CRT.ireland)

plot(CRT.Model.ireland, type = 'infotrace', which.items = c(1:3), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(CRT.Model.ireland, type = 'trace', which.items = c(1:3), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))
plot(CRT.Model.ireland)

M2(CRT.Model.ireland, type = "C2", calcNull = T)
CRT_Ireland <- as.data.frame(fscores(CRT.Model.ireland))
#############################

##########_Belief_in_conspiracry_#################
Conspiracy.Ireland<- 'Conspiracy = 1-5'
Conspiracy.Model.ireland <- mirt(data = Ireland[, c(38:42)], model = Conspiracy.Ireland, 
                                     itemtype = "gpcm", SE = T, verbose = F)

Coef.Conspiracy.ireland <- coef(Conspiracy.Model.ireland, IRTpars = T, simplify = T)

items.Conspiracy.ireland <- as.data.frame(Coef.Conspiracy.ireland$items)

print(items.Conspiracy.ireland)

plot(Conspiracy.Model.ireland, type = 'infotrace', which.items = c(1:5), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(Conspiracy.Model.ireland, type = 'trace', which.items = c(1:5), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))
plot(Conspiracy.Model.ireland)

M2(Conspiracy.Model.ireland, type = "C2", calcNull = T)
Conspiracy_Ireland <- fscores(Conspiracy.Model.ireland)
######################################################

##########_Paranoia_#################
Paranoia.Ireland<- 'Paranoia = 1-5'
Paranoia.Model.ireland <- mirt(data = Ireland[, c(59:63)], model = Paranoia.Ireland, 
                                 itemtype = "gpcm", SE = T, verbose = F)

Coef.Paranoia.ireland <- coef(Paranoia.Model.ireland, IRTpars = T, simplify = T)

items.Paranoia.ireland <- as.data.frame(Coef.Paranoia.ireland$items)

print(items.Paranoia.ireland)

plot(Paranoia.Model.ireland, type = 'infotrace', which.items = c(1:5), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(Paranoia.Model.ireland, type = 'trace', which.items = c(1:5), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))
plot(Conspiracy.Model.ireland)

M2(Paranoia.Model.ireland, type = "C2", calcNull = T)
Paranoia_Ireland <- fscores(Paranoia.Model.ireland)
#########################################################

###########_LOC_1_Internal_Locus##################
LOC1.Ireland<- 'LOC_1 = 1-3'
LOC1.Model.ireland <- mirt(data = Ireland[, c(67, 68, 71)], model = LOC1.Ireland, 
                               itemtype = "gpcm", SE = T, verbose = F)

Coef.LOC1.ireland <- coef(LOC1.Model.ireland, IRTpars = T, simplify = T)

items.LOC1.ireland <- as.data.frame(Coef.LOC1.ireland$items)

print(items.LOC1.ireland)

plot(LOC1.Model.ireland, type = 'infotrace', which.items = c(1:3), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(LOC1.Model.ireland, type = 'trace', which.items = c(1:3), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))
plot(LOC1.Model.ireland)

M2(LOC1.Model.ireland, type = "C2", calcNull = T)
LOC1_Ireland <- fscores(LOC1.Model.ireland)
##################################################

###########_LOC_2_Chance_Locus##################
LOC2.Ireland<- 'LOC2 = 1-3'
LOC2.Model.ireland <- mirt(data = Ireland[, c(64, 66, 72)], model = LOC2.Ireland, 
                           itemtype = "gpcm", SE = T, verbose = F)

Coef.LOC2.ireland <- coef(LOC2.Model.ireland, IRTpars = T, simplify = T)

items.LOC2.ireland <- as.data.frame(Coef.LOC2.ireland$items)

print(items.LOC2.ireland)

plot(LOC2.Model.ireland, type = 'infotrace', which.items = c(1:3), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(LOC2.Model.ireland, type = 'trace', which.items = c(1:3), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))
plot(LOC2.Model.ireland)

M2(LOC2.Model.ireland, type = "C2", calcNull = T)
LOC2_Ireland <- fscores(LOC2.Model.ireland)
##################################################

###########_LOC_3_Powerful_Locus##################
LOC3.Ireland<- 'LOC3 = 1-3'
LOC3.Model.ireland <- mirt(data = Ireland[, c(65, 69, 70)], model = LOC3.Ireland, 
                           itemtype = "gpcm", SE = T, verbose = F)

Coef.LOC3.ireland <- coef(LOC3.Model.ireland, IRTpars = T, simplify = T)

items.LOC3.ireland <- as.data.frame(Coef.LOC3.ireland$items)

print(items.LOC3.ireland)

plot(LOC3.Model.ireland, type = 'infotrace', which.items = c(1:3), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(LOC3.Model.ireland, type = 'trace', which.items = c(1:3), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))
plot(LOC3.Model.ireland)

M2(LOC3.Model.ireland, type = "C2", calcNull = T)
LOC3_Ireland <- fscores(LOC3.Model.ireland)
##################################################

##########_Trust_in_state_##############
TIS.Ireland <- 'TIS = 1-5'
TIS.Model.ireland <- mirt(data = Ireland[, c(30:34)], model = TIS.Ireland, 
                           itemtype = "gpcm", SE = T, verbose = F)

Coef.TIS.ireland <- coef(TIS.Model.ireland, IRTpars = T, simplify = T)

items.TIS.ireland <- as.data.frame(Coef.TIS.ireland$items)

print(items.TIS.ireland)

plot(TIS.Model.ireland, type = 'infotrace', which.items = c(1:5), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(TIS.Model.ireland, type = 'trace', which.items = c(1:5), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))
plot(LOC3.Model.ireland)

M2(TIS.Model.ireland, type = "C2", calcNull = T)
TIS_Ireland <- fscores(TIS.Model.ireland)

###########_Altruism_###################
Alt.Ireland <- 'Alt = 1-9'
Alt.Model.ireland <- mirt(data = Ireland[, c(21:29)], model = Alt.Ireland, 
                          itemtype = "gpcm", SE = T, verbose = F)

Coef.Alt.ireland <- coef(Alt.Model.ireland, IRTpars = T, simplify = T)

items.Alt.ireland <- as.data.frame(Coef.Alt.ireland$items)

print(items.Alt.ireland)

plot(Alt.Model.ireland, type = 'infotrace', which.items = c(1:9), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(Alt.Model.ireland, type = 'trace', which.items = c(1:9), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))
plot(LOC3.Model.ireland)

M2(Alt.Model.ireland, type = "C2", calcNull = T)
Alt_Ireland <- fscores(Alt.Model.ireland)
###########################################
##################_Personality_traits_##### *Already_reverse_scored
Agreeableness <- Ireland$Personality2 + Ireland$Personality7
Conscientiousness <- Ireland$Personality3 + Ireland$Personality8
Neuroticism <- Ireland$Personality4 + Ireland$Personality9
Openness <- Ireland$Personality5 + Ireland$Personality10
Extraversion <- Ireland$Personality1 + Ireland$Personality6

#########_DATA_Preparation_for_mgm_######
#########################################

Ireland_mgm_data <- as.data.frame(cbind(Authoritarian_Ireland, Sociodominance_Ireland, 
                                        CRT_Ireland, Conspiracy_Ireland, Paranoia_Ireland,
                                        LOC1_Ireland, LOC2_Ireland, LOC3_Ireland, TIS_Ireland, 
                                        Alt_Ireland, Agreeableness, Conscientiousness, 
                                        Neuroticism, Openness, Extraversion, Ireland$Trust_in_Irish_State, 
                                        Ireland$MigrantAttitudes1, Ireland$MigrantAttitudes2, Ireland$C19_Infected_Binary, 
                                        Ireland$C19_SomeoneClose_Infected_Binary, Ireland$Vax_Self_Binary, 
                                        Ireland$ï..Sex, Ireland$Occupation_Binary, Ireland$Education_Binary))
setwd("D:/Vax_A")
write.csv(Ireland_mgm_data, "Ireland_mgm_IRT_fscores.csv")

#######_Cleaned_reverse_coded_data_#######

MGM.Ireland <- read.csv("D:/Vax_A/Ireland_mgm_reverse.csv", head = T, sep = ",")

type_mgm_ireland <- c(rep("g", 17), rep("c", 6), c("g", "g"))
level_mgm_ireland <- c(rep(1, 17), rep(2, 6), rep(1, 2))

##Fit_model_lamdaGam_0##
Ireland_Network <- mgm(data = MGM.Ireland, 
                              type = type_mgm_ireland, 
                              level = level_mgm_ireland, 
                              criterion = "CV", 
                              tuning = 0, 
                       order = 2, binarySign = T)

# Compute Predictability
Predict_Ireland <- predict(Ireland_Network, MGM.Ireland)
Predict_Ireland$errors
Ireland_Piela <- c(as.numeric(as.character(Predict_Ireland$errors[c(1:17, 24:25), 3])),
                   as.numeric(as.character(Predict_Ireland$errors[18:23, 4])))
Ireland_Piela

##Fit_model_lamdaGam_0.25## 
##Fit_model_lamdaGam_0##
Ireland_Network_2 <- mgm(data = MGM.Ireland, 
                       type = type_mgm_ireland, 
                       level = level_mgm_ireland, 
                       criterion = "CV", 
                       tuning = 0.25, order = 2, binarySign = T)

# Compute Predictability
Predict_Ireland_2 <- predict(Ireland_Network_2, MGM.Ireland)
Predict_Ireland_2$errors
Ireland_Piela_2 <- c(as.numeric(as.character(Predict_Ireland_2$errors[c(1:17, 24:25), 3])), 
                         as.numeric(as.character(Predict_Ireland_2$errors[18:23, 4])))
Ireland_Piela_2

#Pred1:0.230 0.307 0.139 0.229 0.383 0.341 0.280 0.328 0.352 0.315 0.254 0.202 0.293 0.051 0.128 0.570 0.599 0.472 0.477 0.958 0.933 0.682 0.664 0.804 0.708
#Pred2:0.232 0.294 0.140 0.231 0.382 0.342 0.278 0.327 0.352 0.314 0.257 0.202 0.293 0.047 0.129 0.570 0.598 0.474 0.475 0.958 0.933 0.680 0.664 0.804 0.708

########Selecting_network_with_Gam=0############

Names_Ireland_Network<-c("Authoritarianism", "Socio-Dominance Orientation", "Cognitive_reflection", 
           "Conspiracry_beliefs","Paranoia_beliefs", "Internal Control", "Chance", "Powerful Others","Trust in State", "Altruism", 
           "Agreeableness", "Conscientiousness", "Neuroticism", "Openness", "Extraversion", "Migrant Attitudes 1",
           "Migrant Attitudes 2", "Self-infected with Covid", "Relative infected with Covid", "Vaccine Attitude", "Gender", 
           "Employement status", "Educational Qualification", "Trust in Scientist", "Trust in doctors")

Labels_Ireland_Network<-c("Ath^", "SDO", "CRT", "Consp^", "Para^", "Loc_1", "Loc_2", "Loc_3", "TIS", "Altru^", "Agr^", "Consc^", "Neuro^",
            "Openness", "Ext^", "MA1", "MA2", "Self^", "Rela^", "Vax_att^", "Gender", "Employement",
            "Education", "Scientists^", "Doc^")

Groups_Ireland_Network <- list("Psychological Traits" = c(1, 2, 4, 5, 6, 7, 8, 10), "Cognitive_reflection" = 3, "Personality" = c(11:15), 
                      "Infection_in_past: 0 = No, 1 = Yes" = c(18, 19), "Vaccine Attitude: 0 = No, 1 = Yes" = 20, 
                      "Employement Status: 0 = No, 1 = Yes" = 22, "Education Attainment: 0 = No, 1 = Yes" = 23, "Trust" = c(9, 24, 25),
                      "Migrant Attitude" = c(16, 17), "Gender: 0 = Female, 1 = Male" = 21 
                      )   
Groups_colors_Ireland <- list("#4B71B3","#CC79A7", "#FFC237","#E35959","#E8ED61","#8FC45A","#FFFFFF", 
                              "#64dd3c", "#3c97dd", "#c94ee4")

#create graph Total
#enables theme colorblind because we don't need to specify edge.color#Jonas_Dalege
inputGraphIrelandTotal <- Ireland_Network$pairwise$wadj
signsGraphIrelandTotal <- Ireland_Network$pairwise$signs
signsGraphIrelandTotal[which(is.na(signsGraphIrelandTotal))] <- 1
inputGraphIrelandTotal <- inputGraphIrelandTotal*signsGraphIrelandTotal
write.csv(inputGraphIrelandTotal, "Ireland_IRT_estimation.csv")

setwd("D:/Vax_A")

pdf('Vaccine_attitude_Ireland_IRT.pdf', paper = "USr", height = 12, width = 16)

Plot_Ireland_mgm <- qgraph(inputGraphIrelandTotal, 
                           theme = "Hollywood", 
                           vsize = 6.5, 
                           esize = 25, 
                           edge.labels = F,
                           layout = "spring", 
                           legend.cex = .4,
                           palette = "rainbow",
                           nodeNames = Names_Ireland_Network,
                           labels = Labels_Ireland_Network,
                           legend = T,
                           groups = Groups_Ireland_Network,
                           title = "Vaccine Attitudes Network IRT: Ireland",
                           title.cex = 1.2,
                           details = T,
                           cut = 0.07, minimum = 0.07, fade = F, curve = .3, 
                           curveAll = T, maximum = 1,
                           pie = Ireland_Piela, pieBorder = 0.1)
dev.off()

###################################################################
#################_DATA_FIT_CLASSICAL_TEST_THEORY_##################

Ath.model.CTT <- 'Authoritariansim =~ Authoritarianism1 + Authoritarianism2 + Authoritarianism3 + 
                                      Authoritarianism4 + Authoritarianism5 + Authoritarianism6'

SDO.model.CTT <- 'Socio_Dominance_Orientation =~ Social_Dominace1 + Social_Dominance2 + Social_Dominance3 +
                                         Social_Dominance4 + Social_Dominance5 + Social_Dominance6 + 
                                         Social_Dominance7 + Social_Dominance8'

LOC.model.CTT.Hierarchical <- 'Internal =~ LOC4 + LOC5 + LOC8 
                               Chance =~ LOC1 + LOC3 + LOC9
                               Powerful others =~ LOC2 + LOC6 + LOC9 
                               locus of control =~ Internal + Chance + Powerful others'

Paranoia.model.CTT <- 'Paranoia_mod =~ Paranoia1 + Paranoia2 + Paranoia3 + Paranoia4 + Paranoia5'

TIS.model.CTT <- 'TIS =~ Trust_Body1 + Trust_Body2 + Trust_Body3 + Trust_Body4 + Trust_Body5'

Conspiracy.model.CTT <- 'Conspiracy_mod =~ Conspiracy_1 + Conspiracy_2 + Conspiracy_3 + Conspiracy_4 + Conspiracy_5'

Altruism.model.CTT <- 'Altruism =~ IWAH1 + IWAH2 + IWAH3 + IWAH4 + IWAH5 + IWAH6 + IWAH7 + IWAH8 + IWAH9'

Models <- c(Ath.model.CTT, SDO.model.CTT, LOC.model.CTT.Hierarchical, Paranoia.model.CTT, TIS.model.CTT, Conspiracy.model.CTT, 
            Altruism.model.CTT)

CFA.models <- lapply(Models, cfa, data = Ireland)

Fit.measures <- lapply(CFA.models, summary, fit.measures = T)

##############_BAD_FIT_####################################

#Some_data_sorting# 

CRT <- Ireland$CRT1 + Ireland$CRT2 + Ireland$CRT3 
Internal_locus <- Ireland$LOC4 + Ireland$LOC5 + Ireland$LOC8 
Chance <- Ireland$LOC1 + Ireland$LOC3 + Ireland$LOC9
Powerful_Others <- Ireland$LOC2 + Ireland$LOC6 + Ireland$LOC7
TIS <- Ireland$Trust_Body1 + Ireland$Trust_Body2 + Ireland$Trust_Body3 + Ireland$Trust_Body4 + Ireland$Trust_Body5

Data_for_CTT <- as.data.frame(cbind(CRT, Internal_locus, Chance, Powerful_Others, TIS))
setwd("D:/Vax_A")
write.csv(Data_for_CTT, "CTTextra.csv")

################_CTT_Model_Network_FIT_#####################
############################################################

MGM.Ireland.CTT <- read.csv("D:/Vax_A/MGM.Ireland.CTT.csv", head = T, sep = ",")

##Fit_model_lamdaGam_0##
Ireland_Network_CTT1 <- mgm(data = MGM.Ireland.CTT, 
                       type = type_mgm_ireland, 
                       level = level_mgm_ireland, 
                       criterion = "CV", 
                       tuning = 0, 
                       order = 2, binarySign = T)

# Compute Predictability
Predict_Ireland_CTT1 <- predict(Ireland_Network_CTT1, MGM.Ireland.CTT)
Predict_Ireland_CTT1$errors
Ireland_Piela_CTT1 <- c(as.numeric(as.character(Predict_Ireland$errors[c(1:17, 24:25), 3])),
                   as.numeric(as.character(Predict_Ireland$errors[18:23, 4])))
Ireland_Piela_CTT1

##Fit_model_lamdaGam_0.25## 
##Fit_model_lamdaGam_0##
Ireland_Network_CTT2 <- mgm(data = MGM.Ireland.CTT, 
                         type = type_mgm_ireland, 
                         level = level_mgm_ireland, 
                         criterion = "CV", 
                         tuning = 0.25, order = 2, binarySign = T)

# Compute Predictability
Predict_Ireland_CTT2 <- predict(Ireland_Network_CTT2, MGM.Ireland.CTT)
Predict_Ireland_CTT2$errors
Ireland_Piela_CTT2 <- c(as.numeric(as.character(Predict_Ireland_CTT2$errors[c(1:17, 24:25), 3])), 
                     as.numeric(as.character(Predict_Ireland_CTT2$errors[18:23, 4])))
Ireland_Piela_CTT2

#Pred1:0.230 0.307 0.139 0.229 0.383 0.341 0.280 0.328 0.352 0.315 0.254 0.202 0.293 0.051 0.128 0.570 0.599 0.472 0.477 0.958 0.933 0.682 0.664 0.804 0.708
#Pred2:0.189 0.290 0.132 0.204 0.391 0.457 0.339 0.557 0.387 0.297 0.237 0.208 0.308 0.052 0.127 0.567 0.598 0.471 0.485 0.958 0.933 0.676 0.660 0.804 0.709

########_MODEL_WITH_GAM = 0.25_Selected#############

#create graph Total
#enables theme colorblind because we don't need to specify edge.color#Jonas_Dalege
inputGraphIrelandTotal_2 <- Ireland_Network_CTT2$pairwise$wadj
signsGraphIrelandTotal_2 <- Ireland_Network_CTT2$pairwise$signs
signsGraphIrelandTotal_2[which(is.na(signsGraphIrelandTotal_2))] <- 1
inputGraphIrelandTotal_2 <- inputGraphIrelandTotal_2*signsGraphIrelandTotal_2
write.csv(inputGraphIrelandTotal_2, "Ireland_CTT_estimation.csv")

L <- averageLayout(inputGraphIrelandTotal, inputGraphIrelandTotal_2)

setwd("D:/Vax_A")

pdf('Vaccine_attitude_Ireland_CTT.pdf', paper = "USr", height = 12, width = 16)

Plot_Ireland_mgm_CTT <- qgraph(inputGraphIrelandTotal, 
                           theme = "Hollywood", 
                           vsize = 6.5, 
                           esize = 25, 
                           edge.labels = F,
                           layout = L, 
                           legend.cex = .4,
                           palette = "rainbow",
                           nodeNames = Names_Ireland_Network,
                           labels = Labels_Ireland_Network,
                           legend = T,
                           groups = Groups_Ireland_Network,
                           title = "Vaccine Attitudes Network CTT: Ireland",
                           title.cex = 1.2,
                           details = T,
                           cut = 0.07, minimum = 0.07, fade = F, curve = .3, 
                           curveAll = T, maximum = 1,
                           pie = Ireland_Piela, pieBorder = 0.1)
dev.off()

cor.test(inputGraphIrelandTotal, inputGraphIrelandTotal_2)
#######_Invariance_test_between_IRT_and_CTT################

NCT.Ireland <- NCT(Ireland_Network, Ireland_Network_CTT2, it = 100, edges = "all", verbose = T, test.centrality = F)

##Correlation_of_0.92_between_edge  = (strength, sign, presence)_____##

############Shortest_pathways_graph#############
setwd("D:/Vax_A")
pdf("ASPL_IRT_Ireland.pdf", paper = "USr", height = 12, width = 16)
IRT_Vax <- pathways(Plot_Ireland_mgm, 
                    from = c(1:19, 21:25),
                    to = 20)
dev.off()
pdf("ASPL_CTT_Ireland.pdf", paper = "USr", height = 12, width = 16)
CTT_Vax <- pathways(Plot_Ireland_mgm_CTT, 
                    from = c(1:19, 21:25),
                    to = 20)
dev.off()
#############Degree_Centrality_graph############
pdf("Combined_Strength_centrality.pdf", paper = "USr", height = 12, width = 16)
centralityPlot(list(IRT=Plot_Ireland_mgm,CTT=Plot_Ireland_mgm_CTT), include = c("Strength"),scale = "raw")
dev.off()

##########_USE: rm(list=ls()) _ Here_Otherwise_further_analysis_will_become_a_farrago_###  
######_Now_Load_following_Data_############## 

Uk <- read.csv("D:/Vax_A/WithoutNAuk.csv", head = T, sep = ",")

#####################################
#######_Data_fit_Mirt_Uk########

########_Authoritariansm_############
Authoritarian.Uk <- 'Authoritariansm = 1-6'
Authoritarian.Model.Uk <- mirt(data = Uk[, c(4:9)], model = Authoritarian.Uk, 
                               itemtype = "gpcm", SE = T, verbose = F)

Coef.auth.Uk <- coef(Authoritarian.Model.Uk, IRTpars = T, simplify = T)

items.auth.Uk <- as.data.frame(Coef.auth.Uk$items)

print(items.auth.Uk)

plot(Authoritarian.Model.Uk, type = 'infotrace', which.items = c(1:6), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(Authoritarian.Model.Uk, type = 'trace', which.items = c(1:6), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))

M2(Authoritarian.Model.Uk, type = "C2", calcNull = T)

Authoritarian_Uk <- as.data.frame(fscores(Authoritarian.Model.Uk))
#################################################################################

########_Socio-dominance_Orientation_#######
Sociodominance.Uk <- 'SDO = 1-8'
Sociodominance.Model.Uk <- mirt(data = Uk[, c(11:18)], model = Sociodominance.Uk, 
                                itemtype = "gpcm", SE = T, verbose = F)

Coef.Sociodominance.Uk <- coef(Sociodominance.Model.Uk, IRTpars = T, simplify = T)

items.Sociodominance.Uk <- as.data.frame(Coef.Sociodominance.Uk$items)

print(items.Sociodominance.Uk)

plot(Sociodominance.Model.Uk, type = 'infotrace', which.items = c(1:8), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(Sociodominance.Model.Uk, type = 'trace', which.items = c(1:8), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))
plot(Sociodominance.Model.Uk)

M2(Sociodominance.Model.Uk, type = "C2", calcNull = T)
Sociodominance_Uk <- as.data.frame(fscores(Sociodominance.Model.Uk))
##################################################################################

#########_CRT_############# 
CRT.Uk <- 'CRT = 1-3'
CRT.Model.Uk <- mirt(Uk[, c(84:86)], model = CRT.Uk, 
                     itemtype = '2PL', SE = T, verbose = F)
Coef.CRT.Uk <- coef(CRT.Model.Uk, IRTpars = T, simplify = T)

items.CRT.Uk <- as.data.frame(Coef.CRT.Uk$items)

print(items.CRT.Uk)

plot(CRT.Model.Uk, type = 'infotrace', which.items = c(1:3), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(CRT.Model.Uk, type = 'trace', which.items = c(1:3), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))
plot(CRT.Model.Uk)

M2(CRT.Model.Uk, type = "C2", calcNull = T)
CRT_Uk <- as.data.frame(fscores(CRT.Model.Uk))
#############################

##########_Belief_in_conspiracry_#################
Conspiracy.Uk<- 'Conspiracy = 1-5'
Conspiracy.Model.Uk <- mirt(data = Uk[, c(51:55)], model = Conspiracy.Uk, 
                            itemtype = "gpcm", SE = T, verbose = F)

Coef.Conspiracy.Uk <- coef(Conspiracy.Model.Uk, IRTpars = T, simplify = T)

items.Conspiracy.Uk <- as.data.frame(Coef.Conspiracy.Uk$items)

print(items.Conspiracy.Uk)

plot(Conspiracy.Model.Uk, type = 'infotrace', which.items = c(1:5), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(Conspiracy.Model.Uk, type = 'trace', which.items = c(1:5), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))
plot(Conspiracy.Model.Uk)

M2(Conspiracy.Model.Uk, type = "C2", calcNull = T)
Conspiracy_Uk <- fscores(Conspiracy.Model.Uk)
######################################################

##########_Paranoia_#################
Paranoia.Uk<- 'Paranoia = 1-5'
Paranoia.Model.Uk <- mirt(data = Uk[, c(61:65)], model = Paranoia.Uk, 
                          itemtype = "gpcm", SE = T, verbose = F)

Coef.Paranoia.Uk <- coef(Paranoia.Model.Uk, IRTpars = T, simplify = T)

items.Paranoia.Uk <- as.data.frame(Coef.Paranoia.Uk$items)

print(items.Paranoia.Uk)

plot(Paranoia.Model.Uk, type = 'infotrace', which.items = c(1:5), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(Paranoia.Model.Uk, type = 'trace', which.items = c(1:5), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))
plot(Conspiracy.Model.Uk)

M2(Paranoia.Model.Uk, type = "C2", calcNull = T)
Paranoia_Uk <- fscores(Paranoia.Model.Uk)
#########################################################

###########_LOC_1_Internal_Locus##################
LOC1.Uk<- 'LOC_1 = 1-3'
LOC1.Model.Uk <- mirt(data = Uk[, c(70, 71, 74)], model = LOC1.Uk, 
                      itemtype = "gpcm", SE = T, verbose = F)

Coef.LOC1.Uk <- coef(LOC1.Model.Uk, IRTpars = T, simplify = T)

items.LOC1.Uk <- as.data.frame(Coef.LOC1.Uk$items)

print(items.LOC1.Uk)

plot(LOC1.Model.Uk, type = 'infotrace', which.items = c(1:3), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(LOC1.Model.Uk, type = 'trace', which.items = c(1:3), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))
plot(LOC1.Model.Uk)

M2(LOC1.Model.Uk, type = "C2", calcNull = T)
LOC1_Uk <- fscores(LOC1.Model.Uk)
##################################################

###########_LOC_2_Chance_Locus##################
LOC2.Uk<- 'LOC2 = 1-3'
LOC2.Model.Uk <- mirt(data = Uk[, c(67, 69, 75)], model = LOC2.Uk, 
                      itemtype = "gpcm", SE = T, verbose = F)

Coef.LOC2.Uk <- coef(LOC2.Model.Uk, IRTpars = T, simplify = T)

items.LOC2.Uk <- as.data.frame(Coef.LOC2.Uk$items)

print(items.LOC2.Uk)

plot(LOC2.Model.Uk, type = 'infotrace', which.items = c(1:3), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(LOC2.Model.Uk, type = 'trace', which.items = c(1:3), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))
plot(LOC2.Model.Uk)

M2(LOC2.Model.Uk, type = "C2", calcNull = T)
LOC2_Uk <- fscores(LOC2.Model.Uk)
##################################################

###########_LOC_3_Powerful_Locus##################
LOC3.Uk<- 'LOC3 = 1-3'
LOC3.Model.Uk <- mirt(data = Uk[, c(68, 72, 73)], model = LOC3.Uk, 
                      itemtype = "gpcm", SE = T, verbose = F)

Coef.LOC3.Uk <- coef(LOC3.Model.Uk, IRTpars = T, simplify = T)

items.LOC3.Uk <- as.data.frame(Coef.LOC3.Uk$items)

print(items.LOC3.Uk)

plot(LOC3.Model.Uk, type = 'infotrace', which.items = c(1:3), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(LOC3.Model.Uk, type = 'trace', which.items = c(1:3), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))
plot(LOC3.Model.Uk)

M2(LOC3.Model.Uk, type = "C2", calcNull = T)
LOC3_Uk <- fscores(LOC3.Model.Uk)
##################################################

##########_Trust_in_state_##############
TIS.Uk <- 'TIS = 1-5'
TIS.Model.Uk <- mirt(data = Uk[, c(34:38)], model = TIS.Uk, 
                     itemtype = "gpcm", SE = T, verbose = F)

Coef.TIS.Uk <- coef(TIS.Model.Uk, IRTpars = T, simplify = T)

items.TIS.Uk <- as.data.frame(Coef.TIS.Uk$items)

print(items.TIS.Uk)

plot(TIS.Model.Uk, type = 'infotrace', which.items = c(1:5), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(TIS.Model.Uk, type = 'trace', which.items = c(1:5), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))
plot(TIS.Model.Uk)

M2(TIS.Model.Uk, type = "C2", calcNull = T)
TIS_Uk <- fscores(TIS.Model.Uk)

###########_Altruism_###################
Alt.Uk <- 'Alt = 1-9'
Alt.Model.Uk <- mirt(data = Uk[, c(22:30)], model = Alt.Uk, 
                     itemtype = "gpcm", SE = T, verbose = F)

Coef.Alt.Uk <- coef(Alt.Model.Uk, IRTpars = T, simplify = T)

items.Alt.Uk <- as.data.frame(Coef.Alt.Uk$items)

print(items.Alt.Uk)

plot(Alt.Model.Uk, type = 'infotrace', which.items = c(1:9), 
     main = "", par.settings = simpleTheme(lwd = 2))

plot(Alt.Model.Uk, type = 'trace', which.items = c(1:9), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))
plot(LOC3.Model.Uk)

M2(Alt.Model.Uk, type = "C2", calcNull = T)
Alt_Uk <- fscores(Alt.Model.Uk)

###########################################################################
##############_DATA_PREPARATION_FOR_MGM####################################
Uk$VAX[Uk$VAX == "4"] <- 0
Uk$VAX[Uk$VAX == "5"] <- 0
Uk$VAX[Uk$VAX == "6"] <- 1
Uk$Sex[Uk$Sex == "2"] <- 0
Uk$Sex[Uk$Sex == "1"] <- 1
Uk$Education_Binary[Uk$Education_Binary == "0"] <- 24
Uk$Education_Binary[Uk$Education_Binary == "1"] <- 0
Uk$Education_Binary[Uk$Education_Binary == "24"] <- 1
Uk$Employ_Binary[Uk$Employ_Binary == "0"] <- 24
Uk$Employ_Binary[Uk$Employ_Binary == "1"] <- 0
Uk$Employ_Binary[Uk$Employ_Binary == "24"] <- 1
write.csv(Uk, "Clean_Uk_data_for_CTT.csv")

Uk_mgm_data <- as.data.frame(cbind(Authoritarian_Uk, Sociodominance_Uk, 
                                        CRT_Uk, Conspiracy_Uk, Paranoia_Uk,
                                        LOC1_Uk, LOC2_Uk, LOC3_Uk, TIS_Uk, 
                                        Alt_Uk, Uk$Agreeableness, Uk$Conscientiousness, 
                                        Uk$Neuroticism, Uk$Openness, Uk$Extraversion, 
                                        Uk$MIGRANT1, Uk$MIGRANT2, Uk$COVID_Infected_Self_Binary, 
                                        Uk$COVID_Infected_Rel_Binary, Uk$VAX, 
                                        Uk$Sex, Uk$Employ_Binary, Uk$Education_Binary, Uk$TRUST6, Uk$TRUST7))

setwd("D:/Vax_A")
write.csv(Uk_mgm_data, "Uk_mgm.csv")

#######_Cleaned_data_#######

MGM.Uk <- read.csv("D:/Vax_A/Uk_mgm.csv", head = T, sep = ",")
MGM.Uk <- filter(MGM.Uk, Uk.Sex < 3)

type_mgm_Uk <- c(rep("g", 17), rep("c", 6), c("g", "g"))
level_mgm_Uk <- c(rep(1, 17), rep(2, 6), rep(1, 2))

##Fit_model_lamdaGam_0##
Uk_Network <- mgm(data = MGM.Uk, 
                       type = type_mgm_Uk, 
                       level = level_mgm_Uk, 
                       lambdaSel = "CV", 
                       lambdaGam = 0, 
                       k = 2, binarySign = T)

# Compute Predictability
Predict_Uk <- predict(Uk_Network, MGM.Uk)
Predict_Uk$errors
Uk_Piela <- c(as.numeric(as.character(Predict_Uk$errors[c(1:17, 24:25), 3])),
                   as.numeric(as.character(Predict_Uk$errors[18:23, 4])))
Uk_Piela

##Fit_model_lamdaGam_0.25## 
##Fit_model_lamdaGam_0##
Uk_Network_2 <- mgm(data = MGM.Uk, 
                         type = type_mgm_Uk, 
                         level = level_mgm_Uk, 
                         lambdaSel = "CV", 
                         lambdaGam = 0.25, k = 2, binarySign = T)

# Compute Predictability
Predict_Uk_2 <- predict(Uk_Network_2, MGM.Uk)
Predict_Uk_2$errors
Uk_Piela_2 <- c(as.numeric(as.character(Predict_Uk_2$errors[c(1:17, 24:25), 3])), 
                     as.numeric(as.character(Predict_Uk_2$errors[18:23, 4])))
Uk_Piela_2

#Pred1:0.318 0.323 0.138 0.183 0.457 0.196 0.496 0.552 0.321 0.230 0.238 0.207 0.325 0.415 0.437 0.641 0.664 0.604 0.591 0.957 0.944 0.688 0.638 0.884 0.972
#Pred2:0.316 0.322 0.136 0.183 0.457 0.194 0.496 0.550 0.320 0.230 0.241 0.207 0.325 0.415 0.437 0.640 0.664 0.604 0.591 0.957 0.944 0.686 0.639 0.884 0.972

########Selecting_network_with_Gam=0############

Names_Uk_Network<-c("Authoritarianism", "Socio-Dominance Orientation", "Cognitive_reflection", 
                         "Conspiracry_beliefs","Paranoia_beliefs", "Internal Control", "Chance", "Powerful Others","Trust in State", "Altruism", 
                         "Agreeableness", "Conscientiousness", "Neuroticism", "Openness", "Extraversion", "Migrant Attitudes 1",
                         "Migrant Attitudes 2", "Self-infected with Covid", "Relative infected with Covid", "Vaccine Attitude", "Gender", 
                         "Employement status", "Educational Qualification", "Trust in Scientist", "Trust in doctors")

Labels_Uk_Network<-c("Ath^", "SDO", "CRT", "Consp^", "Para^", "Loc_1", "Loc_2", "Loc_3", "TIS", "Altru^", "Agr^", "Consc^", "Neuro^",
                          "Openness", "Ext^", "MA1", "MA2", "Self^", "Rela^", "Vax_att^", "Gender", "Employement",
                          "Education", "Scientists^", "Doc^")

Groups_Uk_Network <- list("Psychological Traits" = c(1, 2, 4, 5, 6, 7, 8, 10), "Cognitive_reflection" = 3, "Personality" = c(11:15), 
                               "Infection_in_past: 0 = No, 1 = Yes" = c(18, 19), "Vaccine Attitude: 0 = No, 1 = Yes" = 20, 
                               "Employement Status: 0 = No, 1 = Yes" = 22, "Education Attainment: 0 = No, 1 = Yes" = 23, "Trust" = c(9, 24, 25),
                               "Migrant Attitude" = c(16, 17), "Gender: 0 = Female, 1 = Male" = 21 
)   
Groups_colors_Uk <- list("#4B71B3","#CC79A7", "#FFC237","#E35959","#E8ED61","#8FC45A","#FFFFFF", 
                              "#64dd3c", "#3c97dd", "#c94ee4")

#create graph Total
#enables theme colorblind because we don't need to specify edge.color#Jonas_Dalege
inputGraphUkTotal <- Uk_Network$pairwise$wadj
signsGraphUkTotal <- Uk_Network$pairwise$signs
signsGraphUkTotal[which(is.na(signsGraphUkTotal))] <- 1
inputGraphUkTotal <- inputGraphUkTotal*signsGraphUkTotal
write.csv(inputGraphUkTotal, "Uk_IRT_estimation.csv")

setwd("D:/Vax_A")

pdf('Vaccine_attitude_Uk_IRT.pdf', paper = "USr", height = 12, width = 16)

Plot_Uk_mgm <- qgraph(inputGraphUkTotal, 
                           theme = "Hollywood", 
                           vsize = 6.5, 
                           esize = 25, 
                           edge.labels = F,
                           layout = L, 
                           legend.cex = .4,
                           palette = "rainbow",
                           nodeNames = Names_Uk_Network,
                           labels = Labels_Uk_Network,
                           legend = T,
                           groups = Groups_Uk_Network,
                           title = "Vaccine Attitudes Network IRT: Uk",
                           title.cex = 1.2,
                           details = T,
                           cut = 0.07, minimum = 0.07, fade = F, curve = .3, 
                           curveAll = T, maximum = 1,
                           pie = Uk_Piela, pieBorder = 0.1)
dev.off()

##########--------CTT--------------#########
##########__Uk_CTT_Network---------#########

CTT.Uk <- read.csv("D:/Vax_A/Uk_CTT.csv", head = T, sep = ",")
CTT.Uk <- filter(MGM.Uk, Uk.Sex < 3)

##Fit_model_lamdaGam_0##
Uk_Network_CTT <- mgm(data = CTT.Uk, 
                  type = type_mgm_Uk, 
                  level = level_mgm_Uk, 
                  lambdaSel = "CV", 
                  lambdaGam = 0, 
                  k = 2, binarySign = T)

# Compute Predictability
Predict_Uk_CTT <- predict(Uk_Network, CTT.Uk)
Predict_Uk_CTT$errors
Uk_Piela_CTT <- c(as.numeric(as.character(Predict_Uk_CTT$errors[c(1:17, 24:25), 3])),
              as.numeric(as.character(Predict_Uk_CTT$errors[18:23, 4])))
Uk_Piela_CTT

##Fit_model_lamdaGam_0.25## 

Uk_Network_2_CTT <- mgm(data = CTT.Uk, 
                    type = type_mgm_Uk, 
                    level = level_mgm_Uk, 
                    lambdaSel = "CV", 
                    lambdaGam = 0.25, k = 2, binarySign = T)

# Compute Predictability
Predict_Uk_2_CTT <- predict(Uk_Network_2_CTT, CTT.Uk)
Predict_Uk_2_CTT$errors
Uk_Piela_2_CTT <- c(as.numeric(as.character(Predict_Uk_2_CTT$errors[c(1:17, 24:25), 3])), 
                as.numeric(as.character(Predict_Uk_2_CTT$errors[18:23, 4])))
Uk_Piela_2_CTT

#Pred_1_CTT: 0.319 0.323 0.137 0.184 0.457 0.195 0.496 0.551 0.320 0.230 0.241 0.207 0.326 0.414 0.437 0.641 0.664 0.602 0.591 0.957 0.944 0.686 0.639 0.884 0.972
#Pred_2_CTT: 0.318 0.323 0.138 0.183 0.457 0.196 0.496 0.549 0.321 0.229 0.238 0.207 0.327 0.415 0.437 0.641 0.665 0.604 0.591 0.957 0.944 0.689 0.638 0.884 0.972

######_Selecting_network_with_Gam = 0.25_######
#create graph Total
#enables theme colorblind because we don't need to specify edge.color#Jonas_Dalege
inputGraphUkTotal_CTT <- Uk_Network_2_CTT$pairwise$wadj
signsGraphUkTotal_CTT <- Uk_Network_2_CTT$pairwise$signs
signsGraphUkTotal_CTT[which(is.na(signsGraphUkTotal_CTT))] <- 1
inputGraphUkTotal_CTT <- inputGraphUkTotal_CTT*signsGraphUkTotal_CTT
write.csv(inputGraphUkTotal_CTT, "Uk_CTT_estimation.csv")
cor.test(inputGraphUkTotal, inputGraphUkTotal_CTT)

#########_Correlation_Between_Networks_is_0.99_###############

setwd("D:/Vax_A")

pdf('Vaccine_attitude_Uk_CTT.pdf', paper = "USr", height = 12, width = 16)

Plot_Uk_mgm_CTT <- qgraph(inputGraphUkTotal_CTT, 
                      theme = "Hollywood", 
                      vsize = 6.5, 
                      esize = 25, 
                      edge.labels = F,
                      layout = L, 
                      legend.cex = .4,
                      palette = "rainbow",
                      nodeNames = Names_Uk_Network,
                      labels = Labels_Uk_Network,
                      legend = T,
                      groups = Groups_Uk_Network,
                      title = "Vaccine Attitudes Network CTT: Uk",
                      title.cex = 1.2,
                      details = T,
                      cut = 0.07, minimum = 0.07, fade = F, curve = .3, 
                      curveAll = T, maximum = 1,
                      pie = Uk_Piela_2_CTT, pieBorder = 0.1)
dev.off()


########_Correlation_between_Networks_for_both_samples_############
cor.test(inputGraphIrelandTotal, inputGraphIrelandTotal_2) # cor = 0.92 (p < 0.001) 
cor.test(inputGraphIrelandTotal, inputGraphUkTotal) # cor = 0.61 (p < 0.001)
cor.test(inputGraphIrelandTotal, inputGraphUkTotal_CTT) # cor = 0.61 (p < 0.001)
cor.test(inputGraphIrelandTotal_2, inputGraphUkTotal) # cor = 0.59 (p < 0.001)
cor.test(inputGraphIrelandTotal_2, inputGraphUkTotal_CTT) # cor = 0.59 (p < 0.001)
cor.test(inputGraphUkTotal, inputGraphUkTotal_CTT) # cor = 0.99 (p < 0.001)

############Shortest_pathways_graph_Uk_#############
setwd("D:/Vax_A")
pdf("ASPL_IRT_Uk.pdf", paper = "USr", height = 12, width = 16)
IRT_Vax <- pathways(Plot_Uk_mgm, 
                    from = c(1:19, 21:25),
                    to = 20)
dev.off()
pdf("ASPL_CTT_Uk.pdf", paper = "USr", height = 12, width = 16)
CTT_Vax <- pathways(Plot_Uk_mgm_CTT, 
                    from = c(1:19, 21:25),
                    to = 20)
dev.off()
#############Degree_Centrality_graph_Uk_############
pdf("Combined_Strength_centrality_Uk.pdf", paper = "USr", height = 12, width = 16)
centralityPlot(list(IRT=Plot_Uk_mgm,CTT=Plot_Uk_mgm_CTT), include = c("Strength"),scale = "raw")
dev.off()

#_Combined_Strength_Centrality_for_both_Uk_&_Ireland_# 
pdf("Combined_Network_Strength_Centrality_Uk_and_Ireland.pdf", paper = "USr", height = 12, width = 10)
centralityPlot(list(IRT_Ireland = Plot_Ireland_mgm, CTT_Ireland = Plot_Ireland_mgm_CTT, 
                    IRT_Uk = Plot_Uk_mgm, CTT_Uk = Plot_Uk_mgm_CTT), include = c("Strength"), scale = "raw")
dev.off()
