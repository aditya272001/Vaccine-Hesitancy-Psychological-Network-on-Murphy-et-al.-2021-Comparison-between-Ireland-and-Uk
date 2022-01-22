# Vaccine Hesitancy Psychological Network on Murphy et al., 2021:Comparison between Ireland and Uk
In this project, I have used the vaccine hesitancy data retrived from (Murphy et al., 2021) under Creative Commons license 4.0 fair use. I have 
created a mixed graphical model (Haslbeck & Waldrop, 2015) which comprises of continuous and binary variables on data from two different demographics (Uk and Ireland). 
For each country I have fitted two different networks, One Network comparises of factor scores of various 
psychological constructs using 2-Parameter Item Response theory model (Chalmers, 2012), and Basic sum-score network. 


## Demo Code


----packages-----

library(qgraph)
library(NetworkComparisonTest)
library(bootnet)
library(tidyverse)
library(mirt)
library(ltm)
library(mgm)
library(lavaan)

-----2-PL IRT Model Fit-Example------- 

Authoritarian.ireland <- 'Authoritariansm = 1-6'
Authoritarian.Model.ireland <- mirt(data = Ireland[, c(4:9)], model = Authoritarian.ireland, 
                                    itemtype = "gpcm", SE = T, verbose = F)

Coef.auth.ireland <- coef(Authoritarian.Model.ireland, IRTpars = T, simplify = T)

items.auth.ireland <- as.data.frame(Coef.auth.ireland$items)

print(items.auth.ireland)

plot(Authoritarian.Model.ireland, type = 'infotrace', which.items = c(1:6), 
     main = "", par.settings = simpleTheme(lwd = 2))
     
![Screenshot 2022-01-22 225010](https://user-images.githubusercontent.com/96023170/150649981-388d2325-0c75-4bde-8322-b5115b56c1fb.png)

plot(Authoritarian.Model.ireland, type = 'trace', which.items = c(1:6), 
     main = "", par.settings = simpleTheme(lty = 1:4, lwd = 2))

![Screenshot 2022-01-22 232645](https://user-images.githubusercontent.com/96023170/150650083-372897d5-45a4-4591-9fe0-9b36756a8367.png)

M2(Authoritarian.Model.ireland, type = "C2", calcNull = T)

Authoritarian_Ireland <- as.data.frame(fscores(Authoritarian.Model.ireland))

------Network-1-Fitting-----------------
#######_Cleaned_reverse_coded_data_#######

MGM.Ireland <- read.csv("D:/Vax_A/Ireland_mgm_reverse.csv", head = T, sep = ",")

type_mgm_ireland <- c(rep("g", 17), rep("c", 6), c("g", "g"))
level_mgm_ireland <- c(rep(1, 17), rep(2, 6), rep(1, 2))


Ireland_Network <- mgm(data = MGM.Ireland, 
                              type = type_mgm_ireland, 
                              level = level_mgm_ireland, 
                              criterion = "CV", 
                              tuning = 0, 
                       order = 2, binarySign = T)


Predict_Ireland <- predict(Ireland_Network, MGM.Ireland)
Predict_Ireland$errors
Ireland_Piela <- c(as.numeric(as.character(Predict_Ireland$errors[c(1:17, 24:25), 3])),
                   as.numeric(as.character(Predict_Ireland$errors[18:23, 4])))
Ireland_Piela


Ireland_Network_2 <- mgm(data = MGM.Ireland, 
                       type = type_mgm_ireland, 
                       level = level_mgm_ireland, 
                       criterion = "CV", 
                       tuning = 0.25, order = 2, binarySign = T)

Predict_Ireland_2 <- predict(Ireland_Network_2, MGM.Ireland)
Predict_Ireland_2$errors
Ireland_Piela_2 <- c(as.numeric(as.character(Predict_Ireland_2$errors[c(1:17, 24:25), 3])), 
                         as.numeric(as.character(Predict_Ireland_2$errors[18:23, 4])))
Ireland_Piela_2

#Pred1:0.230 0.307 0.139 0.229 0.383 0.341 0.280 0.328 0.352 0.315 0.254 0.202 0.293 0.051 0.128 0.570 0.599 0.472 0.477 0.958 0.933 0.682 0.664 0.804 0.708

#Pred2:0.232 0.294 0.140 0.231 0.382 0.342 0.278 0.327 0.352 0.314 0.257 0.202 0.293 0.047 0.129 0.570 0.598 0.474 0.475 0.958 0.933 0.680 0.664 0.804 0.708

----------Plotting_Network_1---------------

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

![Screenshot 2022-01-22 233015](https://user-images.githubusercontent.com/96023170/150650193-ff79befa-b13c-49ea-b1b3-1d79acdcc0a6.png)

########_Correlation_between_Networks_for_both_samples_############
cor.test(inputGraphIrelandTotal, inputGraphIrelandTotal_2) # cor = 0.92 (p < 0.001) 
cor.test(inputGraphIrelandTotal, inputGraphUkTotal) # cor = 0.61 (p < 0.001)
cor.test(inputGraphIrelandTotal, inputGraphUkTotal_CTT) # cor = 0.61 (p < 0.001)
cor.test(inputGraphIrelandTotal_2, inputGraphUkTotal) # cor = 0.59 (p < 0.001)
cor.test(inputGraphIrelandTotal_2, inputGraphUkTotal_CTT) # cor = 0.59 (p < 0.001)
cor.test(inputGraphUkTotal, inputGraphUkTotal_CTT) # cor = 0.99 (p < 0.001)

#_Combined_Strength_Centrality_for_both_Uk_&_Ireland_# 
pdf("Combined_Network_Strength_Centrality_Uk_and_Ireland.pdf", paper = "USr", height = 12, width = 10)
centralityPlot(list(IRT_Ireland = Plot_Ireland_mgm, CTT_Ireland = Plot_Ireland_mgm_CTT, 
                    IRT_Uk = Plot_Uk_mgm, CTT_Uk = Plot_Uk_mgm_CTT), include = c("Strength"), scale = "raw")
dev.off()

![Screenshot 2022-01-22 233145](https://user-images.githubusercontent.com/96023170/150650255-10d5c621-d9da-45f3-9424-54cf8516d6e2.png)

## Acknowledgements
Chalmers, R. P. (2012). mirt: A multidimensional item response theory package for the R environment. Journal of statistical Software, 48(1), 1-29.

Haslbeck, J., & Waldorp, L. J. (2015). mgm: Estimating time-varying mixed graphical models in high-dimensional data. arXiv preprint arXiv:1510.06871.

Murphy, J., Vallières, F., Bentall, R. P., Shevlin, M., McBride, O., Hartman, T. K., ... & Hyland, P. (2021). Psychological characteristics associated with COVID-19 vaccine hesitancy and resistance in Ireland and the United Kingdom. Nature communications, 12(1), 1-15.





## Authors

Aditya Agrawal
