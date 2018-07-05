install.packages("lme4")
install.packages("sjPlot")
install.packages("sjmisc")
install.packages("effects")
library(tidyverse)
library(lme4)
library(effects)
library(sjmisc)
bugTables <- readxl::read_xlsx("R-Formated Data/Sorted Master Bug Data Set.xlsx")
ecoTables <-readxl::read_xlsx("R-Formated Data/County Eco Map (4 Groupings).xlsx")
wingTable <-readxl::read_xlsx("R-Formated Data/Species Hosts and Wingspan.xlsx")

#mergeBugTable <- merge(bugTables,wingTable, by. = "SPECIES" )


eco6Anova<-aov(NumberOfSamples ~ NumberOfSpecies * EcologicalRegionSixCount,data = ecoTables)
eco4Anova<-aov(NumberOfSamples ~ NumberOfSpecies * EcologicalRegionFourCount,data = ecoTables)
eco6Sum<-summary(eco6Anova)
eco4Sum<-summary(eco4Anova)

testModel <- lmer(QUANTITY ~ COUNTY*(1|SPECIES), data = bugTables)
elaModel<-lmer(NumberOfSpecies~Elavation*(1|EcologicalRegionFourCount), data = ecoTables)
ecoModel <- lmer(NumberOfSamples ~ NumberOfSpecies *(1|EcologicalRegionFourCount),data = ecoTables)
areaModel <- lmer(NumberOfSpecies ~ Area * (1|EcologicalRegionFourCount), data = ecoTables)
latLongModel<-lmer(NumberOfSpecies ~ Latitude+Logitude*(1|EcologicalRegionFourCount), data = ecoTables)
tempModel<-lmer(NumberOfSpecies ~ AverageLowTemp+AverageHiTemp*(1|EcologicalRegionFourCount), data = ecoTables)
rainModel<-lmer(NumberOfSpecies ~ AveragePerciptation*(1|EcologicalRegionFourCount), data = ecoTables)

summary(elaModel)
summary(latLongModel)
summary(areaModel)
summary(tempModel)
summary(rainModel)
print(elaModel,correlation=TRUE)
print(latLongModel, correlation=TRUE)
print(areaModel,correlation=TRUE)
summary(ecoModel)
summary(testModel)
print(testModel, correlation=TRUE)

#ggplot(fortify(elaModel), aes())

sjp.lmer(testModel, y.offset = .4)
baseGraph<-ggplot(bugTables, aes(COUNTY, QUANTITY, color =SPECIES)) + geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = .6, hjust = 1))

yearGraph<-ggplot(bugTables, aes(YEAR, QUANTITY, color =SPECIES)) + geom_point() + theme(axis.text.x = element_text(angle = 90, vjust = .6, hjust = 1))

 

#testAnova<-aov(QUANTITY ~ COUNTY*SPECIES, data = bugTables)
#summary(testAnova)

pdf("Results/Graphs.pdf")
print(yearGraph)
print(baseGraph)
dev.off()

baseGraph + facet_wrap(~COUNTY) 

