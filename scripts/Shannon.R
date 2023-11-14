rm(list=ls())
library(here)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(vegan)
library(glmmTMB)
library(lme4)
library(effects)
library(DHARMa)

# Load dataframe
# Obs att antal arter inte är samma som i artrikedom analys pga det saknades abundansmått för vissa arter
div.raw <- read.csv2(here("data", "diversity.herb.csv" ))

# Select Shannon and restructure
div <- div.raw %>% 
  select(Year, Station, Pairnr, Treatment, Shannon_H, Brillouin, Vegetation.height,
         Litter.depth, Bare.soil, Area.for.growth.m2) %>% 
  mutate(Year = as.factor(Year)) %>% 
  mutate(Treatment = as.factor(Treatment)) %>% 
  mutate(Station = as.factor(Station)) %>% 
  rename(Growth.area = Area.for.growth.m2)
div

shann.box <- ggplot(div, aes(x=Year, y=Shannon_H , color=Treatment)) + geom_boxplot()
shann.box
summary(div)

# Run analysis
model.shannon <- lm(Shannon_H ~ Treatment * Year + Vegetation.height + Litter.depth + Growth.area + Station, data=div)
summary(model.shannon)
car::Anova(model.shannon, type= "III")
AICc(model.shannon)
mod_dharma1 <- model.shannon %>% simulateResiduals(n=1000)
plot(mod_dharma1)
plotResiduals(model.shannon, rank = TRUE, quantreg = FALSE)
plot(allEffects(model.shannon))
