rm(list=ls())
library(here)
library(tidyverse)
library(emmeans)
library(vegan)
library(betapart)
library(lmerTest)
library(cowplot)
library(knitr)
library(pairwiseAdonis)
library(png)
library(magick)
library(ggpubr)
library(dendextend)
library(betapart)

# Load data
df.all <- read.csv2(here("data", "dataframe.csv"))

# Remove plots with zeroes
  # df.nozero <- df %>%
  #   rowwise() %>%
  #   filter(sum(c_across(5:ncol(df))) != 0)
  # df.nozero
treatment.species <- df.all[,4:ncol(df.all)]
occurrences <- df.all[,5:ncol(df.all)]
treatment <- df.all$Treatment
year <- df.all$Year


# Beta diversity: abundance based
beta.abund <- beta.pair.abund(occurrences, index.family = "bray")
# Treatment
bd.treat <-betadisper(beta.abund[[3]], treatment)
plot(bd.treat)
# Year
bd.year <-betadisper(beta.abund[[3]], year)
plot(bd.year)

# Compare species compositions across time
y.2020 <- df.all %>% 
  filter(Year == "Year 1")
y.2020.comm <- y.2020[,5:ncol(y.2020)]
pa.2020 <- presabs <- ifelse(y.2020.comm>0,1,0)

y.2022 <- df.all %>% 
  filter(Year == "Year 2")
y.2022.comm <- y.2022[,5:ncol(y.2022)]
pa.2022 <- presabs <- ifelse(y.2022.comm>0,1,0)

# Using all data:

beta.temporal <- beta.temp(pa.2020, pa.2022, index.family = "sorensen")
# beta.sim = turnover
# beta.sor = sorensen dissimilarity
# beta.sne = nestedness

# plot:
beta.years <- read.csv2(here("data", "beta.temporal.csv"))

beta.milling <- ggplot(beta.years, aes(x=Station, y=value, fill=Beta.type)) + geom_boxplot() +
  facet_wrap(vars(Treatment)) +
  ggtitle("Temporal beta diversity (incidence based: Sorensen)")
beta.milling


# Remove all plot pairs that had no species at some point (more correct apparently):
beta.years <- read.csv2(here("data", "beta.temporal.csv"))

beta.years.NoNAs <- beta.years %>% 
  filter(!Pairnr %in% c("4", "5", "6", "1", "3"))
beta.years.NoNAs


beta.milling.NoNAs <- ggplot(beta.years.NoNAs, aes(x=Station, y=value, fill=Beta.type)) + geom_boxplot() +
  facet_wrap(vars(Treatment)) +
  ggtitle("Temporal beta diversity (incidence based: Sorensen). Pairs with zero observations removed")
beta.milling.NoNAs 

# Remove Mo grindar

beta.years.NoMG <- beta.years %>% 
  filter(!Station == "Mo Grindar") %>% 
  filter(!Pairnr %in% c("1", "3"))
beta.years.NoMG


beta.milling.NoMG <- ggplot(beta.years.NoMG, aes(x=Station, y=value, fill=Beta.type)) + geom_boxplot() +
  facet_wrap(vars(Treatment)) +
  ggtitle("Temporal beta diversity (incidence based: Sorensen). Pairs with zero observations removed")
beta.milling.NoMG

