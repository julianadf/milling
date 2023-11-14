rm(list=ls())
library(here)
library(tidyverse)
library(ggplot2)
library(ggpubr)


# Create database without trees and with the new sites
raw <- read.csv2(here("data", "Raw data.csv" ))

# Select columns and transform data
db <- raw %>% 
  select(Years.after.milling, Station, Pairnr, Treatment, Vegetation.height.cm, Litter.depth.cm, Dug.holes, 
         No.subplots.with.more.than.50.bare.soil, No.subplots.with.more.than.50.rock, Area.for.growth.m2,  
         No.subplots.with.more.than.50.grass, Species, Occurrences.out.of.25, Family, Tree.bush) %>% 
  mutate(Years.after.milling = as.factor(Years.after.milling)) %>% 
  mutate(Station = as.factor(Station)) %>% 
  mutate(Treatment = as.factor(Treatment)) %>% 
  rename(Year = Years.after.milling) %>% 
  rename(Vegetation.height = Vegetation.height.cm) %>% 
  rename(Litter.depth = Litter.depth.cm)  %>% 
  rename(Bare.soil = No.subplots.with.more.than.50.bare.soil) %>% 
  rename(Rock = No.subplots.with.more.than.50.rock) %>% 
  rename(Grass = No.subplots.with.more.than.50.grass ) %>% 
  rename(Occurrences = Occurrences.out.of.25)
str(db)


# Check that the max number of subplots is 25
dotchart(db$Bare.soil, xlab="Bare soil", ylab="Observation")

soil <- db %>% 
  group_by(Year, Station, Pairnr, Treatment, Bare.soil) %>% 
  summarise()
soil

# Plot
soil.box <- ggplot(soil, aes(x=Station, y=Bare.soil, fill = Treatment)) + geom_boxplot() +
  facet_wrap(vars(Year))
soil.box


# look at dug holes in 2020
dugholes2020 <- db %>% 
  filter(Year=="Year 1") %>% 
  group_by(Year, Station, Pairnr, Treatment, Dug.holes) %>% 
  summarise()
dugholes2020

# Plot
plot(x=dugholes2020$Station, y=dugholes2020$Dug.holes)

holes.box <- ggplot(dugholes2020, aes(x=Station, y=Dug.holes, fill = Treatment)) + geom_boxplot() +
  ggtitle("Only data for 2020")
holes.box

