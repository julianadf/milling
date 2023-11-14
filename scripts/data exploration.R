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

# Check that the max occurrences is 25
dotchart(db$Occurrences, xlab="Occurrences per plot", ylab="Observation")

# Summarize total number of occurrences per species
sum.fig <- ggplot(db, aes(x=fct_infreq(Species), y = Occurrences, fill=Treatment)) + geom_col() +
  facet_wrap(vars(Year), nrow =2) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_blank(), 
        panel.grid.major = element_blank()) 
sum.fig

# Calculate species richness per plot
rich <- db %>% 
  group_by(Year, Station, Pairnr, Treatment, Vegetation.height, Litter.depth, Bare.soil, Rock, Grass, 
           Area.for.growth.m2) %>% 
  summarise(rich = n())
rich

#write.csv2(rich, "species.richness.csv")

#rich <- read.csv2(here("data", "species.richness.all.csv" ))

# Plot
rich.box <- ggplot(rich, aes(x=Station, y=rich, fill = Treatment)) + geom_boxplot() +
  facet_wrap(vars(Year)) + 
  ylab("Species richness")
rich.box

#write.csv2(rich, "species.richness.csv")

# Remove trees and bushes and look at them separately
# Trees and bushes
trees.bushes <- db %>% 
  filter(Tree.bush ==1)
str(trees.bushes)

trees.bushes.gg <- ggplot(trees.bushes, aes(x=fct_infreq(Species), y = Occurrences, fill=Treatment)) + geom_col() +
  facet_wrap(vars(Year, Station), nrow =2) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_blank(), 
        panel.grid.major = element_blank()) +
  ggtitle("Trees and bushes only")
trees.bushes.gg


rich.trees <- trees.bushes %>% 
  group_by(Year, Station, Pairnr, Treatment, Vegetation.height, Litter.depth, Bare.soil, Rock, Grass) %>% 
  summarise(rich = n())
rich.trees

# Plot
richtrees.box <- ggplot(rich.trees, aes(x=Station, y=rich, fill = Treatment)) + geom_boxplot() +
  facet_wrap(vars(Year)) + ggtitle("Trees and bushes only")
richtrees.box

# Örter
örter <- db %>% 
  filter(Tree.bush ==0)
str(örter)

örter.gg <- ggplot(örter, aes(x=fct_infreq(Species), y = Occurrences, fill=Treatment)) + geom_col() +
  facet_wrap(vars(Year), nrow =2) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_blank(), 
        panel.grid.major = element_blank()) +
  ggtitle("Örter")
örter.gg

rich.örter <- örter %>% 
  group_by(Year, Station, Pairnr, Treatment, Vegetation.height, Litter.depth, Bare.soil, Rock, Grass, 
           Area.for.growth.m2) %>% 
  summarise(rich = n())
rich.örter

#write.csv2(rich.örter, "species.richness.örter.csv")

# Plot
örter.box <- ggplot(rich.örter, aes(x=Station, y=rich, fill = Treatment)) + geom_boxplot() +
  facet_wrap(vars(Year)) + ggtitle("Örter")
örter.box

# Create dataframe all species
# Turn into wide
df <- db %>% 
  drop_na(Species) %>% 
  #drop_na(Occurrences) %>% 
  select(Year, Station, Pairnr, Treatment, Species, Occurrences) %>% 
  spread(., key="Species", value = "Occurrences") %>% 
  replace(is.na(.), 0)
head(df)

write.csv2(df, "dataframe.csv")

# Create dataframe örter
# Turn into wide
df.herb <- db %>% 
  filter(Tree.bush ==0) %>% 
  drop_na(Species) %>% 
  #drop_na(Occurrences) %>% 
  select(Year, Station, Pairnr, Treatment, Species, Occurrences) %>% 
  spread(., key="Species", value = "Occurrences") %>% 
  replace(is.na(.), 0)
head(df.herb)

write.csv2(df.herb, "dataframe.herb.csv")

# explore the other variables
# rocks = area for growth (total plot area - area with rocks)
dotchart(db$Rock, xlab="Rock density", ylab="Observation")

rocks <- db %>% 
  group_by(Year, Station, Pairnr, Treatment, Area.for.growth.m2, Rock) %>% 
  summarise()
rocks

# Plot
rock.box <- ggplot(rocks, aes(x=Station, y=Area.for.growth.m2, fill = Treatment)) + geom_boxplot() +
  facet_wrap(vars(Year)) + ggtitle("Area available for growth")
rock.box

rock.box <- ggplot(rocks, aes(x=Station, y=Rock, fill = Treatment)) + geom_boxplot() +
  facet_wrap(vars(Year)) + ggtitle("Number of subplots with more than 50% rock")
rock.box

# grass: kommer inte använda alls. finns ett "p" nånstans som gör det till en character 
# dotchart(db$Grass, xlab="Grass density", ylab="Observation")
# 
# grass <- db %>% 
#   group_by(Year, Station, Pairnr, Treatment, Grass) %>% 
#   summarise()
# grass
# 
# # Plot
# grass.box <- ggplot(grass, aes(x=Station, y=Grass, fill = Treatment)) + geom_boxplot() +
#   facet_wrap(vars(Year))
# grass.box
# 
