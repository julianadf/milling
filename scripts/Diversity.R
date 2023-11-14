rm(list=ls())
library(here)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(glmmTMB)
library(visreg)
library(DHARMa)
library(vegan)
library(effects)

# Does milling increase plant diversity?

# Species richness ----
rich.herbs <- read.csv2(here("data", "species.richness.örter.csv" ))

# restructure
rich <- rich.herbs %>% 
  mutate(Year = as.factor(Year)) %>% 
  mutate(Station = as.factor(Station)) %>% 
  mutate(Treatment = as.factor(Treatment)) %>% 
  rename(Growth.area = Area.for.growth.m2)
  #mutate(obs = seq(1, nrow(rich.herbs)))
str(rich)
  

model.rich <- glmmTMB(rich ~ Treatment * Year + Vegetation.height + Litter.depth + Growth.area + Station, 
                      family= compois, data=rich)

summary(model.rich)
#car::Anova(model.rich, type= "III")
#AICc(model.rich)
mod_dharma1 <- model.rich %>% simulateResiduals(n=1000)
plot(mod_dharma1)
testDispersion(mod_dharma1)
testDispersion(mod_dharma1, alternative = "less")
plot(allEffects(model.rich))
#visreg(model.rich, scale = "response", by= "Year")
#visreg(model.rich, "Treatment", "Year", gg=TRUE)#, ylab="Ozone")



# NMDS ----
# df <- read.csv2(here("data", "dataframe.csv" ))
# 
# # Remove plots with zeroes
# df.nozero <- df %>% 
#   rowwise() %>% 
#   filter(sum(c_across(5:ncol(df))) != 0)
# df.nozero
# 
# occurrences <- df.nozero[,5:ncol(df.nozero)]
# Treatment <- df.nozero$Treatment
# Year <- df.nozero$Year


df.fem <- read.csv2(here("data", "dataframe 5 occurrences.csv" ))

occurrences <- df.fem[,5:ncol(df.fem)]
Treatment <- df.fem$Treatment
Year <- df.fem$Year

# With Bray-Curtis distances
NMDS <- metaMDS(occurrences, distance = "bray",  try = 100, trymax = 1000, maxit = 20000, k=2)
stressplot(NMDS, main="Bray-Curtis")
gof <- goodness(NMDS)
plot(NMDS)
plot(NMDS, type="t", main="Without species with less than 10 total occurrences")
points(NMDS, display = "sites", cex=gof*300)

# Create figure with ggplot: treatment
site.scores <- as.data.frame(scores(NMDS)$sites)  #Using the scores function from vegan to extract the site scores and convert to a data.frame
site.scores$Treatment <- rownames(scores)  # create a column of site names, from the rownames of data.scores
site.scores$Treatment <- Treatment
head(site.scores)  #look at the data

sp.scores <- as.data.frame(scores(NMDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
sp.scores$species <- rownames(sp.scores)  # create a column of species, from the rownames of species.scores
head(sp.scores)  #look at the data

milled <- site.scores[site.scores$Treatment == "Milled", ][chull(site.scores[site.scores$Treatment =="Milled", c("NMDS1", "NMDS2")]), ]  # hull values 
control <- site.scores[site.scores$Treatment == "Control", ][chull(site.scores[site.scores$Treatment =="Control", c("NMDS1", "NMDS2")]), ]  # hull 


hull <- rbind(milled, control)  #combine treatments
hull

gg.1<- ggplot() + 
  geom_polygon(data=hull,aes(x=NMDS1,y=NMDS2,fill=Treatment),alpha=0.25) + # add the convex hulls
  geom_text(data=sp.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5, size=3) + # add the species labels
  geom_point(data= site.scores,aes(x=NMDS1,y=NMDS2,shape=Treatment,colour=Treatment),size=2) +  # add the point markers
  #geom_text(data=site.scores,aes(x=NMDS1,y=NMDS2,label=site),size=2,vjust=0) +  # add the site labels
  scale_color_manual("Treatment", values = c("#31a354", "#e6550d")) +
  scale_fill_manual("Treatment", values = c("#31a354", "#e6550d")) +
  scale_shape_discrete("Treatment") +
  coord_equal() +
  theme(legend.position = "bottom", legend.title = element_text(size=24), legend.text = element_text(size=20),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.text.x.bottom = element_text(size=26), axis.text.y.left = element_text(size=26),
        axis.text.x = element_text(angle = 90, vjust=0.5),
        axis.title.y = element_text(size=28), axis.title.x = element_text(size=28), 
        plot.title = element_text(hjust = 0, size= 26))
gg.1 

# Create figure with ggplot: year
site.scores <- as.data.frame(scores(NMDS)$sites)  #Using the scores function from vegan to extract the site scores and convert to a data.frame
site.scores$Year <- rownames(scores)  # create a column of site names, from the rownames of data.scores
site.scores$Year <- Year
head(site.scores)  #look at the data

sp.scores <- as.data.frame(scores(NMDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
sp.scores$species <- rownames(sp.scores)  # create a column of species, from the rownames of species.scores
head(sp.scores)  #look at the data

y2020 <- site.scores[site.scores$Year == "Year 1", ][chull(site.scores[site.scores$Year =="Year 1", c("NMDS1", "NMDS2")]), ]  # hull values 
y2022 <- site.scores[site.scores$Year == "Year 2", ][chull(site.scores[site.scores$Year =="Year 2", c("NMDS1", "NMDS2")]), ]  # hull 


hull <- rbind(y2020, y2022)  #combine treatments
hull

gg.2<- ggplot() + 
  geom_polygon(data=hull,aes(x=NMDS1,y=NMDS2,fill=Year),alpha=0.25) + # add the convex hulls
  geom_text(data=sp.scores,aes(x=NMDS1,y=NMDS2,label=species),alpha=0.5, size=3) + # add the species labels
  geom_point(data= site.scores,aes(x=NMDS1,y=NMDS2,shape=Year,colour=Year),size=2) +  # add the point markers
  #geom_text(data=site.scores,aes(x=NMDS1,y=NMDS2,label=site),size=2,vjust=0) +  # add the site labels
  scale_color_manual("Year", values = c("#fa9fb5", "#7fcdbb")) +
  scale_fill_manual("Year", values = c("#fa9fb5", "#7fcdbb")) +
  scale_shape_discrete("Year") +
  coord_equal() +
  theme(legend.position = "bottom", legend.title = element_text(size=24), legend.text = element_text(size=20),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.text.x.bottom = element_text(size=26), axis.text.y.left = element_text(size=26),
        axis.text.x = element_text(angle = 90, vjust=0.5),
        axis.title.y = element_text(size=28), axis.title.x = element_text(size=28), 
        plot.title = element_text(hjust = 0, size= 26))
gg.2 

