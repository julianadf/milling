rm(list=ls())
library(here)
library(tidyverse)
library(ggplot2)
library(lme4)
library(DHARMa)
library(vegan)
library(MuMIn)
library(visreg)
library(effects)
library(emmeans)
library(performance)
library(betapart)
library(ggpubr)

# Artdata ----

# Create database without trees and with the new sites
artdata.1722 <- read.csv2(here("Ytskrapning Rotryckning/data", "artdata ytskrapning.csv" ))

arter.skrap <- artdata.1722 %>% 
  select(År, Year, Station, Behandling, Ruta, Species, tree.bush) %>% 
  mutate(År = as.factor(År)) %>% 
  mutate(Year = as.factor(Year)) %>% 
  mutate(Station = as.factor(Station)) %>% 
  mutate(Behandling = as.factor(Behandling)) %>% 
  mutate(Ruta = as.factor(Ruta)) %>% 
  filter(Station %in% c("Brunflo", "Mattmar", "Ope", "Trångsviken")) %>% 
  distinct(.keep_all = TRUE) 
str(arter.skrap)

# Summarize total number of occurrences per species
occ.skrap <- arter.skrap %>% 
  group_by(År, Year, Station, Behandling, Species) %>% 
  summarise(Occurrences = n())
occ.skrap

# Check that the max occurrences is 6
dotchart(occ.skrap$Occurrences, xlab="Occurrences per station", ylab="Observation")

# Summarize total number of occurrences per species
sum.fig <- ggplot(occ.skrap, aes(x=fct_infreq(Species), y = Occurrences, fill=År)) + geom_col() +
  #facet_wrap(vars(Year), nrow =2) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_blank(), 
        panel.grid.major = element_blank()) 
sum.fig

sum.station <- ggplot(occ.skrap, aes(x=fct_infreq(Species), y = Occurrences, fill=År)) + geom_col() +
  facet_wrap(vars(Station), ncol =1) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_blank(), 
        panel.grid.major = element_blank()) 
sum.station

örter <- arter.skrap %>% 
  filter(tree.bush==0) %>% 
  group_by(År, Year, Station, Behandling, Species) %>% 
  summarise(Occurrences = n())
örter

örter.station <- ggplot(örter, aes(x=fct_infreq(Species), y = Occurrences, fill=År)) + geom_col() +
  facet_wrap(vars(Station), ncol =1) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_blank(), 
        panel.grid.major = element_blank()) 
örter.station + ggtitle("only herbs")

# Calculate richness per ruta for herbs only
rich.ytskr <- arter.skrap %>% 
  filter(tree.bush==0) %>% 
  group_by(Year, Station, Behandling, Ruta) %>% 
  summarise(rich=n())
rich.ytskr 

rich.box <- ggplot(rich.ytskr, aes(x=Station, y=rich, fill = Year)) + geom_boxplot() +
  ggtitle("Average species richness per station: herbs")
rich.box

# Model
mod.herbsrich0 <- glm(rich ~ Year * Station, family = poisson(link = "log"), data=rich.ytskr) #underdispersed
mod.herbsrich <- glmmTMB(rich ~ Station * Year, data=rich.ytskr, family=compois)
summary(mod.herbsrich)
nmod_dharma1 <- mod.herbsrich %>% simulateResiduals(n=1000)
plot(mod_dharma1)
testDispersion(mod_dharma1)
testDispersion(mod_dharma1, alternative = "less")
plot(allEffects(mod.herbsrich), lines= list(multiline = T), confint = list(style="auto"))
# post-hoc
emmeans(mod.herbsrich, pairwise ~ Year|Station, adjust= "fdr")$contrasts
emmeans(mod.herbsrich, pairwise ~ Station|Year, adjust= "fdr")$contrasts

emmeans(mod.herbsrich, pairwise ~ Year|Station, adjust= "bonferroni")$contrasts
emmeans(mod.herbsrich, pairwise ~ Station|Year, adjust= "bonferroni")$contrasts
x2 <- emmeans(mod.herbsrich, "Year", by="Station")
plot(x2)

# Temporal beta diversity 

# Create dataframe örter
occ.skrap.ört <- arter.skrap %>% 
  filter(tree.bush ==0) %>% 
  group_by(År, Year, Station, Behandling, Species) %>% 
  summarise(Occurrences = n())
occ.skrap.ört

# Turn into wide
df.herb <- occ.skrap.ört %>% 
  drop_na(Species) %>% 
  #drop_na(Occurrences) %>% 
  select(Year, Station, Species, Occurrences) %>% 
  spread(., key="Species", value = "Occurrences") %>% 
  replace(is.na(.), 0)
head(df.herb)

occurrences <- df.herb[,5:ncol(df.herb)]
station <- df.herb$Station
year <- df.herb$Year


# Compare species compositions across time
year1 <- df.herb %>% 
  filter(Year == "1")
y1.comm <- year1[,5:ncol(year1)]
pa.y1 <- presabs <- ifelse(y1.comm>0,1,0)

year6 <- df.herb %>% 
  filter(Year == "6")
y6.comm <- year6[,5:ncol(year6)]
pa.y6 <- presabs <- ifelse(y6.comm>0,1,0)

beta.temporal <- beta.temp(pa.y1, pa.y6, index.family = "sorensen")
# beta.sim = turnover
# beta.sor = sorensen dissimilarity
# beta.sne = nestedness

# plot:
beta.years <- read.csv2(here("data", "betatemp.ytskrap.csv"))

beta.skrap <- ggplot(beta.years, aes(x=Station, y=Value, color=Beta.type)) + geom_point() +
   ggtitle("Temporal beta diversity (incidence based: Sorensen)")
beta.skrap


# Träd och Buskar -----
trädobusk.raw <- read.csv2(here("Ytskrapning Rotryckning/data", "Träd och buskar raw.csv" ))

tbs.db <- trädobusk.raw %>% 
  select(År, Year, Station, Behandling, Ruta, Total.täckningsgrad.träd.och.buskar.., 
         Förekommande.arter, Dead.Alive, Kat.1..Årsskott., Kat.2...3cm., Kat.3..3.5.cm., 
         Kat.4..5.10.cm., Sandförekomst.., Total.täckningsgrad.per.ruta) %>% 
  mutate(År = as.factor(År)) %>% 
  mutate(Year = as.factor(Year)) %>% 
  mutate(Station = as.factor(Station)) %>% 
  mutate(Behandling = as.factor(Behandling)) %>% 
  filter(Station %in% c("Repbäcken", "Ställdalen", "Dagarn", "Dala-Järna", "Mockfjärd", "Vansbro", 
                        "Brunflo", "Mattmar", "Ope", "Trångsviken")) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename(Täckning.arter =Total.täckningsgrad.träd.och.buskar..) %>% 
  rename(Täckning.ruta = Total.täckningsgrad.per.ruta) %>% 
  rename(Sandförekomst = Sandförekomst..) %>% 
  rename(Kat1 = Kat.1..Årsskott.) %>% 
  rename(Kat2 = Kat.2...3cm.) %>% 
  rename(Kat3 = Kat.3..3.5.cm.) %>% 
  rename(Kat4 = Kat.4..5.10.cm.)
str(tbs.db)

# summarise and visualise the data
täck.db <- tbs.db %>% dplyr::filter(!(Förekommande.arter==""))
se.täckart <- täck.db %>% 
  group_by(Behandling, Year, Förekommande.arter) %>% 
  mutate(mean.täck = mean(Täckning.arter)) %>% 
  mutate(se = sd(Täckning.arter) / sqrt(length(Täckning.arter))) %>% 
  ungroup() %>% 
  group_by(Behandling, Year, Förekommande.arter, Täckning.arter, se) %>% 
  summarise()
se.täckart


gg.arttäckning <- ggplot(se.täck, aes(x=Förekommande.arter, y= mean.täck, fill=Year)) + 
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin=mean.täck-se, ymax=mean.täck+se), 
                size=.3, width=.2, position=position_dodge(.9)) +
  facet_wrap(vars(Behandling)) + 
  ylab("Medektäckningsgrad") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_blank(), 
        panel.grid.major = element_blank()) 
gg.arttäckning

se.täckruta <- tbs.db %>% 
  group_by(Station, Behandling, Year, Ruta, Täckning.ruta) %>% 
  summarise() %>% 
  mutate(mean.täck = mean(Täckning.ruta)) %>% 
  ungroup() %>% 
  group_by(Behandling, Year) %>%
  mutate(se = sd(Täckning.ruta) / sqrt(length(Täckning.ruta)))
se.täckruta

gg.täckningruta <- ggplot(se.täckruta, aes(x=Station, y= Täckning.ruta, fill=Year)) + 
  geom_bar(stat = "summary", fun= "mean", position = "dodge") + 
  geom_errorbar(aes(ymin=mean.täck-se, ymax=mean.täck+se), 
                size=.3, width=.2, position=position_dodge(.9)) +
  facet_wrap(vars(Behandling)) + 
  ylab("Medektäckningsgrad") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_blank(), 
        panel.grid.major = element_blank()) 
gg.täckningruta

se.ruta <- se.täckruta %>% group_by(Behandling, Year, se) %>% summarise(mean.ruta = mean(Täckning.ruta))

behandling.comp <- list(c("0", "1"), c("0", "6"), c("1", "6"), c("6", "6"))


gg.täckningruta2 <- ggplot(se.ruta, aes(x=Behandling, y= mean.ruta, fill=Year)) + 
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin=mean.ruta-se, ymax=mean.ruta+se), 
                size=.3, width=.2, position=position_dodge(.9)) +
  ylab("Medektäckningsgrad") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_blank(), 
        panel.grid.major = element_blank()) 
gg.täckningruta2 

# doesnt worK:
gg.täckningruta2 +
  geom_pwc(aes(group = Year), tip.length = 0,
  method = "t.test", label = "p.adj.format",
  bracket.nudge.y = -0.08
) 

# Dead or alive ----


# Sand ----

sand.db <- tbs.db %>% 
  group_by(Year, Station, Behandling, Ruta, Sandförekomst) %>% 
  summarise() %>% 
  ungroup() %>% 
  group_by(Year, Station, Behandling) %>% 
  mutate(se = sd(Sandförekomst) / sqrt(length(Sandförekomst))) %>% 
  mutate(mean.sand = mean(Sandförekomst))
sand.db

sand.summ <- sand.db %>% 
  group_by(Year, Behandling) %>% 
  mutate(se.sand = sd(Sandförekomst) / sqrt(length(Sandförekomst))) %>% 
  mutate(mean.sand=mean(Sandförekomst)) %>% 
  ungroup() %>% 
  group_by(Year, Behandling, mean.sand, se.sand) %>% 
  summarise()
sand.summ

gg.sand1 <- ggplot(sand.summ, aes(x=Station, y= mean.sand, fill=Year)) + 
  geom_col(position = "dodge") +
  geom_bar(stat = "summary", fun= "mean", position = "dodge") + 
  geom_errorbar(aes(ymin=mean.sand-se, ymax=mean.sand+se), 
                size=.3, width=.2, position=position_dodge(.9)) +
  facet_wrap(vars(Behandling)) + 
  ylab("Medelsandförekomst") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_blank(), 
        panel.grid.major = element_blank()) 
gg.sand1


gg.sand2 <- ggplot(sand.summ, aes(x=Behandling, y= mean.sand, fill=Year)) + 
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin=mean.sand-se.sand, ymax=mean.sand+se.sand), 
                size=.3, width=.2, position=position_dodge(.9)) +
  ylab("Medelsandförekomst") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), axis.title.x = element_blank(), 
        panel.grid.major = element_blank()) 
gg.sand2

# Change
sand.change<- read.csv2(here("Ytskrapning Rotryckning/data", "sand.change.csv" ))

# try modelling

# year 0 - year 6: rotrycki
db.06 <- sand.db %>% 
  filter(Behandling == c("rotryckning/slåtter", "ytskrapning/slåtter")) %>% 
  mutate(perc = Sandförekomst/100)
db.06

model.06 <- glm(perc ~ Year * Behandling + Station, data=db.06, family= quasibinomial())
summary(model.06)
check_model(model.06)
plot(allEffects(model.06))

# old code -----

# Join trees and bushes and sand
db <- left_join(trädobusk, sand, by= c("Year", "Station", "Behandling", "Ruta"))

db.mean <- db %>% 
  drop_na(Tree.species) %>% 
  group_by(Year, Station, Behandling, Ruta, Tree.species) %>% 
  mutate(mean=mean(Täckningsgrad)) 
db.mean


# Model
mod.täck <- glmmTMB(perc ~ Station + Behandling, family= beta_family(link = "logit"), data=db.final)
summary(mod.täck)
AICc(mod.täck)
mod_dharma1 <- mod.täck %>% simulateResiduals(n=1000)
plot(mod_dharma1)
plotResiduals(mod.täck, rank = TRUE, quantreg = FALSE)
plot(allEffects(mod.täck))





db.rutor <- read.csv2(here("Ytskrapning Rotryckning/data", "db.trädsandbuskar.csv"))

db.final <- db.rutor %>% 
  group_by(Year, Station, Behandling, Ruta, Sandförekomst, Täckning.per.ruta) %>% 
  summarise() %>% 
  mutate(Year = as.factor(Year)) %>% 
  mutate(Station = as.factor(Station)) %>% 
  mutate(Behandling = as.factor(Behandling)) %>% 
  mutate(perc = Täckning.per.ruta/100)
str(db.final)

gg.täckning <- ggplot(db.final, aes(x=Station, y=Täckning.per.ruta, color=Behandling)) + geom_boxplot() + facet_wrap(vars(Year))
gg.täckning






sand.raw <-  read.csv2(here("Ytskrapning Rotryckning/data", "sand.csv"))
sand <- sand.raw %>% 
  select(Year, Station, Behandling, Ruta, Sandförekomst..) %>% 
  mutate(Year = as.factor(Year)) %>% 
  mutate(Station = as.factor(Station)) %>% 
  mutate(Behandling = as.factor(Behandling)) %>% 
  filter(Station %in% c("Repbäcken", "Ställdalen", "Dagarn", "Dala-Järna", "Mockfjärd", "Vansbro",
                        "Brunflo", "Mattmar", "Ope", "Trångsviken")) %>% 
  rename(Sandförekomst = Sandförekomst..) %>% 
  drop_na(Sandförekomst) 
sand

