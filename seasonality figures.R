### plotting seasonal variation in seagrass events

library(readr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(data.table)

## add in missing months; only had to do this once for Best and Stachowicz data, and then replaced the main datafile.
data <- read_csv("./older data/sg.seasonality.data.csv")
vals <- data %>%
  unite(ID, variable, site, sep = "/")

vals1 <- as.data.table(vals)
setkey(vals1, ID, month)
vals2 <- as.data.frame(vals1[CJ(unique(ID), seq(min(month), max(month)))])
data1 <- vals2 %>%
  separate(ID, c("variable", "site"), sep = "/")
write.csv(data1, "sg.seasonality.csv")


## load data
data <- read_csv("sg.seasonality.csv")
data$month <- as.numeric(data$month)
head(data)
View(data)

sgb <- data %>%
  filter(., variable == "seagrass shoot biomass") %>%
  group_by(site)

sbg.fig <-
  ggplot(sgb, aes(x = month, y = relative.value)) +
  geom_jitter(aes(x = month, y = relative.value, color = site), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = site), se = FALSE) +
  ggtitle("seagrass biomass")

sbg.fig

class(data$site)
levels(unique(as.factor(data$site)))

GWI <- data %>%
  filter(., site == "Goodwin Islands") 

GWI.fig <-
  ggplot(GWI, aes(x = month, y = relative.value), group_by(variable)) +
  theme_minimal() +
  scale_x_continuous(breaks = GWI$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE) +
  ggtitle("Goodwin Islands")

GWI.fig


TW <- data %>%
  filter(., site == "Tsawwassen") 

TW.fig <-
  ggplot(TW, aes(x = month, y = relative.value), group_by(variable)) +
  theme_minimal() +
  scale_x_continuous(breaks = TW$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE) +
  ggtitle("Tsawsassen BC 2012")

TW.fig


SF <- data %>%
  filter(., site == "San Francisco Bay - KB") 

SF.fig <-
  ggplot(SF, aes(x = month, y = relative.value), group_by(variable)) +
  theme_minimal() +
  scale_x_continuous(breaks = SF$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE) +
  ggtitle("San Francisco - KB 2007")

SF.fig


SJI <- data %>%
  filter(., site == "San Juan Islands") 

SJI.fig <-
  ggplot(SJI, aes(x = month, y = relative.value), group_by(variable)) +
  theme_minimal() +
  scale_x_continuous(breaks = SJI$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE) +
  ggtitle("San Juan Islands")

SJI.fig


PB <- data %>%
  filter(., site == "Padilla Bay") 

PB.fig <-
  ggplot(PB, aes(x = month, y = relative.value), group_by(variable)) +
  theme_minimal() +
  scale_x_continuous(breaks = PB$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE) +
  ggtitle("Padilla Bay")

PB.fig

CB <- data %>%
  filter(., site == "Crescent Beach BC") 

CB.fig <-
  ggplot(CB, aes(x = month, y = relative.value), group_by(variable)) +
  theme_minimal() +
  scale_x_continuous(breaks = CB$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE) +
  ggtitle("Crescent Beach")

CB.fig


## Odawa Bay
OB <- data %>%
  filter(., site == "Odawa Bay, Japan") 
View(OB)

OB.fig <-
  ggplot(OB, aes(x = month, y = relative.value), group_by(variable)) +
  theme_minimal() +
  scale_x_continuous(breaks = OB$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE) +
  ggtitle("Odawa Bay")

OB.fig


## Bodega Bay
BB.GA <- data %>%
  filter(., site == "BodegaHarbor_bed_GA") %>%
  filter(., variable != "Zost_Flowering_biomass") %>%
  filter(., variable != "Zost_Flowering_Shoots") %>%
  filter(., variable != "Zost_Total_Shoots") %>%
  filter(., variable != "Zost_Veg_biomass") %>%
  filter(., variable != "Zost_Veg_Shoots") %>%
  filter(., variable != "Ulva_biomass") %>%
  filter(., variable != "Macrophyte_consumers")
View(BB.GA)

BB.GA.fig <-
  ggplot(BB.GA, aes(x = month, y = relative.value), group_by(variable)) +
  theme_minimal() +
  scale_x_continuous(breaks = BB.GA$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE) +
  ggtitle("Bodega Bay GA")

BB.GA.fig


BB.GD <- data %>%
  filter(., site == "BodegaHarbor_bed_GD") %>%
  filter(., variable != "Zost_Flowering_biomass") %>%
  filter(., variable != "Zost_Flowering_Shoots") %>%
  filter(., variable != "Zost_Total_Shoots") %>%
  filter(., variable != "Zost_Veg_biomass") %>%
  filter(., variable != "Zost_Veg_Shoots") %>%
  filter(., variable != "Ulva_biomass") %>%
  filter(., variable != "Macrophyte_consumers")
View(BB.GA)

BB.GD.fig <-
  ggplot(BB.GD, aes(x = month, y = relative.value), group_by(variable)) +
  theme_minimal() +
  scale_x_continuous(breaks = BB.GD$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE) +
  ggtitle("Bodega Bay GD")

BB.GD.fig


BB.GG <- data %>%
  filter(., site == "BodegaHarbor_bed_GG") %>%
  filter(., variable != "Zost_Flowering_biomass") %>%
  filter(., variable != "Zost_Flowering_Shoots") %>%
  filter(., variable != "Zost_Total_Shoots") %>%
  filter(., variable != "Zost_Veg_biomass") %>%
  filter(., variable != "Zost_Veg_Shoots") %>%
  filter(., variable != "Ulva_biomass") %>%
  filter(., variable != "Macrophyte_consumers")
View(BB.GG)

BB.GG.fig <-
  ggplot(BB.GG, aes(x = month, y = relative.value), group_by(variable)) +
  theme_minimal() +
  scale_x_continuous(breaks = BB.GG$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE) +
  ggtitle("Bodega Bay GG")

BB.GG.fig


BB.GB <- data %>%
  filter(., site == "BodegaHarbor_bed_GB") %>%
  filter(., variable != "Zost_Flowering_biomass") %>%
  filter(., variable != "Zost_Flowering_Shoots") %>%
  filter(., variable != "Zost_Total_Shoots") %>%
  filter(., variable != "Zost_Veg_biomass") %>%
  filter(., variable != "Zost_Veg_Shoots") %>%
  filter(., variable != "Ulva_biomass") %>%
  filter(., variable != "Macrophyte_consumers")
View(BB.GB)

BB.GB.fig <-
  ggplot(BB.GB, aes(x = month, y = relative.value), group_by(variable)) +
  theme_minimal() +
  scale_x_continuous(breaks = BB.GB$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE) +
  ggtitle("Bodega Bay GB")

BB.GB.fig

BB.GF <- data %>%
  filter(., site == "BodegaHarbor_bed_GF") %>%
  filter(., variable != "Zost_Flowering_biomass") %>%
  filter(., variable != "Zost_Flowering_Shoots") %>%
  filter(., variable != "Zost_Total_Shoots") %>%
  filter(., variable != "Zost_Veg_biomass") %>%
  filter(., variable != "Zost_Veg_Shoots") %>%
  filter(., variable != "Ulva_biomass") %>%
  filter(., variable != "Macrophyte_consumers")
View(BB.GF)

BB.GF.fig <-
  ggplot(BB.GF, aes(x = month, y = relative.value), group_by(variable)) +
  theme_minimal() +
  scale_x_continuous(breaks = BB.GF$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE) +
  ggtitle("Bodega Bay GF")

BB.GF.fig

## all Bodega figs at once
theme_set(theme_cowplot(font_size = 10))
BB.all <- plot_grid(BB.GA.fig, BB.GB.fig, BB.GD.fig, BB.GG.fig, BB.GF.fig, nrow = 3, ncol = 2, scale = 0.9, label_size = 9, align = "v")

ggsave("BB.all.jpg", plot = BB.all, width = 10, height = 6)

BB.all

## Roscoff, France
RF <- data %>%
  filter(., site == "Roscoff, France") 
View(BB)

RF.fig <-
  ggplot(RF, aes(x = month, y = relative.value), group_by(variable)) +
  theme_minimal() +
  scale_x_continuous(breaks = RF$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE) +
  ggtitle("Roscoff France")

RF.fig

## Beaufort, NC
NC <- data %>%
  filter(., site == "Beaufort, NC") 
View(NC)

NC.fig <-
  ggplot(NC, aes(x = month, y = relative.value), group_by(variable)) +
  theme_minimal() +
  scale_x_continuous(breaks = NC$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE) +
  ggtitle("Beaufort, NC")

NC.fig


## Akkeshi-ko Estuary
AK <- data %>%
  filter(., site == "Akkeshi-ko Estuary, Japan") 
View(AK)

AK.fig <-
  ggplot(AK, aes(x = month, y = relative.value), group_by(variable)) +
  theme_minimal() +
  scale_x_continuous(breaks = AK$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE) +
  ggtitle("Akkeshi-ko")

AK.fig


OB <- data %>%
  filter(., site == "Otsuchi Bay, Japan") 

OB.fig <-
  ggplot(OB, aes(x = month, y = relative.value), group_by(variable)) +
  theme_minimal() +
  scale_x_continuous(breaks = OB$month) +
  geom_jitter(aes(x = month, y = relative.value, color = variable), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = variable), se = FALSE) +
  ggtitle("Otsuchi Bay 1996")

OB.fig
ggsave("OB.jpg", OB.fig)

## all figs at once
all <- plot_grid(GWI.fig,TW.fig, SF.fig, CB.fig, OB.fig, nrow = 3, ncol = 2, scale = 0.9, label_size = 9, align = "v")

cowplot::ggsave("allsites.jpg", plot = all, width = 8, height = 10)


### ok, this will work to visualize the patterns. next step, try to get our (and others?) seasonal data into a figure...
