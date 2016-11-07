### plotting seasonal variation in seagrass events

library(readr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)


data <- read_csv("sg.seasonality.data.csv")
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




## all figs at once
all <- plot_grid(GWI.fig,TW.fig, SF.fig, PB.fig, SJI.fig, nrow = 3, ncol = 2)

cowplot::ggsave("allsites.jpg", plot = all)

### ok, this will work to visualize the patterns. next step, try to get our (and others?) seasonal data into a figure...
