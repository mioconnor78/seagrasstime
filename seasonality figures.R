### plotting seasonal variation in seagrass events

library(readr)
library(tidyverse)
library(ggplot2)


data <- read_csv("seagrass seasonality data.csv")
head(data)

sgb <- data %>%
  filter(., variable == "seagrass shoot biomass") %>%
  group_by(site)

seasonality <-
  ggplot(sgb, aes(x = month, y = relative.value)) +
  geom_jitter(aes(x = month, y = relative.value, color = site), na.rm = TRUE) +
  geom_smooth(aes(x = month, y = relative.value, color = site), se = FALSE) +
  ggtitle("seagrass biomass")

seasonality
