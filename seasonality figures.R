### plotting seasonal variation in seagrass events

library(readr)
library(tidyverse)
library(ggplot2)


data <- read_csv("sg.seasonality.data.csv")
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

### ok, this will work to visualize the patterns. next step, try to get our (and others?) seasonal data into a figure...
