### Overlay again but this time with Brazil category 6 

setwd("/Users/ameliagoetz/Desktop/VIP/Steno/Brazil")

library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)


## Plotting reference category 6 

Brazil6 <- tidyrefB %>%
  filter(category == "X6")

View(Brazil6)

Brazil6a <- Brazil6 %>%
  rename(whistle = category)

View(Brazil6a)

ggplot(Brazil6a) +
  geom_line(aes(x=time, y=frequency)) +
  labs(title = "Brazil ref cat 6")


## Plotting whistle 1 

Sb_20160522_154000_sel027_enc02A26_BrazilPMCBS <- read_csv("Sb_20160522_154000_sel027_enc02A26_BrazilPMCBS.csv")
View(Sb_20160522_154000_sel027_enc02A26_BrazilPMCBS)
B_6_27 <- read.csv("Sb_20160522_154000_sel027_enc02A26_BrazilPMCBS.csv", header=T, sep="," )

## adding time variable (number of columns numbered by r - 1)
B_6_27$time <- c(seq(from = 0, to = 0.159, by = 0.003))
head(B_6_27)

ggplot(B_6_27) +
  geom_line(aes(x=time, y=Peak.Frequency..Hz.)) +
  labs(title = "Sb_20160522_154000_sel027_enc02A26_BrazilPMCBS")

TidyB_6_27 <- B_6_27 %>% select(time, Peak.Frequency..Hz.) %>%
  mutate(whistle = rep("6_27", length.out = nrow(B_6_27))) %>%
  rename(frequency = Peak.Frequency..Hz.)

ggplot(data=TidyB_6_27,
       aes(x=time, y=frequency, colour=whistle)) +
  geom_line()


## joining ref cat 6 and whistle 1

join_6_1 <- full_join(Brazil6a, TidyB_6_27, by = colnames(Brazil6a))
View(join_6_1)

## Creating a plot with both ref and whistle 1

ggplot(data=join_6_1,
       aes(x=time, y=frequency, colour=whistle)) +
  geom_line()


## Adding whistle 2

Sb_20160522_154000_sel035_enc02A26_BrazilPMCBS <- read_csv("Sb_20160522_154000_sel035_enc02A26_BrazilPMCBS.csv")
View(Sb_20160522_154000_sel035_enc02A26_BrazilPMCBS)
B_6_35 <- read.csv("Sb_20160522_154000_sel035_enc02A26_BrazilPMCBS.csv", header=T, sep="," )

## adding time variable (number of columns numbered by r - 1)
B_6_35$time <- c(seq(from = 0, to = 0.096, by = 0.003))
head(B_6_35)

ggplot(B_6_35) +
  geom_line(aes(x=time, y=Peak.Frequency..Hz.)) +
  labs(title = "Sb_20160522_154000_sel035_enc02A26_BrazilPMCBS")

TidyB_6_35 <- B_6_35 %>% select(time, Peak.Frequency..Hz.) %>%
  mutate(whistle = rep("6_35", length.out = nrow(B_6_35))) %>%
  rename(frequency = Peak.Frequency..Hz.)

join_6_2 <- full_join(TidyB_6_35, join_6_1, by = colnames(join_6_1))
View(join_6_2)

ggplot(data=join_6_2,
       aes(x=time, y=frequency, colour=whistle)) +
  labs(title = "Brazil_category_6", x = "Time (s)", y = "Frequency (Hz)") +
  geom_line()



