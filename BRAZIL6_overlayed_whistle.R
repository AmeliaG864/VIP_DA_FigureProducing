######## An overlay plot using whistles used to generarate refernce category 6 (X6) from the Brazil_107 data set
### code very similar to the code for "BRAZIL1_overlayed_whistle_test"


######## to ensure this code works, first run code from BRAZIL_plot.R as the below code requires the df 'tidyrefB' to run
### BRAZIL_plot.R can be found in my github: AmeliaG864 > VIP_DA_FigureProducing > BRAZIL_plot.R 
### For a slightly more detailed overview of code, see BRAZIL1_overlayed_whistle_test which can also be found in my github: AmeliaG864 > VIP_DA_FigureProducing > BRAZIL1_overlayed_whistle_test 

setwd("/Users/ameliagoetz/Desktop/VIP/Steno/Brazil")

library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)


####### 1. Plot reference category 6 utilising 'tidyrefB' from BRAZIL_plot.R


### Isolate reference category 6 from all cateogries in tidyrefB
Brazil6 <- tidyrefB %>%
  filter(category == "X6")

View(Brazil6)

### rename column from 'cateogry' to 'whistle'
Brazil6a <- Brazil6 %>%
  rename(whistle = category)

View(Brazil6a)

### Plot the reference contour from category 6 
ggplot(Brazil6a) +
  geom_line(aes(x=time, y=frequency)) +
  labs(title = "Brazil ref cat 6")


####### 2. Plotting whistle 1 

### Import whistle data
Sb_20160522_154000_sel027_enc02A26_BrazilPMCBS <- read_csv("Sb_20160522_154000_sel027_enc02A26_BrazilPMCBS.csv")
View(Sb_20160522_154000_sel027_enc02A26_BrazilPMCBS)

B_6_27 <- read.csv("Sb_20160522_154000_sel027_enc02A26_BrazilPMCBS.csv", header=T, sep="," )


##### Adding a time variable
### temp res = 3 
### temp res changes by whistle depending on folder it came from in OneDrive (see handover doc)
### multiply number of columns - 1 by 0.003
### (54 -1) * 0.003 = 0.159

B_6_27$time <- c(seq(from = 0, to = 0.159, by = 0.003))
head(B_6_27)

### Plotting B_6_27 to check it looks as expected

ggplot(B_6_27) +
  geom_line(aes(x=time, y=Peak.Frequency..Hz.)) +
  labs(title = "Sb_20160522_154000_sel027_enc02A26_BrazilPMCBS")

### Tidying 'B_6_27' df so it has only three columns: freq, time and whistle, where whistle = 6_27 (as came from sel27, category 6)

TidyB_6_27 <- B_6_27 %>% select(time, Peak.Frequency..Hz.) %>%
  mutate(whistle = rep("6_27", length.out = nrow(B_6_27))) %>%
  rename(frequency = Peak.Frequency..Hz.)

### Plotting the tidied B_6_27 to check it looks as expected

ggplot(data=TidyB_6_27,
       aes(x=time, y=frequency, colour=whistle)) +
  geom_line()

### Joining the reference contour df (Brazil6a) with the df for the first whistle in ref. category 6 (TidyB_6_27)

join_6_1 <- full_join(Brazil6a, TidyB_6_27, by = colnames(Brazil6a))
View(join_6_1)

### Creating the plot with both the refernce contourr (X6) and whistle B6_27

ggplot(data=join_6_1,
       aes(x=time, y=frequency, colour=whistle)) +
  geom_line()


############ Adding the second whistle to the plot

## Import the data
Sb_20160522_154000_sel035_enc02A26_BrazilPMCBS <- read_csv("Sb_20160522_154000_sel035_enc02A26_BrazilPMCBS.csv")
View(Sb_20160522_154000_sel035_enc02A26_BrazilPMCBS)

B_6_35 <- read.csv("Sb_20160522_154000_sel035_enc02A26_BrazilPMCBS.csv", header=T, sep="," )

##### Adding a time variable
### temp res = 3 
### (33 - 1) * 0.003 = 0.096
B_6_35$time <- c(seq(from = 0, to = 0.096, by = 0.003))
head(B_6_35)

### Plotting B_6_35 to check it looks as expected
ggplot(B_6_35) +
  geom_line(aes(x=time, y=Peak.Frequency..Hz.)) +
  labs(title = "Sb_20160522_154000_sel035_enc02A26_BrazilPMCBS")

### Tidying 'B_6_35' df so it has only three columns: freq, time and whistle, where whistle = 6_27 (as came from sel35, category 6)

TidyB_6_35 <- B_6_35 %>% select(time, Peak.Frequency..Hz.) %>%
  mutate(whistle = rep("6_35", length.out = nrow(B_6_35))) %>%
  rename(frequency = Peak.Frequency..Hz.)

### Creating a new df with all the whistles (2) and ref contour (X6) so far (columns: whistle, frequency, time)

join_6_2 <- full_join(TidyB_6_35, join_6_1, by = colnames(join_6_1))
View(join_6_2)

### Creating a final plot with all whistles (B_6_27 and B_6_35) and the reference contour (X6)

ggplot(data=join_6_2,
       aes(x=time, y=frequency, colour=whistle)) +
  labs(title = "Brazil_category_6", x = "Time (s)", y = "Frequency (Hz)") +
  geom_line()



