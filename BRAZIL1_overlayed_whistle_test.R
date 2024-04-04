############ Plotting all the whistles from a reference category on top of the reference whistle for that category #############
###### Here using whistles used to generate reference category 1 (X1) from the Brazil_107 data set

### to ensure this code works, first run code from BRAZIL_plot.R as the below code requires the df 'tidyrefB' to run
### BRAZIL_plot.R can be found in my github: AmeliaG864 > VIP_DA_FigureProducing > BRAZIL_plot.R 


setwd("/Users/ameliagoetz/Desktop/VIP/Steno/Brazil")

library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)



############## Tidying and plotting reference category 1 (X1) ###############

### Creating new df (Brazil1a) with a renamed column 'category' > 'whistle' with only values from category X1
# command requires 'tidyrefB' df; to load tidyrefb run code from 'BRAZIL_plot.R (see above) #

Brazil1a <- tidyrefB %>%
  filter(category == "X1") %>%
  rename(whistle = category)

head(Brazil1a)
tail(Brazil1a)

### Plotting the reference contour to ensure it looks as expected 

ggplot(Brazil1a) +
  geom_line(aes(x=time, y=frequency)) +
  labs(title = "Brazil ref cat 1")



############ WHISTLE 1 ; Sb_20160522_154000_sel015_enc02A26_BrazilPMCBS 

### Import data file 

Sb_20160522_154000_sel015_enc02A26_BrazilPMCBS_ <- read_csv("Sb_20160522_154000_sel015_enc02A26_BrazilPMCBS .csv")
View(Sb_20160522_154000_sel015_enc02A26_BrazilPMCBS_)


### Renaming to 'B15' (for Brazil sel015)

B15 <- read.csv("Sb_20160522_154000_sel015_enc02A26_BrazilPMCBS .csv", header=T, sep="," )
head(B15)


##### Adding a time variable
### temp res = 3 
### temp res changes by whistle depending on folder it came from in OneDrive (see handover doc)
### multiply number of columns - 1 by 0.003
### (38 -1) * 0.003 = 0.111

B15$time <- c(seq(from = 0, to = 0.111, by = 0.003))
head(B15)
names(B15)


### Plotting B15 to check it looks as expected

ggplot(B15) +
  geom_line(aes(x=time, y=Peak.Frequency..Hz.)) +
  labs(title = "Sb_20160522_154000_sel015_enc02A26_BrazilPMCBS")


### Tidying 'B15' df so it has only three columns: freq, time and whistle, where whistle = 15 (as came from sel015)

colnames(B15)

TidyB15 <- B15 %>% select(time, Peak.Frequency..Hz.)
head(TidyB15)

TidyB15a <- TidyB15 %>%
  mutate(whistle = rep("15", length.out = nrow(TidyB15))) %>%
  rename(frequency = Peak.Frequency..Hz.)

head(TidyB15a)
tail(TidyB15a)


### Joining the reference contour df (Brazil1a) with the df for the first whistle in ref. category 1 (TidyB15a)

join2 <- full_join(Brazil1a, TidyB15a, by = colnames(Brazil1a))
head(join2)
tail(join2)


### Plotting both the reference contour (X1) and the first whistle that went to create that contour (B15) on the same plot

ggplot(data=join2,
       aes(x=time, y=frequency, colour=whistle)) +
  geom_line()




##################### From here on repeating same steps of : ##########################
#### 1. Import whistle
#### 2. Adjust temp res
#### 3. Check plot
#### 4. Tidy df
#### 5. Join df with the one from the whistle before it 
#### 6. Plot all whistles (and ref. contour) on same plot




####### WHISTLE 2; Sb_20160522_155000_sel012_enc02A26_BrazilPMCBS

### Import data file

Sb_20160522_155000_sel012_enc02A26_BrazilPMCBS <- read_csv("Sb_20160522_155000_sel012_enc02A26_BrazilPMCBS.csv")
View(Sb_20160522_155000_sel012_enc02A26_BrazilPMCBS)


### Renaming to B12

B12 <- read.csv("Sb_20160522_155000_sel012_enc02A26_BrazilPMCBS.csv", header=T, sep="," )
head(B12)


##### Adding a time variable
### temp res = 3 
### (52 - 1) * 0.003 = 0.153

B12$time <- c(seq(from = 0, to = 0.153, by = 0.003))
head(B12)
names(B12)


### Plotting B12 to check it looks as expected

ggplot(B12) +
  geom_line(aes(x=time, y=Peak.Frequency..Hz.)) +
  labs(title = "Sb_20160522_155000_sel012_enc02A26_BrazilPMCBS")


### Tidying B12 for the join

# Remove all columns other than time and Peak.Frequency..Hz
# In whistle column repeat 12 for all values
# Rename Peak.Frequency..Hz to frequency

colnames(B12)

TidyB12 <- B12 %>% select(time, Peak.Frequency..Hz.) %>%
  mutate(whistle = rep("12", length.out = nrow(B12))) %>%
  rename(frequency = Peak.Frequency..Hz.)
  
View(TidyB12)


### Joining the previous df (with reference contour and B15 whistle to current df with only B12 whistle)

join3 <- full_join(join2, TidyB12, by = colnames(Brazil1a))
head(join3)
tail(join3)

### Creating a new plot with all whistles so far (and reference contour)
ggplot(data=join3,
       aes(x=time, y=frequency, colour=whistle)) +
  geom_line()



####### WHISTLE 3 ###########


### Import data file 

Sb_20160522_155000_sel016_enc02A26_BrazilPMCBS <- read_csv("Sb_20160522_155000_sel016_enc02A26_BrazilPMCBS.csv")
View(Sb_20160522_155000_sel016_enc02A26_BrazilPMCBS)

B16 <- read.csv("Sb_20160522_155000_sel016_enc02A26_BrazilPMCBS.csv", header=T, sep="," )


##### Adding a time variable
### temp res = 3 
### (49 -1) * 0.003 = 0.144

B16$time <- c(seq(from = 0, to = 0.144, by = 0.003))
head(B16)


### Plotting B16 to check it looks as expected

ggplot(B16) +
  geom_line(aes(x=time, y=Peak.Frequency..Hz.)) +
  labs(title = "Sb_20160522_154000_sel016_enc02A26_BrazilPMCBS")


### An experiment with geom_smooth to see how the plot looks smoothed

ggplot(B16) +
  geom_smooth(aes(x=time, y=Peak.Frequency..Hz.)) +
  labs(title = "Sb_20160522_155000_sel016_enc02A26_BrazilPMCBS")


### Tidying B16 for the join

# Remove all columns other than time and Peak.Frequency..Hz
# In whistle column repeat 16 for all values
# Rename Peak.Frequency..Hz to frequency

TidyB16 <- B16 %>% select(time, Peak.Frequency..Hz.) %>%
  mutate(whistle = rep("16", length.out = nrow(B16))) %>%
  rename(frequency = Peak.Frequency..Hz.)


### Creating a df with all whistles and ref contour so far (columns: whistle, frequency, time)

join4 <- full_join(join3, TidyB16, by = colnames(Brazil1a))
head(join4)
tail(join4)

### Creating a new plot with all whistles so far (and reference contour)

ggplot(data=join4,
       aes(x=time, y=frequency, colour=whistle)) +
  geom_line()



###### WHISTLE 4 #########

### Import data file 
Sb_20160522_155000_sel027_enc02A26_BrazilPMCBS <- read_csv("Sb_20160522_155000_sel027_enc02A26_BrazilPMCBS.csv")
View(Sb_20160522_155000_sel027_enc02A26_BrazilPMCBS)

B27 <- read.csv("Sb_20160522_155000_sel027_enc02A26_BrazilPMCBS.csv", header=T, sep="," )


##### Adding a time variable
### temp res = 3 
### (45 -1) * 0.003 = 0.132

B27$time <- c(seq(from = 0, to = 0.132, by = 0.003))
head(B27)

### Plotting B27 to check it looks as expected

ggplot(B27) +
  geom_line(aes(x=time, y=Peak.Frequency..Hz.)) +
  labs(title = "Sb_20160522_154000_sel027_enc02A26_BrazilPMCBS")


### Tidying B27 for the join

TidyB27 <- B27 %>% select(time, Peak.Frequency..Hz.) %>%
  mutate(whistle = rep("27", length.out = nrow(B27))) %>%
  rename(frequency = Peak.Frequency..Hz.)

### Creating a df with all whistles and ref contour so far (columns: whistle, frequency, time)

join5 <- full_join(join4, TidyB27, by = colnames(Brazil1a))
head(join5)
tail(join5)

### Creating a new plot with all whistles so far (and reference contour)

ggplot(data=join5,
       aes(x=time, y=frequency, colour=whistle)) +
  geom_line()



############## WHISTLE 5 ##################


### Import data file 

Sb_20160522_161000_sel005_enc02A26_BrazilPMCBS <- read_csv("Sb_20160522_161000_sel005_enc02A26_BrazilPMCBS.csv")
View(Sb_20160522_161000_sel005_enc02A26_BrazilPMCBS)

B05 <- read.csv("Sb_20160522_161000_sel005_enc02A26_BrazilPMCBS.csv", header=T, sep="," )


##### Adding a time variable
### temp res = 5
### (65 -1) * 0.003 = 0.32

B05$time <- c(seq(from = 0, to = 0.32, by = 0.005))
head(B05)


### Plotting B05 to check it looks as expected

ggplot(B05) +
  geom_line(aes(x=time, y=Peak.Frequency..Hz.)) +
  labs(title = "Sb_20160522_161000_sel005_enc02A26_BrazilPMCBS")


### Tidying B05 for the join

TidyB05 <- B05 %>% select(time, Peak.Frequency..Hz.) %>%
  mutate(whistle = rep("05", length.out = nrow(B05))) %>%
  rename(frequency = Peak.Frequency..Hz.)


### Creating a df with all whistles and ref contour so far (columns: whistle, frequency, time)

join6 <- full_join(join5, TidyB05, by = colnames(Brazil1a))
head(join6)
tail(join6)

### Creating a new plot with all whistles so far (and reference contour)

ggplot(data=join6,
       aes(x=time, y=frequency, colour=whistle)) +
  geom_line()



################# WHISTLE 6 ###################

### Import data file 

Sb_20190403_171000_sel001_enc08A64_BrazilPMCBS <- read_csv("Sb_20190403_171000_sel001_enc08A64_BrazilPMCBS.csv")
View(Sb_20190403_171000_sel001_enc08A64_BrazilPMCBS)

B01 <- read.csv("Sb_20190403_171000_sel001_enc08A64_BrazilPMCBS.csv", header=T, sep="," )


##### Adding a time variable
### temp res = 3 
### (119 -1) * 0.003 = 0.354

B01$time <- c(seq(from = 0, to = 0.354, by = 0.003))
head(B01)

### Plotting B01 to check it looks as expected

ggplot(B01) +
  geom_line(aes(x=time, y=Peak.Frequency..Hz.)) +
  labs(title = "Sb_20190403_171000_sel001_enc08A64_BrazilPMCBS")

### Tidying B01 for the join

TidyB01 <- B01 %>% select(time, Peak.Frequency..Hz.) %>%
  mutate(whistle = rep("01", length.out = nrow(B01))) %>%
  rename(frequency = Peak.Frequency..Hz.)

### Creating a df with all whistles and ref contour so far (columns: whistle, frequency, time)
join7 <- full_join(join6, TidyB01, by = colnames(Brazil1a))
head(join7)

### Creating a new plot with all whistles so far (and reference contour)
ggplot(data=join7,
       aes(x=time, y=frequency, colour=whistle)) +
  geom_line()



############ WHISTLE 7 ############### 

### Import data file 

Sb_20190403_172000_sel003_enc08A64_BrazilPMCBS <- read_csv("Sb_20190403_172000_sel003_enc08A64_BrazilPMCBS.csv")
View(Sb_20190403_172000_sel003_enc08A64_BrazilPMCBS)

B03 <- read.csv("Sb_20190403_172000_sel003_enc08A64_BrazilPMCBS.csv", header=T, sep="," )

##### Adding a time variable
### temp res = 3
### (151 -1) * 0.003 = 0.453

B03$time <- c(seq(from = 0, to = 0.453, by = 0.003))
head(B03)

### Plotting B03 to check it looks as expected

ggplot(B03) +
  geom_line(aes(x=time, y=Peak.Frequency..Hz.)) +
  labs(title = "Sb_20190403_172000_sel003_enc08A64_BrazilPMCBS")

### Tidying B03 for the join

TidyB03 <- B03 %>% select(time, Peak.Frequency..Hz.) %>%
  mutate(whistle = rep("03", length.out = nrow(B03))) %>%
  rename(frequency = Peak.Frequency..Hz.)

### Creating a df with all whistles and ref contour so far (columns: whistle, frequency, time)

join8 <- full_join(join7, TidyB03, by = colnames(Brazil1a))
head(join8)
tail(join8)

### Creating a new plot with all whistles so far (and reference contour)

ggplot(data=join8,
       aes(x=time, y=frequency, colour=whistle)) +
  geom_line()



############## WHISTLE 8 #################

Sb_20190403_172000_sel012_enc08A64_BrazilPMCBS <- read_csv("Sb_20190403_172000_sel012_enc08A64_BrazilPMCBS.csv")
View(Sb_20190403_172000_sel012_enc08A64_BrazilPMCBS)

B12a <- read.csv("Sb_20190403_172000_sel012_enc08A64_BrazilPMCBS.csv", header=T, sep="," )

## adding time variable 
# temp res = 3
B12a$time <- c(seq(from = 0, to = 0.432, by = 0.003))
head(B12a)

ggplot(B12a) +
  geom_line(aes(x=time, y=Peak.Frequency..Hz.)) +
  labs(title = "Sb_20190403_172000_sel012_enc08A64_BrazilPMCBS")

TidyB12a <- B12a %>% select(time, Peak.Frequency..Hz.) %>%
  mutate(whistle = rep("12a", length.out = nrow(B12a))) %>%
  rename(frequency = Peak.Frequency..Hz.)

join9 <- full_join(join8, TidyB12a, by = colnames(Brazil1a))
head(join9)

ggplot(data=join9,
       aes(x=time, y=frequency, colour=whistle)) +
  geom_line()



############## WHISTLE 9 #################

Sb_20190403_172000_sel029_enc08A64_BrazilPMCBS <- read_csv("Sb_20190403_172000_sel029_enc08A64_BrazilPMCBS.csv")
View(Sb_20190403_172000_sel029_enc08A64_BrazilPMCBS)

B29 <- read.csv("Sb_20190403_172000_sel029_enc08A64_BrazilPMCBS.csv", header=T, sep="," )

## adding time variable 
# temp res = 3
B29$time <- c(seq(from = 0, to = 0.198, by = 0.003))
head(B29)

ggplot(B29) +
  geom_line(aes(x=time, y=Peak.Frequency..Hz.)) +
  labs(title = "Sb_20190403_172000_sel029_enc08A64_BrazilPMCBS")

TidyB29 <- B29 %>% select(time, Peak.Frequency..Hz.) %>%
  mutate(whistle = rep("29", length.out = nrow(B29))) %>%
  rename(frequency = Peak.Frequency..Hz.)

join10 <- full_join(join9, TidyB29, by = colnames(Brazil1a))
head(join10)

ggplot(data=join10,
       aes(x=time, y=frequency, colour=whistle)) +
  geom_line()



############### WHISTLE 10 ###################

Sb_20190403_173000_sel017_enc08A64_BrazilPMCBS <- read_csv("Sb_20190403_173000_sel017_enc08A64_BrazilPMCBS.csv")
View(Sb_20190403_173000_sel017_enc08A64_BrazilPMCBS)

B17 <- read.csv("Sb_20190403_173000_sel017_enc08A64_BrazilPMCBS.csv", header=T, sep="," )

## adding time variable 
# temp res = 5
B17$time <- c(seq(from = 0, to = 0.29, by = 0.005))
head(B17)

ggplot(B17) +
  geom_line(aes(x=time, y=Peak.Frequency..Hz.)) +
  labs(title = "Sb_20190403_173000_sel017_enc08A64_BrazilPMCBS")

TidyB17 <- B17 %>% select(time, Peak.Frequency..Hz.) %>%
  mutate(whistle = rep("17", length.out = nrow(B17))) %>%
  rename(frequency = Peak.Frequency..Hz.)

join11 <- full_join(join10, TidyB17, by = colnames(Brazil1a))
head(join11)

ggplot(data=join11,
       aes(x=time, y=frequency, colour=whistle)) +
  geom_line()



############# WHISTLE 11 ################

Sb_20190403_173000_sel039_enc08A64_BrazilPMCBS <- read_csv("Sb_20190403_173000_sel039_enc08A64_BrazilPMCBS.csv")
View(Sb_20190403_173000_sel039_enc08A64_BrazilPMCBS)

B39 <- read.csv("Sb_20190403_173000_sel039_enc08A64_BrazilPMCBS.csv", header=T, sep="," )

## adding time variable 
#temp res = 5

B39$time <- c(seq(from = 0, to = 0.16, by = 0.005))
head(B39)

ggplot(B39) +
  geom_line(aes(x=time, y=Peak.Frequency..Hz.)) +
  labs(title = "Sb_20190403_173000_sel039_enc08A64_BrazilPMCBS")

TidyB39 <- B39 %>% select(time, Peak.Frequency..Hz.) %>%
  mutate(whistle = rep("39", length.out = nrow(B39))) %>%
  rename(frequency = Peak.Frequency..Hz.)

join12 <- full_join(join11, TidyB39, by = colnames(Brazil1a))
View(join12)

ggplot(data=join12,
       aes(x=time, y=frequency, colour=whistle)) +
  geom_line()


########### WHISTLE 12 ##############

##Sb_sel_54_PMC-BS_8_20190403-173000_ch1_PMC_8_A64_Brazil
## ^ the above name is the original, R was not a fan as saved with a dash instead of underscore
## naming mistake??

Sb_sel_54_PMC_BS_8_20190403_173000_ch1_PMC_8_A64_Brazil <- read_csv("Sb_sel_54_PMC_BS_8_20190403_173000_ch1_PMC_8_A64_Brazil.csv")
View(Sb_sel_54_PMC_BS_8_20190403_173000_ch1_PMC_8_A64_Brazil)

B54 <- read.csv("Sb_sel_54_PMC_BS_8_20190403_173000_ch1_PMC_8_A64_Brazil.csv", header=T, sep="," )

## adding time variable 
# temp res = 5

B54$time <- c(seq(from = 0, to = 0.36, by = 0.005))
head(B54)

ggplot(B54) +
  geom_line(aes(x=time, y=Peak.Frequency..Hz.)) +
  labs(title = "Sb_sel_54_PMC_BS_8_20190403_173000_ch1_PMC_8_A64_Brazil")

TidyB54 <- B54 %>% select(time, Peak.Frequency..Hz.) %>%
  mutate(whistle = rep("54", length.out = nrow(B54))) %>%
  rename(frequency = Peak.Frequency..Hz.)

join13 <- full_join(join12, TidyB54, by = colnames(Brazil1a))
View(join13)



################# FINAL PLOT WITH ALL WHISTLES AND REFERENCE CONTOUR (X1) #######################

## colour scheme is not very clear


ggplot(data=join13,
       aes(x=time, y=frequency, colour=whistle)) +
  geom_line() 



## an attempt to better the colour scheme by making all whistles other than the ref. contour grey and ref contour red

ggplot(data=join13, aes(x=time, y=frequency, colour=whistle)) +
  geom_line() +
  scale_colour_manual(values = c("X1" = "red"))






