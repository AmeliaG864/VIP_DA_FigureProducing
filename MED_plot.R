############## Code for plotting reference contours (with >1 whistle) from the Mediterranean_107 data set ###############
####### Code includes loop code made by Emily, Rosie, Tom and Tristan

setwd("/Users/ameliagoetz/Desktop/VIP/Steno/MED")

library(readr)
library(ggplot2)
library(tidyverse)
library("ggpubr")

######### Read in the .csv of the ARTwarp reference contour outputs with NO single whistles

MED_refARTwarp_NOSINGwhistles <- read_csv("MED_refARTwarp_NOSINGwhistles.csv")
View(MED_refARTwarp_NOSINGwhistles)

### Re-name to 'refM' 

refM <- read.csv("MED_refARTwarp_NOSINGwhistles.csv", header=T, sep="," )

### Checking refM to ensure df has loaded correctly

str(refM)
head(refM)
tail(refM)
dim(refM)

############# Code here adapted and taken from Emily, Rosie, Tom and Tristan's code for creating the loop
############# Includes code for adding time variable, checking df after adding the time variable, and creating loop

####### Adding a time variable to refH ########
### To do this multiply number of rows by 0.01 and minus 0.01
### 249*0.01-0.01 = 2.48

refM$time <- c(seq(from = 0, to = 2.48, by = 0.01))

### Check that the time variable has been added correctly 

str(refM)
head(refM)
tail(refM)

names(refM)

######## LOOP CODE ########

### final code used for creating a loop and saving each reference contour as a jpeg
### collated by rosie, emily, tom and tristan

##### NOTE: The loop code is NOT necessary for creating the final reference contour figure
##### It is nice however as saves each reference contour as a jpeg, allows you to check that they look ok
##### The loop can be skipped though, if don't want to run each time

for (n in names(refM)){
  ytemp=refM[[n]]
  filename=n
  timen=c(seq(from=1,to=length(ytemp), by=1))
  plotM <- ggplot() +
    geom_line(data = refM , aes(x = timen, y = ytemp, colour="a1"), linetype = 1, size = 2) +
    xlab("Time(s)")+
    ylab("Frequency(Hz)")+
    scale_colour_manual(values=c(a1="#0000FF")) + 
    xlim(0,8.5) + 
    ylim(0,20000) +
    theme_bw() + 
    theme( panel.border = element_blank(),  
           panel.background = element_blank(),   
           axis.line.x = element_line(size = 0.5, colour = "black"), 
           axis.line.y = element_line(size = 0.5, colour = "black"),    
           axis.text=element_text(size=20),    
           axis.title.y=element_text(vjust=0, size=20, margin=margin(0,10,0,0)), 
           axis.title.x=element_text(vjust=0, size=20, margin=margin(10,0,0,0)),
           legend.position="none") 
  ggsave(sprintf("%s.jpeg", filename), plot = plot, dpi = 400)
  print(plotM)
  dev.off()
}

####### END of code adapted form Emily, Rosie, Tom and Tristan

###'Tidying' refM df to create a new df with columns 'time', 'category' and 'frequency' 
### refM currently has data where each column = frequency outputs from one reference category
### By altering df allows easier manipulation to produce figure

View(refM)

tidyrefM <- refM %>%
  pivot_longer(!time, names_to = "category", values_to = "frequency") %>%
  filter(is.nan(frequency) == FALSE)

### Checking that the df has been 'tidied' correctly

View(tidyrefM)

########## Creating the reference contour figure #############

### The x and y axis have been standardised with the Brazil and Hawaii 107 figures
### Figure facet-wrapped by category so each box of figure represents one reference category 

ggplot(tidyrefM) +
  geom_line(aes(x=time, y=frequency, colour = category)) +
  labs(title = "Mediterranean_107", x = "Time (s)", y = "Frequency (Hz)") +
  xlim(0, 2.5) +
  ylim(3000, 15900) +
  facet_wrap(~category )

### Non-standardised axis figure
### Very similar to above figure as Med data had largest maximum x and y variables

ggplot(tidyrefM) +
  geom_line(aes(x=time, y=frequency, colour = category)) +
  labs(title = "Mediterranean_107", x = "Time (s)", y = "Frequency (Hz)") +
  facet_wrap(~category )

