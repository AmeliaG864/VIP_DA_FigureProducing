
####### Code for plotting reference contours (with >1 whistle) from the Brazil_107 data set #########
####### Code includes loop code made by Emily, Rosie, Tom and Tristan

setwd("/Users/ameliagoetz/Desktop/VIP/Steno/Brazil")

library(readr)
library(ggplot2)
library(tidyverse)
library("ggpubr")

######### Read in the .csv of the ARTwarp reference contour outputs with NO single whistles

BRAZIL_refARTwarp_NOSINGwhistles <- read_csv("BRAZIL_refARTwarp_NOSINGwhistles.csv")
View(BRAZIL_refARTwarp_NOSINGwhistles)

### Re-name to 'refB' 

refB <- read.csv("BRAZIL_refARTwarp_NOSINGwhistles.csv", header=T, sep="," )

### Checking refB to ensure df has loaded correctly 

str(refB)
head(refB)
tail(refB)
dim(refB)

############# Code here adapted and taken from Emily, Rosie, Tom and Tristan's code for creating the loop
############# Includes code for adding time variable, checking df after adding the time variable, and creating loop

### Adding a time variable to refB
### To do this multiply number of rows by 0.001 and minus 0.01
### 116*0.01-0.01 = 1.15

refB$time <- c(seq(from = 0, to = 1.15, by = 0.01))

### Check that the time variable has been added correctly 

str(refB)
head(refB)
tail(refB)
names(refB)

######## LOOP CODE ########

### final code used for creating a loop and saving each reference contour as a jpeg
### collated by rosie, emily, tom and tristan

##### NOTE: The loop code is not necessary for creating the final reference contour figure
##### It is nice however as saves each reference contour as a plot, allows you to check that they look ok
##### The loop can be skipped though, if don't want to run each time 

for (n in names(refB)){
  ytemp=refB[[n]]
  filename=n
  timen=c(seq(from=1,to=length(ytemp), by=1))
  plotB <-ggplot() +
    geom_line(data = refB , aes(x = timen, y = ytemp, colour="a1"), linetype = 1, size = 2) +
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
  print(plotB)
  dev.off()
}

####### END of code adapted form Emily, Rosie, Tom and Tristan

###'Tidying' refB df to create a new df with columns 'time', 'category' and 'frequency' 
### refB currently has code where each column = frequency outputs from one reference category
### By altering df allows easier manipulation to produce figure

View(refB)

tidyrefB <- refB %>%
  pivot_longer(!time, names_to = "category", values_to = "frequency") %>%
  filter(is.nan(frequency) == FALSE)

### Checking that the df has been 'tidied' correctly

View(tidyrefB)

########## Creating the reference contour figure #############

### The x and y axis have been standardised with the Med and Hawaii 107 data/ figures for easier visual comparison between locations
### Figure facet-wrapped by category so each box of figure represents one reference category 

ggplot(tidyrefB) +
  geom_line(aes(x=time, y=frequency, colour = category)) +
  labs(title = "Brazil_107", x = "Time (s)", y = "Frequency (Hz)") + 
  xlim(0, 2.5) +
  ylim(3000, 15900) +
  facet_wrap(~category )

### Plot with non-standardised axis
### Especially helpful for Brazil data, which had the smallest maximum x values
### Standardised plot can therefore make smaller reference categories very hard to see

ggplot(tidyrefB) +
  geom_line(aes(x=time, y=frequency, colour = category)) +
  labs(title = "Brazil_107", x = "Time (s)", y = "Frequency (Hz)") + 
  facet_wrap(~category )

