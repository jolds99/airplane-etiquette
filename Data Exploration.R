attach(airplane_etiquette)
library(tidyverse)
library(ggplot2)
#### Obnoxious kids question ####

#### Reclining vs height ####
nrow(airplane_etiquette)-
  length(which(is.na(`Do you ever recline your seat when you fly?`) | is.na(`How tall are you?`)))

recline <- airplane_etiquette[-which(is.na(`Do you ever recline your seat when you fly?`) | is.na(`How tall are you?`)),]
colnames(recline)[c(3,4)] <- c("recline", "height")
length(which(recline$height=="6\'6\" and above"))
length(which(recline$height=="Under 5 ft."))

recline <- recline %>%
  mutate(height = if_else(height == "6\'6\" and above","6\'6\"", height)) %>%
  mutate(height = if_else(height == "Under 5 ft.","5\'0\"", height))

recline$inches = sapply(strsplit(as.character(recline$height),"'|\""),
                         function(x){12*as.numeric(x[1]) + as.numeric(x[2])})
ggplot(data = recline, aes(x=inches, y =..density..))+geom_histogram(bins=18)+
  labs(x= "Height (in)", y= "Density", title = "Distriburion of Heights of Respondents")+
  theme_minimal()
  
recline$recline <- factor(recline$recline, levels=c("Never", "Once in a while", "About half the time", "Usually", "Always"))
ggplot(data = recline, aes(x=recline))+ geom_bar()+
  labs(x= "Repsonse to \'Do you ever recline your seat when you fly?\'", y= "Density", title = "Distriburion of Reclining Responses")+
  theme_minimal()



#### Armrest ####
nrow(airplane_etiquette)-
  length(which(is.na(`In a row of three seats, who should get to use the two arm rests?`) 
               | is.na(`In a row of two seats, who should get to use the middle arm rest?`)
               | is.na(`Gender`)
               | is.na(`Age`)))
armrest <- airplane_etiquette[-which(is.na(`In a row of three seats, who should get to use the two arm rests?`) 
                                     | is.na(`In a row of two seats, who should get to use the middle arm rest?`)
                                     | is.na(`Gender`)
                                     | is.na(`Age`)),]
