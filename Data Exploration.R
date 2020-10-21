attach(airplane_etiquette)
library(tidyverse)
library(ggplot2)
library(knitr)
#### Tolerance of Babies and Children ####
airplane_etiquette = read_csv("airplane-etiquette.csv")

# Creating subset of data with complete cases for the three questions
children = airplane_etiquette[which(!is.na(`Do you have any children under 18?`) &
                                   !is.na(`In general, is itrude to bring a baby on a plane?`) &
                                   !is.na(`In general, is it rude to knowingly bring unruly children on a plane?`)),]

# Creating table summarizing counts and proportion of respondents who own children
childrentable = children %>% group_by(`Do you have any children under 18?`) %>% 
  summarise(Count = n()) %>% 
  mutate(Percent = round(Count/sum(Count),3))

kable(childrentable, format = "latex")

# Distribution of responses to baby question
ggplot(children, aes(x = `In general, is itrude to bring a baby on a plane?`)) + 
  geom_bar(stat="count", color = "black", fill = "tan") +
  ggtitle("Distribution of Responses to Baby Question") + 
  xlab("Responses to \'In general, is it rude to bring a baby on a plane?\'") + 
  ylab("Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))

# Distribution of responses to children question
ggplot(children, aes(x = `In general, is it rude to knowingly bring unruly children on a plane?`)) + 
  geom_bar(stat="count", color = "black", fill = "tan") +
  ggtitle("Distribution of Responses to Unruly Children Question") + 
  xlab("Responses to \'In general, is it rude to knowingly bring unruly children on a plane?\'") + 
  ylab("Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))

babyunruly = children %>% select(RespondentID, `Do you have any children under 18?`, `In general, is itrude to bring a baby on a plane?`,
                                   `In general, is it rude to knowingly bring unruly children on a plane?`) %>%
                          gather("Question", "Response", 3:4)

babyunruly$Question = factor(babyunruly$Question, levels = c("In general, is itrude to bring a baby on a plane?",
                                                                  "In general, is it rude to knowingly bring unruly children on a plane?"))
ggplot(babyunruly, aes(x = Question, fill = factor(Response))) + 
         geom_bar(stat = "count", position = position_dodge()) + 
         ggtitle("Distribution of Baby & Unruly Children Questions") + 
         labs(fill = "Response") + 
         ylab("Frequency") + 
         scale_x_discrete(labels = c("In general, is it rude to bring a baby on a plane?", 
                                     "In general, is it rude to knowingly bring unruly children on a plane?")) + 
         scale_fill_manual(values = c("Green4", "Gold2", "Red3")) + 
         theme(plot.title = element_text(hjust = 0.5))


#### Reclining vs Height ####
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

ggplot(data = recline, aes(x=inches))+geom_histogram(bins=19, color = "black", fill = "#0073C2FF")+
  labs(x= "Height (in)", y= "Frequency", title = "Distribution of Heights of Respondents")+
  theme_minimal()
  
recline$recline <- factor(recline$recline, levels=c("Never", "Once in a while", "About half the time", "Usually", "Always"))
ggplot(data = recline, aes(x=recline))+ geom_bar(color = "black", fill = "#0073C2FF")+
  labs(x= "Repsonse to \'Do you ever recline your seat when you fly?\'", y= "Frequency", title = "Distribution of Reclining Responses")+
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
