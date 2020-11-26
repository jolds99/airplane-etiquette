## Load Packages
library(tidyverse)
library(ggplot2)
library(knitr)
detach(package:plyr)
detach(package:dplyr)
library(plyr)
library(dplyr)
## Load Data
airplane_etiquette = read_csv("airplane-etiquette.csv")

    # Creating subset of data with complete cases
    travelfreq = airplane_etiquette[which(!is.na(`How often do you travel by plane?`) &
                                            !is.na(`Is itrude to move to an unsold seat on a plane?`) &
                                            !is.na(`Generally speaking, is it rude to say more than a few words tothe stranger sitting next to you on a plane?`) & 
                                            !is.na(`Is itrude to recline your seat on a plane?`) & 
                                            !is.na(`Is it rude to ask someone to switch seats with you in order to be closer to friends?`) &
                                            !is.na(`Is itrude to ask someone to switch seats with you in order to be closer to family?`) & 
                                            !is.na(`Is it rude to wake a passenger up if you are trying to go to the bathroom?`) & 
                                            !is.na(`Is itrude to wake a passenger up if you are trying to walk around?`) & 
                                            !is.na(`In general, is itrude to bring a baby on a plane?`) & 
                                            !is.na(`In general, is it rude to knowingly bring unruly children on a plane?`)),]
    # Selecting necessary columns
    travelfreq = travelfreq %>% select(c(`How often do you travel by plane?`, `Is itrude to move to an unsold seat on a plane?`,
                                         `Generally speaking, is it rude to say more than a few words tothe stranger sitting next to you on a plane?`,
                                         `Is itrude to recline your seat on a plane?`, `Is it rude to ask someone to switch seats with you in order to be closer to friends?`,
                                         `Is itrude to ask someone to switch seats with you in order to be closer to family?`,
                                         `Is it rude to wake a passenger up if you are trying to go to the bathroom?`,
                                         `Is itrude to wake a passenger up if you are trying to walk around?`,
                                         `In general, is itrude to bring a baby on a plane?`,
                                         `In general, is it rude to knowingly bring unruly children on a plane?`))
    
    # Changing column names for simplicity
    colnames(travelfreq) = c("Travel_Frequency", "Unsold_Seat", "Talk_Stranger", "Recline_Seat",
                             "Switch_Friends", "Switch_Family", "Wake_Bathroom", "Wake_Walk",
                             "Bring_Baby", "Bring_Children")
    
    # Grouping frequency of travel into two categories
    travelfreq$Travel_Frequency = ifelse(travelfreq$Travel_Frequency == "Once a year or less", "No more than once a year", "More than once a year")
    
    # Change Rude grouping function
    rude_group_function = function(columnname){
      travelfreq[[columnname]] = ifelse((travelfreq[[columnname]] == "No, not rude at all" | travelfreq[[columnname]] == "No, not at all rude"), "No", "Yes")
    }
     

    travelfreq[,2:10] <- lapply(colnames(travelfreq[,2:10]), rude_group_function)
    
    
    travelfreq = travelfreq %>% rowwise %>% mutate(Rude_Count = sum(Unsold_Seat == "Yes",  
                                                           Talk_Stranger == "Yes",
                                                           Recline_Seat == "Yes",
                                                           Switch_Friends == "Yes",
                                                           Switch_Family == "Yes",
                                                           Wake_Bathroom == "Yes",
                                                           Wake_Walk == "Yes",
                                                           Bring_Baby == "Yes",
                                                           Bring_Children == "Yes"))
  
    mu <- ddply(travelfreq, "Travel_Frequency", summarise, grp.mean=mean(Rude_Count))
    ggplot(travelfreq, aes(x = Rude_Count, color = Travel_Frequency)) + geom_density() + 
      geom_vline(data=mu, aes(xintercept=grp.mean, color = Travel_Frequency),
                 linetype="dashed") + 
      scale_x_continuous(breaks = seq(0,10,2))
    
    travelfreq = travelfreq %>% group_by(Travel_Frequency) %>% mutate(mean = mean(Rude_Count))

    t.test(travelfreq[which(travelfreq$Travel_Frequency == "No more than once a year"),11],
           travelfreq[which(travelfreq$Travel_Frequency == "More than once a year"), 11],
           alternative = "two.sided", var.equal = FALSE)
    
    travelfreq %>% group_by(Travel_Frequency) %>%  
    