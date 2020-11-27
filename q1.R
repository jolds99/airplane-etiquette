## Load Packages
library(tidyverse)
library(ggplot2)
library(knitr)

## Load Data
    airplane_etiquette = read_csv("airplane-etiquette.csv")
    
    # Creating subset of data with complete cases for the three questions
    children = airplane_etiquette[which(!is.na(`Do you have any children under 18?`) &
                                          !is.na(`In general, is itrude to bring a baby on a plane?`) &
                                          !is.na(`In general, is it rude to knowingly bring unruly children on a plane?`)),]

## Exploratory Data Analysis

    # Creating table summarizing counts and proportion of respondents who own children
    childrentable = children %>% group_by(`Do you have any children under 18?`) %>% 
      summarise(Count = n()) %>% 
      mutate(Percent = round(Count/sum(Count),3))
    
    kable(childrentable)
    
    colnames(children)[c(5,19:20)] = c("Have_Children", "Baby", "Unruly_Children")
    
    children$Baby = ifelse(children$Baby == "No, not at all rude", "No", "Yes")
    children$Unruly_Children = ifelse(children$Unruly_Children == "No, not at all rude", "No", "Yes")
    
    # Distribution of responses to baby question
    ggplot(children, aes(x = Baby)) + 
      geom_bar(stat="count", color = "black", fill = "tan") +
      ggtitle("Distribution of Responses to Baby Question") + 
      xlab("Responses to \'In general, is it rude to bring a baby on a plane?\'") + 
      ylab("Frequency") + 
      theme(plot.title = element_text(hjust = 0.5))
    
    # Distribution of responses to children question
    ggplot(children, aes(x = Unruly_Children)) + 
      geom_bar(stat="count", color = "black", fill = "tan") +
      ggtitle("Distribution of Responses to Unruly Children Question") + 
      xlab("Responses to \'In general, is it rude to knowingly bring unruly children on a plane?\'") + 
      ylab("Frequency") + 
      theme(plot.title = element_text(hjust = 0.5))
    
    
    babyunruly = children %>% select(RespondentID, Have_Children, Baby, Unruly_Children) %>%
      gather("Question", "Response", 3:4)
    
    babyunruly$Question = factor(babyunruly$Question, levels = c("Baby", "Unruly_Children"))
    
    ## Distribution of both questions together
    ggplot(babyunruly, aes(x = Question, fill = factor(Response))) + 
      geom_bar(stat = "count", position = position_dodge()) + 
      ggtitle("Distribution of Baby & Unruly Children Questions") + 
      labs(fill = "Response") + 
      ylab("Frequency") + 
      scale_x_discrete(labels = c("In general, is it rude to bring a baby on a plane?", 
                                  "In general, is it rude to knowingly bring unruly children on a plane?")) + 
      scale_fill_manual(values = c("Green4", "Gold2", "Red3")) + 
      theme(plot.title = element_text(hjust = 0.5))
    
    
    babyunruly = babyunruly %>% spread(Question, Response)
    
## Babies
    ## Determining sample proportions
    phat_babies_child = sum(babyunruly$Have_Children == "Yes" &
                        babyunruly$Baby == "Yes")/sum(babyunruly$Have_Children == "Yes")

    phat_babies_nochild = sum(babyunruly$Have_Children == "No" &
                               babyunruly$Baby == "Yes")/sum(babyunruly$Have_Children == "No")
    ## Confidence intervals
    n_babies_child = sum(babyunruly$Have_Children == "Yes")
    varhat_phat_babies_child = (phat_babies_child * (1-phat_babies_child))/(n_babies_child-1)
    zscore = qnorm(1-(0.05/2))
    moe_babies_child = zscore * sqrt(varhat_phat_babies_child)
    ci_lower_babies_child = phat_babies_child - moe_babies_child
    ci_upper_babies_child = phat_babies_child + moe_babies_child
    ci_babies_child = cbind(ci_lower_babies_child, ci_upper_babies_child)
    ci_babies_child
    
    prop.test(x = sum(babyunruly$Have_Children == "Yes" & babyunruly$Baby == "Yes"), 
              n = sum(babyunruly$Have_Children == "Yes"), conf.level = .95, correct = FALSE)
    
    n_babies_nochild = sum(babyunruly$Have_Children == "No")
    varhat_phat_babies_nochild = (phat_babies_nochild * (1-phat_babies_nochild))/(n_babies_nochild-1)
    zscore = qnorm(1-(0.05/2))
    moe_babies_nochild = zscore * sqrt(varhat_phat_babies_nochild)
    ci_lower_babies_nochild = phat_babies_nochild - moe_babies_nochild
    ci_upper_babies_nochild = phat_babies_nochild + moe_babies_nochild
    ci_babies_nochild = cbind(ci_lower_babies_nochild, ci_upper_babies_nochild)
    ci_babies_nochild
    
    prop.test(x = sum(babyunruly$Have_Children == "No" & babyunruly$Baby == "Yes"), 
              n = sum(babyunruly$Have_Children == "No"), conf.level = .95, correct = FALSE)
    
    babies_df = as.data.frame(as.matrix(NA, nrow = 2, ncol = 4))
    babies_df[1,1:4] = c("Children", 0.159, 0.107, 0.212)
    babies_df[2,1:4] = c("No Children", 0.342, 0.306, 0.379)
    colnames(babies_df) = c("Own Children?", "P Estimate", "CI Lower Bound", "CI Upper Bound")
    babies_df[,2:4] = lapply(babies_df[,2:4], as.numeric)
    
    
    ggplot(babies_df, aes(x =`P Estimate`, y = `Own Children?`, color = `Own Children?`)) + 
      geom_point() + 
      scale_x_continuous(name = "Proportion of Airplane Travellers that Perceive Bringing a Baby on a Plane as Rude ", limits = c(0,0.5)) + 
      scale_color_manual(values = c("red", "blue")) + 
      geom_segment(aes(x = babies_df$`CI Lower Bound`[1], y = "Children", 
                       xend = babies_df$`CI Upper Bound`[1], yend = "Children"), color = "red") + 
      geom_segment(aes(x = babies_df$`CI Lower Bound`[2], y = "No Children", 
                       xend = babies_df$`CI Upper Bound`[2], yend = "No Children"), color = "blue") + 
      ggtitle("Point Estimates of Proportion that Perceive Bringing a Baby on a Plane as Rude with 95% CI") + 
      theme(plot.title = element_text(hjust = 0.5))
    
    # Test of Proportions
    babies_test <- prop.test(x = c(sum(babyunruly$Have_Children == "Yes" & babyunruly$Baby == "Yes"),
                                   sum(babyunruly$Have_Children == "No" & babyunruly$Baby == "Yes")),
                             n = c(sum(babyunruly$Have_Children == "Yes"), sum(babyunruly$Have_Children == "No")), correct = FALSE)
    
    babies_test
    
## Children
    ## Determining sample proportions
    phat_unruly_child = sum(babyunruly$Have_Children == "Yes" &
                              babyunruly$Unruly_Children == "Yes")/sum(babyunruly$Have_Children == "Yes")
    
    phat_unruly_nochild = sum(babyunruly$Have_Children == "No" &
                                babyunruly$Unruly_Children == "Yes")/sum(babyunruly$Have_Children == "No")
    
    
    ## Confidence intervals
    n_unruly_child = sum(babyunruly$Have_Children == "Yes")
    varhat_phat_unruly_child = (phat_unruly_child * (1-phat_unruly_child))/(n_unruly_child-1)
    zscore = qnorm(1-(0.05/2))
    moe_unruly_child = zscore * sqrt(varhat_phat_unruly_child)
    ci_lower_unruly_child = phat_unruly_child - moe_unruly_child
    ci_upper_unruly_child = phat_unruly_child + moe_unruly_child
    ci_unruly_child = cbind(ci_lower_unruly_child, ci_upper_unruly_child)
    ci_unruly_child
    
    prop.test(x = sum(babyunruly$Have_Children == "Yes" & babyunruly$Unruly_Children == "Yes"), 
              n = sum(babyunruly$Have_Children == "Yes"), conf.level = .95, correct = FALSE)
    
    n_unruly_nochild = sum(babyunruly$Have_Children == "No")
    varhat_phat_unruly_nochild = (phat_unruly_nochild * (1-phat_unruly_nochild))/(n_unruly_nochild-1)
    zscore = qnorm(1-(0.05/2))
    moe_unruly_nochild = zscore * sqrt(varhat_phat_unruly_nochild)
    ci_lower_unruly_nochild = phat_unruly_nochild - moe_unruly_nochild
    ci_upper_unruly_nochild = phat_unruly_nochild + moe_unruly_nochild
    ci_unruly_nochild = cbind(ci_lower_unruly_nochild, ci_upper_unruly_nochild)
    ci_unruly_nochild
    
    prop.test(x = sum(babyunruly$Have_Children == "No" & babyunruly$Unruly_Children == "Yes"), 
              n = sum(babyunruly$Have_Children == "No"), conf.level = .95, correct = FALSE)
    
    unruly_df = as.data.frame(as.matrix(NA, nrow = 2, ncol = 4))
    unruly_df[1,1:4] = c("Children", 0.734, 0.671, 0.797)
    unruly_df[2,1:4] = c("No Children", 0.854, 0.827, 0.880)
    colnames(unruly_df) = c("Own Children?", "P Estimate", "CI Lower Bound", "CI Upper Bound")
    unruly_df[,2:4] = lapply(unruly_df[,2:4], as.numeric)
    
    
    ggplot(unruly_df, aes(x =`P Estimate`, y = `Own Children?`, color = `Own Children?`)) + 
      geom_point() + 
      scale_x_continuous(name = "Proportion of Airplane Travellers that Perceive Bringing Unruly Children on a Plane as Rude ", limits = c(0.5,1)) + 
      scale_color_manual(values = c("red", "blue")) + 
      geom_segment(aes(x = unruly_df$`CI Lower Bound`[1], y = "Children", 
                       xend = unruly_df$`CI Upper Bound`[1], yend = "Children"), color = "red") + 
      geom_segment(aes(x = unruly_df$`CI Lower Bound`[2], y = "No Children", 
                       xend = unruly_df$`CI Upper Bound`[2], yend = "No Children"), color = "blue") + 
      ggtitle("Point Estimates of Proportion that Perceive Bringing a Baby on a Plane as Rude with 95% CI") + 
      theme(plot.title = element_text(hjust = 0.5))
    
    ## Test of Proportions
    unruly_test <- prop.test(x = c(sum(babyunruly$Have_Children == "Yes" & babyunruly$Unruly_Children == "Yes"),
                                   sum(babyunruly$Have_Children == "No" & babyunruly$Unruly_Children == "Yes")),
                             n = c(sum(babyunruly$Have_Children == "Yes"), sum(babyunruly$Have_Children == "No")), correct = FALSE)
    
    unruly_test
    