library(tidyverse)
library(questionr)
library(nnet)
library(ggplot2)
library(data.table)
library(gplots)
library(caret)
library(kableExtra)

#Getting data only necessary for question 4
q4 <- airplane_etiquette %>%
  mutate(seats3 = `In a row of three seats, who should get to use the two arm rests?`) %>%
  mutate(seats2 = `In a row of two seats, who should get to use the middle arm rest?`) %>%
  select(., seats3, seats2, Gender, Age, `Location (Census Region)`)
#Removing NAs
q4 <- na.rm(q4)

#Renaming "> 60" to "60+"
q4$Age <- ifelse(q4$Age == "> 60", "60+", q4$Age)

#Getting levels of responses
unique(q4$seats3)
unique(q4$seats2)
unique(q4$Gender)
unique(q4$Age)

#Plotting armrest responses
#2 seats
ggplot(data = q4, aes(x = seats2, fill = Age)) +
  geom_bar() +
  labs(title = "Who should have claim over the middle armrest in a row of two seats?",
       subtitle = "Separated by Gender", x = "Response", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(~ Gender) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))
  
#3 seats
ggplot(data = q4, aes(x = seats3, fill = Age)) +
  geom_bar() +
  labs(title = "Who should have two armrests in a row of three seats?",
       subtitle = "Separated by Gender", x = "Response", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(~ Gender) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))

#Initial multinomial regression test
#Rows with 2 seats
twoseat <- multinom(seats2 ~ Gender + Age, data = q4)
twoseatoutput <- summary(twoseat)
z2 <- twoseatoutput$coefficients/twoseatoutput$standard.errors
p2 <- (1-pnorm(abs(z2),0,1))*2 # I am using two-tailed z test
print(p2, digits =2)

maledf2 <- data.frame(Age = c("18-29", "30-44", "45-60", "60+" ), Gender = "Male")
malepred2 <- predict(twoseat, newdata = maledf2, "probs")
malepred2 <- cbind(maledf2, malepred2)
femaledf2 <- data.frame(Age = c("18-29", "30-44", "45-60", "60+" ), Gender = "Female")
femalepred2 <- predict(twoseat, newdata = femaledf2, "probs")
femalepred2 <- cbind(femaledf2, femalepred2)
pred2 <- rbind(malepred2, femalepred2)
pred2 <- melt(pred2, value.name = "prob")%>%
  rename("Response" = "variable")

#Plotting predictions
ggplot(pred2, aes(x = Age, y = prob, colour = Response, shape = Gender)) +
  geom_point() +
  labs(title = "Who should have claim over the middle armrest in a row of two seats?",
       subtitle = "Predicted probability that someone in each category would choose each response",
       x = "Age", y = "Probability of choosing response")+
  theme_minimal()

#Rows with three seats
threeseat <- multinom(seats3 ~ Gender + Age, data = q4)
threeseatoutput <- summary(threeseat)
z3 <- threeseatoutput$coefficients/threeseatoutput$standard.errors
p3 <- (1-pnorm(abs(z3),0,1))*2 # I am using two-tailed z test
print(p3, digits =2)

maledf3 <- data.frame(Age = c("18-29", "30-44", "45-60", "60+" ), Gender = "Male")
malepred3 <- predict(threeseat, newdata = maledf3, "probs")
malepred3 <- cbind(maledf3, malepred3)
femaledf3 <- data.frame(Age = c("18-29", "30-44", "45-60", "60+" ), Gender = "Female")
femalepred3 <- predict(threeseat, newdata = femaledf3, "probs")
femalepred3 <- cbind(femaledf3, femalepred3)
pred3 <- rbind(malepred3, femalepred3)
pred3 <- melt(pred3, value.name = "prob") %>%
  rename("Response" = "variable")

#Plotting predictions
ggplot(pred3, aes(x = Age, y = prob, colour = Response, shape = Gender)) +
  geom_point() +
  labs(title = "Who should have two armrests in a row of three seats?",
    subtitle = "Predicted probability that someone in each category would choose each response",
    x = "Age", y = "Probability of choosing response")+
  theme_minimal()


mean(q4$seats3 == 'The arm rests should be shared')
mean(q4$seats2 == 'The arm rests should be shared')
nrow(q4)



kable(with(q4, table(seats2)) %>% prop.table(), format = "latex") %>%
  kable_styling(latex_options="scale_down")

           