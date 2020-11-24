library(tidyverse)
library(questionr)
library(nnet)
library(ggplot2)
library(data.table)
library(gplots)
library(caret)

#Getting data only necessary for question 4
q4 <- airplane_etiquette %>%
  select(., `In a row of three seats, who should get to use the two arm rests?`,
         `In a row of two seats, who should get to use the middle arm rest?`,
         Gender, Age, `Location (Census Region)`)
#Removing NAs
q4 <- na.rm(q4)

#Renaming "> 60" to "60+"
q4$Age <- ifelse(q4$Age == "> 60", "60+", q4$Age)

#Dummy variables
q4$Gender <- as.factor(q4$Gender)
q4$Age <- as.factor(q4$Age)
q4dum <- model.matrix(~ -1 + Age + Gender, data = q4)
q4dum <- cbind("ThreeSeats" = q4$`In a row of three seats, who should get to use the two arm rests?`,
            "TwoSeats" = q4$`In a row of two seats, who should get to use the middle arm rest?`,
            q4dum)
#Removing Age18-34 to remove collinearity
q4dum <- as.data.frame(q4dum[,-3])

#Getting levels of responses
unique(q4$`In a row of three seats, who should get to use the two arm rests?`)
unique(q4$`In a row of two seats, who should get to use the middle arm rest?`)
unique(q4$Gender)
unique(q4$Age)

#Initial multinomial regression test
twoseat <- multinom(`In a row of two seats, who should get to use the middle arm rest?` ~ Gender + Age, data = q4)
summary(twoseat)
z2 <- summary(twoseat)$coefficients/summary(twoseat)$standard.errors
p2 <- (1 - pnorm(abs(z2), 0, 1)) * 2; p2

maledf2 <- data.frame(Age = c("18-29", "30-44", "45-60", "60+" ), Gender = "Male")
malepred2 <- predict(twoseat, newdata = maledf2, "probs")
malepred2 <- cbind(maledf2, malepred2)
femaledf2 <- data.frame(Age = c("18-29", "30-44", "45-60", "60+" ), Gender = "Female")
femalepred2 <- predict(twoseat, newdata = femaledf2, "probs")
femalepred2 <- cbind(femaledf2, femalepred2)
pred2 <- rbind(malepred2, femalepred2)
pred2 <- melt(pred2, value.name = "prob")

ggplot(pred2, aes(x = (Age), y = prob, colour = variable)) +
  geom_point() +
  facet_grid(Gender ~ ., scales = "free")


threeseat <- multinom(`In a row of three seats, who should get to use the two arm rests?` ~ Gender + Age, data = q4)
summary(threeseat)
z3 <- summary(threeseat)$coefficients/summary(threeseat)$standard.errors
p3 <- (1 - pnorm(abs(z3), 0, 1)) * 2; p3

maledf3 <- data.frame(Age = c("18-29", "30-44", "45-60", "60+" ), Gender = "Male")
malepred3 <- predict(threeseat, newdata = maledf3, "probs")
malepred3 <- cbind(maledf3, malepred3)
femaledf3 <- data.frame(Age = c("18-29", "30-44", "45-60", "60+" ), Gender = "Female")
femalepred3 <- predict(threeseat, newdata = femaledf3, "probs")
femalepred3 <- cbind(femaledf3, femalepred3)
pred3 <- rbind(malepred3, femalepred3)
pred3 <- melt(pred3, value.name = "prob")

ggplot(pred3, aes(x = (Age), y = prob, colour = variable)) +
  geom_point() +
  facet_grid(Gender ~ ., scales = "free")
lm(ThreeSeats ~ . - TwoSeats, data = q4dum)

chisq.test(q4$Age, q4$`In a row of three seats, who should get to use the two arm rests?`)
chisq.test(q4$Gender, q4$`In a row of three seats, who should get to use the two arm rests?`)
chisq.test(q4$Age, q4$`In a row of two seats, who should get to use the middle arm rest?`)
chisq.test(q4$Gender, q4$`In a row of two seats, who should get to use the middle arm rest?`)

