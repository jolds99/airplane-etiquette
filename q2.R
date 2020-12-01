library(ggplot2)
library(rstatix)
library(nnet)
library(ggpubr)
library(car)
ggplot(q2, aes (x = inches, y = recline)) + geom_boxplot(aes(fill = recline)) + 
  theme_minimal() +
  theme(legend.position = "none") + 
  ylab("Response to 'Do you ever recline your seat when you fly?'") +
  xlab("Height (in.)") +
  labs(title = "Respondents' Height vs Reclining Preferences")
rec.ttest <- q2 %>% pairwise_t_test(inches ~ recline); rec.ttest
kruskal.test(inches ~ recline, data = q2)
