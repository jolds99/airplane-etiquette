library(ggplot2)
library(rstatix)
library(nnet)
ggplot(q2, aes (x = inches, y = recline)) + geom_boxplot(aes(fill = recline)) + 
  theme_minimal() +
  theme(legend.position = "none") + 
  ylab("Response to 'Do you ever recline your seat when you fly?'") +
  xlab("Height (in.)") +
  labs(title = "Respondents' Height vs Reclining Preferences")
rec.ttest <- q2 %>% pairwise_t_test(recline ~ inches); rec.ttest
multinom(recline ~ inches, data = q2)
kruskal.test(recline ~ inches, data = q2)
