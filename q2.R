library(ggplot2)
library(rstatix)
ggplot(recline, aes (x = recline, y = inches)) + geom_boxplot(aes(fill = recline)) + 
  theme_minimal() +
  theme(legend.position = "none") + 
  xlab("Response to 'Do you ever recline your seat when you fly?'") +
  ylab("Height (in.)") +
  labs(title = "Respondents' Reclining Preferences vs Height")
rec.ttest <- recline %>% pairwise_t_test(inches ~ recline); rec.ttest
