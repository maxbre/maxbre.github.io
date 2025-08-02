
y <- c(1.8, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 4.7, 4.5, 4.3, 4.0, 3.8, 3.5, 3.0, 2.5, 2.2, 2.0, 1.5, 1.0, 0.8, 0.5, 1.0, 1.8,
       2.2, 2.5, 2.0, 1.7, 1.1, 0.7, 0.5, 0.3, 0.3, 0.3, 0.5, 1.2, 1.5, 1.8, 2.0, 2.2, 2.5, 2.9, 3.5, 4.5,
       5.0, 5.0, 4.8, 4.5, 4.3, 4.1, 3.5, 3.0, 2.5, 2.8, 2.2, 1.8, 1.0, 1.5, 1.8, 1.9)

x=1:length(y)

mydf <- data.frame(x=x, y=y)

library(tidyverse)

mydf %>% 
  ggplot()+
  geom_line(aes(x = x, y = y), colour = "blue", size = 1.5)+
  annotate("text", x = 20, y= 4, 
           label = "actual ambient\nodour concentration\nin the environment",
           color = "blue", size = 3, fontface = "bold")+
  scale_x_continuous(n.breaks = 6, minor_breaks = 1:60)+
  scale_y_continuous()+
  labs(x = "elapsed time [min]", y = "odour concentration [OUE/m3]" )+
  geom_hline(yintercept = 2, colour="darkgreen", linetype = "dashed")+
  annotate("text", x = 47, y= 1.6, 
           label = "model prediction\nodour concentration\nmean 1h",
           color = "darkgreen", size=3, fontface = "bold")+
  geom_hline(yintercept = 5, colour = "red", linetype = "dashed")+
  annotate("segment", x = 35, xend = 35, y = 2, yend = 5,
           colour = "red", size=1,
           arrow = arrow(ends = "both", length = unit(.3,"cm"))
           )+
  annotate("text", x = 30, y= 3.3, 
           label = "peak-to-mean\nratio",
           color = "red", size=3, fontface = "bold")+
  annotate("text", x = 25, y= 5.2, 
           label = "odour concentration at which odour recognition occours",
           color = "red", size=3, fontface = "bold")+
  theme_minimal()
  
