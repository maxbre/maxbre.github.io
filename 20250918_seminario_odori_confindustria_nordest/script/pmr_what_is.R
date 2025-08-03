
library(tidyverse)

y <- c(1.8, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 4.7, 4.5, 4.3, 4.0, 
       3.3, 3.0, 2.8, 2.3, 2.2, 2.0, 1.5, 1.0, 0.8, 0.5, 1.0, 1.8,
       2.2, 2.4, 2.0, 1.7, 1.1, 0.7, 0.5, 0.3, 0.3, 0.3, 0.5, 1.2,
       1.5, 1.8, 2.0, 2.2, 2.5, 2.9, 3.5, 4.5, 4.8, 4.7, 4.6, 4.4, 
       4.2, 3.8, 3.5, 3.0, 2.8, 2.5, 2.2, 1.8, 1.0, 1.5, 1.8, 1.9)

x = 1:length(y)

mydf <- data.frame(x=x, y=y)


mydf %>% 
  ggplot()+
  geom_line(aes(x = x, y = y), colour = "blue", size = 0.6)+
  annotate("text", x = 20, y= 4.5, 
           label = "actual ambient\nodour concentration",
           color = "blue", size = 3, fontface = "bold")+
  scale_x_continuous(n.breaks = 6, minor_breaks = 1:60)+
  scale_y_continuous()+
  labs(x = "elapsed time [min]", y = "odour concentration [OU]" )+
  geom_hline(yintercept = 2.5, colour="darkgreen", linetype = "solid", size=0.6)+
  annotate("text", x = 26, y= 2.8, 
           label = "model prediction\nodour concentration",
           color = "darkgreen", size=3, fontface = "bold")+
  geom_hline(yintercept = 5, colour = "red", linetype = "dashed", size=0.6)+
  annotate("segment", x = 39, xend = 39, y = 2.5, yend = 5,
           colour = "red", size=0.8, linetype= "solid",
           arrow = arrow(ends = "both", length = unit(.3,"cm"))
  )+
  annotate("text", x = 32, y= 3.8, 
           label = "peak-to-mean\nratio",
           color = "red", size=3, fontface = "bold")+
  annotate("text", x = 17, y= 5.3, 
           label = "odour concentration at which\nrecognition occours",
           color = "red", size=3, fontface = "bold")+
  theme_minimal()+
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_line(colour = "gray"))
