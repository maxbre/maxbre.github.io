library(openair)
data(mydata)
c <- sample(mydata$nox, 12*60)
s <- seq(5, 3600, 5)

mydf <- data.frame(s=s, conc=c/200)
mydf$conc[sample(1:720, 150)]<-NA

library(ggplot2)

ggplot(data=mydf, aes(x=s, y=conc))+
  geom_line(aes(colour='5 s'), size=0.3)+
  geom_line(aes(y=mean(conc, na.rm=TRUE), colour='1 h'), linewidth=0.5)+
  geom_hline(yintercept=1, linetype="dashed", color = "blue")+
  annotate('text',
           x=3670, 
           y=1.09, 
           label="ref.", 
           size=3.5, colour="blue")+ 
  scale_x_continuous(breaks = seq(0, 3600, 600))+
  scale_color_manual(name = 'avg time', values = c('5 s' = 'orange', '1 h' = 'darkgreen'))+
  xlab('time [sec]')+
  ylab('["odour"]')+
  theme_minimal()+
  theme(legend.position = "bottom")

