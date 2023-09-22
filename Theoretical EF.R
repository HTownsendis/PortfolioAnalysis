library(tidyverse)


a =30
x=0:22
# y= (a*x^2+ 2*x-1)/(x*(x+12))
# y_p = (a*x^2+ 2*x-1)/(x*(x+10))
y= ((a+5)*x^2)/(x*(x+20)+1)
y_p = (a*x^2)/(x*(x+10)+1)

theo_y <- c(10,10,10)
theo_x <- c(5,8,15)

       
EF_y <- as.data.frame(cbind(x,y))
EF_yp <- as.data.frame(cbind(x,y_p))

EF_y <- EF_y %>% rename("StdDev" = "x","Revenue" = "y")
EF_yp <- EF_yp %>% rename("StdDev" = "x","Revenue" = "y_p")

EF_y$type <- "F"
EF_yp$type <- "F'"

EF_theo <- as.data.frame(rbind(EF_yp, EF_y))
EF_theo$type <- factor(EF_theo$type , levels = c("F'", "F"))
EF_theo <- EF_theo %>% arrange(type)

Theo_plt <- ggplot(data = EF_theo, aes(x= StdDev, y=Revenue, color=type)) +
  scale_x_continuous(limits = c(0,25), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,24), expand = c(0, 0)) +
  geom_line() +
    scale_color_manual(values=c('orangered','turquoise2'), 
                       labels=c("F' (EBFM)","F (Single Species)")) +
  geom_point(aes(x=5.115,y=10),shape=1, color="gray45", size=3) + 
    geom_text(x=5.115, y=10, label="a'", color="gray45", hjust=2, vjust=2) +
  geom_point(aes(x=8.105,y=10),shape=1, color="gray45", size=3) +
   geom_text(x=8.105, y=10, label="a", color="gray45", hjust=2, vjust=2) +
  geom_point(aes(x=15,y=10),shape=1, color="gray45", size=3) +
   geom_text(x=15, y=10, label="b", color="gray45", hjust=2, vjust=2) +
  geom_segment(aes(x=0.5,xend=15,y=10,yend=10), color="gray45", linetype = "dashed") +
    geom_text(x=2, y=12, label= "R", color="gray45") + 
  geom_segment(aes(x=15,xend=15,y=0.5,yend=10), color="gray45", linetype = "dashed") +
    geom_text(x=17, y=2, label= "SD", color="gray45") + 
  labs(x=expression(paste("Risk (Standard Deviation)")), 
       y= expression(paste("Expected Return (Mean Revenue)"))) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(colour = "black"),
        aspect.ratio = 1,
        axis.title = element_text(size = 12)) 
Theo_plt

pdf("Theoretical_EF.pdf", width=11, height=8.5) #print to pdf
Theo_plt
dev.off() #turns off pdf printing


