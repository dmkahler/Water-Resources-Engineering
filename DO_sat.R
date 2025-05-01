library(ggplot2)
library(latex2exp)

temp <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45)
do <- c(14.6,14.19,13.81,13.44,13.09,12.75,12.43,12.12,11.83,11.55,11.27,11.01,10.76,10.52,10.29,10.07,9.85,9.65,9.45,9.26,9.07,8.9,8.72,8.56,8.4,8.24,8.09,7.95,7.81,7.67,7.54,7.41,7.28,7.16,7.16,6.93,6.82,6.71,6.61,6.51,6.41,6.41,6.22,6.13,6.04,5.95)
DO_sat <- data.frame(temp,do)

ggplot(DO_sat) +
     geom_point(aes(x=temp,y=do)) +
     labs(x = TeX('Temperature ($^o C$)'), y = TeX('Dissolved Oxygen ($mg/l$)')) +
     theme(panel.background = element_rect(fill = "white", colour = "black")) +
     theme(aspect.ratio = 1) +
     theme(axis.text = element_text(face = "plain", size = 14), axis.title = element_text(face = "plain", size = 14)) +
     theme(legend.text = element_text(face = "plain", size = 14), legend.title = element_text(face = "plain", size = 14))
