library(dataRetrieval)
library(dplyr) # for `rename` & `select`
library(tidyr) # for `gather`
library(ggplot2)
setwd('D:/')
library("readxl")
#install.packages("gridExtra")
library("gridExtra")
#install.packages("cowplot")
library("cowplot")
#theme_set(theme_cowplot(font_size=8))
library(gridExtra)
library(ggtext)

cor2 <- read_excel("data.xlsx", sheet = 'NH3_Sheet1')
head(cor2)
cor2$Period=factor(cor2$Period,levels=c('Zeroth hour'))
cor2$CT<-factor(cor2$CT,levels=c('Test','Control'),labels=c('Test','Control'))
cor2$Temperature<-factor(cor2$Temperature,levels=c('Room Temperature'),labels=c('Room Temperature'))

p1= ggplot(data=cor2, aes(x = `Day`,y = `Mg/L`,fill = CT,color=CT)) +
  geom_bar(stat="identity",color = "black",position=position_dodge(width=0.9))+
  geom_errorbar(aes(y = `Mg/L`, ymin = `Mg/L` - STD, ymax = `Mg/L` + STD),color="black",position=position_dodge(width=0.9)) +  
  theme_light() +
  scale_x_continuous("Day", labels = as.character(cor2$Day), breaks = cor2$Day)+
  theme(legend.text = element_markdown())+
  facet_grid(.~Period, scales = "free_x") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background.x = element_rect(color="black", fill="yellowgreen", size=1, linetype="solid"),
        strip.text = element_text(colour = 'black'))+
  theme(legend.title=element_blank())+
  #scale_fill_manual(values = c("#FF9999", "#9999CC"))+
  scale_color_manual(values = c("#FF9999", "#9999CC"))
  
p1

cor3 <- read_excel("Shelf life -NBC.xlsx", sheet = 'NH3_2')
head(cor2)
cor3$Period=factor(cor3$Period,levels=c('After Week 1','After Week 2','After Week 3'))
cor3$CT<-factor(cor3$CT,levels=c('Test','Control'),labels=c('Test','Control'))
cor3$Temperature<-factor(cor3$Temperature,levels=c('Room Temperature','4 Degree Celcius'),labels=c('Room Temperature','4 Degree Celcius'))

p2= ggplot(data=cor3, aes(x = `Day`,y = `Mg/L`,fill = Temperature,color=Temperature)) +
  geom_bar(stat="identity",color = "black",position=position_dodge(width=0.9))+
  geom_errorbar(aes(y = `Mg/L`, ymin = `Mg/L` - STD, ymax = `Mg/L` + STD),color="black",position=position_dodge(width=0.9)) +  
  theme_light() +
  scale_x_continuous("Day", labels = as.character(cor2$Day), breaks = cor2$Day)+
  theme(legend.text = element_markdown())+
  facet_grid(.~Period, scales = "free_x") +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 1), 
        strip.background.x = element_rect(color="black", fill="yellowgreen", size=1, linetype="solid"),
        strip.text = element_text(colour = 'black'))+
  theme(legend.title=element_blank())+
  #scale_fill_manual(values = c("lightgreen", "brown"))+
  #scale_color_manual(values = c("lightgreen", "brown"))
  #scale_fill_brewer(palette="Paired")
  scale_fill_brewer(palette="Dark2")

p2

g1 <- p1 %+% dplyr::filter(cor2, cor2$Period == 'Zeroth hour') + theme(legend.position = "top")
g2 <- p2 + theme(legend.position = "top")
library(ggpubr)
ggarrange(g1,g2)


gridExtra::grid.arrange(g1, g2,
                        layout_matrix = 
                          matrix(c(1, 1, 2, 2, 2, 2, 2,
                                   1, 1, 2, 2, 2, 2, 2,
                                   1, 1, 2, 2, 2, 2, 2),
                                 byrow = TRUE, nrow = 3) 
)


#wi9dth 1500 h 650
