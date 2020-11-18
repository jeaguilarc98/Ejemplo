library(tidyverse)
library(plotly)
library(IRdisplay)
library(rio)
colors ＜- c("#FFFFFF","#F5FCC2","#E0ED87","#CCDE57",
            "#B3C732","#94A813","#718200")

data ＜- diamonds %＞% 
  group_by(color) %＞% 
  summarize(counts = n(),
            percentage = n()/nrow(diamonds))
# Donut chart with ggplot2

donut ＜- ggplot(data = data, aes(x=2, y = percentage, fill = color))+
  geom_col(color = "black") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(round(percentage*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18)) +
  ggtitle("Donut Chart of Diamond Color (ggplot2)") +
  scale_fill_manual(values = colors) +
  xlim(0.5, 2.5)
donut

dir()
migracion_cantonal<-import("migracion_cantonal.xlsx")
library(dplyr)  
library(readr)
emigrantes<-aggregate(migracion_cantonal$`2010`, by=list(Category=migracion_cantonal$Destino), FUN=sum)
colnames(emigrantes)<- c("Cantón","Emigrantes")
total_emi<- sum(emigrantes$Emigrantes)
emigrantes$percentage<- (emigrantes$Emigrantes/total_inm)
colnames(inmigrantes)<- c("Cantón","Inmigrantes","Porcentaje Inmigrantes")

emig<- emigrantes %>% arrange(desc(emigrantes$Emigrantes))
head(emig)
inmigrantes2<- subset(head(inmig))
emigrantes2<- subset(emig, Cantón %in% c("Quito","Guayaquil","Ambato","Cuenca","Machala"))
total_emi<-sum(emigrantes2$Emigrantes)
emigrantes2$percentage<-NULL
emigrantes2$percentage<- emigrantes2$Emigrantes/total_emi
sum(emigrantes2$percentage)
library(ggplot2)
donut ＜- ggplot(data = emigrantes2, aes(x=2, y = percentage, fill = Cantón))+
  geom_col(color = "black") +
  coord_polar("y", start = 0) + 
  geom_label(x = 2, aes(y = percentage, label = Cantón), 
             size = 4.5, color = "white", fontface = "bold", show.legend = FALSE)+
  geom_text(aes(label = paste0(round(percentage*100), "%")), 
            position = position_stack(vjust = 0.5)) +
  theme(panel.background = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(), 
        plot.title = element_text(hjust = 0.5, size = 18)) +
  ggtitle("Porcentaje de Emigrantes de las 5 ciudades \nautorepresentadas del Ecuador") +
  scale_fill_manual(values = colors) +
  xlim(0.5, 2.5)
donut


