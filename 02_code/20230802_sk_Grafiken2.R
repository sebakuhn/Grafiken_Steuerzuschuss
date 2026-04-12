library(ggplot2)
library(readxl)
library(viridis)
library(tidyverse)
library(dplyr)
library(aokaux)
library(aokbwcd)
library(ggsci)

#Kuchendiagramm Claudi

data_pie <- read_excel("01_proc_data/Abbildungen_Steuerzuschuss_n.xlsx", sheet = "Kuchendiagramm")

View(data_pie)


data <- data_pie %>% 
  arrange(desc(Posten)) %>%
  mutate(ypos = cumsum(Prozentual)- 0.5*Prozentual)

ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) # Try to remove that to see how to make a pie chart


pie<-ggplot(data , aes(x="", y=Prozentual, fill=Posten)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  scale_fill_manual(values = c("Beiträge" = "#000080", "Zusatzbeiträge" ="#0000CD", "Steuerzuschuss" = "#0000FF", "Covid-19-Erstattungen" = "#00BFFF" ))+ 
  geom_text(x=1.3, aes(family = "AOK Buenos Aires", 
                y=ypos, label=scales::percent(Prozentual, accuracy = 1)), color = "white", size=12)+
  #geom_text(x=1.7, aes(y=ypos, label=c("Zusatzbeiträge", "Steuerzuschuss", "Covid-19-Erstattungen", "Beiträge")), color = c("#0000CD", "#0000FF", "#00BFFF", "#000080" ), size=9)+
  labs(
      #title = 'Abbildung 2: Einnahmen des Gesundheitsfonds 2020',
       subtitle = '',
      #caption = 'Quelle: Eigene Berechnungen auf Basis der KJ1 des Gesundheitsfonds 2020.',
       fill ="" ) + #das sorgt für einen leeren Legendentitel, der kann aber auch beliebig angepasst werden
  theme_aok(map=TRUE) + # Keinerlei Hintergrundgedöns
  theme(text = element_text(family = "AOK Buenos Aires"),
        legend.position = "right",
        legend.text = element_text(family="Frutiger for AOK", size = 30),
        plot.title = element_text(hjust = 0, size = 25, face = "italic", color="#000080"),
        plot.subtitle = element_text(hjust = 0, size = 20),
        plot.caption=element_text(hjust = 0, size = 25, color="black"))

jpeg("06_graphs/Einnahmen_Pie_Seba.jpeg",
     units="cm",
     width=50, height=30, res=500) # hier kannst du an der Größe rumdrehen
pie + scale_fill_d3()
dev.off()

jpeg("06_graphs/Einnahmen_Pie_Seba_bw.jpeg",
     units="cm",
     width=50, height=30, res=500) # hier kannst du an der Größe rumdrehen
pie + scale_fill_grey()
dev.off()

### Kippunkt Claudi

kipppunkt <- read_excel("01_proc_data/Abbildungen_Steuerzuschuss_n.xlsx", sheet = "Kipppunkt")

kipppunkt<-kipppunkt %>% gather(Jahr, "Value", 2:15) %>% filter(Wert == "Beitragseinnahmen ohne Zusatzbeiträge"| Wert =="Ausgaben GKV") %>% mutate(Wert = as.factor(Wert))

label<-data.frame(
  Jahr  = c(2, 6, 11),
  Value = c(145, 160, 197),
  label = c("Erhöhung allg. \n Beitragssatz von\n 14,9% auf 15,5%", "Absenkung allg. \n Beitragssatz von\n 15,5% auf 14,6%","Geringere\n Grundlohnsteigerung\n aufgrund von Covid-19"))

kipp_graph<-ggplot(kipppunkt, aes(x=Jahr, y=Value, group=Wert))+
  geom_line(aes(colour = Wert), size = 3.5)+
  scale_color_manual(values = c("Ausgaben GKV" = "#000080",  "Beitragseinnahmen ohne Zusatzbeiträge" = "#0000FF"))+ 
  labs(
       #title = 'Abbildung 1: Einnahmen- und Ausgabenentwicklung in der GKV',
       subtitle = '',
       caption = '*Prognose des Schätzerkreises im Oktober 2022',
       colour="") + #das sorgt für einen leeren Legendentitel, der kann aber auch beliebig angepasst werden
  ylab("in Mrd. Euro")+
  ylim(140, 305)+
  theme_aok(map=FALSE)+ # Keinerlei Hintergrundgedöns
  theme(text = element_text(family = "AOK Buenos Aires"),
        legend.position = c(0.3,0.9),
        legend.direction="vertical",
        legend.key.size = unit(3, 'lines'),
        legend.text = element_text(size = 34, color="black"),
        plot.title = element_text(hjust = 0, size = 33, face = "italic", color="#000080"),
        plot.subtitle = element_text(hjust = 0, size = 33),
        plot.caption=element_text(hjust = 0, size = 25, face = "italic", color="black"),
        axis.text.x = element_text(size = 32, color="black", angle = 45, hjust = 1),
        axis.title.x = element_text(size =35),
        axis.text.y = element_text(size = 32, color="black"),
        axis.title.y = element_text(size =35),
        panel.grid = element_blank()
        )+
  #geom_label(data = label, aes(x=Jahr, y=Value, label =label), label.r = unit(0.5, "cm"), inherit.aes = FALSE)+
  geom_text(data = label,
            aes(family="AOK Buenos Aires", x=Jahr, y=Value, label =label), size = 10.5, inherit.aes = FALSE)+
  geom_segment(aes(x = 2, y = 155, xend = 2, yend = 167),
               arrow = arrow(length = unit(0.4, "cm"), type="closed"))+
  geom_segment(aes(x = 6, y = 170, xend = 6, yend =182),
               arrow = arrow(length = unit(0.4, "cm"), type="closed"))+
  geom_segment(aes(x = 11, y = 207, xend = 11, yend = 219),
               arrow = arrow(length = unit(0.4, "cm"), type="closed"))


jpeg("06_graphs/Einnahmen_Ausagben_Seba.jpeg",
     units="cm",
     width=50, height=40, res=600) # hier kannst du an der Größe rumdrehen
kipp_graph + scale_color_d3()
dev.off()

jpeg("06_graphs/Einnahmen_Ausagben_Seba_bw.jpeg",
     units="cm",
     width=50, height=40, res=600) # hier kannst du an der Größe rumdrehen
kipp_graph + scale_color_grey()
dev.off()

#---- Balkendiagramm Claudi

balken <- read_excel("01_proc_data/Abbildungen_Steuerzuschuss_n.xlsx", sheet = "Balkendiagramm")
balken<-balken %>% arrange(Wert) %>% gather(Jahr, "Value", 2:21)%>%filter(Wert == "Steuerzuschuss nach § 221 SGB V"| Wert == "Steuerzuschuss nach § 221a SGB V" )%>%mutate(Wert = as.factor(Wert))

barplot<-ggplot(balken, aes(x = Jahr, y = Value, fill = Wert))+
  geom_col(position = position_stack(reverse = TRUE))+
  geom_text(aes(family = "AOK Buenos Aires", x= Jahr, y= Value, label=format(Value, decimal.mark = ","), group = Wert), 
            position = position_stack(vjust=0.5, reverse=TRUE), color = "white", size =8) +
  guides(fill = guide_legend(reverse = FALSE))+
  scale_fill_manual(values = c("#000080", "#0000FF"))+ 
  labs(
      #title = 'Abbildung 3: Entwicklung des Steuerzuschusses seit dessen Einführung 2004',
       subtitle = '',
       caption =c('* Zusätzlicher Steuerzuschuss i. H. v. 3,5 Mrd. EUR abweichend nach §12a Haushaltsgesetz 2020',
                  '\n** einschl. 0,3 Mrd. EUR (2021 und 2022) bzw. 150 Mio. EUR (2023) an die Liquiditätsreserve des Gesundheitsfonds für Kinderkrankengeld'),
       fill="") + #das sorgt für einen leeren Legendentitel, der kann aber auch beliebig angepasst werden
  ylab("in Mrd. Euro")+
  theme_aok(map=FALSE)+ # Keinerlei Hintergrundgedöns
  theme(text = element_text(family = "AOK Buenos Aires"),
        legend.position = c(0.25,0.865),
        legend.direction="vertical",
        legend.key.size = unit(3, 'lines'),
        legend.text = element_text(size = 30),
        plot.title = element_text(hjust = 0, size = 25, face = "italic", color="#000080"),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        plot.caption=element_text(hjust = c(0,0), size = 20, face="italic", color="black"),
        axis.text.x = element_text(size = 27, color="black", angle = 45, hjust = 1),
        axis.title.x = element_text(size =35),
        axis.text.y = element_text(size = 27, color="black"),
        axis.title.y = element_text(size =35),
        panel.grid = element_blank()
        )

jpeg("06_graphs/Säulen_Seba.jpeg",
     units="cm",
     width=50, height=30, res=600) # hier kannst du an der Größe rumdrehen
barplot + scale_fill_d3()
dev.off()

jpeg("06_graphs/Säulen_Seba_bw.jpeg",
     units="cm",
     width=50, height=30, res=600) # hier kannst du an der Größe rumdrehen
barplot + scale_fill_grey()
dev.off()
