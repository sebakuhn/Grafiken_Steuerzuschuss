library(readxl)
 
#Kuchendiagramm

data_pie <- read_excel("01_proc_data/Abbildungen_Steuerzuschuss_n.xlsx", sheet = "Kuchendiagramm")

View(data_pie)


data <- data_pie %>% 
  arrange(desc(Posten)) %>%
  mutate(ypos = cumsum(Prozentual)- 0.5*Prozentual)

pie<-ggplot(data , aes(x="", y=Prozentual, fill=Posten)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  scale_fill_manual(values = c("Beiträge" = "#009036", "Zusatzbeiträge" ="#63b339", "Steuerzuschuss" = "#17a6d6", "Covid-19-Erstattungen" = "#bc388a" ))+ #als Farben habe ich welche aus dem Powerpointmaster gewählt
  geom_text(aes(y=ypos, label=scales::percent(Prozentual, accuracy = 1)), color = "white", size=10)+
  labs(title = 'Zusammensetzung Einnahmen',
       subtitle = '',
       caption = 'Quelle: Eigene Darstellung',
       fill ="" ) + #das sorgt für einen leeren Legendentitel, der kann aber auch beliebig angepasst werden
  theme_aok(map=TRUE)+ # Keinerlei Hintergrundgedöns
  theme(legend.position = "right",
        legend.text = element_text(family="Frutiger for AOK", size = 25),
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        plot.caption=element_text(hjust = 0.5, size = 20))

jpeg("06_graphs/Einnahmen_Pie.jpeg",
     units="cm",
     width=40, height=40, res=500) # hier kannst du an der Größe rumdrehen
pie
dev.off()

### Kippunkt

kippunkt <- read_excel("01_proc_data/Abbildungen_Steuerzuschuss_n.xlsx", sheet = "Kipppunkt")

kippunkt<-kippunkt %>%  gather(Jahr, "Value", 2:15)%>%filter(Wert == "Beitragseinnahmen ohne Zusatzbeiträge"| Wert =="Ausgaben GKV")%>%mutate(Wert = as.factor(Wert))

label<-data.frame(
  Jahr  = c(1.5, 5.5, 10.5),
  Value = c(145, 170, 200),
  label = c("Erhöhung allg. \n Beitragssatz\n von 14,9% auf 15,5%", "Absenkung allg. \n Beitragssatz\n von 15,5% auf 14,6%","Geringere\n Grundlohnsteigerung\n auf Grund von COVID-19"))

kipp_graph<-ggplot(kippunkt, aes(x=Jahr, y=Value, group=Wert))+
  geom_line(aes(colour = Wert), size = 1.2)+
  scale_color_manual(values = c("Ausgaben GKV" = "#e30c19",  "Beitragseinnahmen ohne Zusatzbeiträge" = "#17a6d6"))+ #als Farben habe ich welche aus dem Powerpointmaster gewählt
  labs(title = 'Entwicklung Beitragseinnahmen und Ausgaben GKV',
       subtitle = '',
       caption = '*Prognose des Schätzerkreises im Oktober 2022',
       colour="") + #das sorgt für einen leeren Legendentitel, der kann aber auch beliebig angepasst werden
  ylab("In Mio. Euro")+
  theme_aok(map=FALSE)+ # Keinerlei Hintergrundgedöns
  theme(legend.position = "top",
        legend.text = element_text(family="Frutiger for AOK", size = 25),
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        plot.caption=element_text(hjust = 0, size = 12))+
  geom_label(data = label, aes(x=Jahr, y=Value, label =label), inherit.aes = FALSE)+
  geom_segment(aes(x = 1.5, y = 151, xend = 1.5, yend = 161),
               arrow = arrow(length = unit(0.5, "cm")))+
  geom_segment(aes(x =5.5, y = 176, xend = 5.5, yend =183),
               arrow = arrow(length = unit(0.5, "cm")))+
  geom_segment(aes(x =10.5, y = 206, xend = 10.5, yend = 218),
               arrow = arrow(length = unit(0.5, "cm")))

jpeg("06_graphs/Einnahmen_Ausagben.jpeg",
     units="cm",
     width=40, height=30, res=500) # hier kannst du an der Größe rumdrehen
kipp_graph
dev.off()

#---- Balkendiagramm

balken <- read_excel("01_proc_data/Abbildungen_Steuerzuschuss_n.xlsx", sheet = "Balkendiagramm")
balken<-balken %>% arrange(Wert) %>% gather(Jahr, "Value", 2:21)%>%filter(Wert == "Steuerzuschuss nach § 221 SGB V"| Wert == "Steuerzuschuss nach § 221a SGB V" )%>%mutate(Wert = as.factor(Wert))

barplot<-ggplot(balken, aes(x = Jahr, y = Value, fill = Wert))+
  geom_col(position = position_stack(reverse = TRUE))+
  geom_text(aes(x= Jahr, y= Value, label=Value, group = Wert), position=position_stack(vjust=0.5, reverse=TRUE), color = "white") +
  guides(fill = guide_legend(reverse = FALSE))+
  scale_fill_manual(values = c("#17a6d6", "#e30c19"))+ #als Farben habe ich welche aus dem Powerpointmaster gewählt
  labs(title = 'Entwicklung des Steuerzuschusses seit dessen Einführung 2004',
       subtitle = '',
       caption =c('*Zusätzlicher Steuerzuschuss i. H. v. 3,5 Mrd. EUR abweichend nach §12a Haushaltsgesetz 2020',
                  '\n**einschl. 0,3 Mrd. EUR (2021 und 2022) bzw. 150 Mio. EUR (2023) an die Liquiditätsreserve des Gesundheitsfonds für Krankengeld bei Erkrankung des Kindes'),
       fill="") + #das sorgt für einen leeren Legendentitel, der kann aber auch beliebig angepasst werden
  ylab("In Mrd. Euro")+
  theme_aok(map=FALSE)+ # Keinerlei Hintergrundgedöns
  theme(legend.position = "top",
        legend.text = element_text(family="Frutiger for AOK", size = 25),
        plot.title = element_text(hjust = 0.5, size = 30, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        plot.caption=element_text(hjust = c(0,0), size = 12))


jpeg("06_graphs/Säulen.jpeg",
     units="cm",
     width=40, height=30, res=500) # hier kannst du an der Größe rumdrehen
barplot
dev.off()

