library(readxl)

#Kuchendiagramm Claudi

data_pie <- read_excel("01_proc_data/Abbildungen_Steuerzuschuss_n.xlsx", sheet = "Kuchendiagramm")

View(data_pie)


data <- data_pie %>% 
  arrange(desc(Posten)) %>%
  mutate(ypos = cumsum(Prozentual)- 0.5*Prozentual)

pie<-ggplot(data , aes(x="", y=Prozentual, fill=Posten)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0)+
  scale_fill_manual(values = c("Beiträge" = "#000080", "Zusatzbeiträge" ="#0000CD", "Steuerzuschuss" = "#0000FF", "Covid-19-Erstattungen" = "#00BFFF" ))+ 
  geom_text(x=1.3, aes(y=ypos, label=scales::percent(Prozentual, accuracy = 1)), color = "white", size=10)+
  #geom_text(x=1.7, aes(y=ypos, label=c("Zusatzbeiträge", "Steuerzuschuss", "Covid-19-Erstattungen", "Beiträge")), color = c("#0000CD", "#0000FF", "#00BFFF", "#000080" ), size=9)+
  labs(
      #title = 'Abbildung 2: Einnahmen des Gesundheitsfonds 2020',
       subtitle = '',
      #caption = 'Quelle: Eigene Berechnungen auf Basis der KJ1 des Gesundheitsfonds 2020.',
       fill ="" ) + #das sorgt für einen leeren Legendentitel, der kann aber auch beliebig angepasst werden
  theme_aok(map=TRUE) + # Keinerlei Hintergrundgedöns
  theme(legend.position = "right",
        legend.text = element_text(family="Frutiger for AOK", size = 30),
        plot.title = element_text(hjust = 0, size = 25, face = "italic", color="#000080"),
        plot.subtitle = element_text(hjust = 0, size = 20),
        plot.caption=element_text(hjust = 0, size = 25, color="black"))

jpeg("06_graphs/Einnahmen_Pie_Claudi.jpeg",
     units="cm",
     width=50, height=30, res=500) # hier kannst du an der Größe rumdrehen
pie
dev.off()


### Kippunkt Claudi

kipppunkt <- read_excel("01_proc_data/Abbildungen_Steuerzuschuss_n.xlsx", sheet = "Kipppunkt")

kipppunkt<-kipppunkt %>%  gather(Jahr, "Value", 2:15)%>%filter(Wert == "Beitragseinnahmen ohne Zusatzbeiträge"| Wert =="Ausgaben GKV")%>%mutate(Wert = as.factor(Wert))

label<-data.frame(
  Jahr  = c(1.5, 5.5, 10.5),
  Value = c(145, 166, 200),
  label = c("Erhöhung allg. \n Beitragssatz\n von 14,9% auf 15,5%", "Absenkung allg. \n Beitragssatz\n von 15,5% auf 14,6%","Geringere\n Grundlohnsteigerung\n auf Grund von COVID-19"))

kipp_graph<-ggplot(kipppunkt, aes(x=Jahr, y=Value, group=Wert))+
  geom_line(aes(colour = Wert), size = 1.5)+
  scale_color_manual(values = c("Ausgaben GKV" = "#000080",  "Beitragseinnahmen ohne Zusatzbeiträge" = "#0000FF"))+ 
  labs(
       #title = 'Abbildung 1: Einnahmen- und Ausgabenentwicklung in der GKV',
       subtitle = '',
       caption = '*Prognose des Schätzerkreises im Oktober 2022',
       colour="") + #das sorgt für einen leeren Legendentitel, der kann aber auch beliebig angepasst werden
  ylab("in Mrd. Euro")+
  ylim(140, 305)+
  theme_aok(map=FALSE)+ # Keinerlei Hintergrundgedöns
  theme(legend.position = c(0.25,0.865),
        legend.direction="vertical",
        legend.key.size = unit(3, 'lines'),
        legend.text = element_text(family="Frutiger for AOK", size = 25, color="black"),
        plot.title = element_text(hjust = 0, size = 25, face = "italic", color="#000080"),
        plot.subtitle = element_text(hjust = 0, size = 25),
        plot.caption=element_text(hjust = 0, size = 20, face = "italic", color="black"),
        axis.text.x = element_text(size = 25, color="black"),
        axis.title.x = element_text(size =25),
        axis.text.y = element_text(size = 25, color="black"),
        axis.title.y = element_text(size =25),
        )+
  #geom_label(data = label, aes(x=Jahr, y=Value, label =label), label.r = unit(0.5, "cm"), inherit.aes = FALSE)+
  geom_text(data = label, aes(x=Jahr, y=Value, label =label), size = 7, inherit.aes = FALSE)+
  geom_segment(aes(x = 1.5, y = 153, xend = 1.5, yend = 161),
               arrow = arrow(length = unit(0.5, "cm"), type="closed"))+
  geom_segment(aes(x =5.5, y = 174, xend = 5.5, yend =182),
               arrow = arrow(length = unit(0.5, "cm"), type="closed"))+
  geom_segment(aes(x =10.5, y = 208, xend = 10.5, yend = 216),
               arrow = arrow(length = unit(0.5, "cm"), type="closed"))

jpeg("06_graphs/Einnahmen_Ausagben_Claudi.jpeg",
     units="cm",
     width=50, height=40, res=500) # hier kannst du an der Größe rumdrehen
kipp_graph
dev.off()


#---- Balkendiagramm Claudi

balken <- read_excel("01_proc_data/Abbildungen_Steuerzuschuss_n.xlsx", sheet = "Balkendiagramm")
balken<-balken %>% arrange(Wert) %>% gather(Jahr, "Value", 2:21)%>%filter(Wert == "Steuerzuschuss nach § 221 SGB V"| Wert == "Steuerzuschuss nach § 221a SGB V" )%>%mutate(Wert = as.factor(Wert))

barplot<-ggplot(balken, aes(x = Jahr, y = Value, fill = Wert))+
  geom_col(position = position_stack(reverse = TRUE))+
  geom_text(aes(x= Jahr, y= Value, label=format(Value, decimal.mark = ","), group = Wert), position=position_stack(vjust=0.5, reverse=TRUE), color = "white", size = 6) +
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
  theme(legend.position = c(0.25,0.865),
        legend.direction="vertical",
        legend.key.size = unit(3, 'lines'),
        legend.text = element_text(family="Frutiger for AOK", size = 25),
        plot.title = element_text(hjust = 0, size = 25, face = "italic", color="#000080"),
        plot.subtitle = element_text(hjust = 0.5, size = 20),
        plot.caption=element_text(hjust = c(0,0), size = 20, face="italic", color="black"),
        axis.text.x = element_text(size = 20, color="black"),
        axis.title.x = element_text(size =25),
        axis.text.y = element_text(size = 25, color="black"),
        axis.title.y = element_text(size =25))

jpeg("06_graphs/Säulen_Claudi.jpeg",
     units="cm",
     width=50, height=30, res=500) # hier kannst du an der Größe rumdrehen
barplot
dev.off()
