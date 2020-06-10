library(gtrendsR)
library(ggplot2)
library(gridExtra)
library(dplyr)

### Coronavirus Weltweit (2020) ###

#Daten von Google Trends abfragen und sortieren
coronavirus_trends <- gtrends(keyword="Coronavirus", time="2019-12-31 2020-05-15", low_search_volume=TRUE)
coronavirus_trends <- coronavirus_trends$interest_over_time[order(coronavirus_trends$interest_over_time$date),]

coronavirus_trends$hits <- ifelse(coronavirus_trends$hits<1,0,coronavirus_trends$hits)

#Plotten des Suchinteresses weltweit
date <- coronavirus_trends$date
hits <- as.numeric(coronavirus_trends$hits)
data <- data.frame(date, hits)

covid1 <- ggplot(data, aes(x=date, y=hits)) +
  geom_line(color="#770058", size=1.3, alpha=0.9)+
  ggtitle('Suchinteressse von "Coronavirus" (Januar-Mai 2020)') +
  theme_light()

##

#COVID-19 Fälle einlesen und verarbeiten
coronavirus <- read.csv(file = "Seminarprojekt_gTrendsR/Daten/covid-19_cases.csv",header=TRUE, sep = ",")[ ,c("year", "month", "day", "geoId", "cases")]
coronavirus <- coronavirus[order(coronavirus$year, coronavirus$month, coronavirus$day),]

coronavirus$date <- as.Date(with(coronavirus, paste(year, month, day,sep="-")), "%Y-%m-%d")

#Daten auf Anzahl neuer COVID-19 Fälle weltweit zusammenfassen
coronavirus_sum <- data.frame(date=c(), cases=c())

for(tag in unique(coronavirus$date)){
  coronavirus_sum <- rbind(coronavirus_sum, data.frame(date=c(as.Date(tag, "1970-01-01")), cases=c(sum(filter(coronavirus, coronavirus$date == tag)$cases))))
}

#Plotten der COVID-19 Fälle
date <- coronavirus_sum$date
cases <- coronavirus_sum$cases 
data <- data.frame(date, cases)

covid2 <- ggplot(data, aes(x=date, y=cases)) +
  geom_line(color="#770058", size=1.3, alpha=0.9) +
  ggtitle("COVID-19 Fälle weltweit (Januar-Mai 2020)") +
  theme_light()

#Ansicht der Plots
grid.arrange(covid1, covid2, nrow = 2)



### Coronavirus Weltweit, normalisiert (2020) ###

#Daten von Google Trends abfragen und sortieren
coronavirus_trends <- gtrends(keyword="Coronavirus", time="2019-12-31 2020-05-15", low_search_volume=TRUE)
coronavirus_trends <- coronavirus_trends$interest_over_time[order(coronavirus_trends$interest_over_time$date),]

coronavirus_trends$hits <- ifelse(coronavirus_trends$hits<1,0,coronavirus_trends$hits)

#COVID-19 Fälle einlesen und verarbeiten
coronavirus <- read.csv(file = "Seminarprojekt_gTrendsR/Daten/covid-19_cases.csv",header=TRUE, sep = ",")[ ,c("year", "month", "day", "geoId", "cases")]
coronavirus <- coronavirus[order(coronavirus$year, coronavirus$month, coronavirus$day),]

coronavirus$date <- as.Date(with(coronavirus, paste(year, month, day,sep="-")), "%Y-%m-%d")
coronavirus_sum <- data.frame(date=c(), cases=c())

for(tag in unique(coronavirus$date)){
  coronavirus_sum <- rbind(coronavirus_sum, data.frame(date=c(as.Date(tag, "1970-01-01")), cases=c(sum(filter(coronavirus, coronavirus$date == tag)$cases))))
}

#Fälle normalisieren
coronavirus_sum$cases <- ceiling(coronavirus_sum$cases / max(coronavirus_sum$cases) * 100)

#Plotten 
ggplot() + 
  geom_line(data=coronavirus_trends, aes(x=as.Date(date), y=as.numeric(hits)), color="#9E5796", size=1.4 , alpha=1) +
  geom_line(data=coronavirus_sum, aes(x=as.Date(date), y=cases), color="#E93835", size=1.4 , alpha=1) +
  
  labs(x="Zeit", y="Anzahl Fälle (normalisiert) bzw. Suchinteresse", title="Dengue-Fieber Fälle in Singapur mit Suchinteresse verglichen (2014-2020)") +
  theme_light()

#Korrelationskoeffizient
cor(as.numeric(coronavirus_trends$hits),coronavirus_sum$cases)
