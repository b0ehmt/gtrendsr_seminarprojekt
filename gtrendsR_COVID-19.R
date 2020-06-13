library(gtrendsR)
library(ggplot2)
library(gridExtra)
library(dplyr)

### COVID-19 weltweit (Zeitraum Januar–Juni 2020) ###

#Daten von Google Trends abfragen und sortieren
coronavirus_trends <- gtrends(keyword="Coronavirus", time="2019-12-31 2020-05-15", low_search_volume=TRUE)
coronavirus_trends <- coronavirus_trends$interest_over_time[order(coronavirus_trends$interest_over_time$date),]

coronavirus_trends$hits <- ifelse(coronavirus_trends$hits<1,0,coronavirus_trends$hits)

#COVID-19 Fälle einlesen und verarbeiten
coronavirus <- read.csv(file = "/home/boehmt/gtrendsR_Seminarprojekt/Daten/covid-19_cases.csv",header=TRUE, sep = ",")[ ,c("year", "month", "day", "geoId", "cases")]
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
  
  labs(x="Zeit", y="Anzahl Fälle (normalisiert) bzw. Suchinteresse", 
       title="COVID-19 Fälle weltweit mit Suchinteresse verglichen (2020)") +
  theme_light()


#Korrelationskoeffizient
cor(as.numeric(coronavirus_trends$hits),coronavirus_sum$cases)
