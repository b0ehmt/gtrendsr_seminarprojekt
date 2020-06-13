library(gtrendsR)
library(ggplot2)
library(gridExtra)
library(dplyr)

### Dengue-Fieber in Singapur (2014–2020) ####

#Daten von Google Trends abfragen und sortieren
dengue_trends <- gtrends(keyword="dengue", geo="SG", time="2014-01-01 2019-12-31", low_search_volume=TRUE)
dengue_trends <- dengue_trends$interest_over_time[order(dengue_trends$interest_over_time$date),]

dengue_trends$hits <- ifelse(dengue_trends$hits<1,0,dengue_trends$hits)

#Dengue-Fieber Fälle in Singapur einlesen und verarbeiten
dengue <- read.csv(file = "/home/boehmt/gtrendsR_Seminarprojekt/Daten/weekly_denguecases_sg.csv",header=TRUE, sep = ",")[ ,c("epi_week", "disease", "no._of_cases")]
dengue <- dengue[order(dengue$epi_week),]

#Daten nach gewünschten Parametern filtern
dengue <- filter(dengue, dengue$disease == "Dengue Fever")

dengue$epi_week <- gsub("W", "", dengue$epi_week)
dengue$date <- as.Date(paste(dengue$epi_week, 1, sep="-"), format="%Y-%U-%u")
dengue <- filter(dengue, dengue$date >= "2014-01-01")

#Fälle normalisieren
dengue$no._of_cases <- ceiling(dengue$no._of_cases / max(dengue$no._of_cases) * 100)

#Plotten 
ggplot() + 
  geom_line(data=dengue_trends, aes(x=as.Date(date), y=hits), color="#9E5796", size=1.4 , alpha=1) +
  geom_line(data=dengue, aes(x=as.Date(date), y=no._of_cases), color="#E93835", size=1.4 , alpha=1) +
  
  labs(x="Zeit", y="Anzahl Fälle (normalisiert) bzw. Suchinteresse",
       title="Dengue-Fieber Fälle in Singapur mit Suchinteresse verglichen (2014-2020)") +
  theme_light()


#Korrelationskoeffizient
dengue_with_month <- transform(dengue,month=as.numeric(format(as.Date(dengue$date),"%m")))
dengue_with_month <- transform(dengue_with_month,year=as.numeric(format(as.Date(dengue_with_month$date),"%Y")))
dengue_monthly <- aggregate(cbind(no._of_cases)~month+year,
                            data=dengue_with_month,FUN=sum)

cor(dengue_trends$hits,dengue_monthly$no._of_cases)