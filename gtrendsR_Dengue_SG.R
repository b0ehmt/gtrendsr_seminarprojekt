library(gtrendsR)
library(ggplot2)
library(gridExtra)
library(dplyr)

### Dengue-Fieber in Singapur (2014-2020) ####

#Daten von Google Trends abfragen und sortieren
dengue_trends <- gtrends(keyword="dengue", geo="SG", time="2014-01-01 2019-12-31", low_search_volume=TRUE)
dengue_trends <- dengue_trends$interest_over_time[order(dengue_trends$interest_over_time$date),]

dengue_trends$hits <- ifelse(dengue_trends$hits<1,0,dengue_trends$hits)

#Plotten des Suchinteresses in Singapur
date <- dengue_trends$date
hits <- as.numeric(dengue_trends$hits)
data <- data.frame(date, hits)

dengue1 <- ggplot(data, aes(x=date, y=hits)) +
  geom_line(color="#770058", size=1.3, alpha=0.9)+
  ggtitle('Suchinteressse von "dengue" (2014-2020)') +
  theme_light()

##

#Dengue-Fieber Fälle in Singapur einlesen und verarbeiten
dengue <- read.csv(file = "Seminarprojekt_gTrendsR/Daten/weekly_denguecases_sg.csv",header=TRUE, sep = ",")[ ,c("epi_week", "disease", "no._of_cases")]
dengue <- dengue[order(dengue$epi_week),]

#Daten nach gewünschten Parametern filtern
dengue <- filter(dengue, dengue$disease == "Dengue Fever")

dengue$epi_week <- gsub("W", "", dengue$epi_week)
dengue$date <- as.Date(paste(dengue$epi_week, 1, sep="-"), format="%Y-%U-%u")
dengue <- filter(dengue, dengue$date >= "2014-01-01")

#Plotten der Dengue-Fieber Fälle in Singapur
date <- dengue$date
cases <- dengue$no._of_cases
data <- data.frame(date, cases)

dengue2 <- ggplot(data, aes(x=date, y=cases)) +
  geom_line(color="#770058", size=1.3, alpha=0.9) +
  ggtitle("Dengue-Fieber Fälle in Singapur (2014-2020)") +
  theme_light()

#Ansicht der Plots
grid.arrange(dengue1, dengue2)



### Dengue-Fieber in Singapur, normalisiert (2014-2020) ####

#Daten von Google Trends abfragen und sortieren
dengue_trends <- gtrends(keyword="dengue", geo="SG", time="2014-01-01 2019-12-31", low_search_volume=TRUE)
dengue_trends <- dengue_trends$interest_over_time[order(dengue_trends$interest_over_time$date),]

dengue_trends$hits <- ifelse(dengue_trends$hits<1,0,dengue_trends$hits)

#Dengue-Fieber Fälle in Singapur einlesen und verarbeiten
dengue <- read.csv(file = "/home/boehmt/Seminarprojekt_gTrendsR/Daten/weekly_denguecases_sg.csv",header=TRUE, sep = ",")[ ,c("epi_week", "disease", "no._of_cases")]
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
  
  labs(x="Zeit", y="Anzahl Fälle (normalisiert) bzw. Suchinteresse", title="Dengue-Fieber Fälle in Singapur mit Suchinteresse verglichen (2014-2020)") +
  theme_light()


#Korrelationskoeffizient
dengue_with_month <- transform(dengue,month=as.numeric(format(as.Date(dengue$date),"%m")))
dengue_with_month <- transform(dengue_with_month,year=as.numeric(format(as.Date(dengue_with_month$date),"%Y")))
dengue_monthly <- aggregate(cbind(no._of_cases)~month+year,
                            data=dengue_with_month,FUN=sum)

cor(dengue_trends$hits,dengue_monthly$no._of_cases)
