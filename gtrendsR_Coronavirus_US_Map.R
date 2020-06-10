library(gtrendsR)
library(ggplot2)
library(dplyr)
library(maps)

### Suchinteresse von "Coronavirus" in den USA, mapped (2020) ###

#Daten von Google Trends abfragen und sortieren
coronavirus_trends <- gtrends(keyword="Coronavirus", geo= "US", time="2019-12-31 2020-05-15", low_search_volume=TRUE)

state <- map_data("state")

coronavirus_trends$interest_by_region %>%
  mutate(region = tolower(location)) %>%
  filter(region %in% state$region) %>%
  select(region, hits) -> map

ggplot() +
  geom_map(data = state,
           map = state,
           aes(x = long, y = lat, map_id = region),
           fill="#ffffff", color="#ffffff", size=0.15) +
  geom_map(data = map,
           map = state,
           aes(fill = hits, map_id = region),
           color="#ffffff", size=0.15) +
  scale_fill_continuous(low = 'grey', high = 'purple') +
  ggtitle('Suchinteressse von "Coronavirus" in den USA (Januar-Mai 2020)') +
  theme(panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

