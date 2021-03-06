breweries = read.csv('/Users/spencerfogelman/Downloads/CaseStudy1_2_2/Breweries.csv', stringsAsFactors = FALSE)
beers = read.csv('/Users/spencerfogelman/Downloads/CaseStudy1_2_2/Beers.csv', stringsAsFactors = FALSE)
#Question 1
library(dplyr)
head(breweries)
head(beers)
breweries %>% group_by(State) %>% summarise(Total = n()) 


#Question 2
merged = merge(x = breweries, y = beers, by.x = 'Brew_ID', by.y= 'Brewery_id', all = TRUE)
head(merged, 6)
tail(merged, 6)

#Question 3
for (i in 1:length(names(merged))){
  column = merged[,i]
  nas = sum(is.na(column))
  print(paste(names(merged)[i], ':', nas))
}

#Question 4
merged %>% group_by(State) %>% summarise(MedianAlcoholContent = median(ABV, na.rm=TRUE),
                                         MedianIBU = median(IBU, na.rm=TRUE))

#Question 5
maxABV = max(merged$ABV, na.rm=TRUE)
maxABV
maxIBU = max(merged$IBU, na.rm=TRUE)
maxIBU

merged %>% filter(ABV == maxABV) %>% select(State)
merged %>% filter(IBU == maxIBU) %>% select(State)
#Question 6
merged %>% select(ABV) %>% summary()
#Question 7
library(ggplot2)
ggplot(merged, aes(x=ABV, y=IBU)) + geom_point(colour = "blue", size = 0.8, na.rm = TRUE) + labs(title='Alcohol Content vs Bitterness') +
  theme(plot.title = element_text(hjust = 0.5))




