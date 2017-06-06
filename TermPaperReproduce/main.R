set.seed(30)
require(tsne)
require(dtw)
require(dplyr)
require(tidyr)
require(data.table)
require(xlsx)
require(ggplot2)
require(ggdendro)
require(lattice)
require(latticeExtra)
require(AER)
require(e1071)
require(randomForest)
require(caret)
require(googleVis)
require(ggmap)
require(stargazer)

## Data Cleansing
## Coffee data, 1996-2013, Bergium and Lussenburg deleted
coffee <- read.csv('other_data/Coffee.csv', stringsAsFactor=F)
coffee <- coffee[-c(2,3,19),-(c(2:7, 26:27))]
coffee <- data.frame(coffee[,1], apply(coffee[,-1], 2, function(x) as.numeric(gsub(',', '', x))))
colnames(coffee)[1] = 'country'
## Convert to panel data
coffee <- melt(coffee[-34,], id.vars = 'country', variable.name = 'year', value.name = 'coffee')
coffee$year <- as.numeric(gsub('X','',coffee$year))
## Change coding from USA to United States
coffee$country <- as.character(coffee$country)
coffee$country[which(coffee$country=='USA')] <- 'United States'
## Change the unit of coffee consumption to kg
coffee$coffee <- coffee$coffee * 60 * 1000

## Price Data
price <- read.csv('other_data/Price.csv', stringsAsFactor=F)
price <- price[,-(c(2:7, 26:27))]
price <- data.frame(price[,1], apply(price[,-1], 2, function(x) as.numeric(gsub(',', '', x))))
colnames(price)[1] = 'country'
## Convert to Panel Data
price <- melt(price, id.vars = 'country', variable.name = 'year', value.name = 'price')
price$year <- as.numeric(gsub('X','',price$year))

## Population Data
Pop <- read.csv('other_data/Population.csv', skip = 3, header = T)
Pop <- Pop[,-c(2,3,4,which(colnames(Pop)=='X1960'):which(colnames(Pop)=='X1995'))]
Pop <- melt(Pop, id.vars = 'Country.Name', variable.name = 'year', value.name = 'Pop')
Pop$year <- as.numeric(gsub('X','',Pop$year))
colnames(Pop)[1] <- 'country'
Pop$country <- as.character(Pop$country)
Pop$country[which(Pop$country=='Slovak Republic')] <- 'Slovakia'
## GDP Data
GDP <- read.csv('other_data/GDP.csv', skip = 3, header = T)
GDP <- GDP[,-c(2,3,4,which(colnames(GDP)=='X1960'):which(colnames(GDP)=='X1995'))]
GDP <- melt(GDP, id.vars = 'Country.Name', variable.name = 'year', value.name = 'GDP')
GDP$year <- as.numeric(gsub('X','',GDP$year))
colnames(GDP)[1] <- 'country'
GDP$country <- as.character(GDP$country)
GDP$country[which(GDP$country=='Slovak Republic')] <- 'Slovakia'
## Merge with Coffee Data
Dat <- left_join(coffee, Pop, by = c('country'='country','year'='year'))
Dat <- mutate(Dat, coffee_percap = coffee/Pop)
## Merge with Coffee Data
Dat <- left_join(Dat, GDP, by = c('country'='country','year'='year'))
Dat <- mutate(Dat, GDP_percap = GDP/Pop)
## Merge with Price Data
price$country <- as.character(price$country)
price$country <- as.character(gsub('   ','',price$country))
Dat <- left_join(Dat, price, by = c('country'='country','year'='year'))

## Citation Data
path <- 'Citation_data/'
files <- dir(path)
Citation <- data.frame()
for (i in files){
  temp <- read.xlsx(paste0(path,i), 1) %>%
    mutate(year = 1995 + which(files==i))
  temp$Country <- as.character(temp$Country)
  Citation <- bind_rows(Citation, temp)
}
Dat <- left_join(Dat, Citation, by = c('country'='Country','year'='year')) %>%
  mutate(doc_percap = Documents/Pop)
Dat <- Dat %>%
  group_by(country) %>%
  mutate(scaled_coffee = as.vector(scale(coffee_percap)),
         scaled_doc = as.vector(scale(doc_percap)),
         scaled_gdp = as.vector(scale(GDP_percap)),
         scaled_cite_perdoc = as.vector(scale(Citations.per.document)),
         scaled_price = as.vector(scale(price))) %>% ungroup()


## Output
plot1 <- xyplot(scaled_coffee ~ year | country, Dat, type = 'l', ylim = c(-3,3),
                ylab = 'Normalized Time Trend', col = 5, xlab = 'Year',
                key=list(space="top",
                         lines=list(col=c(5,6)),
                         text=list(c("Coffee Consumption Percap","Documents Percap"))
                ))
plot2 <- xyplot(scaled_doc ~ year | country, Dat, type = 'l', col = 6, ylim = c(-3,3), xlab = 'Year')
plot1+plot2
#plot3 <- xyplot(scaled_price ~ year | country, Dat, type = 'l', col = 3, ylim = c(-3,3))
#png(filename="/Users/home/Documents/Workspace/Github/Coffee/Plots/5beforeREG.png")
#xyplot(scaled_coffee ~ scaled_price | country, Dat, ylim = c(-3,3))

xyplot(H.index ~ year | country, Dat)
## Map
Dat_Map <- Dat %>% group_by(country) %>% summarise(avg = mean(coffee_percap))
geoMap <- gvisGeoChart(Dat_Map, locationvar="country", colorvar="avg",
                       options=list(dataMode="regions"))
plot(geoMap)


## Clustering on Consumption Level
tmp <- left_join(coffee, Pop, by = c('country'='country','year'='year'))
tmp <- mutate(tmp, coffee_percap = coffee/Pop)
tmp <- spread(tmp[,-c(3,4)], year, coffee_percap)
tmp2 <- tmp
rownames(tmp2) <- tmp2$country
distMatrix <- dist(tmp2[,-1], method='DTW')
hc <- hclust(distMatrix, method='average')
G1 <- c("Turkey", "Russian Federation")
G2 <- c("Finland")
G3 <- c("Switzerland","Austria","Sweden","Germany", "Netherlands", "Norway","Denmark")
G4 <- c("United States","Portugal","Spain","Cyprus","Estonia","Slovenia","Italy","France","Greece","Croatia")
labelCol <- function(x) {
  if (is.leaf(x)) {
    ## fetch label
    label <- attr(x, "label") 
    ## set label color to red for A and B, to blue otherwise
    attr(x, "nodePar") <- list(lab.col=ifelse(label %in% G1, "red",
                                              ifelse(label %in% G2, "blue", 
                                                     ifelse(label %in% G3, "orange", 
                                                            ifelse(label %in% G4, "darkgreen", "purple") ))))
  }
  return(x)
}
d <- dendrapply(as.dendrogram(hc), labelCol)
#png(filename="/Users/home/Documents/Workspace/Github/Coffee/Plots/3cluster1.png")
plot(d, horiz=T)


## GGPLOT
ggplot(coffee, aes(x = year, y = coffee, colour = country)) + 
  geom_line(alpha = 0.6) + 
  labs(x="Year", y="Coffee Consumption")
#ggsave("/Users/home/Documents/Workspace/Github/Coffee/Plots/1coffee.png",
#width = 8.04, height = 5, units = 'in')
ggplot(Dat, aes(x = year, y = coffee_percap, colour = country)) + 
  geom_line(alpha = 0.6) + 
  labs(x="Year", y="Coffee Consumption Per Cap")
#ggsave("/Users/home/Documents/Workspace/Github/Coffee/Plots/2coffee_percap.png",
#width = 8.04, height = 5, units = 'in', dpi = 300)
#ggplot(Dat, aes(x = year, y = GDP_percap, colour = country)) + geom_line(alpha = 0.6)
#ggplot(Dat, aes(x = scaled_coffee, y = scaled_price, colour = country)) + geom_line(alpha = 0.6)


## Latitude
Dat$latitude <- rep(geocode(unique(Dat$country))$lat,18)


## ML
RF1 <- randomForest(log(coffee_percap) ~ . - coffee,
                    data = as.data.frame(Dat[,-c(1,2,17:21)]),
                    importance=T, na.action = na.omit, ntree=2500)
print(RF1)
importance(RF1, scale = T)
varImpPlot(RF1,type=2, main="Variable Importance")


## Regresions
## Simple Linear Regression
LR_Base <- lm(log(coffee_percap) ~ log(GDP_percap) + log(doc_percap) + log(price) + log(Pop) +
                latitude, data = Dat)
summary(LR_Base)
vif(LR_Base)
## SLR w/ Control
LR_Control <- lm(log(coffee_percap) ~ log(GDP_percap) + log(doc_percap)
                 + log(price) + log(Pop) +
                   latitude + as.factor(year), data = Dat)
summary(LR_Control)
vif(LR_Control)


## Trend and Lag
Dat2 <- (as.matrix(Dat[-c(1:33),-c(1,2,15,22)]) - as.matrix(Dat[-c(562:594),-c(1,2,15,22)]))/Dat[-c(562:594),-c(1,2,15,22)]
Dat3 <- data.frame(Dat[c(34:594), c(1,2)], Dat2, Dat[c(34:594), c(15,22)])
coffee_lag1 <- Dat2$coffee_percap[-c(1:33,529:561)]
coffee_lag2 <- Dat2$coffee_percap[-c(496:561)]
Dat4 <- data.frame(Dat3[-c(1:66),], coffee_lag1, coffee_lag2)
LR_LAG_Base <- lm(doc_percap ~ GDP_percap + coffee_percap + price + Pop +
                    + coffee_lag1 + coffee_lag2 + latitude, data = Dat4)
summary(LR_LAG_Base)
LR_LAG_Control <- lm(doc_percap ~ GDP_percap + coffee_percap + price + Pop +
                       + coffee_lag1 + coffee_lag2 + latitude + as.factor(year), data = Dat4)
summary(LR_LAG_Control)
xyplot()
# plot01 <- xyplot(doc_percap ~ coffee_lag2| country, Dat4, type = 'p',
#                  ylab = 'Percentage change of per-cap academic publication at time t', col = 1, xlab = 'Percentage change of per-cap coffee consumption at time t-2')
# plot02 <- xyplot(doc_percap ~ coffee_lag2| country, Dat4, type = 'r', col=2)
# plot01+plot02

## FD estimator 1
Dat_FD <- (log(as.matrix(Dat[-c(1:33),-c(1,2)])) - as.matrix(log(Dat[-c(562:594),-c(1,2)])))
Dat_FD <- data.frame(Dat[c(34:594), c(1,2)], Dat_FD)
LR_FD <- lm(coffee_percap ~ GDP_percap + doc_percap + price + Pop, Dat_FD)
summary(LR_FD)

Dat_FD <- (log(as.matrix(Dat[-c(1:33),-c(1,2)])) - as.matrix(log(Dat[-c(562:594),-c(1,2)])))
Dat_FD <- apply(Dat_FD, 2, exp)
Dat_FD <- data.frame(Dat[c(34:594), c(1,2)], Dat_FD)
LR_FD <- lm(log(coffee_percap) ~ log(GDP_percap) + log(doc_percap) + log(price) + log(Pop), Dat_FD)
summary(LR_FD)


## FD estimator 2
Dat_LAG_FD <- as.matrix(Dat4[-c(1:33),-c(1,2)]) - as.matrix(Dat4[-c(463:495),-c(1,2)])
Dat_LAG_FD <- data.frame(Dat[c(34:495), c(1,2)], Dat_LAG_FD)
LR_LAG_FD <- lm(doc_percap ~ GDP_percap + coffee_percap + price + Pop +
                  + coffee_lag1 + coffee_lag2, Dat_LAG_FD)
summary(LR_LAG_FD)
vif(LR_LAG_FD)


## Tables
LR_Base
LR_Control
LR_FD
LR_LAG_Base
LR_LAG_Control
LR_LAG_FD

stargazer(LR_Base, LR_Control, LR_FD,
          title="Per-capita Coffee Consumption ", omit.stat=c("f","ser"), align=T, no.space=T,
          omit = 'as.factor',
          column.labels = c('OLS', 'OLS with year control', 'FD'))

stargazer(LR_LAG_Base, LR_LAG_Control, LR_LAG_FD,
          title="Co-movements of Per-capita Coffee Consumption and Per-capita Academic Publication", omit.stat=c("f","ser"), align=T, no.space=T,
          omit = 'as.factor',
          column.labels = c('OLS', 'OLS with year control', 'FD'))
