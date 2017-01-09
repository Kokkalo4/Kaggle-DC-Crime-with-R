#ABOUT The data
#Dataset of all of the crimes in the DC metro police system ranging from Theft, Arson, Assault, Homicide, Sex Abuse, Robbery, and Burglary.

#loading of libraries
library(readr)
library(plyr)
library(Hmisc)
library(dplyr)
library(GGally)
library(readr)
library(ggplot2)
library(tidyr)
library(grid)
library(gridExtra)

#load file 
crimedb <- read.csv("c:/users/alex/desktop/r/exercise data/done/crime/crime_homicide_subset.csv" , header = TRUE)

#str
str(crimedb)
describe(crimedb)
summary(crimedb)
head(crimedb)
tail(crimedb)
glimpse(crimedb)

#describe function gives us a very good view of the data set. 

#CLEANING THE DATA.
#since as we can see from the describe function there are no NAs in the dataset we will clean the data from columns we wont be needing for our analysis.

crimedb <- crimedb %>%
  select(-BLOCK_GROUP, -CENSUS_TRACT, -CCN, -XBLOCK, -YBLOCK, -lat, -long)
str(crimedb)

#basic outlook of the data.
#how many unique offenses and methods are there in our dataset.

unique(crimedb$METHOD)
unique(crimedb$OFFENSE)
unique(crimedb$SHIFT)
unique(crimedb$BLOCK)
unique(crimedb$DISTRICT)

#making plots with full dataset 

#plotting offences in all districts
pie(as.numeric(unique(crimedb$OFFENSE[crimedb$DISTRICT])), main = "Pie chart of Offenses in all Districts", 
     labels = c("70% Sexual Abuse", "30% Homicide"))  

#plotting accuracy
pie(as.numeric(unique(crimedb$accuracy)), main = "Pie chart of accuracy", 
    labels = c("Street address", "Route","Premise","Intersection"))

#plotting count of offenses per year
plot(crimedb$OFFENSE[crimedb$year==2011] , main = "Graph of Frequency of Crimes in 2011" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 250))
plot(crimedb$OFFENSE[crimedb$year==2012] , main = "Graph of Frequency of Crimes in 2012" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 250))
plot(crimedb$OFFENSE[crimedb$year==2013] , main = "Graph of Frequency of Crimes in 2013" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 400))
plot(crimedb$OFFENSE[crimedb$year==2014] , main = "Graph of Frequency of Crimes in 2014" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 400))
plot(crimedb$OFFENSE[crimedb$year==2015] , main = "Graph of Frequency of Crimes in 2015" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 300))
plot(crimedb$OFFENSE[crimedb$year==2016] , main = "Graph of Frequency of Crimes in 2016" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 150))

#plotting offense per SHIFT

plot(crimedb$OFFENSE[crimedb$SHIFT == "DAY"] , main = "Graph of Frequency of Crimes in Day Shift" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 500))
plot(crimedb$OFFENSE[crimedb$SHIFT == "EVENING"] , main = "Graph of Frequency of Crimes in Evening Shift" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 500))
plot(crimedb$OFFENSE[crimedb$SHIFT == "MIDNIGHT"] , main = "Graph of Frequency of Crimes in Midnight Shift" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 700))

#plot method per SHIFT

plot(crimedb$METHOD[crimedb$SHIFT == "DAY"] , main = "Graph of Frequency of Method in Day Shift" , xlab = "Weapon" , ylab = "Frequency" , ylim = c(0, 500))
plot(crimedb$METHOD[crimedb$SHIFT == "EVENING"] , main = "Graph of Frequency of Method in Evening Shift" , xlab = "Weapon" , ylab = "Frequency" , ylim = c(0, 500))
plot(crimedb$METHOD[crimedb$SHIFT == "MIDNIGHT"] , main = "Graph of Frequency of Method in Midnight Shift" , xlab = "Weapon" , ylab = "Frequency" , ylim = c(0, 700))

#plot offence per district

plot(crimedb$OFFENSE[crimedb$DISTRICT == 1] , main = "Graph of Frequency of Crimes in District 1" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 500))
plot(crimedb$OFFENSE[crimedb$DISTRICT == 2] , main = "Graph of Frequency of Crimes in District 2" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 500))
plot(crimedb$OFFENSE[crimedb$DISTRICT == 3] , main = "Graph of Frequency of Crimes in District 3" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 700))
plot(crimedb$OFFENSE[crimedb$DISTRICT == 4] , main = "Graph of Frequency of Crimes in District 4" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 500))
plot(crimedb$OFFENSE[crimedb$DISTRICT == 5] , main = "Graph of Frequency of Crimes in District 5" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 500))
plot(crimedb$OFFENSE[crimedb$DISTRICT == 6] , main = "Graph of Frequency of Crimes in District 6" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 700))
plot(crimedb$OFFENSE[crimedb$DISTRICT == 7] , main = "Graph of Frequency of Crimes in District 7" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 700))


#plot method per district

plot(crimedb$METHOD[crimedb$DISTRICT == 1] , main = "Graph of Frequency of Method of Crimes in District 1" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 500))
plot(crimedb$METHOD[crimedb$DISTRICT == 2] , main = "Graph of Frequency of Method of Crimes in District 2" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 500))
plot(crimedb$METHOD[crimedb$DISTRICT == 3] , main = "Graph of Frequency of Method of Crimes in District 3" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 700))
plot(crimedb$METHOD[crimedb$DISTRICT == 4] , main = "Graph of Frequency of Method of Crimes in District 4" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 500))
plot(crimedb$METHOD[crimedb$DISTRICT == 5] , main = "Graph of Frequency of Method of Crimes in District 5" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 500))
plot(crimedb$METHOD[crimedb$DISTRICT == 6] , main = "Graph of Frequency of Method of Crimes in District 6" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 700))
plot(crimedb$METHOD[crimedb$DISTRICT == 7] , main = "Graph of Frequency of Method of Crimes in District 7" , xlab = "Crime" , ylab = "Frequency" , ylim = c(0, 700))

#plot offence per month
plot(crimedb$OFFENSE, crimedb$mont)

#plot method per month
plot(crimedb$METHOD, crimedb$mont)

plot(crimedb$OFFENSE, crimedb$NEIGHBORHOOD_CLUSTER)

# count of offense per hour of day

ggplot(crimedb, aes(crimedb$hour)) + geom_bar(aes(fill = factor(hour))) + xlab("Hours of Day") + ylab("Number of Offences") + 
  guides(fill=guide_legend(title="Hour"))

#crime per district

ggplot(crimedb, aes(crimedb$DISTRICT )) + geom_bar(aes(fill = factor(DISTRICT))) + xlab("District") + ylab("Number of Offences") + 
  guides(fill=guide_legend(title="District"))

#crime per Neighborhood Cluster
ggplot(crimedb, aes(crimedb$NEIGHBORHOOD_CLUSTER )) + geom_bar(aes(fill = factor(NEIGHBORHOOD_CLUSTER))) + xlab("Neighborhood Cluster") + ylab("Number of Offences") +
   theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())  + 
  guides(fill=guide_legend(title="Neighborhood Cluster"))

#crime per precinct
ggplot(crimedb, aes(crimedb$VOTING_PRECINCT )) + geom_bar(aes(fill = factor(VOTING_PRECINCT))) + xlab("Voting Precinct") + ylab("Number of Offences") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())  + 
  guides(fill=guide_legend(title="Voting Precinct"))

#number of crimes per month
ggplot(crimedb, aes(crimedb$mont)) + geom_bar(aes(fill = factor(crimedb$mont))) + xlab("Month") + ylab("Number of Offences") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())  + 
  guides(fill=guide_legend(title="Month"))




library(ggmap)

crimehom <- subset(crimedb, OFFENSE == "HOMICIDE")
mapgilbert <- get_map(location = c(lon = mean(crimehom$long), lat = mean(crimehom$lat)), zoom = 12,
                      maptype = "terrain", scale = 2)
ggmap(mapgilbert) +
  geom_point(data = crimehom, aes(x = long, y = lat, fill = "red", alpha = 0.8), size = 2, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

crimesex <- subset(crimedb, OFFENSE == "SEX ABUSE")
mapgilbert2 <- get_map(location = c(lon = mean(crimehom$long), lat = mean(crimehom$lat)), zoom = 12,
                      maptype = "terrain", scale = 2)
ggmap(mapgilbert2) +
  geom_point(data = crimesex, aes(x = long, y = lat, fill = "red", alpha = 0.8), size = 1, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

#full data
mapgilbert3 <- get_map(location = c(lon = mean(crimedb$long), lat = mean(crimedb$lat)), zoom = 12,
                       maptype = "terrain", scale = 2)
ggmap(mapgilbert2) +
  geom_point(data = crimedb, aes(x = long, y = lat, fill = factor(OFFENSE), alpha = 0.8), size = 3, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)


#multi plot
#copying the function as found on
#http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
     plots <- c(list(...), plotlist)
    numPlots = length(plots)
      if (is.null(layout)) {
     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }
    if (numPlots==1) {
    print(plots[[1]])
      } else {
        grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
            for (i in 1:numPlots) {
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
            print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(p1,p2,p3,p4)


