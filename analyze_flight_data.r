# Name : Alireza Ghanbarzadeh


flight <- read.csv("F://wqd7004/2008.csv")

find.package('dplyr')
install.packages('dplyr')
installed.packages('dplyr') # scans the 'DESCRIPTION' files of each package
library(dplyr)
glimpse(flight)

# 1) Sort in decreasing order the ten most popular airports according to the number of origins of flights
# # (hint : use decreasing=T and indexing )
flightOrigin <- table(flight$Origin)
sort(flightOrigin, decreasing=TRUE)[1:10]

# dplyr
most_popular_10  <- flight %>% group_by(Origin)%>%
  count(sort=TRUE)
most_popular_10 [1:10,]


# 2) Assign the names of the ten most popular airports according to the number of origins of flights to 
## variable called mostPopularOrg
# # (hint : use names() and indexing)
mostPopularOrg <- names(sort(flightOrigin, decreasing=TRUE))[1:10]
mostPopularOrg

# dplyr
mostPopualr_names <- most_popular_10[1:10,] %>% 
  select(Origin)

mostPopualr_names

# 3) Assign the names of the ten most popular airports according to the number of destinations of flights to 
## variable called mostPopularDes
flightDestination <- table(flight$Dest)
mostPopularDes <- names(sort(flightDestination, decreasing=TRUE))[1:10]
mostPopularDes

# dplyr
mostPopularDest <- flight %>% group_by(Dest) %>%
  count(sort=T) 

mostPopularDest <- mostPopularDest %>% select(Dest) %>% head(10)

mostPopularDest


# 4) How many flights had their origin in one of these 10 most popular airports
## (hint : use %in%)
sum(flight$Origin %in% mostPopularDes)

# dplyr

flight %>% filter(Origin %in% pull(mostPopualr_names)) %>% nrow

# 5)How many flights had their destinationn in one of these 10 most popular airports
## (hint : use %in%)
sum(flight$Dest %in% mostPopularDes)

# dplyr

flight %>% filter(Dest %in% pull(mostPopularDest)) %>% nrow

# 6) Find flights for which the origin and the destination
# were among the 10 most popular airports
## (hint : use %in%)

sum(flight$Origin %in% mostPopularOrg & flight$Dest %in% mostPopularDes)

# dplyr

flight %>% filter(Origin %in% pull(mostPopualr_names), Dest %in% pull(mostPopularDest)) %>% nrow

# 7) For the purposes of this question, treat the group 
# of the 200 least popular airports according to the 
# number of flights having these as the origins.
# How many flights had one of these 200 least popular 
# airports as their origin?
leastPopularOrg <- sort(flightOrigin)[1:200]
sum(leastPopularOrg)

# dplyr

least_popular_airport_200  <- flight %>% group_by(Origin) %>% count(sort=T) %>% tail(n=200) %>% select(Origin)

flight %>% filter(Origin %in% pull(least_popular_airport_200)) %>% nrow


# 8) Index a vector according to the names of the elements in the vector
##8a) How many flights departed from "IND" ?
flightOrigin['IND']

# dplyr
flight %>% filter(Origin == 'IND') %>% nrow

##8b) How many flights departed from "IND","ORD","JFK","EWR","IAD" ?

flightOrigin[c('IND', 'ORD', 'JFK', 'EWR', 'IAD')]

# dplyr

flight %>% filter(Origin %in% c('IND', 'ORD', 'JFK', 'EWR', 'IAD')) %>% group_by(Origin) %>% tally()

##8c) How many flights departed from each of the 10 most popular airports ?

flightOrigin[mostPopularOrg]

# dplyr

flight  %>%  filter(Origin %in% most_popular_10) %>% group_by(Origin) %>% tally()


##8d) How many flights departed from each of the 200 least popular airports ?

flightOrigin[names(leastPopularOrg)]

# dplyr

flightsFromLeastPop <- flight %>% filter(Origin %in% pull(least_popular_airport_200)) %>%
  group_by(Origin) %>% tally(sort=T)
flightsFromLeastPop

# 8e) How many flights landed at Ronald Reagan Washington
## National ("DCA") or Washington Dulles Airport ("IAD") in 2008? 

flightDestination[c('DCA', 'IAD')]

# dplyr

flight %>% filter(Dest %in% c('DCA', 'IAD')) %>% group_by(Dest) %>% tally

# 9)Check the first 20 flights and see which one departed on time or early
first20 <- head(flight, 20)
first20DepOnTimeEarly <- subset(first20, first20$DepDelay <= 0); first20DepOnTimeEarly
nrow(first20DepOnTimeEarly)

# dplyr

flight %>%   slice(1:20) %>% filter(DepDelay <= 0)  %>%  nrow

##9a) Restrict attention to only the 10 most popular airports 
##and see which one departed on time or early

mostPopDepOnTimeEarly <- tapply(flight$DepDelay <= 0, flight$Origin, sum, na.rm=TRUE)[mostPopularOrg]
mostPopDepOnTimeEarly
# dplyr


early_depart_pop10 <- flight %>% filter(Origin %in% pull(mostPopualr_names), DepDelay <=0 ) %>% 
  group_by(Origin) %>% tally() 

early_depart_pop10

##9b)Find the percentage of flights at each of the 10 most popular 
# airports that departed on time or early

mostPopDepOnTimeEarly / flightOrigin[mostPopularOrg]  * 100
# dplyr

flights_most_popular <- flight %>% filter(Origin %in% pull(mostPopualr_names)) %>% group_by(Origin) %>% tally ; flights_most_popular

flights_most_popular %>% mutate(EarlyFlightPct = early_depart_pop10$n / n * 100) %>% select(Origin, EarlyFlightPct)



# 9c) What percentage of flights departed from IND on time or early?

sum(flight$Origin=='IND' & flight$DepDelay <= 0, na.rm=TRUE) / flightOrigin['IND'] * 100

# dplyr


early_flight_IND <- flight %>% filter(Origin == "IND" ,DepDelay <=0 ) %>% tally

total_IND_flights <- flight %>%  filter(Origin == "IND") %>% tally

IND_tota_perct <- early_flight_IND / total_IND_flights *100
IND_tota_perct




#10) Analyze Flights by Origin Airport and Month of Departure
##10a) Break the data in the DepDelay vector according to which city of origin 
depOrg <- tapply(flight$DepDelay, flight$Origin, length)
head(depOrg)

# dplyr

flight %>% select(DepDelay,Origin) %>% group_by(Origin) %>%  tally()

##10b) Break the data in the DepDelay vector according to month
depMonth <- tapply(flight$DepDelay, flight$Month, length)
head(depMonth)

# dplyr

flight %>% select(DepDelay, Month) %>% group_by(Month) %>% tally



#11) How many flights delay occur from each airport in each month ?
tapply(flight$DepDelay > 0, list(flight$Origin, flight$Month), sum, na.rm=T)

# dplyr

flight %>% select(DepDelay, Month, Origin) %>%
  filter(DepDelay > 0) %>% group_by(Origin, Month) %>%
  tally

##11a) Extract the data from origin airport = "IND"
# and from the month of June
tapply(flight$DepDelay > 0, list(flight$Origin, flight$Month), sum, na.rm=T)['IND', 6]

# dplyr


flight %>%  filter(Origin == "IND",DepDelay >0, Month == 6 ) %>% tally

##11b) Extract the data from origin airport = "ATL"
# and from the month of March
tapply(flight$DepDelay > 0, list(flight$Origin, flight$Month), sum, na.rm=T)['ATL',3]

# dplyr
flight %>% filter(DepDelay > 0, Origin=='ATL', Month==3) %>% tally

flight %>% filter(Origin == "ATL" , Month == 3) %>% tally


# 11c) The number of flights delay from 3 airports = "ATL","AUS","BDL"
# during the months of July through October
flightDelay3airport <- tapply(flight$DepDelay > 0, list(flight$Origin, flight$Month), sum, na.rm=T)[c('ATL', 'AUS', 'BDL'), 7:10]
flightDelay3airport

# dplyr


dely_from_3_airport <- flight %>% 
  filter(Origin %in% c('ATL','AUS','BDL'), Month %in% c(7:10),DepDelay >0) %>%
  group_by(Month,Origin) %>% tally

dely_from_3_airport

 

# 11d) How many delayed departure flights altogether from ATL, AUS, and BDL during the months of 
#July 2008 through October 2008?
sum(flightDelay3airport)
colSums(flightDelay3airport)
rowSums(flightDelay3airport)

# dplyr
sum(dely_from_3_airport$n)

# 11e) All the flight delays, month by month, frm IND airport
tapply(flight$DepDelay > 0, list(flight$Origin, flight$Month), sum, na.rm=T)['IND', ]

# dply

flight %>% filter(Origin=='IND', DepDelay > 0) %>% group_by(Month) %>% tally

# 11f) All the flight delays, month by month, frm both IND and ORD at once
flightDelayIndOrd <- tapply(flight$DepDelay > 0, list(flight$Origin, flight$Month), sum, na.rm=T)[c('IND', 'ORD'),]
flightDelayIndOrd

# dplyr


all_delay_INDORD <- flight %>% filter(Origin %in% c("IND","ORD"), DepDelay >0) %>% group_by(Month,Origin) %>% tally

all_delay_INDORD


# 12) Calculating Percentages of Flights with delayed more than 30 minutes when departing

moreThan30min <- subset(flight, flight$DepDelay > 30)
delayedFlight <- tapply(moreThan30min$DepDelay, list(moreThan30min$Origin, moreThan30min$Month), length)[c('IND', 'ORD'),]
pct <- delayedFlight / flightDelayIndOrd * 100
pct


# dplyr

#1 extracting flights wih origin IND and ORD and departure delay more than 30 minutes 
delay_INDORD_30 <- flight %>% filter(DepDelay > 30, Origin %in% c('IND', 'ORD')) %>% group_by(Origin, Month) %>% tally

#2 converrting results to  a data frame 
delay_INDORD_30 <- as.data.frame(delay_INDORD_30)

#3 filtering by departure delay > 0 for calculating total delay
total_delayed <- flight %>% filter(DepDelay > 0, Origin %in% c('IND', 'ORD')) %>% group_by(Origin, Month) %>% tally

#4 converting total delay to data frame 
total_delayed <-  as.data.frame(total_delayed)

#5 create a new column by mutation
total_delayed <- total_delayed %>% mutate(late_pct = delay_INDORD_30$n / n * 100) %>% select(Origin, Month, late_pct)

total_delayed


# 12a) find the percentage of flights with long delays and plot with dotchart()
dotchart(pct)

# dplyr

dotchart(total_Delay$late_pct, total_Delay$Origin, total_Delay$Month, main = "Mont by Month percentage of late flights",
         xlab="Late flights pct", ylab="Origin/Months")

# 12b) How many flights departed altogether from IND 
# or ORD in 2008 with a delay of more than 30 minutes each?
sum(delayedFlight)

# dplyr

flight %>% filter(DepDelay > 30, Origin %in% c('IND', 'ORD')) %>% nrow

#12c) In which month of 2008 was the percentage of long delays 
#(i.e., flights with more than 30 minute delays) the highest?
delayPerMonth <- tapply(flight$DepDelay > 30, flight$Month, sum, na.rm=T)
delayPerMonth
total_flightsPerMonth <- tapply(flight$Month, flight$Month, length)
total_flightsPerMonth
delayPctPerMonth <- delayPerMonth / total_flightsPerMonth * 100
delayPctPerMonth
names(which.max(delayPctPerMonth))

# dplyr

#1 filter flights by dparture delay >30 and group them by motnh 
long_dealy <- flight %>% filter(DepDelay > 30) %>% group_by(Month) %>% tally

#2 convert to long delayed results to data frame format 
long_dealy <- as.data.frame(long_dealy); long_dealy

#3 group all flights by month 
total_flights <- flight %>% group_by(Month) %>% tally

#4 convert total flights result to data frame format 
total_flights <- as.data.frame(total_flights); total_flights

#5 creare late_pct column by using mutate function to calcualte perecntage of long delays
long_dealy <- long_dealy %>% mutate(late_pct = n / total_flights$n * 100) 

which.max(long_dealy$late_pct)


# 13) Analyzing Flights by Time of Day for Departure
# Break the day into 4 parts:
# early morning (1) correspond to the times to 6 am
# late morning (2) correspond to the times to 6 am to 12 noon
# early evening (3) correspond to the times to 12 noon to 6 pm
# late evening (4) correspond to the times to 6 pm to 12 midnight
v<-ceiling(flight$DepTime/600)
# dplyr

# build a vector called parts of the day
partsofday <- rep(NA, times=dim(flight)[1])
partsofday
partsofday[v==1]<-"early morning"
partsofday[v==2]<-"late morning"
partsofday[v==3]<-"early evening"
partsofday[v==4]<-"late evening"
table(partsofday)

# dplyr

# and we can create a new column in the flight data frame called "timeofday"
# and we can store this information we just found into this column

flight$timeofday <- partsofday
dim(flight)

# dplyr
flight <- flight %>% mutate(timeofday = partsofday)

dim(flight)

# just check to make sure that the first 6 flights were done properly
head(flight$timeofday)

head(flight$DepTime)

# dplyr

flight %>% select(timeofday) %>% slice(1:6)

flight %>% select(DepTime) %>% slice(1:6)


# 13a) How many flights departed from IND early in the morning?

sum(flight$Origin=='IND' & flight$timeofday=='early morning', na.rm = TRUE)

# dplyr

flight %>% filter(Origin=='IND', timeofday == 'early morning') %>% nrow

# 13b) Tabulate how many flights occur, by splitting the flights according to
# both the city of origin and also the time of the day when the flight departed
tapply(flight$DepDelay, list(flight$Origin, flight$timeofday), length)

# dplyr

flight %>% group_by (Origin, timeofday) %>% tally

