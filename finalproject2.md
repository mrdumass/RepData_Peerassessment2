Analysis of Severe Storms in the United States
========================================================

## Synopsis ##
Using the data from the Storm Data from NOAA I use the data to determine which types of storms are the most harmful to population health and which types of storms have the greatest economic impact. By Impact I mean impact on the entire United States. I did not look at any particular region in this analysis. The analysis shows that tornados are the leading cause of death and injury while flooding is the leading ecomonic impact.
## Data Processing ##
The first thing we must do is read the raw data set into R

```r
rawdata<-read.csv("repdata-data-StormData.csv")
```
The next thing that has to be done is to simplify and clean up this raw dataset. Since I am only concerned about the effect on the United States as a whole and the financial other health outcome of the storms. I do not need the columns of the raw data set that deal with more detailed location. I also will retain the BGN_DATE Column to retain the date of the event.

```r
keep<-c("BGN_DATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")
keepdata<-rawdata[keep]
rm<-rawdata
```
Then in EVTYPE need to clean up so they are properly named. For example, Thunderstorms are named many different ways (TSTM WIND, THUNDERSTROM WINDS, THUNDERSTROM WIND). I will correct that among others listed here:


```r
keepdata[keepdata$EVTYPE=="TSTM WIND",]$EVTYPE<-"THUNDERSTORM WIND"
keepdata[keepdata$EVTYPE=="TSTM WIND (G45)",]$EVTYPE<-"THUNDERSTORM WIND"
keepdata[keepdata$EVTYPE=="THUNDERSTORM WINDS",]$EVTYPE<-"THUNDERSTORM WIND"
keepdata[keepdata$EVTYPE=="MARINE TSTM WIND",]$EVTYPE<-"MARINE THUNDERSTORM WIND"
keepdata[keepdata$EVTYPE=="HURRICANE/TYPHOON",]$EVTYPE<-"HURRICANE"
keepdata[keepdata$EVTYPE=="EXTREME COLD",]$EVTYPE<-"EXTREME COLD/WIND CHILL"
keepdata[keepdata$EVTYPE=="HEAT WAVE",]$EVTYPE<-"HEAT"
keepdata[keepdata$EVTYPE=="EXTREME HEAT",]$EVTYPE<-"EXCESSIVE HEAT"
keepdata[keepdata$EVTYPE=="WILD/FOREST FIRE",]$EVTYPE<-"WILDFIRE"
```
I also needed to convert the Crop and Property Damage back into their correct formats

```r
keepdata[keepdata$PROPDMGEXP=="K",]$PROPDMG<-keepdata[keepdata$PROPDMGEXP=="K",]$PROPDMG*10^3
keepdata[keepdata$PROPDMGEXP=="m",]$PROPDMG<-keepdata[keepdata$PROPDMGEXP=="m",]$PROPDMG*10^6
keepdata[keepdata$PROPDMGEXP=="M",]$PROPDMG<-keepdata[keepdata$PROPDMGEXP=="M",]$PROPDMG*10^6
keepdata[keepdata$PROPDMGEXP=="B",]$PROPDMG<-keepdata[keepdata$PROPDMGEXP=="B",]$PROPDMG*10^9
keepdata[keepdata$CROPDMGEXP=="k",]$CROPDMG<-keepdata[keepdata$CROPDMGEXP=="k",]$CROPDMG*10^3
keepdata[keepdata$CROPDMGEXP=="K",]$CROPDMG<-keepdata[keepdata$CROPDMGEXP=="K",]$CROPDMG*10^3
keepdata[keepdata$CROPDMGEXP=="m",]$CROPDMG<-keepdata[keepdata$CROPDMGEXP=="m",]$CROPDMG*10^6
keepdata[keepdata$CROPDMGEXP=="M",]$CROPDMG<-keepdata[keepdata$CROPDMGEXP=="M",]$CROPDMG*10^6
keepdata[keepdata$CROPDMGEXP=="B",]$CROPDMG<-keepdata[keepdata$CROPDMGEXP=="B",]$CROPDMG*10^9
```
## RESULTS ##

# Population Health #

The two measures of effect on population health are death and injuries. Lets look at death first. The total number of deaths listed in this data is:

```r
sum(keepdata$FATALITIES)
```

```
## [1] 15145
```
To determine the number of deaths for each type of storm we use the aggregate function and rank the events in desending order from highest to lowest.

```r
deaths<-aggregate(FATALITIES~EVTYPE,data=keepdata,sum)
rankdeath<-deaths[order(deaths$FATALITIES,decreasing=TRUE),]
head(rankdeath,n=20)
```

```
##                      EVTYPE FATALITIES
## 828                 TORNADO       5633
## 130          EXCESSIVE HEAT       1999
## 273                    HEAT       1109
## 151             FLASH FLOOD        978
## 460               LIGHTNING        816
## 755       THUNDERSTORM WIND        701
## 168                   FLOOD        470
## 580             RIP CURRENT        368
## 140 EXTREME COLD/WIND CHILL        285
## 356               HIGH WIND        248
## 19                AVALANCHE        224
## 963            WINTER STORM        206
## 581            RIP CURRENTS        204
## 307              HEAVY SNOW        127
## 399               HURRICANE        125
## 671             STRONG WIND        103
## 30                 BLIZZARD        101
## 347               HIGH SURF        101
## 287              HEAVY RAIN         98
## 79          COLD/WIND CHILL         95
```
Clearly the top causing of event death are tornados and Excessive heat which represent roughly 50% of the event deaths by themselves. I will repeat the same analysis for injuries.

```r
sum(keepdata$INJURIES)
```

```
## [1] 140528
```
Now get the number of injuries for each type of event

```r
injury<-aggregate(INJURIES~EVTYPE,data=keepdata,sum)
rankinjury<-injury[order(injury$INJURIES,decreasing=TRUE),]
head(rankinjury,n=20)
```

```
##                EVTYPE INJURIES
## 828           TORNADO    91346
## 755 THUNDERSTORM WIND     9356
## 168             FLOOD     6789
## 130    EXCESSIVE HEAT     6680
## 460         LIGHTNING     5230
## 273              HEAT     2409
## 423         ICE STORM     1975
## 151       FLASH FLOOD     1777
## 948          WILDFIRE     1456
## 242              HAIL     1361
## 399         HURRICANE     1321
## 963      WINTER STORM     1321
## 356         HIGH WIND     1137
## 307        HEAVY SNOW     1021
## 30           BLIZZARD      805
## 186               FOG      734
## 117        DUST STORM      440
## 969    WINTER WEATHER      398
## 89          DENSE FOG      342
## 842    TROPICAL STORM      340
```
Tornados are the dominate cause of injuries causing over 64% of all injuries. No other event causes more than 6.8% of total injuries.

I will plot this same data in a graph  for both deaths and injuries which really show how significant the top events are in death and how massive tornados are in terms of causing injuries

```r
barplot(rankdeath[1:20,2],col=rainbow(20),main="Top 20 Causes of Weather Event Deaths",ylab="Deaths",
        legend=rankdeath[1:20,1])
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-91.png) 

```r
barplot(rankinjury[1:20,2],col=rainbow(20),main="Top 20 Causes of Weather Event Injuries",ylab="Injuries",
        legend=rankinjury[1:20,1])
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-92.png) 
# Event Economic Cost #
Since we are only looking at cost for the ecomonic issue we will use the aggregate function as before but then add the crop and property together to determine the significant ecomonic events

```r
prop<-aggregate(PROPDMG~EVTYPE,data=keepdata,sum)
crop<-aggregate(CROPDMG~EVTYPE,data=keepdata,sum)
prop[,2]<-prop[,2]+crop[,2]
rankprop<-prop[order(prop$PROPDMG,decreasing=TRUE),]
```
The total ecomonic impact of Events is

```r
sum(rankprop[,2])
```

```
## [1] 4.764e+11
```
The table of the top 20 ecomonic Events are

```r
head(rankprop,n=20)
```

```
##                         EVTYPE   PROPDMG
## 168                      FLOOD 1.503e+11
## 399                  HURRICANE 8.652e+10
## 828                    TORNADO 5.735e+10
## 665                STORM SURGE 4.332e+10
## 242                       HAIL 1.876e+10
## 151                FLASH FLOOD 1.756e+10
## 95                     DROUGHT 1.502e+10
## 755          THUNDERSTORM WIND 1.086e+10
## 585                RIVER FLOOD 1.015e+10
## 423                  ICE STORM 8.967e+09
## 842             TROPICAL STORM 8.382e+09
## 948                   WILDFIRE 8.169e+09
## 963               WINTER STORM 6.715e+09
## 356                  HIGH WIND 5.909e+09
## 666           STORM SURGE/TIDE 4.642e+09
## 405             HURRICANE OPAL 3.192e+09
## 296  HEAVY RAIN/SEVERE WEATHER 2.500e+09
## 836 TORNADOES, TSTM WIND, HAIL 1.602e+09
## 287                 HEAVY RAIN 1.428e+09
## 140    EXTREME COLD/WIND CHILL 1.369e+09
```
Flooding does almost twice as much economic damage as the next largest Event

```r
barplot(rankprop[1:20,2],col=rainbow(20),main="Top 20 Causes of Weather Event Costs",ylab="Dollars",
        legend=rankprop[1:20,1])
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 
