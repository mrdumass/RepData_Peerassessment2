Analysis of Severe Storms in the United States
========================================================

## Synopsis ##
Using the data from the Storm Data from NOAA I use the data to determine which types of storms are the most harmful to population health and which types of storms have the greatest economic impact. By Impact I mean impact on the entire United States. I did not look at any particular region in any of the analysis. 
## Data Processing ##
The first thing we must do is read the raw data set into R

```r
rawdata<-read.csv("repdata-data-StormData.csv")
```
The first thing that has to be done is to simplify and clean up this raw dataset. Since I am only concerned about the effect on the United States as a whole and the financial other health outcome of the storms. I do not need the columns of the raw data set that deal with more detailed location. I also will retain the BGN_DATE Column to retain the date of the event.

```r
keep<-c("BGN_DATE","EVTYPE","FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")
keepdata<-rawdata[keep]
rm<-rawdata
```
The in the EVTYPE need to be clean up so they are properly named. For example, Thunderstorms are named many different ways (TSTM WIND, THUNDERSTROM WINDS, THUNDERSTROM WIND). I will correct that amonng others listed here:


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

## RESULTS ##

# Population Health

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
par(mfrow=c(2,1),mar=c(1,1,1,1))
barplot(rankdeath[1:20,2],col=rainbow(20),main="Top 20 Causes of Weather Event Death",ylab="Deaths",
        legend=rankdeath[1:20,1])
barplot(rankinjury[1:20,2],col=rainbow(20),main="Top 20 Causes of Weather Event Injuries",ylab="Injuries",
        legend=rankinjury[1:20,1])
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 
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
## [1] 12262327
```
The table of the top 20 ecomonic Events are

```r
head(rankprop,n=20)
```

```
##                   EVTYPE PROPDMG
## 828              TORNADO 3312277
## 755    THUNDERSTORM WIND 2854003
## 151          FLASH FLOOD 1599325
## 242                 HAIL 1268290
## 168                FLOOD 1067976
## 460            LIGHTNING  606932
## 356            HIGH WIND  342015
## 963         WINTER STORM  134700
## 948             WILDFIRE  132358
## 307           HEAVY SNOW  124418
## 423            ICE STORM   67690
## 671          STRONG WIND   64611
## 287           HEAVY RAIN   61965
## 373           HIGH WINDS   57385
## 842       TROPICAL STORM   54323
## 95               DROUGHT   37998
## 162       FLASH FLOODING   33623
## 399            HURRICANE   31491
## 911 URBAN/SML STREAM FLD   28846
## 30              BLIZZARD   25490
```
Flooding does almost twice as much economic damage as the next largest Event

```r
par(mfram(1,1))
```

```
## Error: could not find function "mfram"
```

```r
barplot(rankprop[1:20,2],col=rainbow(20),main="Top 20 Causes of Weather Event Costs",ylab="Dollars",
        legend=rankprop[1:20,1])
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 
