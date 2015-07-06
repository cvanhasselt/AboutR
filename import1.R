library(ggplot2)
library(dplyr)

# set working directory
setwd("E:/Personal/Toastmasters/AboutR")

# load data, with timing data
system.time(printerData <- read.csv("E:/Personal/Toastmasters/AboutR/March2015allprint.csv"))

# show the first five rows of printerData data frame.
head(printerData,5)

#show description of data
str(printerData,vec.len=3)

# define localized weekdays
dayNames <- weekdays(x=as.Date(seq(7), origin="1950-01-01"))

# identify color and black&white print jobs, creating new data frame column
# identify duplex jobs, creating new data frame column
# create usable date column for time-series
printerData <- mutate(printerData,
                      xInkType = ifelse( grepl( "C",JobProperties),"C","B&W"),
                      xDuplexJob =  ifelse( grepl( "D",JobProperties),"D","ND"),
                      xDate =  as.Date(cTransactDate,"%m/%d/%Y"),
                      xDayOfWeek = weekdays(xDate))

# find all dates in xDate column, then find the number of days for each weekday
# for example, find the number of Fridays or Wednesdays within the date range
dataDays     <- unique(printerData$xDate)
dataDayCount <- sapply(dayNames,function(x) sum(weekdays(dataDays) == x))

# aggregate printer data for plotting by date (equivalent to a pivot.table)
printJobsByDate <- printerData %>%
        group_by(xDate,xInkType) %>%
        select(cUnits) %>%
        summarise(impressions = sum(cUnits))

# aggregate printer data for plotting by date
printJobsByDateCombined <- printerData %>%
        group_by(xDayOfWeek) %>%
        select(xDate,cUnits,xDayOfWeek,xInkType) %>%
        summarise(impressions = sum(cUnits))

# aggregate by days of the week
printJobsDoW <- printerData %>%
        filter(xDayOfWeek != "Saturday" & xDayOfWeek != "Sunday" ) %>%
        group_by(xDayOfWeek,xInkType) %>%
        select(cUnits) %>%
        summarise(impressions = sum(cUnits))

# define  color palette
cbPalette <- c("#1f78b4", "#33a02c")

# construct ggplot plot object. stat="identity" sets height of bar to a value.
pdPlot <- ggplot(printJobsByDate,
                 aes(x = xDate,
                     y = impressions,
                     fill = xInkType)) +
    geom_bar(stat="identity") +
    scale_fill_manual(values=cbPalette) +
    guides(fill=guide_legend(title="Print Job Type")) +
    xlab("Date") +
    ylab("Number of Print Impressions") +
    ggtitle("Printing Activity for March 2015, per day") +
    theme(plot.title = element_text(lineheight=1, face="bold"))

# display plot
pdPlot

# construct second plot, aggregating information by day of week
pdPlot2 <- ggplot(printJobsDoW,
                  aes(x = factor(xDayOfWeek,dayNames),
                      y = impressions,
                      fill=xInkType,
                      ymax=max(impressions))) +
  geom_bar(stat="identity") +
  geom_text(aes(label=impressions),
            position="stack",
            size=4,
            hjust=.5,
            vjust=2,
            color="grey") +
  scale_fill_manual(values=cbPalette) +
  guides(fill=guide_legend(title="Print Job Type")) +
  xlab("Day of Week") +
  ylab("Print Impressions per Weekday") +
  ggtitle("Printing Activity for March 2015, per day of week, weekdays only") +
  theme(plot.title = element_text(lineheight=1, face="bold"))

# display plot
pdPlot2

dayTotals <- sapply(dayNames,
                    FUN=function(x) round(sum(printJobsDoW[printJobsDoW$xDayOfWeek==x,]$impressions)/ dataDayCount[x],0))

# construct second plot, aggregating information by day of week
pdPlot3 <- ggplot(printJobsDoW,
                  aes(x = factor(xDayOfWeek,dayNames),
                      y = impressions / dataDayCount[factor(xDayOfWeek,dayNames)],
                      fill=xInkType)) +
  geom_bar(stat="identity") +
  geom_text(aes(x = factor(xDayOfWeek,dayNames),
                y=dayTotals[factor(xDayOfWeek,dayNames)],
                label=dayTotals[factor(xDayOfWeek,dayNames)]),
            vjust=-1,
            size=5,
            color="slategrey") +
  geom_text(aes(label= round(impressions / dataDayCount[factor(xDayOfWeek,dayNames)],0)),
            position="stack",
            size=4,
            hjust=.5,
            vjust=2,
            color="grey") +
  scale_fill_manual(values=cbPalette,
                      name="Print Job Type",
                      breaks=c("C","B&W"),
                      labels=c("Color","Black & White")) +
  xlab("Day of Week") +
  ylab("Average Print Impressions per Weekday") +
  ggtitle("Printing Activity for March 2015, average per day of week, weekdays only") +
  theme(plot.title = element_text(lineheight=1, face="bold"))

# display plot
pdPlot3


