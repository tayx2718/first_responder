## Data
airway = read.csv('./Data/airway.csv', header=TRUE)
arterial = read.csv('./Data/arterial.csv', header=TRUE)
pretableQ1 = read.csv('./Data/pretableQ1.csv', header=TRUE)
pretableQ2 = read.csv('./Data/pretableQ2.csv', header=TRUE)
pretableQ3 = read.csv('./Data/pretableQ3.csv', header=TRUE)

## General Functions
toSeconds = function(col) {
  minutes = suppressWarnings(as.numeric(gsub('^.*([0-9]+)m.*$', '\\1', col)))
  seconds = suppressWarnings(as.numeric(gsub('^.*\\D([0-9])s.*$', '\\1', col)))
  seconds2 = suppressWarnings(as.numeric(gsub('^.*([0-9]{2})s.*$', '\\1', col)))
  minutes[is.na(minutes)] <- 0
  seconds[is.na(seconds)] <- 0
  seconds2[is.na(seconds2)] <- 0
  return(seconds + seconds2 + minutes * 60)
}

## Pre-Test


## Airway

timeToFirst = toSeconds(airway$Time_to_1st_Action)
timeToSol = toSeconds(airway$Time_to_Solution)
timeToReassess = toSeconds(airway$Time_to_Reassess)

## Plot function

func.check = function(data, someTitle) {

  hist(data, main = someTitle)
  }

## Plots of all times
a = function(){
myList = list(timeToFirst, timeToSol, timeToReassess)
myTitle = list('Time to 1st Action', 'Time to Solution', 'Time to Reassess')

png('myHist.png')
par(mfrow = c(2, 2), mar = c(3, 3, 3, 3))
for (i in 1:3)
  hist(myList[[i]], main = myTitle[[i]])
title('Histogram of Times', line = -1, outer = TRUE)
dev.off()
}

## Arterial

art.timeToFirst = toSeconds(arterial$Time.to.1st.Action)
art.timeToSol = toSeconds(arterial$Time.to.Solution)
