# goal: decrease time and increase training for laymen
## Data
airway = read.csv('../Data/airway.csv', header=TRUE)
arterial = read.csv('../Data/arterial.csv', header=TRUE)
#pretableQ1 = read.csv('../Data/pretableQ1.csv', header=TRUE)
#pretableQ2 = read.csv('../Data/pretableQ2.csv', header=TRUE)[[1:5]]
#pretableQ3 = read.csv('../Data/pretableQ3.csv', header=TRUE)[[1:5]]

## General Functions
# convert string in from 'Xm XXs', where X is numeric value, to number
#   of seconds. supress warning since data has missing values, treated 
#   as N/A
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
## Arterial

art.timeToFirst = toSeconds(arterial$Time.to.1st.Action)
art.timeToSol = toSeconds(arterial$Time.to.Solution)

# Y1 = art.timeToFirst[grep(' (T|t)rained', arterial$Subject)]
# Y2 = art.timeToFirst[grep('(U|u)ntrained', arterial$Subject)]
Y1 = art.timeToSol[grep(' (T|t)rained', arterial$Subject)]
Y2 = art.timeToSol[grep('(U|u)ntrained', arterial$Subject)]
Y3 = art.timeToFirst[grep(' (T|t)rained', arterial$Subject)]
Y4 = art.timeToFirst[grep('(U|u)ntrained', arterial$Subject)]
treatment = c(rep(1, 4), rep(2, 4))

# basic stats
Y1med = median(Y1)
Y2med = median(Y2)
Y1bar = mean(Y1)
Y2bar = mean(Y2)
SSE = sum((Y1 - Y1bar)^2, (Y2 - Y2bar)^2)
MSE = SSE / (24 - 3)

Y3bar = mean(Y3)
Y4bar = mean(Y4)
SSE = sum((Y3 - Y3bar)^2, (Y4 - Y4bar)^2)
MSE = SSE / (24 - 3)

residuals = c(Y1 - Y1bar, Y2 - Y2bar)
residuals2 = c(Y3 - Y3bar, Y4 - Y4bar)
t.test(x=Y1, y=Y2, alternative=c('less'))

# plot of residuals
# png("~/Desktop/residuals.png")
x11()
plot(treatment, residuals, main = 'Time to First Solution Residuals')
x11()
plot(treatment, residuals2, main = 'Time to First Action Residuals')
# dev.off()

# normal probability plot
# png("~/Desktop/qqnorm.png")
x11()
plot(qqnorm(residuals), main = 'Time to First Solution Normality Test')
x11()
plot(qqnorm(residuals2), main = 'Time to First Action Normality Test')
# dev.off()




## Airway
airwayTrained = timeToSol[grep(' (T|t)rained', airway$Subject)]
airwayUntrained = timeToSol[grep('(U|u)ntrained', airway$Subject)]

t.test(x=airwayTrained, y=airwayUntrained, alternative=c('less'))


timeToFirst = toSeconds(airway$Time_to_1st_Action)
timeToSol = toSeconds(airway$Time_to_Solution)
timeToReassess = toSeconds(airway$Time_to_Reassess)

## Plot function
func.check = function(data, someTitle) {

  hist(data, main = someTitle)
}

## Plots of all times
level = ('nurse', 'nurse', 'trained', 'untrained', 'trained', 
         'untrained', 'trained', 'untrained', 'trained', 'untrained', 'fire')
xyplot(airway$Subject ~ airway$Time_to_1st_Action | level)




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


