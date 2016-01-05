# initial setup
library('xtable') # for latex functions
library('lattice') # for latex functions

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
# None at the moment. Apparently not as important for analysis
## Arterial

# Table of time to first solution
art.timeToFirst = toSeconds(arterial$Time.to.1st.Action)
art.timeToSol = toSeconds(arterial$Time.to.Solution)

Arterial_Trained = art.timeToSol[grep(' (T|t)rained', arterial$Subject)]
Arterial_Untrained = art.timeToSol[grep('(U|u)ntrained', arterial$Subject)]

addtorow = list()
addtorow$pos = list(0, 0)
addtorow$command = c("& \\multicolumn{2}{c}{Time to Solution} \\\\\n",
                    "Group & Trained & Untrained \\\\\n")

art.Sol = cbind(Arterial_Trained, Arterial_Untrained)
xtable(art.Sol, add.to.row = addtorow, include.colnames = TRUE)

# XY Plot
level = c('nurse', 'nurse', 'trained', 'untrained', 'trained', 'untrained', 'untrained', 'trained', 'trained', 'untrained', 'ems')
png('../Plots/Arterial_Time.png')
xyplot(arterial$Subject ~ art.timeToSol, groups = level,
      key = list(corner = c(1, 1), cex=0.95, title = 'Training', text = list(unique(level)), points = list(pch = 1:3, col = 1:3)), pch = 1:3, col = 1:3,
      main = 'Time to Solution', xlim = c(0, 600),
      ylab = 'Group', xlab = 'Time in Seconds')
dev.off()

## Airway
# Table of time to first solution
timeToSol = toSeconds(airway$Time_to_Solution)
Airway_Trained = timeToSol[grep(' (T|t)rained', airway$Subject)]
Airway_Untrained = timeToSol[grep('(U|u)ntrained', airway$Subject)]

addtorow = list()
addtorow$pos = list(0, 0)
addtorow$command = c("& \\multicolumn{2}{c}{Time to Solution} \\\\\n",
                    "Group & Trained & Untrained \\\\\n")

air.Sol = cbind(Airway_Trained, Airway_Untrained)
xtable(air.Sol, add.to.row = addtorow, include.colnames = TRUE)

# XY Plot
level = c('nurse', 'nurse', 'trained', 'untrained', 'trained', 'untrained', 'trained', 'untrained', 'trained', 'untrained', 'fire')
png('../Plots/Airway_Time.png')
xyplot(airway$Subject ~ timeToSol, groups = level,
      key = list(corner = c(1, 1), cex=0.95, title = 'Training', text = list(unique(level)), points = list(pch = 1:4, col = 1:4)), pch = 1:4, col = 1:4,
      main = 'Time to Solution', xlim = c(0,600),
      ylab = 'Group', xlab = 'Time in Seconds')
dev.off()

quit()





# t-test for time to first solution
#print(t.test(x=Trained, y=Untrained, alternative=c('less')))

print(t.test(x=airwayTrained, y=airwayUntrained, alternative=c('less')))


# timeToFirst = toSeconds(airway$Time_to_1st_Action)
# timeToReassess = toSeconds(airway$Time_to_Reassess)

## Plot function
func.check = function(data, someTitle) {

  hist(data, main = someTitle)
}

#############
#### OLD
#############

## Plots of all times
level = c('nurse', 'nurse', 'trained', 'untrained', 'trained', 
         'untrained', 'trained', 'untrained', 'trained', 'untrained', 'fire')
png('../Plots/Airway_Time.png')
xyplot(airway$Subject ~ airway$Time_to_1st_Action | level)
dev.off()
# 
# 
# 
# 
# a = function(){
# myList = list(timeToFirst, timeToSol, timeToReassess)
# myTitle = list('Time to 1st Action', 'Time to Solution', 'Time to Reassess')
# 
# png('myHist.png')
# par(mfrow = c(2, 2), mar = c(3, 3, 3, 3))
# for (i in 1:3)
#   hist(myList[[i]], main = myTitle[[i]])
# title('Histogram of Times', line = -1, outer = TRUE)
# dev.off()
# }


