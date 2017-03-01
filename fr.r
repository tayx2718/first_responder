# initial setup
#install.packages('xtable')
library(xtable) # for latex functions
library(lattice) # for graph functions
# to do
# 1. add arterial action/solution graph
# 2. make table of what action was

## Data
airway = read.csv('../Data/airway.csv', header=TRUE)[,1:4]
arterial = read.csv('../Data/arterial.csv', header=TRUE)[,1:4]

## Initial tables#{{{
addtorow = list()
addtorow$pos = list(0, 0)
addtorow$command = c("& \\multicolumn{2}{c}{Time to Solution} \\\\\n",
                    "Group & Trained & Untrained \\\\\n")

xtable(airway, add.to.row = addtorow, include.colnames = TRUE)

addtorow = list()
addtorow$pos = list(0, 0)
addtorow$command = c("& \\multicolumn{2}{c}{Time to Solution} \\\\\n",
                    "Group & Trained & Untrained \\\\\n")

xtable(arterial, add.to.row = addtorow, include.colnames = TRUE)

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
timeToAction = toSeconds(arterial$Time.to.1st.Action)
timeToSol = toSeconds(arterial$Time.to.Solution)
timeToBoth = c(timeToAction, timeToSol)
print(timeToAction)
print(timeToSol)

level = c('nurse', 'nurse', 'trained', 'untrained', 'trained', 'untrained', 'untrained', 'trained', 'trained', 'untrained', 'ems')
bothLevel = rep(level, 2)
cat1 = rep('action', 11)
cat2 = rep('solution', 11)
catBoth = c(cat1, cat2)
subjectBoth = rep(arterial$Subject, 2)

png('../Plots/Arterial_Action.png')
xyplot( subjectBoth ~ timeToBoth| catBoth, 
        groups = bothLevel, pch = 19:25,
        auto.key = list(corner = c(1, .95), cex = 0.5, title = 'Cat'), 
        xlab = 'Time in Seconds', ylab = 'Group', main = 'Arterial: Time to First Action vs Solution
        by Training Level')  
dev.off()



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
      main = 'Arterial: Time to Solution', xlim = c(0, 600),
      ylab = 'Group', xlab = 'Time in Seconds')
dev.off()
#}}}
# barplot
# split into two tables, was one matrix with nrow=2
#airwayTable = matrix(c(34.75, 203.25, 111, 480, 48, 158), nrow=2, ncol=3)
airwayTable1 = c(75, 48, 34.75, 111) # c(111, 34.75, 48, 75)
airwayTable2 = c(480, 158, 203.25, 480) # c(480, 203.25, 158, 480)
names(airwayTable1) = c('Health Care\nProfessional', 'Engine Co', 'Trained', 'Untrained')
names(airwayTable2) = c('Health Care\nProfessional', 'Engine Co', 'Trained', 'Untrained')
# combined barplot
# png('../Plots/Arterial_Plot.png')
# mp = barplot(airwayTable, main='Arterial: Hemorrhage Control Scenario (Matched Groups)', 
#         xlab='Time (sec)', legend=c('Time to 1st Action','Time to Solution'),
#         horiz=TRUE, col=c('white', 'grey'), xlim=c(0,600),
#         args.legend=list(x="bottomright"))
# dev.off()

png('../Plots/Arterial_Plot1.png')
mp = barplot(airwayTable1, main='Arterial: Hemorrhage Control Scenario (Matched Groups) \n Time to 1st Action', 
        xlab='Time (sec)', col = 'white', horiz=TRUE, xlim=c(0,200))

dev.off()

png('../Plots/Arterial_Plot2.png')
mp = barplot(airwayTable2, main='Arterial: Hemorrhage Control Scenario (Matched Groups) \n Time to Solution', 
        xlab='Time (sec)', col = 'white', horiz=TRUE, xlim=c(0,600))
dev.off()




## Airway
# Table of time to first action and solution#{{{
timeToAction = toSeconds(airway$Time_to_1st_Action)
timeToSol = toSeconds(airway$Time_to_Solution)
timeToBoth = c(timeToAction, timeToSol)
head(airway)

level = c('nurse', 'nurse', 'trained', 'untrained', 'trained', 'untrained', 'trained', 'untrained', 'trained', 'untrained', 'fire')
bothLevel = rep(level, 2)
cat1 = rep('action', 11)
cat2 = rep('solution', 11)
catBoth = c(cat1, cat2)
subjectBoth = rep(airway$Subject, 2)

png('../Plots/Airway_Action.png')
xyplot( subjectBoth ~ timeToBoth| catBoth, 
       groups = bothLevel, pch = 19:25,
        auto.key = list(corner = c(1, .95), cex = 0.5, title = 'Cat'), 
        xlab = 'Time in Seconds', ylab = 'Group', main = 'Airway: Time to First Action vs Solution
        by Training Level')  
dev.off()

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
      main = 'Airway: Time to Solution', xlim = c(0,600),
      ylab = 'Group', xlab = 'Time in Seconds')
dev.off()
#}}}
# barplot
# airwayTable = matrix(c(20.5, 32.6, 43, 423, 25, 81), nrow=2, ncol=3)
# order is: untrained, trained, engine co, nursing grad--> reverse
airwayTable1 = c(87, 25, 20.5, 43) #c(43, 20.5, 25, 87)
airwayTable2 = c(480, 81, 32.6, 423) #c(423, 32.6, 81, 480)
names(airwayTable1) = c('Health Care\nProfessional', 'Engine Co', 'Trained', 'Untrained') # c('Untrained', 'Trained', 'Engine Co', 'Health Care\nProfessional')
names(airwayTable2) = c('Health Care\nProfessional', 'Engine Co', 'Trained', 'Untrained')
png('../Plots/Airway_Plot1.png')
# single barplot
mp = barplot(airwayTable1, main='Airway: Compromised Airway Scenario (Matched Groups)\n Time to 1st Action', 
        xlab='Time (sec)', horiz=TRUE, col=c('grey'), xlim=c(0,150))

# old way with combined bar plot
# mp = barplot(airwayTable, main='Airway: Compromised Airway Scenario (Matched Groups)', 
#         xlab='Time (sec)', legend=c('Time to 1st Action','Time to Solution'),
#         horiz=TRUE, col=c('white', 'grey'), xlim=c(0,500),
#         args.legend=list(x="bottomright"))
dev.off()

png('../Plots/Airway_Plot2.png')
mp = barplot(airwayTable2, main='Airway: Compromised Airway Scenario (Matched Groups)\n Time to Solution', 
        xlab='Time (sec)', horiz=TRUE, col=c('grey'), xlim=c(0,500))
dev.off()

# old, only plots trained and untrained
# airwayTable = matrix(c(21, 33, 54, 423), nrow=2, ncol=2)
# colnames(airwayTable) = c('Average \n(Trained) All', 'Average \n(Untrained) All')
# png('../Plots/Airway_Plot2.png')
# mp = barplot(airwayTable, main='Compromised Airway Scenario (All Groups)', 
#         xlab='Time (sec)', legend=c('Time to 1st Action','Time to Solution'),
#         horiz=TRUE, col=c('white', 'grey'), xlim=c(0,500),
#         args.legend=list(x="bottomright"))
# dev.off()




# t-test for time to first solution
print(t.test(x=Trained, y=Untrained, alternative=c('less')))

print(t.test(x=airwayTrained, y=airwayUntrained, alternative=c('less')))

quit()

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


