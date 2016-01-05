## Data
cfr = read.csv('../Data/tccc/cfr.csv', header=TRUE)
kia = read.csv('../Data/tccc/kia.csv', header=TRUE)
pp_deaths_oif_oef = read.csv('../Data/tccc/pp_deaths_oif_oef.csv', header=TRUE)


## CFR: Case Fertality Rate, by war
# war: WWII, Vietnam, OIF/OEF
png('../Plots/cfr.png')
plot(x = cfr$war, y = cfr$percent, main="Case Fertality Rate")
dev.off()

## KIA
# causes of death in vieatnam war
png('../Plots/kia.png')
plot(x = kia$cause, y = kia$percent, main = 'Causes of Death', sub = 'in Vietnam War')
dev.off()

## PP Deaths Iraq
png('../Plots/deaths_oif_oef.png')
plot(x = pp_deaths_oif_oef$type, y = pp_deaths_oif_oef$percent, 
     main = 'Potentially Preventable Deaths', sub = 'in OIF/OEF')
dev.off()


## Plot function

func.check = function(data, someTitle) {

  hist(data, main = someTitle)
  }

## Plots of all times
a = function(){
myList = list(timeToFirst, timeToSol, timeToReassess)
myTitle = list('Time to 1st Action', 'Time to Solution', 'Time to Reassess')

png('./Plots/myHist.png')
par(mfrow = c(2, 2), mar = c(3, 3, 3, 3))
for (i in 1:3)
  hist(myList[[i]], main = myTitle[[i]])
title('Histogram of Times', line = -1, outer = TRUE)
dev.off()
}
