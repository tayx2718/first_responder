## Data
cfr = read.csv('./Data/tccc/cfr.csv', header=TRUE)
kia = read.csv('./Data/tccc/kia.csv', header=TRUE)
pp_deaths_oif_oef = read.csv('./Data/tccc/pp_deaths_oif_oef.csv', header=TRUE)


## CFR
x11()
plot(x = cfr$war, y = cfr$percent)

x11()
par(mfrow = c(1, 2), mar = c(3, 3, 3, 3))
## KIA
plot(x = kia$cause, y = kia$percent, main = 'kia')

## PP Deaths Iraq
plot(x = pp_deaths_oif_oef$type, y = pp_deaths_oif_oef$percent, main = 'Iraq')


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
