library('xtable') # for latex functions
library('lattice') # for graph functions

table1 = read.table('../Data/table1.txt', sep = '\t', header=TRUE)

addtorow = list()
addtorow$pos = list(0, 0)
addtorow$command = c("& \\multicolumn{2}{c}{Time to Solution} \\\\\n",
                    "Group & Trained & Untrained \\\\\n")
xtable(table1, add.to.row = addtorow, include.colnames = TRUE)


table2 = read.table('../Data/911_response.txt', sep = '\t', header=TRUE)

addtorow = list()
addtorow$pos = list(0, 0)
addtorow$command = c("& \\multicolumn{2}{c}{Time to Solution} \\\\\n",
                    "Group & Trained & Untrained \\\\\n")

xtable(table2, add.to.row = addtorow, include.colnames = TRUE)

table3 = read.table('../Data/table3.txt', sep = ' ', header=TRUE)

addtorow = list()
addtorow$pos = list(0, 0)
addtorow$command = c("& \\multicolumn{2}{c}{Time to Solution} \\\\\n",
                    "Group & Trained & Untrained \\\\\n")

xtable(table3, add.to.row = addtorow, include.colnames = TRUE)

table4 = read.table('../Data/table4.txt', sep = '\t', header=TRUE)

addtorow = list()
addtorow$pos = list(0, 0)
addtorow$command = c("& \\multicolumn{2}{c}{Time to Solution} \\\\\n",
                    "Group & Trained & Untrained \\\\\n")

xtable(table4, add.to.row = addtorow, include.colnames = TRUE)

table5 = read.table('../Data/table5.txt', sep = '\t', header=TRUE)

addtorow = list()
addtorow$pos = list(0, 0)
addtorow$command = c("& \\multicolumn{2}{c}{Time to Solution} \\\\\n",
                    "Group & Trained & Untrained \\\\\n")

xtable(table5, add.to.row = addtorow, include.colnames = TRUE)

quit()


x1 = c(0.15, 0.85)
names(x1) = c('Untrained', 'Trained')

png('../Plots/table1.png')
barplot(x1, main = 'Correct Answer to Cause of Death Ages 1-44 in U.S.', 
     xlab = 'Group', ylab = 'Percent Correct')
dev.off()


