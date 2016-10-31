#setwd("~/Desktop/Fall_2016/Stat159/stat159-fall2016-hw02")
#advertising = read.csv('data/Advertising.csv')
args = commandArgs(trailingOnly =TRUE)
advertising = read.csv(args[1], header = TRUE)

# Compute the summary statistics
# Summary statistics of TV
tvSummary = c(summary(advertising$TV))
tvSummary = c(tvSummary, var(tvSummary), sd(tvSummary))
names(tvSummary)[7] = 'Variance'
names(tvSummary)[8] = 'Standard Deviation'

#Summary statistics of Sale
saleSummary = summary(advertising$Sales)
saleSummary = c(saleSummary, var(advertising$Sales), sd(advertising$Sales))
names(saleSummary)[7] = 'Variance'
names(saleSummary)[8] = 'Standard Deviation'

# Data Output
bothSummary = matrix(ncol = 8, nrow = 2)
bothSummary[1,] = tvSummary
bothSummary[2,] = saleSummary
bothSummary = data.frame(bothSummary)
rownames(bothSummary) = c('TV', 'Sales')
colnames(bothSummary) = names(tvSummary)
#write.table(bothSummary, file = 'data/eda-output.txt', )
sink('data/eda-output.txt')
bothSummary
sink()
save(bothSummary, file = 'data/eda-output.txt')


# Graph Output
pdf('images/histogram-tv.pdf')
hist(advertising$TV, main = 'Histogram of TV', xlab = 'TV')
dev.off()
png('images/histogram-tv.png')
hist(advertising$TV, main = 'Histogram of TV', xlab = 'TV')
dev.off()

pdf('images/histogram-sales.pdf')
hist(advertising$Sales, main = 'Histogram of Sales', xlab = 'Sales')
dev.off()

png('images/histogram-sales.png')
hist(advertising$Sales, main = 'Histogram of Sales', xlab = 'Sales')
dev.off()

