
# Grab the ToothGrowth dataset
library(datasets)
data(ToothGrowth)
tgdata <- ToothGrowth

# Tabulate the mean and var per group
library(dplyr)


# Exploratory charts, understand the data
library(ggplot2)

# Basic scatterplot
g <- ggplot(tgdata, aes(x = dose, y = len, color=supp)) 
g <- g + ggtitle("Growth of Tooth - Effect of varying doses")
g <- g + labs(x = "Applied Dose", y = "Length of Tooth")
g <- g + geom_point() + geom_smooth()
g <- g + facet_grid(. ~ supp)
print(g)

g <- ggplot(tgdata, aes(x = supp, y = len, color = dose)) 
g <- g + ggtitle("Growth of Tooth - Comparison of two supplements")
g <- g + labs(x = "Supplement", y = "Length of Tooth")
g <- g + geom_point()
print(g)

# T tests
confi <- function(x) {
  df <- as.data.frame(x)
  names(df) <- c("len","supp","dose")
  t.test(len ~ supp, paired=FALSE, var.equal=FALSE, data=df)$conf
}
divtg = split(tgdata, tgdata$dose)
result = data.frame(from=c(0,0,0), to=c(0,0,0))
for (i in 1:length(divtg))
{
   x = confi(divtg[i])
   result[i,]$from <- x[1]
   result[i,]$to <- x[2]
}