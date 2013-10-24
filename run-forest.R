###################################################################################
##
## Forest growing platform
source("~/Allometry/simulation/automate-forest.R")
source("~/Allometry/functions.R")
source("~/Allometry/simulation/functions-simulation.R")
source("~/Allometry/neighbor-model/neighborhood-models.R")

###################################################################################
##
##  Test forest model
##
## Input
dat <- read.csv("~/Allometry/data/long-bc-derived.csv")
time <- 76
samplePlots <- c(1,2)
numPlots <- 1
plotSize = 100
sizeVar = "bv"
nRadius = 6
growthCycles = 2
killFunc = "killTrees1"

mod <- growForest(plotSize, nRadius, growthCycles, samplePlots, numPlots, sizeVar, dat, time, killFunc)

## growth and prior columns
mod$growth <- rep(NA, nrow(mod))
mod$priorsize <- rep(NA, nrow(mod))
for (i in 2:range(mod$time)[2]) {
    tags <- mod[!is.na(mod$size) & mod$time == i & mod$stat == "ALIVE",]$tag
    mod[!is.na(mod$size) & mod$time == i & mod$stat == "ALIVE",]$growth <-
        mod[!is.na(mod$size) & mod$time == i & mod$stat == "ALIVE",]$size -
            mod[mod$tag %in% tags & mod$time == i-1 & mod$stat == "ALIVE",]$size
    mod[!is.na(mod$size) & mod$time == i & mod$stat == "ALIVE",]$priorsize <-
        mod[mod$tag %in% tags & mod$time == i-1 & mod$stat == "ALIVE",]$size
}
## subset with 7 measures
tags <- mod[mod$time == 8, ]$tag
comp <- mod[mod$tag %in% tags, ]
medGrowth <- ddply(comp, .(time), function(x) {
    data.frame(median = median(x$growth, na.rm = TRUE),
               time = mean(x$time), mean = mean(x$growth,na.rm = TRUE))
})

table(mod$time)
ggplot(mod, aes(size)) + geom_histogram() + facet_wrap(~time)
ggplot(mod, aes(time, size, group = tag, color = time)) + geom_path()

## location plot, sized by growth, colored by year
ggplot(mod, aes(x,y, size = growth, color = time)) + geom_point()

## growth histogram
ggplot(mod, aes(growth)) + geom_histogram() + facet_wrap(~time)

plot(medGrowth$time, medGrowth$median, ylim = c(0,.03))
points(medGrowth$time, medGrowth$mean, col = "red")
lines(medGrowth$time, medGrowth$median, col = "blue")
lines(medGrowth$time, medGrowth$mean, col = "red")

## growth histgram
ggplot(comp, aes(growth)) + geom_histogram() + facet_wrap(~time)

## growth vs size
plot(comp$priorsize, comp$growth)
ggplot(mod, aes(priorsize, growth, color = time, group = tag)) + geom_path()
ggplot(mod, aes(time, size,  group = tag)) + geom_path()

## Look at which trees died
ggplot(comp, aes(x,y,color = stat, size = size)) + geom_point()
ggplot(mod, aes(x,y,color = stat, size = size)) + geom_point()

boxplot(bc$rgrsisdp ~ bc$si)
