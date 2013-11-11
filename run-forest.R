###################################################################################
##
## Forest growing platform
source("~/work/simulation/automate-forest.R")
source("~/work/functions/functions.R")
source("~/work/functions/functions-growth.R")
source("~/work/functions/functions-simulation.R")
source("~/work/neighborhoods/neighborhood-models.R")

###################################################################################
##
##  Test forest model
##
## Input
dat <- read.csv("~/work/data/data/long-bc-derived.csv")
time <- 76
samplePlots <- c(1,2)
numPlots <- 1
plotSize = 100
sizeVar = "bv"
nRadius = 6
growthCycles = 10
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

