## Grow a forest using randomly generated data
## Requires:
## - new growth model that doesn't rely on rGR (rGR is calculated with data from next
##   period)
## - new way to calculate SDP (SDP is calculated using data across all samplings)
## - new way to calculate rGR for hazard model (generated data has no prior values)
source("~/Allometry/neighbor-model/neighborhood-models.R")
source("~/Allometry/functions.R")
source("~/Allometry/simulation/functions-simulation.R")

## data
bc <- read.csv("~/Allometry/data/long-bc-derived.csv")
bc$sdpclass <-as.numeric(bc$sdpclass)
tst <- subset(bc, install == 1 & plot == 16 & time == 73)
simBC <- makeRandomPlot(tst, 100, sizeVar = "bv")

## check the dist
ggplot(simBC, aes(x,y, size = size)) + geom_point()

####################################################################################
##
## Growth
##
## New growth model
## - Use a and b parameters from fits to observed data that matches the SDP class and
##   SI class of the the simulated data
## Growth = a*priorSize^b * comp.effect

## make neighbor matrices, all living neighbors included unless bigger = TRUE is specified
neighborMatrices(simBC, nRadius = 6, plotSize = 100, ind.var = "size")

## Retrieve a, b parameters for size.effect (a*x^b)
rgrs <- read.csv("~/Allometry/data/rgr-params-for-sim.csv")
a <- rgrs[rgrs$install == mean(simBC$install) & rgrs$plot == mean(simBC$plot) &
          rgrs$sdpclass == mean(simBC$sdpclass) & rgrs$si == mean(simBC$si),]$a
b <- rgrs[rgrs$install == mean(simBC$install) & rgrs$plot == mean(simBC$plot) &
          rgrs$sdpclass == mean(simBC$sdpclass) & rgrs$si == mean(simBC$si),]$b

## Get parameters
pars <- read.csv("~/Allometry/parameters.csv")
ps <- get.params(sr=6,spec="FD",ind.var="priorbv",dep.var="rgrsisdp",
                 currentmodel = "simplest")

## Grow the forest
targets$pred.rGR <- simplest(ps)
growth <- apply(targets, 1, function(x) {
    a * as.numeric(x[["size"]]) ^ b * as.numeric(x[["pred.rGR"]])
})
per2 <- targets
per2$size <- per2$size + growth
per2$time <- 2
newTargs <- rbind.fill(targets, per2)
newTargs$growth <- rep(NA, nrow(newTargs))
newTargs[newTargs$time==2,] <- growth

## What does it look like
ggplot(newTargs, aes(time, size, group = tag)) + geom_path()
ggplot(newTargs, aes(x,y, size = size)) + geom_point()
