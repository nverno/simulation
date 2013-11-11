## Grow a forest using randomly generated data
## Requires:
## - new growth model that doesn't rely on rGR (rGR is calculated with data from next
##   period)
## - new way to calculate SDP (SDP is calculated using data across all samplings)
## - new way to calculate rGR for hazard model (generated data has no prior values)
source("~/work/neighborhoods/neighborhood-models.R")
source("~/work/functions/functions.R")
source("~/work/functions/functions-growth.R")
source("~/work/functions/functions-simulation.R")

## data
bc <- read.csv("~/work/data/data/long-bc-derived.csv")
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
rgrs <- read.csv("~/work/data/data/simulation/rgr-params-for-sim.csv")
a <- rgrs[rgrs$install == mean(simBC$install) & rgrs$plot == mean(simBC$plot) &
          rgrs$sdpclass == mean(simBC$sdpclass) & rgrs$si == mean(simBC$si),]$a
b <- rgrs[rgrs$install == mean(simBC$install) & rgrs$plot == mean(simBC$plot) &
          rgrs$sdpclass == mean(simBC$sdpclass) & rgrs$si == mean(simBC$si),]$b

## Get parameters
pars <- read.csv("~/work/data/data/parameters/parameters-growth.csv")
ps <- getParams(sr=6,spec="FD",ind.var="priorbv",dep.var="rgrsisdp",
                 currentmodel = "simplest")

## Grow the forest
targets$pred.rGR <- simplest(ps)
per2 <- targets
per2$time <- 2
newTargs <- rbind.fill(per2, targets)
newTargs$growth <- apply(newTargs, 1, function(x) {
    ifelse (x[["time"]] == 2,
            a * as.numeric(x[["size"]]) ^ b * as.numeric(x[["pred.rGR"]]), NA)
})
newTargs$size <- apply(newTargs, 1, function(x) {
    ifelse(x[["time"]] == 2,
       { as.numeric(x[["size"]]) + as.numeric(x[["growth"]]) },
     { as.numeric(x[["size"]]) })
})

## What does it look like
ggplot(newTargs, aes(time, size, group = tag)) + geom_path()
ggplot(newTargs, aes(x,y, size = size)) + geom_point()
