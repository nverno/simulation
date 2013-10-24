## Automate the forest growth process, so all that is required from the user
##  is number of growth cycles, neighbor radius, starting plot size, size variable, and
##  plots to sample from?

#################################################################################
## Function to grow forest
## - generate random field
## - get neighbor model parameters
## - grow forest
##   - get size.effect parameters
##   - make neighbor matrices (size, distance matrices)
##   - predict rGR
##   - predict growth: size * rGR
##   - add new forest data to previous forest data
growForest <- function(plotSize, nRadius, growthCycles, samplePlots, numPlots = 1,
                       sizeVar = "bv", dat, time, killFunc) {
    ## Put sdpclass in numeric if its not
    dat$sdpclass <- as.numeric(dat$sdpclass)
    ## choose random plot from install
    pnum <- floor(runif(1, 1, 3))
    samp <- dat[dat$install == samplePlots[1] & dat$time == time,]
    samp <- samp[samp$plot == unique(samp$plot)[pnum] & samp$stat == "ALIVE",]
    ## Generate random field from sample plot
    simPlot <- makeRandomPlot(samp, plotSize, sizeVar)
    ## Get neighbor model parameters
    pars <- read.csv("~/Allometry/parameters.csv")
    ps <- get.params(sr=nRadius,spec="FD",ind.var="priorbv",dep.var="rgrsisdp",
                     currentmodel = "simplest")
    ## dataframe to accumulate results
    output <- simPlot
    ## Grow forest one cycle at a time
    for (i in 1:growthCycles-1) {
        ## Step forest (grow it one cycle) and add results to output
        print(paste("Growth cycle: ",i+1))
        pSize <- plotSize - (i-1)*nRadius
        simPlot <- stepForest(simPlot, pSize, ps)
        simPlot <- killTrees(simPlot, killFunc = "killTrees1")
        output <- rbind.fill(output, simPlot)
    }
    return(output)
}

## Function to return forest after next growth phase
stepForest <- function(simPlot, pSize, ps) {
    ab <- abPars(simPlot)
    ## Make neighbor matrices, all living neighbors included unless bigger = TRUE
    ##  is specified
    neighborMatrices(dat=simPlot, nRadius=nRadius, plotSize=pSize, ind.var="size")
    ## Grow the forest
    targets$pred.rGR <- simplest(ps)
    growth <- apply(targets, 1, function(x) {
        ab[1] * as.numeric(x[["size"]]) ^ ab[2] * as.numeric(x[["pred.rGR"]])
    })
    nextPeriod <- targets
    nextPeriod$size <- nextPeriod$size + growth
    nextPeriod$time <- nextPeriod$time + 1
    return(nextPeriod)
}


## Function to retrieve a,b growth parameters for growForest
abPars <- function(simBC) {
    rgrs <- read.csv("~/Allometry/data/rgr-params-for-sim.csv")
    a <- rgrs[rgrs$install == mean(simBC$install) & rgrs$plot == mean(simBC$plot) &
              rgrs$sdpclass == mean(simBC$sdpclass) & rgrs$si == mean(simBC$si),]$a
    b <- rgrs[rgrs$install == mean(simBC$install) & rgrs$plot == mean(simBC$plot) &
              rgrs$sdpclass == mean(simBC$sdpclass) & rgrs$si == mean(simBC$si),]$b
    c(a,b)
}

## Wrapper for tree killing functions that just calls the appropriate killing function
killTrees <- function(simBC, killFunc) {
    do.call(killFunc, list(simBC))
}

##################################################################################
##
## Methods for Tree Killing
##
## Function to kill trees after a growth step
killTrees1 <- function(simBC) {
    ## retrieve prob of death
    pDeath <- read.csv("~/Allometry/data/pDeath-by-sdpclass.csv")
    pDie <- pDeath[pDeath$sdpclass == unique(simBC$sdpclass),]$pDeath
    ## randomly kill off trees from simBC
    samp <- nrow(simBC)
    numDie <- samp * pDie
    rowsToDie <- floor(runif(numDie, 1, samp+1))
    levels(simBC$stat) <- c("ALIVE","DEAD")
    simBC[rowsToDie, ]$stat <- "DEAD"
    return(simBC)
}

## Kill trees using hazard model
killTrees2 <- function(

## simBC <- mod[mod$time == 1,]
## simBC <- killTrees(simBC)

