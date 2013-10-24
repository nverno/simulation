## Hazard model to be incorporated in the simulation
## Categorical SDP (prior to or on self-thinning) and SI (low, medium, high)
hazard <- function(lambda, b0, b1, b2, b3, b4, b5) {
    lambda = pars["lambda"]
    b0 = pars["b0"]
    b0 * lambda * LRS^(b1 + b3 * SDP + b4 * SI + b5 * SDP * SI) * rGR ^ b2
}

## Function to produce upper and lower bounds for continuous data classes
cont2class <- function(dat, colm, num = NULL, splits = NULL) {
    if (!is.null(num)) labs <- levels(cut(unique(dat[,colm]), num))
    if (!is.null(splits)) labs <- levels(cut(unique(dat[,colm]),breaks = splits))
    bounds <- data.frame(cbind( as.numeric(sub("\\((.+),.+", "\\1", labs)),
                    as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", labs))))
    names(bounds) <- c("lower", "upper")
    return(bounds)
}

## Function to assign class to continuous data in forest sim
assignClass <- function(forest, colm, bounds) {
    simLev <- unique(forest[, colm])
    for (i in 1:nrow(bounds)) {
        if (max(simLev) < bounds[i, "upper"] & min(simLev) >= bounds[i, "lower"])
            return(i)
    }
    stop("Continuous data doesn't fit into any of the classes")
}

## Function to retrieve hazard parameters from factors of SI and SDP
## Note: requires original dataset to calculate classes for continuous data
getHazPars <- function(forest, dat) {
    siLev <- unique(forest$si)
    tmp <- read.csv("~/Allometry/data/hazPars.csv")
    siBounds <- cont2class(dat, colm = "si", num = 3)
    siClass <- assignClass(forest, colm = "si", siBounds)
    pars <- tmp[tmp$SIclass == siClass & tmp$SDPclass == unique(forest$sdpclass),]
    return(pars)
}

## Function to calculate LRS
## - current size + est. growth / max size in plot
computeLRS <- function(forest) {
    maxSize <- max(forest[forest$stat == "ALIVE" & forest$time ==
                      max(unique(forest$time)),]$size)
    return(forest[forest$stat == "ALIVE" & forest$time ==
                  max(unique(forest$time)),]$size / maxSize)
}

## Function to calculate rGR for current trees (obs growth / pred growth)
## - Only works if this is not first growth cycle
## - Use parameters from rgr-params-for-sim
computeRgR <- function(forest) {
    ## assert this is not the first growth cycle
    if(length(unique(forest[,"time"])) < 2) stop("Can't compute rGR on first growth cycle")
    ## retrieve a,b params for power fits
    ab <- abPars(forest)
    ## calculate rGR based on previous growth
    times <- unique(forest$time)
    liveTrees <- forest[forest$time == max(times) & forest$stat == "ALIVE",]$tag
    prev <- forest[forest$tag %in% liveTrees & forest$time == max(times)-1,]
    predGrowth <- ab[1] * prev$size ^ ab[2]
    actualGrowth <- forest[forest$tag %in% liveTrees & forest$time == max(times),]$size -
        forest[forest$tag %in% liveTrees & forest$time == max(times)-1,]$size
    return(actualGrowth/predGrowth)
}

## Function to estimate where a plot is on the stand development trajectory

