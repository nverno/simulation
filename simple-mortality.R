## Get some basic mortality probability to add to the model
source("~/Allometry/neighbor-model/neighborhood-models.R")
source("~/Allometry/functions.R")
source("~/Allometry/simulation/functions-simulation.R")

## data
bc <- read.csv("~/Allometry/data/long-bc-derived.csv")
bc$sdpclass <-as.numeric(bc$sdpclass)

## summary
table(bc$stat)
probDeath <- ddply(bc, .(si, sdpclass), .fun = function(x) {
    data.frame(si = mean(x$si), sdpclass = mean(x$sdpclass),
               total = nrow(unique(x[c("tag","install","plot")])))})

## 6212 unique trees
nrow(unique(bc[c("tag", "install", "plot")]))
## 13970 unique if include sdpclass as well
nrow(unique(bc[c("tag", "install", "plot","sdpclass")]))


tst <- subset(bc, install == 1 & plot == 16 & time == 76)

## For each combination of SDP and SI, compute an average probability of death.
## - Subset by combn, plotden/prior plotden
probDeath <- ddply(bc, .(si,sdpclass), .fun = function(x) {
    byPlot <- ddply(x, .(install, plot), .fun = function(y) {
        data.frame(pDeath = y$plotden/y$pplotden)
    })
    data.frame(si = mean(x$si), sdpclass = mean(x$sdpclass),
               probDeath = 1-mean(byPlot$pDeath, na.rm = TRUE))
})

## Max death is .38 in si == 20 & sdpclass == 3
unique(bc[bc$si == 20 & bc$sdpclass == 3, c("install", "plot")])
## Locations: (4, 17), (7, 6), (7, 8)
range(subset(probDeath, sdpclass == 3)$probDeath)
range(subset(probDeath, sdpclass == 2)$probDeath)
range(subset(probDeath, sdpclass == 1)$probDeath, na.rm = TRUE)


pDeath = ddply(probDeath, .(sdpclass, si), function(x) {
    data.frame(pDeath = mean(x$probDeath, na.rm = TRUE),
               nNA = sum(is.na(x$probDeath)),
               samp = nrow(x)) })
## save pDeath as data.frame to use as predictor of mortality in simulation
write.csv(pDeath, "~/Allometry/data/pDeath-by-sdpclass.csv", row.names=FALSE)










