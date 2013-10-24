
## Hazard parameters
int <- -2.348
LRS <- 1.123
rGR <- -0.549
levsSDP <- c(1,2)
levsSI <- c(1,2,3)
combo <- expand.grid(SDPclass = levsSDP, SIclass = levsSI)
SDP.LRS.pre <- -0.129 ## (pre-thin vs post-thin)
SI.LRS.low <- -0.225 ## (low vs high SI)
SI.LRS.med <- -0.227 ## (medium vs high SI)
SDP.SI.LRS.low.pre <- -0.012 ## (Low SI, and pre-thin. vs. high SI, thin. line)
SDP.SI.LRS.med.pre <- 0.248 ## (Medium SI, pre-thin. vs. high SI, thin. line)
## Create hazard parameter dataframe
hazPars <- data.frame(SIclass = combo$SIclass, SDPclass = combo$SDPclass,
                         intercept = rep(int, nrow(combo)), LRS = rep(LRS, nrow(combo)),
                         rGR = rep(rGR, nrow(combo)), SDP.LRS = rep(1, nrow(combo)),
                         SI.LRS = rep(1, nrow(combo)),
                         SDP.SI.LRS = rep(1, nrow(combo)))
hazPars[hazPars$SIclass == 1,]$SI.LRS <- SI.LRS.low
hazPars[hazPars$SIclass == 2,]$SI.LRS <- SI.LRS.med
hazPars[hazPars$SDPclass == 1,]$SDP.LRS <- SDP.LRS.pre
hazPars[hazPars$SIclass == 1 & hazPars$SDPclass == 1,]$SDP.SI.LRS <-
    SDP.SI.LRS.low.pre
hazPars[hazPars$SIclass == 2 & hazPars$SDPclass == 1,]$SDP.SI.LRS <-
    SDP.SI.LRS.med.pre

## write.csv(hazPars, "C:/home/Allometry/data/hazPars.csv", row.names = FALSE)
