## Using library gstat
library(gstat)
library(sp)
library(fields)

## create grid
xy <- expand.grid(1:100, 1:100)

## name variables
names(xy) <- c('x','y')

## Defining the spatial model and performing the simulations.
## Second, we define the spatial model as a gstat object:
g.dummy <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1,
                 model=vgm(psill=0.025, range=5, model='Exp'), nmax=20)

## where formula defines the dependent variable (z) as a linear model of independent variables. For ordinary and simple kriging we can use the formula z~1; for simple kriging it is necessary to define a beta parameter too (see below); for universal kriging, if z is linearly dependent on x and y use the formula z~x+y. We are using simple kriging here. locations define the data coordinates, e.g. ~x+y in our case here. dummy is a logical value, and it needs to be TRUE for unconditional simulation. beta is used only for simple kriging, and is a vector with the trend coefficients (including an intercept); if no independent variables are defined the model only contains an intercept, i.e. the simple kriging mean. model defines the variogram model, as defined by a call to vgm. vgm allows defining the (partial) sill, range and nugget paramaters, as well as the variogram model type (e.g. exponential, gaussian, spherical, etc). Anisotropy can also be used. nmax defines the number of nearest observations that should be used for a kriging prediction or simulation.

## Now we are ready to make as many simulations as we like based on the gstat object (four simulations in this example):
yy <- predict(g.dummy, newdata=xy, nsim=4)

## To see one realisation of the simulations:
gridded(yy) = ~x+y
spplot(obj=yy[1])

## All four in single trellis plot
spplot(yy)

## By modifying the range parameter in the variogram model it is possible to control the degree of spatial correlation. For example, by setting it at 15 instead of 5 we get a random field with a ‘coarser’ autocorrelation (Figure 2).
g.dummy1 <- gstat(formula=z~1, locations=~x+y, dummy=T, beta=1, model=vgm(psill=0.025, range=15, model='Exp'), nmax=20)
yy1 <- predict(g.dummy1, newdata = xy, nsim = 4)
gridded(yy1) = ~x+y
spplot(yy1)

## For including a linear trend surface in the simulation we can perform a universal kriging. For doing so it is necessary to specify it in the formula parameter as ~1+x+y, and coefficients for the x and y components need to be specified in the beta parameter. For example, the following defines a model with a spatial trend in the y dimension (Figure 3):
g.dummy2 <- gstat(formula=z~1+x+y, locations=~x+y, dummy=T, beta=c(1,0,0.005), model=vgm(psill=0.025, range=15, model='Exp'), nmax=20)
yy2 <- predict(g.dummy2, newdata = xy, nsim=4)
gridded(yy2) = ~x+y
spplot(yy2)

## The following code defines a model with a trend in both dimensions:
g.dummy3 <- gstat(formula=z~1+x+y, locations=~x+y, dummy=T, beta=c(1,0.01,0.005), model=vgm(psill=0.025, range=15, model='Exp'), nmax=20)
yy3 <- predict(g.dummy3, newdata = xy, nsim = 4)
gridded(yy3) = ~x+y
spplot(yy3)



####################################################################
## Using fields library example
## load Maunga Whau volcano (Mt Eden) elevation dataset (matrix format)
data(volcano)

## reduce size
volcano2 <- volcano[10:55, 14:51]
filled.contour(volcano2, color.palette=terrain.colors, asp=1)
cols <- length(volcano2[1,])
rows <- length(volcano2[,1])

## create dataframe (xyz format)
X <- rep(1:cols, each=rows)
Y <- rep(1:rows, cols)
Z <- as.vector(volcano2)
volcano.df <- data.frame(X,Y,Z,cellid=1:cols*rows)
attach(volcano.df)
quilt.plot(Y,X,Z,nrow=rows,ncol=cols,add=F)

## create a spatial autocorrelation signature
## coordinate list
coords <- data.frame(X,Y)

## distance matrix
dist <- as.matrix(dist(coords))

## create a correlation structure (exponential)
str <- -0.1 ## strength of autocorrelation, inv. proportional to str
omega1 <- exp(str*dist)

## calculate correlation weights, and invert weights matrix
weights <- chol(solve(omega1))
weights_inv <- solve(weights)

## create an autocorrelated random field
set.seed(1011)
error <- weights_inv %*% rnorm(dim(dist)[1])
quilt.plot(Y,X,error,nrow=rows,ncol=cols,add=F)

## create a variable as a linear function of the elevation
a <- 10
b <- 0.5
Z2 <- a + b*Z
quilt.plot(Y,X,Z2,nrow=rows,ncol=cols,add=F)

## add the autocorrelated error to the new variable
Z3 <- Z2 + error
quilt.plot(Y,X,Z3,nrow=rows,ncol=cols,add=F)





