## Make some visuals to show simulation process
source("~/work/neighborhoods/neighborhood-models.R")
source("~/work/functions/functions.R")
source("~/work/simulation/functions-simulation.R")

## data
bc <- read.csv("~/work/data/data/long-bc-derived.csv")
bc$sdpclass <-as.numeric(bc$sdpclass)

## Create a randomly simulated plot
inst = 1
p = 16
yr = 73
tst <- subset(bc, install == inst & plot == p & time == yr)

## original plot
ggplot(tst, aes(x,y, size = bv)) + geom_point() +
    ggtitle(paste0("Original plot: install ", inst, ", plot ", plot, ", time ",
                   time))
ggsave(file = "~/work/simulation/visuals/realPlot.pdf", width=16, height=8)

## simulated plot
simBC <- makeRandomPlot(tst, 100, sizeVar = "bv")
ggplot(simBC, aes(x,y, size = size)) + geom_point() +
    ggtitle(paste0("Plot simulated from install ", inst, ", plot ", plot, ", time ",
                   time))
ggsave(file = "~/work/simulation/visuals/simPlot.pdf", width=16, height=8)

table(mod$time)
ggplot(mod, aes(size)) + geom_histogram() + facet_wrap(~time)
ggplot(mod, aes(time, size, group = tag, color = time)) + geom_path()

#############################################################################
##
## Run forest simulation, adjust parameters in the run-forest script
##
source("~/work/simulation/run-forest.R")

## location plot, sized by growth, colored by year
ggplot(mod, aes(x,y, size = growth, color = time)) + geom_point() +
    ggtitle(paste0("Forest simulated for ", growthCycles, " growth cycles"))
ggsave(file = "~/work/simulation/visuals/sim-xy-by-growth.pdf", width=16, height = 8)

## growth histogram
ggplot(mod, aes(growth)) + geom_density() + facet_wrap(~time) +
    ggtitle(paste0("Density of growth from different growth periods"))
ggsave(file = "~/work/simulation/visuals/sim-growth-density.pdf", width=16, height = 8)

## median growth across time periods
pdf(file = "~/work/simulation/visuals/sim-median-growth.pdf", width = 16, height=8)
plot(medGrowth$time, medGrowth$median, ylim = c(0,.03), main = "Median growth across time periods", xlab = "growth cycle", ylab = "median growth")
points(medGrowth$time, medGrowth$mean, col = "red")
lines(medGrowth$time, medGrowth$median, col = "blue")
lines(medGrowth$time, medGrowth$mean, col = "red")
dev.off()

## Path graph: growth vs size
ggplot(mod, aes(priorsize, growth, color = time, group = tag)) + geom_path() +
    ggtitle("Path of individuals growth across growth cycles") + xlab("size")
ggsave(file = "~/work/simulation/visuals/sim-path-growth-vs-size.pdf", width=16,
       height = 8)
plot(comp$priorsize, comp$growth)
ggplot(mod, aes(time, size,  group = tag)) + geom_path()

## Look at which trees died
ggplot(comp, aes(x,y,color = stat, size = size)) + geom_point()
ggplot(mod, aes(x,y,color = stat, size = size)) + geom_point()

boxplot(bc$rgrsisdp ~ bc$si)
