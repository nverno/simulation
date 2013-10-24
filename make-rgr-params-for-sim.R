## Take the rgr params dataset created during the rgr fitting by SI and SDP and
##  just transform it so it only contains a, b values /install/plot/sdpclass/si
rgrs <- read.csv("~/Allometry/data/rgr-parameters-sisdp.csv")
table(rgrs$sdpclass)
nrow(bc[bc$stat == "ALIVE" & bc$bvgrowth >= 0 & !is.na(bc$bvgrowth),])

rgr <- rbind.fill(ddply( rgrs, .(install, plot, sdpclass, si), .fun = function(x) {
    x <- droplevels(x)
    data.frame(install = mean(x$install), plot = mean(x$plot),
               a = mean(x$top.a), b = mean(x$top.b), sdpclass = mean(x$sdpclass),
               si = mean(x$si))
}))
write.csv(rgr, "~/Allometry/data/rgr-params-for-sim.csv", row.names = FALSE)
