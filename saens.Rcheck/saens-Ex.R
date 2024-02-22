pkgname <- "saens"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('saens')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("eblupfh")
### * eblupfh

flush(stderr()); flush(stdout())

### Name: eblupfh
### Title: EBLUPs based on a Fay-Herriot Model.
### Aliases: eblupfh

### ** Examples

library(saens)

m1 <- eblupfh(y ~ x1 + x2 + x3, data = na.omit(mys), vardir = "var")
m1 <- eblupfh(y ~ x1 + x2 + x3, data = na.omit(mys), vardir = ~var)




cleanEx()
nameEx("eblupfh_cluster")
### * eblupfh_cluster

flush(stderr()); flush(stdout())

### Name: eblupfh_cluster
### Title: EBLUPs based on a Fay-Herriot Model with Cluster Information.
### Aliases: eblupfh_cluster

### ** Examples

library(saens)

m1 <- eblupfh_cluster(y ~ x1 + x2 + x3, data = mys, vardir = "var", cluster = "clust")
m1 <- eblupfh_cluster(y ~ x1 + x2 + x3, data = mys, vardir = ~var, cluster = ~clust)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
