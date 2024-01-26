pkgname <- "saens"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('saens')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("AIC.eblupres")
### * AIC.eblupres

flush(stderr()); flush(stdout())

### Name: AIC.eblupres
### Title: Akaike's An Information Criterion
### Aliases: AIC.eblupres BIC.eblupres

### ** Examples

m1 <- eblupfh_cluster(y ~ x1 + x2 + x3, data = mys, vardir = "var", cluster = "clust")
AIC(m1)




cleanEx()
nameEx("autoplot.eblupres")
### * autoplot.eblupres

flush(stderr()); flush(stdout())

### Name: autoplot.eblupres
### Title: Autoplot EBLUP results
### Aliases: autoplot.eblupres

### ** Examples

library(saens)

m1 <- eblupfh_cluster(y ~ x1 + x2 + x3, data = mys, vardir = "var", cluster = "clust")
autoplot(m1)




cleanEx()
nameEx("coef.eblupres")
### * coef.eblupres

flush(stderr()); flush(stdout())

### Name: coef.eblupres
### Title: Extract Model Coefficients
### Aliases: coef.eblupres

### ** Examples

m1 <- eblupfh_cluster(y ~ x1 + x2 + x3, data = mys, vardir = "var", cluster = "clust")
coef(m1)




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



cleanEx()
nameEx("logLik.eblupres")
### * logLik.eblupres

flush(stderr()); flush(stdout())

### Name: logLik.eblupres
### Title: Extract Log-Likelihood
### Aliases: logLik.eblupres

### ** Examples

library(saens)

model1 <- eblupfh_cluster(y ~ x1 + x2 + x3, data = mys, vardir = "var", cluster = "clust")
logLik(model1)




cleanEx()
nameEx("summary.eblupres")
### * summary.eblupres

flush(stderr()); flush(stdout())

### Name: summary.eblupres
### Title: Summarizing EBLUP Model Fits
### Aliases: summary.eblupres

### ** Examples

library(saens)

model1 <- eblupfh_cluster(y ~ x1 + x2 + x3, data = mys, vardir = "var", cluster = "clust")
summary(model1)




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
