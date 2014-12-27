library(data.table)
library(plyr)
library(ggplot2)

load(file = 'data/nut_dat.RData')
load(file = 'data/met_dat.RData')
load(file = 'data/wq_dat.RData')
load(file = 'data/met_dat.RData')
load(file = 'data/north_am.Rdata')
load(file = 'data/meta.RData')

sel_parm <- 'sal'
yrs <- c(2000, 2010)

summs_fun(wq_dat, sel_parm, yrs)

# get salinity classes - see Todd's email
# add date selection, min yrs for estimating trends too



