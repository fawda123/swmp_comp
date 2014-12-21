library(reshape2)
library(devtools)
library(plyr)
load_all('M:/docs/SWMPr')

# aggregate all data to monthly for quick comps
out_ls <- vector('list', length = length(files_s3))
names(out_ls) <- files_s3
for(fl in files_s3){
  
  cat(fl, '\n')
  stat <- gsub('\\.RData$', '', fl)
  raw_content <- paste0('https://s3.amazonaws.com/swmpagg/', stat, '.RData')
  raw_content <- httr::GET(raw_content)$content
  connect <- rawConnection(raw_content)
  load(connect)
  dat <- get(stat)
  rm(list = stat)
  close(connect) 
  
  dat <- aggregate(dat, by =  'months')

  out_ls[[fl]] <- dat
  
}

nut_dat <- out_ls[grep('nut.RData$', names(out_ls))]
met_dat <- out_ls[grep('met.RData$', names(out_ls))]
wq_dat <- out_ls[grep('wq.RData$', names(out_ls))]

nut_dat <- do.call('rbind', nut_dat)
nut_dat$stat <- gsub('\\.RData\\.[0-9]*$', '', row.names(nut_dat))
nut_dat <- melt(nut_dat, id.var = c('datetimestamp', 'stat'))
nut_dat$year <- strftime(nut_dat$datetimestamp, '%Y')
nut_dat <- split(nut_dat, nut_dat$variable)

sel_parm <- 'no2f'

nut_sel <- nut_dat[[sel_parm]]
ddply(na.omit(nut_sel), .variable = 'stat', 
  .fun = function(x) lm(value ~ datetimestamp, data = na.omit(x))
)

# get salinity classes - see Todd's email
# add date selection, min yrs for estimating trends too


