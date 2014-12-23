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

# separate objects for nut, met, wq
nut_dat <- out_ls[grep('nut.RData$', names(out_ls))]
met_dat <- out_ls[grep('met.RData$', names(out_ls))]
wq_dat <- out_ls[grep('wq.RData$', names(out_ls))]

# rearrange, split into list by parameters
nut_dat <- do.call('rbind', nut_dat)
nut_dat$stat <- gsub('\\.RData\\.[0-9]*$', '', row.names(nut_dat))
nut_dat <- melt(nut_dat, id.var = c('datetimestamp', 'stat'))
nut_dat$year <- strftime(nut_dat$datetimestamp, '%Y')
nut_dat <- split(nut_dat, nut_dat$variable)

# rearrange, split into list by parameters
met_dat <- do.call('rbind', met_dat)
met_dat$stat <- gsub('\\.RData\\.[0-9]*$', '', row.names(met_dat))
met_dat <- melt(met_dat, id.var = c('datetimestamp', 'stat'))
met_dat$year <- strftime(met_dat$datetimestamp, '%Y')
met_dat <- split(met_dat, met_dat$variable)

# rearrange, split into list by parameters
wq_dat <- do.call('rbind', wq_dat)
wq_dat$stat <- gsub('\\.RData\\.[0-9]*$', '', row.names(wq_dat))
wq_dat <- melt(wq_dat, id.var = c('datetimestamp', 'stat'))
wq_dat$year <- strftime(wq_dat$datetimestamp, '%Y')
wq_dat <- split(wq_dat, wq_dat$variable)

# save output
save(nut_dat, file = 'data/nut_dat.RData')
save(met_dat, file = 'data/met_dat.RData')
save(wq_dat, file = 'data/wq_dat.RData')


