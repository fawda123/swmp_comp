library(reshape2)
library(devtools)
library(plyr)
library(httr)
library(XML)
library(SWMPr)

# create meta ---------------------------------------------------------------------------------

meta <- readr::read_csv('data-raw/sampling_stations.csv') |>
  dplyr::filter(Status == 'Active') |>
  dplyr::select(
    stat = `Station Code`,
    Latitude,
    Longitude
  ) |>
  dplyr::mutate(
    Latitude = as.numeric(Latitude),
    Longitude = -1 * as.numeric(Longitude)
  ) |>
  dplyr::distinct()

save(meta, file = 'data/meta.RData')

# create all_dat ------------------------------------------------------------------------------

# names of files on server
files_s3 <- httr::GET('https://s3.amazonaws.com/swmpagg/')$content
files_s3 <- rawToChar(files_s3)
files_s3 <- htmlTreeParse(files_s3, useInternalNodes = T)
files_s3 <- xpathSApply(files_s3, '//contents//key', xmlValue)

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
  
  dat <- aggreswmp(dat, by =  'months')

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

# remove empty parameters (only in wq)
# llply(wq_dat, function(x) sum(is.na(x$value)) == nrow(x))
wq_dat <- wq_dat[!names(wq_dat) %in% c('cdepth', 'clevel')]

# save output
all_dat <- c(wq_dat, met_dat, nut_dat)
save(all_dat, file = 'data/all_dat.RData')


