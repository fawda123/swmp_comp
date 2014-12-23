library(data.table)

load(file = 'data/nut_dat.RData')
load(file = 'data/met_dat.RData')
load(file = 'data/wq_dat.RData')
load(file = 'data/met_dat.RData')
load(file = 'data/meta.RData')

sel_parm <- 'chla_n'

nut_sel <- nut_dat[[sel_parm]]
res <- dlply(na.omit(nut_sel), .variables = 'stat', 
  .fun = function(x){
    out <- tryCatch({
      mod <- lm(value ~ datetimestamp, data = x)
      summary(mod)$coefficients['datetimestamp', c(1, 4)]
    }, warning = function(w) w, error = function(e) e)
    if(any(c('warning', 'try-error') %in% class(out))) out <- NA
    c(sign(out[1]), out[2])
  }
)
res <- na.omit(do.call('rbind', res))
res <- data.frame(stat = row.names(res), res)
row.names(res) <- NULL
names(res) <- c('stat', 'sign', 'pval')
res$sign <- factor(res$sign, levels = c('1', '-1'), labels = c('POS', 'NEG'))
res$pval <- cut(res$pval,
  c(-Inf, 0.01, 0.05, +Inf),
  labels=c('p<0.01', 'p<0.05', 'nonsig')
  )
res$lab <- factor(paste0(res$sign, ' (', res$pval, ')'))

res <- data.table(res, key = 'stat')

meta <- data.table(meta, key = 'Station.Code')

# merge locations with data for plotting

# to_plo <- merge(res, meta)

# get salinity classes - see Todd's email
# add date selection, min yrs for estimating trends too

