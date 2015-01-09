######
#' summmarize trends in SWMP data parameters across reserves
#'
#' @param dat_in summarized time series of selected SWMP data parameter for all stations
#' @param chr of parameter to compare
#' @param two element vector of years to subset data
#' @param meta_in is data frame of station metadata, including lat/long 
#' @param cols is numeric vector of colors for plotting, correspond to indices in colors()
#
#' @return ggplot object
summs_fun <- function(dat_in, yrs = NULL, meta_in = meta, cols = c(520, 522, 523, 503, 506, 507)){
  
  # use all years if missing
  if(is.null(yrs)) yrs <- range(dat_in$year)
  
  # subset by parameter and year range
  dat_sel <- dat_in[dat_in$year >= yrs[1] & dat_in$year <= yrs[2], ]
  
  # process on non-NA data
  res <- dlply(na.omit(dat_sel), .variables = 'stat', 
    .fun = function(x){
      out <- tryCatch({
        mod <- lm(value ~ datetimestamp, data = x)
        summary(mod)$coefficients['datetimestamp', c(1, 4)]
      }, warning = function(w) w, error = function(e) e)
      if(any(c('warning', 'try-error', 'error', 'simpleError') %in% class(out))) out <- NA
      c(sign(out[1]), out[2])
    }
  )
  res <- na.omit(do.call('rbind', res))
  res <- data.frame(stat = row.names(res), res)
  row.names(res) <- NULL
  names(res) <- c('stat', 'sign', 'pval')
  res$sign <- factor(res$sign, levels = c('1', '-1'), labels = c('POS', 'NEG'))
  res$pval_rad <- as.numeric(as.character(cut(res$pval,
    c(-Inf, 0.01, 0.05, +Inf),
    labels=c('20', '10', '5')
  )))
  res$pval <- cut(res$pval,
    c(-Inf, 0.01, 0.05, +Inf),
    labels=c('p<0.01', 'p<0.05', 'nonsig')
    )
  res$lab <- factor(paste0(res$sign, ' (', res$pval, ')'))
  
  res <- data.table(res, key = 'stat')
  
  # merge locations with data for plotting
  to_plo <- merge(res, meta_in, by = 'stat')
  
  # set legend based on unique values in results
  leg_look <- data.frame(
    lab = c('NEG (nonsig)', 'NEG (p<0.05)', 'NEG (p<0.01)', 'POS (nonsig)', 
      'POS (p<0.05)', 'POS (p<0.01)'),
    cols = colors()[cols],
    shps = c(rep(25, 3), rep(24, 3))
  )
  leg_nms <- leg_look$lab %in% levels(res$lab)  
  leg_cols <- as.character(leg_look[leg_nms, 'cols'])
  leg_shps <- leg_look[leg_nms, 'shps']
    
  # merge w/ leg_look for colors, shapes
  out <- data.frame(merge(to_plo, leg_look, by = 'lab', all.x = T))
  out$cols <- as.character(out$cols)
  
  # return summary data
  return(out)

}

#' Create summary plot of seasonal, annual variation for a single reserve and parameters
#' Taken from SwMPr package, same input with addition of stat, removed aggregate stuff
#' 
plot_summary <- function(dat_in, param, stat, years, ...){
  
  # select years to plot
  dat_plo <- data.frame(dat_in[dat_in$year %in% seq(years[1], years[2]), ])
  
  # label lookups
  lab_look <- list(
    temp = 'Temperature (C)', 
    spcond = 'Specific conductivity (mS/cm)',
    sal = 'Salinity (psu)',
    do_pct = 'Dissolved oxyxgen (%)',
    do_mgl = 'Dissolved oxygen (mg/L)',
    depth = 'Depth (m)',
    cdepth = 'Depth (nonvented, m)',
    level = 'Referenced depth (m)',
    clevel = 'Referenced depth (nonvented, m)',
    ph = 'pH',
    turb = 'Turbidity (NTU)',
    chlfluor = 'Chl fluorescence (ug/L)',
    atemp = 'Air temperature (C)',
    rh = 'Relative humidity (%)',
    bp = 'Barometric pressure (mb)',
    wspd = 'Wind speed (m/s)',
    maxwspd = 'Max wind speed (m/s)',
    wdir = 'Wind direction (degrees)',
    sdwdir = 'Wind direction (sd, degrees)',
    totpar = 'Total PAR (mmol/m2)',
    totprcp = 'Total precipitation (mm)',
    cumprcp = 'Cumulative precipitation (mm)',
    totsorad = 'Total solar radiation (watts/m2)',
    po4f = 'Orthophosphate (mg/L)', 
    nh4f = 'Ammonium (mg/L)',
    no2f = 'Nitrite (mg/L)',
    no3f = 'Nitrate (mg/L)',
    no23f = 'Nitrite + Nitrate (mg/L)',
    chla_n = 'Chlorophyll (ug/L)'
  )
  ylab <- lab_look[[param]]
  
  # annual agg
  form_in <- formula(paste0(param, ' ~ year'))
  agg_fun <- function(x) mean(x, na.rm = T)
  yr_agg <- plyr::ddply(dat_plo, .variables = 'year', 
                        .fun = function(x) mean(x[, param], na.rm = T))
  names(yr_agg)[names(yr_agg) %in% 'V1'] <- param
  
  ##
  # plots
  
  # universal plot setting
  my_theme <- theme(axis.text = element_text(size = 8))
  
  # annual anomalies
  yr_avg <- mean(yr_agg[, param], na.rm = T)
  yr_agg$anom <- yr_agg[, param] - yr_avg
  
  # create continuous yr vector based on input
  years <- data.frame(year = seq(years[1], years[2]))
  yr_agg <- merge(years, yr_agg, by = 'year', all.x = T)
  yr_agg$year <- as.character(yr_agg$year)
  
  p <- ggplot(yr_agg, aes(x = year, y = anom, group = 1, fill = anom)) +
    geom_bar(stat = 'identity') +
    scale_fill_gradient2(name = ylab,
                         low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = 0
    ) +
    stat_smooth(method = 'lm', se = F, linetype = 'dashed', size = 1) +
    theme_classic() +
    ylab('Annual anomalies') +
    xlab('') +
    theme(legend.position = 'none') +
    my_theme
  
  ##
  # combine plots
  suppressWarnings(gridExtra::grid.arrange(
    p
  ))
  
}


