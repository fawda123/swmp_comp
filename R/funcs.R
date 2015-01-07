######
#' summmarize trends in SWMP data parameters across reserves
#'
#' @param dat_in summarized time series of selected SWMP data parameter for all stations
#' @param chr of parameter to compare
#' @param two element vector of years to subset data
#' @param meta_in is data frame of station metadata, including lat/long 
#' @param poly_in is SpatialPolygonsDataFrame of north america
#' @param noplot is logical if summary data should be returned as output, default F
#' @param cols is numeric vector of colors for plotting, correspond to indices in colors()
#
#' @return ggplot object
summs_fun <- function(dat_in, yrs, meta_in = meta, poly_in = north_am, noplot = F, cols = c(520, 522, 523, 503, 506, 507)){
  
  # use all years if missing
  if(missing(yrs)) yrs <- range(dat_in$year)
  
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
  
  # return summary data if T
  if(noplot){
    
    # merge w/ leg_look for colors, shapes
    out <- data.frame(merge(to_plo, leg_look, by = 'lab', all.x = T))
    out$cols <- as.character(out$cols)
    return(out)
    
  }
  
  # base plot
  poly_in <- fortify(poly_in)
  
  p <- ggplot(poly_in, aes(x = long, y = lat)) + 
    geom_polygon(aes(group = group), colour = 'lightgrey', fill = 'lightgrey') + 
    coord_equal() +
    theme_classic() + 
    geom_point(data = to_plo, aes(x = Longitude, y = Latitude, 
      shape = lab, fill = lab), size = 5) + 
    scale_shape_manual('trends', values = leg_shps) + 
    scale_fill_manual('trends', values = leg_cols) +
    theme(legend.position = 'bottom', axis.title = element_blank(), legend.title = element_blank())
  
  p

}

#' Create summary plot of seasonal, annual variation for a single reserve and parameters
#' Taken from SwMPr package, same input with addition of stat, removed aggregate stuff
#' 
plot_summary <- function(dat_in, param, stat, years, ...){

  mo_labs <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
  mo_levs <- c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')
  
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
  
  # monthly, annual aggs
  agg_fun <- function(x) mean(x, na.rm = T)
  form_in <- formula(paste0(param, ' ~ month'))
  mo_agg <- aggregate(form_in, data = dat_plo[, !names(dat_plo) %in% c('datetimestamp', 'year')], FUN = agg_fun)
  mo_agg_med <- aggregate(form_in, data = dat_plo[, !names(dat_plo) %in% c('datetimestamp', 'year')], FUN = function(x) median(x, na.rm = T))
  form_in <- formula(paste0(param, ' ~ year'))
  yr_agg <- aggregate(form_in, data = dat_plo[, !names(dat_plo) %in% c('datetimestamp', 'month')], FUN = agg_fun, na.action = na.pass)
  
  ##
  # plots
  
  # universal plot setting
  my_theme <- theme(axis.text = element_text(size = 8))
  
  # plot 1 - means and obs
  cols <- colorRampPalette(c('lightblue', 'lightgreen'))(nrow(mo_agg))
  cols <- cols[rank(mo_agg[, param])]
  p1 <- ggplot(dat_plo, aes_string(x = 'month', y = param)) +
    geom_point(size = 2, alpha = 0.5, 
               position=position_jitter(width=0.1)
    ) +
    theme_classic() +
    ylab(ylab) + 
    xlab('Monthly distributions and means') +
    geom_point(data = mo_agg, aes_string(x = 'month', y = param), 
               colour = 'darkgreen', fill = cols, size = 7, pch = 21) + 
    my_theme
  
  # box aggs, colored by median
  cols <- colorRampPalette(c('lightblue', 'lightgreen'))(nrow(mo_agg_med))
  cols <- cols[rank(mo_agg_med[, param])]
  p2 <- ggplot(dat_plo, aes_string(x = 'month', y = param)) + 
    geom_boxplot(fill = cols) +
    theme_classic() +
    ylab(ylab) + 
    xlab('Monthly distributions and medians') +
    my_theme
  
  # month histograms
  to_plo <- dat_plo
  to_plo$month <- factor(to_plo$month, levels = rev(mo_levs), labels = rev(mo_labs))
  p3 <- ggplot(to_plo, aes_string(x = param)) + 
    geom_histogram(aes(y = ..density..), colour = 'lightblue', binwidth = diff(range(to_plo[, param], na.rm = T))/30) + 
    facet_grid(month ~ .) + 
    xlab(ylab) +
    theme_bw(base_family = 'Times') + 
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          strip.text.y = element_text(size = 8, angle = 90),
          strip.background = element_rect(size = 0, fill = 'lightblue')) +
    my_theme
  
  # monthly means by year
  to_plo <- plyr::ddply(dat_plo, 
                        .variables = c('month', 'year'), 
                        .fun = function(x) mean(x[, param],  na.rm = T)
  )
  to_plo$month <- factor(to_plo$month, labels = mo_labs, level = mo_levs)
  midpt <- mean(to_plo$V1, na.rm = T)
  p4 <- ggplot(to_plo, aes(x = year, y = month, fill = V1)) +
    geom_tile() +
    scale_fill_gradient2(name = ylab,
                         low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = midpt) +
    theme_classic() +
    ylab('Monthly means') +
    xlab('') +
    theme(legend.position = 'top', legend.title = element_blank()) +
    guides(fill = guide_colorbar(barheight = 0.5)) +
    my_theme
  
  # monthly anomalies
  mo_agg$month <- factor(mo_agg$month, labels = mo_labs, levels = mo_levs)
  to_plo <- merge(to_plo, mo_agg, by = 'month', all.x = T)
  names(to_plo)[names(to_plo) %in% param] <- 'trend'
  to_plo$anom <- with(to_plo, V1 - trend)
  rngs <- max(abs(range(to_plo$anom, na.rm = T)))
  p5 <- ggplot(to_plo, aes(x = year, y = month, fill = anom)) +
    geom_tile() +
    scale_fill_gradient2(name = ylab,
                         low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = 0,
                         limits = c(-1 * rngs, rngs)) +
    theme_classic() +
    ylab('Monthly anomalies') +
    xlab('') +
    theme(legend.position = 'top', legend.title = element_blank()) +
    guides(fill = guide_colorbar(barheight= 0.5)) +
    my_theme
  
  # annual anomalies
  yr_avg <- mean(yr_agg[, param], na.rm = T)
  yr_agg$anom <- yr_agg[, param] - yr_avg
  p6 <- ggplot(yr_agg, aes(x = year, y = anom, group = 1, fill = anom)) +
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
    arrangeGrob(p1, p2, ncol = 1), 
    p3, 
    arrangeGrob(p4, p5, p6, ncol = 1, heights = c(1, 1, 0.8)), 
    ncol = 3, widths = c(1, 0.5, 1)
  ))
  
}


