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
summs_fun <- function(dat_in, yrs = NULL, sumby = 'yrs_summ', meta_in = meta){
  
  # use all years if missing
  if(is.null(yrs)) yrs <- as.numeric(range(dat_in$year))
  
  # subset by parameter and year range
  dat_sel <- dat_in[dat_in$year >= yrs[1] & dat_in$year <= yrs[2], ]
  
  if(sumby == 'yrs_summ'){
    # aggregate by years to match plots
    dat_sel <- aggregate(value ~ stat + year, data = dat_sel, function(x) mean(x, na.rm = T))
    dat_sel <- dat_sel[order(dat_sel$stat), ]
    
    # create continuous yr vector based on input
    years <- data.frame(year = seq(yrs[1], yrs[2]))
    dat_sel <- merge(years, dat_sel, by = 'year', all.x = T)
    dat_sel$year <- as.numeric(as.character(dat_sel$year))
    
    # assign year to summary column
    dat_sel$summ_val <- dat_sel$year
    
  
  } else {
    
    # otherwise summary column is timestamp (defaults to months)
    dat_sel$year <- as.numeric(as.character(dat_sel$year))
    dat_sel$summ_val <- dat_sel$datetimestamp
    
  }
  
  # organize for regression 
  # gets mean and anomalous value of parameter by period
  # creates continous annual data
  dat_org <- dlply(dat_sel, .variables = 'stat',
    .fun = function(x){
      
      # avg and anoms across period
      val_avg <- mean(x$value, na.rm = T)
      x$anom <- x$value - val_avg
      
      return(x)
        
    })
  
  # process data running regression model
  res <- llply(dat_org, 
    .fun = function(x){
      
      out <- tryCatch({
       
        # create mod              
        x$value <- scale(x$value, scale = F, center = T)
        mod <- lm(value ~ summ_val, data = x, na.action = na.omit)
        sign_out <- sign(coef(mod)['summ_val'])
        pval <- summary(mod)$fstatistic
        pval <- pf(pval[1], pval[2], pval[3], lower.tail = F)
        c(sign_out, pval)
      }, warning = function(w) w, error = function(e) e)
      
      if(any(c('warning', 'try-error', 'error', 'simpleError') %in% class(out))) out <- NA
      
      c(out[1], out[2])
    }
  )
  
  # format output list for res
  res <- na.omit(do.call('rbind', res))
  res <- data.frame(stat = row.names(res), res)
  row.names(res) <- NULL
  names(res) <- c('stat', 'sign', 'pval')
  
  # get map marker data from map_marks function
  to_plo <- map_marks(res)
  
  # return raw and modelled data
  out <- list(
    raw_dat = dat_org, 
    res = to_plo
    )
  return(out)

}

#' Create summary plot of seasonal, annual variation for a single reserve and parameter
plot_summary <- function(dat_in, param, stat, years, trend_in){
  
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
  
  ##
  # plots
  
  # universal plot setting
  my_theme <- theme(axis.text = element_text(size = 8))

  # need to conver year limits to date if months are used  
  if(class(dat_in$summ_val) %in% 'Date'){
    years <- as.Date(as.character(years), origin = c('1970-01-01'), 
      format = '%Y')
  }
    
  p <- ggplot(dat_in, aes(x = summ_val, y = anom, group = 1, fill = anom)) +
    geom_bar(stat = 'identity') +
    scale_fill_gradient2(name = ylab,
                         low = 'lightblue', mid = 'lightgreen', high = 'tomato', midpoint = 0
    ) +
    stat_smooth(method = 'lm', se = F, linetype = 'dashed', 
      size = 1,color = 'black') +
    theme_classic() +
    ylab('Anomalies from long-term average') +
    xlab('') +
    xlim(years[1], years[2]) +
    theme(legend.position = 'none') +
    ggtitle(paste(stat, ylab, trend_in, sep = ', ')) + 
    my_theme
  
  ##
  # output plot
  p
  
}

######
#' Get colors and shapes for plotting map symbols
#'
#' Get colors and shapes for plotting map symbols based on significance and direction of parameter trends over time
#' 
#' @param res_in data frame of results from linear regression of parameter trends by year
#' @param pval_cut numeric vector of values to cut pvalues for plotting
#' @param chr string of radii for color markers in map used for p-value significance
#' @param pval_lab chr string of p values labels
#' @param dir_lab chr string of lables for direction of trend
#' @param meta_in data table of station locations for plotting
#' @param cols numeric vector of colors for plotting, used to index  \code{colors()}
#' 
#' @return a \code{\link[data.table]{data.table}} of station summary data for plotting showing significance of trends over time
map_marks <- function(res_in, pval_cut = c(-Inf, 0.01, 0.05, +Inf), 
  pval_radius = c('25', '17', '10'), pval_lab = c('p<0.01', 'p<0.05', 'nonsig'),  dir_lab = c('POS', 'NEG'), meta_in = meta, cols = c(520, 523, 522, 503, 507, 506)) {
  
  if(!any(names(res_in) %in% c('stat', 'sign', 'pval')))
    stop('Incorrect naming convention for markers to plot')
  
  if(length(pval_cut) != 1 + length(pval_radius))
    stop('Cut dimensions must equal 1 plus dimensions of marker radius')

  if(length(pval_radius) != length(pval_lab))
    stop('Label dimensions must equal radius dimensions')

  # change sign levels to POS/NEG
  res_in$sign <- factor(res_in$sign, levels = c('1', '-1'), 
    labels = dir_lab)
  
  # set pval radius
  res_in$pval_rad <- as.numeric(as.character(cut(res_in$pval,
    pval_cut,
    labels=pval_radius
  )))
  res_in$pval <- cut(res_in$pval,
    pval_cut,
    labels=pval_lab
    )
  res_in$lab <- factor(paste0(res_in$sign, ' (', res_in$pval, ')'))
  
  # get colors, have to do this because sometimes colors are subset
  col_lookup <- expand.grid(dir_lab, pval_lab, stringsAsFactors = F)
  col_lookup <- paste0(col_lookup[, 1], ' (', col_lookup[, 2], ')')
  col_lookup <- data.frame(lab = sort(col_lookup), cols = cols, stringsAsFactors = F)
  labs <- levels(res_in$lab)
  col_lookup <- col_lookup[col_lookup$lab %in% labs, 'cols']
  
  res_in$cols <- factor(res_in$lab, levels = labs, 
    labels = colors()[col_lookup])
  res_in$cols <- as.character(res_in$cols)
  
  # merge locations with data for plotting
  res_in <- data.table(res_in, key = 'stat')
  to_plo <- merge(res_in, meta_in, by = 'stat')
 
  return(to_plo)

}