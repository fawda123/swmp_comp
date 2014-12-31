
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


