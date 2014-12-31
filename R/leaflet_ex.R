library(leaflet)
library(htmltools)
load('data/north_am.RData')
load('data/meta.RData')
load('data/all_dat.RData')

to_plo <- all_dat[[4]]
to_plo <- summs_fun(to_plo, meta_in = meta, poly_in = north_am, noplot = T)
to_plo$pop_txt <- with(to_plo, paste0(stat, ', ', lab))

col_vec <- unique(to_plo[, c('lab', 'cols')])
col_vec <- colorFactor(palette = col_vec$cols, levels = col_vec$lab)

m <- leaflet(to_plo) %>% addTiles()
m %>%  addCircleMarkers(lat = ~Latitude, lng = ~Longitude, radius = ~pval_rad,
                        color = ~col_vec(lab), opacity = 0.8, 
                        popup = ~htmlEscape(pop_txt))
