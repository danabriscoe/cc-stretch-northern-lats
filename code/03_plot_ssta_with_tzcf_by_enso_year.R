# 03_plot_tzcf_by_enso.R

# 01
library(tidyverse)
library(data.table)
library(ggplot2)
library(plotly)
library(leaflet)
library(mapview)
library(leafsync)
library(leafgl)
library(sf)
library(kableExtra)
library(knitr)
# 02
library(lubridate)
library(scales)
# 03 
library(RColorBrewer)
library(fields)
library(colorRamps)
require(latticeExtra)
require(zoo)
suppressMessages(library(raster))
library(glue)

# 05
library(tidyverse)
library(data.table)
library(mgcv)
library(ggalt)
library(scales)
library(plotly)
library("gratia")

library(hrbrthemes)

source('./code/00_northern_lats_helper_functions.R')


## set ncpath ----
nc_path = "/Users/briscoedk/dbriscoe@stanford.edu - Google Drive/My Drive/ncdf/npac"

## set ras params ----
params <- tibble(
    eov = "sst",
    timestep = "month",
    # min_dt = '1997-07-16',
    min_dt = '1985-01-16',
    max_dt = '2023-10-16')

## set spatial extents ----
bbox <- c(make360(-170), make360(-110), 20, 50)
e <- extent(bbox[1], bbox[2], bbox[3], bbox[4])

bbox180 <- c(-170, -110, 20, 50)

tzcf_bbox <- c(make360(-160), make360(-130), 25, 50)
e_tzcf <- extent(tzcf_bbox[1], tzcf_bbox[2], tzcf_bbox[3], tzcf_bbox[4])


## load sst and ssta rasters ----
### sst ----
sst_ncs <- list.files(nc_path, pattern = str_c("sst_"), full.names=T) %>%
    .[grep(params$timestep, .)]


sst_fdates <- sst_ncs %>% 
    str_split(., "_") %>%
    purrr::map_chr(~ pluck(., 6)) %>%
    substr(., start=1, stop=10)


sst_ras_stack <- #ncs[fdates >= params$min_dt & fdates <= params$max_dt] %>%
    sst_ncs %>%
    raster::stack(.) %>%
    crop(., e)


### ssta ----
ssta_ncs <- list.files(nc_path, pattern = str_c("ssta_"), full.names=T) %>%
    .[grep(params$timestep, .)]


ssta_fdates <- ssta_ncs %>% 
    str_split(., "_") %>%
    purrr::map_chr(~ pluck(., 6)) %>%
    substr(., start=1, stop=10)


ssta_ras_stack <- #ncs[fdates >= params$min_dt & fdates <= params$max_dt] %>%
    ssta_ncs %>%
    raster::stack(.) %>%
    crop(., e)


## subset rasters by year, month -----
stopifnot(sst_fdates == ssta_fdates)

# assign ras names
sst_ras_names_yrmon <- as.yearmon(sst_fdates)
ssta_ras_names_yrmon <- as.yearmon(ssta_fdates)


sst_ras_stack <- setZ(sst_ras_stack, sst_ras_names_yrmon)
ssta_ras_stack <- setZ(ssta_ras_stack, ssta_ras_names_yrmon)

names(sst_ras_stack) <- as.character(sst_ras_names_yrmon)  
names(ssta_ras_stack) <- as.character(ssta_ras_names_yrmon)  

# get unique years from time attribute
years <- getZ(sst_ras_stack) |> format("%Y") |> unique()
mons <- getZ(sst_ras_stack) |> format("%b") |> unique()

dte <- getZ(sst_ras_stack)
m <- format(dte, "%b")
y <- format(dte, "%Y")

## pull sept rasters only ----
sept_sst_ras_stack <- raster::subset(sst_ras_stack, which(m=="Sep"))
sept_ssta_ras_stack <- raster::subset(ssta_ras_stack, which(m=="Sep"))

# # function
# subset_by_year <- function(year, stack) {
#     subset(stack, grep(year, names(stack)))
# }
# 
# # subset stack into a list by year
# ras_stack_by_year_list <- lapply(years, subset_by_year, stack = ras_stack)

sept_idx <- which(m=="Sep")
sept_sst_ras_stack <- setZ(sept_sst_ras_stack, as.yearmon(sst_fdates[sept_idx]))
sept_ssta_ras_stack <- setZ(sept_ssta_ras_stack, as.yearmon(ssta_fdates[sept_idx]))

names(sept_sst_ras_stack) <- as.character(sst_fdates[sept_idx]) 
names(sept_ssta_ras_stack) <- as.character(ssta_fdates[sept_idx])  


## Create all-time sept ras means (for tzcf location comparison) -----
sept_sst_ras_mean_1985_2023 <- sept_sst_ras_stack%>%
    calc(., mean, na.rm=TRUE)
names(sept_sst_ras_mean_1985_2023) <- c('X2023-09-16')

sept_ssta_ras_mean_1985_2023 <- sept_ssta_ras_stack%>%
    calc(., mean, na.rm=TRUE)
names(sept_ssta_ras_mean_1985_2023) <- c('X2023-09-16')

sept_sst_ras_mean_1985_2023_df <- sept_sst_ras_mean_1985_2023 %>% ras2df()
sept_ssta_ras_mean_1985_2023_df <- sept_ssta_ras_mean_1985_2023 %>% ras2df()


## assign sept ras to enso years ----
# enso_yr <- data.frame(year = seq(1985,2023,1)) %>%
#     mutate(enso = case_when((year == 1987 | year == 1988 | year == 1992 | year == 1995 | year == 1998 | year == 2003 | year == 2007 | year == 2010 | year == 2016 | year == 2023) ~ "el_nino",
#                                          (year == 1989 | year == 1999 | year == 2000 | year == 2008 | year == 2011 | year == 2012 | year == 2021 | year == 2022) ~ "la_nina",
#                                          (year == 2014 | year == 2015 | year == 2019 | year == 2020) ~ "mhw",    
#                                           TRUE ~ "neutral"))


## pre-cond winter yrs
# source: https://ggweather.com/enso/oni.htm
enso_yr <- data.frame(year = seq(1985,2023,1)) %>%
    mutate(enso = case_when((year == 1986 | year == 1987 | year == 1991 | year == 1994 | year == 1997 | 
                                 year == 2002 | year == 2004 | year == 2006 | year == 2009 | year == 2015 | year == 2018 | year == 2023) ~ "el_nino",
                            
                            (year == 1988 | year == 1995 | year == 1998 | year == 1999 | 
                                 year == 2000 | 
                                 # year == 2005 | 
                                 year == 2007 | 
                                 # year == 2008 | 
                                 year == 2010 | year == 2011 | year == 2016 | year == 2017 | 
                                 year == 2020 | year == 2021 | year == 2022) ~ "la_nina",
                            
                            (year == 2014 | year == 2015 | year == 2019 | year == 2020) ~ "mhw",    
                            TRUE ~ "neutral"))



sept_dte <- getZ(sept_sst_ras_stack)
sept_y <- format(sept_dte, "%Y")

## assign el nino rasters by year
# el_idx <- sept_y[sept_y %in% enso_yr$year[enso_yr$enso == "el_nino"]]

### El Nino 
el_sept_sst_ras_stack <- raster::subset(sept_sst_ras_stack, 
                                 which(substr(getZ(sept_sst_ras_stack),5,8)%in% enso_yr$year[enso_yr$enso == "el_nino"]))

el_sept_ssta_ras_stack <- raster::subset(sept_ssta_ras_stack, 
                                        which(substr(getZ(sept_ssta_ras_stack),5,8)%in% enso_yr$year[enso_yr$enso == "el_nino"]))


### La Nina
la_sept_sst_ras_stack <- raster::subset(sept_sst_ras_stack, 
                                        which(substr(getZ(sept_sst_ras_stack),5,8)%in% enso_yr$year[enso_yr$enso == "la_nina"]))

la_sept_ssta_ras_stack <- raster::subset(sept_ssta_ras_stack, 
                                         which(substr(getZ(sept_ssta_ras_stack),5,8)%in% enso_yr$year[enso_yr$enso == "la_nina"]))


### MHW
mhw_sept_sst_ras_stack <- raster::subset(sept_sst_ras_stack, 
                                        which(substr(getZ(sept_sst_ras_stack),5,8)%in% enso_yr$year[enso_yr$enso == "mhw"]))

mhw_sept_ssta_ras_stack <- raster::subset(sept_ssta_ras_stack, 
                                         which(substr(getZ(sept_ssta_ras_stack),5,8)%in% enso_yr$year[enso_yr$enso == "mhw"]))

### Neutral
neutral_sept_sst_ras_stack <- raster::subset(sept_sst_ras_stack, 
                                        which(substr(getZ(sept_sst_ras_stack),5,8)%in% enso_yr$year[enso_yr$enso == "neutral"]))

neutral_sept_ssta_ras_stack <- raster::subset(sept_ssta_ras_stack, 
                                         which(substr(getZ(sept_ssta_ras_stack),5,8)%in% enso_yr$year[enso_yr$enso == "neutral"]))


## convert ras to dfs for plotting -----
el_sept_sst_df  <- el_sept_sst_ras_stack %>% ras2df() %>% mutate(lon = make180(lon)) %>% mutate(year = year(date))
el_sept_ssta_df <- el_sept_ssta_ras_stack %>% ras2df() %>% mutate(lon = make180(lon)) %>% mutate(year = year(date))

la_sept_sst_df  <- la_sept_sst_ras_stack %>% ras2df() %>% mutate(lon = make180(lon)) %>% mutate(year = year(date))
la_sept_ssta_df <- la_sept_ssta_ras_stack %>% ras2df() %>% mutate(lon = make180(lon)) %>% mutate(year = year(date))

mhw_sept_sst_df  <- mhw_sept_sst_ras_stack %>% ras2df() %>% mutate(lon = make180(lon)) %>% mutate(year = year(date))
mhw_sept_ssta_df <- mhw_sept_ssta_ras_stack %>% ras2df() %>% mutate(lon = make180(lon)) %>% mutate(year = year(date))

neutral_sept_sst_df  <- neutral_sept_sst_ras_stack %>% ras2df() %>% mutate(lon = make180(lon)) %>% mutate(year = year(date))
neutral_sept_ssta_df <- neutral_sept_ssta_ras_stack %>% ras2df() %>% mutate(lon = make180(lon)) %>% mutate(year = year(date))


## set plot params ----

mapdata <- map_data('world')

grid_theme <- theme_bw() + theme(panel.ontop=TRUE, panel.background=element_blank())


ssta_pal <- rev(c("#540b0e", rev(brewer.pal(9, "YlOrRd")),"white", brewer.pal(9, "Blues")))
ssta_zCuts <- seq(-4.5,5,0.5)

# chla_pal <- brewer.pal(9, "Greens")
chla_pal <- oce::oceColorsChlorophyll(20)

wc_pal   <- rainbow(25)

smooth_rainbow <- khroma::colour("smooth rainbow")

sst_cpal <- c(smooth_rainbow(length(seq(floor(8), ceiling(32), 1)), range = c(0, 0.9)), "#9e2a2b", "firebrick4", "#540b0e")
sst_zCuts <- seq(10,32,1)

# CBAR
myPallette <-
    c(rev(brewer.pal(8, "Greens")[2:8])
      # , "white"
      , brewer.pal(3, "Blues")[2:2])

chla_zCuts <- c(0,0.2, 0.3, 0.5,1.0,2,5,25)


## gg layers -----
add_baseplot <- function(bbox) {
    list(
        # add land
        geom_polygon(data = mapdata, aes(x=long, y = lat, group = group), color = "gray90", fill = "gray90"),
        # sest cbar height
        guides(fill = guide_colourbar(barheight =12, ticks = TRUE,
                                      frame.colour = "black", ticks.colour = "black")),
        # set map extent
        coord_sf(xlim = c(bbox[1], bbox[2]), ylim = c(bbox[3], bbox[4]), 
                 expand = FALSE, crs = st_crs(4326)), 
        labs(x = "Longitude", y = "Latitude")
    )
}

update_map_extent <- function(xrange, yrange){
    list(
        coord_sf(xlim = c(xrange[1], xrange[2]), ylim = c(yrange[1], yrange[2]), 
                 expand = FALSE, crs = st_crs(4326))
    )
}

add_turtle_pts <- function(df, col="#66CCC0", size=3){
    list(
        geom_point(data=df, aes(x = lon, y = lat, colour = col), size = size)
    )
}

# add_turtle_pts <- function(df, size=3){
#     list(
#         geom_point(data=df, aes(x = lon, y = lat, colour = as.factor(group)), size = size)
#     )
# }

add_track_spLines <- function(df, cpal, linewidth) {
    list(
        # add cohort 1 tracks
        geom_sf(data = df %>% makeSpatialLines(lon360=FALSE), 
                aes(colour = as.factor(id)),linewidth = linewidth, alpha = 0.9, show.legend=FALSE),
        scale_colour_manual(values = cpal)
    )
}

add_tcms_box <- function(){
    list(
        geom_rect(aes(xmin=make180(225),xmax=make180(243),ymin=25,ymax=35),
                  colour="honeydew3",alpha=0, size=0.45)
    )
}

add_tzcf_box <- function(){
    list(
        geom_rect(aes(xmin=make180(195),xmax=make180(220),ymin=20,ymax=50),
                  colour="white",alpha=0, size=0.75)
    )
}

add_contour_lines <- function(df, contours, col='black', alpha=1){
    list(
        geom_contour(data=df, aes(x=lon, y=lat, z = val), 
                     colour = col, linewidth = 1.25, breaks = c(contours), alpha=alpha,
                     show.legend = F)
    )
}

add_contour_lines_dashed <- function(df, contours, col='black'){
    list(
        geom_contour(data=df, aes(x=lon, y=lat, z = val), linetype = 'dashed', 
                     colour = col, linewidth = 1.25, breaks = c(contours), show.legend = F) 
    )
}

resize_cbar <- function(x){
    list(
        guides(fill = guide_colourbar(barheight =x, ticks = TRUE,
                                      frame.colour = "black", ticks.colour = "black"))
    )
}


## Plot EL NINO Years ----
# el_sept_sst_df  <- el_sept_sst_ras_stack %>% ras2df() %>% mutate(lon = make180(lon)) %>% mutate(year = year(date))
# el_sept_ssta_df <- el_sept_ssta_ras_stack %>% ras2df() %>% mutate(lon = make180(lon)) %>% mutate(year = year(date))
# 
# # create a list with a specific length 
# el_years <- unique(el_sept_sst_df$year)
# el_plot_lst <- vector("list", length = length(el_years))
# 
# # for (i in 1:length(el_years)) {
# 
# el_p <- 
#     ggplot() +
#     geom_raster(data = el_sept_ssta_df , 
#     # geom_raster(data = el_sept_ssta_df %>% filter(year == el_years[i]), 
#             # aes(x = lon, y = lat, fill = val, interpolate = TRUE)) +
#             aes(x = lon, y = lat, fill = cut(val, ssta_zCuts), interpolate = TRUE)) + 
# 
#     # scale_fill_manual(values = ssta_pal
#     #                   , drop = FALSE)
#     scale_fill_manual("SSTA", na.translate = F,
#                       values = ssta_pal[2:length(ssta_pal)], 
#                       na.value="transparent", drop=FALSE,
#                       labels = seq(-4,6,0.5)
#     ) +   
#     # add baselayers
#     add_baseplot(bbox180) +
#     
#     add_contour_lines(df=el_sept_sst_df , 
#     # add_contour_lines(df=el_sept_sst_df %>% filter(year == el_years[i]), 
#                       contours=c(18), col='gray10') + 
#     
#     
#     guides(fill = guide_legend(title = 'SSTA (°C)', ncol=1, reverse=T)) + 
#     theme(plot.caption = element_text(hjust = 0, size=10)) +
#     
#     scale_x_continuous(breaks = seq(bbox180[1] + 10, (bbox180[2] - 20), by = 10)) +
#     # annotate("text", x=-115.5, y=44.5, size=6, label= el_years[i])  +
# 
#     facet_wrap(.~date)
# 
# 
# # Add longterm tzcf clim (1985-2023)
# el_p_with_tzcf_clim <- el_p + add_contour_lines(df=sept_sst_ras_mean_1985_2023_df %>% 
#                                             mutate(lon = make180(lon)) %>% 
#                                              dplyr::select(-c('date'))
#                                              , contours=c(18), col='gray60') +
#     # geom_rect(aes(xmin = -165, xmax = -140, ymin = 35, ymax = 45),
#     #       fill = "transparent", color = "honeydew", size = 1.85)
# 
#     geom_rect(aes_all(vars = c('xmin', 'xmax', 'ymin', 'ymax')), fill = "transparent", color = 'gray20', 
#           alpha = .3,
#           data.frame(xmin = -165, xmax = -140, ymin = 30, ymax = 45)) +
# 
#     labs(subtitle = "September SST Anomaly (SSTA) During El Nino Years",
#          caption = str_c("\n18°C SST isotherm (per year: black line; 1985-2023 Average location: gray line)\n NE Pac TZCF area (black box)"))
# # el_plot_lst[[i]] <- el_p_with_tzcf_clim
# 
# # }
# # 
# # 
# # library(patchwork)
# # patch_p <- (el_plot_lst[[1]] + el_plot_lst[[2]] + el_plot_lst[[3]] +  el_plot_lst[[4]] +
# #     el_plot_lst[[5]] + el_plot_lst[[6]] + el_plot_lst[[7]] +  el_plot_lst[[8]] +
# #     el_plot_lst[[9]] + el_plot_lst[[10]]) 
# # 
# # patch_p + 
# #     plot_layout(ncol = 2, guides = "collect") &
# #     plot_annotation(theme(plot.caption = element_text(hjust = 0, size=10)),
# #     labs(subtitle = "September SST Anomaly (SSTA) During El Nino Years",
# #          caption = str_c("18°C SST isotherm (per year, black line; 1985-2023 average, gray line\n NE Pac TZCF area (white box)"))
# #     )
# # 
# 
# # cowplot::plot_grid(plotlist = el_plot_lst, nrow = 4)
# 
# # save 
# ggsave(filename = str_c('plot_sept_ssta_yrs_with_sst_18C_el_nino.png'), plot = el_p_with_tzcf_clim, 
#        # height=9,
#        width=18, bg='white',
#        path = './figs/'
# )
# 


# functionalize -----

facet_enso_plots <- function(ssta_df, sst_df, enso_cond, bbox=bbox180){
    
    if(enso_cond == 'El Nino' | enso_cond == 'La Nina' | enso_cond == 'Neutral'){
        p_title <- str_c("September 18°C SST Isotherm Location: Preceding ", enso_cond," Years")
    } else {
        p_title <- str_c("September 18°C SST Isotherm Location: During ", enso_cond," Years")
    }
    
    tmp_p <- 
        ggplot() +
        geom_raster(data = ssta_df , 
                    # geom_raster(data = el_sept_ssta_df %>% filter(year == el_years[i]), 
                    # aes(x = lon, y = lat, fill = val, interpolate = TRUE)) +
                    aes(x = lon, y = lat, fill = cut(val, ssta_zCuts), interpolate = TRUE)) + 
        
        # scale_fill_manual(values = ssta_pal
        #                   , drop = FALSE)
        scale_fill_manual("SSTA", na.translate = F,
                          values = ssta_pal[1:length(ssta_pal)], 
                          na.value="transparent", drop=FALSE,
                          labels = seq(-4,6,0.5)
        ) +   
        # add baselayers
        add_baseplot(bbox180) +
        
        add_contour_lines(df= sst_df, 
                          # add_contour_lines(df=el_sept_sst_df %>% filter(year == el_years[i]), 
                          contours=c(18), col='gray10') + 
        
        
        guides(fill = guide_legend(title = 'SSTA (°C)', ncol=1, reverse=T)) + 
        theme(plot.caption = element_text(#hjust = 0, 
                                          size=10)) +
        
        scale_x_continuous(breaks = seq(bbox180[1] + 10, (bbox180[2] - 10), by = 10)) +
        # annotate("text", x=-115.5, y=44.5, size=6, label= el_years[i])  +
        
        facet_wrap(.~year, ncol=5)
    
    # Add longterm tzcf clim (1985-2023)
    tmp_p_with_tzcf_clim <- tmp_p + add_contour_lines(df=sept_sst_ras_mean_1985_2023_df %>% 
                                                        mutate(lon = make180(lon)) %>% 
                                                        dplyr::select(-c('date'))
                                                    , contours=c(18), col='gray60') +
        # geom_rect(aes(xmin = -165, xmax = -140, ymin = 35, ymax = 45),
        #       fill = "transparent", color = "honeydew", size = 1.85)
        
        geom_rect(aes_all(vars = c('xmin', 'xmax', 'ymin', 'ymax')), fill = "transparent", color = 'gray20', 
                  alpha = .3,
                  data.frame(xmin = -165, xmax = -140, ymin = 30, ymax = 45)) +
        
        labs(subtitle = p_title,
             caption = str_c("\nBlack Line: Annual location of the 18°C SST isotherm \nGray Line: Average 18°C SST location (1985-2023) \nBlack box: NE Pac TZCF 'study area'"))
  
    return(tmp_p_with_tzcf_clim)
    
}


## RUN ENSO COND FACET PLOTS ------
### el nino -----
el_p_with_tzcf_clim <- facet_enso_plots(ssta_df = el_sept_ssta_df, sst_df = el_sept_sst_df, enso_cond = 'El Nino')    

# save 
ggsave(filename = str_c('plot_sept_ssta_yrs_with_sst_18C_pre_el_nino.png'), plot = el_p_with_tzcf_clim, 
       # height=9,
       width=18, bg='white',
       path = './figs/'
)

### la nina ------
la_p_with_tzcf_clim <- facet_enso_plots(ssta_df = la_sept_ssta_df, sst_df = la_sept_sst_df, enso_cond = 'La Nina')    

# save 
ggsave(filename = str_c('plot_sept_ssta_yrs_with_sst_18C_pre_la_nina.png'), plot = la_p_with_tzcf_clim, 
       # height=9,
       width=18, bg='white',
       path = './figs/'
)


### mhw ------
mhw_p_with_tzcf_clim <- facet_enso_plots(ssta_df = mhw_sept_ssta_df, sst_df = mhw_sept_sst_df, enso_cond = 'MHW')    

# save 
ggsave(filename = str_c('plot_sept_ssta_yrs_with_sst_18C_mhw.png'), plot = mhw_p_with_tzcf_clim, 
       # height=9,
       width=18, bg='white',
       path = './figs/'
)


### neutral ------
neutral_p_with_tzcf_clim <- facet_enso_plots(ssta_df = neutral_sept_ssta_df, sst_df = neutral_sept_sst_df, enso_cond = 'Neutral')    

# save 
ggsave(filename = str_c('plot_sept_ssta_yrs_with_sst_18C_pre_neutral.png'), plot = neutral_p_with_tzcf_clim, 
       # height=9,
       width=18, bg='white',
       path = './figs/'
)
