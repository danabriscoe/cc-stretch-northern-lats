# 04_plot_historic_cc_tracks_sept.R


library(tidyverse)
library(data.table)
library(ggplot2)
library(plotly)

source('./code/00_northern_lats_helper_functions.R')
source('./code/01_prep_turtle_data.R')


## Load Historic & 2023 Data

raw_df <- raw_data_cohort_1_w_names
daily_df <- daily_avg_data_cohort_1

# make historic df consistent with cohort df(s)
hist_df <-  historic_tags %>% wrangleHistDF()


# just pull small sized turtles
subset_tags_170W <-
hist_df %>%
    filter(lon360 >= 170 & lon360 <= 250) %>%
    # filter(month == 9) %>%
    # filter(year == 1998) %>%
    filter(scl_cm <= 40) %>%
    mutate(dt = date) 

n_tags <- 
    subset_tags_170W %>% 
    pull(id) %>%
    n_distinct()

print(str_c('Number of tracks between -170W and -140W: ', n_tags))

subset_tags_170W_sept <- subset_tags_170W %>% 
    group_by(id) %>%
    mutate(IDX = 1:n()) %>%
    filter(month == 9) %>%
    # summarise(sept_start_dt = min(date),
    #           sept_end_dt = max(date)) %>%
    mutate(sept_num = case_when((year == min(year)) ~ 1, 
                                (year != min(year)) ~2,
                                TRUE ~ 0)) %>%
    filter(sept_num == 1)

##
# just pull small sized turtles
subset_tags <-
    hist_df %>%
    # filter(lon360 >= 170 & lon360 <= 250) %>%
    # filter(month == 9) %>%
    # filter(year == 1998) %>%
    filter(scl_cm <= 40) %>%
    mutate(dt = date) %>%
    
    group_by(id) %>%
    mutate(IDX = 1:n()) %>%
    # filter(month == 9) %>%
    # summarise(sept_start_dt = min(date),
    #           sept_end_dt = max(date)) %>%
    mutate(sept_num = case_when((month == 9 & year == min(year)) ~ 1, 
                                (month == 9 & year != min(year)) ~2,
                                TRUE ~ 0)) #%>%
    # filter(sept_num == 1) # want only 1st instance of sept per tag??






# plot
mapdata <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) %>%
    filter(long >= 120 & long <= 270 & lat >= 15 & lat <=80) 

world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")


plot_tracks_along_longitudes <- function(track_data, id, lon_range = '170E_140W', lon_bbox = lon_bbox, .save_plot=TRUE){
    
    track_tmp <- track_data %>% mutate(lon = make360(lon))
    
    id_expr <- enquo(id)
    lon_tag <- subset_tags %>% filter(id == (!!id_expr)) 
    
    g <- 
        ggplot() + 
        geom_rect(aes_all(vars = c('xmin', 'xmax', 'ymin', 'ymax')), fill = "transparent", color = 'gray20', 
                  alpha = .3,
                  data.frame(xmin = lon_bbox[1], xmax = lon_bbox[2], ymin = 30, ymax = 45)) +
        
        geom_polygon(data = mapdata, aes(x=long, y = lat, group = group), color = "black", fill = "black") +
        
        geom_point(data=track_tmp %>% filter(id == (!!id_expr)),
                   aes(x=lon, y=lat,text = paste0('TagID: ', id, 
                                                  '\n', 'Date: ', date)),
                   colour = 'azure3') +
        # plot sept
        suppressWarnings(geom_point(data=track_tmp %>% 
                                        filter(id == (!!id_expr)) %>%

                                        # filter(dt >= min(lon_tag$dt) & dt <= max(lon_tag$dt)), 
                                    filter(month == 9), 
                                    aes(x=lon, y=lat, 
                                        # colour = id, 
                                        text = paste0('TagID: ', id, 
                                         '\n', 'Date: ', dt)), colour = 'coral3')) +
        geom_point(data = track_tmp %>% 
                       filter(id == (!!id_expr)) %>% 
                       filter(date == min(date)),
                   aes(x=lon, y=lat,text = paste0('TagID: ', id, 
                                                  '\n', 'Start Date: ', date)),
                   colour = 'green', pch=2, size=4
                   ) +
        geom_point(data = track_tmp %>% 
                       filter(id == (!!id_expr)) %>% 
                       filter(date == max(date)),
                   aes(x=lon, y=lat,text = paste0('TagID: ', id, 
                                                  '\n', 'Start Date: ', date)),
                   colour = 'darkred', pch=13, size=4
        ) +
        coord_sf(xlim = c(make360(120), make360(-110)), ylim = c(15, 60), 
                 expand = FALSE, crs = st_crs(4326)) + 
        labs(x = "Longitude", y = "Latitude")

    g <- g + 
        theme(plot.caption = element_text(hjust = 0)) +
        labs(subtitle = str_c('Tag ID: ', id),
            caption = str_c('Sept Locations (red)',
                            '\nLongitude Box (black): ', make180(lon_bbox[1])*-1,'°W - ',make180(lon_bbox[2])*-1,'°W',
                
                              '\n\nStart Date: ', track_tmp %>% 
                                      filter(id == (!!id_expr)) %>% 
                                      filter(date == min(date)) %>%
                                      dplyr::select(c(date))%>%
                                      slice(1) %>% pull() %>%
                                      format("%b-%d-%Y"),
                        
                              '\nEnd Date: ', track_tmp %>% 
                                  filter(id == (!!id_expr)) %>% 
                                  filter(date == max(date)) %>%
                                  dplyr::select(c(date)) %>%
                                  slice(1) %>% pull() %>%
                                  format("%b-%d-%Y"),
                             
                              '\nSCL (cm): ', track_tmp %>% 
                                 filter(id == (!!id_expr)) %>% 
                                 filter(date == max(date)) %>%
                                 dplyr::select(c(scl_cm)) %>%
                                  slice(1) %>% pull()
                             )
    )
    
    if(.save_plot){
        # save: 
        ggsave(filename = str_c('plots_indiv_historic_cc_septs_',lon_range,'_', id,'.png'), plot = g, 
               # height=4,
               width=11,
               bg='white',
               path = './figs/indiv_cc_plots/'
        )
    } else {
    
        gg <- ggplotly(g, tooltip="text") %>% layout(
            title = list(x = 0.1, text = str_c("", '<br>','<sup>', 'Tag ID: ', (id))),
            xaxis=list(autorange=F, range=c(170, 250)), yaxis = list(autorange=F, range=c(20,50)))
            # xaxis=list(autorange=T), yaxis = list(autorange=T))
        
        return(gg)  
    }
}


## plot and store in list ----
set_ids <- function(df){
    ids <- unique(df$id) 
    ids <- droplevels(ids) 
    ids <- as.character(factor(ids, levels = ids)) 
    ids <- sort(ids)
return(ids)
}

subset_tags_by_lons <- function(df, lon_rng = c(170, 250), lon_bbox = c(make360(-175), make360(-140))){
    tmp <- df %>% 

    group_by(id) %>%
    filter(any(lon360 >= 181)) %>%
    filter(lon360 >= lon_rng[1] & lon360 <= lon_rng[2]) %>%
    ungroup()
    
    tmp_septs <- tmp %>%
        group_by(id) %>%
        filter(lon360 >= lon_bbox[1] & lon360 <= lon_bbox[2]) %>%
        filter(month == 9)
        # filter(any(month == 9 & lon360 >= lon_rng[1] & lon360 <= lon_rng[2]))
    
    ret <- tmp[which(tmp$id %in% tmp_septs$id),]
    return(ret)
}


print_ntags <- function(df, lon_bbox){
    n_tags <- 
        df %>% 
        pull(id) %>%
        n_distinct()
    
    return(print(str_c('Number of tracks: ', n_tags)))
    
}


## 1) PLOT TAGS BBOX: 175W - 140W (n=56 indivs)
lons <- c(135, 250)
lon_bbox <- c(make360(-175), make360(-140))

subset_tags_175W <- subset_tags_by_lons(df=subset_tags, lon_rng = lons, lon_bbox = lon_bbox)

ids <- set_ids(subset_tags_175W)

# print n tags
print_ntags(subset_tags_175W)

tracks_plot_list <- 
    lapply(seq(1,n_tags), function(i) {
        
        g<- plot_tracks_along_longitudes(track_data = subset_tags_175W, id = ids[i], 
                                         lon_range = '130E_140W_lon_box_175W', 
                                         lon_bbox = lon_bbox,
                                         .save_plot=TRUE) 
        
    })


## 2) PLOT TAGS BBOX: 178W - 140W (n=65 indivs)
lons <- c(135, 250)
lon_bbox <- c(make360(-178), make360(-140))

subset_tags_178W <- subset_tags_by_lons(df=subset_tags, lon_rng = lons, lon_bbox = lon_bbox)

ids <- set_ids(subset_tags_175W)

# print n tags
print_ntags(subset_tags_178W)


tracks_plot_list <- 
    lapply(seq(1,n_tags), function(i) {
        
        g<- plot_tracks_along_longitudes(track_data = subset_tags_178W, id = ids[i], 
                                         lon_range = '130E_140W_lon_box_178W', 
                                         lon_bbox = lon_bbox,
                                         .save_plot=TRUE) 
        
    })



# save subset for extraction. First remove 2nd/3rd... septs (from manual inspection)

subset_tags_178W_rm_uturn_septs <- subset_tags_178W %>%
    filter(!(id == "19594_05" & year == "2007")) %>%
    filter(!(id == "22534_05" & year == "2007")) %>%
    filter(!(id == "23177_05" & year == "2007")) %>%
    filter(!(id == "25317_05" & year == "2008")) %>%
    filter(!(id == "25358_05" & year == "2007")) %>%
    filter(!(id == "50136_05" & year == "2007" & year == "2008")) %>%
    filter(!(id == "50141_05" & year == "2007")) %>%
    filter(!(id == "57148_05" & year == "2007" & year == "2008")) %>%
    filter(!(id == "57142_05" & year == "2006")) 
    # filter(!(id == "23486_05" & year == "2007")) %> # decided to keep in
    
    # filter(!(id == "23082_05" & year == "2007")) %>% # decided to keep in
    # filter(!(id == "23045_03" & year == "2006")) # pre-deleted above b/c scl cm > 40 cm (51.4 cm upon release)
    

## save for xtracto input
saveRDS(subset_tags_178W_rm_uturn_septs, file = "./data/interim/historic_1997_2013_subset_tags_178W_rm_uturn_septs.rds")



# ####
# mapdata <- map_data('world', wrap=c(-25,335), ylim=c(-55,75)) %>%
#     filter(long >= 120 & long <= 270 & lat >= 15 & lat <=80) 
# 
# world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")
# 
# ggplot() + 
#     geom_polygon(data = mapdata, aes(x=long, y = lat, group = group), color = "black", fill = "black") +
#     geom_point(data=hist_df %>%
#                    filter(id == '19608_98'), aes(x=lon360,y=lat), color = 'azure4') +
#     geom_point(data=hist_sept_1998 %>%
#                    filter(id == '19608_98'), aes(x=lon360,y=lat), color = 'red') +
#     
#     geom_point(data=hist_df %>%
#                    filter(id == '19590_98'), aes(x=lon360,y=lat), color = 'azure3') +
#     geom_point(data=hist_sept_1998 %>%
#                    filter(id == '19590_98'), aes(x=lon360,y=lat), color = 'orange') +
#     
#     theme_minimal() + theme(text=element_text(size=13)) +
#     coord_sf(xlim = c(make360(-180),make360(-140)), ylim = c(30, 45), expand = FALSE, crs = st_crs(4326)) +
#     labs(subtitle = "Historic Tracks (n=2, 1998). Portion of track during Sept is color-coded (red, orange)")