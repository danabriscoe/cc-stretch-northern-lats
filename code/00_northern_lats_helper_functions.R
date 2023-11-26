## 00 northern lats helper functions.R

# l <- leaflet() %>% addTiles()
addTrackLines <- function(m, df, cpal=wc_pal, .indiv=TRUE){
    
    df_split <- split(df, df$id)
    
    names(df_split) %>%
        purrr::walk( function(x) {
            m <<- m %>%
                addPolylines(data=df_split[[x]],
                           lng=~lon, lat=~lat,
                           # label=~as.character(turtle_name),
                           popup=paste0(
                               "<strong>", "Name: "  , "</strong>", df_split[[x]]$turtle_name, "<br>",
                               "<strong>", "ID: ", "</strong>", x),
                           
                           color = ~cpal(id),
                           group = ifelse(.indiv==TRUE, x, 
                                          str_c("All Tracks (n=", daily_df$id %>% n_distinct(),")"))
                           # labelOptions = labelOptions(noHide = F,
                           #                             direction = 'auto')
                           )
        })
    return(m)
} # end func
###-------


# add all/indiv tracks to map as lines +/- circle markers
addMapTracks <- function(m, df, cpal, .indiv=TRUE, .addLines=TRUE, .addMarkers=TRUE){
    # m = base leaflet map
    # df = dataframe to map
    # ids = indiv track ids
    
    ids <- unique(df$id)    
    t_names <- unique(df$turtle_name)
    t_nums <- unique(df$turtle_num)
    
    # Add track lines
    for (id in ids) {
        
        df_id <- df[df$id == id, ]
        
        if(!.indiv){
            grp <- str_c("All Tracks (n=", ids %>% n_distinct(),")")
            grps <- grp
        } else {
            grp <- id
            grps <- ids #c(str_c(t_nums, ' - ', t_names))
        }
        
        if(.addLines & !.addMarkers){
            m <- m |>
                # Add track lines
                addPolylines(data = df_id, group = as.character(id),
                             lng = ~lon,
                             lat = ~lat,
                             
                             popup=paste0(
                                 "<strong>", "ID: ", "</strong>", df_id$id), 
                             
                             color = ~cpal(id)) 
            
        } else if (.addLines & .addMarkers) {
            m <- m |>
                # Add track lines
                addPolylines(data = df_id, group = as.character(id),
                             lng = ~lon,
                             lat = ~lat,
                             color = ~cpal(id)) 
            
        } 
        if(.addMarkers | !.addLines){
            m <- m |>    
                # Turtle Data Points
                addCircleMarkers(data = df_id[df_id$id == id, ], group = as.character(grp),
                                 lng= ~lon,
                                 lat= ~lat, # the spatial data, requires "~" here because it's NOT sp or sf object
                                 
                                 popup=paste0(
                                     "<strong>", "Name: "  , "</strong>", df_id$turtle_name, "<br><strong>",
                                     "ID: ", "</strong>", df_id$id, "<br><br><strong>",
                                     "Date: ",  "</strong>", format(df_id$date, format="%Y-%b-%d"), "<br><strong>",
                                     "Lat: ", "</strong>", round(df_id$lat,2), "°N","<br><strong>",
                                     "Lon: ", "</strong>", round(df_id$lon,2),"°W"),
                                 
                                 stroke=TRUE, weight=0.6,radius=5,
                                 fillOpacity = 0.75, color="black",
                                 fillColor= ~cpal(id))  # mapping to the color palette
        }
    } # end for loop
    
    
    
    #create layer control
    ret_map <- m %>%
        addLayersControl(
            baseGroups = c("ESRI World Imagery", "ESRI OpenStreetMap"
                           # , "Esri.WorldPhysical"
            ),
            overlayGroups = c(grps),
            # hideGroup(as.character(c(ids[3:25])))
            options = layersControlOptions(collapsed = TRUE)) %>%
        
        addScaleBar(position = c("bottomleft"),
                    scaleBarOptions(
                        # maxWidth = 100,
                        metric = TRUE,
                        imperial = TRUE,
                        updateWhenIdle = TRUE)
        )
    
    return(ret_map)
    
} # end function




addMapInset <- function(m){
    m %>%  addMiniMap(
        tiles = "Esri.WorldImagery",
        position = 'bottomleft', 
        # width = 200, height = 200,
        toggleDisplay = TRUE) 
}    



## Attach metadata to df
attach_metadata <- function(dat_df, meta_df, by="id"){
    merged_df <- 
        dat_df %>%
        merge(., meta_df, by="id", all.x = TRUE)
    
    return(merged_df)
}


## Bin Freq Table
binFreqTable <- function(x, bins) {
    
    freq = hist(x, breaks=bins, include.lowest=TRUE, plot=FALSE)
    ranges = paste(head(freq$breaks,-1), freq$breaks[-1], sep=" - ")
    
    return(data.frame(range = ranges, frequency = freq$counts))
    
}


## Calc avg daily location
calc_avg_daily_loc <- function(x, ...) {
    # check date attr
    if(!is.Date(raw_data_cohort_1_w_names$date)){
        x <- x %>%
            mutate(date = as.Date(date))
    } else {
        print('error: fix date attribute')
    }
    
    # vars to group
    vars1 <- syms(...) # must be in quotes
    
    # calc daily avg by groups 
    ret <- x %>%
        group_by(!!!vars1) %>%
        summarise(lat = mean(lat), 
                  lon = mean(lon),
                  scl_cm = mean(scl_cm), 
                  .groups = 'drop')
    return(ret)
}


# calc weekly summary stats
calcWeeklyStats <- function(x, var, by=week_idx){
    var <- enquo(var)
    # dots <- ensyms(...)
    grp_by <- enquo(by)
    # if(.unique_weeks){
    #     grp <- as.name(substitute('end_of_week'))
    # } else {
    #     grp <- as.name(substitute(week_idx))
    # }
    # 
    ret <- x %>%
        group_by(!!grp_by) %>%
        summarize(
            avg = mean(!!var, na.rm=TRUE),
            med = median(!!var, na.rm=TRUE),
            min = min(!!var),
            max = max(!!var),
            high = mean(!!var, na.rm = T) + 0.2 * sd(!!var, na.rm= T),
            low  = mean(!!var, na.rm = T) - 0.2 * sd(!!var, na.rm= T), .groups = "drop")  
    return(ret)
}


## Get Date Range
getDateRange <- function(startdate, enddate, unit = "month", format = "%Y/%mm/%dd"){
    if(unit == 'daily'){unit <- 'day'}
    ret <- seq(as.Date(startdate), as.Date(enddate), by = unit,format = format)
    
    return(ret)
}

# ## Get HovLon
# getHovLon <- function(r, idx){
#     dirLayer <- r %>% 
#         init(., v='y')            # spatial avg across lon (in this case, 20 deg chunks)
#     
#     z <- zonal(r, dirLayer, FUN='mean', digits=2)
# 
#     # idx <- seq(as.Date('2023-01-16'), as.Date('2023-08-16'), by='month')
# 
#     dat <- expand.grid(y=z[,1], x=idx)
#     dat$z <- as.vector(z[,-1], mode='numeric')
#     
#     dat$x <- as.Date(dat$x)
#     
#     return(dat)
# }

# getHovLon function by month year
# getHovLon <- function(subset_r, year) {
#     dirLayer <- subset_r %>% init(., v = 'y') # spatial avg across lon (in this case, 20 deg chunks)
#     
#     z <- zonal(subset_r, dirLayer, FUN = 'mean', digits = 2)
#     
#     dat <- expand.grid(y = z[, 1], x = as.Date(paste0(year, "-01-01")))
#     dat$z <- as.vector(z[, -1], mode = 'numeric')
#     
#     dat$x <- as.Date(dat$x)
#     
#     return(dat)
# }

# Define your modified getHovLon function
getHovLon <- function(subset_r, year, month) {
    dirLayer <- subset_r %>% init(., v = 'y') # spatial avg across lon (in this case, 20 deg chunks)
    
    z <- zonal(subset_r, dirLayer, FUN = 'mean', digits = 2)
    
    dat <- expand.grid(y = z[, 1], x = as.Date(paste0(year, "-", sprintf("%02d", month), "-01")))
    dat$z <- as.vector(z[, -1], mode = 'numeric')
    
    dat$x <- as.Date(dat$x)
    
    return(dat)
}

## Get week date - convert week index back into a Date (ex uses 2023 as year)
getWeekDate <- function(x){
    ret <-    
        as.Date(
            paste('2023', x, '1'), 
            format = "%Y %U %u")
    return(ret)
}


## Get max lat by id
getMaxLat <- function(df){
    ret_max <- df %>% 
        group_by(id) %>%
        slice(which.max(lat))
    return(ret_max)
}


## Load raw data (from wc downloads)
load_wc_downloads <- function(wc_files){
    ret <- rbindlist(lapply(wc_files, fread)) %>%
        dplyr::select('Platform ID No.', 'Latitude', 'Longitude', 'Loc. quality', 'Loc. date') %>%
        rename(id = 1,
               lat = 2, 
               lon = 3, 
               loc_quality = 4, 
               date = 5
        ) %>%
        mutate(date = as.POSIXct(date, format= "%m/%d/%Y %H:%M:%S", tz="UTC")) %>%
        as_tibble()
    
    return(ret)
}

## Load metadata files
load_metadata_xls <- function(meta_files){
    ret <- readxl::read_excel(path = meta_files) %>%
    dplyr::select(1,3,5,6) %>%
    rename("turtle_num"=1, "id"=2, "turtle_name"=3, "scl_cm"=4)
    return(ret)
}    

## Load historic tags
load_all_tags_historic <- function(path = '~/github/cc-stretch-exploratory/data/interim/', keep_all=FALSE){
    load(file = str_c(path, 'historic_napc_tags.RData')) 
    
    if(keep_all){
        (message('note: all_tags is an untidy list'))
        all_tags <- list(all_tags, wpac.tags, cnp.tags, baja.tags)
    }else{
        (message('note: all_tags is a tidy dataframe'))
        all_tags <- rbind(wpac.tags, cnp.tags) %>%             # pull together western and central npac tag deploys
            mutate(id = id %>% as.factor(),
                   dt = as.Date(date)) %>%                     # convert id to factor
            group_by(id) %>%                                   # group by tag id
            arrange(date) %>%                                  # sort by ascending date
            mutate(ndays_transmitting = difftime(date, date[1], units = "days")) %>%        # calc number of days transmitting since day 1
            ungroup()
        
        rm(wpac.tags, cnp.tags, baja.tags)
    }
    return(all_tags)
}





# make180
make180 <- function(lon){
    isnot360<-min(lon)<0
    if (!isnot360) {
        ind<-which(lon>180)
        lon[ind]<-lon[ind]-360
    }
    return(lon)
}

# make360
make360 <- function(lon){
    isnot360<-min(lon)<0
    if(isnot360){
        ind<-which(lon<0)
        lon[ind]<-lon[ind]+360
    }
    return(lon)
}


# make spatial lines (for leafgl)
makeSpatialLines <- function(df, lon360=TRUE){
    if(lon360){
        df <- df %>% mutate(lon = make360(lon))
    }
    
    ret <- df %>%
        dplyr::mutate(id = as.factor(id)) %>% 
        st_as_sf(coords=c('lon', 'lat'), crs=4326) %>%
        group_by(id) %>%
        summarise(do_union = FALSE) %>%
        st_cast("LINESTRING")
    return(ret)
}

# make color pal
makePal <- function(pal = 'honeydew', col){
    colorFactor(palette = pal, 
                levels = col)
}


## parseDT
parseDT <- function(x, idx, start, stop, format){
    ret <- fs::path_file(x) %>%
        substr(., start=start, stop=stop) %>%
        as.character(strptime(.,format=format,tz='UTC'))
    return(ret)
}


## Prep rasterZ
prepRasterZ <- function(rs = ras_stack, dt_idx){
    ras_names_yrmon <- as.yearmon(dt_idx)
    rs <- setZ(rs, ras_names_yrmon)
    # names(ssta_monthly_stack) <- as.character(ras_names_yrmon)  # mon-yr abbreb
    names(rs) <- as.character(dt_idx) # full Xyr-mon-d 
    return(rs)
}

## Raster to DF
ras2df <- function(x){
    ret<- x %>%
        rasterToPoints %>%
        as.data.frame() %>%
        `colnames<-`(c("lon", "lat", names(x))) %>%
        # pivot_longer(cols = starts_with("X20"),
        pivot_longer(cols = starts_with("X"),
                     names_to = "layer",
                     values_to = "val") %>%
        mutate(layer = substr(layer, 2, 14)) %>%
        mutate(date = as.POSIXct(layer, "%Y.%m.%d", tz = "UTC")
        ) %>% 
        # mutate(x = make360(x)) %>%
        mutate(date = as.Date(date))

    return(ret)
}
    
## subset by lat/lon
subsetByLatLon <- function(df, lon_min, lon_max, lat_min, lat_max){
    ret <- df %>% 
        group_by(id) %>%
        filter(lon >= lon_min & lon <= lon_max) %>%
        filter(lat >= lat_min & lat <= lat_max) %>%
        ungroup()
    
    return(ret)
}

# make historic df consistent with current df
wrangleHistDF <- function(df){
    ret <- df %>%
        dplyr::select(c("id", "dt", "lat", "lon", "SCL_cm")) %>%
        rename("date" = "dt", "scl_cm" = "SCL_cm") %>%
        group_by(id) %>%
        mutate(turtle_num = cur_group_id(),
               turtle_name = NA_real_) %>%
        ungroup() %>%
        relocate("turtle_num", "turtle_name", .after = "id") %>%
        mutate(lon = make180(lon)) %>%
        mutate(lon360 = make360(lon)) %>%
        relocate("lon360", .after = "lon") %>%
        mutate(year = lubridate::year(date),
               month = lubridate::month(date),
               day = lubridate::day(date)) %>%
        mutate(group = 'historic')
    return(ret)
}



### misc ---
# ## function to plot a map with layer selection
# addTracks <- function(m, df, cpal) {
#     # m = base leaflet map
#     # df = dataframe to map
#     
#     
#     m <- m %>% 
#         # # Add track lines --- LEAVE LINES OUT FOR NOW. NOT WORKING
#         # addPolylines(data = df, group = "All Tracks (n=25)",
#         #              lng = ~lon,
#         #              lat = ~lat,
#         #              color = ~cpal(id)) %>%
#         
#         # Turtle Data Points
#         addCircleMarkers(data=df, group="All Tracks (n=25)",
#                          lng= ~lon, 
#                          lat= ~lat, # the spatial data, requires "~" here because it's NOT sp or sf object
#                          
#                          popup=paste0(
#                              "<strong>", "Name: "  , "</strong>", df$turtle_name, "<br><strong>", 
#                              "ID: ", "</strong>", df$id, "<br><br><strong>", 
#                              "Date: ",  "</strong>", format(df$date, format="%Y-%b-%d"), "<br><strong>", 
#                              "Lat: ", "</strong>", round(df$lat,2), "°N","<br><strong>", 
#                              "Lon: ", "</strong>", round(df$lon,2),"°W"),
#                          
#                          stroke=TRUE, weight=0.6,radius=5,
#                          fillOpacity = 0.75, color="black",
#                          fillColor= ~cpal(id))
#     
#     #create layer control
#     ret_map <- m %>% 
#         addLayersControl(
#             baseGroups = c("ESRI World Imagery", "ESRI OpenStreetMap"
#                            # , "Esri.WorldPhysical"
#             ),
#             overlayGroups = c("All Tracks (n=25)"),
#             # hideGroup(as.character(c(ids[3:25]))) 
#             options = layersControlOptions(collapsed = TRUE)) %>%
#         
#         addScaleBar(position = c("bottomleft"),    
#                     scaleBarOptions(
#                         # maxWidth = 100,
#                         metric = TRUE,
#                         imperial = TRUE,
#                         updateWhenIdle = TRUE)
#         )
#     
#     return(ret_map)
# }
# 
# add individual track lines to map
addIndivTracks <- function(m, df, cpal){
    # m = base leaflet map
    # df = dataframe to map

    ids <- unique(df$id)

    t_names <- unique(daily_df$turtle_name)
    t_nums <- unique(daily_df$turtle_num)

    # Add track lines
    for (id in ids) {

        df_id <- df[df$id == id, ]

        m <- m %>%
            # Add track lines
            addPolylines(data = df_id, group = as.character(id),
                         lng = ~lon,
                         lat = ~lat,
                         color = ~wc_pal(id)) %>%

            # Turtle Data Points
            addCircleMarkers(data = df_id[df_id$id == id, ], group = as.character(id),
                             lng= ~lon,
                             lat= ~lat, # the spatial data, requires "~" here because it's NOT sp or sf object

                             popup=paste0(
                                 "<strong>", "Name: "  , "</strong>", df_id$turtle_name, "<br><strong>",
                                 "ID: ", "</strong>", df_id$id, "<br><br><strong>",
                                 "Date: ",  "</strong>", format(df_id$date, format="%Y-%b-%d"), "<br><strong>",
                                 "Lat: ", "</strong>", round(df_id$lat,2), "°N","<br><strong>",
                                 "Lon: ", "</strong>", round(df_id$lon,2),"°W"),

                             stroke=TRUE, weight=0.6,radius=5,
                             fillOpacity = 0.75, color="black",
                             fillColor= ~wc_pal(id))  # mapping to the color palette
    }



    #create layer control
    ret_map <- m %>%
        addLayersControl(
            baseGroups = c("ESRI World Imagery", "ESRI OpenStreetMap"
                           # , "Esri.WorldPhysical"
            ),
            overlayGroups = c(ids),
            # hideGroup(as.character(c(ids[3:25])))
            options = layersControlOptions(collapsed = TRUE)) %>%

        addScaleBar(position = c("bottomleft"),
                    scaleBarOptions(
                        # maxWidth = 100,
                        metric = TRUE,
                        imperial = TRUE,
                        updateWhenIdle = TRUE)
        )

    return(ret_map)

} # end function