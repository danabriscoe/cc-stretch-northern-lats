# 05_plot_historic_sept_mar_cc_env_histos_and_gams.R


library(tidyverse)
library(data.table)
library(ggplot2)
library(plotly)

source('./code/00_northern_lats_helper_functions.R')
# source('./code/01_prep_turtle_data.R')


## Load Xtracted Data -----

# obsdata <- readRDS(file = "./data/processed/xtracted_cc_sept_mar_178W_140W_1997_2023.rds")
# obsdata_1st_septs <- readRDS(file = "./data/processed/xtracted_cc_1st_septs_mar_178W_140W_1997_2023.rds")


## Histograms ----
sst_br <- seq(12,25,1)

# lons <- c(-178, -140)
# lons <- c(-170, -140)
lons <- c(-165, -140)

mons = c(9, 3)
envs = c('sst', 'chl')
only_1st_septs = TRUE


if(only_1st_septs){
# pull only first septs of historic data
# obsdata_1st_septs <- obsdata %>%
#     group_by(id) %>%
#     mutate(IDX = 1:n()) %>%
#     # filter(month == 9) %>%
#     # summarise(sept_start_dt = min(date),
#     #           sept_end_dt = max(date)) %>%
#     mutate(sept_num = case_when((year == min(year)) ~ 1,
#                                 (year != min(year)) ~2,
#                                 TRUE ~ 0)) %>%
#     filter(sept_num == 1)
# 
#     obsdata <- obsdata_1st_septs
    obsdata_1st_septs <- readRDS(file = "./data/processed/xtracted_cc_1st_septs_next_mar_178W_140W_1997_2023.rds")
    obsdata <- obsdata_1st_septs
} else {
    obsdata <- readRDS(file = "./data/processed/xtracted_cc_sept_mar_178W_140W_1997_2023.rds")
}

## RUN PLOTS ------
for(m in mons){

    for(env in envs){    
    
    df <- 
        obsdata %>%
        filter(month == m) %>% 
        filter(lon >= lons[1] & lon <= lons[2])
    
        if(env == 'sst'){
            df <- df %>%
                mutate(var = sst_dhw_5km_new_mean)
            br <- seq(12,25,1)
            env_bins <- 1
            x_var_lab <- 'SST (°C)'
            env_color <- "steelblue"
            
        } else if(env == 'chl'){
            df <- df %>%
                mutate(var = chl_mean) %>%
                filter(var <= 0.6)
            br <- seq(0,0.8,0.05)
            env_bins <- 0.05
            x_var_lab <- 'Chla (mg/m3)'
            env_color <- "#69b3a2"
        }
    
    yr_var_mean <- df %>%
        group_by(year) %>%
        summarise(mean = mean(var, na.rm=T))
    
    
        ## histogram
    p_facet_var_histogram_by_yr <- 
    df  %>%
        ggplot(aes(x=var)#,
                   # fill = factor(group),
                   # colour = factor(group))
               ) +
        # geom_histogram(binwidth = 0.5, center=0, colour = 1, alpha = 0.6, position = "identity") + 
        geom_histogram(binwidth = env_bins, fill = env_color, colour = "navy", alpha = 0.6) + 
        geom_vline(data = yr_var_mean, aes(xintercept = mean), color = 'red', linetype = "dashed", lwd=.75) +
            
        scale_x_continuous(labels = br, breaks=br) +
        theme_bw() +
        theme(plot.caption = element_text(hjust = 0, size=10)) +
        labs(x = x_var_lab, y= "Count",
             subtitle = str_c(month.name[m], " Turtle-",x_var_lab," Preference: (",lons[1]*-1, "°W to ", lons[2]*-1, "°W)"),
             caption = "Vertical dashed line (red) represents annual mean value") +
     
            facet_wrap(.~year, , scales = "free")
        
    # save
    if(only_1st_septs){
        fname_grp <- str_c('plot_facet_',month.abb[m],'_', env,'_histogram_by_yr_',lons[1]*-1,'W_',lons[2]*-1,'W_1st_septs_next_mar.png')
    } else {
        fname_grp <- str_c('plot_facet_',month.abb[m],'_', env,'_histogram_by_yr_',lons[1]*-1,'W_',lons[2]*-1,'W.png')
    }
    
    ggsave(filename = fname_grp, plot = p_facet_var_histogram_by_yr, 
           # height=4,
           width=10,
           bg='white',
           path = './figs/'
    )
        
    
    ## PLOT ALL Years (< 2013 vs 2023)
    group_var_mean <- df  %>% 
        group_by(group) %>%
        summarise(mean = mean(var, na.rm=T),
                  med = median(var, na.rm=T))
    
    
    p_facet_var_histogram_by_group <-
    df %>% 
        ggplot(aes(x=var)#,
               # fill = factor(group),
               # colour = factor(group))
        ) +
        # geom_histogram(binwidth = 0.5, center=0, colour = 1, alpha = 0.6, position = "identity") + 
        geom_histogram(binwidth = env_bins, fill = env_color, colour = "navy", alpha = 0.6) + 
        geom_vline(data = group_var_mean, aes(xintercept = mean), color = 'red', linetype = "dashed", lwd=.75) +
        # geom_vline(data = group_sst_mean, aes(xintercept = med), color = 'red', linetype = "dashed", lwd=1) +
        
        scale_x_continuous(labels = br, breaks=br) +
        theme_bw() +
        theme(plot.caption = element_text(hjust = 0, size=10)) +
        labs(x = x_var_lab, y= "Count",
             subtitle = str_c(month.name[m], " Turtle-", x_var_lab," Preference: (",lons[1]*-1, "°W to ", lons[2]*-1, "°W)"),
             caption = "Vertical dashed line (red) represents group mean value") +
        facet_wrap(.~group, , scales = "free")
    
   
    # save
    if(only_1st_septs){
        fname_yr <- str_c('plot_facet_',month.abb[m],'_', env, '_histogram_by_group_',lons[1]*-1,'W_',lons[2]*-1,'W_1st_septs_next_mar.png')
    } else {
        fname_yr <- str_c('plot_facet_',month.abb[m],'_', env, '_histogram_by_group_',lons[1]*-1,'W_',lons[2]*-1,'W.png')
    }
    
    # save
    ggsave(filename = fname_yr, plot = p_facet_var_histogram_by_group, 
           # height=4,
           width=13,
           bg='white',
           path = './figs/'
    )

    } # end env
} # end mon



## GAMS -------

# ### 1) Sept
lons <- c(-178, -140)
#
#
data <-
    obsdata %>%
    filter(month == 9) %>%
    filter(lon >= lons[1] & lon <= lons[2]) #%>%
    # group_by(year, month) %>%
    # summarise(avg_lat = mean(lat, na.rm=T),
    #           avg_lon = mean(lon, na.rm=T),
    #           avg_lon360 = mean(lon360, na.rm=T))

# 
# 
# gam_df = data
# 
# three_smooth_model <- gam(lat ~ s(year, k=3, fx=T) +
#                                 s(lon360, k=3), data = gam_df, method = "REML")
# (three_smooth_summary <- summary(three_smooth_model))
# 
# m=9
# mon = month.abb[m]
# 
# partial_plot <- draw(three_smooth_model, residuals = TRUE) #+ labs(title = glue("Partial Plot: {mon}"))
# partial_plot
# gratia::difference_smooths(three_smooth_model, residuals = TRUE)
# 
# 
# 
# 
# 
# 
# head(gam_df)
# 
# run_gam_kfolds <- function(data, kfolds = 5, knts=3, mon = "Sep"){
#     # Define the formula for the GAM model
#     # model_formula <- y ~ s(year, k = 20)
#     model_formula <- lat ~ s(year, k = knts, fx=T) + s(lon)
# 
# 
# 
#     # Create an empty vector to store RMSE values
#     rmse_values <- numeric()
# 
#     # Set the number of folds (e.g., 5-fold cross-validation)
#     k <- kfolds
# 
#     # Create a function to calculate the RMSE for the GAM model
#     calculate_rmse <- function(train_indices) {
#         train_data <- data[train_indices, ]
#         test_data <- data[-train_indices, ]
# 
#         gam_model <- gam(model_formula, data = train_data, method = "ML")
#         predictions <- predict(gam_model, newdata = test_data)
# 
#         rmse <- sqrt(mean((test_data$y - predictions)^2))
# 
#         # Create a partial dependence plot for 'year' variable
#         partial_plot <- draw(gam_model, residuals = TRUE) + labs(title = glue("Partial Plot: {mon}"))
#         # title("Partial Dependence Plot")
# 
#         return(list(rmse = rmse, partial_plot = partial_plot, model_summary = summary(gam_model)))
#     }
# 
#     # Perform k-fold cross-validation
#     set.seed(123)  # Set a random seed for reproducibility
#     folds <- sample(1:k, nrow(data), replace = TRUE)
# 
#     results_list <- list()
# 
#     for (i in 1:k) {
#         results <- calculate_rmse(folds != i)
#         rmse_values <- c(rmse_values, results$rmse)
#         print(results$partial_plot)
#         print(results$model_summary)
# 
#         results_list[[i]] <- list(k=i,results)#, rmse_values, partial_plot=results$partial_plot)
#     }
# 
#     # View the RMSE values for each fold
#     cat("RMSE values for each fold:\n")
#     print(rmse_values)
# 
#     # Calculate the mean RMSE
#     mean_rmse <- mean(rmse_values)
#     cat("Mean RMSE:", mean_rmse, "\n")
# 
#     return(results_list)
# }