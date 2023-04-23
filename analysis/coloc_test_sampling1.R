# source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/coloc_test_sampling1.R")
   set.seed(23458892)
   require(lmodel2)
   require(viridis)
   require(ggplot2)
   require(ggforce)
   require(maps)
   #require(akima)

# ================================================================= #
# Load buoys.
   source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/load_buoy_locs.R")
# ================================================================= #
# Edit here
# ----------------------------------------------------------------- #
# Plotting and analysis.
   flag_plot_cor <- TRUE

# Sea state sampling by period.
   flag_period_thresh <- FALSE
   flag_swell_only <- FALSE
   period_thresh <- 8

# Data and sampling.
   buoy_radius <- 75
   Sidx <- 1
   vec_tandem_labs <- c("J3","S6LRM","S6SAR")

   lab_month <- c("Dec (2020)","Jan (2021)","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# Offshore of neashore.
   flag_OS <- FALSE

   if ( flag_OS ) {
#-------------------------------------------------------------------------------------------------#
# PAC Offshore (active).
      b_idx_list <- 3:13
#-------------------------------------------------------------------------------------------------#
# as.POSIXct(S6_46246_march[[4]][1:100], origin = '2000-01-01', tz='GMT')
# Attach J3.
      attach("./output/buoys_J3/list_buoy_data_swh_ocean_PAC_OS.Robj")
      mat_list_J3 <- list_buoy_data[[2]]
      detach()

# Attach S6 LRM.
      attach("./output/buoys_S6/list_buoy_data_LRM_swh_ocean_PAC_OS.Robj")
      mat_list_S6_LRM <- list_buoy_data[[2]]
      detach()

# Attach S6 SAR.
      attach("./output/buoys_S6/list_buoy_data_SAR_swh_ocean_PAC_OS.Robj")
      mat_list_S6_SAR <- list_buoy_data[[2]]
      detach()

# Attach ERA5.
      attach("./output/ERA5/buoy_array_PAC_OS_2020-2022.Robj")
      mat_list_ERA5 <- list_buoy_data[[2]]
      detach()
   } else {
#-------------------------------------------------------------------------------------------------#
# PAC Nearshore (active).
      b_idx_list <- (1:52)[-c(7:14,20,22,30,33,35,46,48,51,52)]
#-------------------------------------------------------------------------------------------------#
# Attach J3.
      attach("./output/buoys_J3/list_buoy_data_swh_ocean_PAC_NS.Robj")
      mat_list_J3 <- list_buoy_data[[2]]
      detach()

# Attach S6 LRM.
      attach("./output/buoys_S6/list_buoy_data_LRM_swh_ocean_PAC_NS.Robj")
      mat_list_S6_LRM <- list_buoy_data[[2]]
      detach()

# Attach S6 SAR.
      attach("./output/buoys_S6/list_buoy_data_SAR_swh_ocean_PAC_NS.Robj")
      mat_list_S6_SAR <- list_buoy_data[[2]]
      detach()
# Attach ERA5.
# "coordinates"     "latitude"        "longitude"       "time"            "wave_parameters"
# "swh"     "p140121" "mp2"     "p140123"
      attach("./output/ERA5/buoy_array_PAC_NS_2020-2022.Robj")
      mat_list_ERA5 <- list_buoy_data[[2]]
      detach()
   }

#-------------------------------------------------------------------------------------------------#
# Code to approximately convert degrees to km.
# Radius of the Earth in km.
   i_radius = 6371
# Function for radians.
   func_rads <- function(x) { x * pi / 180 }
# Function for distance from buoy.
   func_buoy_dist <- function(x,B_idx) {
      fl_d_lat = func_rads(x[1]) - func_rads(df_buoy_data$buoy_lat[B_idx])
      fl_d_lon = func_rads(x[2]) - func_rads(df_buoy_data$buoy_lon[B_idx])
      fl_h = sin(fl_d_lat / 2) * sin(fl_d_lat / 2) + cos( func_rads(df_buoy_data$buoy_lat[B_idx]) ) * cos( func_rads(x[1]) ) * sin(fl_d_lon / 2) * sin(fl_d_lon / 2)
      #fl_h = sin(fl_d_lat / 2) * sin(fl_d_lat / 2) + cos( func_rads(5.0) ) * cos( func_rads(x[1]) ) * sin(fl_d_lon / 2) * sin(fl_d_lon / 2)
      #fl_h = 2*asin(sqrt((sin((fl_d_lat)/2))^2 + cos(func_rads(x[1]))*cos(func_rads(df_buoy_data$buoy_lat[B_idx]))*(sin((fl_d_lon)/2))^2))
      2 * i_radius * asin(sqrt(fl_h))
   }

# Function for inter-1 Hz point distance.
   func_sat_dist <- function(x,y) {
      fl_d_lat = func_rads(x[1]) - func_rads(y[1])
      fl_d_lon = func_rads(x[2]) - func_rads(y[2])
      fl_h = sin(fl_d_lat / 2) * sin(fl_d_lat / 2) + cos( func_rads(y[1]) ) * cos( func_rads(x[1]) ) * sin(fl_d_lon / 2) * sin(fl_d_lon / 2)
      2 * i_radius * asin(sqrt(fl_h))
   }

#-------------------------------------------------------------------------------------------------#

   #for (b_idx in 1:length(b_idx_list)) {
   #for (b_idx in 1:15) {
# Nearshore.
#  1 46077 (in channel, exclude?)
#  2 46080
#  4 46082 (some sheltering ~10% reduction)
#  5 46083
# 13 46098
# Offshore.
   for (b_idx in 1) {

      ERA5_b_idx <- buoy_idx <- b_idx_list[b_idx]

#=================================================================================================#
# Load buoy data.
# Buoy time offset: 946684800
#-------------------------------------------------------------------------------------------------#
      buoy_data_file <- list.files(path = "/home/ben/research/waves/buoy_data/NDBC_complete_records/", pattern = paste("^",buoy_list[buoy_idx],".*hs.csv",sep="") )
      #buoy_data_file <- "46005_1976-2021_hs.csv"
      mat_buoy_csv1 <- read.csv(paste("/home/ben/research/waves/buoy_data/NDBC_complete_records/",buoy_data_file,sep=""))
      mat_buoy_csv <- mat_buoy_csv1[!is.na(mat_buoy_csv1$hs),]
      vec_buoy_time <- strptime(as.character(mat_buoy_csv[,1]),format="%Y-%m-%d %H:%M:%S",tz="GMT")
# Get indices for 2020/12 - 2021/12
      #date_start_idx <- which( format( as.POSIXct( vec_buoy_time, tz='GMT'), "%Y%m") %in% "202012" )[1]
      date_start_idx <- c( which( format( as.POSIXct( vec_buoy_time, tz='GMT'), "%Y%m") %in% "202012" ), which( format( as.POSIXct( vec_buoy_time, tz='GMT'), "%Y") %in% "2021" ) )[1]

      print(paste("Buoy ID:",buoy_idx,"[",buoy_list[buoy_idx],"] date_start_idx:",date_start_idx))
#      if ( !is.na(date_start_idx) ) {
         date_idx <- date_start_idx:length(vec_buoy_time)

         vec_buoy_time_num <- as.numeric( vec_buoy_time[date_idx] ) - 946684800
         vec_buoy_hs <- mat_buoy_csv$hs[date_idx]
         vec_buoy_ap <- mat_buoy_csv$ap[date_idx]

#=================================================================================================#
# Process ERA5 data.
# Buoy time offset: 946684800
# as.POSIXct( vec_ERA5_time_num*3600, origin = "1900-01-01",tz='GMT')
#-------------------------------------------------------------------------------------------------#
         vec_ERA5_time_num <- vec_ERA5_hs <- vec_ERA5_ap <- NULL
         for ( i_month in 1:dim(mat_list_ERA5)[1]) {
            vec_ERA5_time_num <- c(vec_ERA5_time_num,mat_list_ERA5[[i_month,ERA5_b_idx]][[4]])
            vec_ERA5_hs <- c(vec_ERA5_hs,mat_list_ERA5[[i_month,ERA5_b_idx]][[5]][[1]][2,2,])
            vec_ERA5_ap <- c(vec_ERA5_ap,mat_list_ERA5[[i_month,ERA5_b_idx]][[5]][[3]][2,2,])
         }
# Adjust time to common scale starting at 2020-12-01 00:00:00.
         vec_ERA5_time_num <- vec_ERA5_time_num*3600 - 3155673600

# ERA5 bilinear interpolation
# To do: timing information.
         vec_ERA5_hs_BILIN <- NULL

# Identify which of the four grid cells contains the buoy.
         #for ( i_month in 1 ) {
         for ( i_month in 1:dim(mat_list_ERA5)[1]) {
# Left side.
            if ( df_buoy_data$buoy_lon[ERA5_b_idx] < (mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lon_mid[2]-360) ) {
               vec_X <- mat_list_ERA5[[i_month,ERA5_b_idx]][[1]]$buoy_lon_mid[1:2]-360
# Bottom left.
               if ( df_buoy_data$buoy_lat[ERA5_b_idx] < mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lat_mid[2] ) {
                  vec_Y <- mat_list_ERA5[[i_month,ERA5_b_idx]][[1]]$buoy_lat_mid[3:2]
                  mat_QX <- t(mat_list_ERA5[[i_month,ERA5_b_idx]][[5]][[1]][1:2,3,])
                  mat_QY <- t(mat_list_ERA5[[i_month,ERA5_b_idx]][[5]][[1]][1:2,2,])
# Top left.
               } else {
                  vec_Y <- mat_list_ERA5[[i_month,ERA5_b_idx]][[1]]$buoy_lat_mid[2:1]
                  mat_QX <- t(mat_list_ERA5[[i_month,ERA5_b_idx]][[5]][[1]][1:2,2,])
                  mat_QY <- t(mat_list_ERA5[[i_month,ERA5_b_idx]][[5]][[1]][1:2,1,])
               }
# Right side.
            } else {
               vec_X <- mat_list_ERA5[[i_month,ERA5_b_idx]][[1]]$buoy_lon_mid[2:3]-360
# Bottom right.
               if ( df_buoy_data$buoy_lat[ERA5_b_idx] < mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lat_mid[2] ) {
                  vec_Y <- mat_list_ERA5[[i_month,ERA5_b_idx]][[1]]$buoy_lat_mid[3:2]
                  mat_QX <- t(mat_list_ERA5[[i_month,ERA5_b_idx]][[5]][[1]][2:3,3,])
                  mat_QY <- t(mat_list_ERA5[[i_month,ERA5_b_idx]][[5]][[1]][2:3,2,])
# Top right.
               } else {
                  vec_Y <- mat_list_ERA5[[i_month,ERA5_b_idx]][[1]]$buoy_lat_mid[2:1]
                  mat_QX <- t(mat_list_ERA5[[i_month,ERA5_b_idx]][[5]][[1]][2:3,2,])
                  mat_QY <- t(mat_list_ERA5[[i_month,ERA5_b_idx]][[5]][[1]][2:3,1,])
               }
            }
# Latitude.

            vec_ERA5_hs_BILIN <- c( vec_ERA5_hs_BILIN,sapply( X=1:length(mat_list_ERA5[[i_month,ERA5_b_idx]][[5]][[1]][2,2,]), FUN=function(x,y,t) { 1/(0.5*0.5) * cbind(c(vec_X[2]-x),c(x-vec_X[1])) %*% cbind(mat_QX[t,],mat_QY[t,]) %*% rbind(c(vec_Y[2]-y),c(y-vec_Y[1])) },x=df_buoy_data$buoy_lon[ERA5_b_idx], y=df_buoy_data$buoy_lat[ERA5_b_idx] ) )
         }

#=================================================================================================#
# Process satellite data.
#-------------------------------------------------------------------------------------------------#
         mat_list_1Hz_dist <- matrix(list(),nrow=13,ncol=3)
         mat_list_Lvec_buoy_samp <- matrix(list(),nrow=13,ncol=3)
         mat_list_buoy_data1 <- matrix(list(),nrow=13,ncol=3)
         mat_list_breaks_master <- matrix(list(),nrow=13,ncol=3)
         mat_list_mean_time <- matrix(list(),nrow=13,ncol=3)
         mat_list_Xing <- matrix(list(),nrow=13,ncol=3)
         list_Xing_lat1 <- vector(mode = "list",length=3)
         mat_list_XXing <- matrix(list(),nrow=13,ncol=3)
         mat_list_Lvec_break_dist_min_idx <- matrix(list(),nrow=13,ncol=3)
      
         mat_list_1Hz_lat <- matrix(list(),nrow=13,ncol=3)
         mat_list_1Hz_lon <- matrix(list(),nrow=13,ncol=3)
         mat_list_1Hz_hs <- matrix(list(),nrow=13,ncol=3)
         mat_list_Lvec_QC <- matrix(list(),nrow=13,ncol=3)
      
         mat_list_median_hs <- matrix(list(),nrow=13,ncol=3)
         mat_list_median_hs_min <- matrix(list(),nrow=13,ncol=3)
         mat_list_1Hz_qual <- matrix(list(),nrow=13,ncol=3)
         mat_list_1Hz_numval <- matrix(list(),nrow=13,ncol=3)
         mat_list_1Hz_rms <- matrix(list(),nrow=13,ncol=3)
         mat_list_median_hs_qual <- matrix(list(),nrow=13,ncol=3)
         mat_list_median_hs_numval <- matrix(list(),nrow=13,ncol=3)
         mat_list_median_hs_rms <- matrix(list(),nrow=13,ncol=3)

#-------------------------------------------------------------------------------------------------#
# Loop 1: First loop over months to identify track segments.
#-------------------------------------------------------------------------------------------------#
         for ( m_idx in 1:13 ) {
# Monthly tandem data at buoy.
            list_buoy_data1 <- list(mat_list_J3[[m_idx,buoy_idx]],mat_list_S6_LRM[[m_idx,buoy_idx]],mat_list_S6_SAR[[m_idx,buoy_idx]])
            mat_list_buoy_data1[m_idx,] <- list(mat_list_J3[[m_idx,buoy_idx]],mat_list_S6_LRM[[m_idx,buoy_idx]],mat_list_S6_SAR[[m_idx,buoy_idx]])

            list_breaks_master <- vector(mode = "list",length = 3)
#-------------------------------------------------------------------------------------------------#
# Loop over missions.
            #for (S_idx in 1:1) {
            for (S_idx in Sidx) {

# Find distance from buoy for all points.
               mat_list_1Hz_dist[[m_idx,S_idx]] <- apply(X=cbind(list_buoy_data1[[S_idx]][[2]],list_buoy_data1[[S_idx]][[3]]-360),MAR=1,FUN=func_buoy_dist,B_idx=buoy_idx)
# Find points within sampling radius (no QC).
               mat_list_Lvec_buoy_samp[[m_idx,S_idx]] <- mat_list_1Hz_dist[[m_idx,S_idx]] < buoy_radius
# Find lat and lon for sampled points.
               mat_list_1Hz_lat[[m_idx,S_idx]] <- mat_list_buoy_data1[[m_idx,S_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
               mat_list_1Hz_lon[[m_idx,S_idx]] <- mat_list_buoy_data1[[m_idx,S_idx]][[3]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
# Find 1 Hz Hs QC information for sampled points.
               mat_list_1Hz_qual[[m_idx,S_idx]]       <- mat_list_buoy_data1[[m_idx,S_idx]][[7]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
               mat_list_1Hz_numval[[m_idx,S_idx]]     <- mat_list_buoy_data1[[m_idx,S_idx]][[8]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
               mat_list_1Hz_rms[[m_idx,S_idx]]        <- mat_list_buoy_data1[[m_idx,S_idx]][[9]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
# 1 Hz Hs and application of QC criteria.
               vec_hs <- vec_hs_QC <- mat_list_buoy_data1[[m_idx,S_idx]][[5]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
               mat_list_Lvec_QC[[m_idx,S_idx]] <- ! ( mat_list_1Hz_numval[[m_idx,S_idx]] < 16 | mat_list_1Hz_rms[[m_idx,S_idx]] > 1.0 )
               vec_hs_QC[ ! mat_list_Lvec_QC[[m_idx,S_idx]] ] <- NA
# Assign QC Hs to mat_list_1Hz_hs.
               mat_list_1Hz_hs[[m_idx,S_idx]] <- vec_hs_QC

# Find time stamps for sampled points.
               nc1_time_idx_cell <- list_buoy_data1[[S_idx]][[4]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]

#-------------------------------------------------------------------------------------------------#
# Loop over time stamps to identify "breaks" between separate tracks.
         nc1_breaks <- NULL
         for (i in 2:length(nc1_time_idx_cell)) {
            #print(paste(" Difference:",( nc1_time_idx_cell[i] - nc1_time_idx_cell[i-1] )))
            if ( abs( nc1_time_idx_cell[i] - nc1_time_idx_cell[i-1] ) > 2 ) {
               nc1_breaks <- c(nc1_breaks,i)
               #print(paste(" Break before:",i))
            }
         }
         mat_nc1_breaks <- cbind(c(1,nc1_breaks),c(nc1_breaks-1,length(nc1_time_idx_cell)))
# Create a list of time stamp sequences for each "break" (track segment).
         list_nc1_breaks_temp <- list()
         for (i in 1:dim(mat_nc1_breaks)[1]) { list_nc1_breaks_temp[[i]] <- mat_nc1_breaks[i,1]:mat_nc1_breaks[i,2] }
         list_nc1_breaks <- list_nc1_breaks_temp[ sapply(X=1:length(list_nc1_breaks_temp),FUN=function(x) { length(list_nc1_breaks_temp[[x]]) > 3 }) ]
         mat_list_breaks_master[[m_idx,S_idx]] <- list_nc1_breaks
# Find minimum distance for each "break" (track segment).
         list_break_dist              <- lapply( X=1:length(list_nc1_breaks), FUN=function(x) { mat_list_1Hz_dist[[m_idx,S_idx]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][ list_nc1_breaks[[x]] ] } )
         list_break_dist_min          <- lapply( X=1:length(list_nc1_breaks), FUN=function(x) { min( list_break_dist[[x]] ) } )
         list_break_dist_min_idx      <- lapply( X=1:length(list_nc1_breaks), FUN=function(x) { AA <- which( list_break_dist[[x]] == list_break_dist_min[[x]] ) } )
         mat_list_Lvec_break_dist_min_idx[[m_idx,S_idx]] <- lapply( X=1:length(list_nc1_breaks), FUN=function(x) { AA <- rep(FALSE,length(list_nc1_breaks[[x]])); AA[list_break_dist_min_idx[[x]]] <- TRUE; AA } )
# Find mean time for each "break" (track segment).
         mat_list_mean_time[[m_idx,S_idx]] <- sapply( X=1:length(list_nc1_breaks), FUN=function(x) { floor( mean( nc1_time_idx_cell[ list_nc1_breaks[[x]] ] ) ) } )

#-------------------------------------------------------------------------------------------------#
# Find increasing ([A]scending) or decreasing ([D]escending) latitude for each segment.
         mat_list_Xing[[m_idx,S_idx]] <- sapply( X=1:length(list_nc1_breaks), FUN=function(x) { N12 <- mat_list_buoy_data1[[m_idx,S_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][ list_nc1_breaks[[x]] ][1:2]; if ( N12[2] > N12[1] ) { "A" } else { "D" } } )

      }
   }
   }

#-------------------------------------------------------------------------------------------------#
# Loop 2: Second loop over months to identify [A]scending, [D]escending tracks and vec_Xing_lat1.
#-------------------------------------------------------------------------------------------------#
# Loop over missions.
   #for (S_idx in 1:3) {
   for (S_idx in Sidx) {

# Use second month (m_idx = 2) to ensure reliable data (m_idx = 1 is partial).
# WARNING: This does not work well when there are "glancing" tracks, with only a few sampled points.
      m_idx <- 2
# Find unqiue (first) latitude point for each repeated segment (max. 4?).
      vec_Xing_temp <- numeric(length(mat_list_Xing[[m_idx,S_idx]]))
      if ( any( mat_list_Xing[[m_idx,S_idx]] == "A" ) ) {
         vec_Xing_temp[ mat_list_Xing[[m_idx,S_idx]] == "A" ] <- sapply( X=1:length(mat_list_breaks_master[[m_idx,S_idx]][ mat_list_Xing[[m_idx,S_idx]] == "A" ]), FUN=function(x) { mat_list_buoy_data1[[m_idx,S_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][ mat_list_Xing[[m_idx,S_idx]] == "A" ][[x]][1]] } )
      }
      if ( any( mat_list_Xing[[m_idx,S_idx]] == "D" ) ) {
         vec_Xing_temp[ mat_list_Xing[[m_idx,S_idx]] == "D" ] <- sapply( X=1:length(mat_list_breaks_master[[m_idx,S_idx]][ mat_list_Xing[[m_idx,S_idx]] == "D" ]), FUN=function(x) { mat_list_buoy_data1[[m_idx,S_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][ mat_list_Xing[[m_idx,S_idx]] == "D" ][[x]][1]] } )
      }

      vec_Xing_lat1 <- NULL
      vec_Xing_lat1[1] <- vec_Xing_temp[1]
      jj <- 2
      if ( length(vec_Xing_temp) > 1 ) {
         for ( jjj in 2:length(vec_Xing_temp) ) { if ( all( abs( vec_Xing_temp[jjj] - vec_Xing_lat1 ) > 0.05 ) ) { vec_Xing_lat1[jj] <- vec_Xing_temp[jjj]; jj <- jj+1 } }
      }

#-------------------------------------------------------------------------------------------------#
# Match first latitude point for each segment against unique latitude points, to get unique [A]scending, [D]escending track labels.
# A1, D2, etc stored in mat_list_XXing.
      for ( m_idx in 1:13 ) {
# Find unqiue (first) latitude point for each repeated segment (max. 4?).
         vec_Xing_temp <- numeric(length(mat_list_Xing[[m_idx,S_idx]]))
         if ( any( mat_list_Xing[[m_idx,S_idx]] == "A" ) ) {
            vec_Xing_temp[ mat_list_Xing[[m_idx,S_idx]] == "A" ] <- sapply( X=1:length(mat_list_breaks_master[[m_idx,S_idx]][ mat_list_Xing[[m_idx,S_idx]] == "A" ]), FUN=function(x) { mat_list_buoy_data1[[m_idx,S_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][ mat_list_Xing[[m_idx,S_idx]] == "A" ][[x]][1]] } )
         }
         if ( any( mat_list_Xing[[m_idx,S_idx]] == "D" ) ) {
            vec_Xing_temp[ mat_list_Xing[[m_idx,S_idx]] == "D" ] <- sapply( X=1:length(mat_list_breaks_master[[m_idx,S_idx]][ mat_list_Xing[[m_idx,S_idx]] == "D" ]), FUN=function(x) { mat_list_buoy_data1[[m_idx,S_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][ mat_list_Xing[[m_idx,S_idx]] == "D" ][[x]][1]] } )
         }

         #vec_XXing_temp <- numeric(length(mat_list_Xing[[m_idx,S_idx]]))
         vec_XXing_temp <- rep(NA,length(mat_list_Xing[[m_idx,S_idx]]))
         for ( ii in 1:length(vec_Xing_lat1) ) {
            vec_XXing_temp[abs( vec_Xing_temp - vec_Xing_lat1[ii] ) < 0.05] <- paste(mat_list_Xing[[m_idx,S_idx]][abs( vec_Xing_temp - vec_Xing_lat1[ii] ) < 0.05],ii,sep='')
         }
# Check for spurious NAs and replace with "best match".
         if ( any(is.na(vec_XXing_temp)) ) {
            if ( (mat_list_Xing[[m_idx,S_idx]] == "D")[which(is.na(vec_XXing_temp))] ) {
               vec_XXing_temp[which(is.na(vec_XXing_temp))] <- vec_XXing_temp[which( mat_list_Xing[[m_idx,S_idx]] == "D" )][ !is.na(vec_XXing_temp[which( mat_list_Xing[[m_idx,S_idx]] == "D" )]) ][1]
            }
         }
# Write output.
         mat_list_XXing[[m_idx,S_idx]] <- vec_XXing_temp
      }

#-------------------------------------------------------------------------------------------------#
# Find average of list_Xing_lat1 to ensure best track segment matching for overlay and temporal analysis.
# Loop over track segments.
      vec_unique_trackID_loc <- unique(unlist(mat_list_XXing[,S_idx])[!is.na(unlist(mat_list_XXing[,S_idx]))])
      for ( ii in 1:length(vec_unique_trackID_loc) ) {
# Loop over months.
         vec_temp <- NULL
         for ( m_idx in 2:13 ) {
            vec_temp <- c( vec_temp,sapply( X=1:length(mat_list_breaks_master[[m_idx,S_idx]][ mat_list_XXing[[m_idx,S_idx]] == vec_unique_trackID_loc[ii] ]), FUN=function(x) { mat_list_buoy_data1[[m_idx,S_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][ mat_list_XXing[[m_idx,S_idx]] == vec_unique_trackID_loc[ii] ][[x]][1]] } ) )
         }
         list_Xing_lat1[[S_idx]][ii] <- median(unlist(vec_temp))
      }
   }
   vec_unique_trackID <- unique(unlist(mat_list_XXing)[!is.na(unlist(mat_list_XXing))])

#=================================================================================================#
# Temporal collocation.
#=================================================================================================#
# Find the "mean" time stamp for each track segment (J3,S6).
      vec_buoy_hs_coloc_ALL <- vec_buoy_ap_coloc_ALL <- vec_ERA5_hs_coloc_ALL <- vec_ERA5_hs_BILIN_coloc_ALL <- NULL

      #mat_list_mean_time1 <- matrix(list(),nrow=13,ncol=3)
      mat_list_time_diff_J3B <- matrix(list(),nrow=13,ncol=3)
      mat_list_time_diff_J3ERA <- matrix(list(),nrow=13,ncol=3)
      Lmat_list_slot_J3B <- vector(mode = "list",length = 13)
      Lmat_list_slot_J3B_TEST <- vector(mode = "list",length = 13)
      Lmat_list_slot_J3ERA <- vector(mode = "list",length = 13)
# AD = all data.
      vec_unique_trackID <- unique(unlist(mat_list_XXing))
      mat_list_buoy_hs_coloc_AD <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
      mat_list_buoy_hs_coloc_AD_TEST <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
      mat_list_ERA5_hs_coloc_AD <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
      mat_list_ERA5_hs_BILIN_coloc_AD <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))

      for ( m_idx in 1:13 ) {
         list_buoy_data1 <- list(mat_list_J3[[m_idx,buoy_idx]],mat_list_S6_LRM[[m_idx,buoy_idx]],mat_list_S6_SAR[[m_idx,buoy_idx]])
         for (S_idx in Sidx) {
            #mat_list_mean_time1[[m_idx,S_idx]] <- sapply( X=1:length(mat_list_breaks_master[[m_idx,S_idx]]), FUN=function(x) { floor( mean( list_buoy_data1[[S_idx]][[4]][ mat_list_Lvec_buoy_samp[[m_idx,S_idx]] ][ mat_list_breaks_master[[m_idx,S_idx]][[x]] ] ) ) } )

# Find buoy time that matches J3 block mean time.
            Lmat_list_slot_J3B[[m_idx]] <- sapply( X=1:length(mat_list_mean_time[[m_idx,S_idx]]),FUN=function(x) { abs(mat_list_mean_time[[m_idx,S_idx]][x] - vec_buoy_time_num) < 1800 & abs(mat_list_mean_time[[m_idx,S_idx]][x] - vec_buoy_time_num) == min( abs(mat_list_mean_time[[m_idx,S_idx]][x] - vec_buoy_time_num) ) } )
            Lmat_list_slot_J3B_TEST[[m_idx]] <- sapply( X=1:length(mat_list_mean_time[[m_idx,S_idx]]),FUN=function(x) { abs(mat_list_mean_time[[m_idx,S_idx]][x] - vec_buoy_time_num) < 1800 & abs(mat_list_mean_time[[m_idx,S_idx]][1] - vec_buoy_time_num) == min( abs(mat_list_mean_time[[m_idx,S_idx]][1] - vec_buoy_time_num) ) } )
# Find absolute time difference.
            mat_list_time_diff_J3B[[m_idx,S_idx]] <- sapply( X=1:length(mat_list_mean_time[[m_idx,S_idx]]),FUN=function(x) { min( abs(mat_list_mean_time[[m_idx,S_idx]][x] - vec_buoy_time_num) ) } )
            mat_list_time_diff_J3B[[m_idx,S_idx]][ sapply( X=1:dim(Lmat_list_slot_J3B[[m_idx]])[2], FUN=function(x) { sum( Lmat_list_slot_J3B[[m_idx]][,x] ) == 0 } ) ] <- NA
# Find buoy Hs and ap for all J3-B colocs.
            vec_buoy_hs_coloc <- vec_buoy_ap_coloc <- rep(NA,length(mat_list_mean_time[[m_idx,S_idx]]))
            vec_buoy_hs_coloc[which(Lmat_list_slot_J3B[[m_idx]],arr.ind=T)[,2]] <- vec_buoy_hs[which(Lmat_list_slot_J3B[[m_idx]],arr.ind=T)[,1]]
            vec_buoy_hs_coloc_ALL <- c(vec_buoy_hs_coloc_ALL,vec_buoy_hs_coloc)

            vec_buoy_ap_coloc[which(Lmat_list_slot_J3B[[m_idx]],arr.ind=T)[,2]] <- vec_buoy_ap[which(Lmat_list_slot_J3B[[m_idx]],arr.ind=T)[,1]]
            vec_buoy_ap_coloc_ALL <- c(vec_buoy_ap_coloc_ALL,vec_buoy_ap_coloc)
# Find buoy Hs and ap for all J3-B colocs, separated by trackID.
            for ( jj in 1:length(vec_unique_trackID) ) {
               vec_hs_temp <- rep(NA,sum(mat_list_XXing[[m_idx,S_idx]] == vec_unique_trackID[jj]))
               if ( length(vec_hs_temp) > 0 ) {
                  mat_hs_idx_temp <- which( Lmat_list_slot_J3B[[m_idx]][,mat_list_XXing[[m_idx,S_idx]] == vec_unique_trackID[jj]],arr.ind=T )
                  if ( length(mat_hs_idx_temp) == 1 ) {
                     mat_list_buoy_hs_coloc_AD[[m_idx,jj]] <- vec_buoy_hs[mat_hs_idx_temp]
                  } else if ( length(mat_hs_idx_temp) == 0 ) {
                     mat_list_buoy_hs_coloc_AD[[m_idx,jj]] <- vec_hs_temp
                  } else {
                     vec_hs_temp[mat_hs_idx_temp[,2]] <- vec_buoy_hs[mat_hs_idx_temp[,1]]
                     mat_list_buoy_hs_coloc_AD[[m_idx,jj]] <- vec_hs_temp
                  }
               }

               mat_list_buoy_hs_coloc_AD_TEST[[m_idx,jj]] <- vec_buoy_hs[which(Lmat_list_slot_J3B[[m_idx]],arr.ind=T)[,1]][mat_list_XXing[[m_idx,1]] == vec_unique_trackID[jj]]
               #mat_list_buoy_hs_coloc_AD_TEST[[m_idx,jj]] <- vec_buoy_hs[which(Lmat_list_slot_J3B_TEST[[m_idx]],arr.ind=T)[,1]][mat_list_XXing[[m_idx,1]] == vec_unique_trackID[jj]]
            }

# Find ERA5 time that matches J3 block mean time.
            Lmat_list_slot_J3ERA[[m_idx]] <- sapply( X=1:length(mat_list_mean_time[[m_idx,S_idx]]),FUN=function(x) { abs(mat_list_mean_time[[m_idx,S_idx]][x] - vec_ERA5_time_num) < 1800 } )
            mat_list_time_diff_J3ERA[[m_idx,S_idx]] <- sapply( X=1:length(mat_list_mean_time[[m_idx,S_idx]]),FUN=function(x) { min( abs(mat_list_mean_time[[m_idx,S_idx]][x] - vec_ERA5_time_num) ) } )
# Find buoy Hs and ap for all J3-ERA colocs.
            vec_ERA5_hs_coloc <- vec_ERA5_hs_BILIN_coloc <- vec_ERA5_ap_coloc <- rep(NA,length(mat_list_mean_time[[m_idx,S_idx]]))
            vec_ERA5_hs_coloc[which(Lmat_list_slot_J3ERA[[m_idx]],arr.ind=T)[,2]] <- vec_ERA5_hs[ which(Lmat_list_slot_J3ERA[[m_idx]],arr.ind=T)[,1] ]
            vec_ERA5_hs_coloc_ALL <- c(vec_ERA5_hs_coloc_ALL,vec_ERA5_hs_coloc)

            vec_ERA5_hs_BILIN_coloc[which(Lmat_list_slot_J3ERA[[m_idx]],arr.ind=T)[,2]] <- vec_ERA5_hs_BILIN[ which(Lmat_list_slot_J3ERA[[m_idx]],arr.ind=T)[,1] ]
            vec_ERA5_hs_BILIN_coloc_ALL <- c(vec_ERA5_hs_BILIN_coloc_ALL,vec_ERA5_hs_BILIN_coloc)
# Find buoy Hs and ap for all J3-ERA5 colocs, separated by trackID.
            for ( jj in 1:length(vec_unique_trackID) ) {
               mat_list_ERA5_hs_coloc_AD[[m_idx,jj]] <- vec_ERA5_hs[which(Lmat_list_slot_J3ERA[[m_idx]],arr.ind=T)[,1]][mat_list_XXing[[m_idx,1]] == vec_unique_trackID[jj]]
               mat_list_ERA5_hs_BILIN_coloc_AD[[m_idx,jj]] <- vec_ERA5_hs_BILIN[which(Lmat_list_slot_J3ERA[[m_idx]],arr.ind=T)[,1]][mat_list_XXing[[m_idx,1]] == vec_unique_trackID[jj]]
            }
         }
      }

# OUT OF S_idx LOOP???
#=================================================================================================#
# Begin correlation algorithm.
#=================================================================================================#
# Function for mode.
      func_mode <- function(x) {
         ux <- unique(x)
         ux[which.max(tabulate(match(x, ux)))]
      }

# Find indices for track IDs (monthly).
      mat_list_trackID <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
      array_list_trackID <- array(list(),dim=c(13,length(vec_unique_trackID),3))
      array_scale_m_idx <- array(NA,dim=c(2,length(vec_unique_trackID),3))

      list_mode <- list()
      list_mode_min <- list()
      vec_mode <- NA
      vec_mode_min <- NA

      for (S_idx in Sidx) {

      for ( jj in 1:length(vec_unique_trackID) ) {
         vec_mode_temp <- NULL
         vec_mode_min_temp <- NULL
# Loop over months.
         for ( m_idx in 1:13 ) {
            array_list_trackID[[m_idx,jj,S_idx]] <- mat_list_trackID[[m_idx,jj]] <- mat_list_breaks_master[[m_idx,S_idx]][which(mat_list_XXing[[m_idx,S_idx]] == vec_unique_trackID[jj])]
# Find mode of trackID length.
            vec_mode_temp <- c(vec_mode_temp,sapply(X=mat_list_trackID[[m_idx,jj]],length))
# Find mode of closest points.
            vec_mode_min_temp <- c(vec_mode_min_temp,sapply(X=mat_list_Lvec_break_dist_min_idx[[m_idx,S_idx]][mat_list_XXing[[m_idx,S_idx]] == vec_unique_trackID[jj]],which))
         }
         vec_mode[jj] <- func_mode(unlist(vec_mode_temp))
         vec_mode_min[jj] <- func_mode(unlist(vec_mode_min_temp))
# Find the month index and
         scale_idx <- which(vec_mode_temp == vec_mode[jj])[2]
         iii <- 1; while( sum( sapply(X=1:iii,FUN=function(x) length( mat_list_trackID[[x,jj]] )) ) < scale_idx ) { iii <- iii + 1 }
         array_scale_m_idx[,jj,S_idx] <- c(iii,scale_idx - sum( sapply(X=1:iii,FUN=function(x) length( mat_list_trackID[[x,jj]] )) ) + length( mat_list_trackID[[iii,jj]] ))
      }
      list_mode[[S_idx]] <- vec_mode
      list_mode_min[[S_idx]] <- vec_mode_min
      }
#      #for ( m_idx in 1:13 ) { print(paste("m_idx:",m_idx)); for ( jj in 1:length(vec_unique_trackID) ) { print(paste("jj:",jj)); for ( ii in 1:length(mat_list_trackID[[m_idx,jj]]) ) { print( length( mat_list_trackID[[m_idx,jj]][[ii]] ) ) } } }
#      #for ( m_idx in 1:13 ) { print(paste("m_idx:",m_idx)); for ( jj in 1 ) { print(paste("jj:",jj)); for ( ii in 1:length(mat_list_trackID[[m_idx,jj]]) ) { print( paste(length( mat_list_trackID[[m_idx,jj]][[ii]] ),mat_list_1Hz_lat[[m_idx,1]][mat_list_trackID[[m_idx,jj]][[ii]][1]] ) ) } } }

#-------------------------------------------------------------------------------------------------#
# Find Hs values for each 1 Hz point in each track ID.
   fl_track_tol <- 0.016
   array_list_trackID_hs <- array(list(),dim=c(13,length(vec_unique_trackID),3))
   mat_list_trackID_hs <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
   mat_list_trackID_hs1 <- matrix(list(),nrow=length(vec_unique_trackID),ncol=3)
   list_lat_test <- list()

   for (S_idx in Sidx) {
# Assign from array (for convenience).
   mat_list_trackID <- array_list_trackID[,,S_idx]
   vec_mode <- list_mode[[S_idx]]

   for ( jj in 1:length(vec_unique_trackID) ) {
   mat_temp1 <- NULL
   vec_lat_test <- NULL
   #print(paste("TrackID:",jj))
      for ( m_idx in 1:13 ) {
         #print(paste("m_idx:",m_idx))
         mat_trackID_hs_temp <- matrix(NA,nrow=length(mat_list_trackID[[m_idx,jj]]),ncol=vec_mode[jj])
         if ( length(mat_list_trackID[[m_idx,jj]]) > 0 ) {
            for ( ii in 1:length(mat_list_trackID[[m_idx,jj]]) ) {
               if ( length( mat_list_trackID[[m_idx,jj]][[ii]] ) == (vec_mode[jj]+1) & abs(mat_list_1Hz_lat[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]][2]] - list_Xing_lat1[[S_idx]][jj]) < fl_track_tol ) {
                  #print( paste(length( mat_list_trackID[[m_idx,jj]][[ii]] ),mat_list_1Hz_lat[[m_idx,1]][mat_list_trackID[[m_idx,jj]][[ii]][2]] ) )
                  #print( paste("Lat diff:",mat_list_1Hz_lat[[m_idx,1]][mat_list_trackID[[m_idx,jj]][[ii]][2]] - vec_Xing_lat1[jj]) )
                  vec_lat_test <- c(vec_lat_test,mat_list_1Hz_lat[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]][2]] - list_Xing_lat1[[S_idx]][jj])
                  mat_trackID_hs_temp[ii,] <- sapply(X=2:length( mat_list_trackID[[m_idx,jj]][[ii]] ),FUN=function(x) { mat_list_1Hz_hs[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]][x]] })

               } else if ( length( mat_list_trackID[[m_idx,jj]][[ii]] ) == (vec_mode[jj]+1) & abs(mat_list_1Hz_lat[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]][1]] - list_Xing_lat1[[S_idx]][jj]) < fl_track_tol ) {
                  #print( paste("Lat diff:",mat_list_1Hz_lat[[m_idx,1]][mat_list_trackID[[m_idx,jj]][[ii]][1]] - vec_Xing_lat1[jj]) )
                  vec_lat_test <- c(vec_lat_test,mat_list_1Hz_lat[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]][1]] - list_Xing_lat1[[S_idx]][jj])
                  mat_trackID_hs_temp[ii,] <- sapply(X=1:(length( mat_list_trackID[[m_idx,jj]][[ii]] )-1),FUN=function(x) { mat_list_1Hz_hs[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]][x]] })

               } else if ( length( mat_list_trackID[[m_idx,jj]][[ii]] ) == vec_mode[jj] & abs(mat_list_1Hz_lat[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]][1]] - list_Xing_lat1[[S_idx]][jj]) < fl_track_tol ) {
                  vec_lat_test <- c(vec_lat_test,mat_list_1Hz_lat[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]][1]] - list_Xing_lat1[[S_idx]][jj])
                  mat_trackID_hs_temp[ii,] <- sapply(X=1:length( mat_list_trackID[[m_idx,jj]][[ii]] ),FUN=function(x) { mat_list_1Hz_hs[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]][x]] })

               } else if ( length( mat_list_trackID[[m_idx,jj]][[ii]] ) == vec_mode[jj] & abs(mat_list_1Hz_lat[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]][2]] - list_Xing_lat1[[S_idx]][jj]) < fl_track_tol ) {
                  vec_lat_test <- c(vec_lat_test,mat_list_1Hz_lat[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]][1]] - vec_Xing_lat1[jj])
                  mat_trackID_hs_temp[ii,1:(length( mat_list_trackID[[m_idx,jj]][[ii]])-1)] <- sapply(X=2:length( mat_list_trackID[[m_idx,jj]][[ii]] ),FUN=function(x) { mat_list_1Hz_hs[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]][x]] })

               } else if ( length( mat_list_trackID[[m_idx,jj]][[ii]] ) == (vec_mode[jj]-1) & abs(mat_list_1Hz_lat[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]][1]] - list_Xing_lat1[[S_idx]][jj]) < fl_track_tol ) {
                  vec_lat_test <- c(vec_lat_test,mat_list_1Hz_lat[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]][1]] - vec_Xing_lat1[jj])
                  mat_trackID_hs_temp[ii,1:length( mat_list_trackID[[m_idx,jj]][[ii]])] <- sapply(X=1:length( mat_list_trackID[[m_idx,jj]][[ii]] ),FUN=function(x) { mat_list_1Hz_hs[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]][x]] })

               } else {
                  print( paste("EXCEED TOLERANCE ** Month: ",m_idx,"; TrackID: ",vec_unique_trackID[jj],"; Mode length: ",vec_mode[jj],"; Actual length: ",length( mat_list_trackID[[m_idx,jj]][[ii]]),"; #: ",ii,sep="") )
               }
            }
            array_list_trackID_hs[[m_idx,jj,S_idx]] <- mat_list_trackID_hs[[m_idx,jj]] <- mat_trackID_hs_temp
# Create a list of single array of all Hs trackID data (not monthly).
            mat_temp1 <- rbind(mat_temp1,mat_trackID_hs_temp)
         } else {
            print( paste("NO TRACKS        ** Month: ",m_idx,"; TrackID: ",vec_unique_trackID[jj],"; Mode length: ",vec_mode[jj],sep="") )
         }
      }
      mat_list_trackID_hs1[[jj,S_idx]] <- mat_temp1
   }
   }

# OLD
#   list_trackID_hs <- list()
#   for ( jj in 1:length(vec_unique_trackID) ) {
#      mat_temp <- NULL
#      for ( m_idx in 1:13 ) { mat_temp <- rbind(mat_temp,mat_list_trackID_hs[[m_idx,jj]]) }
#      #for ( m_idx in 1:4 ) { mat_temp <- rbind(mat_temp,mat_list_trackID_hs[[m_idx,jj]]) }
#      list_trackID_hs[[jj]] <- mat_temp
#   }

#=================================================================================================#
# Alogirthm for calculating along track 1 Hz correlations.
# Select target for correlation (buoy, ERA, ERA5_bilin).
   plot_data <- mat_list_buoy_hs_coloc_AD
   #plot_data <- mat_list_ERA5_hs_coloc_AD
   #plot_data <- mat_list_ERA5_hs_BILIN_coloc_AD

   for (S_idx in Sidx) {
# Assign from array (for convenience).
   mat_list_trackID <- array_list_trackID[,,S_idx]
   list_trackID_hs <- mat_list_trackID_hs1[,S_idx]

   list_vec_cor <- list()
   list_vec_cor_95 <- list()
   list_1Hz_idx_dist <- list()
# Loop over unique tracks.
   for ( jj in 1:length(vec_unique_trackID) ) {
# Get track length.
      i_len_trackID <- dim(list_trackID_hs[[jj]])[2]
      list_vec_cor[[jj]] <- rep(NA,i_len_trackID)
# Find inter-point distance in km using an appropriate track (corresponds to vec_mode).
# array_scale_m_idx contains the required indices.
      sc_midx <- array_scale_m_idx[1,jj,S_idx]
      sc_idxB <- array_scale_m_idx[2,jj,S_idx]
      mat_trackID_latlon <- cbind(mat_list_1Hz_lat[[sc_midx,S_idx]][mat_list_trackID[[sc_midx,jj]][[sc_idxB]]],mat_list_1Hz_lon[[sc_midx,S_idx]][mat_list_trackID[[sc_midx,jj]][[sc_idxB]]])
      colnames(mat_trackID_latlon) <- c("lat","lon")
      vec_trackID_dist <- sapply(X=1:(i_len_trackID-1),FUN=function(x) { func_sat_dist(mat_trackID_latlon[x,],mat_trackID_latlon[x+1,]) })
      list_1Hz_idx_dist[[jj]] <- c( sapply(X=1:(vec_mode_min[jj]-1),FUN=function(x) {-sum(vec_trackID_dist[x:(vec_mode_min[jj]-1)])}),
                                    0,
                                    sapply(X=(vec_mode_min[jj]):(i_len_trackID-1),FUN=function(x) {sum(vec_trackID_dist[vec_mode_min[jj]:x])}) )
# Test for sufficient number of points to compute correlation.
      for ( ii in 1:i_len_trackID ) {
         if ( sum( !is.na( unlist(plot_data[,jj]) ) & !is.na( list_trackID_hs[[jj]][,ii] ) ) > 10 ) {
            list_vec_cor[[jj]][ii] <- cor(unlist(plot_data[,jj]),list_trackID_hs[[jj]][,ii],use="pairwise.complete.obs")
         }
      }
      list_vec_cor_95[[jj]] <- list_vec_cor[[jj]] > 0.90
   }

#-------------------------------------------------------------------------------------------------#
# Plotting.
## Plot median of super-observation.
#   X11(); plot(unlist(mat_list_buoy_hs_coloc_AD[,1]),apply(X=list_trackID_hs[[1]],MAR=1,FUN=median ),xlim=c(0,8),ylim=c(0,8)); abline(0,1)
## Plot by sub-point.
#   X11(); plot(unlist(mat_list_buoy_hs_coloc_AD[,1]),list_trackID_hs[[1]][,1],xlim=c(0,8),ylim=c(0,8)); abline(0,1)

   if ( flag_plot_cor) {

      system(paste("if [ ! -d ./figures/test_sampling1/",buoy_list[buoy_idx]," ]; then mkdir ./figures/test_sampling1/",buoy_list[buoy_idx]," &> /dev/null; fi",sep=""))
      fig_cor_file_name <- paste("./figures/test_sampling1/",buoy_list[buoy_idx],"/track_cor_",vec_tandem_labs[S_idx],"_",buoy_list[buoy_idx],"_",buoy_radius,"km_016.pdf",sep="")
      pdf(fig_cor_file_name,width = (4.0 * length(vec_unique_trackID)), height = 8.4)
      par(mfrow=c(2,length(vec_unique_trackID)),mar=c(5,4,4,5),mgp=c(3.1,1,0))

      for ( jj in 1:length(vec_unique_trackID) ) {
         i_len_trackID <- dim(list_trackID_hs[[jj]])[2]
         title_top <- paste(vec_tandem_labs[S_idx],"Track ID:",vec_unique_trackID[[jj]],"\nBuoy:",buoy_list[buoy_idx])
         #plot(1:i_len_trackID,list_vec_cor[[jj]],ylim=c(0.5,1.0),xlab="1 Hz ground distance (km)",ylab="Correlation",main=paste("Track ID:",vec_unique_trackID[[jj]],"\nBuoy:",buoy_list[buoy_idx]),axes=F)
         plot(1:i_len_trackID,list_vec_cor[[jj]],ylim=c(0.5,1.0),xlab="1 Hz ground distance (km)",ylab="Correlation",main=title_top,axes=F)
         axis(side=2,at=seq(0.5,1.0,0.1),labels=seq(0.5,1.0,0.1))
         axis(side=1,at=1:i_len_trackID,labels=format(list_1Hz_idx_dist[[jj]],digits=2),las=2)
         abline(h=c(0.95,1.0),col="grey")
         abline(v=seq(5,40,5),col="grey")
         abline(v=vec_mode_min[jj],col="blue")
         points((1:i_len_trackID)[vec_mode_min[jj]],list_vec_cor[[jj]][vec_mode_min[jj]],pch=19,col="blue")
         par(new=T)
         plot(1:i_len_trackID,sapply(X=1:i_len_trackID,FUN=function(x) { sum( !is.na( unlist(plot_data[,jj]) ) & !is.na( list_trackID_hs[[jj]][,x] ) ) }),pch=4,ylim=c(0,60),axes=F,xlab="",ylab="")
         axis(side=4,at=seq(0,40,5))
         if ( jj == 1) {
            legend(1,20,legend=c("Point closest to buoy","Correlation","Number of temporal samples"),pch=c(19,1,4),col=c("blue","black","black"))
         }
# Histogram of collocation time differences.
         if ( jj == 1) {
            hist(unlist( mat_list_time_diff_J3B )/60,breaks=30,xlim=c(0,30),main=paste("Distribution of collocation\ntime differences. Total:",sum( !is.na( unlist( mat_list_time_diff_J3B ) ) )),xlab="Time difference (minutes)")
         }
      }

      dev.off()
      system(paste("okular",fig_cor_file_name,"&> /dev/null &"))
   }
   }

#=================================================================================================#
# Triple collocation.
#=================================================================================================#




# Calculate median values based on "adaptive" sampling.
   mat_list_trackID_hs_med <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
   mat_list_trackID_hs_med_adapt <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
   mat_list_trackID_hs_med_rand <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
   mat_list_trackID_hs_min <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
   for ( jj in 1:length(vec_unique_trackID) ) {
      i_len_trackID <- dim(list_trackID_hs[[jj]])[2]
      for ( m_idx in 1:13 ) {
         if ( !is.null(mat_list_trackID_hs[[m_idx,jj]]) ) {
            mat_list_trackID_hs_med[[m_idx,jj]] <- sapply(X=1:dim(mat_list_trackID_hs[[m_idx,jj]])[1],FUN=function(x) { median(mat_list_trackID_hs[[m_idx,jj]][x,],na.rm=T) })
            mat_list_trackID_hs_med_adapt[[m_idx,jj]] <- sapply(X=1:dim(mat_list_trackID_hs[[m_idx,jj]])[1],FUN=function(x) { median(mat_list_trackID_hs[[m_idx,jj]][x,list_vec_cor_95[[jj]]],na.rm=T) })
            mat_list_trackID_hs_med_rand[[m_idx,jj]] <- sapply(X=1:dim(mat_list_trackID_hs[[m_idx,jj]])[1],FUN=function(x) { median(mat_list_trackID_hs[[m_idx,jj]][x,runif(i_len_trackID) > 0.1],na.rm=T) })
            mat_list_trackID_hs_min[[m_idx,jj]] <- sapply(X=1:dim(mat_list_trackID_hs[[m_idx,jj]])[1],FUN=function(x) { mat_list_trackID_hs[[m_idx,jj]][x,vec_mode_min[[jj]]] })
         }
      }
   }
   print(paste(" SQRT ERROR VAR [MED - ADAPT] **",sqrt( var( unlist(mat_list_trackID_hs_med)-unlist(mat_list_trackID_hs_med_adapt), na.rm=T ) ) ) )
   print(paste(" SQRT ERROR VAR [MED - MIN]   **",sqrt( var( unlist(mat_list_trackID_hs_med)-unlist(mat_list_trackID_hs_min), na.rm=T ) ) ) )

#   vec_unique_trackID
#   print(paste(" SQRT ERROR VAR  **",sqrt( var( unlist(plot_data[,1])-unlist(mat_list_trackID_hs_med[,1]), na.rm=T ) ) ) )
#   print(paste(" SQRT ERROR VAR  **",sqrt( var( unlist(plot_data[,1])-unlist(mat_list_trackID_hs_med_adapt[,1]), na.rm=T ) ) ) )
#   print(paste(" SQRT ERROR VAR  **",sqrt( var( unlist(plot_data[,2])-unlist(mat_list_trackID_hs_med[,2]), na.rm=T ) ) ) )
#   print(paste(" SQRT ERROR VAR  **",sqrt( var( unlist(plot_data[,2])-unlist(mat_list_trackID_hs_med_adapt[,2]), na.rm=T ) ) ) )
#   print(paste(" SQRT ERROR VAR  **",sqrt( var( unlist(plot_data[,3])-unlist(mat_list_trackID_hs_med[,3]), na.rm=T ) ) ) )
#   print(paste(" SQRT ERROR VAR  **",sqrt( var( unlist(plot_data[,3])-unlist(mat_list_trackID_hs_med_adapt[,3]), na.rm=T ) ) ) )

   print(paste(" SQRT ERROR VAR [PLOT - MED]   **",sqrt( var( unlist(plot_data)-unlist(mat_list_trackID_hs_med), na.rm=T ) ) ) )
   print(paste(" SQRT ERROR VAR [PLOT - ADAPT] **",sqrt( var( unlist(plot_data)-unlist(mat_list_trackID_hs_med_adapt), na.rm=T ) ) ) )
   print(paste(" SQRT ERROR VAR [PLOT - MIN] **",sqrt( var( unlist(plot_data)-unlist(mat_list_trackID_hs_min), na.rm=T ) ) ) )

### Trouble shooting.
### E.g. buoy 13, ii <- 8
###   X11(); plot(unlist(plot_data[,jj]),list_trackID_hs[[jj]][,ii]); abline(0,1)
### Find point mismatch and time index (24).
###   cbind(unlist(plot_data[,jj]),list_trackID_hs[[jj]][,ii])
### Correspond time index to month (m_idx). E.g. 38 A or D tracks for 13 months.
### Identify specific point.
###   mat_list_1Hz_hs[[9,1]][66:70]
### Identify quality issues.
###   mat_list_1Hz_rms[[9,1]][66:70]
##
##
#### Find S6LRM time that matches J-3 block mean time.
#### These will be mostly identical but occasionally there is missing data with no match-up.
###      mat_slot_J3S6L <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { abs(list_mean_time[[1]][x] - list_mean_time[[2]]) < 40 } )
###      if ( !is.matrix(mat_slot_J3S6L) ) { mat_slot_J3S6L <- t(as.matrix(mat_slot_J3S6L)) }
###
#### Find S6SAR time that matches J-3 block mean time.
###      mat_slot_J3S6SAR <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { abs(list_mean_time[[1]][x] - list_mean_time[[3]]) < 40 } )
###      if ( !is.matrix(mat_slot_J3S6SAR) ) { mat_slot_J3S6SAR <- t(as.matrix(mat_slot_J3S6SAR)) }
##
###-------------------------------------------------------------------------------------------------#
##
