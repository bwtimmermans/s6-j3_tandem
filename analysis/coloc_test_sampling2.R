# source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/coloc_test_sampling2.R")
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
   flag_plot_cor <- FALSE
   flag_plot_multi_cor <- FALSE
   flag_TC <- FALSE

# Sea state sampling by wave period.
   flag_period_thresh <- FALSE
   flag_swell_only <- FALSE
   period_thresh <- 8

# Data and sampling.
   buoy_radius <- 100
   cor_thresh <- 0.92

   flag_ERA5_BILIN <- FALSE

# OS.
   Bidx <- c(1,4,5,6,8)
# NS (75 km).
#   Bidx <- c(1:10,12:16,18:27,29:34)

   Sidx <- c(1,3)

   flag_S6SAR_correction <- TRUE
   if ( flag_S6SAR_correction ) {
      path_S6SAR_correction_model <- "/home/ben/research/NOC/projects/s6-j3_tandem/analysis/S6_correction/lm_J3mS6SAR_46066_46078.Robj"
      load(path_S6SAR_correction_model)
   }
   #lm_J3mS6SAR

   vec_tandem_labs <- c("J3","S6LRM","S6SAR")

   lab_month <- c("Dec (2020)","Jan (2021)","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# Offshore of neashore.
   flag_OS <- TRUE

   if ( flag_OS ) {
#-------------------------------------------------------------------------------------------------#
# PAC Offshore (active).
      fig_lab_region <- "OS"
      b_idx_list <- 3:13
#-------------------------------------------------------------------------------------------------#
# as.POSIXct(S6_46246_march[[4]][1:100], origin = '2000-01-01', tz='GMT')
# Attach J3.
      attach("./output/buoys_J3/list_buoy_data_swh_ocean_PAC_OS.Robj")
      mat_list_J3 <- list_buoy_data[[2]]
      detach()

# Attach S6 LRM.
      attach("./output/buoys_S6/list_buoy_data_LRM_swh_ocean_PAC_OS_F06.Robj")
      mat_list_S6_LRM <- list_buoy_data[[2]]
      detach()

# Attach S6 SAR.
      attach("./output/buoys_S6/list_buoy_data_SAR_swh_ocean_PAC_OS_f06.Robj")
      mat_list_S6_SAR <- list_buoy_data[[2]]
      detach()

# Attach ERA5.
      attach("./output/ERA5/buoy_array_PAC_OS_2020-2022.Robj")
      mat_list_ERA5 <- list_buoy_data[[2]]
      detach()
   } else {
#-------------------------------------------------------------------------------------------------#
# PAC Nearshore (active).
      fig_lab_region <- "NS"
      b_idx_list <- (1:52)[-c(7:14,20,22,30,33,35,46,48,51,52)]
#-------------------------------------------------------------------------------------------------#
# Attach J3.
      attach("./output/buoys_J3/list_buoy_data_swh_ocean_PAC_NS.Robj")
      mat_list_J3 <- list_buoy_data[[2]]
      detach()

# Attach S6 LRM.
      attach("./output/buoys_S6/list_buoy_data_LRM_swh_ocean_PAC_NS_F06.Robj")
      mat_list_S6_LRM <- list_buoy_data[[2]]
      detach()

# Attach S6 SAR.
      attach("./output/buoys_S6/list_buoy_data_SAR_swh_ocean_PAC_NS_f06.Robj")
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
# Functions for inter-1 Hz point distance, with +ve or -ve.
# [A]scending track.
   func_sat_dist1A <- function(x,y) {
      fl_d_lat = func_rads(x[1]) - func_rads(y[1])
      fl_d_lon = func_rads(x[2]) - func_rads(y[2])
      fl_h = sin(fl_d_lat / 2) * sin(fl_d_lat / 2) + cos( func_rads(y[1]) ) * cos( func_rads(x[1]) ) * sin(fl_d_lon / 2) * sin(fl_d_lon / 2)
      sign(x[1]-y[1]) * 2 * i_radius * asin(sqrt(fl_h))
   }
# [D]escending track.
   func_sat_dist1D <- function(x,y) {
      fl_d_lat = func_rads(x[1]) - func_rads(y[1])
      fl_d_lon = func_rads(x[2]) - func_rads(y[2])
      fl_h = sin(fl_d_lat / 2) * sin(fl_d_lat / 2) + cos( func_rads(y[1]) ) * cos( func_rads(x[1]) ) * sin(fl_d_lon / 2) * sin(fl_d_lon / 2)
      sign(y[1]-x[1]) * 2 * i_radius * asin(sqrt(fl_h))
   }

#-------------------------------------------------------------------------------------------------#
# Master lists for each buoy.
   list_B_mat_list_1Hz_dist <- list()
   list_B_mat_list_Lvec_buoy_samp <- list()
   list_B_mat_list_buoy_data1 <- list()
   list_B_mat_list_breaks_master <- list()
   list_B_mat_list_Xing <- list()
   list_B_mat_list_1Hz_hs <- list()
   list_B_mat_list_Lvec_break_dist_min_idx <- list()
   list_B_mat_list_mean_time <- list()

   array_list_XXing <- array(list(),dim=c(13,3,length(Bidx)))
   array_list_bins_master <- array(list(),dim=c(13,3,length(Bidx)))
   list_B_vec_buoy_time_num <- list()
   list_B_vec_buoy_hs <- list()
   list_B_vec_buoy_ap <- list()

   for (b_idx in 1:length(Bidx)) {

      ERA5_b_idx <- buoy_idx <- b_idx_list[Bidx[b_idx]]

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

      list_B_vec_buoy_time_num[[b_idx]] <- vec_buoy_time_num <- as.numeric( vec_buoy_time[date_idx] ) - 946684800
      list_B_vec_buoy_hs[[b_idx]] <- vec_buoy_hs <- mat_buoy_csv$hs[date_idx]
      list_B_vec_buoy_ap[[b_idx]] <- vec_buoy_ap <- mat_buoy_csv$ap[date_idx]

#=================================================================================================#
# Process ERA5 data.
# Buoy time offset: 946684800
# as.POSIXct( vec_ERA5_time_num*3600, origin = "1900-01-01",tz='GMT')
#-------------------------------------------------------------------------------------------------#
      vec_ERA5_time_num <- vec_ERA5_hs <- vec_ERA5_ap <- NULL
      for ( i_month in 1:dim(mat_list_ERA5)[1]) {
         vec_ERA5_time_num <- c(vec_ERA5_time_num,mat_list_ERA5[[i_month,ERA5_b_idx]][[4]])
# Fix for the centre point (nearest neighbour) being NA.
# if ( sum(is.na(mat_list_ERA5[[i_month,ERA5_b_idx]][[5]][[1]][2,2,])) > 10 ) { print("YES") }
# mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lon_mid
# mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lat_mid
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
         #array_list_Xing <- array(list(),dim=c(13,3,length(b_idx_list)))

         #list_Xing_lat1 <- vector(mode = "list",length=3)
         #mat_list_XXing <- matrix(list(),nrow=13,ncol=3)
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
            mat_list_buoy_data1[m_idx,] <- list(mat_list_J3[[m_idx,buoy_idx]],mat_list_S6_LRM[[m_idx,buoy_idx]],mat_list_S6_SAR[[m_idx,buoy_idx]])
            #list_buoy_data1 <- list(mat_list_J3[[m_idx,buoy_idx]],mat_list_S6_LRM[[m_idx,buoy_idx]],mat_list_S6_SAR[[m_idx,buoy_idx]])
            list_buoy_data1 <- mat_list_buoy_data1[m_idx,]

#-------------------------------------------------------------------------------------------------#
# Loop over missions.
            #for (S_idx in 1:1) {
            for (S_idx in Sidx) {

# Find distance from buoy for all points.
               mat_list_1Hz_dist[[m_idx,S_idx]] <- apply(X=cbind(list_buoy_data1[[S_idx]][[2]],list_buoy_data1[[S_idx]][[3]]-360),MAR=1,FUN=func_buoy_dist,B_idx=buoy_idx)
# Find points within sampling radius (no QC).
               mat_list_Lvec_buoy_samp[[m_idx,S_idx]] <- mat_list_1Hz_dist[[m_idx,S_idx]] < buoy_radius
# Fix for out-of-orbit early S6 LRM data.
               if ( m_idx == 1 & S_idx == 2) { mat_list_Lvec_buoy_samp[[m_idx,S_idx]][1:141] <- FALSE }
# Extract lat and lon for sampled points.
               mat_list_1Hz_lat[[m_idx,S_idx]] <- mat_list_buoy_data1[[m_idx,S_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
               mat_list_1Hz_lon[[m_idx,S_idx]] <- mat_list_buoy_data1[[m_idx,S_idx]][[3]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
# Extract 1 Hz Hs QC information for sampled points.
               mat_list_1Hz_qual[[m_idx,S_idx]]       <- mat_list_buoy_data1[[m_idx,S_idx]][[7]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
               mat_list_1Hz_numval[[m_idx,S_idx]]     <- mat_list_buoy_data1[[m_idx,S_idx]][[8]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
               mat_list_1Hz_rms[[m_idx,S_idx]]        <- mat_list_buoy_data1[[m_idx,S_idx]][[9]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
# Extract 1 Hz Hs.
               vec_hs <- mat_list_buoy_data1[[m_idx,S_idx]][[5]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
# Apply linear model correction to S6SAR.
               if ( S_idx == 3 & flag_S6SAR_correction ) {
                  vec_hs[ vec_hs < 0.2 ] <- NA
                  vec_hs_QC <- predict.lm(object=lm_J3mS6SAR,newdata=data.frame(S6SAR=vec_hs))
               } else {
                  vec_hs_QC <- vec_hs
               }
# Apply quality controls.
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
               if ( abs( nc1_time_idx_cell[i] - nc1_time_idx_cell[i-1] ) > 10 ) {
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
            mat_list_Xing[[m_idx,S_idx]] <- sapply( X=1:length(list_nc1_breaks), FUN=function(x) { vec_X_lat <- mat_list_buoy_data1[[m_idx,S_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][ list_nc1_breaks[[x]] ]; CC <- NA; kk <- 2; while( is.na(CC) ) { CC <- vec_X_lat[kk]-vec_X_lat[1] ; kk <- kk + 1 }; if ( CC > 0 ) { "A" } else { "D" } } )
            #array_list_Xing[[m_idx,S_idx,b_idx]] <- sapply( X=1:length(list_nc1_breaks), FUN=function(x) { vec_X_lat <- mat_list_buoy_data1[[m_idx,S_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][ list_nc1_breaks[[x]] ]; CC <- NA; kk <- 2; while( is.na(CC) ) { CC <- vec_X_lat[kk]-vec_X_lat[1] ; kk <- kk + 1 }; if ( CC > 0 ) { "A" } else { "D" } } )

         }
      }
# Capture data for multiple buoys.
      list_B_mat_list_buoy_data1[[b_idx]] <- mat_list_buoy_data1
      list_B_mat_list_1Hz_dist[[b_idx]] <- mat_list_1Hz_dist
      list_B_mat_list_Lvec_buoy_samp[[b_idx]] <- mat_list_Lvec_buoy_samp
      list_B_mat_list_breaks_master[[b_idx]] <- mat_list_breaks_master
      list_B_mat_list_Xing[[b_idx]] <- mat_list_Xing
      list_B_mat_list_1Hz_hs[[b_idx]] <- mat_list_1Hz_hs
      list_B_mat_list_Lvec_break_dist_min_idx[[b_idx]] <- mat_list_Lvec_break_dist_min_idx
      list_B_mat_list_mean_time[[b_idx]] <- mat_list_mean_time
   }

#-------------------------------------------------------------------------------------------------#
# Loop 2: Second loop over months to identify different [A]scending or [D]escending tracks.
# Here we take each track segment and match individuals points (defined by lon,lat) to other tracks.
#-------------------------------------------------------------------------------------------------#
# Loop over buoys.
   for (b_idx in 1:length(Bidx)) {
# Assign variables from lists.
      mat_list_Lvec_buoy_samp <- list_B_mat_list_Lvec_buoy_samp[[b_idx]]
      mat_list_Xing <- list_B_mat_list_Xing[[b_idx]]
      mat_list_breaks_master <- list_B_mat_list_breaks_master[[b_idx]]
      mat_list_buoy_data1 <- list_B_mat_list_buoy_data1[[b_idx]]
# Loop over missions.
      for (S_idx in Sidx) {
# Assign a numeric label for the separate tracks.
         tr_lab_idx <- 1
         list_tr_test <- list()
# Loop over each of the A and D tracks to find the longest.
# The longest track has the largest number of points to compare with.
      #for (m_idx in c(2,1,3:13)) {

         #mm_idx <- 1
         #vec_m_test <- unlist(mat_list_Xing[1:mm_idx,1])
	 vec_unique_AD <- unique( unlist(mat_list_Xing[,S_idx]) )
	 if ( length(vec_unique_AD) > 1 ) {
            mm_idx <- which( sapply(X=1:length(mat_list_Xing[,S_idx]),function(x) length(mat_list_Xing[[x,S_idx]])) == max( sapply(X=1:length(mat_list_Xing[,S_idx]),function(x) length(mat_list_Xing[[x,S_idx]])) ) )[1]
            #while ( ! ( any( vec_m_test == "A" ) & any( vec_m_test == "D" ) ) ) { mm_idx <- mm_idx + 1; vec_m_test <- unlist(mat_list_Xing[1:mm_idx,1]) }
	 } else {
	    mm_idx <- 1
	 }

         for (m_idx in mm_idx) {
            vec_XXing_temp <- rep(NA,length(mat_list_Xing[[m_idx,S_idx]]))
            for (AD_idx in 1:length(vec_unique_AD)) {
               #tr_ch <- c("A","D")[AD_idx]
               tr_ch <- sort(vec_unique_AD)[AD_idx]

               if ( any( mat_list_Xing[[m_idx,S_idx]] == tr_ch ) ) {

               list_tr_test_temp <- list()

               tr_A_idx <- which(mat_list_Xing[[m_idx,S_idx]] == tr_ch)
               tr_A_len <- sapply(X=mat_list_breaks_master[[m_idx,S_idx]][ tr_A_idx ],length)
               tr_A_long_idx <- which(tr_A_len == max(tr_A_len))[1] 
# Find the longest track to test against, and label this "1".
               vec_tr_test <- mat_list_breaks_master[[m_idx,S_idx]][[ tr_A_idx[tr_A_long_idx] ]]
               vec_XXing_temp[ tr_A_idx[tr_A_long_idx] ] <- paste(tr_ch,tr_lab_idx,sep="")
               list_tr_test_temp[[tr_lab_idx]] <- vec_tr_test
# Loop over all A or D tracks that are not the longest (for perform comparison).
               for ( tr_idx in tr_A_idx[-tr_A_long_idx] ) {
# It's not inconceivable that the first index does not match any points in the longest segment.
# Perhaps we just keep looping until we get a match.
                  mat_tr_lonlat <- cbind(
                                      mat_list_buoy_data1[[m_idx,S_idx]][[3]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][[tr_idx]]],
                                      mat_list_buoy_data1[[m_idx,S_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][[tr_idx]]] )
# Cycle through 1 Hz points (index tr_p) along the "test track" (vec_tr_test,longest track).
# It's not inconceivable that the first index does not match any points in the longest segment.
# Perhaps we just keep looping until we get a match.
                  if ( length(vec_tr_test) < 10 ) {
                     tr_p_len <- length(vec_tr_test)
		  } else {
                     tr_p_len <- 10
		  }
                  Lvec_match <- rep(TRUE,tr_p_len)
                  for ( tr_p in 1:tr_p_len ) {
                     #print(paste("tr_p:",tr_p))
                     vec_tr_test_p <- cbind(
                                         mat_list_buoy_data1[[mm_idx,S_idx]][[3]][mat_list_Lvec_buoy_samp[[mm_idx,S_idx]]][vec_tr_test[tr_p]],
                                         mat_list_buoy_data1[[mm_idx,S_idx]][[2]][mat_list_Lvec_buoy_samp[[mm_idx,S_idx]]][vec_tr_test[tr_p]] )
# For all tracks excluding the longest, cycle through 1 Hz points (index yy) until a match is found.
                     #tr_p_dist <- 1; yy <- 1; while( tr_p_dist > 0.06 ) { tr_p_dist <- sum( abs( mat_tr_lonlat[yy,] - vec_tr_test_p ) ); print(tr_p_dist); yy <- yy + 1; if (yy > 30) { tr_p_dist <- 0; Lvec_match[tr_p] <- FALSE } }
                     tr_p_dist <- 1; yy <- 1; while( tr_p_dist > 0.06 ) { tr_p_dist <- sum( abs( mat_tr_lonlat[yy,] - vec_tr_test_p ) ); yy <- yy + 1; if (yy > dim(mat_tr_lonlat)[1]) { tr_p_dist <- 0; Lvec_match[tr_p] <- FALSE } }
                  }
                  if ( ! any(Lvec_match) ) {
                     vec_XXing_temp[tr_idx] <- paste(tr_ch,tr_lab_idx+1,sep="")
                     #list_tr_test_temp[[tr_lab_idx+1]] <- mat_list_breaks_master[[m_idx,S_idx]][ tr_idx ]
                  } else {
                     vec_XXing_temp[tr_idx] <- paste(tr_ch,tr_lab_idx,sep="")
                  }
               }
               #list_tr_test[[AD_idx]] <- list_tr_test_temp
               list_tr_test[[AD_idx]] <- vec_tr_test
               }
            }
            array_list_XXing[[m_idx,S_idx,b_idx]] <- vec_XXing_temp
         }
#-------------------------------------------------------------------------------------------------#
         for (m_idx in (1:13)[-mm_idx]) {
         #for (m_idx in 6) {
            vec_XXing_temp <- rep(NA,length(mat_list_Xing[[m_idx,S_idx]]))
            for (AD_idx in 1:length(vec_unique_AD)) {
               #tr_ch <- c("A","D")[AD_idx]
               tr_ch <- sort(vec_unique_AD)[AD_idx]
               #list_tr_test_temp <- list()

               tr_A_idx <- which(mat_list_Xing[[m_idx,S_idx]] == tr_ch)
# Find the longest track to test against, and label this "1".
               vec_tr_test <- list_tr_test[[AD_idx]]
# Loop over all A or D tracks.
               for ( tr_idx in tr_A_idx ) {
# It's not inconceivable that the first index does not match any points in the longest segment.
# Perhaps we just keep looping until we get a match.
                  mat_tr_lonlat <- cbind(
                                      mat_list_buoy_data1[[m_idx,S_idx]][[3]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][[tr_idx]]],
                                      mat_list_buoy_data1[[m_idx,S_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][[tr_idx]]] )
# Cycle through 1 Hz points (index tr_p) along the "test track" (vec_tr_test,longest track).
# It's not inconceivable that the first index does not match any points in the longest segment.
# Perhaps we just keep looping until we get a match.
                  if ( length(vec_tr_test) < 10 ) {
                     tr_p_len <- length(vec_tr_test)
		  } else {
                     tr_p_len <- 10
		  }
                  Lvec_match <- rep(TRUE,tr_p_len)
                  for ( tr_p in 1:tr_p_len ) {
                     #print(paste("tr_p:",tr_p))
                     vec_tr_test_p <- cbind(
                                         mat_list_buoy_data1[[mm_idx,S_idx]][[3]][mat_list_Lvec_buoy_samp[[mm_idx,S_idx]]][vec_tr_test[tr_p]],
                                         mat_list_buoy_data1[[mm_idx,S_idx]][[2]][mat_list_Lvec_buoy_samp[[mm_idx,S_idx]]][vec_tr_test[tr_p]] )
# For all tracks excluding the longest, cycle through 1 Hz points (index yy) until a match is found.
                     #tr_p_dist <- 1; yy <- 1; while( tr_p_dist > 0.060 ) { print(paste("tr_p_dist:",tr_p_dist)); tr_p_dist <- sum( abs( mat_tr_lonlat[yy,] - vec_tr_test_p ) ); yy <- yy + 1; if (yy > dim(mat_tr_lonlat)[1]) { tr_p_dist <- 0; Lvec_match[tr_p] <- FALSE } }
                     tr_p_dist <- 1; yy <- 1; while( tr_p_dist > 0.060 ) { tr_p_dist <- sum( abs( mat_tr_lonlat[yy,] - vec_tr_test_p ) ); yy <- yy + 1; if (yy > dim(mat_tr_lonlat)[1]) { tr_p_dist <- 0; Lvec_match[tr_p] <- FALSE } }
                  }
                  if ( ! any(Lvec_match) ) {
                     vec_XXing_temp[tr_idx] <- paste(tr_ch,tr_lab_idx+1,sep="")
                  } else {
                     vec_XXing_temp[tr_idx] <- paste(tr_ch,tr_lab_idx,sep="")
                  }
               }
            }
            array_list_XXing[[m_idx,S_idx,b_idx]] <- vec_XXing_temp
         }
      }
   }

#-------------------------------------------------------------------------------------------------#
# Here we use all the data to fit a track and find the closest (theoretical) point to the buoy.
# Phase 1: Fit a track and find the closest (theoretical) point to the buoy.
# Phase 2: The closest point is used to bin each 1 Hz point.
#-------------------------------------------------------------------------------------------------#
   mat_list_tr_min_lon_lat <- matrix(list(),nrow=3,ncol=length(Bidx))
# Loop over buoys.
   for (b_idx in 1:length(Bidx)) {
# Assign variables from master lists.
      buoy_idx <- b_idx_list[Bidx[b_idx]]
      mat_list_Lvec_buoy_samp <- list_B_mat_list_Lvec_buoy_samp[[b_idx]]
      mat_list_breaks_master <- list_B_mat_list_breaks_master[[b_idx]]
      mat_list_buoy_data1 <- list_B_mat_list_buoy_data1[[b_idx]]
# Loop over missions.
      for (S_idx in Sidx) {
# Plotting.
         #X11(); plot(NULL,xlim=c(186,189.5),ylim=c(50,53))
         X11(); plot(NULL,xlim=c(round(df_buoy_data$buoy_lon[buoy_idx]+360)-2,round(df_buoy_data$buoy_lon[buoy_idx]+360)+2),ylim=c(round(df_buoy_data$buoy_lat[buoy_idx])-2,round(df_buoy_data$buoy_lat[buoy_idx])+2),main=vec_tandem_labs[S_idx])

         vec_unique_trackID <- unique(unlist(array_list_XXing[,S_idx,b_idx]))

         tr_min_lon_lat <- matrix(NA,nrow=length(vec_unique_trackID),ncol=2)
         rownames(tr_min_lon_lat) <- vec_unique_trackID
         tr_min_idx <- 0

         for (pp_idx in 1:length(vec_unique_trackID)) {
         #for (pp_idx in 2) {
            pp <- vec_unique_trackID[pp_idx]
            tracks_lon_temp <- NULL
            tracks_lat_temp <- NULL

            for (m_idx in 1:13) {
            #for (m_idx in 8) {
               tr_idx <- which( array_list_XXing[[m_idx,S_idx,b_idx]] == pp )
               if ( length(tr_idx) > 0 ) {
                  for (kk in 1:length(tr_idx)) {
                     tracks_lon_temp <- c(tracks_lon_temp,mat_list_buoy_data1[[m_idx,S_idx]][[3]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][[ which(array_list_XXing[[m_idx,S_idx,b_idx]] == pp)[kk] ]] ] )
                     tracks_lat_temp <- c(tracks_lat_temp,mat_list_buoy_data1[[m_idx,S_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][[ which(array_list_XXing[[m_idx,S_idx,b_idx]] == pp)[kk] ]] ] )
                  }
               }
            }
# Fit linear regression model to find "average track" with which to use for binning.
            df_AA <- data.frame(lon=tracks_lon_temp,lat=tracks_lat_temp)
            lm_lonlat <- lm(lon ~ lat + I(lat^2), data = df_AA)
# Use an idealised "high resolution", generated with the linear model, track to find the closest possible point (within resolution tolerance).
            vec_lat_seq <- seq( floor( min(tracks_lat_temp) * 10 ) / 10, ceiling( max(tracks_lat_temp) * 10 ) / 10,, 1001)
            vec_BB <- predict.lm(lm_lonlat,newdata=data.frame(lat=vec_lat_seq))
            tr_min_idx[pp_idx] <- which.min( apply(X=cbind(vec_lat_seq,vec_BB),MAR=1,FUN=func_buoy_dist,B_idx=buoy_idx) )
            tr_min_lon_lat[pp_idx,] <- cbind(vec_BB[tr_min_idx[pp_idx]],vec_lat_seq[tr_min_idx[pp_idx]])
            colnames(tr_min_lon_lat) <- c("lon","lat")

# Plotting.
            mat_AA <- as.matrix(df_AA)
            #vec_all_dist <- sapply(X=1:length(tracks_lat_temp),FUN=function(x) { func_sat_dist1(rev(mat_AA[x,]),rev(tr_min_lon_lat)) } )
# Plot the 1 Hz points.
            #points(tracks_lon_temp[vec_all_dist > 0],tracks_lat_temp[vec_all_dist > 0])
            #points(tracks_lon_temp[vec_all_dist < 0],tracks_lat_temp[vec_all_dist < 0])
            points(tracks_lon_temp,tracks_lat_temp)
# Plot the fitted tracks.
            lines(vec_BB,vec_lat_seq,col="orange",lwd=2)
# Plot the nearest point.
            points(tr_min_lon_lat,col="red",pch=19)
         }
         mat_list_tr_min_lon_lat[[S_idx,b_idx]] <- tr_min_lon_lat
# Plot buoy.
         points(df_buoy_data$buoy_lon[buoy_idx]+360,df_buoy_data$buoy_lat[buoy_idx],col="blue",pch=23,cex=2,bg="yellow")
      }
   }

#-------------------------------------------------------------------------------------------------#
# Phase 2: Bin all the 1 Hz points.
# To-do: Add mission loop.
#        Add buoy loop.
#-------------------------------------------------------------------------------------------------#
   mat_dist_bins <- matrix(c(seq(-147.5,147.5,5)[1:59],seq(-147.5,147.5,5)[2:60]),ncol=2)
# Loop over buoys.
   for (b_idx in 1:length(Bidx)) {
# Assign variables from lists.
      mat_list_Lvec_buoy_samp <- list_B_mat_list_Lvec_buoy_samp[[b_idx]]
      mat_list_breaks_master <- list_B_mat_list_breaks_master[[b_idx]]
      mat_list_buoy_data1 <- list_B_mat_list_buoy_data1[[b_idx]]
      mat_list_Xing <- list_B_mat_list_Xing[[b_idx]]
# Loop over missions.
      for (S_idx in Sidx) {
         vec_unique_trackID <- unique(unlist(array_list_XXing[,S_idx,b_idx]))

         for (m_idx in 1:13) {
# Set up list to store monthly data.
            list_bins_temp <- list()
            for (pp_idx in 1:length(vec_unique_trackID)) {
            #for (pp_idx in 1) {
               pp <- vec_unique_trackID[pp_idx]
               tr_idx <- which( array_list_XXing[[m_idx,S_idx,b_idx]] == pp )

               if ( length(tr_idx) > 0 ) {

                  for (kk in 1:length(tr_idx)) {
                     mat_AA <- cbind(
                                  mat_list_buoy_data1[[m_idx,S_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][[ tr_idx[kk] ]] ],
                                  mat_list_buoy_data1[[m_idx,S_idx]][[3]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][[ tr_idx[kk] ]] ] )
                     if ( mat_list_Xing[[m_idx,S_idx]][tr_idx[kk]] == "A" ) {
                        #vec_all_dist <- sapply(X=1:dim(mat_AA)[1],FUN=function(x) { func_sat_dist1A(mat_AA[x,],rev(tr_min_lon_lat[pp_idx,])) } )
                        vec_all_dist <- sapply(X=1:dim(mat_AA)[1],FUN=function(x) { func_sat_dist1A(mat_AA[x,],rev( mat_list_tr_min_lon_lat[[S_idx,b_idx]][pp_idx,] )) } )
                     } else {
                        #vec_all_dist <- sapply(X=1:dim(mat_AA)[1],FUN=function(x) { func_sat_dist1D(mat_AA[x,],rev(tr_min_lon_lat[pp_idx,])) } )
                        vec_all_dist <- sapply(X=1:dim(mat_AA)[1],FUN=function(x) { func_sat_dist1D(mat_AA[x,],rev( mat_list_tr_min_lon_lat[[S_idx,b_idx]][pp_idx,] )) } )
                     }
                     list_bins_temp[[ tr_idx[kk] ]] <- sapply(X=1:length(vec_all_dist),FUN=function(x) { which( sapply(X=1:59,FUN=function(y) { mat_dist_bins[y,1] < vec_all_dist[x] & mat_dist_bins[y,2] > vec_all_dist[x] }) ) })
                  }
               }
            }
            array_list_bins_master[[m_idx,S_idx,b_idx]] <- list_bins_temp
         }
      }
   }

#-------------------------------------------------------------------------------------------------#
# Phase 3: Arrange the data for correlation analysis, find physical scale for each track.
#          Here, we separate 1 Hz points into track IDs.
# To-do: Add buoy loop.
#-------------------------------------------------------------------------------------------------#
# Function for mode.
   func_mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
   }

# Find indices for track IDs (monthly).

   #array_list_trackID <- array(list(),dim=c(13,length(vec_unique_trackID),3))
   array_list_trackID <- matrix(list(),nrow=3,ncol=length(Bidx))
   #array_list_bins_trackID <- array(list(),dim=c(13,length(vec_unique_trackID),3))
   mat_list_bins_trackID <- matrix(list(),nrow=3,ncol=length(Bidx))

# Loop over buoys.
   for (b_idx in 1:length(Bidx)) {

   mat_list_breaks_master <- list_B_mat_list_breaks_master[[b_idx]]
   mat_list_Lvec_break_dist_min_idx <- list_B_mat_list_Lvec_break_dist_min_idx[[b_idx]]

   array_scale_m_idx <- array(NA,dim=c(2,length(vec_unique_trackID),3))

   list_mode <- list()
   list_mode_min <- list()
   vec_mode <- NA
   vec_mode_min <- NA

   for (S_idx in Sidx) {

      vec_unique_trackID <- unique(unlist(array_list_XXing[,S_idx,b_idx]))

      mat_list_trackID <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
      mat_list_bins_trackID_temp <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))

      for ( jj in 1:length(vec_unique_trackID) ) {
         vec_mode_temp <- NULL
         vec_mode_min_temp <- NULL
# Loop over months.
         for ( m_idx in 1:13 ) {
            mat_list_trackID[[m_idx,jj]] <- mat_list_breaks_master[[m_idx,S_idx]][which(array_list_XXing[[m_idx,S_idx,b_idx]] == vec_unique_trackID[jj])]
            #array_list_bins_trackID[[m_idx,jj,S_idx]] <- array_list_bins_master[[m_idx,S_idx,b_idx]][which(array_list_XXing[[m_idx,S_idx,b_idx]] == vec_unique_trackID[jj])]
            mat_list_bins_trackID_temp[[m_idx,jj]] <- array_list_bins_master[[m_idx,S_idx,b_idx]][which(array_list_XXing[[m_idx,S_idx,b_idx]] == vec_unique_trackID[jj])]

# Deprecated ??
# Find mode of trackID length.
            vec_mode_temp <- c(vec_mode_temp,sapply(X=mat_list_trackID[[m_idx,jj]],length))
# Find mode of closest points.
            vec_mode_min_temp <- c(vec_mode_min_temp,sapply(X=mat_list_Lvec_break_dist_min_idx[[m_idx,S_idx]][array_list_XXing[[m_idx,S_idx,b_idx]] == vec_unique_trackID[jj]],which))
         }
         vec_mode[jj] <- func_mode(unlist(vec_mode_temp))
         vec_mode_min[jj] <- func_mode(unlist(vec_mode_min_temp))
## Find the month index and
#         scale_idx <- which(vec_mode_temp == vec_mode[jj])[2]
#         iii <- 1; while( sum( sapply(X=1:iii,FUN=function(x) length( mat_list_trackID[[x,jj]] )) ) < scale_idx ) { iii <- iii + 1 }
#         array_scale_m_idx[,jj,S_idx] <- c(iii,scale_idx - sum( sapply(X=1:iii,FUN=function(x) length( mat_list_trackID[[x,jj]] )) ) + length( mat_list_trackID[[iii,jj]] ))
      }
      array_list_trackID[[S_idx,b_idx]] <- mat_list_trackID
      mat_list_bins_trackID[[S_idx,b_idx]] <- mat_list_bins_trackID_temp

      list_mode[[S_idx]] <- vec_mode
      list_mode_min[[S_idx]] <- vec_mode_min
   }
   }

# Find Hs values for each 1 Hz point in each track ID.
   mat_list_trackID_hs1 <- matrix(list(),nrow=3,ncol=length(Bidx))

# Loop over buoys.
   for (b_idx in 1:length(Bidx)) {
      mat_list_1Hz_hs <- list_B_mat_list_1Hz_hs[[b_idx]]

      for (S_idx in Sidx) {
         vec_unique_trackID <- unique(unlist(array_list_XXing[,S_idx,b_idx]))
         list_trackID_hs1 <- list()
# Assign from array (for convenience).
         mat_list_trackID <- as.matrix( array_list_trackID[[S_idx,b_idx]] )
         #vec_mode <- list_mode[[S_idx]]

# Loop over track IDs.
         for ( jj in 1:length(vec_unique_trackID) ) {
            mat_temp1 <- NULL
#print(paste("TrackID:",jj))
            for ( m_idx in 1:13 ) {
#print(paste("m_idx:",m_idx))
               mat_trackID_hs_temp <- matrix(NA,nrow=length(mat_list_trackID[[m_idx,jj]]),ncol=dim(mat_dist_bins)[1])

# Fix for missing tracks on some passes (is this a good solution?).
               if ( length(mat_list_trackID[[m_idx,jj]]) > 0 ) {

                  for ( ii in 1:length(mat_list_trackID[[m_idx,jj]]) ) {
                     #mat_trackID_hs_temp[ii, array_list_bins_trackID[[m_idx,jj,S_idx]][[ii]] ] <- mat_list_1Hz_hs[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]]]
                     mat_trackID_hs_temp[ii, mat_list_bins_trackID[[S_idx,b_idx]][[m_idx,jj]][[ii]] ] <- mat_list_1Hz_hs[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]]]
                     #array_list_trackID_hs[[m_idx,jj,S_idx]] <- mat_trackID_hs_temp
                  }
               }

               mat_temp1 <- rbind(mat_temp1,mat_trackID_hs_temp)
            }
            #mat_list_trackID_hs1[[jj,S_idx]] <- mat_temp1
            list_trackID_hs1[[jj]] <- mat_temp1
         }
         mat_list_trackID_hs1[[S_idx,b_idx]] <- list_trackID_hs1
      }
   }

#=================================================================================================#
# Temporal collocation.
# To-do:
# Buoy loop.
# Assign ERA5 output to matrix for S_idx and b_idx.
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
   mat_list_buoy_hs_coloc_AD <- matrix(list(),nrow=3,ncol=length(Bidx))
   mat_list_buoy_hs_coloc_AD_TEST <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
   mat_list_ERA5_hs_coloc_AD <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
   mat_list_ERA5_hs_BILIN_coloc_AD <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))

# Loop over buoys.
   for (b_idx in 1:length(Bidx)) {
      buoy_idx <- b_idx_list[Bidx[b_idx]]
      vec_buoy_time_num <- list_B_vec_buoy_time_num[[b_idx]]
      vec_unique_trackID <- unique(unlist(array_list_XXing[,,b_idx])) 
      mat_list_mean_time <- list_B_mat_list_mean_time[[b_idx]]
      vec_buoy_hs <- list_B_vec_buoy_hs[[b_idx]]
      vec_buoy_ap <- list_B_vec_buoy_ap[[b_idx]]

      for (S_idx in Sidx) {
         vec_unique_trackID <- unique(unlist(array_list_XXing[,S_idx,b_idx]))
         mat_list_buoy_hs_coloc_AD_temp <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))

         for ( m_idx in 1:13 ) {
            list_buoy_data1 <- list(mat_list_J3[[m_idx,buoy_idx]],mat_list_S6_LRM[[m_idx,buoy_idx]],mat_list_S6_SAR[[m_idx,buoy_idx]])
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
               vec_hs_temp <- rep(NA,sum(array_list_XXing[[m_idx,S_idx,b_idx]] == vec_unique_trackID[jj]))
               if ( length(vec_hs_temp) > 0 ) {
                  mat_hs_idx_temp <- which( Lmat_list_slot_J3B[[m_idx]][,array_list_XXing[[m_idx,S_idx,b_idx]] == vec_unique_trackID[jj]],arr.ind=T )
                  if ( length(mat_hs_idx_temp) == 1 ) {
                     mat_list_buoy_hs_coloc_AD_temp[[m_idx,jj]] <- vec_buoy_hs[mat_hs_idx_temp]
                  } else if ( length(mat_hs_idx_temp) == 0 ) {
                     mat_list_buoy_hs_coloc_AD_temp[[m_idx,jj]] <- vec_hs_temp
                  } else {
                     vec_hs_temp[mat_hs_idx_temp[,2]] <- vec_buoy_hs[mat_hs_idx_temp[,1]]
                     mat_list_buoy_hs_coloc_AD_temp[[m_idx,jj]] <- vec_hs_temp
                  }
               }

               #mat_list_buoy_hs_coloc_AD_TEST[[m_idx,jj]] <- vec_buoy_hs[which(Lmat_list_slot_J3B[[m_idx]],arr.ind=T)[,1]][array_list_XXing[[m_idx,S_idx,b_idx]] == vec_unique_trackID[jj]]
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
## Find buoy Hs and ap for all J3-ERA5 colocs, separated by trackID.
#            for ( jj in 1:length(vec_unique_trackID) ) {
#               mat_list_ERA5_hs_coloc_AD[[m_idx,jj]] <- vec_ERA5_hs[which(Lmat_list_slot_J3ERA[[m_idx]],arr.ind=T)[,1]][array_list_XXing[[m_idx,S_idx,b_idx]] == vec_unique_trackID[jj]]
#               mat_list_ERA5_hs_BILIN_coloc_AD[[m_idx,jj]] <- vec_ERA5_hs_BILIN[which(Lmat_list_slot_J3ERA[[m_idx]],arr.ind=T)[,1]][array_list_XXing[[m_idx,S_idx,b_idx]] == vec_unique_trackID[jj]]
#            }
         }
         mat_list_buoy_hs_coloc_AD[[S_idx,b_idx]] <- mat_list_buoy_hs_coloc_AD_temp
      }
   }

#=================================================================================================#
# Algorithm for calculating along track 1 Hz correlations.
   mat_list_cor <- matrix(list(),nrow=3,ncol=length(Bidx))
   mat_list_cor_95 <- matrix(list(),nrow=3,ncol=length(Bidx))
   mat_list_rmse <- matrix(list(),nrow=3,ncol=length(Bidx))
   mat_list_bias <- matrix(list(),nrow=3,ncol=length(Bidx))

# Loop over buoys.
   for (b_idx in 1:length(Bidx)) {
      for (S_idx in Sidx) {
# Select target for correlation (buoy, ERA, ERA5_bilin).
         plot_data <- mat_list_buoy_hs_coloc_AD[[S_idx,b_idx]]
         #plot_data <- mat_list_ERA5_hs_coloc_AD
         #plot_data <- mat_list_ERA5_hs_BILIN_coloc_AD

# Assign from array.
         mat_list_trackID <- as.matrix( array_list_trackID[[S_idx,b_idx]] )
         list_trackID_hs <- mat_list_trackID_hs1[[S_idx,b_idx]]
         vec_unique_trackID <- unique(unlist(array_list_XXing[,S_idx,b_idx]))

         list_vec_cor <- list()
         list_vec_cor_95 <- list()
         list_vec_rmse <- list()
         list_vec_bias <- list()
         list_1Hz_idx_dist <- list()
# Loop over unique tracks.
         for ( jj in 1:length(vec_unique_trackID) ) {
# Get track length.
            i_len_trackID <- dim(list_trackID_hs[[jj]])[2]
            list_vec_cor[[jj]] <- rep(NA,i_len_trackID)
            list_vec_rmse[[jj]] <- rep(NA,i_len_trackID)
            list_vec_bias[[jj]] <- rep(NA,i_len_trackID)
## Find inter-point distance in km using an appropriate track (corresponds to vec_mode).
## array_scale_m_idx contains the required indices.
#      sc_midx <- array_scale_m_idx[1,jj,S_idx]
#      sc_idxB <- array_scale_m_idx[2,jj,S_idx]
#      mat_trackID_latlon <- cbind(mat_list_1Hz_lat[[sc_midx,S_idx]][mat_list_trackID[[sc_midx,jj]][[sc_idxB]]],mat_list_1Hz_lon[[sc_midx,S_idx]][mat_list_trackID[[sc_midx,jj]][[sc_idxB]]])
#      colnames(mat_trackID_latlon) <- c("lat","lon")
#      vec_trackID_dist <- sapply(X=1:(i_len_trackID-1),FUN=function(x) { func_sat_dist(mat_trackID_latlon[x,],mat_trackID_latlon[x+1,]) })
#      list_1Hz_idx_dist[[jj]] <- c( sapply(X=1:(vec_mode_min[jj]-1),FUN=function(x) {-sum(vec_trackID_dist[x:(vec_mode_min[jj]-1)])}),
#                                    0,
#                                    sapply(X=(vec_mode_min[jj]):(i_len_trackID-1),FUN=function(x) {sum(vec_trackID_dist[vec_mode_min[jj]:x])}) )
# Test for sufficient number of points to compute correlation, rmse, etc.
            for ( ii in 1:i_len_trackID ) {
               if ( sum( !is.na( unlist(plot_data[,jj]) ) & !is.na( list_trackID_hs[[jj]][,ii] ) ) > 10 ) {
# Correlation.
                  list_vec_cor[[jj]][ii] <- cor( unlist(plot_data[,jj]),list_trackID_hs[[jj]][,ii],use="pairwise.complete.obs" )
# RMSE.
                  list_vec_rmse[[jj]][ii] <- sqrt( mean( (unlist(plot_data[,jj])-list_trackID_hs[[jj]][,ii])^2,na.rm=T ) )
# Bias.
                  mat_stat_data <- cbind(unlist(plot_data[,jj]),list_trackID_hs[[jj]][,ii])
                  Lvec_bias <- !apply(X=mat_stat_data,MAR=1,function(x) { any(is.na(x)) })
                  list_vec_bias[[jj]][ii] <- mean(mat_stat_data[Lvec_bias,1])-mean(mat_stat_data[Lvec_bias,2])
               }
            }
            list_vec_cor_95[[jj]] <- list_vec_cor[[jj]] > cor_thresh
         }
         mat_list_cor[[S_idx,b_idx]] <- list_vec_cor
         mat_list_cor_95[[S_idx,b_idx]] <- list_vec_cor_95
         mat_list_rmse[[S_idx,b_idx]] <- list_vec_rmse
         mat_list_bias[[S_idx,b_idx]] <- list_vec_bias
      }
   }

#-------------------------------------------------------------------------------------------------#
# Plotting.
## Plot median of super-observation.
#   X11(); plot(unlist(mat_list_buoy_hs_coloc_AD[,1]),apply(X=list_trackID_hs[[1]],MAR=1,FUN=median ),xlim=c(0,8),ylim=c(0,8)); abline(0,1)
## Plot by sub-point.
#   X11(); plot(unlist(mat_list_buoy_hs_coloc_AD[,1]),list_trackID_hs[[1]][,1],xlim=c(0,8),ylim=c(0,8)); abline(0,1)

   if ( flag_plot_cor) {

# Different figure for each satellite dataset.
   for (S_idx in Sidx) {
      plot_data <- mat_list_buoy_hs_coloc_AD[[S_idx,b_idx]]
      vec_unique_trackID <- unique(unlist(array_list_XXing[,S_idx,b_idx]))
      list_trackID_hs <- mat_list_trackID_hs1[[S_idx,b_idx]]

      system(paste("if [ ! -d ./figures/test_sampling2/",buoy_list[buoy_idx]," ]; then mkdir ./figures/test_sampling2/",buoy_list[buoy_idx]," &> /dev/null; fi",sep=""))
      fig_cor_file_name <- paste("./figures/test_sampling2/",buoy_list[buoy_idx],"/track_cor_",vec_tandem_labs[S_idx],"_",buoy_list[buoy_idx],"_",buoy_radius,"km_f06_2.pdf",sep="")
      pdf(fig_cor_file_name,width = (4.0 * length(vec_unique_trackID)), height = 8.4)
      par(mfrow=c(2,length(vec_unique_trackID)),mar=c(5,4,4,5),mgp=c(3.1,1,0))

      for ( jj in 1:length(vec_unique_trackID) ) {
         i_len_trackID <- dim(list_trackID_hs[[jj]])[2]
         title_top <- paste(vec_tandem_labs[S_idx]," [",buoy_radius," km] Track ID: ",vec_unique_trackID[[jj]],"\nBuoy: ",buoy_list[buoy_idx],sep="")
         #plot(1:i_len_trackID,list_vec_cor[[jj]],ylim=c(0.5,1.0),xlab="1 Hz ground distance (km)",ylab="Correlation",main=paste("Track ID:",vec_unique_trackID[[jj]],"\nBuoy:",buoy_list[buoy_idx]),axes=F)
         #plot(1:i_len_trackID,list_vec_cor[[jj]],ylim=c(0.5,1.0),xlab="1 Hz surface distance (km)",ylab="Correlation",main=title_top,axes=F,cex.lab=1.2)
         plot(1:i_len_trackID,mat_list_cor[[S_idx,b_idx]][[jj]],ylim=c(0.5,1.0),xlab="1 Hz surface distance (km)",ylab="Correlation",main=title_top,axes=F,cex.lab=1.2)
         axis(side=2,at=seq(0.5,1.0,0.1),labels=seq(0.5,1.0,0.1))
         #axis(side=1,at=1:i_len_trackID,labels=format(list_1Hz_idx_dist[[jj]],digits=2),las=2)
         abline(h=c(0.95,1.0),col="grey")
         abline(v=seq(5,40,5),col="grey")
         abline(v=vec_mode_min[jj],col="blue")
         #points((1:i_len_trackID)[vec_mode_min[jj]],list_vec_cor[[jj]][vec_mode_min[jj]],pch=19,col="blue")
         points((1:i_len_trackID)[vec_mode_min[jj]],mat_list_cor[[S_idx,b_idx]][[jj]][vec_mode_min[jj]],pch=19,col="blue")
         par(new=T)
         plot(1:i_len_trackID,sapply(X=1:i_len_trackID,FUN=function(x) { sum( !is.na( unlist(plot_data[,jj]) ) & !is.na( list_trackID_hs[[jj]][,x] ) ) }),pch=4,ylim=c(0,60),axes=F,xlab="",ylab="")
         axis(side=4,at=seq(0,40,5))
         mtext("Number of temporal samples", side=4, line=2, cex=0.8)
         if ( jj == 1) {
            legend(1,15,legend=c("Point closest to buoy","Correlation","Number of temporal samples"),pch=c(19,1,4),col=c("blue","black","black"))
         }
      }

      for ( jj in 1:length(vec_unique_trackID) ) {
         i_len_trackID <- dim(list_trackID_hs[[jj]])[2]
         title_top <- paste(vec_tandem_labs[S_idx]," [",buoy_radius," km] Track ID: ",vec_unique_trackID[[jj]],"\nBuoy: ",buoy_list[buoy_idx],sep="")
         plot(1:i_len_trackID,mat_list_rmse[[S_idx,b_idx]][[jj]],ylim=c(-0.3,0.7),xlab="1 Hz surface distance (km)",ylab="RMSE",main=title_top,axes=F,cex.lab=1.2)
         axis(side=2,at=seq(0.1,0.7,0.1),labels=seq(0.1,0.7,0.1))
         #axis(side=1,at=1:i_len_trackID,labels=format(list_1Hz_idx_dist[[jj]],digits=2),las=2)
         #abline(h=c(0.95,1.0),col="grey")
         abline(v=seq(5,40,5),col="grey")
         abline(v=vec_mode_min[jj],col="blue")
         points((1:i_len_trackID)[vec_mode_min[jj]],mat_list_rmse[[S_idx,b_idx]][[jj]][vec_mode_min[jj]],pch=19,col="blue")
         par(new=T)
# Plot BIAS.
         plot(1:i_len_trackID,mat_list_bias[[S_idx,b_idx]][[jj]],pch=4,ylim=c(-0.5,3),axes=F,xlab="",ylab="")
         abline(h=c(0),col="grey")
         axis(side=4,at=seq(-0.5,1,0.25))
         mtext("Mean bias", side=4, line=2, cex=0.8)
         if ( jj == 1) {
            legend(1,15,legend=c("Point closest to buoy","RMSE","Number of temporal samples"),pch=c(19,1,4),col=c("blue","black","black"))
         }
## Histogram of collocation time differences.
#         if ( jj == 1) {
#            hist(unlist( mat_list_time_diff_J3B )/60,breaks=30,xlim=c(0,30),main=paste("Distribution of collocation\ntime differences. Total:",sum( !is.na( unlist( mat_list_time_diff_J3B ) ) )),xlab="Time difference (minutes)")
#         }
      }

      dev.off()
      #system(paste("okular",fig_rmse_file_name,"&> /dev/null &"))
      system(paste("okular",fig_cor_file_name,"&> /dev/null &"))

      }
   }
#-------------------------------------------------------------------------------------------------#
# Multi-buoy plotting.
   if ( flag_plot_multi_cor ) {

      fig_multi_cor_file_name <- paste("./figures/test_sampling2/multi_cor_",fig_lab_region,"_",vec_tandem_labs[S_idx],"_",buoy_radius,"km_S6ADJ.pdf",sep="")
      pdf(fig_multi_cor_file_name,width = 10,height = 12)
      par(mfrow=c(5,4),mar=c(5,4,4,5),mgp=c(3.1,1,0))
   
      #for (b_idx in 1:length(Bidx)) {
      for (b_idx in 1:5) {
         buoy_idx <- b_idx_list[Bidx[b_idx]]
         #for (S_idx in Sidx) {
         for (S_idx in 1) {
            list_trackID_hs <- mat_list_trackID_hs1[[S_idx,b_idx]]
            vec_unique_trackID <- unique(unlist(array_list_XXing[,S_idx,b_idx]))

            for (pp_idx in 1:length(mat_list_cor[[S_idx,b_idx]])) {
               title_top <- paste(vec_tandem_labs[S_idx]," [",buoy_radius," km] Track ID: ",vec_unique_trackID[[pp_idx]],"\nBuoy: ",buoy_list[buoy_idx],sep="")
               i_len_trackID <- dim(list_trackID_hs[[pp_idx]])[2]
               plot( 1:i_len_trackID,mat_list_cor[[S_idx,b_idx]][[pp_idx]], ylim=c(0.8,1), xlab="1 Hz surface distance (km)", ylab="Correlation", main=title_top, axes=F )
               if ( length(mat_list_cor[[3,b_idx]]) >= pp_idx ) {
                  points( 1:i_len_trackID,mat_list_cor[[3,b_idx]][[pp_idx]], pch=4, col="red" )
	       }

               abline(h=c(0.95,1.0),col="grey")
               #axis(side=2,at=seq(0.8,1.0,0.05),labels=seq(0.5,1.0,0.1))
               axis(side=2,at=seq(0.8,1.0,0.05))
               axis(side=1,at=(1:i_len_trackID)[seq(10,50,5)],labels=seq(-145,145,5)[seq(10,50,5)],las=2)
            }
            if ( pp_idx < 4 ) { plot(NULL,xlim=c(0,1),ylim=c(1,0),xlab="",ylab="",axes=F); pp_idx <- pp_idx + 1 }
            if ( pp_idx < 4 ) { plot(NULL,xlim=c(0,1),ylim=c(1,0),xlab="",ylab="",axes=F) }
         }
      }
      dev.off()
      system(paste("okular",fig_multi_cor_file_name,"&> /dev/null &"))
   }

#=================================================================================================#
# Calculate median values based on "adaptive" sampling.

   #mat_list_trackID_hs_med <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
   #mat_list_trackID_hs_med_adapt <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
   #mat_list_trackID_hs_med_rand <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
   #mat_list_trackID_hs_min <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
   mat_list_trackID_hs_med <- matrix(list(),nrow=3,ncol=length(Bidx))
   mat_list_trackID_hs_med_adapt <- matrix(list(),nrow=3,ncol=length(Bidx))
   #list_trackID_hs_med_rand <- list()
   #list_trackID_hs_min <- list()

   S_idx <- 1
   for (b_idx in 1:2) {

      list_trackID_hs <- mat_list_trackID_hs1[[S_idx,b_idx]]
      list_vec_cor_95 <- mat_list_cor_95[[S_idx,b_idx]]
      vec_unique_trackID <- unique(unlist(array_list_XXing[,S_idx,b_idx]))

      list_trackID_hs_med <- vector(mode = "list",length = length(vec_unique_trackID))
      list_trackID_hs_med_adapt <- vector(mode = "list",length = length(vec_unique_trackID))

      for ( jj in 1:length(vec_unique_trackID) ) {
         i_len_trackID <- dim(list_trackID_hs[[jj]])[2]
# Don't need the monthly loop...
         #for ( m_idx in 1:13 ) {
            if ( !is.null(list_trackID_hs[[jj]]) ) {
               list_trackID_hs_med[[jj]] <- sapply(X=1:dim(list_trackID_hs[[jj]])[1],FUN=function(x) { median(list_trackID_hs[[jj]][x,],na.rm=T) })
               list_trackID_hs_med_adapt[[jj]] <- sapply(X=1:dim(list_trackID_hs[[jj]])[1],FUN=function(x) { median(list_trackID_hs[[jj]][x,list_vec_cor_95[[jj]]],na.rm=T) })
               #mat_list_trackID_hs_med_rand[[m_idx,jj]] <- sapply(X=1:dim(mat_list_trackID_hs[[m_idx,jj]])[1],FUN=function(x) { median(mat_list_trackID_hs[[m_idx,jj]][x,runif(i_len_trackID) > 0.1],na.rm=T) })
               #mat_list_trackID_hs_min[[m_idx,jj]] <- sapply(X=1:dim(mat_list_trackID_hs[[m_idx,jj]])[1],FUN=function(x) { mat_list_trackID_hs[[m_idx,jj]][x,vec_mode_min[[jj]]] })
            }
#         if ( !is.null(mat_list_trackID_hs[[m_idx,jj]]) ) {
#            mat_list_trackID_hs_med[[m_idx,jj]] <- sapply(X=1:dim(mat_list_trackID_hs[[m_idx,jj]])[1],FUN=function(x) { median(mat_list_trackID_hs[[m_idx,jj]][x,],na.rm=T) })
#            mat_list_trackID_hs_med_adapt[[m_idx,jj]] <- sapply(X=1:dim(mat_list_trackID_hs[[m_idx,jj]])[1],FUN=function(x) { median(mat_list_trackID_hs[[m_idx,jj]][x,list_vec_cor_95[[jj]]],na.rm=T) })
#            mat_list_trackID_hs_med_rand[[m_idx,jj]] <- sapply(X=1:dim(mat_list_trackID_hs[[m_idx,jj]])[1],FUN=function(x) { median(mat_list_trackID_hs[[m_idx,jj]][x,runif(i_len_trackID) > 0.1],na.rm=T) })
#            mat_list_trackID_hs_min[[m_idx,jj]] <- sapply(X=1:dim(mat_list_trackID_hs[[m_idx,jj]])[1],FUN=function(x) { mat_list_trackID_hs[[m_idx,jj]][x,vec_mode_min[[jj]]] })
#         }
#      }
      }
      mat_list_trackID_hs_med[[S_idx,b_idx]] <- list_trackID_hs_med
      mat_list_trackID_hs_med_adapt[[S_idx,b_idx]] <- list_trackID_hs_med_adapt
   }
   print(paste(" SQRT ERROR VAR [MED - ADAPT] **",sqrt( var( unlist(list_trackID_hs_med)-unlist(list_trackID_hs_med_adapt), na.rm=T ) ) ) )
   #print(paste(" SQRT ERROR VAR [MED - MIN]   **",sqrt( var( unlist(list_trackID_hs_med)-unlist(list_trackID_hs_min), na.rm=T ) ) ) )

# Refactor the buoy data for trackID (to match the sat data).
   mat_list_buoy_hs_coloc_trackID <- matrix(list(),nrow=3,ncol=length(Bidx))
   for (b_idx in 1:length(Bidx)) { BB <- mat_list_buoy_hs_coloc_AD[[1,b_idx]]; list_BB <- list(); for ( mm in 1:dim(BB)[2] ) { list_BB[[mm]] <- unlist(BB[,mm]) }; mat_list_buoy_hs_coloc_trackID[[1,b_idx]] <- list_BB }

# Plotting.
# Correlation and regression.
   #df_reg <- data.frame(buoy_hs=unlist(mat_list_buoy_hs_coloc_AD[1,][,1]),sat_hs=unlist(mat_list_trackID_hs_med[[1,1]][[1]]))
   df_reg <- data.frame(
                        buoy_hs=unlist(sapply(X=1:2,FUN=function(x) { unlist(mat_list_buoy_hs_coloc_trackID[[1,x]]) })),
                        sat_hs=unlist(sapply(X=1:2,FUN=function(x) { unlist(mat_list_trackID_hs_med_adapt[[1,x]]) })) )
   lm_hs <- lm(sat_hs ~ buoy_hs,data=df_reg)
# Correlation.
   hs_cor <- cor(df_reg$buoy_hs,df_reg$sat_hs,use="pairwise.complete.obs")
# Bias.
   hs_bias <- mean(df_reg$sat_hs,na.rm=T) - mean(df_reg$buoy_hs,na.rm=T)
# RMSE
   hs_rmse <- sqrt(mean(lm_hs$residuals^2))

   #png(fig_file_name_tm2, width = 3000, height = 3000)
   #par(mfrow=c(2,2),oma=c(8,8,8,9),mar=c(12,14,9,9),mgp=c(9,4,0))
   X11(); plot(df_reg$buoy_hs,df_reg$sat_hs); abline(0,1)
   mtext(text=paste("Correlation: ",format(hs_cor,digits=2),sep=''), side=3, line=-6, adj=0.03, cex=1, outer=FALSE)
   mtext(text=paste("Bias: ",format(hs_bias,digits=2),sep=''), side=3, line=-12, adj=0.03, cex=1, outer=FALSE)
   mtext(text=paste("RMSE: ",format(round(hs_rmse,2),nsmall=2),sep=''), side=3, line=-18, adj=0.03, cex=1, outer=FALSE)

   #X11(); plot(unlist(list_trackID_hs_med),unlist(list_trackID_hs_med_adapt)); abline(0,1)
   #X11(); plot(unlist(mat_list_trackID_hs_med[1,][[1]]),unlist(mat_list_trackID_hs_med_adapt[1,][[1]])); abline(0,1)


