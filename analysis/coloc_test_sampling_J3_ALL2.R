# source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/coloc_test_sampling_J3_ALL2.R")
   set.seed(23458892)
   require(lmodel2)
   #require(akima)

# ================================================================= #
# Load buoys.
   source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/load_buoy_locs.R")
# ================================================================= #
# Edit here
# ----------------------------------------------------------------- #
# Plotting and analysis.
   flag_plot_track_stats <- FALSE
   flag_plot_multi_cor <- FALSE

   flag_scatter_plot <- FALSE
   flag_single_panel <- FALSE
   flag_radius_plot <- FALSE

   flag_buoy_bias_hist <- FALSE

   flag_plot_ggmap <- FALSE
# Correlation (COR) or mean bias (BIAS).
   flag_plot_ggmap_COR <- FALSE

# Sea state sampling by wave period.
   flag_period_thresh <- FALSE
   flag_swell_only <- FALSE
   period_thresh <- 8

# Offshore of neashore.
   flag_OS <- TRUE

# Data and sampling.
   if ( flag_OS ) {
      #str_region <- "PAC_OS"
      #buoy_list <- c(buoy_list_PAC_OS)

      buoy_radius <- 75
      cor_thresh <- 0.98
# Omit 46246.
# 46066(1) doesn't work for tandem phase.
# 46246(3) strong bias everywhere.
# 46085(4) bias gradient with latitude.
# 46001(5) bias gradient with latitude? (Similar to 46085).
      Bidx_init <- c(1,2,4:9)
      Bidx_init <- 2:9
   } else {
      #str_region <- "PAC_NS"
      #buoy_list <- c(buoy_list_PAC_NS)

      buoy_radius <- 75
      cor_thresh <- 0.98

      Bidx_init <- c(1:6,15:18,21:29,31,32,34)
      Bidx_init <- 1
   }

# Bin width for along-track sampling (5 km).
   dist_bin_width <- 10

   flag_ERA5_BILIN <- FALSE

# OS.
   #Bidx_init <- c(1,2,4:9)
# NS (75 km).
# 46085 (no data 202012, Env. Canada)
# Env Canada buoys all knackered from CMEMS.
# 46248 (19: no sampling, 80 km)
# 46029 (20: no sampling, 75 km)
# 46022 (30: lack of sampling, 80 km, UKNOWN)
# 46013 (33: lack of sampling, 80 km, UKNOWN)
   #Bidx_init <- c(1:6,8:10,12:16,18:27,29:34)
   #Bidx_init <- c(1:6,15:18,21:29,31,32,34)

   Sidx <- 1:2
   flag_tandem <- TRUE
   if ( flag_tandem ) {
      m_limit <- 13
   } else {
      m_limit <- 60
   }

   flag_S6SAR_correction <- FALSE
   if ( flag_S6SAR_correction ) {
      path_S6SAR_correction_model <- "/home/ben/research/NOC/projects/s6-j3_tandem/analysis/S6_correction/lm_J3mS6SAR_46066_46078.Robj"
      load(path_S6SAR_correction_model)
   }
   #lm_J3mS6SAR

   vec_tandem_labs <- c("J3","S6LRM","S6SAR")

   lab_month <- c("Dec (2020)","Jan (2021)","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

   if ( flag_OS ) {
#-------------------------------------------------------------------------------------------------#
# PAC Offshore (active).
      fig_lab_region <- "OS"
      b_idx_list <- 5:13
#-------------------------------------------------------------------------------------------------#
# as.POSIXct(S6_46246_march[[4]][1:100], origin = '2000-01-01', tz='GMT')
# Attach J3.
      if ( flag_tandem ) {
         attach("./output/buoys_J3/list_buoy_data_swh_ocean_PAC_OS.Robj")
      } else {
         attach("./output/buoys_J3/list_buoy_data_swh_ocean_PAC_OS_2017-2022.Robj")
      }
      mat_list_J3 <- list_buoy_data[[2]]
      detach()

# Attach S6 LRM.
      attach("./output/buoys_S6/list_buoy_data_LRM_swh_ocean_PAC_OS_F06.Robj")
      mat_list_S6_LRM <- list_buoy_data[[2]]
      detach()

# Attach S6 SAR.
      #attach("./output/buoys_S6/archive/list_buoy_data_SAR_swh_ocean_PAC_OS.Robj")
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
      b_idx_list <- 1:52
#-------------------------------------------------------------------------------------------------#
# Attach J3.
      if ( flag_tandem ) {
         attach("./output/buoys_J3/list_buoy_data_swh_ocean_PAC_NS.Robj")
      } else {
         attach("./output/buoys_J3/list_buoy_data_swh_ocean_PAC_NS_2017-2022.Robj")
      }
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
      attach("./output/ERA5/buoy_array_PAC_NS_2017-2022.Robj")
      mat_list_ERA5 <- list_buoy_data[[2]]
      detach()
   }

#=================================================================================================#
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
      sign(x[2]-y[2]) * 2 * i_radius * asin(sqrt(fl_h))
   }
# [D]escending track.
   func_sat_dist1D <- function(x,y) {
      fl_d_lat = func_rads(x[1]) - func_rads(y[1])
      fl_d_lon = func_rads(x[2]) - func_rads(y[2])
      fl_h = sin(fl_d_lat / 2) * sin(fl_d_lat / 2) + cos( func_rads(y[1]) ) * cos( func_rads(x[1]) ) * sin(fl_d_lon / 2) * sin(fl_d_lon / 2)
      sign(y[1]-x[1]) * 2 * i_radius * asin(sqrt(fl_h))
   }

#=================================================================================================#
# Loop over sampling radius.
# Array to store plot statistics.
# Dims: 1 = stat
#       2 = sampling method
#       3 = radius

   array_radius_stats <- array(NA,dim=c(7,4,10))
   vec_buoy_radius <- c(100,75,60,50,45,40,35,30,25)
   vec_buoy_radius <- c(100)
   list_df_reg_radius <- matrix(list(),nrow=length(vec_buoy_radius),ncol=3)

   for ( i_buoy_radius in 1:length(vec_buoy_radius) ) {
      buoy_radius <- vec_buoy_radius[i_buoy_radius]
#=================================================================================================#
# Define "master" dataset.
   mat_list_buoy_data <- list(mat_list_J3,mat_list_S6_LRM,mat_list_S6_SAR)
# Check for valid satellite data at buoy site(s) and update list (if required).
   Bidx_remove <- NULL
   for (b_idx in 1:length(Bidx_init)) {
      buoy_idx <- b_idx_list[Bidx_init[b_idx]]
      mat_list_buoy_data1 <- mat_list_buoy_data[[1]][,buoy_idx]
# Find distance from buoy for all points.
      vec_1Hz_dist <- apply(X=cbind(mat_list_buoy_data1[[1]][[2]],mat_list_buoy_data1[[1]][[3]]-360),MAR=1,FUN=func_buoy_dist,B_idx=buoy_idx)
# Check for valid data to sample.
      if ( sum(vec_1Hz_dist < buoy_radius) < 10 ) {
         Bidx_remove <- c(Bidx_remove,b_idx)
         print(paste("Buoy ID:",buoy_idx,"[",buoy_list[buoy_idx],"] No sampling data. Excluded."))
      }
   }
   if ( !is.null(Bidx_remove) ) {
      print(paste("New Bidx list:",paste(Bidx_init[-Bidx_remove],collapse=" ")))
      Bidx <- Bidx_init[-Bidx_remove]
   } else {
      print(paste("Bidx list:",paste(Bidx_init,collapse=" ")))
      Bidx <- Bidx_init
   }
   if ( i_buoy_radius == 1 ) {
      Bidx_max <- Bidx
   }

#=================================================================================================#
# Master lists for each buoy.
   list_B_mat_list_1Hz_dist <- list()
   list_B_mat_list_Lvec_buoy_samp <- list()
   #list_B_mat_list_buoy_data1 <- list()
   list_B_mat_list_breaks_master <- list()
   list_B_mat_list_Xing <- list()
   list_B_mat_list_1Hz_hs <- list()
   list_B_mat_list_1Hz_rms <- list()
   list_B_mat_list_Lvec_break_dist_min_idx <- list()
   list_B_mat_list_mean_time <- list()

   array_list_XXing <- array(list(),dim=c(60,3,length(Bidx)))
   array_list_bins_master <- array(list(),dim=c(60,3,length(Bidx)))
   list_B_vec_buoy_time_num <- list()
   list_B_vec_buoy_hs <- list()
   #list_B_vec_buoy_ap <- list()

   vec_B_active <- rep(TRUE,length(Bidx))

#=================================================================================================#
   for (b_idx in 1:length(Bidx)) {

      ERA5_b_idx <- buoy_idx <- b_idx_list[Bidx[b_idx]]

#=================================================================================================#
# Load buoy data.
# Buoy time offset: 946684800
#-------------------------------------------------------------------------------------------------#
      buoy_data_file <- list.files(path = "/home/ben/research/waves/buoy_data/NDBC_complete_records/", pattern = paste0("^",buoy_list[buoy_idx],".*hs.csv") )
      #buoy_data_file <- "46005_1976-2021_hs.csv"
      mat_buoy_csv1 <- read.csv(paste0("/home/ben/research/waves/buoy_data/NDBC_complete_records/",buoy_data_file))
      mat_buoy_csv <- mat_buoy_csv1[!is.na(mat_buoy_csv1$hs),]
# QC for Environment Canada buoys.
      if ("hs_qc" %in% colnames(mat_buoy_csv)) {
         mat_buoy_csv <- mat_buoy_csv[mat_buoy_csv$hs_qc == 1,]
      }
      vec_buoy_time <- strptime(as.character(mat_buoy_csv[,1]),format="%Y-%m-%d %H:%M:%S",tz="GMT")
# Get indices for 2020/12 - 2021/12
      if ( flag_tandem ) {
         date_start_idx <- c( which( format( as.POSIXct( vec_buoy_time, tz='GMT'), "%Y%m") %in% "202012" ), which( format( as.POSIXct( vec_buoy_time, tz='GMT'), "%Y") %in% "2021" ) )[1]
      } else {
        date_start_idx <- which( format( as.POSIXct( vec_buoy_time, tz='GMT'), "%Y") %in% as.character(2017:2021) )[1]
      }

      print(paste("Buoy ID:",buoy_idx,"[",buoy_list[buoy_idx],"] date_start_idx:",date_start_idx))
#      if ( !is.na(date_start_idx) ) {
      date_idx <- date_start_idx:length(vec_buoy_time)

      list_B_vec_buoy_time_num[[b_idx]] <- vec_buoy_time_num <- as.numeric( vec_buoy_time[date_idx] ) - 946684800
      list_B_vec_buoy_hs[[b_idx]] <- vec_buoy_hs <- mat_buoy_csv$hs[date_idx]
      #list_B_vec_buoy_ap[[b_idx]] <- vec_buoy_ap <- mat_buoy_csv$ap[date_idx]

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
      mat_list_1Hz_dist <- matrix(list(),nrow=60,ncol=3)

      mat_list_Lvec_buoy_samp <- matrix(list(),nrow=60,ncol=3)

      mat_list_breaks_master <- matrix(list(),nrow=60,ncol=3)

      mat_list_mean_time <- matrix(list(),nrow=60,ncol=3)

      mat_list_Xing <- matrix(list(),nrow=60,ncol=3)
      #array_list_Xing <- array(list(),dim=c(13,3,length(b_idx_list)))

      #list_Xing_lat1 <- vector(mode = "list",length=3)
      #mat_list_XXing <- matrix(list(),nrow=60,ncol=3)
      mat_list_Lvec_break_dist_min_idx <- matrix(list(),nrow=60,ncol=3)
   
      mat_list_1Hz_time <- matrix(list(),nrow=60,ncol=3)
      mat_list_1Hz_lat <- matrix(list(),nrow=60,ncol=3)
      mat_list_1Hz_lon <- matrix(list(),nrow=60,ncol=3)
      mat_list_1Hz_hs <- matrix(list(),nrow=60,ncol=3)
      mat_list_Lvec_QC <- matrix(list(),nrow=60,ncol=3)
   
      mat_list_median_hs <- matrix(list(),nrow=60,ncol=3)
      mat_list_median_hs_min <- matrix(list(),nrow=60,ncol=3)
      mat_list_1Hz_qual <- matrix(list(),nrow=60,ncol=3)
      mat_list_1Hz_numval <- matrix(list(),nrow=60,ncol=3)
      mat_list_1Hz_rms <- matrix(list(),nrow=60,ncol=3)
      mat_list_1Hz_rms_QC <- matrix(list(),nrow=60,ncol=3)
      mat_list_median_hs_qual <- matrix(list(),nrow=60,ncol=3)
      mat_list_median_hs_numval <- matrix(list(),nrow=60,ncol=3)
      mat_list_median_hs_rms <- matrix(list(),nrow=60,ncol=3)

#-------------------------------------------------------------------------------------------------#
# Loop 1: First loop over months to identify track segments.
#-------------------------------------------------------------------------------------------------#
      #mat_list_buoy_data1 <- list(mat_list_J3[,buoy_idx],mat_list_S6_LRM[,buoy_idx],mat_list_S6_SAR[,buoy_idx])[[Sidx]]
      for ( m_idx in 1:m_limit ) {
# Monthly tandem data at buoy.
         #list_buoy_data1 <- mat_list_J3[[m_idx,buoy_idx]]

#-------------------------------------------------------------------------------------------------#
# Loop over missions.
         #for (S_idx in 1:1) {
         for (S_idx in Sidx) {
            mat_list_buoy_data1 <- mat_list_buoy_data[[S_idx]][,b_idx_list[Bidx[b_idx]]]

# Find distance from buoy for all points.
            mat_list_1Hz_dist[[m_idx,S_idx]] <- apply(X=cbind(mat_list_buoy_data1[[m_idx]][[2]],mat_list_buoy_data1[[m_idx]][[3]]-360),MAR=1,FUN=func_buoy_dist,B_idx=buoy_idx)
# Find points within sampling radius (no QC).
            mat_list_Lvec_buoy_samp[[m_idx,S_idx]] <- mat_list_1Hz_dist[[m_idx,S_idx]] < buoy_radius
# Fix for out-of-orbit early S6 LRM data.
            #if ( m_idx == 1 & S_idx == 2) { mat_list_Lvec_buoy_samp[[m_idx,S_idx]][1:141] <- FALSE }
# Extract time stamp for sampled points.
            mat_list_1Hz_time[[m_idx,S_idx]] <- mat_list_buoy_data1[[m_idx]][[4]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
# Extract lat and lon for sampled points.
            mat_list_1Hz_lat[[m_idx,S_idx]] <- mat_list_buoy_data1[[m_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
            mat_list_1Hz_lon[[m_idx,S_idx]] <- mat_list_buoy_data1[[m_idx]][[3]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
# Extract 1 Hz Hs QC information for sampled points.
            mat_list_1Hz_qual[[m_idx,S_idx]]       <- mat_list_buoy_data1[[m_idx]][[7]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
            mat_list_1Hz_numval[[m_idx,S_idx]]     <- mat_list_buoy_data1[[m_idx]][[8]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
            mat_list_1Hz_rms[[m_idx,S_idx]]        <- vec_rms_QC <- mat_list_buoy_data1[[m_idx]][[9]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
# Extract 1 Hz Hs.
            vec_hs <- mat_list_buoy_data1[[m_idx]][[5]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
# Apply linear model correction to S6SAR.
            if ( S_idx == 3 & flag_S6SAR_correction ) {
               vec_hs[ vec_hs < 0.2 ] <- NA
               vec_hs_QC <- predict.lm(object=lm_J3mS6SAR,newdata=data.frame(S6SAR=vec_hs))
            } else {
               vec_hs_QC <- vec_hs
            }
# Apply quality controls Hs.
            mat_list_Lvec_QC[[m_idx,S_idx]] <- ! ( mat_list_1Hz_numval[[m_idx,S_idx]] < 16 | mat_list_1Hz_rms[[m_idx,S_idx]] > 1.0 )
            vec_hs_QC[ ! mat_list_Lvec_QC[[m_idx,S_idx]] ] <- NA
# Assign QC Hs to mat_list_1Hz_hs.
            mat_list_1Hz_hs[[m_idx,S_idx]] <- vec_hs_QC
# Apply quality controls rms.
            vec_rms_QC[ ! mat_list_Lvec_QC[[m_idx,S_idx]] ] <- NA
# Assign QC rms to mat_list_1Hz_rms_QC.
            mat_list_1Hz_rms_QC[[m_idx,S_idx]] <- vec_rms_QC

# Find time stamps for sampled points.
            nc1_time_idx_cell <- mat_list_buoy_data1[[m_idx]][[4]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]

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
# Test for sufficient number of sampled points.
            if ( any( sapply(X=1:length(list_nc1_breaks_temp),function(x) length(list_nc1_breaks_temp[[x]])) > 3 ) ) {

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
               mat_list_Xing[[m_idx,S_idx]] <- sapply( X=1:length(list_nc1_breaks), FUN=function(x) { vec_X_lat <- mat_list_buoy_data1[[m_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][ list_nc1_breaks[[x]] ]; CC <- NA; kk <- 2; while( is.na(CC) ) { CC <- vec_X_lat[kk]-vec_X_lat[1] ; kk <- kk + 1 }; if ( CC > 0 ) { "A" } else { "D" } } )
            } else {
# Set a flag when useful data is present.
               print(paste("S_idx:",S_idx,"m_idx:",m_idx,"vec_B_active: FALSE"))
               vec_B_active[b_idx] <- FALSE
            }
         }
      }
# Capture data for multiple buoys.
      #list_B_mat_list_buoy_data1[[b_idx]] <- mat_list_buoy_data1
      list_B_mat_list_1Hz_dist[[b_idx]] <- mat_list_1Hz_dist
      list_B_mat_list_Lvec_buoy_samp[[b_idx]] <- mat_list_Lvec_buoy_samp
      list_B_mat_list_breaks_master[[b_idx]] <- mat_list_breaks_master
      list_B_mat_list_Xing[[b_idx]] <- mat_list_Xing
      list_B_mat_list_1Hz_hs[[b_idx]] <- mat_list_1Hz_hs
      list_B_mat_list_1Hz_rms[[b_idx]] <- mat_list_1Hz_rms
      list_B_mat_list_Lvec_break_dist_min_idx[[b_idx]] <- mat_list_Lvec_break_dist_min_idx
      list_B_mat_list_mean_time[[b_idx]] <- mat_list_mean_time
   }

#-------------------------------------------------------------------------------------------------#
# Loop 2: Second loop over months to identify different [A]scending or [D]escending tracks.
# Here we take each track segment and match individuals points (defined by lon,lat) to other tracks.
#-------------------------------------------------------------------------------------------------#
# Loop over (active) buoys.
   for (b_idx in (1:length(Bidx))[vec_B_active]) {
# Assign variables from lists.
      mat_list_Lvec_buoy_samp <- list_B_mat_list_Lvec_buoy_samp[[b_idx]]
      mat_list_Xing <- list_B_mat_list_Xing[[b_idx]]
      mat_list_breaks_master <- list_B_mat_list_breaks_master[[b_idx]]
# Loop over missions.
      for (S_idx in Sidx) {
         mat_list_buoy_data1 <- mat_list_buoy_data[[S_idx]][,b_idx_list[Bidx[b_idx]]]
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
                                      mat_list_buoy_data1[[m_idx]][[3]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][[tr_idx]]],
                                      mat_list_buoy_data1[[m_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][[tr_idx]]] )
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
                                         mat_list_buoy_data1[[mm_idx]][[3]][mat_list_Lvec_buoy_samp[[mm_idx,S_idx]]][vec_tr_test[tr_p]],
                                         mat_list_buoy_data1[[mm_idx]][[2]][mat_list_Lvec_buoy_samp[[mm_idx,S_idx]]][vec_tr_test[tr_p]] )
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
         for (m_idx in (1:m_limit)[-mm_idx]) {
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
                                      mat_list_buoy_data1[[m_idx]][[3]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][[tr_idx]]],
                                      mat_list_buoy_data1[[m_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][[tr_idx]]] )
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
                                         mat_list_buoy_data1[[mm_idx]][[3]][mat_list_Lvec_buoy_samp[[mm_idx,S_idx]]][vec_tr_test[tr_p]],
                                         mat_list_buoy_data1[[mm_idx]][[2]][mat_list_Lvec_buoy_samp[[mm_idx,S_idx]]][vec_tr_test[tr_p]] )
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
   #vec_dist_bins_centre <- seq(-145,145,5)
   vec_dist_bins_centre <- seq(-floor(149 / dist_bin_width)*dist_bin_width,floor(149 / dist_bin_width)*dist_bin_width,dist_bin_width)
   mat_list_tr_min_lon_lat <- matrix(list(),nrow=3,ncol=length(Bidx))
   mat_list_bin_coords <- matrix(list(),nrow=3,ncol=length(Bidx))
   list_B_list_tr_lm_seq <- list()
# Loop over (active) buoys.
   for (b_idx in (1:length(Bidx))[vec_B_active]) {
# Assign variables from master lists.
      buoy_idx <- b_idx_list[Bidx[b_idx]]
      mat_list_Lvec_buoy_samp <- list_B_mat_list_Lvec_buoy_samp[[b_idx]]
      mat_list_breaks_master <- list_B_mat_list_breaks_master[[b_idx]]
      #mat_list_buoy_data1 <- list_B_mat_list_buoy_data1[[b_idx]]
# Loop over missions.
      for (S_idx in Sidx) {
         mat_list_buoy_data1 <- mat_list_buoy_data[[S_idx]][,b_idx_list[Bidx[b_idx]]]
#-------------------------------------------------------------------------------------------------#
## Plotting.
#         X11(); plot(NULL,xlim=c(round(df_buoy_data$buoy_lon[buoy_idx]+360)-2,round(df_buoy_data$buoy_lon[buoy_idx]+360)+2),ylim=c(round(df_buoy_data$buoy_lat[buoy_idx])-2,round(df_buoy_data$buoy_lat[buoy_idx])+2),main=vec_tandem_labs[S_idx])
#-------------------------------------------------------------------------------------------------#

         vec_unique_trackID <- unique(unlist(array_list_XXing[,S_idx,b_idx]))
# List to capture "high res" predicted track lon / lat.
         list_tr_lonlat_pred <- vector(mode = "list",length = length(vec_unique_trackID))
# Matrix for nearest point to buoy (per track).
         mat_tr_min_lon_lat <- matrix(NA,nrow=length(vec_unique_trackID),ncol=3)
         rownames(mat_tr_min_lon_lat) <- vec_unique_trackID
# List for matrix of bin coordinates (lon,lat) for each track. 
         list_bin_centre_coords <- list()
         tr_min_idx <- 0

         for (pp_idx in 1:length(vec_unique_trackID)) {
         #for (pp_idx in 2) {
            pp <- vec_unique_trackID[pp_idx]
            tracks_lon_temp <- NULL
            tracks_lat_temp <- NULL

            for (m_idx in 1:m_limit) {
            #for (m_idx in 8) {
               tr_idx <- which( array_list_XXing[[m_idx,S_idx,b_idx]] == pp )
               if ( length(tr_idx) > 0 ) {
                  for (kk in 1:length(tr_idx)) {
                     tracks_lon_temp <- c(tracks_lon_temp,mat_list_buoy_data1[[m_idx]][[3]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][[ which(array_list_XXing[[m_idx,S_idx,b_idx]] == pp)[kk] ]] ] )
                     tracks_lat_temp <- c(tracks_lat_temp,mat_list_buoy_data1[[m_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][[ which(array_list_XXing[[m_idx,S_idx,b_idx]] == pp)[kk] ]] ] )
                  }
               }
            }
# Fit linear regression model to find "average track" with which to use for binning.
            df_AA <- data.frame(lon=tracks_lon_temp,lat=tracks_lat_temp)
            lm_lonlat <- lm(lon ~ lat + I(lat^2), data = df_AA)
# Use an idealised "high resolution" track (1001 points long), generated with the linear model, to find the closest possible point (within resolution tolerance).
# Train on latitude.
            vec_lat_seq <- seq( floor( min(tracks_lat_temp) * 10 ) / 10, ceiling( max(tracks_lat_temp) * 10 ) / 10,, 1001)
            if ( any(pp == paste0("D",1:3)) ) {
               vec_lat_seq <- rev(vec_lat_seq)
	    }
            vec_pred_lon <- predict.lm(lm_lonlat,newdata=data.frame(lat=vec_lat_seq))
            list_tr_lonlat_pred[[pp_idx]] <- cbind(pred_lon=vec_pred_lon,pred_lat=vec_lat_seq)
# Train on longitude (not used).
            #lm_latlon <- lm(lat ~ lon + I(lon^2), data = df_AA)
            #vec_lon_seq <- seq( floor( min(tracks_lon_temp) * 10 ) / 10, ceiling( max(tracks_lon_temp) * 10 ) / 10,, 1001)
            #vec_pred_lat <- predict.lm(lm_latlon,newdata=data.frame(lon=vec_lon_seq))
            #list_tr_lonlat_pred[[pp_idx]] <- cbind(pred_lon=vec_lon_seq,pred_lat=vec_pred_lat)
            #vec_buoy_dist <- apply(X=cbind(vec_pred_lat,vec_lon_seq),MAR=1,FUN=func_buoy_dist,B_idx=buoy_idx)
# Find the minimum distance from the buoy.
            vec_buoy_dist <- apply(X=cbind(vec_lat_seq,vec_pred_lon),MAR=1,FUN=func_buoy_dist,B_idx=buoy_idx)
            tr_min_idx[pp_idx] <- which.min( vec_buoy_dist )
            mat_tr_min_lon_lat[pp_idx,] <- cbind(vec_pred_lon[tr_min_idx[pp_idx]],vec_lat_seq[tr_min_idx[pp_idx]],min(vec_buoy_dist))
            #mat_tr_min_lon_lat[pp_idx,] <- cbind(vec_lon_seq[tr_min_idx[pp_idx]],vec_pred_lat[tr_min_idx[pp_idx]])
            colnames(mat_tr_min_lon_lat) <- c("lon","lat","min_dist")

# Convert the high resolution lat / lon track to absolute distances (in metres, along track) for plotting bins on maps.
            vec_track_dist <- apply(X=cbind(vec_lat_seq,vec_pred_lon),MAR=1,FUN=func_sat_dist1A,y=rev(mat_tr_min_lon_lat[pp_idx,1:2]))
            df_BB <- data.frame(dist=vec_track_dist,lat=vec_lat_seq,lon=vec_pred_lon)
# Latitude.
            lm_buoy_dist_lat_neg <- lm(lat ~ dist + I(dist^2) + I(dist^3), data = df_BB[1:tr_min_idx[pp_idx],])
            lm_buoy_dist_lat_pos <- lm(lat ~ dist + I(dist^2) + I(dist^3), data = df_BB[tr_min_idx[pp_idx]:1001,])

            #vec_dist_bins_centre <- seq(-floor(149 / dist_bin_width)*dist_bin_width,floor(149 / dist_bin_width)*dist_bin_width,dist_bin_width)
            #vec_dist_bins_neg <- seq(5*floor(range(vec_track_dist[1:tr_min_idx[pp_idx]])[1] / 5),0,5)
            vec_dist_bins_neg <- seq(dist_bin_width*floor(range(vec_track_dist[1:tr_min_idx[pp_idx]])[1] / dist_bin_width),0,dist_bin_width)
            vec_pred_tr_lat_neg <- predict.lm(lm_buoy_dist_lat_neg,newdata=data.frame(dist=vec_dist_bins_neg))
            #vec_dist_bins_pos <- seq(5,5*floor(range(vec_track_dist[tr_min_idx[pp_idx]:1001])[2] / 5),dist_bin_width)
            vec_dist_bins_pos <- seq(dist_bin_width,dist_bin_width*floor(range(vec_track_dist[tr_min_idx[pp_idx]:1001])[2] / dist_bin_width),dist_bin_width)
            vec_pred_tr_lat_pos <- predict.lm(lm_buoy_dist_lat_pos,newdata=data.frame(dist=vec_dist_bins_pos))
# Longitude.
            #lm_buoy_dist_lon_neg <- lm(lon ~ dist + I(dist^0.5) + I(dist^0.33), data = df_BB[1:tr_min_idx[pp_idx],])
            lm_buoy_dist_lon_neg <- lm(lon ~ dist + I(dist^2) + I(dist^3), data = df_BB[1:tr_min_idx[pp_idx],])
            lm_buoy_dist_lon_pos <- lm(lon ~ dist + I(dist^2) + I(dist^3), data = df_BB[tr_min_idx[pp_idx]:1001,])

            vec_pred_tr_lon_neg <- predict.lm(lm_buoy_dist_lon_neg,newdata=data.frame(dist=vec_dist_bins_neg))
            vec_pred_tr_lon_pos <- predict.lm(lm_buoy_dist_lon_pos,newdata=data.frame(dist=vec_dist_bins_pos))

# Reverse the lon / lat coordinates for [D]escending tracks.
            if ( any(pp == paste0("D",1:3)) ) {
               #mat_bin_centre_coords <- cbind(bin_dist=-rev(c(vec_dist_bins_neg,vec_dist_bins_pos)),lon=rev(c(vec_pred_tr_lon_neg,vec_pred_tr_lon_pos)),lat=rev(c(vec_pred_tr_lat_neg,vec_pred_tr_lat_pos)))
               mat_bin_centre_coords <- cbind(bin_dist=c(vec_dist_bins_neg,vec_dist_bins_pos),lon=c(vec_pred_tr_lon_neg,vec_pred_tr_lon_pos),lat=c(vec_pred_tr_lat_neg,vec_pred_tr_lat_pos))
	    } else {
               mat_bin_centre_coords <- cbind(bin_dist=c(vec_dist_bins_neg,vec_dist_bins_pos),lon=c(vec_pred_tr_lon_neg,vec_pred_tr_lon_pos),lat=c(vec_pred_tr_lat_neg,vec_pred_tr_lat_pos))
	    }
            list_bin_centre_coords[[pp_idx]] <- mat_bin_centre_coords

#-------------------------------------------------------------------------------------------------#
## Plotting.
## Plot the 1 Hz points.
#            #points(tracks_lon_temp[vec_all_dist > 0],tracks_lat_temp[vec_all_dist > 0])
#            #points(tracks_lon_temp[vec_all_dist < 0],tracks_lat_temp[vec_all_dist < 0])
#            points(tracks_lon_temp,tracks_lat_temp)
## Plot the fitted tracks.
#            lines(vec_pred_lon,vec_lat_seq,col="orange",lwd=2)
## Plot the nearest point.
#            points(mat_tr_min_lon_lat,col="red",pch=19)
#-------------------------------------------------------------------------------------------------#
         }
         mat_list_tr_min_lon_lat[[S_idx,b_idx]] <- mat_tr_min_lon_lat
         mat_list_bin_coords[[S_idx,b_idx]] <- list_bin_centre_coords
#-------------------------------------------------------------------------------------------------#
## Plot buoy.
#         points(df_buoy_data$buoy_lon[buoy_idx]+360,df_buoy_data$buoy_lat[buoy_idx],col="blue",pch=23,cex=2,bg="yellow")
#-------------------------------------------------------------------------------------------------#
      }
      list_B_list_tr_lm_seq[[b_idx]] <- list_tr_lonlat_pred
   }

#-------------------------------------------------------------------------------------------------#
# Phase 2: Bin all the 1 Hz points.
#-------------------------------------------------------------------------------------------------#
   #mat_dist_bins <- matrix(c(seq(-147.5,147.5,5)[1:59],seq(-147.5,147.5,5)[2:60]),ncol=2)
   vec_dist_bins <- seq( -((floor(149 / dist_bin_width)*dist_bin_width)+dist_bin_width/2), ((floor(149 / dist_bin_width)*dist_bin_width)+dist_bin_width/2),dist_bin_width )
   mat_dist_bins <- matrix( c( vec_dist_bins[1:(length(vec_dist_bins)-1)],vec_dist_bins[2:length(vec_dist_bins)]),ncol=2)
   list_B_list_tr_lonlat_samp <- list()
# Loop over (active) buoys.
   for (b_idx in (1:length(Bidx))[vec_B_active]) {
# Assign variables from lists.
      mat_list_Lvec_buoy_samp <- list_B_mat_list_Lvec_buoy_samp[[b_idx]]
      mat_list_breaks_master <- list_B_mat_list_breaks_master[[b_idx]]
      #mat_list_buoy_data1 <- list_B_mat_list_buoy_data1[[b_idx]]
      mat_list_Xing <- list_B_mat_list_Xing[[b_idx]]
# Loop over missions.
      for (S_idx in Sidx) {
         mat_list_buoy_data1 <- mat_list_buoy_data[[S_idx]][,b_idx_list[Bidx[b_idx]]]
         vec_unique_trackID <- unique(unlist(array_list_XXing[,S_idx,b_idx]))
# List to capture all sampled track lon / lat data.
         list_tr_lonlat_samp <- vector(mode = "list",length = length(vec_unique_trackID))

         for (m_idx in 1:m_limit) {
# Set up list to store monthly data.
            list_bins_temp <- list()
            for (pp_idx in 1:length(vec_unique_trackID)) {
            #for (pp_idx in 1) {
               mat_tr_lonlat <- NULL
               pp <- vec_unique_trackID[pp_idx]
               tr_idx <- which( array_list_XXing[[m_idx,S_idx,b_idx]] == pp )

               if ( length(tr_idx) > 0 ) {

                  for (kk in 1:length(tr_idx)) {
                     mat_AA <- cbind(
                                  mat_list_buoy_data1[[m_idx]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][[ tr_idx[kk] ]] ],
                                  mat_list_buoy_data1[[m_idx]][[3]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][mat_list_breaks_master[[m_idx,S_idx]][[ tr_idx[kk] ]] ] )
                     if ( mat_list_Xing[[m_idx,S_idx]][tr_idx[kk]] == "A" ) {
                        #vec_all_dist <- sapply(X=1:dim(mat_AA)[1],FUN=function(x) { func_sat_dist1A(mat_AA[x,],rev(tr_min_lon_lat[pp_idx,])) } )
                        vec_all_dist <- sapply(X=1:dim(mat_AA)[1],FUN=function(x) { func_sat_dist1A(mat_AA[x,],rev( mat_list_tr_min_lon_lat[[S_idx,b_idx]][pp_idx,1:2] )) } )
                     } else {
                        #vec_all_dist <- sapply(X=1:dim(mat_AA)[1],FUN=function(x) { func_sat_dist1D(mat_AA[x,],rev(tr_min_lon_lat[pp_idx,])) } )
                        vec_all_dist <- sapply(X=1:dim(mat_AA)[1],FUN=function(x) { func_sat_dist1A(mat_AA[x,],rev( mat_list_tr_min_lon_lat[[S_idx,b_idx]][pp_idx,1:2] )) } )
                     }
                     list_bins_temp[[ tr_idx[kk] ]] <- sapply(X=1:length(vec_all_dist),FUN=function(x) { which( sapply(X=1:length(vec_dist_bins_centre),FUN=function(y) { mat_dist_bins[y,1] < vec_all_dist[x] & mat_dist_bins[y,2] > vec_all_dist[x] }) ) })
# Capture the sampled lon / lats in a single matrix for global storage (mostly for plotting maps).
                     mat_tr_lonlat <- rbind(mat_tr_lonlat,mat_AA)
                  }
               }
# Fix for assinging NULL to list element and "deleting" element.
               if ( !( is.null(list_tr_lonlat_samp[[pp_idx]]) & is.null(mat_tr_lonlat) ) ) {
                  list_tr_lonlat_samp[[pp_idx]] <- rbind(mat_tr_lonlat,list_tr_lonlat_samp[[pp_idx]])
               }
            }
            array_list_bins_master[[m_idx,S_idx,b_idx]] <- list_bins_temp
         }
      }
      list_B_list_tr_lonlat_samp[[b_idx]] <- list_tr_lonlat_samp
   }

#-------------------------------------------------------------------------------------------------#
# Phase 3: Arrange the data for correlation analysis, find physical scale for each track.
#          Here, we separate 1 Hz points into track IDs.
#-------------------------------------------------------------------------------------------------#
# Function for mode.
   func_mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
   }

# Find indices for track IDs (monthly).
   array_list_trackID <- matrix(list(),nrow=3,ncol=length(Bidx))
   mat_list_bins_trackID <- matrix(list(),nrow=3,ncol=length(Bidx))

# Loop over (active) buoys.
   for (b_idx in (1:length(Bidx))[vec_B_active]) {

      mat_list_breaks_master <- list_B_mat_list_breaks_master[[b_idx]]
      mat_list_Lvec_break_dist_min_idx <- list_B_mat_list_Lvec_break_dist_min_idx[[b_idx]]

      array_scale_m_idx <- array(NA,dim=c(2,length(vec_unique_trackID),3))

      for (S_idx in Sidx) {

         vec_unique_trackID <- unique(unlist(array_list_XXing[,S_idx,b_idx]))

         mat_list_trackID <- matrix(list(),nrow=60,ncol=length(vec_unique_trackID))
         mat_list_bins_trackID_temp <- matrix(list(),nrow=60,ncol=length(vec_unique_trackID))

         for ( jj in 1:length(vec_unique_trackID) ) {
# Loop over months.
            for ( m_idx in 1:m_limit ) {
               mat_list_trackID[[m_idx,jj]] <- mat_list_breaks_master[[m_idx,S_idx]][which(array_list_XXing[[m_idx,S_idx,b_idx]] == vec_unique_trackID[jj])]
               mat_list_bins_trackID_temp[[m_idx,jj]] <- array_list_bins_master[[m_idx,S_idx,b_idx]][which(array_list_XXing[[m_idx,S_idx,b_idx]] == vec_unique_trackID[jj])]
            }
         }
         array_list_trackID[[S_idx,b_idx]] <- mat_list_trackID
         mat_list_bins_trackID[[S_idx,b_idx]] <- mat_list_bins_trackID_temp
      }
   }

# Find Hs values for each 1 Hz point in each track ID.
   mat_list_trackID_hs1 <- matrix(list(),nrow=3,ncol=length(Bidx))
   mat_list_trackID_hs_monthly <- matrix(list(),nrow=3,ncol=length(Bidx))
   mat_list_trackID_rms1 <- matrix(list(),nrow=3,ncol=length(Bidx))

# Loop over (active) buoys.
   for (b_idx in (1:length(Bidx))[vec_B_active]) {
      mat_list_1Hz_hs <- list_B_mat_list_1Hz_hs[[b_idx]]

      for (S_idx in Sidx) {
         vec_unique_trackID <- unique(unlist(array_list_XXing[,S_idx,b_idx]))
         list_trackID_hs1 <- list()
         list_trackID_hs2 <- list()
         list_trackID_rms1 <- list()
# Assign from array (for convenience).
         mat_list_trackID <- as.matrix( array_list_trackID[[S_idx,b_idx]] )
# Loop over track IDs.
         list_temp2 <- vector(mode = "list",length = 60)
         for ( jj in 1:length(vec_unique_trackID) ) {
            mat_temp1 <- mat_rms_temp1 <- NULL
#print(paste("TrackID:",jj))
            for ( m_idx in 1:m_limit ) {
#print(paste("m_idx:",m_idx))
               mat_trackID_hs_temp <- matrix(NA,nrow=length(mat_list_trackID[[m_idx,jj]]),ncol=dim(mat_dist_bins)[1])
               mat_trackID_rms_temp <- matrix(NA,nrow=length(mat_list_trackID[[m_idx,jj]]),ncol=dim(mat_dist_bins)[1])

# Fix for missing tracks on some passes (is this a good solution?).
               if ( length(mat_list_trackID[[m_idx,jj]]) > 0 ) {

                  for ( ii in 1:length(mat_list_trackID[[m_idx,jj]]) ) {
                     mat_trackID_hs_temp[ii, mat_list_bins_trackID[[S_idx,b_idx]][[m_idx,jj]][[ii]] ] <- mat_list_1Hz_hs[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]]]
                     mat_trackID_rms_temp[ii, mat_list_bins_trackID[[S_idx,b_idx]][[m_idx,jj]][[ii]] ] <- mat_list_1Hz_rms_QC[[m_idx,S_idx]][mat_list_trackID[[m_idx,jj]][[ii]]]
                  }
               }
# Aggregated Hs data (over months) stored here.
               mat_temp1 <- rbind(mat_temp1,mat_trackID_hs_temp)
               mat_rms_temp1 <- rbind(mat_rms_temp1,mat_trackID_rms_temp)
# Monthly Hs data stored here.
               list_temp2[[m_idx]] <- mat_trackID_hs_temp
            }
            list_trackID_hs1[[jj]] <- mat_temp1
            list_trackID_hs2[[jj]] <- list_temp2
            list_trackID_rms1[[jj]] <- mat_rms_temp1
         }
         mat_list_trackID_hs1[[S_idx,b_idx]] <- list_trackID_hs1
         mat_list_trackID_hs_monthly[[S_idx,b_idx]] <- list_trackID_hs2
         mat_list_trackID_rms1[[S_idx,b_idx]] <- list_trackID_rms1
      }
   }

#=================================================================================================#
# Temporal collocation.
# To-do:
# Assign ERA5 output to matrix for S_idx and b_idx.
#=================================================================================================#
# Find the "mean" time stamp for each track segment (J3,S6).
   vec_buoy_hs_coloc_ALL <- vec_buoy_ap_coloc_ALL <- vec_ERA5_hs_coloc_ALL <- vec_ERA5_hs_BILIN_coloc_ALL <- NULL

   #mat_list_mean_time1 <- matrix(list(),nrow=60,ncol=3)
   mat_list_time_diff_J3B <- matrix(list(),nrow=60,ncol=3)
   mat_list_time_diff_J3ERA <- matrix(list(),nrow=60,ncol=3)
   Lmat_list_slot_J3B <- vector(mode = "list",length = 60)
   Lmat_list_slot_J3B_TEST <- vector(mode = "list",length = 60)
   Lmat_list_slot_J3ERA <- vector(mode = "list",length = 60)
# AD = all data.
   mat_list_buoy_hs_coloc_AD <- matrix(list(),nrow=3,ncol=length(Bidx))
   mat_list_buoy_hs_coloc_AD_TEST <- matrix(list(),nrow=60,ncol=length(vec_unique_trackID))
   mat_list_ERA5_hs_coloc_AD <- matrix(list(),nrow=60,ncol=length(vec_unique_trackID))
   mat_list_ERA5_hs_BILIN_coloc_AD <- matrix(list(),nrow=60,ncol=length(vec_unique_trackID))

# Loop over (active) buoys.
   for (b_idx in (1:length(Bidx))[vec_B_active]) {
      buoy_idx <- b_idx_list[Bidx[b_idx]]
      vec_buoy_time_num <- list_B_vec_buoy_time_num[[b_idx]]
      vec_unique_trackID <- unique(unlist(array_list_XXing[,,b_idx])) 
      mat_list_mean_time <- list_B_mat_list_mean_time[[b_idx]]
      vec_buoy_hs <- list_B_vec_buoy_hs[[b_idx]]
      #vec_buoy_ap <- list_B_vec_buoy_ap[[b_idx]]

      for (S_idx in Sidx) {
         vec_unique_trackID <- unique(unlist(array_list_XXing[,S_idx,b_idx]))
         mat_list_buoy_hs_coloc_AD_temp <- matrix(list(),nrow=60,ncol=length(vec_unique_trackID))

         for ( m_idx in 1:m_limit ) {
            #list_buoy_data1 <- list(mat_list_J3[[m_idx,buoy_idx]],mat_list_S6_LRM[[m_idx,buoy_idx]],mat_list_S6_SAR[[m_idx,buoy_idx]])
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

            #vec_buoy_ap_coloc[which(Lmat_list_slot_J3B[[m_idx]],arr.ind=T)[,2]] <- vec_buoy_ap[which(Lmat_list_slot_J3B[[m_idx]],arr.ind=T)[,1]]
            #vec_buoy_ap_coloc_ALL <- c(vec_buoy_ap_coloc_ALL,vec_buoy_ap_coloc)
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
   mat_list_outlier <- matrix(list(),nrow=3,ncol=length(Bidx))
   mat_list_sample <- matrix(list(),nrow=3,ncol=length(Bidx))
# Specify seasonal data.
   lab_season <- c("ONDJFM","AMJJAS","annual")
   list_season_idx <- list()
# Annual.
   list_season_idx[[3]] <- 1:m_limit
# DJF.
   #vec_season_idx <- as.vector(sapply(X=1:5,FUN=function(x) { c(1,2,12)+(x-1)*12 }))
   if ( flag_tandem ) {
# (D)JFMOND.
      list_season_idx[[1]] <- c(1:4,11,12,13)
# AMJJAS.
      list_season_idx[[2]] <- 5:10
   } else {
# JFMOND.
      list_season_idx[[1]] <- as.vector(sapply(X=1:5,FUN=function(x) { c(1,2,3,10,11,12)+(x-1)*12 }))
# AMJJAS.
      list_season_idx[[2]] <- as.vector(sapply(X=1:5,FUN=function(x) { c(4,5,6,7,8,9)+(x-1)*12 }))
   }

# Loop over (active) buoys.
   for (b_idx in (1:length(Bidx))[vec_B_active]) {
# Temporary for seasonal bias info.
      list_cor_season <- list()
      list_cor_95_season <- list()
      list_rmse_season <- list()
      list_bias_season <- list()
# Loop over season.
      vec_season <- 1:3
      for (season_idx in vec_season) {
            for (S_idx in Sidx) {
# Select target for correlation (buoy, ERA, ERA5_bilin).
            situ_data <- as.matrix(mat_list_buoy_hs_coloc_AD[[S_idx,b_idx]][list_season_idx[[season_idx]],])
            #situ_data <- mat_list_ERA5_hs_coloc_AD
            #situ_data <- mat_list_ERA5_hs_BILIN_coloc_AD

            vec_unique_trackID <- unique(unlist(array_list_XXing[,S_idx,b_idx]))

# Assign from array.
            #mat_list_trackID <- as.matrix( array_list_trackID[[S_idx,b_idx]] )
# Assign annual data to anaylsis.
            #list_trackID_hs <- mat_list_trackID_hs1[[S_idx,b_idx]]
# Assign seasonal data to anaylsis.
            list_trackID_hs <- list()
            for ( jj in 1:length(vec_unique_trackID) ) {
               mat_temp <- NULL
               for ( s_idx in list_season_idx[[season_idx]] ) {
                  mat_temp <- rbind(mat_temp,mat_list_trackID_hs_monthly[[S_idx,b_idx]][[jj]][[s_idx]])
               }
               list_trackID_hs[[jj]] <- mat_temp
            }

            list_vec_cor <- list()
            list_Lvec_cor_95 <- list()
            list_vec_rmse <- list()
            list_vec_bias <- list()
            list_1Hz_idx_dist <- list()
            list_outlier <- list()
            list_vec_sample <- list()
# Loop over unique tracks.
            for ( jj in 1:length(vec_unique_trackID) ) {
# Get track length.
               i_len_trackID <- dim(list_trackID_hs[[jj]])[2]
               list_vec_cor[[jj]] <- rep(NA,i_len_trackID)
               list_vec_rmse[[jj]] <- rep(NA,i_len_trackID)
               list_vec_bias[[jj]] <- rep(NA,i_len_trackID)
               list_outlier[[jj]] <- matrix(NA,nrow=length(unlist(situ_data[,jj])),ncol=i_len_trackID)
               list_vec_sample[[jj]] <- rep(NA,i_len_trackID)
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
                  if ( sum( !is.na( unlist(situ_data[,jj]) ) & !is.na( list_trackID_hs[[jj]][,ii] ) ) > 10 ) {
# Print outliers.
                     vec_err_dist <- (unlist(situ_data[,jj]) - list_trackID_hs[[jj]][,ii]) / ( (unlist(situ_data[,jj]) + list_trackID_hs[[jj]][,ii]) / 2 )
                     #outlier_count <- sum( vec_err_dist > 4*sqrt(var(vec_err_dist,na.rm=T)), na.rm=T )
                     #print(paste(" Outliers:",outlier_count)) 
                     list_outlier[[jj]][,ii] <- ! vec_err_dist > 5*sqrt(var(vec_err_dist,na.rm=T))
                     mat_pair_temp <- cbind(unlist(situ_data[,jj]),list_trackID_hs[[jj]][,ii])[list_outlier[[jj]][,ii],]
# Sample count.
                     list_vec_sample[[jj]][ii] <- sum( !is.na( unlist(situ_data[,jj]) ) & !is.na( list_trackID_hs[[jj]][,ii] ) )
# Correlation.
                     list_vec_cor[[jj]][ii] <- cor( unlist(situ_data[,jj]),list_trackID_hs[[jj]][,ii],use="pairwise.complete.obs" )
                     #list_vec_cor[[jj]][ii] <- cor( mat_pair_temp[,1], mat_pair_temp[,2], use="pairwise.complete.obs" )
# RMSD.
                     list_vec_rmse[[jj]][ii] <- sqrt( mean( (unlist(situ_data[,jj])-list_trackID_hs[[jj]][,ii])^2,na.rm=T ) )
# Bias (sat - buoy).
                     mat_stat_data <- cbind(unlist(situ_data[,jj]),list_trackID_hs[[jj]][,ii])
                     Lvec_bias <- !apply(X=mat_stat_data,MAR=1,function(x) { any(is.na(x)) })
                     list_vec_bias[[jj]][ii] <- mean(mat_stat_data[Lvec_bias,2])-mean(mat_stat_data[Lvec_bias,1])
                  }
               }
               list_Lvec_cor_95[[jj]] <- list_vec_cor[[jj]] > cor_thresh
            }
            #mat_list_cor[[S_idx,b_idx]] <- list_vec_cor
            #mat_list_cor_95[[S_idx,b_idx]] <- list_Lvec_cor_95
            #mat_list_rmse[[S_idx,b_idx]] <- list_vec_rmse
            #mat_list_bias[[S_idx,b_idx]] <- list_vec_bias
            list_cor_season[[season_idx]] <- list_vec_cor
            list_cor_95_season[[season_idx]] <- list_Lvec_cor_95
            list_rmse_season[[season_idx]] <- list_vec_rmse
            list_bias_season[[season_idx]] <- list_vec_bias
            mat_list_outlier[[S_idx,b_idx]] <- list_outlier
            mat_list_sample[[S_idx,b_idx]] <- list_vec_sample
         }
      }

#-------------------------------------------------------------------------------------------------#
# Plotting.
## Plot median of super-observation.
#   X11(); plot(unlist(mat_list_buoy_hs_coloc_AD[,1]),apply(X=list_trackID_hs[[1]],MAR=1,FUN=median ),xlim=c(0,8),ylim=c(0,8)); abline(0,1)
## Plot by sub-point.
#   X11(); plot(unlist(mat_list_buoy_hs_coloc_AD[,1]),list_trackID_hs[[1]][,1],xlim=c(0,8),ylim=c(0,8)); abline(0,1)

#-------------------------------------------------------------------------------------------------#
# Along track correlations, bias, RMSD, etc.
      if ( flag_plot_track_stats) {

# X-axis (along-track bin centre) scale.
         xlim1 <- floor( length(vec_dist_bins_centre) / 2 + 1 ) - ( 5 * floor( floor( length(vec_dist_bins_centre) / 2 ) / 5 ) )
         xlim2 <- length(vec_dist_bins_centre) - (xlim1-1)
         vec_stats_xlim <- seq(xlim1,xlim2,5)
         track_centre_point <- 1+floor( length(vec_dist_bins_centre) / 2 )
# Different figure for each satellite dataset.
         for (S_idx in Sidx) {
            plot_data <- mat_list_buoy_hs_coloc_AD[[S_idx,b_idx]]
            vec_unique_trackID <- unique(unlist(array_list_XXing[,S_idx,b_idx]))
            list_trackID_hs <- mat_list_trackID_hs1[[S_idx,b_idx]]

            system(paste0("if [ ! -d ./figures/test_sampling3/",buoy_list[buoy_idx]," ]; then mkdir ./figures/test_sampling3/",buoy_list[buoy_idx]," &> /dev/null; fi"))
            fig_cor_file_name <- paste0("./figures/test_sampling3/",buoy_list[buoy_idx],"/track_cor_","M",m_limit,"_",vec_tandem_labs[S_idx],"_",buoy_list[buoy_idx],"_",buoy_radius,"km_f06_",lab_season[season_idx],".pdf")
            pdf(fig_cor_file_name,width = (4.0 * length(vec_unique_trackID)), height = 8)
            par(mfrow=c(2,length(vec_unique_trackID)),mar=c(5,4,1,4),mgp=c(3.1,1,0),oma=c(0,1,3,0))

            for ( jj in 1:length(vec_unique_trackID) ) {
               i_len_trackID <- dim(list_trackID_hs[[jj]])[2]
               title_top <- paste0("Track ID: ",vec_unique_trackID[[jj]])
               #plot(1:i_len_trackID,list_vec_cor[[jj]],ylim=c(0.5,1.0),xlab="1 Hz ground distance (km)",ylab="Correlation",main=paste("Track ID:",vec_unique_trackID[[jj]],"\nBuoy:",buoy_list[buoy_idx]),axes=F)
               #plot(1:i_len_trackID,list_vec_cor[[jj]],ylim=c(0.5,1.0),xlab="1 Hz surface distance (km)",ylab="Correlation",main=title_top,axes=F,cex.lab=1.2)
               plot(1:i_len_trackID,list_cor_season[[season_idx]][[jj]],ylim=c(0.5,1.0),xlab="Bin distance along-track (km)",ylab="Correlation",main=title_top,axes=F,cex.lab=1.2)
               axis(side=2,at=seq(0.5,1.0,0.1),labels=seq(0.5,1.0,0.1))
               #axis(side=1,at=seq(5,55,5),labels=format(25*seq(-5,5,1),digits=2),las=2)
               axis(side=1,at=vec_stats_xlim,labels=format(vec_dist_bins_centre[vec_stats_xlim],digits=2),las=2)
               abline(h=c(0.95,1.0),col="grey")
               abline(v=vec_stats_xlim,col="grey")
               abline(v=track_centre_point,col="blue",lwd=1.5)
               points(track_centre_point,list_cor_season[[season_idx]][[jj]][track_centre_point],pch=19,col="blue")

               par(new=T)
               if ( flag_tandem ) {
                  vec_counts_scale <- seq(0,50,10)
                  vec_counts_ylim <- c(0,1.8 * 50)
               } else {
                  vec_counts_scale <- seq(0,200,50)
                  vec_counts_ylim <- c(0,1.8 * 200)
               }
               #plot(1:i_len_trackID,sapply(X=1:i_len_trackID,FUN=function(x) { sum( !is.na( unlist(plot_data[,jj]) ) & !is.na( list_trackID_hs[[jj]][,x] ) ) }),pch=4,ylim=vec_counts_ylim,axes=F,xlab="",ylab="")
               plot(1:i_len_trackID,mat_list_sample[[S_idx,b_idx]][[jj]],ylim=vec_counts_ylim,axes=F,xlab="",ylab="",pch=4)
               axis(side=4,at=vec_counts_scale)
               mtext("Number of temporal samples", side=4, line=2, cex=0.8)
               if ( jj == 1) {
                  legend(1,250,legend=c("Point closest to buoy","Correlation","Number of temporal samples"),bg="white",pch=c(19,1,4),col=c("blue","black","black"))
               }
# Top title.
               mtext(paste0(vec_tandem_labs[S_idx]," [",buoy_radius," km] Buoy: ",buoy_list[buoy_idx],", Season: ",lab_season[season_idx]), side=3, line=1, cex=1.5, outer=TRUE)
            }

            for ( jj in 1:length(vec_unique_trackID) ) {
               i_len_trackID <- dim(list_trackID_hs[[jj]])[2]
               title_top <- paste0(vec_tandem_labs[S_idx]," [",buoy_radius," km] Track ID: ",vec_unique_trackID[[jj]])
               plot(1:i_len_trackID,list_rmse_season[[season_idx]][[jj]],ylim=c(-0.3,0.7),xlab="Bin distance along-track (km)",ylab="RMSD",main="",axes=F,cex.lab=1.2)
               axis(side=2,at=seq(0.1,0.7,0.1),labels=seq(0.1,0.7,0.1))
               axis(side=1,at=vec_stats_xlim,labels=format(vec_dist_bins_centre[vec_stats_xlim],digits=2),las=2)
               abline(v=vec_stats_xlim,col="grey")
               abline(v=track_centre_point,col="blue",lwd=1.5)
               par(new=T)
# Plot BIAS.
               #plot(1:i_len_trackID,mat_list_bias[[S_idx,b_idx]][[jj]],pch=4,ylim=c(-0.5,3),axes=F,xlab="",ylab="")
               plot(1:i_len_trackID,list_bias_season[[season_idx]][[jj]],pch=4,ylim=c(-0.5,3),axes=F,xlab="",ylab="")
               abline(h=c(0),col="grey")
               axis(side=4,at=seq(-0.5,1,0.25))
               mtext("Mean bias", side=4, line=2, cex=0.8)
               if ( jj == 1) {
                  legend(1,15,legend=c("Point closest to buoy","RMSD","Number of temporal samples"),pch=c(19,1,4),col=c("blue","black","black"))
               }
## Histogram of collocation time differences.
#               if ( jj == 1) {
#                  hist(unlist( mat_list_time_diff_J3B )/60,breaks=30,xlim=c(0,30),main=paste("Distribution of collocation\ntime differences. Total:",sum( !is.na( unlist( mat_list_time_diff_J3B ) ) )),xlab="Time difference (minutes)")
#               }
            }

            dev.off()
            #system(paste("okular",fig_rmse_file_name,"&> /dev/null &"))
            system(paste("okular",fig_cor_file_name,"&> /dev/null &"))
         }
# Closure for season loop.
      }
      mat_list_cor[[S_idx,b_idx]] <- list_cor_season
      mat_list_cor_95[[S_idx,b_idx]] <- list_cor_95_season
      mat_list_rmse[[S_idx,b_idx]] <- list_rmse_season
      mat_list_bias[[S_idx,b_idx]] <- list_bias_season
# Closure for buoy loop.
   }
#-------------------------------------------------------------------------------------------------#
# Multi-buoy plotting.
   if ( flag_plot_multi_cor ) {

      fig_multi_cor_file_name <- paste("./figures/test_sampling2/multi_cor_",fig_lab_region,"_",vec_tandem_labs[S_idx],"_",buoy_radius,"km_S6ADJ.pdf",sep="")
      pdf(fig_multi_cor_file_name,width = 10,height = 12)
      par(mfrow=c(5,4),mar=c(5,4,4,5),mgp=c(3.1,1,0))
   
# Loop over (active) buoys.
      for (b_idx in (1:length(Bidx))[vec_B_active]) {
      #for (b_idx in 1:5) {
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
# Calculate median values based on different sampling methods
# "Full track" median.
# Nearest 1 Hz.
# Nearest 3 Hz.
# "Adaptive" sampling based on correlation.

   #mat_list_trackID_hs_med_rand <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
   mat_list_trackID_hs_med <- matrix(list(),nrow=3,ncol=length(Bidx))
   mat_list_trackID_hs_med_adapt <- matrix(list(),nrow=3,ncol=length(Bidx))
   mat_list_trackID_hs_min1 <- matrix(list(),nrow=13,ncol=length(Bidx))
   mat_list_trackID_hs_min3 <- matrix(list(),nrow=13,ncol=length(Bidx))

   mat_list_trackID_hs_med_1Hz_count <- matrix(list(),nrow=3,ncol=length(Bidx))
   mat_list_trackID_hs_med_adapt_1Hz_count <- matrix(list(),nrow=3,ncol=length(Bidx))
   mat_list_trackID_hs_min3_1Hz_count <- matrix(list(),nrow=3,ncol=length(Bidx))

   mat_list_trackID_rms_med <- matrix(list(),nrow=3,ncol=length(Bidx))

   for (S_idx in Sidx) {

# Loop over (active) buoys.
      for (b_idx in (1:length(Bidx))[vec_B_active]) {

         list_trackID_hs <- mat_list_trackID_hs1[[S_idx,b_idx]]
         list_trackID_rms <- mat_list_trackID_rms1[[S_idx,b_idx]]

         list_Lvec_cor_95 <- mat_list_cor_95[[S_idx,b_idx]]
         list_outlier <- mat_list_outlier[[S_idx,b_idx]]
         vec_unique_trackID <- unique(unlist(array_list_XXing[,S_idx,b_idx]))

         list_trackID_hs_med <- vector(mode = "list",length = length(vec_unique_trackID))
         list_trackID_hs_med_adapt <- vector(mode = "list",length = length(vec_unique_trackID))
         list_trackID_hs_min1 <- vector(mode = "list",length = length(vec_unique_trackID))
         list_trackID_hs_min3 <- vector(mode = "list",length = length(vec_unique_trackID))

         list_trackID_hs_med_1Hz_count <- vector(mode = "list",length = length(vec_unique_trackID))
         list_trackID_hs_med_adapt_1Hz_count <- vector(mode = "list",length = length(vec_unique_trackID))
         list_trackID_hs_min3_1Hz_count <- vector(mode = "list",length = length(vec_unique_trackID))

         list_trackID_rms_med <- vector(mode = "list",length = length(vec_unique_trackID))

         for ( jj in 1:length(vec_unique_trackID) ) {
            i_len_trackID <- dim(list_trackID_hs[[jj]])[2]
# Don't need the monthly loop...
            #for ( m_idx in 1:13 ) {
               if ( !is.null(list_trackID_hs[[jj]]) ) {
                  mat_outlier_temp <- list_outlier[[jj]]
                  #list_trackID_hs_med[[jj]] <- sapply(X=1:dim(list_trackID_hs[[jj]])[1],FUN=function(x) { median(list_trackID_hs[[jj]][x,mat_outlier_temp[x,]],na.rm=T) })
                  list_trackID_hs_med[[jj]] <- sapply(X=1:dim(list_trackID_hs[[jj]])[1],FUN=function(x) { median(list_trackID_hs[[jj]][x,],na.rm=T) })
                  list_trackID_rms_med[[jj]] <- sapply(X=1:dim(list_trackID_hs[[jj]])[1],FUN=function(x) { median(list_trackID_rms[[jj]][x,],na.rm=T) })
                  #list_trackID_hs_med_1Hz_count[[jj]] <- sapply(X=1:dim(list_trackID_hs[[jj]])[1],FUN=function(x) { sum(!is.na(list_trackID_hs[[jj]][x,mat_outlier_temp[x,]])) })
                  list_trackID_hs_med_1Hz_count[[jj]] <- sapply(X=1:dim(list_trackID_hs[[jj]])[1],FUN=function(x) { sum(!is.na(list_trackID_hs[[jj]][x,])) })

                  list_trackID_hs_med_adapt[[jj]] <- sapply(X=1:dim(list_trackID_hs[[jj]])[1],FUN=function(x) { median(list_trackID_hs[[jj]][x,list_Lvec_cor_95[[3]][[jj]]],na.rm=T) })
                  list_trackID_hs_med_adapt_1Hz_count[[jj]] <- sapply(X=1:dim(list_trackID_hs[[jj]])[1],FUN=function(x) { sum(!is.na(list_trackID_hs[[jj]][x,list_Lvec_cor_95[[3]][[jj]]])) })
                  #mat_list_trackID_hs_med_rand[[m_idx,jj]] <- sapply(X=1:dim(mat_list_trackID_hs[[m_idx,jj]])[1],FUN=function(x) { median(mat_list_trackID_hs[[m_idx,jj]][x,runif(i_len_trackID) > 0.1],na.rm=T) })

                  list_trackID_hs_min1[[jj]] <- sapply(X=1:dim(list_trackID_hs[[jj]])[1],FUN=function(x) { list_trackID_hs[[jj]][x,c(rep(FALSE,(length(vec_dist_bins)/2)-1),TRUE,rep(FALSE,(length(vec_dist_bins)/2)-1))] })

                  list_trackID_hs_min3[[jj]] <- sapply(X=1:dim(list_trackID_hs[[jj]])[1],FUN=function(x) { median(list_trackID_hs[[jj]][x,c(rep(FALSE,(length(vec_dist_bins)/2)-2),rep(TRUE,3),rep(FALSE,(length(vec_dist_bins)/2)-2))],na.rm=T) })
                  list_trackID_hs_min3_1Hz_count[[jj]] <- sapply(X=1:dim(list_trackID_hs[[jj]])[1],FUN=function(x) { sum(!is.na(list_trackID_hs[[jj]][x,c(rep(FALSE,(length(vec_dist_bins)/2)-2),rep(TRUE,3),rep(FALSE,(length(vec_dist_bins)/2)-2))])) })
               }
#         if ( !is.null(mat_list_trackID_hs[[m_idx,jj]]) ) {
#            mat_list_trackID_hs_med[[m_idx,jj]] <- sapply(X=1:dim(mat_list_trackID_hs[[m_idx,jj]])[1],FUN=function(x) { median(mat_list_trackID_hs[[m_idx,jj]][x,],na.rm=T) })
#            mat_list_trackID_hs_med_adapt[[m_idx,jj]] <- sapply(X=1:dim(mat_list_trackID_hs[[m_idx,jj]])[1],FUN=function(x) { median(mat_list_trackID_hs[[m_idx,jj]][x,list_vec_cor_95[[jj]]],na.rm=T) })
#            mat_list_trackID_hs_med_rand[[m_idx,jj]] <- sapply(X=1:dim(mat_list_trackID_hs[[m_idx,jj]])[1],FUN=function(x) { median(mat_list_trackID_hs[[m_idx,jj]][x,runif(i_len_trackID) > 0.1],na.rm=T) })
#            mat_list_trackID_hs_min1[[m_idx,jj]] <- sapply(X=1:dim(mat_list_trackID_hs[[m_idx,jj]])[1],FUN=function(x) { mat_list_trackID_hs[[m_idx,jj]][x,vec_mode_min[[jj]]] })
#         }
#      }
         }
         mat_list_trackID_hs_med[[S_idx,b_idx]] <- list_trackID_hs_med
         mat_list_trackID_hs_med_adapt[[S_idx,b_idx]] <- list_trackID_hs_med_adapt
         mat_list_trackID_hs_min1[[S_idx,b_idx]] <- list_trackID_hs_min1
         mat_list_trackID_hs_min3[[S_idx,b_idx]] <- list_trackID_hs_min3

         mat_list_trackID_hs_med_1Hz_count[[S_idx,b_idx]] <- list_trackID_hs_med_1Hz_count
         mat_list_trackID_hs_med_adapt_1Hz_count[[S_idx,b_idx]] <- list_trackID_hs_med_adapt_1Hz_count
         mat_list_trackID_hs_min3_1Hz_count[[S_idx,b_idx]] <- list_trackID_hs_min3_1Hz_count

         mat_list_trackID_rms_med[[S_idx,b_idx]] <- list_trackID_rms_med
      }
   }

   if ( sum(unlist(list_Lvec_cor_95[[3]]),na.rm=T) == 0 ) {
      print(paste(" NO DATA FOR ADAPTIVE!"))
   }
   print(paste(" SQRT ERROR VAR [MED - ADAPT] **",sqrt( var( unlist(list_trackID_hs_med)-unlist(list_trackID_hs_med_adapt), na.rm=T ) ) ) )
   #print(paste(" SQRT ERROR VAR [MED - MIN]   **",sqrt( var( unlist(list_trackID_hs_med)-unlist(list_trackID_hs_min), na.rm=T ) ) ) )

#=================================================================================================#
# Create data structures for track statistics, plotting and so on.

   for (S_idx in Sidx) {
# Refactor the buoy data for trackID (to match the sat data).
      #S_idx <- Sidx[1]
      mat_list_buoy_hs_coloc_trackID <- matrix(list(),nrow=3,ncol=length(Bidx))
      for (b_idx in (1:length(Bidx))[vec_B_active]) { BB <- mat_list_buoy_hs_coloc_AD[[S_idx,b_idx]]; list_BB <- list(); for ( mm in 1:dim(BB)[2] ) { list_BB[[mm]] <- unlist(BB[,mm]) }; mat_list_buoy_hs_coloc_trackID[[S_idx,b_idx]] <- list_BB }

# Plotting.
# Correlation and regression.
      #df_reg <- data.frame(buoy_hs=unlist(mat_list_buoy_hs_coloc_AD[1,][,1]),sat_hs=unlist(mat_list_trackID_hs_med[[1,1]][[1]]))
      df_reg <- data.frame(
                           buoy_hs=unlist(lapply(X=1:length(Bidx),FUN=function(x) { unlist(mat_list_buoy_hs_coloc_trackID[[S_idx,x]]) })),
                           sat_hs=unlist(lapply(X=1:length(Bidx),FUN=function(x) { unlist(mat_list_trackID_hs_med[[S_idx,x]]) })),
                           sat_hs_adapt=unlist(lapply(X=1:length(Bidx),FUN=function(x) { unlist(mat_list_trackID_hs_med_adapt[[S_idx,x]]) })),
                           sat_hs_min1=unlist(lapply(X=1:length(Bidx),FUN=function(x) { unlist(mat_list_trackID_hs_min1[[S_idx,x]]) })),
                           sat_hs_min3=unlist(lapply(X=1:length(Bidx),FUN=function(x) { unlist(mat_list_trackID_hs_min3[[S_idx,x]]) })),
                           sat_1Hz_count=unlist(lapply(X=1:length(Bidx),FUN=function(x) { unlist(mat_list_trackID_hs_med_1Hz_count[[S_idx,x]]) })),
                           sat_adapt_1Hz_count=unlist(lapply(X=1:length(Bidx),FUN=function(x) { unlist(mat_list_trackID_hs_med_adapt_1Hz_count[[S_idx,x]]) })),
                           sat_min3_1Hz_count=unlist(lapply(X=1:length(Bidx),FUN=function(x) { unlist(mat_list_trackID_hs_min3_1Hz_count[[S_idx,x]]) })),
                           buoy_lab=unlist(lapply(X=1:length(Bidx),FUN=function(x) { rep(buoy_list[b_idx_list[Bidx[x]]],length(unlist(mat_list_buoy_hs_coloc_trackID[[S_idx,x]]))) })),
                           track_dist=unlist( lapply( X=1:length(Bidx),FUN=function(x) {
							 lapply( X=1:length(mat_list_buoy_hs_coloc_trackID[[S_idx,x]]), FUN=function(y) {
								       	rep(mat_list_tr_min_lon_lat[[S_idx,x]][y,3], length(mat_list_buoy_hs_coloc_trackID[[S_idx,x]][[y]])) } ) } ) ),
                           track_dir=unlist( lapply( X=1:length(Bidx),FUN=function(x) {
							 lapply( X=1:length(mat_list_buoy_hs_coloc_trackID[[S_idx,x]]), FUN=function(y) {
								       	rep(rownames(mat_list_tr_min_lon_lat[[S_idx,x]])[y], length(mat_list_buoy_hs_coloc_trackID[[S_idx,x]][[y]])) } ) } ) ) )
# Remove low values.
      df_reg$buoy_hs[df_reg$buoy_hs < 0.25] <- NA
      df_reg$sat_hs[df_reg$sat_hs < 0.25] <- NA
      df_reg$sat_hs_adapt[df_reg$sat_hs_adapt < 0.25] <- NA
      df_reg$sat_hs_min1[df_reg$sat_hs_min1 < 0.75] <- NA
      df_reg$sat_hs_min3[df_reg$sat_hs_min3 < 0.75] <- NA

# Save df_reg into a master list.
      list_df_reg_radius[[i_buoy_radius,S_idx]] <- df_reg
   }

#=================================================================================================#
# Single-panel plotting.
   if ( flag_scatter_plot ) {

      if ( flag_single_panel ) {
# Plotting parameters.
         pl_mfrow <- c(1,1)
         pl_oma <- c(6,7,2,6)
         pl_mar <- c(18,18,16,7)
         pl_mgp <- c(12,6,0)
         pl_cex <- 6; pl_cex_main <- 11; pl_cex_lab <- 7; pl_cex_axis <- 9; pl_cex_leg <- 8

         vec_plot_idx <- 2

         if ( flag_OS ) {
            plot_title <- c("J-3 Offshore","S6-MF LR Offshore","S6-MF HR Offshore")[Sidx]
         } else {
            plot_title <- c("J-3 Nearshore","S6-MF LR Nearshore","S6-MF HR Nearshore")[Sidx]
         }

         cex_mtext <- 8

         if ( length(Bidx) <= 2 ) {
            fig_scatter_adapt_nearest_file_name <- paste0("./figures/test_sampling3/",paste(buoy_list[b_idx_list[Bidx]]),"/scatter_adapt_",fig_lab_region,"_","M",m_limit,"_",vec_tandem_labs[S_idx],"_",buoy_radius,"km_",paste(buoy_list[b_idx_list[Bidx]],collapse='_'),"_",gsub('[.]','',cor_thresh),"_ADAPT.png")
         } else {
               fig_scatter_adapt_nearest_file_name <- paste0("./figures/test_sampling3/",fig_lab_region,"/scatter_adapt_",fig_lab_region,"_","M",m_limit,"_",vec_tandem_labs[S_idx],"_",buoy_radius,"km_BUOYS_",gsub('[.]','',cor_thresh),"_ADAPT.png")
# Multi-panel plotting.
         }
      } else {
# Plotting parameters.
         pl_mfrow <- c(2,2)
         pl_oma <- c(2,2,2,2)
         pl_mar <- c(12,14,8,5)
         pl_mgp <- c(9,3,0)
         pl_cex <- 4; pl_cex_main <- 6; pl_cex_lab <- 6; pl_cex_axis <- 5; pl_cex_leg <- 5

         vec_plot_idx <- 1:4

         plot_title <- c("Track median",paste("Adaptive (cor > ",cor_thresh,")",sep=""),paste("1 Hz nearest to buoy",sep=""),paste("Median 3 nearest to buoy",sep=""))

         cex_mtext <- 4
      
         if ( length(Bidx) <= 2 ) {
            fig_scatter_adapt_nearest_file_name <- paste0("./figures/test_sampling3/",paste(buoy_list[b_idx_list[Bidx]]),"/scatter_adapt_",fig_lab_region,"_","M",m_limit,"_",vec_tandem_labs[S_idx],"_",buoy_radius,"km_",paste(buoy_list[b_idx_list[Bidx]],collapse='_'),"_",gsub('[.]','',cor_thresh),"_MULTI.png")
         } else {
            fig_scatter_adapt_nearest_file_name <- paste0("./figures/test_sampling3/",fig_lab_region,"/scatter_adapt_",fig_lab_region,"_","M",m_limit,"_",vec_tandem_labs[S_idx],"_",buoy_radius,"km_BUOYS_",gsub('[.]','',cor_thresh),"_MULTI.png")
         }
      }

# 1 Hz counts.
      vec_1Hz_count <- c( sum(df_reg$sat_1Hz_count[!is.na(df_reg$buoy_hs)]),
                          sum(df_reg$sat_adapt_1Hz_count[!is.na(df_reg$buoy_hs)]),
                          sum(!is.na(df_reg$buoy_hs) & !is.na(df_reg$sat_hs_min1)),
                          sum(df_reg$sat_min3_1Hz_count[!is.na(df_reg$buoy_hs)]) )

# Buoy by buoy stats.
# list_AA_buoy <- sapply(X=1:length(Bidx),FUN=function(x) { unlist(mat_list_buoy_hs_coloc_trackID[[S_idx,x]]) })
# list_AA_sat <- sapply(X=1:length(Bidx),FUN=function(x) { unlist(mat_list_trackID_hs_min3[[S_idx,x]]) })
# BB <- sapply(X=1:11,function(x) { temp <- !is.na(list_AA_sat[[x]]) & !is.na(list_AA_buoy[[x]]); mean( list_AA_sat[[x]][temp] ) - mean( list_AA_buoy[[x]][temp] ) })
# X11(); hist(BB)

      high_bias <- 0.75
      vec_data_sets <- c("sat_hs","sat_hs_adapt","sat_hs_min1","sat_hs_min3")

## Fudge to examine [A]scending vs [D]ecsending tracks.
#   df_reg1 <- df_reg
##   df_reg <- df_reg1[sapply(X=df_reg1$track_dir,FUN=function(x) any( x== paste0("A",1:3)) ),]

      png(fig_scatter_adapt_nearest_file_name, width = 3000, height = 3000)
      par(mfrow=pl_mfrow,oma=pl_oma,mar=pl_mar,mgp=pl_mgp)

# Plot four panels, for the different sampling approaches.
      for (p_idx in 1:length(vec_plot_idx)) {
         plot_idx <- vec_plot_idx[p_idx]
# Specify sat data.
         vec_sat <- df_reg[,plot_idx+1]
# Get paired data.
         vec_sat_paired <- vec_sat[!is.na(df_reg$buoy_hs) & !is.na(vec_sat)]
         vec_buoy_paired <- df_reg$buoy_hs[!is.na(df_reg$buoy_hs) & !is.na(vec_sat)]
# Regression.
         lm_hs <- eval(parse(text=paste("lm(",vec_data_sets[plot_idx]," ~ buoy_hs,data=df_reg)",sep="")))
# RMSD
         hs_rmsd <- sqrt( mean( ( vec_buoy_paired - vec_sat_paired )^2 ,na.rm=T ) )
# RMSE
         hs_rmse <- sqrt(mean(lm_hs$residuals^2))
         hs_rmse1 <- vec_sat-df_reg$buoy_hs
# Correlation.
         hs_cor <- cor(df_reg$buoy_hs,vec_sat,use="pairwise.complete.obs")
# Bias.
         hs_bias <- mean(vec_sat_paired) - mean(vec_buoy_paired)
         hs_high_bias <- quantile(vec_sat_paired,probs=high_bias) - quantile(vec_buoy_paired,probs=high_bias)

         plot(df_reg$buoy_hs, vec_sat, xlim=c(0,10), ylim=c(0,10), main=plot_title[p_idx], xlab="Buoy", ylab="Sat", pch=19, cex=pl_cex, cex.main=pl_cex_main, cex.lab=pl_cex_lab, cex.axis=pl_cex_axis)
         points(df_reg$buoy_hs[df_reg$track_dist > 25], vec_sat[df_reg$track_dist > 25], pch=19, cex=(pl_cex-1), col="red")
# Points by track direction.
         #points(df_reg$buoy_hs[sapply(X=df_reg$track_dir,FUN=function(x) any( x== paste0("D",1:3)) )], vec_sat[sapply(X=df_reg$track_dir,FUN=function(x) any( x== paste0("D",1:3)) )], pch=19, cex=(pl_cex-1), col="red")

         if ( any( plot_idx == c(2,4) ) ) {
            #text(df_reg$buoy_hs[hs_rmse1 > 1], vec_sat[hs_rmse1 > 1]+0.4, labels=df_reg$buoy_lab[hs_rmse1 > 1],cex=4)
            text(df_reg$buoy_hs[hs_rmse1 < -2], vec_sat[hs_rmse1 < -2]+0.4, labels=df_reg$buoy_lab[hs_rmse1 < -2],cex=4)
            #text(df_reg$buoy_hs[hs_rmse1 > 1], vec_sat[hs_rmse1 > 1]+0.4, labels=df_reg$track_dist[hs_rmse1 > 1],cex=4)
         }
         #segments(x0=mean(vec_buoy_paired), y0=0, y1=mean(vec_sat_paired), col="red", lwd=4)
         #segments(x0=0, x1=mean(vec_buoy_paired), y0=mean(vec_sat_paired), col="red", lwd=4)
         #points(quantile(vec_buoy_paired,probs=high_bias), quantile(vec_sat_paired,probs=high_bias), col="red", pch=19)
         #segments(x0=quantile(vec_buoy_paired,probs=high_bias), y0=0, y1=quantile(vec_sat_paired,probs=high_bias), col="red", lwd=4)
         #segments(x0=0, x1=quantile(vec_buoy_paired,probs=high_bias), y0=quantile(vec_sat_paired,probs=high_bias), col="red", lwd=4)
         points(mean(vec_buoy_paired), mean(vec_sat_paired,na.rm=T), col="red", pch=19)
         par(new=T)
         qqplot(vec_buoy_paired,vec_sat_paired,xlim=c(0,10),ylim=c(0,10),pch=19,cex=3,col="orange",axes=F,xlab="",ylab="")
         abline(0, 1, col="blue", lwd=5.0)

         sat_mean <- mean(vec_sat_paired)
         mtext(side=3, line=-6, adj=0.03, cex=cex_mtext, outer=FALSE, text=paste("Mean bias (Sat mean = ",format(sat_mean,digits=2),"): ",format(hs_bias,digits=2),sep=''))
         sat_Q95 <- quantile(vec_sat_paired,probs=high_bias)
         #mtext(side=3, line=-6, adj=0.03, cex=cex_mtext, outer=FALSE, text=paste("Q95 bias (Sat Q95 = ",format(sat_Q95,digits=2),"): ",format(hs_high_bias,digits=2),sep=''))
         mtext(side=3, line=-12, adj=0.03, cex=cex_mtext, outer=FALSE, text=paste("Correlation: ",format(hs_cor,digits=3),sep=''))
         mtext(side=3, line=-18, adj=0.03, cex=cex_mtext, outer=FALSE, text=paste("RMSD: ",format(round(hs_rmsd,3),nsmall=2),sep=''))
         mtext(side=3, line=-24, adj=0.03, cex=cex_mtext, outer=FALSE, text=paste("RMSE: ",format(round(hs_rmse,3),nsmall=2),sep=''))
         mtext(side=3, line=-30, adj=0.03, cex=cex_mtext, outer=FALSE, text=paste("N:",sum(!is.na(vec_sat_paired))))
         mtext(side=3, line=-36, adj=0.03, cex=cex_mtext, outer=FALSE, text=paste("Total 1 Hz:",vec_1Hz_count[plot_idx]))
# Legend for red and black dots.
         if ( plot_idx == 1 ) { legend(x=5.5,y=2.0,legend=c("< 25 km from buoy","> 25 km from buoy","Q-Q plot"),col=c("black","red","orange"),pch=c(19,19,19),cex=pl_cex_leg) }
# Legend for red and black dots.
         array_radius_stats[,p_idx,i_buoy_radius] <- c(sat_mean,hs_bias,hs_cor,hs_rmsd,hs_rmse,sum(!is.na(vec_sat_paired)),vec_1Hz_count[plot_idx])
      }

      dev.off()
      system(paste("okular",fig_scatter_adapt_nearest_file_name,"&> /dev/null &"))

   }

#=================================================================================================#
   if ( flag_radius_plot & (i_buoy_radius == length(vec_buoy_radius) ) ) {
# Generate stats directly from df_regi (N and N[1 Hz]).
# Lvec_temp <- !is.na(list_df_reg_radius[[1]]$buoy_hs) & !is.na(list_df_reg_radius[[1]]$sat_hs)
# sum(Lvec_temp)
# sum( list_df_reg_radius[[1]]$sat_1Hz_count[Lvec_temp] )

# Plotting statistics as a function of sampling radius.
# Generate mean bias for each buoy (background lines).
      mat_buoy_bias_rad <- matrix(nrow=length(Bidx_max)+1,ncol=length(vec_buoy_radius))
      for ( i_b_rad in 1:length(vec_buoy_radius) ) {
         vec_sat_rad <- list_df_reg_radius[[i_b_rad]]$sat_hs
         vec_buoy_rad <- list_df_reg_radius[[i_b_rad]]$buoy_hs
         Lvec_paired_rad <- !is.na(vec_buoy_rad) & !is.na(vec_sat_rad)
# Total mean bias (46246 removed).
         Lvec_pair_46246 <- list_df_reg_radius[[i_b_rad]]$buoy_lab[Lvec_paired_rad] != 46246
         #Lvec_pair_46246 <- rep(TRUE,length(list_df_reg_radius[[i_b_rad]]$buoy_lab[Lvec_paired_rad]))
         mat_buoy_bias_rad[length(Bidx_max)+1,i_b_rad] <- mean( (vec_sat_rad[Lvec_paired_rad] - vec_buoy_rad[Lvec_paired_rad])[Lvec_pair_46246] )
# Mean bias conditional on buoy.
         for ( b_idx in 1:length(Bidx_max) ) {
            if ( sum( list_df_reg_radius[[i_b_rad]]$buoy_lab[Lvec_paired_rad] == buoy_list[b_idx_list[Bidx_max[b_idx]]] ) > 0 ) {
               mat_buoy_bias_rad[b_idx,i_b_rad] <- mean( (vec_sat_rad[Lvec_paired_rad] - vec_buoy_rad[Lvec_paired_rad])[ list_df_reg_radius[[i_b_rad]]$buoy_lab[Lvec_paired_rad] == buoy_list[b_idx_list[Bidx_max[b_idx]]] ] )
            }
         }
      }
      colnames(mat_buoy_bias_rad) <- vec_buoy_radius
      rownames(mat_buoy_bias_rad) <- c(buoy_list[b_idx_list[Bidx_max]],"total")

# Plotting parameters.
      pl_mfrow <- c(1,1)
      pl_oma <- c(2,2,2,2)
      pl_mar <- c(12,14,8,12)
      pl_mgp <- c(9,3,0)
      pl_cex <- 4; pl_cex_main <- 6; pl_cex_lab <- 6; pl_cex_axis <- 5; pl_cex_leg <- 5
      #cex_mtext <- 4

      plot_title <- paste0(fig_lab_region," ",vec_tandem_labs[S_idx],"[M",m_limit,"] Mean bias with sampling radius")

      fig_radius_stats_file_name <- paste0("./figures/buoy_mean_bootstrap/meanbias_radius_",fig_lab_region,"_","M",m_limit,"_",vec_tandem_labs[S_idx],".png")

# Open file.
      png(fig_radius_stats_file_name, width = 2400, height = 2400)
      par(mfrow=pl_mfrow,oma=pl_oma,mar=pl_mar,mgp=pl_mgp)
      #X11()
# Create the plot.
      plot(NULL,xlim=c(20,100),ylim=c(0,ceiling( max(array_radius_stats[7,1,],na.rm=T) / 5000 ) * 5000),xlab="Sampling radius (km)",ylab="",main="",axes=FALSE,cex=pl_cex, cex.main=pl_cex_main, cex.lab=pl_cex_lab, cex.axis=pl_cex_axis)
# 1 Hz samples.
      vec_x_samp_1Hz_poly <- c(rev(vec_buoy_radius),vec_buoy_radius)
      vec_y_samp_1Hz_poly <- c(rep(0,length(vec_buoy_radius)),array_radius_stats[7,1,1:length(vec_buoy_radius)])
      polygon(vec_x_samp_1Hz_poly,vec_y_samp_1Hz_poly,border = NA,col="lightsteelblue3")
      text(x=vec_buoy_radius[1], y=array_radius_stats[7,1,1], labels=paste0("N[1Hz] = ",array_radius_stats[7,1,1]), pos=2, cex=pl_cex_axis)
      text(x=rev(vec_buoy_radius)[1], y=array_radius_stats[7,1,length(vec_buoy_radius)], labels=paste0("N[1Hz] = ",array_radius_stats[7,1,length(vec_buoy_radius)]), pos=3, cex=pl_cex_axis)
# N samples.
      vec_x_samp_N_poly <- c(rev(vec_buoy_radius),vec_buoy_radius)
      vec_y_samp_N_poly <- c(rep(0,length(vec_buoy_radius)),array_radius_stats[6,1,1:length(vec_buoy_radius)])
      polygon(vec_x_samp_N_poly,vec_y_samp_N_poly,border = NA,col="steelblue3")
      text(x=vec_buoy_radius[1], y=array_radius_stats[6,1,1], labels=paste0("N = ",array_radius_stats[6,1,1]), pos=2, cex=pl_cex_axis)
      text(x=rev(vec_buoy_radius)[1], y=array_radius_stats[6,1,length(vec_buoy_radius)], labels=paste0("N = ",array_radius_stats[6,1,length(vec_buoy_radius)]), pos=1, cex=pl_cex_axis)
      par(new=TRUE)
# Plot mean bias by buoy.
      plot(NULL,xlim=c(20,100),ylim=c(-0.1,0.1),xlab="",ylab="Hs Mean Bias (m)",cex.lab=pl_cex_lab,cex.axis=pl_cex_axis)
      abline(h=c(-0.02,-0.01,0.01,0.02),col="grey",lwd=3)
      abline(h=0,col="darkgrey",lwd=5)
      for ( b_idx in 1:length(Bidx_max) ) {
         lines(vec_buoy_radius,mat_buoy_bias_rad[b_idx,],lwd=10,col="coral1")
         #points(vec_buoy_radius,mat_buoy_bias_rad[b_idx,],cex=1,col="coral1")
      }      
# Plot mean bias average.
      points(vec_buoy_radius,mat_buoy_bias_rad[length(Bidx_max)+1,],cex=1,col="firebrick2")
      lines(vec_buoy_radius,mat_buoy_bias_rad[length(Bidx_max)+1,],lwd=18,col="firebrick2")
# Legend.
      legend(x=20,y=-0.05, legend=c("Bias (all buoys)","Bias (single buoy)"),lwd=c(18,10),col=c("firebrick2","coral1"),cex=pl_cex_leg)
# Plot mean Hs.
      par(new=TRUE)
      plot(vec_buoy_radius,array_radius_stats[1,1,1:length(vec_buoy_radius)],xlim=c(20,100),ylim=c(0,3),xlab="",ylab="",pch=19,col="blue",cex=5,axes=FALSE)
      lines(c(vec_buoy_radius),array_radius_stats[1,1,1:length(vec_buoy_radius)],pch=19,col="white",cex=3)
      axis(side=4,at=c(2,2.25,2.5,2.75,3,3.25),cex=pl_cex_axis,cex.lab=pl_cex_lab,cex.axis=pl_cex_axis)
      mtext("Mean Hs (m)", side=4, line=8, adj=0.75, cex=pl_cex_axis)

      dev.off()
      system(paste("okular",fig_radius_stats_file_name,"&> /dev/null &"))
   }

# Close buoy radius loop.
   }

#=================================================================================================#
# Boostrap for evaluating sat - buoy mean bias.
# https://stackoverflow.com/questions/23431522/calculating-empirical-probabilities
#-------------------------------------------------------------------------------------------------#
   if ( flag_buoy_bias_hist ) {
      BB <- 0

      vec_buoy_lab <- unique( df_reg$buoy_lab )
      mat_BBB <- matrix(NA,nrow=5000,ncol=length(vec_buoy_lab))

      for ( AA in 1:length(vec_buoy_lab) ) {
# Find actual mean bias per buoy.
         BB[AA] <- mean(df_reg$sat_hs[df_reg$buoy_lab == vec_buoy_lab[AA]] - df_reg$buoy_hs[df_reg$buoy_lab == vec_buoy_lab[AA]],na.rm=T)
# Bootstrap loop.
         n_samples <- sum(df_reg$buoy_lab == vec_buoy_lab[AA])
         n_idx <- dim(df_reg)[1]
         for ( AAA in 1:5000) {
            CCC <- sample(1:n_idx, size = n_samples, replace = TRUE)
            mat_BBB[AAA,AA] <- mean(df_reg$sat_hs[CCC] - df_reg$buoy_hs[CCC],na.rm=T)
         }
      }

# Plotting parameters.
      pl_mfrow <- c(3,3)
      pl_oma <- c(2,2,14,2)
      pl_mar <- c(12,14,8,5)
      pl_mgp <- c(9,3,0)
      pl_cex <- 4; pl_cex_main <- 6; pl_cex_lab <- 6; pl_cex_axis <- 5; pl_cex_leg <- 5
      cex_mtext <- 4
      vec_track_lines <- c(-33,-38,-43,-48)
      vec_buoy_lab_100 <- c(46066,46078,46246,46085,46001,46002,46005,46006,46059)
      Lvec_buoy_plot_idx <- sapply(1:length(vec_buoy_lab_100),function(x) any(vec_buoy_lab == vec_buoy_lab_100[x]))

      plot_title <- paste0(fig_lab_region," ",vec_tandem_labs[S_idx],"[M",m_limit,"] ",buoy_radius,"km Bootstrap sampling distribution for mean bias")

      fig_buoy_mean_bootstrap_file_name <- paste0("./figures/buoy_mean_bootstrap/hist_meanbias_",fig_lab_region,"_","M",m_limit,"_",vec_tandem_labs[S_idx],"_",buoy_radius,"km_",gsub('[.]','',cor_thresh),".png")

# Open file.
      png(fig_buoy_mean_bootstrap_file_name, width = 3000, height = 3000)
      par(mfrow=pl_mfrow,oma=pl_oma,mar=pl_mar,mgp=pl_mgp)
# Plotting.
      #for ( AA in 1:length(vec_buoy_lab) ) {
      AA <- 1
      for ( plot_idx in 1:9 ) {
         if ( Lvec_buoy_plot_idx[plot_idx] ) {
            if ( AA == 1 ) {
               hist(mat_BBB[,AA],xlim=c(-0.15,0.15),ylim=c(0,2000),xlab="Mean bias (m)",main=paste(buoy_list[b_idx_list[Bidx]][AA]),cex=pl_cex, cex.main=pl_cex_main, cex.lab=pl_cex_lab, cex.axis=pl_cex_axis)
# Legend.
               legend(x=-0.02,y=1650,legend="Obs mean bias",col="blue",cex=5,lwd=5)
	    } else {
               hist(mat_BBB[,AA],xlim=c(-0.15,0.15),ylim=c(0,2000),xlab="",ylab="",main=paste(buoy_list[b_idx_list[Bidx]][AA]),cex=pl_cex, cex.main=pl_cex_main, cex.lab=pl_cex_lab, cex.axis=pl_cex_axis)
	    }
            abline(v=BB[AA],col="blue",lwd=5)
# Stats.
            mtext(side=3, line=-3, adj=0.06, cex=cex_mtext, outer=FALSE, text=paste("N:",sum(df_reg$buoy_lab == vec_buoy_lab[AA])))
            mtext(side=3, line=-8, adj=0.09, cex=cex_mtext, outer=FALSE, text=paste("2 SD:",format(round(sqrt(var(mat_BBB[,AA])),3),nsmall=2),"(m)"))
            mtext(side=3, line=-13, adj=0.1, cex=cex_mtext, outer=FALSE, text=paste("P[X<=Obs]:",format(round(ecdf(mat_BBB[,AA])( BB[AA] ),3),nsmall=3)))
            mtext(side=3, line=-28, adj=0.0, cex=cex_mtext, outer=FALSE, text=paste("Local tracks:"))
# Track info.
            for ( track_dir_idx in 1:length(unique( df_reg$track_dist[df_reg$buoy_lab == vec_buoy_lab[AA]] ))) {
               mtext(side=3, line=vec_track_lines[track_dir_idx], adj=0.0, cex=cex_mtext, outer=FALSE, text=paste0( unique( df_reg$track_dir[df_reg$buoy_lab == vec_buoy_lab[AA]] )[track_dir_idx],": ", format( round( unique( df_reg$track_dist[df_reg$buoy_lab == vec_buoy_lab[AA]] )[track_dir_idx] ) )," km" ))
            }
            AA <- AA + 1
         } else {
# Plot a blank panel.	 
            plot(NULL,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",axes=FALSE)
         }
      }
      mtext(side=3, line=5, adj=0.5, cex=6, outer=TRUE, text=plot_title)

      dev.off()
      system(paste("okular",fig_buoy_mean_bootstrap_file_name,"&> /dev/null &"))
   }
   #unique( df_reg$track_dist[df_reg$buoy_lab == 46066] )

#=================================================================================================#
# Algorithm to match up J3, tandem data but using the buoy measurements in df_reg.
# Remove NAs.
   df_reg_J3 <- list_df_reg_radius[[1,1]]
   df_reg_S6LRM <- list_df_reg_radius[[1,2]]
   mat_match_idx <- matrix(NA,nrow=length(df_reg_J3$buoy_hs),2)
   if ( any(df_reg_J3$buoy_hs[1] == df_reg_S6LRM$buoy_hs[1:10],na.rm=T) ) {
      mat_match_idx[1,1] <- 1
      mat_match_idx[1,2] <- which( df_reg_J3$buoy_hs[1] == df_reg_S6LRM$buoy_hs[1:10] )
   } else if ( any(df_reg_S6LRM$buoy_hs[1] == df_reg_J3$buoy_hs[1:10],na.rm=T) ) {
      mat_match_idx[1,1] <- which( df_reg_S6LRM$buoy_hs[1] == df_reg_J3$buoy_hs[1:10] )
      mat_match_idx[1,2] <- 1
   } else {
      print(paste("ERROR!"))
   }

   i_index <- 2
   gap_count <- 0
   for ( match_idx in (1+mat_match_idx[1,1]):length(df_reg_J3$buoy_hs) ) {
   #for ( match_idx in (1+mat_match_idx[1,1]):100 ) {
      if ( !is.na(df_reg_J3$buoy_hs[match_idx]) ) {
         if ( gap_count > 0 ) {
            Lvec_test <- df_reg_J3$buoy_hs[match_idx] == df_reg_S6LRM$buoy_hs[ (match_idx-mat_match_idx[i_index-1,2]-3)+(mat_match_idx[i_index-1,2]:(mat_match_idx[i_index-1,2]+2)) ]
            if ( any(Lvec_test,na.rm=T) ) {
               mat_match_idx[i_index,2] <- which( df_reg_J3$buoy_hs[match_idx] == df_reg_S6LRM$buoy_hs[ (match_idx-mat_match_idx[i_index-1,2]-3)+(mat_match_idx[i_index-1,2]:(mat_match_idx[i_index-1,2]+2)) ] ) + mat_match_idx[i_index-1,2] - 1 + (match_idx-mat_match_idx[i_index-1,2]-3)
               mat_match_idx[i_index,1] <- match_idx
               i_index <- i_index + 1
               gap_count <- 0
            } else {
               gap_count <- gap_count + 1
            }
         } else {
            Lvec_test <- df_reg_J3$buoy_hs[match_idx] == df_reg_S6LRM$buoy_hs[ (mat_match_idx[i_index-1,2]+1):(mat_match_idx[i_index-1,2]+1) ]
            if ( any(Lvec_test,na.rm=T) ) {
               #mat_match_idx[i_index,2] <- which( df_reg_J3$buoy_hs[match_idx] == df_reg_S6LRM$buoy_hs[ mat_match_idx[i_index-1,2]:(mat_match_idx[i_index-1,2]+2) ] ) + mat_match_idx[i_index-1,2] - 1
               mat_match_idx[i_index,2] <- which( df_reg_J3$buoy_hs[match_idx] == df_reg_S6LRM$buoy_hs[ (mat_match_idx[i_index-1,2]+1):(mat_match_idx[i_index-1,2]+1) ] ) + mat_match_idx[i_index-1,2]
               mat_match_idx[i_index,1] <- match_idx
               i_index <- i_index + 1
               gap_count <- 0
            } else {
               gap_count <- gap_count + 1
            }
         }
      } else {
         gap_count <- gap_count + 1
      }
   }
   X11(); plot( df_reg_J3$buoy_hs[mat_match_idx[,1]], df_reg_S6LRM$buoy_hs[mat_match_idx[,2]] ); abline(0,1)
   X11(); plot( df_reg_J3$sat_hs[mat_match_idx[,1]], df_reg_S6LRM$sat_hs[mat_match_idx[,2]] ); abline(0,1)
   mean(df_reg_S6LRM$sat_hs[mat_match_idx[,2]],na.rm=T) - mean(df_reg_J3$sat_hs[mat_match_idx[,1]],na.rm=T)

#=================================================================================================#
# ggplot.
#-------------------------------------------------------------------------------------------------#
   if ( flag_plot_ggmap ) {
# Sampling radius.
# https://stackoverflow.com/questions/34183049/plot-circle-with-a-certain-radius-around-point-on-a-map-in-ggplot2
      func_make_circles <- function(centers, radius, nPoints = 100) {
         # centers: the data frame of centers with ID
         # radius: radius measured in kilometer
         #
         meanLat <- mean(centers$latitude)
         # length per longitude changes with lattitude, so need correction
         radiusLon <- radius / 111 / cos(meanLat/57.3)
         radiusLat <- radius / 111
         circleDF <- data.frame(buoy_ID = rep(centers$buoy_ID, each = nPoints))
         angle <- seq(0,2*pi,length.out = nPoints)

         circleDF$lon <- unlist(lapply(centers$longitude, function(x) x + radiusLon * cos(angle)))
         circleDF$lat <- unlist(lapply(centers$latitude, function(x) x + radiusLat * sin(angle)))
         return(circleDF)
      }
# Circle for sampling radius, 25, 50 km.
      df_circles <- NULL
      df_circles_25 <- NULL
      df_circles_50 <- NULL
      for (b_idx in 1:length(Bidx)) {
         df_buoy_test <- data.frame(buoy_ID=buoy_list[b_idx_list[Bidx[b_idx]]],df_buoy_data[b_idx_list[Bidx[b_idx]],c(1,2)])
         colnames(df_buoy_test) <- c("buoy_ID","latitude","longitude")
         df_circles <- rbind(df_circles,func_make_circles(df_buoy_test, buoy_radius))
         df_circles_25 <- rbind(df_circles_25,func_make_circles(df_buoy_test, 25))
         df_circles_50 <- rbind(df_circles_50,func_make_circles(df_buoy_test, 50))
      }

#-------------------------------------------------------------------------------------------------#
      #vec_lon <- unlist( sapply(X=1:60,FUN=function(x) { unlist(mat_list_J3[[x,buoy_idx]][[3]]) }) )
      #vec_lat <- unlist( sapply(X=1:60,FUN=function(x) { unlist(mat_list_J3[[x,buoy_idx]][[2]]) }) )
      #df_plot <- data.frame(lon_sat=vec_lon-360,lat_sat=vec_lat)
      #df_plot <- data.frame(lon_sat=c(vec_lon[which.min(vec_lon)],vec_lon[which.max(vec_lon)])-360,lat_sat=c(vec_lat[which.min(vec_lon)],vec_lat[which.max(vec_lon)]))
# Data frame for idealised J3 tracks (from linear model).
      df_plot_J3 <- NULL
      for (b_idx in 1:length(Bidx)) {
         for (kk in 1:length( list_B_list_tr_lm_seq[[b_idx]] )) {
            df_plot_J3 <- rbind(df_plot_J3,data.frame(pred_lon=list_B_list_tr_lm_seq[[b_idx]][[kk]][,1]-360,pred_lat=list_B_list_tr_lm_seq[[b_idx]][[kk]][,2],tr_id=paste0(buoy_list[b_idx_list[b_idx]],"_",kk)))
         }
      }
# Data frame for sat bin statistics.
      df_plot_cor <- NULL
      for (b_idx in 1:length(Bidx)) {
         for (kk in 1:length( list_B_list_tr_lm_seq[[b_idx]] )) {
            plot_angle <- 1.5*(180/pi)*atan( (list_B_list_tr_lm_seq[[b_idx]][[kk]][1001,2] - list_B_list_tr_lm_seq[[b_idx]][[kk]][1,2]) / (list_B_list_tr_lm_seq[[b_idx]][[kk]][1001,1] - list_B_list_tr_lm_seq[[b_idx]][[kk]][1,1]) )

            for (season_idx in vec_season) {
               if ( !all( is.na( mat_list_cor[[S_idx,b_idx]][[season_idx]][[kk]] ) ) ) {
                  if ( flag_plot_ggmap_COR ) {
# Correlation plot.
                     df_DD <- data.frame(vec_dist_bins_centre,cor=mat_list_cor[[S_idx,b_idx]][[season_idx]][[kk]],lon=NA,lat=NA,season=lab_season[season_idx],plot_angle=plot_angle)
                  } else {
# Bias plot. Loop over seasons.
                     df_DD <- data.frame(vec_dist_bins_centre,cor=mat_list_bias[[S_idx,b_idx]][[season_idx]][[kk]],lon=NA,lat=NA,season=lab_season[season_idx],plot_angle=plot_angle)
                  }
                  temp_idx <- which( df_DD[,1] == mat_list_bin_coords[[S_idx,b_idx]][[kk]][1,1] )
                  df_DD[temp_idx:(temp_idx-1+length(mat_list_bin_coords[[S_idx,b_idx]][[kk]][,3])),3:4] <- mat_list_bin_coords[[S_idx,b_idx]][[kk]][,2:3]
                  df_plot_cor <- rbind( df_plot_cor,cbind(df_DD[!is.na(df_DD$cor),],tr_id=paste0(buoy_list[b_idx_list[b_idx]],"_",kk)) )
               } else {
                  print(paste("Buoy ID:",buoy_idx,"[",buoy_list[buoy_idx],"] No correlation data for track ID:",kk))
               }
            }
         }
      }
      df_plot_cor$lon <- df_plot_cor$lon-360
#-------------------------------------------------------------------------------------------------#
# ggmap plot showing bin correlations (or other statistics).
#-------------------------------------------------------------------------------------------------#
      require(ggplot2)
      require(viridis)
#-------------------------------------------------------------------------------------------------#
# Fix to saturate colour scale below correlation = 0.85
      if ( flag_plot_ggmap_COR ) {
# Colour scale.
         df_plot_cor$cor[df_plot_cor$cor < 0.85] <- 0.85
         ggplot_colour_scale <- scale_colour_viridis_c(option = "plasma")
         legend_title <- "Cor"
         fig_file_name <- paste0("./figures/test_sampling3/",buoy_list[buoy_idx],"/ggmap_","M",m_limit,"_",vec_tandem_labs[S_idx],"_",buoy_list[buoy_idx],"_",buoy_radius,"km_season",paste(vec_season,collapse=""),"_COR.png")
         system(paste0("if [ ! -d ./figures/test_sampling3/",buoy_list[buoy_idx]," ]; then mkdir ./figures/test_sampling3/",buoy_list[buoy_idx]," &> /dev/null; fi"))
      } else {
# Colour scale.
         ggplot_colour_scale <- scale_colour_gradient2(low = "blue", mid="white", high = "red")
         legend_title <- "Bias (m)"
         fig_file_name <- paste0("./figures/test_sampling3/",buoy_list[buoy_idx],"/ggmap_","M",m_limit,"_",vec_tandem_labs[S_idx],"_",buoy_list[buoy_idx],"_",buoy_radius,"km_season",paste(vec_season,collapse=""),"_BIAS.png")
         system(paste0("if [ ! -d ./figures/test_sampling3/",buoy_list[buoy_idx]," ]; then mkdir ./figures/test_sampling3/",buoy_list[buoy_idx]," &> /dev/null; fi"))
      }
#-------------------------------------------------------------------------------------------------#
# Load ggmap.
      require(ggmap)
      #register_google(key = "XXX", write = TRUE)
      #https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
# Using Google Maps.
      if ( flag_OS ) {
         if ( length(Bidx < 3) ) {
            ggplot_zoom <- 7
         } else {
            ggplot_zoom <- 5
         }
      } else {
         if ( length(Bidx < 3) ) {
            ggplot_zoom <- 7
         } else {
            ggplot_zoom <- 5
         }
      }
      ggmap_lon <- range(df_buoy_data[b_idx_list[Bidx],2])[1] + ( range(df_buoy_data[b_idx_list[Bidx],2])[2] - range(df_buoy_data[b_idx_list[Bidx],2])[1] ) / 2
      ggmap_lat <- range(df_buoy_data[b_idx_list[Bidx],1])[1] + ( range(df_buoy_data[b_idx_list[Bidx],1])[2] - range(df_buoy_data[b_idx_list[Bidx],1])[1] ) / 2
      map1 <- get_map(location = c(lon = ggmap_lon, lat = ggmap_lat), zoom = ggplot_zoom, maptype = "satellite")
# Add transparency.
# https://stackoverflow.com/questions/38126102/set-opacity-of-background-map-with-ggmap
      map1_attributes <- attributes(map1)
      map1_transparent <- matrix(adjustcolor(map1,alpha.f = 0.6),nrow = nrow(map1))
      attributes(map1_transparent) <- map1_attributes
#-------------------------------------------------------------------------------------------------#
# Create plot.
      p2 <- ggmap(map1_transparent) + 
# Buoy symbol(s).
         #geom_point(aes(x=df_buoy_data$buoy_lon[buoy_idx], y=df_buoy_data$buoy_lat[buoy_idx], col="yellow"), colour="black", fill="yellow", size=20, shape=23, stroke=5) +
         #geom_label(aes(x=df_buoy_data$buoy_lon[buoy_idx], y=df_buoy_data$buoy_lat[buoy_idx], label=paste(buoy_list[buoy_idx])), label.padding = unit(0.30, "lines"), size = 20, nudge_x = 1.4, nudge_y = 0.7) +
         geom_point(data=df_buoy_data[b_idx_list[Bidx],],aes(x=buoy_lon, y=buoy_lat), colour="black", fill="yellow", shape=23, size=20, stroke=5) +
# Colour scale.
         ggplot_colour_scale +
# Sampling radius.
         geom_polygon(data = df_circles, aes(lon, lat, group = buoy_ID), col="black", linewidth=3, alpha = 0) +
         geom_polygon(data = df_circles_25, aes(lon, lat, group = buoy_ID), col="grey", linewidth=2, alpha = 0) +
         geom_polygon(data = df_circles_50, aes(lon, lat, group = buoy_ID), col="grey", linewidth=2, alpha = 0) +
# Satellite tracks.
         geom_point(data=df_plot_J3, aes(x=pred_lon, y=pred_lat), col="white", size=1, shape=0) +
# Bin correlation points.
         #geom_point(data=df_plot_cor, aes(x=lon, y=lat, col=cor), fill="black", shape=1, size=10, stroke=5) +
         geom_text(data=df_plot_cor, label = "\u25AE", size=18, aes(x=lon, y=lat, col=cor, angle=plot_angle)) +
         facet_wrap(~season, ncol=1) +
# Buoy label.
         geom_label(aes(x=df_buoy_data$buoy_lon[b_idx_list[Bidx]], y=df_buoy_data$buoy_lat[b_idx_list[Bidx]], label=paste(buoy_list[buoy_idx])), label.padding = unit(0.30, "lines"), size = 20, nudge_x = 1.5, nudge_y = 0.7) +
# ERA5 grid cells.
         #geom_point(data = df_plot_ERA5, aes(x=ERA5_lon, y=ERA5_lat, col="4) ERA5 (0.5 deg)"), size=18, shape=10, stroke=3) +
         #geom_segment(data = df_plot_ERA5_box1, aes(x=lon_1, y=lat_1, xend=lon_2, yend=lat_2, col="5) ERA5 bilinear"), linewidth=2) +
# Title.
	 ggtitle(paste0(buoy_list[buoy_idx],"; ",buoy_radius," km sampling; ",dist_bin_width," km bin size")) +
# Theme stuff.
         theme(panel.background = element_rect(fill = "gray30"),
               panel.grid.major = element_line(linewidth = 2),
               plot.title = element_text(size = 70,hjust = 0.5),
               axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               axis.text.x = element_text(size = 40),
               axis.text.y = element_text(size = 40),
               legend.text = element_text(size = 40,margin = margin(b = 10, t = 10, unit = "pt")),
               #legend.title = element_blank() ) + labs(fill = "50km") +
               legend.title = element_text(size = 50),

               strip.text = element_text(size = 60, margin = margin(25,0,25,0)),
               strip.background = element_rect(fill = "white"),
               panel.spacing.x = unit(1, "lines"),
               panel.spacing.y = unit(2, "lines") ) +
         labs(colour = legend_title) +
         guides(colour=guide_colourbar(barwidth = 5, barheight = 20))
         #guides(color=guide_legend(override.aes=list(shape=c(1),linetype=c(0),size=c(5))))
#         guides(fill=guide_legend(reverse=TRUE)) +

      #mat_lay <- cbind(c(1,1,1),matrix(rep(2,9),ncol=3))
      png(fig_file_name,width=1500,height=1400*length(vec_season))
      #pdf(fig_file_name,width = 8, height = 9)
      #grid.arrange(p2,p1,layout_matrix=mat_lay)
      plot(p2)
      dev.off()
      system(paste("okular",fig_file_name,"&> /dev/null &"))
   }

#-------------------------------------------------------------------------------------------------#
## Code for looking at differences between along-track noise.
#   #AA <- mat_list_Lvec_QC[[m_idx,S_idx]][mat_list_trackID[[m_idx,1]][[1]]]
#   #mat_list_1Hz_hs[[m_idx,S_idx]][mat_list_trackID[[m_idx,1]][[1]]][AA]
#   #mat_list_1Hz_rms[[m_idx,S_idx]][mat_list_trackID[[m_idx,1]][[1]]][AA]
#
#   S_idx <- 1
#   mat_list_trackID <- array_list_trackID[[S_idx,1]]
#   AA <- mat_list_1Hz_hs[[m_idx,S_idx]][mat_list_trackID[[m_idx,1]][[1]]]
#
#   S_idx <- 2
#   mat_list_trackID <- array_list_trackID[[S_idx,1]]
#   BB <- mat_list_1Hz_hs[[m_idx,S_idx]][mat_list_trackID[[m_idx,1]][[1]]]
#
#   X11(); plot(AA,BB); abline(0,1)

#   X11(); plot(mat_list_1Hz_rms[[2,1]],ylim=c(0,1)); lines(mat_list_1Hz_rms[[2,1]])
#   points(mat_list_1Hz_rms[[2,2]],pch=19); lines(mat_list_1Hz_rms[[2,2]])

## Plot this for 46078 [Bidx = 2].
#   ZZ <- 25; for ( jj in 2:6 ) { ZZ[jj] <- which(mat_list_1Hz_time[[2,1]][1:(274 - jj)] - mat_list_1Hz_time[[2,2]][1:273][-ZZ] > 0 )[1] }
#   ZZ <- ZZ[!is.na(ZZ)]
#
#   X11(); plot(mat_list_1Hz_rms[[2,1]]-mat_list_1Hz_rms[[2,2]][1:278][-ZZ],ylim=c(-1,1))
#   abline( v=sapply(X=1:length(mat_list_breaks_master[[2,1]]),FUN=function(x){ mat_list_breaks_master[[2,1]][[x]][length(mat_list_breaks_master[[2,1]][[x]])] }) )
#   par(new=T)
#   plot(mat_list_1Hz_hs[[2,1]],ylim=c(0,12),pch=".")
#   lines(mat_list_1Hz_hs[[2,1]],col="red",lwd=1.5)
#   lines(mat_list_1Hz_hs[[2,2]][1:278][-ZZ],col="blue",lwd=1.5)

