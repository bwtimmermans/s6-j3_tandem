# source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/coloc_test_sampling.R")
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
   flag_multi_scatter <- FALSE
   flag_coloc <- FALSE
   flag_plot_junk <- FALSE

# Sea state sampling by period.
   flag_period_thresh <- FALSE
   flag_swell_only <- FALSE
   period_thresh <- 8

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

   lab_month <- c("Dec (2020)","Jan (2021)","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

   buoy_radius <- 50

# Process sat data.
   vec_Q50_J3_ALL <- vec_Q50_S6_SAR_ALL <- vec_Q50_S6_LRM_ALL1 <- vec_buoy_hs_coloc_ALL <- vec_buoy_ap_coloc_ALL <- NULL
   vec_ERA5_hs_coloc_ALL <- vec_ERA5_hs_BILIN_coloc_ALL <- vec_ERA5_ap_coloc_ALL <- NULL
   Lvec_qual_J3_ALL <- Lvec_qual_S6_SAR_ALL <- Lvec_qual_S6_LRM_ALL <- Lvec_qual_numval_J3_ALL <- NULL

# Some set of buoys (mostly, all of them).
#   b_idx_list <- 1:length(buoy_list)
## Offshore (active).
#   array_mean_e <- array(NA,dim=c(3,3,13))
#   rownames(array_mean_e) <- c("","ERA5","buoy")
#   colnames(array_mean_e) <- c("J-3","S-6_LRM","S-6_SAR")

   #JJ <- 1
   #for ( JJ in 1:13 ) {
   #b_idx_list <- (1:13)[-JJ]

   list_1Hz_distance_distn <- vector(mode = "list",length = length(b_idx_list))
   #X11()
   #par(mfrow=c(1,1))

   #for (b_idx in 1:length(b_idx_list)) {
   #for (b_idx in 1:15) {
   for (b_idx in 5) {

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
      if ( !is.na(date_start_idx) ) {
         date_idx <- date_start_idx:length(vec_buoy_time)

         vec_buoy_time_num <- as.numeric( vec_buoy_time[date_idx] ) - 946684800
         vec_buoy_hs <- mat_buoy_csv$hs[date_idx]
         vec_buoy_ap <- mat_buoy_csv$ap[date_idx]

#-------------------------------------------------------------------------------------------------#
## OLD CODE
## Sample Hs within ~50 km radius of buoy.
#         path_buoy_meta <- paste("/home/ben/research/waves/buoy_data/NDBC_metadata/",buoy_list[buoy_idx],"_meta",sep="")
#         if ( buoy_list[buoy_idx] == 41113 | buoy_list[buoy_idx] == 46246 ) {
#            com_b_lat <- paste("sed -ne '4p' ",path_buoy_meta," | cut -f1 -d' '",sep="")
#            com_b_lon <- paste("sed -ne '4p' ",path_buoy_meta," | cut -f3 -d' '",sep="")
#         } else {
#            com_b_lat <- paste("sed -ne '5p' ",path_buoy_meta," | cut -f1 -d' '",sep="")
#            com_b_lon <- paste("sed -ne '5p' ",path_buoy_meta," | cut -f3 -d' '",sep="")
#         }
#         df_buoy_loc <- data.frame(lat=as.numeric(system(com_b_lat,intern = TRUE)), lon=-as.numeric(system(com_b_lon,intern = TRUE)), name=buoy_list[buoy_idx])

# Code to approximately convert degrees to km.
# Radius of the Earth in km.
      i_radius = 6371
# Function for radians.
      func_rads <- function(x) { x * pi / 180 }
# Function for distance.
      func_buoy_dist <- function(x,B_idx) {
         fl_d_lat = func_rads(x[1]) - func_rads(df_buoy_data$buoy_lat[B_idx])
         fl_d_lon = func_rads(x[2]) - func_rads(df_buoy_data$buoy_lon[B_idx])
         fl_h = sin(fl_d_lat / 2) * sin(fl_d_lat / 2) + cos( func_rads(df_buoy_data$buoy_lat[B_idx]) ) * cos( func_rads(x[1]) ) * sin(fl_d_lon / 2) * sin(fl_d_lon / 2)
         #fl_h = sin(fl_d_lat / 2) * sin(fl_d_lat / 2) + cos( func_rads(5.0) ) * cos( func_rads(x[1]) ) * sin(fl_d_lon / 2) * sin(fl_d_lon / 2)
         #fl_h = 2*asin(sqrt((sin((fl_d_lat)/2))^2 + cos(func_rads(x[1]))*cos(func_rads(df_buoy_data$buoy_lat[B_idx]))*(sin((fl_d_lon)/2))^2))
         2 * i_radius * asin(sqrt(fl_h))
      }

## Function for 100 km distance at 5,5 (Lon, Lat).
#      func_buoy_dist2 <- function(x) {
#         fl_d_lat = func_rads(x[1]) - func_rads(5.0)
#         fl_d_lon = func_rads(x[2]) - func_rads(5.0)
#         fl_h = sin(fl_d_lat / 2) * sin(fl_d_lat / 2) + cos( func_rads(5.0) ) * cos( func_rads(x[1]) ) * sin(fl_d_lon / 2) * sin(fl_d_lon / 2)
#         2 * i_radius * asin(sqrt(fl_h))
#      }
#      AA <- expand.grid((1:100)/10,(1:100)/10)
#      BB <- apply(X=AA,MAR=1,FUN=func_buoy_dist2)
#      X11(); plot(AA); points(AA[CC,],pch=19,col="red")

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

# Longitude.
      #for ( i_month in 1 ) {
      for ( i_month in 1:dim(mat_list_ERA5)[1]) {
         if ( df_buoy_data$buoy_lon[ERA5_b_idx] < (mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lon_mid[2]-360) ) {
            vec_X <- mat_list_ERA5[[i_month,ERA5_b_idx]][[1]]$buoy_lon_mid[1:2]-360
            mat_QX <- t(mat_list_ERA5[[i_month,ERA5_b_idx]][[5]][[1]][1:2,2,])
         } else {
            vec_X <- mat_list_ERA5[[i_month,ERA5_b_idx]][[1]]$buoy_lon_mid[2:3]-360
            mat_QX <- t(mat_list_ERA5[[i_month,ERA5_b_idx]][[5]][[1]][2:3,2,])
         }
# Latitude.
         if ( df_buoy_data$buoy_lat[ERA5_b_idx] < mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lat_mid[2] ) {
            vec_Y <- mat_list_ERA5[[i_month,ERA5_b_idx]][[1]]$buoy_lat_mid[3:2]
            mat_QY <- t(mat_list_ERA5[[i_month,ERA5_b_idx]][[5]][[1]][2:3,1,])
         } else {
            vec_Y <- mat_list_ERA5[[i_month,ERA5_b_idx]][[1]]$buoy_lat_mid[2:1]
            mat_QY <- t(mat_list_ERA5[[i_month,ERA5_b_idx]][[5]][[1]][1:2,1,])
         }
         vec_ERA5_hs_BILIN <- c( vec_ERA5_hs_BILIN,sapply( X=1:length(mat_list_ERA5[[i_month,ERA5_b_idx]][[5]][[1]][2,2,]), FUN=function(x,y,t) { 1/(0.5*0.5) * cbind(c(vec_X[2]-x),c(x-vec_X[1])) %*% cbind(mat_QX[t,],mat_QY[t,]) %*% rbind(c(vec_Y[2]-y),c(y-vec_Y[1])) },x=df_buoy_data$buoy_lon[ERA5_b_idx], y=df_buoy_data$buoy_lat[ERA5_b_idx] ) )
      }

#=================================================================================================#
# Process satellite data.
#   vec_Q50_J3_ALL <- vec_Q50_S6_SAR_ALL <- vec_Q50_S6_LRM_ALL1 <- vec_buoy_hs_coloc_ALL <- vec_buoy_ap_coloc_ALL <- NULL
#   Lvec_qual_J3_ALL <- Lvec_qual_S6_SAR_ALL <- Lvec_qual_S6_LRM_ALL <- NULL
## Loop over months 2021 (Jan - Dec).
#   X11()
#   par(mfrow=c(3,4))

   mat_list_1Hz_dist <- matrix(list(),nrow=13,ncol=3)
   mat_list_Lvec_buoy_samp <- matrix(list(),nrow=13,ncol=3)
   mat_list_buoy_data1 <- matrix(list(),nrow=13,ncol=3)
   mat_list_breaks_master <- matrix(list(),nrow=13,ncol=3)
   mat_list_mean_time <- matrix(list(),nrow=13,ncol=3)
   mat_list_Xing <- matrix(list(),nrow=13,ncol=3)
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

   for ( m_idx in 1:13 ) {
      list_buoy_data1 <- list(mat_list_J3[[m_idx,buoy_idx]],mat_list_S6_LRM[[m_idx,buoy_idx]],mat_list_S6_SAR[[m_idx,buoy_idx]])
      mat_list_buoy_data1[m_idx,] <- list(mat_list_J3[[m_idx,buoy_idx]],mat_list_S6_LRM[[m_idx,buoy_idx]],mat_list_S6_SAR[[m_idx,buoy_idx]])

#-------------------------------------------------------------------------------------------------#
# Find distance from buoy. Loop over data sets.
      list_breaks_master <- vector(mode = "list",length = 3)
      list_Lvec_buoy_samp <- vector(mode = "list",length = 3)

      #for (S_idx in 1:3) {
      for (S_idx in 1) {
         mat_list_1Hz_dist[[m_idx,S_idx]] <- apply(X=cbind(list_buoy_data1[[S_idx]][[2]],list_buoy_data1[[S_idx]][[3]]-360),MAR=1,FUN=func_buoy_dist,B_idx=buoy_idx)

         mat_list_Lvec_buoy_samp[[m_idx,S_idx]] <- list_Lvec_buoy_samp[[S_idx]] <- mat_list_1Hz_dist[[m_idx,S_idx]] < buoy_radius
         nc1_time_idx_cell <- list_buoy_data1[[S_idx]][[4]][list_Lvec_buoy_samp[[S_idx]]]
         nc1_breaks <- NULL
         for (i in 2:length(nc1_time_idx_cell)) {
            #print(paste(" Difference:",( nc1_time_idx_cell[i] - nc1_time_idx_cell[i-1] )))
            if ( abs( nc1_time_idx_cell[i] - nc1_time_idx_cell[i-1] ) > 2 ) {
               nc1_breaks <- c(nc1_breaks,i)
               #print(paste(" Break before:",i))
            }
         }
         mat_nc1_breaks <- cbind(c(1,nc1_breaks),c(nc1_breaks-1,length(nc1_time_idx_cell)))
         list_nc1_breaks_temp <- list()
         for (i in 1:dim(mat_nc1_breaks)[1]) { list_nc1_breaks_temp[[i]] <- mat_nc1_breaks[i,1]:mat_nc1_breaks[i,2] }
         list_nc1_breaks <- list_nc1_breaks_temp[ sapply(X=1:length(list_nc1_breaks_temp),FUN=function(x) { length(list_nc1_breaks_temp[[x]]) > 3 }) ]
         mat_list_breaks_master[[m_idx,S_idx]] <- list_nc1_breaks
          
         #mat_list_min_dist[[m_idx,S_idx]] <- lapply( X=1:length(list_nc1_breaks), FUN=function(x) { min( mat_list_1Hz_dist[[m_idx,S_idx]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][ list_nc1_breaks[[x]] ] ) } )
         #mat_list_min_dist[[m_idx,S_idx]][ which( mat_list_1Hz_dist[[1,1]] == min( mat_list_1Hz_dist[[1,1]] ) ) ] <- TRUE

         list_break_dist              <- lapply( X=1:length(list_nc1_breaks), FUN=function(x) { mat_list_1Hz_dist[[m_idx,S_idx]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]][ list_nc1_breaks[[x]] ] } )
         list_break_dist_min          <- lapply( X=1:length(list_nc1_breaks), FUN=function(x) { min( list_break_dist[[x]] ) } )
         list_break_dist_min_idx      <- lapply( X=1:length(list_nc1_breaks), FUN=function(x) { AA <- which( list_break_dist[[x]] == list_break_dist_min[[x]] ) } )
         #list_break_dist_min_idx      <- lapply( X=1:length(list_nc1_breaks), FUN=function(x) { AA <- which( list_break_dist[[x]] == list_break_dist_min[[x]] ); if ( length(list_break_dist[[x]]) > 2 ) { BB <- c(AA-1,AA,AA+1) }; BB } )
         mat_list_Lvec_break_dist_min_idx[[m_idx,S_idx]] <- lapply( X=1:length(list_nc1_breaks), FUN=function(x) { AA <- rep(FALSE,length(list_nc1_breaks[[x]])); AA[list_break_dist_min_idx[[x]]] <- TRUE; AA } )

         mat_list_mean_time[[m_idx,S_idx]] <- sapply( X=1:length(list_nc1_breaks), FUN=function(x) { floor( mean( nc1_time_idx_cell[ list_nc1_breaks[[x]] ] ) ) } )
# Label specific track segments ([A]scendig, [D]escending).
         mat_list_Xing[[m_idx,S_idx]] <- sapply( X=1:length(list_nc1_breaks), FUN=function(x) { N12 <- list_buoy_data1[[S_idx]][[2]][list_Lvec_buoy_samp[[S_idx]]][ list_nc1_breaks[[x]] ][1:2]; if ( N12[2] > N12[1] ) { "A" } else { "D" } } )
# Find unqiue (first) lat point for each repeated segment (max. 4?).
         vec_Xing_temp <- numeric(length(mat_list_Xing[[m_idx,S_idx]]))
         if ( any( mat_list_Xing[[m_idx,S_idx]] == "A" ) ) {
            vec_Xing_temp[ mat_list_Xing[[m_idx,S_idx]] == "A" ] <- sapply( X=1:length(list_nc1_breaks[ mat_list_Xing[[m_idx,S_idx]] == "A" ]), FUN=function(x) { list_buoy_data1[[S_idx]][[2]][list_Lvec_buoy_samp[[S_idx]]][list_nc1_breaks[ mat_list_Xing[[m_idx,S_idx]] == "A" ][[x]][1]] } )
	 }
         if ( any( mat_list_Xing[[m_idx,S_idx]] == "D" ) ) {
            vec_Xing_temp[ mat_list_Xing[[m_idx,S_idx]] == "D" ] <- sapply( X=1:length(list_nc1_breaks[ mat_list_Xing[[m_idx,S_idx]] == "D" ]), FUN=function(x) { list_buoy_data1[[S_idx]][[2]][list_Lvec_buoy_samp[[S_idx]]][list_nc1_breaks[ mat_list_Xing[[m_idx,S_idx]] == "D" ][[x]][1]] } )
	 }
# First two months required to acquire all unique segments (because Dec 2020 is partial).
# This does not work very well when there are "glancing" tracks, with only a few sampled points.
         if ( m_idx == 1 ) {
            vec_Xing_lat1 <- NULL
            vec_Xing_lat1[1] <- vec_Xing_temp[1]
            #if ( length(vec_Xing_temp) > 1 ) {
            #   while( all( abs( vec_Xing_temp[jj] - vec_Xing_lat1 ) > 0.05 ) ) { vec_Xing_lat1[jj] <- vec_Xing_temp[jj]; jj <- jj+1 }
            #}
            jj <- 2
            if ( length(vec_Xing_temp) > 1 ) {
               for ( jjj in 2:length(vec_Xing_temp) ) { if ( all( abs( vec_Xing_temp[jjj] - vec_Xing_lat1 ) > 0.05 ) ) { vec_Xing_lat1[jj] <- vec_Xing_temp[jjj]; jj <- jj+1 } }
            }
         } else if ( m_idx == 2 ) {
            ii <- 1
            while( all( abs( vec_Xing_temp[ii] - vec_Xing_lat1 ) > 0.05 ) ) { vec_Xing_lat1[jj] <- vec_Xing_temp[ii]; jj <- jj+1; ii <- ii+1 }
         }
# Match first lat point for each segment against unique lat points.
         vec_XXing_temp <- numeric(length(mat_list_Xing[[m_idx,S_idx]]))
         for ( ii in 1:length(vec_Xing_lat1) ) {
            vec_XXing_temp[abs( vec_Xing_temp - vec_Xing_lat1[ii] ) < 0.05] <- paste(mat_list_Xing[[m_idx,S_idx]][abs( vec_Xing_temp - vec_Xing_lat1[ii] ) < 0.05],ii,sep='')
	 }
         mat_list_XXing[[m_idx,S_idx]] <- vec_XXing_temp

# Lat and lon for track segments.
         mat_list_1Hz_lat[[m_idx,S_idx]] <- mat_list_buoy_data1[[m_idx,1]][[2]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
         mat_list_1Hz_lon[[m_idx,S_idx]] <- mat_list_buoy_data1[[m_idx,1]][[3]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
# Median Hs qual.
         mat_list_1Hz_qual[[m_idx,S_idx]]       <- mat_list_buoy_data1[[m_idx,1]][[7]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
         mat_list_1Hz_numval[[m_idx,S_idx]]     <- mat_list_buoy_data1[[m_idx,1]][[8]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
         mat_list_1Hz_rms[[m_idx,S_idx]]        <- mat_list_buoy_data1[[m_idx,1]][[9]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
# 1 Hz Hs QC.
         vec_hs <- vec_hs_QC <- mat_list_buoy_data1[[m_idx,1]][[5]][mat_list_Lvec_buoy_samp[[m_idx,S_idx]]]
         mat_list_Lvec_QC[[m_idx,S_idx]] <- ! ( mat_list_1Hz_numval[[m_idx,S_idx]] < 16 | mat_list_1Hz_rms[[m_idx,S_idx]] > 1.0 )
         vec_hs_QC[ ! mat_list_Lvec_QC[[m_idx,S_idx]] ] <- NA

         mat_list_1Hz_hs[[m_idx,S_idx]] <- vec_hs_QC
# Median Hs.
         mat_list_median_hs[[m_idx,S_idx]]     <- sapply( X=1:length(list_nc1_breaks), FUN=function(x) { median( vec_hs[ list_nc1_breaks[[x]] ],na.rm=T ) } )
         mat_list_median_hs_min[[m_idx,S_idx]] <- sapply( X=1:length(list_nc1_breaks), FUN=function(x) { median( vec_hs[ list_nc1_breaks[[x]] ][list_break_dist_min_idx[[x]]],na.rm=T ) } )
         mat_list_median_hs_qual[[m_idx,S_idx]] <- sapply( X=1:length(list_nc1_breaks), FUN=function(x) { median( vec_hs[ list_nc1_breaks[[x]] ][ mat_list_1Hz_qual[[m_idx,S_idx]][ list_nc1_breaks[[x]] ] == 0 ],na.rm=T ) } )
         mat_list_median_hs_numval[[m_idx,S_idx]] <- sapply( X=1:length(list_nc1_breaks), FUN=function(x) { median( vec_hs[ list_nc1_breaks[[x]] ][ mat_list_1Hz_numval[[m_idx,S_idx]][ list_nc1_breaks[[x]] ] > 14 ],na.rm=T ) } )
         #mat_list_median_hs_rms[[m_idx,S_idx]] <- sapply( X=1:length(list_nc1_breaks), FUN=function(x) { median( vec_hs[ list_nc1_breaks[[x]] ][ mat_list_1Hz_numval[[m_idx,S_idx]][ list_nc1_breaks[[x]] ] > 14 ],na.rm=T ) } )
         #mat_list_median_hs_min[[m_idx,S_idx]] <- sapply( X=1:length(list_nc1_breaks), FUN=function(x) { median( vec_hs[ list_nc1_breaks[[x]] ][list_break_dist_min_idx[[x]]],na.rm=T ) } )
      }
   }
   }

# Plot sampling.
   vec_lat <- unlist( sapply(X=1:13,FUN=function(x) { unlist(mat_list_buoy_data1[[x,1]][[2]]) }) )
   vec_lon <- unlist( sapply(X=1:13,FUN=function(x) { unlist(mat_list_buoy_data1[[x,1]][[3]]) }) )

   list_1Hz_distance_distn[[buoy_idx]] <- unlist(mat_list_1Hz_dist[,1])
   #sapply(X=1:2,FUN=function(x) { min(list_1Hz_distance_distn[[x]]) } )

   }
#=================================================================================================#
# OLD STUFF ??
#=================================================================================================#
### Assign colours to ascending (red) and descending (blue) points.
##   AA <- NULL
##   for (i in 1:13) { for (j in 1:length(mat_list_breaks_master[[i,1]])) { AA <- c(AA,rep(mat_list_Xing[[i,1]][j],length(mat_list_breaks_master[[i,1]][[j]]))) } }
##   BB <- 0; BB[AA == "A"] <- "red"; BB[AA == "D"] <- "blue"
##
###   X11()
##   plot( vec_lon-360, vec_lat )
##   points(df_buoy_data$buoy_lon[buoy_idx],df_buoy_data$buoy_lat[buoy_idx],pch=23,cex=3,col="black")
##   points( vec_lon[unlist(mat_list_Lvec_buoy_samp[,1])]-360,vec_lat[unlist(mat_list_Lvec_buoy_samp[,1])],pch=19,cex=1.5,col=BB )
##   points( vec_lon[unlist(mat_list_Lvec_buoy_samp[,1])][unlist(mat_list_Lvec_break_dist_min_idx)]-360,vec_lat[unlist(mat_list_Lvec_buoy_samp[,1])][unlist(mat_list_Lvec_break_dist_min_idx)],pch=19,cex=2.0,col="purple" )
#
#   }
#
## Scatterplot of median against individual points.
#   #X11(); plot(unlist(mat_list_median_hs_numval[,1]),unlist(mat_list_median_hs_min[,1]),xlim=c(0,5),ylim=c(0,5),xlab="Median Hs (all points)",ylab="Hs (closest single point)",main="Hs [numval > 14]");abline(0,1)
#   #X11(); plot(unlist(mat_list_median_hs_qual[,1]),unlist(mat_list_median_hs_min[,1]),xlim=c(0,5),ylim=c(0,5),xlab="Median Hs (all points)",ylab="Median Hs (closest single point)",main="Hs [qual = 1]");abline(0,1)
#
## Loop over all ascending (or descending) passes to get single point values.
#   list_ascending_lat <- list()
#   mat_ascending_lat <- NULL
#   list_ascending_lon <- list()
#   mat_ascending_lon <- NULL
#   list_descending_lat <- list()
#   mat_descending_lat <- NULL
#   list_descending_lon <- list()
#   mat_descending_lon <- NULL
#
#   list_ascending <- list()
#   mat_ascending <- NULL
#   list_descending <- list()
#   mat_descending <- NULL
#
#   MMidx <- 1:3
#   for (j in -14:14) {
#      for (m_idx in MMidx) {
## Lat and lon.
#          list_ascending_lat[[m_idx]] <- sapply( X=1:length(mat_list_Xing[[m_idx,1]]), FUN=function(x) { if ( mat_list_Xing[[m_idx,1]][[x]] == "A" ) { mat_list_1Hz_lat[[m_idx,1]][ mat_list_breaks_master[[m_idx,1]][[x]][ j+which( mat_list_Lvec_break_dist_min_idx[[m_idx,1]][[x]] ) ] ] } else { NA } } )
#          list_ascending_lon[[m_idx]] <- sapply( X=1:length(mat_list_Xing[[m_idx,1]]), FUN=function(x) { if ( mat_list_Xing[[m_idx,1]][[x]] == "A" ) { mat_list_1Hz_lon[[m_idx,1]][ mat_list_breaks_master[[m_idx,1]][[x]][ j+which( mat_list_Lvec_break_dist_min_idx[[m_idx,1]][[x]] ) ] ] } else { NA } } )
#          list_descending_lat[[m_idx]] <- sapply( X=1:length(mat_list_Xing[[m_idx,1]]), FUN=function(x) { if ( mat_list_Xing[[m_idx,1]][[x]] == "D" ) { mat_list_1Hz_lat[[m_idx,1]][ mat_list_breaks_master[[m_idx,1]][[x]][ j+which( mat_list_Lvec_break_dist_min_idx[[m_idx,1]][[x]] ) ] ] } else { NA } } )
#          list_descending_lon[[m_idx]] <- sapply( X=1:length(mat_list_Xing[[m_idx,1]]), FUN=function(x) { if ( mat_list_Xing[[m_idx,1]][[x]] == "D" ) { mat_list_1Hz_lon[[m_idx,1]][ mat_list_breaks_master[[m_idx,1]][[x]][ j+which( mat_list_Lvec_break_dist_min_idx[[m_idx,1]][[x]] ) ] ] } else { NA } } )
## Hs.
#          list_ascending[[m_idx]] <- sapply( X=1:length(mat_list_Xing[[m_idx,1]]), FUN=function(x) { if ( mat_list_Xing[[m_idx,1]][[x]] == "A" ) { mat_list_1Hz_hs[[m_idx,1]][ mat_list_breaks_master[[m_idx,1]][[x]][ j+which( mat_list_Lvec_break_dist_min_idx[[m_idx,1]][[x]] ) ] ] } else { NA } } )
#          list_descending[[m_idx]] <- sapply( X=1:length(mat_list_Xing[[m_idx,1]]), FUN=function(x) { if ( mat_list_Xing[[m_idx,1]][[x]] == "D" ) { mat_list_1Hz_hs[[m_idx,1]][ mat_list_breaks_master[[m_idx,1]][[x]][ j+which( mat_list_Lvec_break_dist_min_idx[[m_idx,1]][[x]] ) ] ] } else { NA } } )
#      }
#      mat_ascending_lat <- cbind(mat_ascending_lat,unlist(list_ascending_lat))
#      mat_ascending_lon <- cbind(mat_ascending_lon,unlist(list_ascending_lon))
#      mat_descending_lat <- cbind(mat_descending_lat,unlist(list_descending_lat))
#      mat_descending_lon <- cbind(mat_descending_lon,unlist(list_descending_lon))
#
#      mat_ascending <- cbind(mat_ascending,unlist(list_ascending))
#      mat_descending <- cbind(mat_descending,unlist(list_descending))
#   }
#   #X11()
#   #par(mfrow=c(3,3))
#   #for (j in 1:9) {
#   #   plot(unlist(mat_list_median_hs[,1]),unlist(mat_ascending[,j]),xlim=c(0,5),ylim=c(0,5),xlab="Median Hs (all points)",ylab="Median Hs (closest single point)",main="Hs");abline(0,1)
#   #}
#
#   #X11()
#   #plot( -14:14,sapply( X=1:29, FUN=function(x) { cor(unlist(mat_list_median_hs_min[MMidx,1]),mat_ascending[,x],use="pairwise.complete.obs") } ),
#   #   ylim=c(0.5,1.0),xlab="1Hz point index relative to closest", ylab="Correlation with closest point Hs", main=paste("Ascending",paste(MMidx,collapse="_")) )
#   #abline(v=0,lwd=0.5)
#   #abline(h=c(0.9,0.95,1.0),lwd=0.5)
#
#   #X11()
#   ##plot( -14:14,sapply( X=1:29, FUN=function(x) { cor(unlist(mat_list_median_hs_numval[,1]),mat_descending[,x],use="pairwise.complete.obs") } ),
#   #plot( -14:14,sapply( X=1:29, FUN=function(x) { cor(unlist(mat_list_median_hs_min[MMidx,1]),mat_descending[,x],use="pairwise.complete.obs") } ),
#   #   ylim=c(0.5,1.0),xlab="1Hz point index relative to closest", ylab="Correlation with closest point Hs", main=paste("Descending",paste(MMidx,collapse="_")) )
#   #abline(v=0,lwd=0.5)
#   #abline(h=c(0.9,0.95,1.0),lwd=0.5)
#
### Bivariate data for testing.
##   require(MASS)
##   df_bivar <- as.data.frame(mvrnorm(n=10000,mu=c(0, 0),Sigma=matrix(c(1,0.98,0.98,1), ncol=2)))
##   func_int <- function(x) { c(0.98*x,1.02*x) }
##   AA <- t(sapply( X=df_bivar[,1], FUN=func_int))
##   BB <- apply( X=cbind(df_bivar[,2],AA),MAR=1, FUN=function(x) { x[1] > x[2] & x[1] < x[3] } )
##   sum(BB)
#
#   vec_asc_lat <- sapply( X=1:29, FUN=function(x) { mean(mat_ascending_lat[,x],na.rm=T) } )
#   vec_asc_lon <- sapply( X=1:29, FUN=function(x) { mean(mat_ascending_lon[,x],na.rm=T) } )
#   vec_des_lat <- sapply( X=1:29, FUN=function(x) { mean(mat_descending_lat[,x],na.rm=T) } )
#   vec_des_lon <- sapply( X=1:29, FUN=function(x) { mean(mat_descending_lon[,x],na.rm=T) } )
#
#-------------------------------------------------------------------------------------------------#
# ERA5 interpolation
# To do: timing information.
   df_plot_ERA5 <- as.data.frame( expand.grid( mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lon_mid-360,mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lat_mid ) )
   colnames(df_plot_ERA5) <- c("ERA5_lon","ERA5_lat")
# Plot the box.
   df_plot_ERA5_box <- as.data.frame( t(c(vec_X,vec_Y)) )
   colnames(df_plot_ERA5_box) <- c("x1","x2","y1","y2")
#-------------------------------------------------------------------------------------------------#

## ERA5 Hs gradient and scale (lon).
#   if ( df_buoy_data$buoy_lon[ERA5_b_idx] < (mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lon_mid[2]-360) ) {
#      fl_ERA5_lon_grad <- t(mat_list_ERA5[[1,ERA5_b_idx]][[5]][[1]][,,1])[2,1] - t(mat_list_ERA5[[1,ERA5_b_idx]][[5]][[1]][,,1])[2,2]
#      vec_X <- mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lon_mid[1:2]-360
#      vec_QX <- rbind(mat_list_ERA5[[1,ERA5_b_idx]][[5]][[1]][1,2,1],mat_list_ERA5[[1,ERA5_b_idx]][[5]][[1]][2,2,1])
#   } else {
#      fl_ERA5_lon_grad <- t(mat_list_ERA5[[1,ERA5_b_idx]][[5]][[1]][,,1])[2,3] - t(mat_list_ERA5[[1,ERA5_b_idx]][[5]][[1]][,,1])[2,2]
#      vec_X <- mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lon_mid[2:3]-360
#      vec_QX <- rbind(mat_list_ERA5[[1,ERA5_b_idx]][[5]][[1]][2,2,1],mat_list_ERA5[[1,ERA5_b_idx]][[5]][[1]][3,2,1])
#   }
#   fl_ERA5_lon_scale <- abs( df_buoy_data$buoy_lon[3] - (mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lon_mid[2]-360) ) / 0.5
## ERA5 Hs gradient and scale (lat).
#   if ( df_buoy_data$buoy_lat[ERA5_b_idx] < mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lat_mid[2] ) {
#      fl_ERA5_lat_grad <- t(mat_list_ERA5[[1,ERA5_b_idx]][[5]][[1]][,,1])[3,2] - t(mat_list_ERA5[[1,ERA5_b_idx]][[5]][[1]][,,1])[2,2]
#      vec_Y <- mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lat_mid[3:2]
#      vec_QY <- rbind(mat_list_ERA5[[1,ERA5_b_idx]][[5]][[1]][2,1,1],mat_list_ERA5[[1,ERA5_b_idx]][[5]][[1]][3,1,1])
#   } else {
#      fl_ERA5_lat_grad <- t(mat_list_ERA5[[1,ERA5_b_idx]][[5]][[1]][,,1])[1,2] - t(mat_list_ERA5[[1,ERA5_b_idx]][[5]][[1]][,,1])[2,2]
#      vec_Y <- mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lat_mid[2:1]
#      vec_QY <- rbind(mat_list_ERA5[[1,ERA5_b_idx]][[5]][[1]][1,1,1],mat_list_ERA5[[1,ERA5_b_idx]][[5]][[1]][2,1,1])
#   }
#   fl_ERA5_lat_scale <- abs( df_buoy_data$buoy_lat[3] - mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lat_mid[2] ) / 0.5
## Adjustments.
#   mat_list_ERA5[[1,ERA5_b_idx]][[5]][[1]][,,1][2,2] + fl_ERA5_lon_grad*fl_ERA5_lon_scale + fl_ERA5_lat_grad*fl_ERA5_lat_scale
#
## Bilinear equation (wikipedia).
#   func_bilinear <- function(x,y) { 1/(0.5*0.5) * cbind(c(vec_X[2]-x),c(x-vec_X[1])) %*% cbind(vec_QX,vec_QY) %*% rbind(c(vec_Y[2]-y),c(y-vec_Y[1])) }
#   func_bilinear(df_buoy_data$buoy_lon[ERA5_b_idx],df_buoy_data$buoy_lat[ERA5_b_idx])

   #df_circles <- data.frame(
   #   x0 = c(vec_asc_lon,vec_des_lon),
   #   y0 = c(vec_asc_lat,vec_des_lat),
   #   rr = c( 0.005/-log( 0.93*sapply( X=1:29, FUN=function(x) { cor(unlist(mat_list_median_hs_numval[,1]),mat_ascending[,x],use="pairwise.complete.obs") } ) ),
   #      0.005/-log( 0.93*sapply( X=1:29, FUN=function(x) { cor(unlist(mat_list_median_hs_numval[,1]),mat_descending[,x],use="pairwise.complete.obs") } ) ) )
   #)
   #geom_circle(aes(x0 = x0, y0 = y0, r = rr, fill = rr), data = df_circles)

#=================================================================================================#
# ggplot.
#-------------------------------------------------------------------------------------------------#
   lon_sat_samp <- rep(NA,length(vec_lon))
   #lon_sat_samp[unlist(mat_list_Lvec_buoy_samp[,1])] <- vec_lon[unlist(mat_list_Lvec_buoy_samp[,1])]
   lon_sat_samp[ which(unlist(mat_list_Lvec_buoy_samp[,1]))[unlist(mat_list_Lvec_QC[,1])] ] <- vec_lon[ which(unlist(mat_list_Lvec_buoy_samp[,1]))[unlist(mat_list_Lvec_QC[,1])] ]-360
   lon_sat_samp_min <- rep(NA,length(vec_lon))
   lon_sat_samp_min[which(unlist(mat_list_Lvec_buoy_samp))[which(unlist(mat_list_Lvec_break_dist_min_idx))]] <- vec_lon[which(unlist(mat_list_Lvec_buoy_samp))[which(unlist(mat_list_Lvec_break_dist_min_idx))]]

   lat_sat_samp <- rep(NA,length(vec_lat))
   #lat_sat_samp[unlist(mat_list_Lvec_buoy_samp[,1])] <- vec_lat[unlist(mat_list_Lvec_buoy_samp[,1])]
   lat_sat_samp[ which(unlist(mat_list_Lvec_buoy_samp[,1]))[unlist(mat_list_Lvec_QC[,1])] ] <- vec_lat[ which(unlist(mat_list_Lvec_buoy_samp[,1]))[unlist(mat_list_Lvec_QC[,1])] ]
   lat_sat_samp_min <- rep(NA,length(vec_lat))
   lat_sat_samp_min[which(unlist(mat_list_Lvec_buoy_samp))[which(unlist(mat_list_Lvec_break_dist_min_idx))]] <- vec_lat[which(unlist(mat_list_Lvec_buoy_samp))[which(unlist(mat_list_Lvec_break_dist_min_idx))]]

   df_plot <- data.frame(lon_sat=vec_lon-360,lat_sat=vec_lat,lon_sat_samp=lon_sat_samp,lat_sat_samp=lat_sat_samp,lon_sat_samp_min=lon_sat_samp_min,lat_sat_samp_min=lat_sat_samp_min)

#   #mapWorld <- map_data("world")
#   #setnames(mapWorld, c("X","Y","PID","POS","region","subregion"))
#   #worldmap = clipPolys(mapWorld, xlim=c(-179,129),ylim=c(-75,75), keepExtra=TRUE)
#   #worldmap = clipPolys(mapWorld, xlim=c(-50,200),ylim=c(-75,75), keepExtra=TRUE)

   p2 <- ggplot(data = df_plot) + #scale_x_continuous(limits = c(-100, -50), expand = c(0,0)) + scale_y_continuous(limits = c(10, 50), expand = c(0,0)) +
         ylab("Latitude\n") + xlab("\nLongitude") +
         scale_color_manual(values = c('black','red','blue','green','green')) +
         coord_map(xlim = c(df_buoy_data$buoy_lon[buoy_idx]-2,df_buoy_data$buoy_lon[buoy_idx]+2), ylim = c(df_buoy_data$buoy_lat[buoy_idx]-2,df_buoy_data$buoy_lat[buoy_idx]+2), projection = "lambert", lat0 = df_buoy_data$buoy_lat[buoy_idx]-1, lat1 = df_buoy_data$buoy_lat[buoy_idx]+1) +
         #coord_map(xlim = c(200,210), ylim = c(50,60), projection = "lambert", lat0 = df_buoy_data$buoy_lat[1]-1, lat1 = df_buoy_data$buoy_lat[1]+1) +
# Pacific.
         #coord_map(xlim = c(148,232), ylim = c(-15,58), projection = "bonne", lat0 = 20) +
# Pacific (East).
         #coord_map(xlim = c(200,210), ylim = c(50,60), projection = "bonne", lat0 = 55) +
         geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), color = "#000000", fill = NA, linewidth = 0.35) +
# Satellite tracks.
         geom_point(aes(x=lon_sat, y=lat_sat, col="1) J-3 1 Hz"), size=1, shape=1) +
         geom_point(aes(x=lon_sat_samp, y=lat_sat_samp, col="2) Sampled points"), size=2, shape=1) +
         geom_point(aes(x=lon_sat_samp_min, y=lat_sat_samp_min, col='3) Closest sample'), size=3, shape=1) +
# Satellite tracks influence.
#         geom_circle(aes(x0 = x0, y0 = y0, r = rr), data = df_circles) +
# Buoy symbol(s).
         geom_point(aes(x=df_buoy_data$buoy_lon[buoy_idx], y=df_buoy_data$buoy_lat[buoy_idx], col="yellow"), colour="black", fill="yellow", size=5, shape=23) +
         geom_label(aes(x=df_buoy_data$buoy_lon[buoy_idx], y=df_buoy_data$buoy_lat[buoy_idx], label=paste(buoy_list[buoy_idx])), label.padding = unit(0.30, "lines"), size = 4, nudge_x = 1.2, nudge_y = 0.0) +
# ERA5 grid cells.
         #geom_point(aes(x=mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lon_mid[2]-360, y=mat_list_ERA5[[1,ERA5_b_idx]][[1]]$buoy_lat_mid[2]), colour="red", size=4, shape=10) +
         geom_point(data = df_plot_ERA5, aes(x=ERA5_lon, y=ERA5_lat, col="4) ERA5 (0.5 deg)"), size=4, shape=10) +
         geom_rect(data = df_plot_ERA5_box, mapping = aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2,col="5) ERA5 bilinear"), alpha=0.0) +
##         #scale_fill_viridis_d() +
##         scale_fill_manual(values = c('blue','yellow')) +
### Area boxes (in blue).
##         geom_rect(data=df_rect,mapping=aes(xmin=x1,xmax=x2,ymin=y1,ymax=y2), color="blue", alpha=0.0) +
##         geom_text(data=df_rect,aes(x=x1+0.9*(x2-x1),y=y1+0.2*(y2-y1),label=r),size=6) +
# Theme stuff.
         theme(axis.title.x = element_blank(),
               axis.title.y = element_blank(),
               axis.text.x = element_text(size = 10),
               axis.text.y = element_text(size = 10),
               legend.text=element_text(size = 14),
               legend.title = element_blank() ) +
         guides(color=guide_legend(override.aes=list(shape=c(1,1,1,10,NA),linetype=c(0,0,0,0,1),size=c(5,5,5,5,NA))))
#         guides(fill=guide_legend(reverse=TRUE)) +

   fig_file_name <- paste("./figures/test_sampling/",buoy_list[buoy_idx],"/map_",buoy_list[buoy_idx],"_",buoy_radius,"km.pdf",sep="")
   system(paste("if [ ! -d ./figures/test_sampling/",buoy_list[buoy_idx]," ]; then mkdir ./figures/test_sampling/",buoy_list[buoy_idx]," &> /dev/null; fi",sep=""))
   mat_lay <- cbind(c(1,1,1),matrix(rep(2,9),ncol=3))
   pdf(fig_file_name,width = 8, height = 9)
   #grid.arrange(p2,p1,layout_matrix=mat_lay)
   plot(p2)
   dev.off()
   system(paste("okular",fig_file_name,"&> /dev/null &"))


#-------------------------------------------------------------------------------------------------#
# Find the "mean" time stamp for each track segment (J3,S6).
      #list_breaks <- mat_list_breaks_master[,1]

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
         for (S_idx in 1:1) {
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
               vec_hs_temp <- rep(NA,sum(mat_list_XXing[[m_idx,1]] == vec_unique_trackID[jj]))
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


# J3 median.
            vec_Q50_J3 <- sapply( X=1:length(mat_list_breaks_master[[m_idx,S_idx]]),FUN=function(x) { median( list_buoy_data1[[1]][[5]][ mat_list_Lvec_buoy_samp[[m_idx,S_idx]] ][ mat_list_breaks_master[[m_idx,S_idx]][[x]] ], na.rm=T ) } )
            vec_Q50_J3_ALL <- c(vec_Q50_J3_ALL,vec_Q50_J3)
# Loop over each 1 Hz point to obtain correlation with buoy and ERA5.
#            for ( ii in 1:length(mat_list_breaks_master[[m_idx,S_idx]]) ) {
#               vec_1Hz_temp <- [[1]][[5]][ mat_list_Lvec_buoy_samp[[m_idx,S_idx]] ][ mat_list_breaks_master[[m_idx,S_idx]][[ii]] ] ) ) {
#               mat_list_cor[[m_idx,S_idx]] <- sapply( X=1:length(vec_1Hz_temp),FUN=function(x) {  } )
#            }
         }
      }

    #for ( ii in unique(unlist(mat_list_XXing)) ) { sapply(X=1:sum(mat_list_XXing[[m_idx,1]] == ii),FUN=function(x) { print( paste(ii,length( mat_list_breaks_master[[m_idx,S_idx]][[which(mat_list_XXing[[m_idx,1]] == ii)[x]]] ) ) ) }) }
      list2_trackID <- NULL
      mat_list_trackID <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
      for ( ii in 1:length(vec_unique_trackID) ) {
         #list_trackID_temp <- NULL
         for ( m_idx in 1:13 ) {
            #list_trackID_temp <- c( list_trackID_temp,mat_list_breaks_master[[m_idx,S_idx]][which(mat_list_XXing[[m_idx,1]] == vec_unique_trackID[ii])] )
            mat_list_trackID[[m_idx,ii]] <- mat_list_breaks_master[[m_idx,S_idx]][which(mat_list_XXing[[m_idx,1]] == vec_unique_trackID[ii])]
         }
         #list2_trackID[[ii]] <- list_trackID_temp
      }
      #for ( kk in 1:13 ) { print(paste("kk:",kk)); for ( jj in 1:length(vec_unique_trackID) ) { print(paste("jj:",jj)); for ( ii in 1:length(mat_list_trackID[[kk,jj]]) ) { print( length( mat_list_trackID[[kk,jj]][[ii]] ) ) } } }
      #for ( kk in 1:13 ) { print(paste("m_idx:",kk)); for ( jj in 1 ) { print(paste("jj:",jj)); for ( ii in 1:length(mat_list_trackID[[kk,jj]]) ) { print( paste(length( mat_list_trackID[[kk,jj]][[ii]] ),mat_list_1Hz_lat[[m_idx,1]][mat_list_trackID[[kk,jj]][[ii]][1]] ) ) } } }

# Correlation calculations.
   func_mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
   }

# Find mode of trackID length and closest points.
   vec_mode <- NA
   vec_mode_min <- NA
   for ( jj in 1:length(vec_unique_trackID) ) {
# Length.
      vec_mode_temp <- NULL
      for ( kk in 1:13 ) { vec_mode_temp <- c(vec_mode_temp,sapply(X=mat_list_trackID[[kk,jj]],length)) }
      vec_mode[jj] <- func_mode(vec_mode_temp)
# Closest point.
      vec_mode_temp <- NULL
      for ( kk in 1:13 ) { vec_mode_temp <- c(vec_mode_temp,sapply(X=mat_list_Lvec_break_dist_min_idx[[kk,1]][mat_list_XXing[[kk,1]] == vec_unique_trackID[jj]],which)) }
      vec_mode_min[jj] <- func_mode(vec_mode_temp)
   }

# Find Hs values for each 1 Hz point in each track ID.
   flag_QC <- FALSE
   fl_track_tol <- 0.016
   mat_list_trackID_hs <- matrix(list(),nrow=13,ncol=length(vec_unique_trackID))
   list_lat_test <- list()
   for ( jj in 1:length(vec_unique_trackID) ) {
   vec_lat_test <- NULL
   #print(paste("TrackID:",jj))
      for ( kk in 1:13 ) {
         #print(paste("m_idx:",kk))
         mat_trackID_hs_temp <- matrix(NA,nrow=length(mat_list_trackID[[kk,jj]]),ncol=vec_mode[jj])
            for ( ii in 1:length(mat_list_trackID[[kk,jj]]) ) {
               if ( length( mat_list_trackID[[kk,jj]][[ii]] ) == (vec_mode[jj]+1) & abs(mat_list_1Hz_lat[[kk,1]][mat_list_trackID[[kk,jj]][[ii]][2]] - vec_Xing_lat1[jj]) < fl_track_tol ) {
                  #print( paste(length( mat_list_trackID[[kk,jj]][[ii]] ),mat_list_1Hz_lat[[kk,1]][mat_list_trackID[[kk,jj]][[ii]][2]] ) )
                  #print( paste("Lat diff:",mat_list_1Hz_lat[[kk,1]][mat_list_trackID[[kk,jj]][[ii]][2]] - vec_Xing_lat1[jj]) )
                  vec_lat_test <- c(vec_lat_test,mat_list_1Hz_lat[[kk,S_idx]][mat_list_trackID[[kk,jj]][[ii]][2]] - vec_Xing_lat1[jj])
                  mat_trackID_hs_temp[ii,] <- sapply(X=2:length( mat_list_trackID[[kk,jj]][[ii]] ),FUN=function(x) { mat_list_1Hz_hs[[kk,1]][mat_list_trackID[[kk,jj]][[ii]][x]] })

               } else if ( length( mat_list_trackID[[kk,jj]][[ii]] ) == (vec_mode[jj]+1) & abs(mat_list_1Hz_lat[[kk,1]][mat_list_trackID[[kk,jj]][[ii]][1]] - vec_Xing_lat1[jj]) < fl_track_tol ) {
                  #print( paste("Lat diff:",mat_list_1Hz_lat[[kk,1]][mat_list_trackID[[kk,jj]][[ii]][1]] - vec_Xing_lat1[jj]) )
                  vec_lat_test <- c(vec_lat_test,mat_list_1Hz_lat[[kk,S_idx]][mat_list_trackID[[kk,jj]][[ii]][1]] - vec_Xing_lat1[jj])
                  mat_trackID_hs_temp[ii,] <- sapply(X=1:(length( mat_list_trackID[[kk,jj]][[ii]] )-1),FUN=function(x) { mat_list_1Hz_hs[[kk,1]][mat_list_trackID[[kk,jj]][[ii]][x]] })

               } else if ( length( mat_list_trackID[[kk,jj]][[ii]] ) == vec_mode[jj] & abs(mat_list_1Hz_lat[[kk,1]][mat_list_trackID[[kk,jj]][[ii]][1]] - vec_Xing_lat1[jj]) < fl_track_tol ) {
                  vec_lat_test <- c(vec_lat_test,mat_list_1Hz_lat[[kk,S_idx]][mat_list_trackID[[kk,jj]][[ii]][1]] - vec_Xing_lat1[jj])
                  mat_trackID_hs_temp[ii,] <- sapply(X=1:length( mat_list_trackID[[kk,jj]][[ii]] ),FUN=function(x) { mat_list_1Hz_hs[[kk,1]][mat_list_trackID[[kk,jj]][[ii]][x]] })

               } else if ( length( mat_list_trackID[[kk,jj]][[ii]] ) == vec_mode[jj] & abs(mat_list_1Hz_lat[[kk,1]][mat_list_trackID[[kk,jj]][[ii]][2]] - vec_Xing_lat1[jj]) < fl_track_tol ) {
                  vec_lat_test <- c(vec_lat_test,mat_list_1Hz_lat[[kk,S_idx]][mat_list_trackID[[kk,jj]][[ii]][1]] - vec_Xing_lat1[jj])
                  mat_trackID_hs_temp[ii,1:(length( mat_list_trackID[[kk,jj]][[ii]])-1)] <- sapply(X=2:length( mat_list_trackID[[kk,jj]][[ii]] ),FUN=function(x) { mat_list_1Hz_hs[[kk,1]][mat_list_trackID[[kk,jj]][[ii]][x]] })

               } else if ( length( mat_list_trackID[[kk,jj]][[ii]] ) == (vec_mode[jj]-1) & abs(mat_list_1Hz_lat[[kk,1]][mat_list_trackID[[kk,jj]][[ii]][1]] - vec_Xing_lat1[jj]) < fl_track_tol ) {
                  vec_lat_test <- c(vec_lat_test,mat_list_1Hz_lat[[kk,S_idx]][mat_list_trackID[[kk,jj]][[ii]][1]] - vec_Xing_lat1[jj])
                  mat_trackID_hs_temp[ii,1:length( mat_list_trackID[[kk,jj]][[ii]])] <- sapply(X=1:length( mat_list_trackID[[kk,jj]][[ii]] ),FUN=function(x) { mat_list_1Hz_hs[[kk,1]][mat_list_trackID[[kk,jj]][[ii]][x]] })
# Catch for strongly misaligned tracks.
#               } else if ( length( mat_list_trackID[[kk,jj]][[ii]] ) == vec_mode[jj] & abs(mat_list_1Hz_lat[[kk,1]][mat_list_trackID[[kk,jj]][[ii]][1]] - vec_Xing_lat1[jj]) < 0.025 ) {
#                  vec_lat_test <- c(vec_lat_test,mat_list_1Hz_lat[[kk,S_idx]][mat_list_trackID[[kk,jj]][[ii]][1]] - vec_Xing_lat1[jj])
#                  mat_trackID_hs_temp[ii,] <- sapply(X=1:length( mat_list_trackID[[kk,jj]][[ii]] ),FUN=function(x) { mat_list_1Hz_hs[[kk,1]][mat_list_trackID[[kk,jj]][[ii]][x]] })

               } else {
                  print( paste("Month: ",kk,"; TrackID: ",vec_unique_trackID[jj],"; Mode length: ",vec_mode[jj],"; Actual length: ",length( mat_list_trackID[[kk,jj]][[ii]]),"; #: ",ii,sep="") )
               }
            }
         mat_list_trackID_hs[[kk,jj]] <- mat_trackID_hs_temp
      }
      if ( !flag_QC ) { list_lat_test[[jj]] <- vec_lat_test }
      #max(abs(list_lat_test[[4]]))
   }
# Create a list of single array of all Hs trackID data (not monthly).
   list_trackID_hs <- list()
   for ( jj in 1:length(vec_unique_trackID) ) {
      mat_temp <- NULL
      for ( kk in 1:13 ) { mat_temp <- rbind(mat_temp,mat_list_trackID_hs[[kk,jj]]) }
      #for ( kk in 1:4 ) { mat_temp <- rbind(mat_temp,mat_list_trackID_hs[[kk,jj]]) }
      list_trackID_hs[[jj]] <- mat_temp
   }
# Plotting.
## Plot median of super-observation.
#   X11(); plot(unlist(mat_list_buoy_hs_coloc_AD[,1]),apply(X=list_trackID_hs[[1]],MAR=1,FUN=median ),xlim=c(0,8),ylim=c(0,8)); abline(0,1)
## Plot by sub-point.
#   X11(); plot(unlist(mat_list_buoy_hs_coloc_AD[,1]),list_trackID_hs[[1]][,1],xlim=c(0,8),ylim=c(0,8)); abline(0,1)
# Plot correlations for all tracks.
   plot_data <- mat_list_buoy_hs_coloc_AD
   #plot_data <- mat_list_ERA5_hs_coloc_AD
   #plot_data <- mat_list_ERA5_hs_BILIN_coloc_AD

   fig_cor_file_name <- paste("./figures/test_sampling/",buoy_list[buoy_idx],"/track_cor_",buoy_list[buoy_idx],"_",buoy_radius,"km_016.pdf",sep="")
   pdf(fig_cor_file_name,width = (3.5 * length(vec_unique_trackID)), height = 8.4)

   par(mfrow=c(2,length(vec_unique_trackID)))
   for ( jj in 1:length(vec_unique_trackID) ) {
      i_len_trackID <- dim(list_trackID_hs[[jj]])[2]
      vec_plot_cor <- rep(NA,i_len_trackID)
# Test for sufficient number of points to ccompute correlation.
      for ( ii in 1:i_len_trackID ) {
         if ( sum( !is.na( unlist(plot_data[,jj]) ) & !is.na( list_trackID_hs[[jj]][,ii] ) ) > 10 ) {
            vec_plot_cor[ii] <- cor(unlist(plot_data[,jj]),list_trackID_hs[[jj]][,ii],use="pairwise.complete.obs")
            #vec_plot_cor <- sapply(X=1:i_len_trackID,FUN=function(x) { cor(unlist(mat_list_ERA5_hs_coloc_AD[,jj]),list_trackID_hs[[jj]][,x],use="pairwise.complete.obs") })
            #vec_plot_cor <- sapply(X=1:i_len_trackID,FUN=function(x) { cor(unlist(mat_list_ERA5_hs_BILIN_coloc_AD[,jj]),list_trackID_hs[[jj]][,x],use="pairwise.complete.obs") })
         }
      }
      plot(1:i_len_trackID,vec_plot_cor,ylim=c(0.5,1.0),xlab="1 Hz point index",ylab="Correlation",main=paste("Track ID:",vec_unique_trackID[[jj]],"\nBuoy:",buoy_list[buoy_idx]))
      abline(h=c(0.95,1.0),col="grey")
      abline(v=seq(5,40,5),col="grey")
      points((1:i_len_trackID)[vec_mode_min[jj]],vec_plot_cor[vec_mode_min[jj]],pch=19,col="blue")
      par(new=T)
      plot(1:i_len_trackID,sapply(X=1:i_len_trackID,FUN=function(x) { sum( !is.na( unlist(plot_data[,jj]) ) & !is.na( list_trackID_hs[[jj]][,x] ) ) }),pch=4,ylim=c(0,60),axes=F,xlab="",ylab="")
      axis(side=4,at=seq(0,40,5))
# Histogram of collocation time differences.
      hist(unlist( mat_list_time_diff_J3B )/60,breaks=30,xlim=c(0,30),main=paste("Distribution of collocation\ntime differences. Total:",sum( !is.na( unlist( mat_list_time_diff_J3B ) ) )),xlab="Time difference (minutes)")
   }

   dev.off()
   system(paste("okular",fig_cor_file_name,"&> /dev/null &"))

# Trouble shooting.
# E.g. buoy 13, ii <- 8
#   X11(); plot(unlist(plot_data[,jj]),list_trackID_hs[[jj]][,ii]); abline(0,1)
# Find point mismatch and time index (24).
#   cbind(unlist(plot_data[,jj]),list_trackID_hs[[jj]][,ii])
# Correspond time index to month (kk). E.g. 38 A or D tracks for 13 months.
# Identify specific point.
#   mat_list_1Hz_hs[[9,1]][66:70]
# Identify quality issues.
#   mat_list_1Hz_rms[[9,1]][66:70]


## Find S6LRM time that matches J-3 block mean time.
## These will be mostly identical but occasionally there is missing data with no match-up.
#      mat_slot_J3S6L <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { abs(list_mean_time[[1]][x] - list_mean_time[[2]]) < 40 } )
#      if ( !is.matrix(mat_slot_J3S6L) ) { mat_slot_J3S6L <- t(as.matrix(mat_slot_J3S6L)) }
#
## Find S6SAR time that matches J-3 block mean time.
#      mat_slot_J3S6SAR <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { abs(list_mean_time[[1]][x] - list_mean_time[[3]]) < 40 } )
#      if ( !is.matrix(mat_slot_J3S6SAR) ) { mat_slot_J3S6SAR <- t(as.matrix(mat_slot_J3S6SAR)) }

#-------------------------------------------------------------------------------------------------#


#-------------------------------------------------------------------------------------------------#

#      vec_ERA5_ap_coloc[which(mat_slot_J3ERA,arr.ind=T)[,2]] <- vec_ERA5_ap[ which(mat_slot_J3ERA,arr.ind=T)[,1] ]
#      vec_ERA5_ap_coloc_ALL <- c(vec_ERA5_ap_coloc_ALL,vec_ERA5_ap_coloc)
##-------------------------------------------------------------------------------------------------#
#
## Find swh track medians.
## J3.
#      #vec_Q50_J3 <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { median( list_buoy_data1[[1]][[5]][ list_buoy_data1[[1]][[6]][[x]] ], na.rm=T ) } )
#      vec_Q50_J3 <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { median( list_buoy_data1[[1]][[5]][ list_Lvec_buoy_samp[[1]] ][ list_breaks[[1]][[x]] ], na.rm=T ) } )
#      vec_Q50_J3_ALL <- c(vec_Q50_J3_ALL,vec_Q50_J3)
## J3 quality.
#      #Lvec_qual_J3 <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { sum( list_buoy_data1[[1]][[7]][ list_buoy_data1[[1]][[6]][[x]] ] ) > 0 } )
#      Lvec_qual_J3 <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { sum( list_buoy_data1[[1]][[7]][ list_Lvec_buoy_samp[[1]] ][ list_breaks[[1]][[x]] ] ) > 0 } )
#      Lvec_qual_J3_ALL <- c(Lvec_qual_J3_ALL,Lvec_qual_J3)
## RMS
#      #Lvec_qual_J3 <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { any( list_buoy_data1[[1]][[9]][ list_Lvec_buoy_samp[[1]] ][ list_breaks[[1]][[x]] ] > 1 ) } )
## NUMVALS
#      Lvec_qual_numval_J3 <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { any( list_buoy_data1[[1]][[8]][ list_Lvec_buoy_samp[[1]] ][ list_breaks[[1]][[x]] ] < 9 ) } )
#      Lvec_qual_numval_J3_ALL <- c(Lvec_qual_numval_J3_ALL,Lvec_qual_numval_J3)
#
## S6 LRM.
#      #vec_Q50_S6_LRM <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { median( list_buoy_data1[[2]][[5]][ unlist( list_buoy_data1[[2]][[6]][mat_slot_J3S6L[,x]] ) ], na.rm=T ) } )
#      vec_Q50_S6_LRM <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { median( list_buoy_data1[[2]][[5]][ list_Lvec_buoy_samp[[2]] ][ unlist( list_breaks[[2]][mat_slot_J3S6L[,x]] ) ], na.rm=T ) } )
#      vec_Q50_S6_LRM_ALL1 <- c(vec_Q50_S6_LRM_ALL1,vec_Q50_S6_LRM)
## S6 LRM quality.
#      #Lvec_qual_S6_LRM <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { sum( list_buoy_data1[[2]][[7]][ unlist( list_buoy_data1[[2]][[6]][mat_slot_J3S6L[,x]] ) ] ) > 0 } )
#      Lvec_qual_S6_LRM <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { sum( list_buoy_data1[[2]][[7]][ list_Lvec_buoy_samp[[2]] ][ unlist( list_breaks[[2]][mat_slot_J3S6L[,x]] ) ] ) > 0 } )
#      Lvec_qual_S6_LRM_ALL <- c(Lvec_qual_S6_LRM_ALL,Lvec_qual_S6_LRM)
## S6 SAR.
#      #vec_Q50_S6_SAR <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { median( list_buoy_data1[[3]][[5]][ unlist( list_buoy_data1[[3]][[6]][mat_slot_J3S6SAR[,x]] ) ], na.rm=T ) } )
#      vec_Q50_S6_SAR <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { median( list_buoy_data1[[3]][[5]][ list_Lvec_buoy_samp[[3]] ][ unlist( list_breaks[[3]][mat_slot_J3S6SAR[,x]] ) ], na.rm=T ) } )
#      vec_Q50_S6_SAR_ALL <- c(vec_Q50_S6_SAR_ALL,vec_Q50_S6_SAR)
## S6 SAR quality.
#      #Lvec_qual_S6_SAR <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { sum( list_buoy_data1[[3]][[7]][ unlist( list_buoy_data1[[3]][[6]][mat_slot_J3S6SAR[,x]] ) ] ) > 0 } )
#      Lvec_qual_S6_SAR <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { sum( list_buoy_data1[[3]][[7]][ list_Lvec_buoy_samp[[3]] ][ unlist( list_breaks[[3]][mat_slot_J3S6SAR[,x]] ) ] ) > 0 } )
#      Lvec_qual_S6_SAR_ALL <- c(Lvec_qual_S6_SAR_ALL,Lvec_qual_S6_SAR)
#
## Plot.
##      plot(vec_Q50_J3,vec_Q50_S6_SAR,xlim=c(0,12),ylim=c(0,12),main=paste(lab_month[m_idx]," N=",length(vec_Q50_J3),sep=""))
##      abline(a=0,b=1)
#   }
#
#   }
#
#   }
#
## Fix for NULL list output.
#   #vec_Q50_S6_LRM_ALL <- numeric(length(vec_Q50_J3_ALL))
#   #AA <- sapply(X=1:length(vec_Q50_S6_LRM_ALL),FUN=function(x) if ( is.null(vec_Q50_S6_LRM_ALL1[[x]]) ) { vec_Q50_S6_LRM_ALL[x] <- NA  } else { vec_Q50_S6_LRM_ALL[x] <- vec_Q50_S6_LRM_ALL1[[x]] } )
#   #vec_Q50_S6_LRM_ALL <- sapply(X=1:length(vec_Q50_J3_ALL),FUN=function(x) if ( is.null(vec_Q50_S6_LRM_ALL1[[x]]) ) { NA } else { vec_Q50_S6_LRM_ALL1[[x]] } )
#   vec_Q50_S6_LRM_ALL <- vec_Q50_S6_LRM_ALL1
#   Lvec_qual_J3_ALL <- Lvec_qual_numval_J3_ALL
#
##=================================================================================================#
## Plotting.
##-------------------------------------------------------------------------------------------------#
## Data frame.
#   df_plot <- data.frame(buoy_hs=vec_buoy_hs_coloc_ALL,buoy_ap=vec_buoy_ap_coloc_ALL,J3=vec_Q50_J3_ALL,S6LRM=vec_Q50_S6_LRM_ALL,S6SAR=vec_Q50_S6_SAR_ALL,J3mS6LRM=vec_Q50_J3_ALL-vec_Q50_S6_LRM_ALL)
##-------------------------------------------------------------------------------------------------#
#   if ( flag_multi_scatter ) {
## File name.
#   if ( flag_period_thresh ) {
#      #fig_file_name <- paste("./figures/swell_test/multiscatter_",length(b_idx_list),"_",buoy_list[buoy_idx],"_",buoy_radius,"km_numval_NOSWELL.png",sep="")
#      fig_file_name <- paste("./figures/multiscatter/multiscatter_",length(b_idx_list),"_",buoy_list[buoy_idx],"_",buoy_radius,"km_numval_NOSWELL.png",sep="")
#   } else {
#      #fig_file_name <- paste("./figures/swell_test/multiscatter_",length(b_idx_list),"_",buoy_list[buoy_idx],"_",buoy_radius,"km_numval.png",sep="")
#      #fig_file_name <- paste("./figures/CMEMS/multiscatter_",length(b_idx_list),"_",buoy_list[buoy_idx],"_",buoy_radius,"km_numval_NDBC.png",sep="")
#      fig_file_name <- paste("./figures/multiscatter/multiscatter_",length(b_idx_list),"_",buoy_list[buoy_idx],"_",buoy_radius,"km_numval_SWELL.png",sep="")
#   }
#
#   png(fig_file_name, width = 3400, height = 3400)
#   par(mfrow=c(4,4),oma=c(8,8,12,9),mar=c(12,14,9,9),mgp=c(9,4,0))
#
##-------------------------------------------------------------------------------------------------#
## Line 1 (Buoy)
##-------------------------------------------------------------------------------------------------#
## Histogram: buoy
#   hist(vec_buoy_hs_coloc_ALL,breaks=10,
#        main="Buoy",xlab="Hs (m)",cex.lab=5,cex.axis=5,cex.main=6)
##-------------------------------------------------------------------------------------------------#
## Plot Buoy/J3 ALL.
#   vec_J3_cols <- rep("black",length(Lvec_qual_J3_ALL))
#   vec_J3_cols[Lvec_qual_J3_ALL] <- "red"
#   vec_J3_pch <- rep(1,length(Lvec_qual_J3_ALL))
#   vec_J3_pch[Lvec_qual_J3_ALL] <- 19
## Pairs:
#   Lvec_pair_idx <- sapply( X=1:length(vec_buoy_hs_coloc_ALL), FUN=function(x) all(!is.na(cbind(vec_buoy_hs_coloc_ALL[x],vec_Q50_J3_ALL[x]))) )
#   if ( flag_period_thresh ) {
#      Lvec_pair_idx <- Lvec_pair_idx & (df_plot$buoy_ap < period_thresh) & !Lvec_qual_J3_ALL
#      #Lvec_pair_idx <- Lvec_pair_idx & (df_plot$buoy_ap > period_thresh) & !Lvec_qual_J3_ALL
#   } else {
#      Lvec_pair_idx <- Lvec_pair_idx & !Lvec_qual_J3_ALL
#   }
#   Lvec_pair_idx[is.na(Lvec_pair_idx)] <- FALSE
#   i_N <- sum( Lvec_pair_idx )
#
#   #plot(vec_buoy_hs_coloc_ALL[Lvec_pair_idx],vec_Q50_J3_ALL[Lvec_pair_idx],xlim=c(0,12),ylim=c(0,12),pch=vec_J3_pch,col=vec_J3_cols,
#   plot(vec_buoy_hs_coloc_ALL[Lvec_pair_idx],vec_Q50_J3_ALL[Lvec_pair_idx],xlim=c(0,10),ylim=c(0,10),
#        main=paste("N=",i_N,sep=""),
#        xlab="Buoy",ylab="J-3",cex=4,cex.lab=5,cex.axis=5,cex.main=6)
#   abline(a=0,b=1)
#
#   mtext(text=paste("Correlation: ",format(cor(df_plot$buoy_hs[Lvec_pair_idx],df_plot$J3[Lvec_pair_idx],use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("Mean bias: ",format(mean(-df_plot$buoy_hs[Lvec_pair_idx],na.rm=T)+mean(df_plot$J3[Lvec_pair_idx],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("RMSE: ",format(mean(sqrt(lm(J3 ~ buoy_hs,data=df_plot[Lvec_pair_idx,])$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("SI: ",format(mean(sqrt(lm(J3 ~ buoy_hs,data=df_plot[Lvec_pair_idx,])$residuals^2))/mean(df_plot$buoy_hs[Lvec_pair_idx]),digits=3),sep=''), side=3, line=-12, adj=0.03, cex=3, outer=FALSE)
##
##-------------------------------------------------------------------------------------------------#
### Plot Buoy/S6 LRM ALL.
##   vec_S6_cols <- rep("black",length(Lvec_qual_S6_LRM_ALL))
##   vec_S6_cols[Lvec_qual_S6_LRM_ALL] <- "red"
##   vec_S6_pch <- rep(1,length(Lvec_qual_S6_LRM_ALL))
##   vec_S6_pch[Lvec_qual_S6_LRM_ALL] <- 19
## Pairs:
#   Lvec_pair_idx <- sapply( X=1:length(vec_buoy_hs_coloc_ALL), FUN=function(x) all(!is.na(cbind(vec_buoy_hs_coloc_ALL[x],vec_Q50_S6_LRM_ALL[x]))) )
#   i_N <- sum( Lvec_pair_idx )
#
#   plot(vec_buoy_hs_coloc_ALL,vec_Q50_S6_LRM_ALL,xlim=c(0,12),ylim=c(0,12),pch=vec_S6_pch,col=vec_S6_cols,
#        main=paste("N=",i_N,sep=""),
#        xlab="Buoy",ylab="S-6 LRM",cex=4,cex.lab=5,cex.axis=5,cex.main=6)
#   abline(a=0,b=1)
#
#   mtext(text=paste("Correlation: ",format(cor(df_plot$buoy_hs,df_plot$S6LRM,use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("Mean bias: ",format(-mean(df_plot$buoy_hs[Lvec_pair_idx],na.rm=T)+mean(df_plot$S6LRM[Lvec_pair_idx],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("RMSE: ",format(mean(sqrt(lm(S6LRM ~ buoy_hs,data=df_plot)$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)
#
##-------------------------------------------------------------------------------------------------#
### Plot Buoy/S6 SAR ALL.
##   vec_S6_cols <- rep("black",length(Lvec_qual_S6_ALL))
##   vec_S6_cols[Lvec_qual_S6_ALL] <- "red"
##   vec_S6_pch <- rep(1,length(Lvec_qual_S6_ALL))
##   vec_S6_pch[Lvec_qual_S6_ALL] <- 19
## Pairs:
#   Lvec_pair_idx <- sapply( X=1:length(vec_buoy_hs_coloc_ALL), FUN=function(x) all(!is.na(cbind(vec_buoy_hs_coloc_ALL[x],vec_Q50_S6_SAR_ALL[x]))) )
#   i_N <- sum( Lvec_pair_idx )
#
#   plot(vec_buoy_hs_coloc_ALL,vec_Q50_S6_SAR_ALL,xlim=c(0,12),ylim=c(0,12),pch=vec_S6_pch,col=vec_S6_cols,
#        main=paste("N=",i_N,sep=""),
#        xlab="Buoy",ylab="S-6 SAR",cex=4,cex.lab=5,cex.axis=5,cex.main=6)
#   abline(a=0,b=1)
#
#   mtext(text=paste("Correlation: ",format(cor(df_plot$buoy_hs,df_plot$S6SAR,use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("Mean bias: ",format(-mean(df_plot$buoy_hs[Lvec_pair_idx],na.rm=T)+mean(df_plot$S6SAR[Lvec_pair_idx],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("RMSE: ",format(mean(sqrt(lm(S6SAR ~ buoy_hs,data=df_plot)$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)
#
##-------------------------------------------------------------------------------------------------#
## Line 2 (J3)
##-------------------------------------------------------------------------------------------------#
## Plot blank.
#   plot(NULL,xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="")
#
##-------------------------------------------------------------------------------------------------#
## Histogram: J3
#   hist(vec_Q50_J3_ALL,breaks=10,
#        main="J3",xlab="Hs (m)",cex.lab=5,cex.axis=5,cex.main=6)
##-------------------------------------------------------------------------------------------------#
## Plot J3/S6 LRM ALL.
## Pairs:
#   Lvec_pair_idx <- sapply( X=1:length(vec_Q50_J3_ALL), FUN=function(x) all(!is.na(cbind(vec_Q50_J3_ALL[x],vec_Q50_S6_LRM_ALL[x]))) )
#   if ( flag_period_thresh ) {
#      Lvec_pair_idx <- Lvec_pair_idx & (df_plot$buoy_ap < period_thresh) & !Lvec_qual_J3_ALL
#      #Lvec_pair_idx <- Lvec_pair_idx & (df_plot$buoy_ap > period_thresh) & !Lvec_qual_J3_ALL
#   } else {
#      Lvec_pair_idx <- Lvec_pair_idx & !Lvec_qual_J3_ALL
#   }
#   Lvec_pair_idx[is.na(Lvec_pair_idx)] <- FALSE
#   i_N <- sum( Lvec_pair_idx )
#
#   #plot(vec_Q50_J3_ALL[Lvec_pair_idx],vec_Q50_S6_LRM_ALL[Lvec_pair_idx],xlim=c(0,6),ylim=c(0,6),pch=vec_J3_pch,col=vec_J3_cols,
#   plot(vec_Q50_J3_ALL[Lvec_pair_idx],vec_Q50_S6_LRM_ALL[Lvec_pair_idx],xlim=c(0,10),ylim=c(0,10),pch=1,
#        main=paste("N=",i_N,sep=""),
#        xlab="J-3",ylab="S-6 LRM",cex=4,cex.lab=5,cex.axis=5,cex.main=6)
#   abline(a=0,b=1)
#
#   mtext(text=paste("Correlation: ",format(cor(df_plot$J3[Lvec_pair_idx],df_plot$S6LRM[Lvec_pair_idx],use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("Mean bias: ",format(-mean(df_plot$J3[Lvec_pair_idx],na.rm=T)+mean(df_plot$S6LRM[Lvec_pair_idx],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("RMSE: ",format(mean(sqrt(lm(S6LRM ~ J3,data=df_plot[Lvec_pair_idx,])$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("SI: ",format(mean(sqrt(lm(S6LRM ~ J3,data=df_plot[Lvec_pair_idx,])$residuals^2))/mean(df_plot$J3[Lvec_pair_idx]),digits=3),sep=''), side=3, line=-12, adj=0.03, cex=3, outer=FALSE)
#
##-------------------------------------------------------------------------------------------------#
## Plot J3/S6 SAR ALL.
#   Lvec_pair_idx <- sapply( X=1:length(vec_Q50_J3_ALL), FUN=function(x) all(!is.na(cbind(vec_Q50_J3_ALL[x],vec_Q50_S6_SAR_ALL[x]))) )
#   if ( flag_period_thresh ) {
#      Lvec_pair_idx <- Lvec_pair_idx & (df_plot$buoy_ap < period_thresh) & !Lvec_qual_J3_ALL
#      #Lvec_pair_idx <- Lvec_pair_idx & (df_plot$buoy_ap > period_thresh) & !Lvec_qual_J3_ALL
#   } else {
#      Lvec_pair_idx <- Lvec_pair_idx & !Lvec_qual_J3_ALL
#   }
#   Lvec_pair_idx[is.na(Lvec_pair_idx)] <- FALSE
#   i_N <- sum( Lvec_pair_idx )
#
#   #plot(vec_Q50_J3_ALL[Lvec_pair_idx],vec_Q50_S6_SAR_ALL[Lvec_pair_idx],xlim=c(0,6),ylim=c(0,6),pch=vec_J3_pch,col=vec_J3_cols,
#   plot(vec_Q50_J3_ALL[Lvec_pair_idx],vec_Q50_S6_SAR_ALL[Lvec_pair_idx],xlim=c(0,10),ylim=c(0,10),pch=1,
#        main=paste("N=",i_N,sep=""),
#        xlab="J-3",ylab="S-6 SAR",cex=4,cex.lab=5,cex.axis=5,cex.main=6)
#   abline(a=0,b=1)
#
#   mtext(text=paste("Correlation: ",format(cor(df_plot$J3[Lvec_pair_idx],df_plot$S6SAR[Lvec_pair_idx],use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("Mean bias: ",format(-mean(df_plot$J3[Lvec_pair_idx],na.rm=T)+mean(df_plot$S6SAR[Lvec_pair_idx],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("RMSE: ",format(mean(sqrt(lm(S6SAR ~ J3,data=df_plot[Lvec_pair_idx,])$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("SI: ",format(mean(sqrt(lm(S6SAR ~ J3,data=df_plot[Lvec_pair_idx,])$residuals^2))/mean(df_plot$J3[Lvec_pair_idx]),digits=3),sep=''), side=3, line=-12, adj=0.03, cex=3, outer=FALSE)
#
##-------------------------------------------------------------------------------------------------#
## Line 3 (S6 LRM)
##-------------------------------------------------------------------------------------------------#
## Plot blank.
#   plot(NULL,xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="")
#
##-------------------------------------------------------------------------------------------------#
## Plot blank.
#   plot(NULL,xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="")
#
##-------------------------------------------------------------------------------------------------#
## Histogram: S6 LRM
#   hist(vec_Q50_S6_SAR_ALL,breaks=10,
#        main="S6 LRM",xlab="Hs (m)",cex.lab=5,cex.axis=5,cex.main=6)
##-------------------------------------------------------------------------------------------------#
## Plot S6 LRM/S6 SAR ALL.
## Pairs:
#   Lvec_pair_idx <- sapply( X=1:length(vec_Q50_S6_LRM_ALL), FUN=function(x) all(!is.na(cbind(vec_Q50_S6_LRM_ALL[x],vec_Q50_S6_SAR_ALL[x]))) )
#   i_N <- sum( Lvec_pair_idx )
#
#   plot(vec_Q50_S6_LRM_ALL,vec_Q50_S6_SAR_ALL,xlim=c(0,12),ylim=c(0,12),pch=vec_J3_pch,col=vec_J3_cols,
#        main=paste("N=",i_N,sep=""),
#        xlab="S-6 LRM",ylab="S-6 SAR",cex=4,cex.lab=5,cex.axis=5,cex.main=6)
#   abline(a=0,b=1)
#
#   mtext(text=paste("Correlation: ",format(cor(df_plot$S6LRM,df_plot$S6SAR,use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("Mean bias: ",format(-mean(df_plot$S6LRM[Lvec_pair_idx],na.rm=T)+mean(df_plot$S6SAR[Lvec_pair_idx],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("RMSE: ",format(mean(sqrt(lm(S6SAR ~ S6LRM,data=df_plot)$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)
#
##-------------------------------------------------------------------------------------------------#
## Plot blank.
#   plot(NULL,xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="")
#
##-------------------------------------------------------------------------------------------------#
## Plot blank.
#   plot(NULL,xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="")
#
##-------------------------------------------------------------------------------------------------#
## Plot blank.
#   plot(NULL,xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="")
#
##-------------------------------------------------------------------------------------------------#
## Histogram: S6 SAR
#   hist(vec_Q50_S6_LRM_ALL,breaks=10,
#        main="S6 SAR",xlab="Hs (m)",cex.lab=5,cex.axis=5,cex.main=6)
#
##-------------------------------------------------------------------------------------------------#
## Top title.
#   #mtext(text=paste("2020/12 - 2021/12 Buoy: ",paste(buoy_list[b_idx_list],collapse=","),sep=""), side=3, line=3, adj=0.05, cex=5, outer=TRUE)
#   mtext(text=paste("2020/12 - 2021/12",str_region,sep=""), side=3, line=3, adj=0.05, cex=5, outer=TRUE)
#
#   dev.off()
#   system(paste("okular",fig_file_name,"&> /dev/null &"))
#
#   }
#
##=================================================================================================#
## Triple collocation:
#   if ( flag_coloc ) {
#      require(ggplot2)
## Labels for the tandem data.
#      vec_data_var <- c("vec_Q50_J3_ALL_CAL","vec_Q50_S6_LRM_ALL_CAL","vec_Q50_S6_SAR_ALL_CAL")
#      vec_data_lab <- c("J-3","S-6_LRM","S-6_SAR")
#
## Remove statistical outliers TBD.
## AA <- df_plot$buoy_hs - df_plot$S6SAR; BB <- 3*sqrt(var(AA,na.rm=T)); which( abs(AA - mean(AA,na.rm=T)) > BB )
#
## Find coloc data by removing all NA colocs.
#      Lvec_coloc_idx_master <- sapply( X=1:length(vec_Q50_J3_ALL), FUN=function(x) all(!is.na(cbind(vec_Q50_J3_ALL[x],vec_Q50_S6_SAR_ALL[x],vec_buoy_hs_coloc_ALL[x],vec_ERA5_hs_coloc_ALL[x]))) )
#      if ( flag_period_thresh ) {
#         if ( flag_swell_only ) {
#            Lvec_coloc_idx_master <- Lvec_coloc_idx_master & (df_plot$buoy_ap > period_thresh) & !Lvec_qual_J3_ALL
#         } else {
#            Lvec_coloc_idx_master <- Lvec_coloc_idx_master & (df_plot$buoy_ap < period_thresh) & !Lvec_qual_J3_ALL
#         }
#      } else {
#         Lvec_coloc_idx_master <- Lvec_coloc_idx_master & !Lvec_qual_J3_ALL
#      }
#
### Calibration from Matt's file.
##    if math.fabs(mdn_x2-mdn_x1)>0.2:
##        x2=x2-(mdn_x2-mdn_x1)
##        
##    if math.fabs(mdn_x3-mdn_x1)>0.2:
##        x3=x3-(mdn_x3-mdn_x1)
## Calibration w.r.t buoys.
#      fl_calib_thresh <- 0.2
## J3.
#      vec_med_diff <- abs( median(vec_Q50_J3_ALL[Lvec_coloc_idx_master],na.rm=T) - median(vec_buoy_hs_coloc_ALL[Lvec_coloc_idx_master],na.rm=T) )
#      if ( abs( vec_med_diff ) > fl_calib_thresh ) {
#         vec_Q50_J3_ALL_CAL <- vec_Q50_J3_ALL - vec_med_diff
#      } else {
#         vec_Q50_J3_ALL_CAL <- vec_Q50_J3_ALL
#      }
## S6LRM.
#      vec_med_diff <- abs( median(vec_Q50_S6_LRM_ALL[Lvec_coloc_idx_master],na.rm=T) - median(vec_buoy_hs_coloc_ALL[Lvec_coloc_idx_master],na.rm=T) )
#      if ( abs( vec_med_diff ) > fl_calib_thresh ) {
#         vec_Q50_S6_LRM_ALL_CAL <- vec_Q50_S6_LRM_ALL - vec_med_diff
#      } else {
#         vec_Q50_S6_LRM_ALL_CAL <- vec_Q50_S6_LRM_ALL
#      }
## S6SAR.
#      vec_med_diff <- abs( median(vec_Q50_S6_SAR_ALL[Lvec_coloc_idx_master],na.rm=T) - median(vec_buoy_hs_coloc_ALL[Lvec_coloc_idx_master],na.rm=T) )
#      if ( abs( vec_med_diff ) > fl_calib_thresh ) {
#         vec_Q50_S6_SAR_ALL_CAL <- vec_Q50_S6_SAR_ALL - vec_med_diff
#      } else {
#         vec_Q50_S6_SAR_ALL_CAL <- vec_Q50_S6_SAR_ALL
#      }
## ERA5.
#      vec_med_diff <- abs( median(vec_ERA5_hs_coloc_ALL[Lvec_coloc_idx_master],na.rm=T) - median(vec_buoy_hs_coloc_ALL[Lvec_coloc_idx_master],na.rm=T) )
#      if ( abs( vec_med_diff ) > fl_calib_thresh ) {
#         vec_ERA5_hs_coloc_ALL_CAL <- vec_ERA5_hs_coloc_ALL - vec_med_diff
#      } else {
#         vec_ERA5_hs_coloc_ALL_CAL <- vec_ERA5_hs_coloc_ALL
#      }
#
## Loop over omission of tandem datasets.
#      df_plot_data <- NULL
#      for (i_data in 1:3) {
## Bootstrap uncertainty.
## Get the indices of the triplets.
#         vec_coloc_idx <- which(Lvec_coloc_idx_master)
#         mat_D <- matrix(NA,ncol=3,nrow=length(Lvec_coloc_idx_master))
#
#         n_samp <- 10000
#         mat_sqrt <- matrix(NA,ncol=3,nrow=n_samp)
#
#         for (ii in 1:n_samp) {
#            vec_idx <- sample(vec_coloc_idx,replace=TRUE)
#            D1 <- eval(parse(text=paste(vec_data_var[i_data],"[vec_idx]",sep="")))
#            #D2 <- vec_Q50_J3_ALL[vec_idx]
#            D2 <- vec_ERA5_hs_coloc_ALL_CAL[vec_idx]
#            D3 <- vec_buoy_hs_coloc_ALL[vec_idx]
#
#            #D1 <- vec_Q50_J3_ALL[vec_idx]
#            ##D2 <- vec_Q50_S6_SAR_ALL[vec_idx]
#            ##D2 <- vec_Q50_S6_LRM_ALL[Lvec_pair_idx]
#            #D2 <- vec_ERA5_hs_coloc_ALL[vec_idx]
#            #D3 <- vec_buoy_hs_coloc_ALL[vec_idx]
#
#            V12 <- var(D1-D2,na.rm=T)
#            V31 <- var(D3-D1,na.rm=T)
#            V23 <- var(D2-D3,na.rm=T)
#
#            mat_sqrt[ii,1] <- sqrt( (V12+V31-V23)/2 )
#            mat_sqrt[ii,2] <- sqrt( (V23+V12-V31)/2 )
#            mat_sqrt[ii,3] <- sqrt( (V31+V23-V12)/2 )
#         }
#         df_plot_data_temp <- rbind(
#			            data.frame(mean_e=mean(mat_sqrt[,1],na.rm=T),sd_e=sqrt(var(mat_sqrt[,1],na.rm=T)),mission=vec_data_lab[i_data],group=i_data,n_coloc=sum(Lvec_coloc_idx_master,na.rm=T)),
#			            data.frame(mean_e=mean(mat_sqrt[,2],na.rm=T),sd_e=sqrt(var(mat_sqrt[,2],na.rm=T)),mission="ERA5",group=i_data,n_coloc=sum(Lvec_coloc_idx_master,na.rm=T)),
#			            data.frame(mean_e=mean(mat_sqrt[,3],na.rm=T),sd_e=sqrt(var(mat_sqrt[,3],na.rm=T)),mission="Buoys",group=i_data,n_coloc=sum(Lvec_coloc_idx_master,na.rm=T)) )
#         df_plot_data <- rbind(df_plot_data,df_plot_data_temp)
#         #array_mean_e[,i_data,JJ] <- df_plot_data_temp$mean_e
#      }
## Plot bar charts.
#      if ( flag_period_thresh ) {
#         if ( flag_swell_only ) {
#            fig_file_name <- paste("./figures/bar_plots/",str_region,"_",buoy_radius,"km_numval_SWELL.png",sep="")
#         } else {
#            fig_file_name <- paste("./figures/bar_plots/",str_region,"_",buoy_radius,"km_numval_NOSWELL.png",sep="")
#         }
#      } else {
#         fig_file_name <- paste("./figures/bar_plots/",str_region,"_",buoy_radius,"km_numval.png",sep="")
#      }
## Plotting.
#      p1 <- ggplot(df_plot_data,aes(x = as.factor(group), y = mean_e, fill = as.factor(mission))) + 
#      geom_col(position = "dodge") +
#      geom_errorbar(aes(ymin = mean_e-sd_e, ymax = mean_e+sd_e), width=0.2,
#                    position=position_dodge(0.9)) +
#      ylim(0,0.6) +
#      ggtitle(paste(str_region," ",buoy_radius," km [N_coloc=",df_plot_data$n_coloc[1],"]",sep="")) +
#      labs(y="Mean error (m)",fill='Dataset') +
#      
#      theme(
#	    plot.title = element_text(size = 70,hjust = 0.5),
#	    axis.title.x=element_blank(),
#            axis.title.y=element_text(size = 50),
#            #panel.grid.minor = element_blank(),
#            #panel.grid.major = element_blank(),
#            #panel.background = element_rect(fill = "black"),
#
#            strip.text = element_text(size = 50, margin = margin(25,0,25,0)),
#            strip.background = element_rect(fill = "white"),
#            panel.spacing.x = unit(1, "lines"),
#            panel.spacing.y = unit(2, "lines"),
#            axis.text.y = element_text(size = 50),
#            axis.text.x = element_text(size = 50),
#            axis.ticks.x = element_blank(),
#
#            legend.position = "left",
#            legend.margin = margin(0,75,0,0),
#            legend.key.width = unit(1.5, "inch"),
#            legend.key.height = unit(2, "inch"),
#            legend.title = element_text(size = 50, margin = margin(25,0,0,0)),
#            legend.title.align = 0.5,
#            legend.text = element_text(size = 40, margin = margin(0,0,0,25))
#         )
#
#      png(fig_file_name, width = 2200, height = 1800)
#      #grid.arrange(p2,p1,ncol=2)
#      plot(p1)
#      dev.off()
#      system(paste("okular",fig_file_name,"&> /dev/null &"))
#
### Plot histograms.
##      X11()
##      par(mfrow=c(1,3))
##      hist(mat_sqrt[,1])
##      hist(mat_sqrt[,2])
##      hist(mat_sqrt[,3])
## Mean uncertainty.
##      print(paste("MEAN 1:",mean(mat_sqrt[,1],na.rm=T)))
##      print(paste("MEAN 2:",mean(mat_sqrt[,2],na.rm=T)))
##      print(paste("MEAN 3:",mean(mat_sqrt[,3],na.rm=T)))
## S.D. for variance uncertainty.
##      print(paste("SQRT 1:",sqrt(var(mat_sqrt[,1],na.rm=T))))
##      print(paste("SQRT 2:",sqrt(var(mat_sqrt[,2],na.rm=T))))
##      print(paste("SQRT 3:",sqrt(var(mat_sqrt[,3],na.rm=T))))
#   }
#
### Set 1 = J3
### Set 2 = S6 SAR
### Set 3 = Buoy
##   D1 <- vec_Q50_J3_ALL[vec_coloc_idx]
##   #D2 <- vec_Q50_S6_LRM_ALL[vec_coloc_idx]
##   #D2 <- vec_Q50_S6_SAR_ALL[Lvec_pair_idx]
##   D2 <- vec_ERA5_hs_coloc_ALL[vec_coloc_idx]
##   D3 <- vec_buoy_hs_coloc_ALL[vec_coloc_idx]
##
##   V12 <- var(D1-D2,na.rm=T)
##   V31 <- var(D3-D1,na.rm=T)
##   V23 <- var(D2-D3,na.rm=T)
##
##   sqrt( (V12+V31-V23)/2 )
##   sqrt( (V23+V12-V31)/2 )
##   sqrt( (V31+V23-V12)/2 )
#
###=================================================================================================#
### Plot bad data.
##   vec_cols <- c("black","red")
##   date_range <- c( min( as.POSIXct( list_buoy_data1[[1]][[4]][ list_buoy_data1[[1]][[6]][[5]] ], origin = '2000-01-01', tz='GMT') ),
##                    max( as.POSIXct( list_buoy_data1[[2]][[4]][ list_buoy_data1[[2]][[6]][[5]] ], origin = '2000-01-01', tz='GMT') ) )
##   lat_range <- c( min( list_buoy_data1[[1]][[2]][ list_buoy_data1[[1]][[6]][[5]] ] ),
##                   max( list_buoy_data1[[3]][[2]][ list_buoy_data1[[3]][[6]][[5]] ] ) )
##   X11()
### Latitude.
##   plot( list_buoy_data1[[1]][[2]][ list_buoy_data1[[1]][[6]][[5]] ],
##         list_buoy_data1[[1]][[5]][ list_buoy_data1[[1]][[6]][[5]] ],
##         xlim=lat_range,ylim=c(0,18),xlab="Latitude",ylab="Hs (m)",
##         pch=19, col=vec_cols[ 1+list_buoy_data1[[1]][[7]][ list_buoy_data1[[1]][[6]][[5]] ] ] )
##   points( list_buoy_data1[[3]][[2]][ list_buoy_data1[[3]][[6]][[5]] ],
##         list_buoy_data1[[3]][[5]][ list_buoy_data1[[3]][[6]][[5]] ],
##         pch=17, col=vec_cols[ 1+list_buoy_data1[[3]][[7]][ list_buoy_data1[[3]][[6]][[5]] ] ] )
### Time.
##   #plot( as.POSIXct( list_buoy_data1[[1]][[4]][ list_buoy_data1[[1]][[6]][[5]] ], origin = '2000-01-01', tz='GMT'),
##   #      list_buoy_data1[[1]][[5]][ list_buoy_data1[[1]][[6]][[5]] ],
##   #      xlim=date_range,ylim=c(0,18),xlab="Time sequence (s) - LABELS INCORRECT",ylab="Hs (m)",
##   #      pch=19, col=vec_cols[ 1+list_buoy_data1[[1]][[7]][ list_buoy_data1[[1]][[6]][[5]] ] ] )
##   #points( as.POSIXct( list_buoy_data1[[3]][[4]][ list_buoy_data1[[3]][[6]][[5]] ], origin = '2000-01-01', tz='GMT'),
##   #      list_buoy_data1[[3]][[5]][ list_buoy_data1[[3]][[6]][[5]] ],
##   #      pch=17, col=vec_cols[ 1+list_buoy_data1[[3]][[7]][ list_buoy_data1[[3]][[6]][[5]] ] ] )
##   abline(h=vec_buoy_hs_coloc[5],col="blue",lwd=2)
##   legend(x=date_range[1],y=18,legend=c("J3","S6 LRM","Buoy","BAD"),pch=c(19,17,NA,19),lty=c(NA,NA,1,NA),col=c("black","black","blue","red"))
##   #legend(x=date_range[1],y=18,legend=c("J3","S6 SAR","S6 LRM","Buoy","BAD"),pch=c(19,18,17,NA,19),lty=c(NA,NA,NA,1,NA),col=c("black","black","black","blue","red"))
##
#
#
#   if ( flag_plot_junk ) {
## Plots against (buoy) wave period.
#   X11()
#   par(mfrow=c(1,2))
#   lm_BHsJ3mS6 <- lm(J3mS6LRM ~ buoy_hs, data=df_plot)
#   plot(vec_buoy_hs_coloc_ALL,df_plot$J3-df_plot$S6LRM,ylim=c(-0.25,0.25),xlab="Buoy Hs (m)",ylab="J3 - S6 LRM (m)"); abline(h=0)
#   abline(lm_BHsJ3mS6,col="blue",lwd=2)
#
#   lm_BTmJ3mS6 = lm(J3mS6LRM ~ buoy_ap, data=df_plot)
#   plot(vec_buoy_ap_coloc_ALL,df_plot$J3-df_plot$S6LRM,ylim=c(-0.25,0.25),xlab="Buoy Tm2 (s)",ylab="J3 - S6 LRM (m)"); abline(h=0)
#   abline(lm_BTmJ3mS6,col="blue",lwd=2)
#
#   mtext(paste("Buoy",buoy_list[buoy_idx]),outer=TRUE,cex=2,line=-3)
#
## S6 SAR.
#   lm2_BHsS6 = lmodel2(S6SAR ~ buoy_hs, data=df_plot, "relative", "relative", nperm=99)
#   lm_BHsS6 = lm(S6SAR ~ buoy_hs, data=df_plot)
#
#   X11()
#   par(mfrow=c(2,2))
#   plot(lm_BHsS6)
#
#   X11(); plot(lm2_BHsS6,method="OLS",xlab="Buoy Hs (m)",ylab="S6 SAR",xlim=c(0,10),ylim=c(0,10)); abline(0,1)
#
#   mtext(paste("Buoy",buoy_list[buoy_idx],": Buoy ~ S6_SAR regression"),outer=TRUE,cex=2,line=-2)
#
## Corrected S6 SAR.
#   BB2 <- df_plot$S6SAR / lm2_BHsS6$regression.results[1,3] - lm2_BHsS6$regression.results[1,2]
#   df_plot1 <- cbind(df_plot,J3mS6SAR=df_plot$J3-df_plot$S6SAR,S6SAR_c=df_plot$J3-BB2)
#   lm_J3mS6SAR = lm(J3mS6SAR ~ buoy_hs, data=df_plot1)
#
#   X11()
#   par(mfrow=c(2,2))
#   plot(vec_buoy_hs_coloc_ALL,df_plot1$J3mS6SAR,ylim=c(-1.0,0.0),xlab="Buoy Hs (m)",ylab="J3 - S6 SAR (m)"); abline(h=0)
#   abline(lm_J3mS6SAR,col="blue",lwd=2)
#
#   plot(vec_buoy_ap_coloc_ALL,df_plot1$J3mS6SAR,ylim=c(-1.0,0.0),xlab="Buoy Tm2 (s)",ylab="J3 - S6 SAR (m)"); abline(h=0)
#
#   lm2_BHsS6_corr = lmodel2(S6SAR_c ~ buoy_hs, data=df_plot1, "interval", "interval", nperm=99)
#   plot(vec_buoy_hs_coloc_ALL,df_plot1$S6SAR_c,ylim=c(-0.50,0.50),xlab="Buoy Hs (m)",ylab="J3 - S6 SAR_corr (m)"); abline(h=0)
#   abline(a=lm2_BHsS6_corr$regression.results[1,2],b=lm2_BHsS6_corr$regression.results[1,3],col="blue",lwd=2)
#
#   lm2_BTmS6_corr = lmodel2(S6SAR_c ~ buoy_ap, data=df_plot1, "interval", "relative", nperm=99)
#   plot(vec_buoy_ap_coloc_ALL,df_plot1$S6SAR_c,ylim=c(-0.50,0.50),xlab="Buoy Tm2 (s)",ylab="J3 - S6 SAR_corr (m)"); abline(h=0)
#   abline(a=lm2_BTmS6_corr$regression.results[1,2],b=lm2_BTmS6_corr$regression.results[1,3],col="blue",lwd=2)
#
#   mtext(paste("Buoy",buoy_list[buoy_idx]),outer=TRUE,cex=2,line=-3)
#
## Joint distribution showing correction.
#   vec_plot_cols <- round(100*df_plot1$S6SAR_c)+abs(min(round(100*df_plot1$S6SAR_c),na.rm=T))+1
#   vec_cols <- viridis(max(vec_plot_cols,na.rm=T))
#
#   X11()
#   plot(df_plot1$buoy_hs,df_plot1$buoy_ap,pch=19,col=vec_cols[vec_plot_cols],xlab="Buoy Hs (m)",ylab="Buoy Tm2 (s)",main=paste("Buoy ",buoy_list[buoy_idx],": colour = J3 - S6",sep=""))
#   #plot(df_plot1$buoy,df_plot1$buoy_ap,pch=19,col=vec_cols[1],xlab="Buoy Hs (m)",ylab="Buoy Tm2 (s)",main=paste("Buoy ",buoy_list[buoy_idx],": colour = J3 - S6",sep=""))
#
## Scatter with buoy_ap colouring.
#   vec_plot_cols <- round( 10*df_plot1$buoy_ap - min(10*df_plot1$buoy_ap,na.rm=T) + 1 )
#   vec_cols <- viridis(max(vec_plot_cols,na.rm=T))
#   X11()
#   plot(df_plot1$buoy_hs,df_plot1$S6SAR_c,pch=19,col=vec_cols[vec_plot_cols],xlab="Buoy Hs (m)",ylab="J3 - S6",main=paste("Buoy ",buoy_list[buoy_idx],": colour = Buoy Tm2 (s)",sep=""))
#
#   }
#
### ggplot
### Remove outliers (J3mS6SAR and buoy_ap)
##   require(ggplot2)
##   X11(); ggplot(df_plot1, aes(buoy_hs, buoy_ap, color = J3mS6SAR)) + geom_point(shape = 16, size = 3, show.legend = TRUE) + scale_colour_viridis_c()
##   X11(); ggplot(df_plot1, aes(buoy_hs, J3mS6SAR, color = buoy_ap)) + geom_point(shape = 16, size = 3, show.legend = TRUE) + scale_colour_viridis_c(option = "plasma",direction=-1)
##
##   lm_J3mS6SAR = lm(J3mS6SAR ~ buoy_hs + I(buoy_hs^2) + I(buoy_ap^2), data=df_plot1)
##   BB <- as.numeric( names( lm_J3mS6SAR$fitted.values ) )
##   vec_temp <- rep(NA,804)
##   vec_temp[BB] <- lm_J3mS6SAR$fitted.values
##
##   lm_S6SAR = lm(S6SAR ~ I(buoy_hs^2) + buoy_ap + I(buoy_ap^3), data=df_plot1)
##   BB <- as.numeric( names( lm_S6SAR$fitted.values ) )
##   vec_temp <- rep(NA,804)
##   vec_temp[BB] <- lm_S6SAR$fitted.values
##
##   df_plot2 <- cbind(df_plot1,multi_reg=vec_temp,S6SAR_C=(df_plot1$S6SAR-vec_temp))
##   X11(); ggplot(df_plot2, aes(buoy_hs, S6SAR_C, color = buoy_ap)) + geom_point(shape = 16, size = 3, show.legend = TRUE) + scale_colour_viridis_c(option = "plasma",direction=-1)
##   X11(); ggplot(df_plot2, aes(buoy_hs, buoy_ap, color = S6SAR_C)) + geom_point(shape = 16, size = 3, show.legend = TRUE) + scale_colour_viridis_c()
#
#
#
