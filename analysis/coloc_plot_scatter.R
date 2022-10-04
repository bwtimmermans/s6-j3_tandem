# source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/coloc_plot_scatter.R")
   set.seed(23458892)
   require(lmodel2)
   require(viridis)

   flag_multi_scatter <- TRUE
   flag_plot_junk <- FALSE

# Attach J3.
   attach("./output/buoys_J3/list_buoy_data_swh_ocean.Robj")
   #attach("./output/buoys_J3/list_buoy_data_swh_ocean_mle3.Robj")
   mat_list_J3 <- list_buoy_data[[2]]
   detach()

# Attach S6 LRM.
   attach("./output/buoys_S6/list_buoy_data_LRM.Robj")
   mat_list_S6_LRM <- list_buoy_data[[2]]
   detach()

# Attach S6 SAR.
   attach("./output/buoys_S6/list_buoy_data_SAR.Robj")
   mat_list_S6_SAR <- list_buoy_data[[2]]
   detach()

   lab_month <- c("Dec (2020)","Jan (2021)","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# as.POSIXct(S6_46246_march[[4]][1:100], origin = '2000-01-01', tz='GMT')
   buoy_list <- c(46246,46085,51004,51001,32012,
                  44011,44137,44139,41010,41040,
                  41044,41048,41049,46006,46082,
                  46083,46084,46059,46002,46005,46001)

   buoy_radius <- 150

   flag_swell <- FALSE
   period_thresh <- 8

# Process sat data.
   vec_Q50_J3_ALL <- vec_Q50_S6_SAR_ALL <- vec_Q50_S6_LRM_ALL1 <- vec_buoy_hs_coloc_ALL <- vec_buoy_ap_coloc_ALL <- NULL
   Lvec_qual_J3_ALL <- Lvec_qual_S6_SAR_ALL <- Lvec_qual_S6_LRM_ALL <- Lvec_qual_numval_J3_ALL <- NULL

# Pacific.
   b_idx_list <- c(1,14,19,20,21)
   b_idx_list <- 20
   for (b_idx in b_idx_list) {
# Atlantic.
   #b_idx_list <- c(9,10,11,12,13)
   #for (b_idx in b_idx_list) {
   #for (b_idx in 1) {

   buoy_idx <- b_idx
#=================================================================================================#
# Load buoy data.
# Buoy time offset: 946684800
#-------------------------------------------------------------------------------------------------#
   #buoy_data_file <- list.files(path = "/home/ben/research/waves/buoy_data/NDBC_complete_records/", pattern = paste("^",buoy_list[buoy_idx],sep="") )
   #buoy_data_file <- "46005_1976-2021_hs.csv"
   buoy_data_file <- "46005_1976-2022_hs1.csv"
   mat_buoy_csv1 <- read.csv(paste("/home/ben/research/waves/buoy_data/NDBC_complete_records/",buoy_data_file,sep=""))
   mat_buoy_csv <- mat_buoy_csv1[!is.na(mat_buoy_csv1$hs),]
   vec_buoy_time <- strptime(as.character(mat_buoy_csv[,1]),format="%Y-%m-%d %H:%M:%S",tz="GMT")
# Get indices for 2020/12 - 2021/12
   date_start_idx <- which( format( as.POSIXct( vec_buoy_time, tz='GMT'), "%Y%m") %in% "202012" )[1]
   date_idx <- date_start_idx:length(vec_buoy_time)

   vec_buoy_time_num <- as.numeric( vec_buoy_time[date_idx] ) - 946684800
   vec_buoy_hs <- mat_buoy_csv$hs[date_idx]
   vec_buoy_ap <- mat_buoy_csv$ap[date_idx]
#-------------------------------------------------------------------------------------------------#
# Sample Hs within ~50 km radius of buoy.
         path_buoy_meta <- paste("/home/ben/research/waves/buoy_data/NDBC_metadata/",buoy_list[buoy_idx],"_meta",sep="")
         if ( buoy_list[buoy_idx] == 41113 | buoy_list[buoy_idx] == 46246 ) {
            com_b_lat <- paste("sed -ne '4p' ",path_buoy_meta," | cut -f1 -d' '",sep="")
            com_b_lon <- paste("sed -ne '4p' ",path_buoy_meta," | cut -f3 -d' '",sep="")
         } else {
            com_b_lat <- paste("sed -ne '5p' ",path_buoy_meta," | cut -f1 -d' '",sep="")
            com_b_lon <- paste("sed -ne '5p' ",path_buoy_meta," | cut -f3 -d' '",sep="")
         }
         df_buoy_loc <- data.frame(lat=as.numeric(system(com_b_lat,intern = TRUE)), lon=-as.numeric(system(com_b_lon,intern = TRUE)), name=buoy_list[buoy_idx])
# Set longitude and latitide in data frame.
         if ( is.null(mat_buoy_csv$longitude[1]) ) {
            mat_buoy_csv$longitude <- df_buoy_loc$lon
            mat_buoy_csv$latitude <- df_buoy_loc$lat
         }
# Code to approximately convert degrees to km.
# Radius of the Earth in km.
         i_radius = 6371
# Function for radians.
         func_rads <- function(x) { x * pi / 180 }
# Function for distance.
         func_buoy_dist <- function(x) {
            #fl_d_lat = func_rads(x[1]) - func_rads(df_buoy_loc$lat)
            #fl_d_lon = func_rads(x[2]) - func_rads(df_buoy_loc$lon)
            fl_d_lat = func_rads(x[1]) - func_rads(x[3])
            fl_d_lon = func_rads(x[2]) - func_rads(x[4])
            fl_h = sin(fl_d_lat / 2) * sin(fl_d_lat / 2) + cos( func_rads(df_buoy_loc$lat) ) * cos( func_rads(x[1]) ) * sin(fl_d_lon / 2) * sin(fl_d_lon / 2)
            2 * i_radius * asin(sqrt(fl_h))
         }

#=================================================================================================#
## Process sat data.
#   vec_Q50_J3_ALL <- vec_Q50_S6_SAR_ALL <- vec_Q50_S6_LRM_ALL1 <- vec_buoy_hs_coloc_ALL <- vec_buoy_ap_coloc_ALL <- NULL
#   Lvec_qual_J3_ALL <- Lvec_qual_S6_SAR_ALL <- Lvec_qual_S6_LRM_ALL <- NULL
## Loop over months 2021 (Jan - Dec).
#   X11()
#   par(mfrow=c(3,4))
   for ( m_idx in 1:13 ) {
      list_buoy_data1 <- list(mat_list_J3[[m_idx,buoy_idx]],mat_list_S6_LRM[[m_idx,buoy_idx]],mat_list_S6_SAR[[m_idx,buoy_idx]])

#-------------------------------------------------------------------------------------------------#
# Find distance from buoy. Loop over data sets.
      list_nc1_breaks_master <- vector(mode = "list",length = 3)
      list_Lvec_buoy_samp <- vector(mode = "list",length = 3)
      for (S_idx in 1:3) {
         vec_sat_buoy_dist <- apply(X=cbind(list_buoy_data1[[S_idx]][[2]],list_buoy_data1[[S_idx]][[3]]-360),MAR=1,FUN=func_buoy_dist)

         list_Lvec_buoy_samp[[S_idx]] <- vec_sat_buoy_dist < buoy_radius
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
         list_nc1_breaks <- list()
         for (i in 1:dim(mat_nc1_breaks)[1]) { list_nc1_breaks[[i]] <- mat_nc1_breaks[i,1]:mat_nc1_breaks[i,2] }
         list_nc1_breaks_master[[S_idx]] <- list_nc1_breaks
      }
#-------------------------------------------------------------------------------------------------#
# Mean time for each segment (J3,S6)
      #list_nc1_breaks_process <- list()
      #for (S_idx in 1:3) {
      #   list_nc1_breaks_process[[S_idx]] <- list_buoy_data1[[S_idx]][[6]]
      #}
      list_nc1_breaks_process <- list_nc1_breaks_master

      list_mean_time <- vector(mode = "list",length = 3)
      for (S_idx in 1:3) {
         list_mean_time[[S_idx]] <- sapply( X=1:length(list_nc1_breaks_process[[S_idx]]), FUN=function(x) { floor( mean( list_buoy_data1[[S_idx]][[4]][ list_Lvec_buoy_samp[[S_idx]] ][ list_nc1_breaks_process[[S_idx]][[x]] ] ) ) } )
      }
      mat_DF <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { abs(list_mean_time[[1]][x] - list_mean_time[[2]]) < 40 } )
      if ( !is.matrix(mat_DF) ) { mat_DF <- t(as.matrix(mat_DF)) }
      mat_DE <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { abs(list_mean_time[[1]][x] - list_mean_time[[3]]) < 40 } )
      if ( !is.matrix(mat_DE) ) { mat_DE <- t(as.matrix(mat_DE)) }
# Find time differences (in seconds).
#      sapply( X=1:length(DD),FUN=function(x) { DD[x] - EE[mat_DE[,x]] } )
# Find matching buoy time.
      mat_GG <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { abs(list_mean_time[[1]][x] - vec_buoy_time_num) < 1800 } )
#if ( length( which(mat_GG,arr.ind=T)[,1] ) < length(DD) ) {
#   print(paste(" Fewer buoy colocs, m_idx:",m_idx))
#}
      vec_buoy_hs_coloc <- vec_buoy_ap_coloc <- rep(NA,length(list_mean_time[[1]]))

      vec_buoy_hs_coloc[which(mat_GG,arr.ind=T)[,2]] <- vec_buoy_hs[ which(mat_GG,arr.ind=T)[,1] ]
      vec_buoy_hs_coloc_ALL <- c(vec_buoy_hs_coloc_ALL,vec_buoy_hs_coloc)

      vec_buoy_ap_coloc[which(mat_GG,arr.ind=T)[,2]] <- vec_buoy_ap[ which(mat_GG,arr.ind=T)[,1] ]
      vec_buoy_ap_coloc_ALL <- c(vec_buoy_ap_coloc_ALL,vec_buoy_ap_coloc)

# Find swh track medians.
# J3.
      #vec_Q50_J3 <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { median( list_buoy_data1[[1]][[5]][ list_buoy_data1[[1]][[6]][[x]] ], na.rm=T ) } )
      vec_Q50_J3 <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { median( list_buoy_data1[[1]][[5]][ list_Lvec_buoy_samp[[1]] ][ list_nc1_breaks_process[[1]][[x]] ], na.rm=T ) } )
      vec_Q50_J3_ALL <- c(vec_Q50_J3_ALL,vec_Q50_J3)
# J3 quality.
      #Lvec_qual_J3 <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { sum( list_buoy_data1[[1]][[7]][ list_buoy_data1[[1]][[6]][[x]] ] ) > 0 } )
      Lvec_qual_J3 <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { sum( list_buoy_data1[[1]][[7]][ list_Lvec_buoy_samp[[1]] ][ list_nc1_breaks_process[[1]][[x]] ] ) > 0 } )
      Lvec_qual_J3_ALL <- c(Lvec_qual_J3_ALL,Lvec_qual_J3)
# RMS
      #Lvec_qual_J3 <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { any( list_buoy_data1[[1]][[9]][ list_Lvec_buoy_samp[[1]] ][ list_nc1_breaks_process[[1]][[x]] ] > 1 ) } )
# NUMVALS
      Lvec_qual_numval_J3 <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { any( list_buoy_data1[[1]][[8]][ list_Lvec_buoy_samp[[1]] ][ list_nc1_breaks_process[[1]][[x]] ] < 9 ) } )
      Lvec_qual_numval_J3_ALL <- c(Lvec_qual_numval_J3_ALL,Lvec_qual_numval_J3)

# S6 LRM.
      #vec_Q50_S6_LRM <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { median( list_buoy_data1[[2]][[5]][ unlist( list_buoy_data1[[2]][[6]][mat_DF[,x]] ) ], na.rm=T ) } )
      vec_Q50_S6_LRM <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { median( list_buoy_data1[[2]][[5]][ list_Lvec_buoy_samp[[2]] ][ unlist( list_nc1_breaks_process[[2]][mat_DF[,x]] ) ], na.rm=T ) } )
      vec_Q50_S6_LRM_ALL1 <- c(vec_Q50_S6_LRM_ALL1,vec_Q50_S6_LRM)
# S6 LRM quality.
      #Lvec_qual_S6_LRM <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { sum( list_buoy_data1[[2]][[7]][ unlist( list_buoy_data1[[2]][[6]][mat_DF[,x]] ) ] ) > 0 } )
      Lvec_qual_S6_LRM <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { sum( list_buoy_data1[[2]][[7]][ list_Lvec_buoy_samp[[2]] ][ unlist( list_nc1_breaks_process[[2]][mat_DF[,x]] ) ] ) > 0 } )
      Lvec_qual_S6_LRM_ALL <- c(Lvec_qual_S6_LRM_ALL,Lvec_qual_S6_LRM)
# S6 SAR.
      #vec_Q50_S6_SAR <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { median( list_buoy_data1[[3]][[5]][ unlist( list_buoy_data1[[3]][[6]][mat_DE[,x]] ) ], na.rm=T ) } )
      vec_Q50_S6_SAR <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { median( list_buoy_data1[[3]][[5]][ list_Lvec_buoy_samp[[3]] ][ unlist( list_nc1_breaks_process[[3]][mat_DE[,x]] ) ], na.rm=T ) } )
      vec_Q50_S6_SAR_ALL <- c(vec_Q50_S6_SAR_ALL,vec_Q50_S6_SAR)
# S6 SAR quality.
      #Lvec_qual_S6_SAR <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { sum( list_buoy_data1[[3]][[7]][ unlist( list_buoy_data1[[3]][[6]][mat_DE[,x]] ) ] ) > 0 } )
      Lvec_qual_S6_SAR <- sapply( X=1:length(list_mean_time[[1]]),FUN=function(x) { sum( list_buoy_data1[[3]][[7]][ list_Lvec_buoy_samp[[3]] ][ unlist( list_nc1_breaks_process[[3]][mat_DE[,x]] ) ] ) > 0 } )
      Lvec_qual_S6_SAR_ALL <- c(Lvec_qual_S6_SAR_ALL,Lvec_qual_S6_SAR)

# Plot.
#      plot(vec_Q50_J3,vec_Q50_S6_SAR,xlim=c(0,12),ylim=c(0,12),main=paste(lab_month[m_idx]," N=",length(vec_Q50_J3),sep=""))
#      abline(a=0,b=1)
   }

   }

# Fix for NULL list output.
   #vec_Q50_S6_LRM_ALL <- numeric(length(vec_Q50_J3_ALL))
   #AA <- sapply(X=1:length(vec_Q50_S6_LRM_ALL),FUN=function(x) if ( is.null(vec_Q50_S6_LRM_ALL1[[x]]) ) { vec_Q50_S6_LRM_ALL[x] <- NA  } else { vec_Q50_S6_LRM_ALL[x] <- vec_Q50_S6_LRM_ALL1[[x]] } )
   #vec_Q50_S6_LRM_ALL <- sapply(X=1:length(vec_Q50_J3_ALL),FUN=function(x) if ( is.null(vec_Q50_S6_LRM_ALL1[[x]]) ) { NA } else { vec_Q50_S6_LRM_ALL1[[x]] } )
   vec_Q50_S6_LRM_ALL <- vec_Q50_S6_LRM_ALL1
   Lvec_qual_J3_ALL <- Lvec_qual_numval_J3_ALL

#=================================================================================================#
# Plotting.
#-------------------------------------------------------------------------------------------------#
# Data frame.
   df_plot <- data.frame(buoy_hs=vec_buoy_hs_coloc_ALL,buoy_ap=vec_buoy_ap_coloc_ALL,J3=vec_Q50_J3_ALL,S6LRM=vec_Q50_S6_LRM_ALL,S6SAR=vec_Q50_S6_SAR_ALL,J3mS6LRM=vec_Q50_J3_ALL-vec_Q50_S6_LRM_ALL)
#-------------------------------------------------------------------------------------------------#
   if ( flag_multi_scatter ) {
# File name.
   if ( flag_swell ) {
      fig_file_name <- paste("./figures/swell_test/multiscatter_",length(b_idx_list),"_",buoy_list[buoy_idx],"_",buoy_radius,"km_numval_NOSWELL.png",sep="")
   } else {
      fig_file_name <- paste("./figures/swell_test/multiscatter_",length(b_idx_list),"_",buoy_list[buoy_idx],"_",buoy_radius,"km_numval.png",sep="")
      fig_file_name <- paste("./figures/CMEMS/multiscatter_",length(b_idx_list),"_",buoy_list[buoy_idx],"_",buoy_radius,"km_numval_NDBC.png",sep="")
   }

   png(fig_file_name, width = 3400, height = 1200)
   par(mfrow=c(1,3),oma=c(8,8,12,9),mar=c(12,14,9,9),mgp=c(9,4,0))

#-------------------------------------------------------------------------------------------------#
# Line 1 (Buoy)
#-------------------------------------------------------------------------------------------------#
## Histogram: buoy
#   hist(vec_buoy_hs_coloc_ALL,breaks=10,
#        main="Buoy",xlab="Hs (m)",cex.lab=5,cex.axis=5,cex.main=6)
#-------------------------------------------------------------------------------------------------#
# Plot Buoy/J3 ALL.
   vec_J3_cols <- rep("black",length(Lvec_qual_J3_ALL))
   vec_J3_cols[Lvec_qual_J3_ALL] <- "red"
   vec_J3_pch <- rep(1,length(Lvec_qual_J3_ALL))
   vec_J3_pch[Lvec_qual_J3_ALL] <- 19
# Pairs:
   Lvec_pair_idx <- sapply( X=1:length(vec_buoy_hs_coloc_ALL), FUN=function(x) all(!is.na(cbind(vec_buoy_hs_coloc_ALL[x],vec_Q50_J3_ALL[x]))) )
   if ( flag_swell ) {
      Lvec_pair_idx <- Lvec_pair_idx & (df_plot$buoy_ap < period_thresh) & !Lvec_qual_J3_ALL
      #Lvec_pair_idx <- Lvec_pair_idx & (df_plot$buoy_ap > period_thresh) & !Lvec_qual_J3_ALL
   } else {
      Lvec_pair_idx <- Lvec_pair_idx & !Lvec_qual_J3_ALL
   }
   Lvec_pair_idx[is.na(Lvec_pair_idx)] <- FALSE
   i_N <- sum( Lvec_pair_idx )

   #plot(vec_buoy_hs_coloc_ALL[Lvec_pair_idx],vec_Q50_J3_ALL[Lvec_pair_idx],xlim=c(0,12),ylim=c(0,12),pch=vec_J3_pch,col=vec_J3_cols,
   plot(vec_buoy_hs_coloc_ALL[Lvec_pair_idx],vec_Q50_J3_ALL[Lvec_pair_idx],xlim=c(0,10),ylim=c(0,10),
        main=paste("N=",i_N,sep=""),
        xlab="Buoy",ylab="J-3",cex=4,cex.lab=5,cex.axis=5,cex.main=6)
   abline(a=0,b=1)

   mtext(text=paste("Correlation: ",format(cor(df_plot$buoy_hs[Lvec_pair_idx],df_plot$J3[Lvec_pair_idx],use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("Mean bias: ",format(mean(-df_plot$buoy_hs[Lvec_pair_idx],na.rm=T)+mean(df_plot$J3[Lvec_pair_idx],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("RMSE: ",format(mean(sqrt(lm(J3 ~ buoy_hs,data=df_plot[Lvec_pair_idx,])$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("SI: ",format(mean(sqrt(lm(J3 ~ buoy_hs,data=df_plot[Lvec_pair_idx,])$residuals^2))/mean(df_plot$buoy_hs[Lvec_pair_idx]),digits=3),sep=''), side=3, line=-12, adj=0.03, cex=3, outer=FALSE)
#
##-------------------------------------------------------------------------------------------------#
## Plot Buoy/S6 LRM ALL.
#   vec_S6_cols <- rep("black",length(Lvec_qual_S6_LRM_ALL))
#   vec_S6_cols[Lvec_qual_S6_LRM_ALL] <- "red"
#   vec_S6_pch <- rep(1,length(Lvec_qual_S6_LRM_ALL))
#   vec_S6_pch[Lvec_qual_S6_LRM_ALL] <- 19
## Pairs:
#   Lvec_pair_idx <- sapply( X=1:length(vec_buoy_hs_coloc_ALL), FUN=function(x) all(!is.na(cbind(vec_buoy_hs_coloc_ALL[x],vec_Q50_S6_LRM_ALL[x]))) )
#   i_N <- sum( Lvec_pair_idx )
#
#   plot(vec_buoy_hs_coloc_ALL,vec_Q50_S6_LRM_ALL,xlim=c(0,12),ylim=c(0,12),pch=vec_S6_pch,col=vec_S6_cols,
#        main=paste("N=",i_N,sep=""),
#        xlab="Buoy",ylab="S-6 LRM",cex=4,cex.lab=5,cex.axis=5,cex.main=6)
#   abline(a=0,b=1)
#
#   mtext(text=paste("Correlation: ",format(cor(df_plot$buoy,df_plot$S6LRM,use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("Mean bias: ",format(-mean(df_plot$buoy[Lvec_pair_idx],na.rm=T)+mean(df_plot$S6LRM[Lvec_pair_idx],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("RMSE: ",format(mean(sqrt(lm(S6LRM ~ buoy,data=df_plot)$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)
#
##-------------------------------------------------------------------------------------------------#
## Plot Buoy/S6 SAR ALL.
#   vec_S6_cols <- rep("black",length(Lvec_qual_S6_ALL))
#   vec_S6_cols[Lvec_qual_S6_ALL] <- "red"
#   vec_S6_pch <- rep(1,length(Lvec_qual_S6_ALL))
#   vec_S6_pch[Lvec_qual_S6_ALL] <- 19
## Pairs:
#   Lvec_pair_idx <- sapply( X=1:length(vec_buoy_hs_coloc_ALL), FUN=function(x) all(!is.na(cbind(vec_buoy_hs_coloc_ALL[x],vec_Q50_S6_SAR_ALL[x]))) )
#   i_N <- sum( Lvec_pair_idx )
#
#   plot(vec_buoy_hs_coloc_ALL,vec_Q50_S6_SAR_ALL,xlim=c(0,12),ylim=c(0,12),pch=vec_S6_pch,col=vec_S6_cols,
#        main=paste("N=",i_N,sep=""),
#        xlab="Buoy",ylab="S-6 SAR",cex=4,cex.lab=5,cex.axis=5,cex.main=6)
#   abline(a=0,b=1)
#
#   mtext(text=paste("Correlation: ",format(cor(df_plot$buoy,df_plot$S6SAR,use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("Mean bias: ",format(-mean(df_plot$buoy[Lvec_pair_idx],na.rm=T)+mean(df_plot$S6SAR[Lvec_pair_idx],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
#   mtext(text=paste("RMSE: ",format(mean(sqrt(lm(S6SAR ~ buoy,data=df_plot)$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)
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
# Plot J3/S6 LRM ALL.
# Pairs:
   Lvec_pair_idx <- sapply( X=1:length(vec_Q50_J3_ALL), FUN=function(x) all(!is.na(cbind(vec_Q50_J3_ALL[x],vec_Q50_S6_LRM_ALL[x]))) )
   if ( flag_swell ) {
      Lvec_pair_idx <- Lvec_pair_idx & (df_plot$buoy_ap < period_thresh) & !Lvec_qual_J3_ALL
      #Lvec_pair_idx <- Lvec_pair_idx & (df_plot$buoy_ap > period_thresh) & !Lvec_qual_J3_ALL
   } else {
      Lvec_pair_idx <- Lvec_pair_idx & !Lvec_qual_J3_ALL
   }
   Lvec_pair_idx[is.na(Lvec_pair_idx)] <- FALSE
   i_N <- sum( Lvec_pair_idx )

   #plot(vec_Q50_J3_ALL[Lvec_pair_idx],vec_Q50_S6_LRM_ALL[Lvec_pair_idx],xlim=c(0,6),ylim=c(0,6),pch=vec_J3_pch,col=vec_J3_cols,
   plot(vec_Q50_J3_ALL[Lvec_pair_idx],vec_Q50_S6_LRM_ALL[Lvec_pair_idx],xlim=c(0,10),ylim=c(0,10),pch=1,
        main=paste("N=",i_N,sep=""),
        xlab="J-3",ylab="S-6 LRM",cex=4,cex.lab=5,cex.axis=5,cex.main=6)
   abline(a=0,b=1)

   mtext(text=paste("Correlation: ",format(cor(df_plot$J3[Lvec_pair_idx],df_plot$S6LRM[Lvec_pair_idx],use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("Mean bias: ",format(-mean(df_plot$J3[Lvec_pair_idx],na.rm=T)+mean(df_plot$S6LRM[Lvec_pair_idx],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("RMSE: ",format(mean(sqrt(lm(S6LRM ~ J3,data=df_plot[Lvec_pair_idx,])$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("SI: ",format(mean(sqrt(lm(S6LRM ~ J3,data=df_plot[Lvec_pair_idx,])$residuals^2))/mean(df_plot$J3[Lvec_pair_idx]),digits=3),sep=''), side=3, line=-12, adj=0.03, cex=3, outer=FALSE)

#-------------------------------------------------------------------------------------------------#
# Plot J3/S6 SAR ALL.
   Lvec_pair_idx <- sapply( X=1:length(vec_Q50_J3_ALL), FUN=function(x) all(!is.na(cbind(vec_Q50_J3_ALL[x],vec_Q50_S6_SAR_ALL[x]))) )
   if ( flag_swell ) {
      Lvec_pair_idx <- Lvec_pair_idx & (df_plot$buoy_ap < period_thresh) & !Lvec_qual_J3_ALL
      #Lvec_pair_idx <- Lvec_pair_idx & (df_plot$buoy_ap > period_thresh) & !Lvec_qual_J3_ALL
   } else {
      Lvec_pair_idx <- Lvec_pair_idx & !Lvec_qual_J3_ALL
   }
   Lvec_pair_idx[is.na(Lvec_pair_idx)] <- FALSE
   i_N <- sum( Lvec_pair_idx )

   #plot(vec_Q50_J3_ALL[Lvec_pair_idx],vec_Q50_S6_SAR_ALL[Lvec_pair_idx],xlim=c(0,6),ylim=c(0,6),pch=vec_J3_pch,col=vec_J3_cols,
   plot(vec_Q50_J3_ALL[Lvec_pair_idx],vec_Q50_S6_SAR_ALL[Lvec_pair_idx],xlim=c(0,10),ylim=c(0,10),pch=1,
        main=paste("N=",i_N,sep=""),
        xlab="J-3",ylab="S-6 SAR",cex=4,cex.lab=5,cex.axis=5,cex.main=6)
   abline(a=0,b=1)

   mtext(text=paste("Correlation: ",format(cor(df_plot$J3[Lvec_pair_idx],df_plot$S6SAR[Lvec_pair_idx],use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("Mean bias: ",format(-mean(df_plot$J3[Lvec_pair_idx],na.rm=T)+mean(df_plot$S6SAR[Lvec_pair_idx],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("RMSE: ",format(mean(sqrt(lm(S6SAR ~ J3,data=df_plot[Lvec_pair_idx,])$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("SI: ",format(mean(sqrt(lm(S6SAR ~ J3,data=df_plot[Lvec_pair_idx,])$residuals^2))/mean(df_plot$J3[Lvec_pair_idx]),digits=3),sep=''), side=3, line=-12, adj=0.03, cex=3, outer=FALSE)

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
#-------------------------------------------------------------------------------------------------#
# Top title.
   mtext(text=paste("2020/12 - 2021/12 Buoy: ",paste(buoy_list[b_idx_list],collapse=","),sep=""), side=3, line=3, adj=0.05, cex=5, outer=TRUE)

   dev.off()
   system(paste("okular",fig_file_name,"&> /dev/null &"))

   }

# Triple coloc:
   Lvec_pair_idx <- sapply( X=1:length(vec_Q50_J3_ALL), FUN=function(x) all(!is.na(cbind(vec_Q50_J3_ALL[x],vec_Q50_S6_SAR_ALL[x],vec_buoy_hs_coloc_ALL[x]))) )
   Lvec_pair_idx <- Lvec_pair_idx & (df_plot$buoy_ap > period_thresh) & !Lvec_qual_J3_ALL
   #Lvec_pair_idx <- Lvec_pair_idx & !Lvec_qual_J3_ALL
# Set 1 = J3
# Set 2 = S6 SAR
# Set 3 = Buoy
   D1 <- vec_Q50_J3_ALL[Lvec_pair_idx]
   D2 <- vec_Q50_S6_SAR_ALL[Lvec_pair_idx]
   #D2 <- vec_Q50_S6_LRM_ALL[Lvec_pair_idx]
   D3 <- df_plot$buoy_hs[Lvec_pair_idx]

   V12 <- var(D1-D2,na.rm=T)
   V31 <- var(D3-D1,na.rm=T)
   V23 <- var(D2-D3,na.rm=T)

   sqrt( (V12+V31-V23)/2 )
   sqrt( (V23+V12-V31)/2 )
   sqrt( (V31+V23-V12)/2 )

##=================================================================================================#
## Plot bad data.
#   vec_cols <- c("black","red")
#   date_range <- c( min( as.POSIXct( list_buoy_data1[[1]][[4]][ list_buoy_data1[[1]][[6]][[5]] ], origin = '2000-01-01', tz='GMT') ),
#                    max( as.POSIXct( list_buoy_data1[[2]][[4]][ list_buoy_data1[[2]][[6]][[5]] ], origin = '2000-01-01', tz='GMT') ) )
#   lat_range <- c( min( list_buoy_data1[[1]][[2]][ list_buoy_data1[[1]][[6]][[5]] ] ),
#                   max( list_buoy_data1[[3]][[2]][ list_buoy_data1[[3]][[6]][[5]] ] ) )
#   X11()
## Latitude.
#   plot( list_buoy_data1[[1]][[2]][ list_buoy_data1[[1]][[6]][[5]] ],
#         list_buoy_data1[[1]][[5]][ list_buoy_data1[[1]][[6]][[5]] ],
#         xlim=lat_range,ylim=c(0,18),xlab="Latitude",ylab="Hs (m)",
#         pch=19, col=vec_cols[ 1+list_buoy_data1[[1]][[7]][ list_buoy_data1[[1]][[6]][[5]] ] ] )
#   points( list_buoy_data1[[3]][[2]][ list_buoy_data1[[3]][[6]][[5]] ],
#         list_buoy_data1[[3]][[5]][ list_buoy_data1[[3]][[6]][[5]] ],
#         pch=17, col=vec_cols[ 1+list_buoy_data1[[3]][[7]][ list_buoy_data1[[3]][[6]][[5]] ] ] )
## Time.
#   #plot( as.POSIXct( list_buoy_data1[[1]][[4]][ list_buoy_data1[[1]][[6]][[5]] ], origin = '2000-01-01', tz='GMT'),
#   #      list_buoy_data1[[1]][[5]][ list_buoy_data1[[1]][[6]][[5]] ],
#   #      xlim=date_range,ylim=c(0,18),xlab="Time sequence (s) - LABELS INCORRECT",ylab="Hs (m)",
#   #      pch=19, col=vec_cols[ 1+list_buoy_data1[[1]][[7]][ list_buoy_data1[[1]][[6]][[5]] ] ] )
#   #points( as.POSIXct( list_buoy_data1[[3]][[4]][ list_buoy_data1[[3]][[6]][[5]] ], origin = '2000-01-01', tz='GMT'),
#   #      list_buoy_data1[[3]][[5]][ list_buoy_data1[[3]][[6]][[5]] ],
#   #      pch=17, col=vec_cols[ 1+list_buoy_data1[[3]][[7]][ list_buoy_data1[[3]][[6]][[5]] ] ] )
#   abline(h=vec_buoy_hs_coloc[5],col="blue",lwd=2)
#   legend(x=date_range[1],y=18,legend=c("J3","S6 LRM","Buoy","BAD"),pch=c(19,17,NA,19),lty=c(NA,NA,1,NA),col=c("black","black","blue","red"))
#   #legend(x=date_range[1],y=18,legend=c("J3","S6 SAR","S6 LRM","Buoy","BAD"),pch=c(19,18,17,NA,19),lty=c(NA,NA,NA,1,NA),col=c("black","black","black","blue","red"))
#


   if ( flag_plot_junk ) {
# Plots against (buoy) wave period.
   X11()
   par(mfrow=c(1,2))
   lm_BHsJ3mS6 <- lm(J3mS6LRM ~ buoy_hs, data=df_plot)
   plot(vec_buoy_hs_coloc_ALL,df_plot$J3-df_plot$S6LRM,ylim=c(-0.25,0.25),xlab="Buoy Hs (m)",ylab="J3 - S6 LRM (m)"); abline(h=0)
   abline(lm_BHsJ3mS6,col="blue",lwd=2)

   lm_BTmJ3mS6 = lm(J3mS6LRM ~ buoy_ap, data=df_plot)
   plot(vec_buoy_ap_coloc_ALL,df_plot$J3-df_plot$S6LRM,ylim=c(-0.25,0.25),xlab="Buoy Tm2 (s)",ylab="J3 - S6 LRM (m)"); abline(h=0)
   abline(lm_BTmJ3mS6,col="blue",lwd=2)

   mtext(paste("Buoy",buoy_list[buoy_idx]),outer=TRUE,cex=2,line=-3)

# S6 SAR.
   lm2_BHsS6 = lmodel2(S6SAR ~ buoy_hs, data=df_plot, "relative", "relative", nperm=99)
   lm_BHsS6 = lm(S6SAR ~ buoy_hs, data=df_plot)

   X11()
   par(mfrow=c(2,2))
   plot(lm_BHsS6)

   X11(); plot(lm2_BHsS6,method="OLS",xlab="Buoy Hs (m)",ylab="S6 SAR",xlim=c(0,10),ylim=c(0,10)); abline(0,1)

   mtext(paste("Buoy",buoy_list[buoy_idx],": Buoy ~ S6_SAR regression"),outer=TRUE,cex=2,line=-2)

# Corrected S6 SAR.
   BB2 <- df_plot$S6SAR / lm2_BHsS6$regression.results[1,3] - lm2_BHsS6$regression.results[1,2]
   df_plot1 <- cbind(df_plot,J3mS6SAR=df_plot$J3-df_plot$S6SAR,S6SAR_c=df_plot$J3-BB2)
   lm_J3mS6SAR = lm(J3mS6SAR ~ buoy_hs, data=df_plot1)

   X11()
   par(mfrow=c(2,2))
   plot(vec_buoy_hs_coloc_ALL,df_plot1$J3mS6SAR,ylim=c(-1.0,0.0),xlab="Buoy Hs (m)",ylab="J3 - S6 SAR (m)"); abline(h=0)
   abline(lm_J3mS6SAR,col="blue",lwd=2)

   plot(vec_buoy_ap_coloc_ALL,df_plot1$J3mS6SAR,ylim=c(-1.0,0.0),xlab="Buoy Tm2 (s)",ylab="J3 - S6 SAR (m)"); abline(h=0)

   lm2_BHsS6_corr = lmodel2(S6SAR_c ~ buoy_hs, data=df_plot1, "interval", "interval", nperm=99)
   plot(vec_buoy_hs_coloc_ALL,df_plot1$S6SAR_c,ylim=c(-0.50,0.50),xlab="Buoy Hs (m)",ylab="J3 - S6 SAR_corr (m)"); abline(h=0)
   abline(a=lm2_BHsS6_corr$regression.results[1,2],b=lm2_BHsS6_corr$regression.results[1,3],col="blue",lwd=2)

   lm2_BTmS6_corr = lmodel2(S6SAR_c ~ buoy_ap, data=df_plot1, "interval", "relative", nperm=99)
   plot(vec_buoy_ap_coloc_ALL,df_plot1$S6SAR_c,ylim=c(-0.50,0.50),xlab="Buoy Tm2 (s)",ylab="J3 - S6 SAR_corr (m)"); abline(h=0)
   abline(a=lm2_BTmS6_corr$regression.results[1,2],b=lm2_BTmS6_corr$regression.results[1,3],col="blue",lwd=2)

   mtext(paste("Buoy",buoy_list[buoy_idx]),outer=TRUE,cex=2,line=-3)

# Joint distribution showing correction.
   vec_plot_cols <- round(100*df_plot1$S6SAR_c)+abs(min(round(100*df_plot1$S6SAR_c),na.rm=T))+1
   vec_cols <- viridis(max(vec_plot_cols,na.rm=T))

   X11()
   plot(df_plot1$buoy_hs,df_plot1$buoy_ap,pch=19,col=vec_cols[vec_plot_cols],xlab="Buoy Hs (m)",ylab="Buoy Tm2 (s)",main=paste("Buoy ",buoy_list[buoy_idx],": colour = J3 - S6",sep=""))
   #plot(df_plot1$buoy,df_plot1$buoy_ap,pch=19,col=vec_cols[1],xlab="Buoy Hs (m)",ylab="Buoy Tm2 (s)",main=paste("Buoy ",buoy_list[buoy_idx],": colour = J3 - S6",sep=""))

# Scatter with buoy_ap colouring.
   vec_plot_cols <- round( 10*df_plot1$buoy_ap - min(10*df_plot1$buoy_ap,na.rm=T) + 1 )
   vec_cols <- viridis(max(vec_plot_cols,na.rm=T))
   X11()
   plot(df_plot1$buoy_hs,df_plot1$S6SAR_c,pch=19,col=vec_cols[vec_plot_cols],xlab="Buoy Hs (m)",ylab="J3 - S6",main=paste("Buoy ",buoy_list[buoy_idx],": colour = Buoy Tm2 (s)",sep=""))

   }

## ggplot
## Remove outliers (J3mS6SAR and buoy_ap)
#   require(ggplot2)
#   X11(); ggplot(df_plot1, aes(buoy_hs, buoy_ap, color = J3mS6SAR)) + geom_point(shape = 16, size = 3, show.legend = TRUE) + scale_colour_viridis_c()
#   X11(); ggplot(df_plot1, aes(buoy_hs, J3mS6SAR, color = buoy_ap)) + geom_point(shape = 16, size = 3, show.legend = TRUE) + scale_colour_viridis_c(option = "plasma",direction=-1)
#
#   lm_J3mS6SAR = lm(J3mS6SAR ~ buoy_hs + I(buoy_hs^2) + I(buoy_ap^2), data=df_plot1)
#   BB <- as.numeric( names( lm_J3mS6SAR$fitted.values ) )
#   vec_temp <- rep(NA,804)
#   vec_temp[BB] <- lm_J3mS6SAR$fitted.values
#
#   lm_S6SAR = lm(S6SAR ~ I(buoy_hs^2) + buoy_ap + I(buoy_ap^3), data=df_plot1)
#   BB <- as.numeric( names( lm_S6SAR$fitted.values ) )
#   vec_temp <- rep(NA,804)
#   vec_temp[BB] <- lm_S6SAR$fitted.values
#
#   df_plot2 <- cbind(df_plot1,multi_reg=vec_temp,S6SAR_C=(df_plot1$S6SAR-vec_temp))
#   X11(); ggplot(df_plot2, aes(buoy_hs, S6SAR_C, color = buoy_ap)) + geom_point(shape = 16, size = 3, show.legend = TRUE) + scale_colour_viridis_c(option = "plasma",direction=-1)
#   X11(); ggplot(df_plot2, aes(buoy_hs, buoy_ap, color = S6SAR_C)) + geom_point(shape = 16, size = 3, show.legend = TRUE) + scale_colour_viridis_c()



