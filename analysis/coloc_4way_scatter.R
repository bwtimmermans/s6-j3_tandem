# source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/coloc_4way_scatter.R")

# Attach J3.
   attach("./output/buoys_J3/list_buoy_data_swh_ocean.Robj")
   #attach("./output/buoys_J3/list_buoy_data_swh_ocean_mle3.Robj")
   mat_list_J3 <- list_buoy_data[[2]]
   detach()

# Attach S6 SAR.
   attach("./output/buoys_S6/list_buoy_data_SAR.Robj")
   mat_list_S6 <- list_buoy_data[[2]]
   detach()

# Attach S6 LRM.
   attach("./output/buoys_S6/list_buoy_data_LRM.Robj")
   mat_list_S6_LRM <- list_buoy_data[[2]]
   detach()

   lab_month <- c("Dec (2020)","Jan (2021)","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# as.POSIXct(S6_46246_march[[4]][1:100], origin = '2000-01-01', tz='GMT')
# 46246,46085,51004,51001,32012,44011,44137,44139,41010,41040,41044,41048,41049,46006
   #buoy_list <- c(46246,46085,51004,51001,32012,44011,44137,44139,41010,41040,41044,41048,41049,46006)
   buoy_list <- c(46246,46085,51004,51001,32012,
                  44011,44137,44139,41010,41040,
                  41044,41048,41049,46006,46082,
                  46083,46084,46059,46002,46005,46001)
   buoy_idx <- 20

#=================================================================================================#
# Load buoy data.
# Buoy time offset: 946684800
#-------------------------------------------------------------------------------------------------#
   #buoy_data_file <- list.files(path = "/home/ben/research/waves/buoy_data/NDBC_complete_records/", pattern = paste("^",buoy_list[buoy_idx],sep="") )
   buoy_data_file <- "46005_1976-2022_hs1.csv"
   buoy_data_file <- "46005_1976-2021_hs.csv"
   mat_buoy_csv1 <- read.csv(paste("/home/ben/research/waves/buoy_data/NDBC_complete_records/",buoy_data_file,sep=""))
   mat_buoy_csv <- mat_buoy_csv1[!is.na(mat_buoy_csv1$hs),]
   vec_buoy_time <- strptime(as.character(mat_buoy_csv[,1]),format="%Y-%m-%d %H:%M:%S",tz="GMT")
# Get indices for 2020/12 - 2021/12
   date_start_idx <- which( format( as.POSIXct( vec_buoy_time, tz='GMT'), "%Y%m") %in% "202012" )[1]
   date_idx <- date_start_idx:length(vec_buoy_time)

   vec_buoy_time_num <- as.numeric( vec_buoy_time[date_idx] ) - 946684800
   vec_buoy_hs <- mat_buoy_csv$hs[date_idx]

#=================================================================================================#
   vec_Q50_J3_ALL <- vec_Q50_S6_ALL <- vec_Q50_S6_LRM_ALL1 <- vec_buoy_hs_coloc_ALL <- NULL
   Lvec_qual_J3_ALL <- Lvec_qual_S6_ALL <- Lvec_qual_S6_LRM_ALL <- NULL
## Loop over months 2021 (Jan - Dec).
#   X11()
#   par(mfrow=c(3,4))
   for ( m_idx in 1:10 ) {
      list_buoy_data1 <- list(mat_list_J3[[m_idx,buoy_idx]],mat_list_S6[[m_idx,buoy_idx]],mat_list_S6_LRM[[m_idx,buoy_idx]])

# Mean time for each segment (J3,S6)
      DD <- sapply( X=1:length(list_buoy_data1[[1]][[6]]), FUN=function(x) { floor( mean( list_buoy_data1[[1]][[4]][ list_buoy_data1[[1]][[6]][[x]] ] ) ) } )
      EE <- sapply( X=1:length(list_buoy_data1[[2]][[6]]), FUN=function(x) { floor( mean( list_buoy_data1[[2]][[4]][ list_buoy_data1[[2]][[6]][[x]] ] ) ) } )
      mat_DE <- sapply( X=1:length(DD),FUN=function(x) { abs(DD[x] - EE) < 40 } )
      FF <- sapply( X=1:length(list_buoy_data1[[3]][[6]]), FUN=function(x) { floor( mean( list_buoy_data1[[3]][[4]][ list_buoy_data1[[3]][[6]][[x]] ] ) ) } )
      mat_DF <- sapply( X=1:length(DD),FUN=function(x) { abs(DD[x] - FF) < 40 } )
# Find time differences (in seconds).
#      sapply( X=1:length(DD),FUN=function(x) { DD[x] - EE[mat_DE[,x]] } )
# Find matching buoy time.
      mat_GG <- sapply( X=1:length(DD),FUN=function(x) { abs(DD[x] - vec_buoy_time_num) < 1800 } )
#if ( length( which(mat_GG,arr.ind=T)[,1] ) < length(DD) ) {
#   print(paste(" Fewer buoy colocs, m_idx:",m_idx))
#}
      vec_buoy_hs_coloc <- rep(NA,length(DD))
      vec_buoy_hs_coloc[which(mat_GG,arr.ind=T)[,2]] <- vec_buoy_hs[ which(mat_GG,arr.ind=T)[,1] ]
      vec_buoy_hs_coloc_ALL <- c(vec_buoy_hs_coloc_ALL,vec_buoy_hs_coloc)

# Find swh track medians.
# J3.
      vec_Q50_J3 <- sapply( X=1:length(DD),FUN=function(x) { median( list_buoy_data1[[1]][[5]][ list_buoy_data1[[1]][[6]][[x]] ], na.rm=T ) } )
      vec_Q50_J3_ALL <- c(vec_Q50_J3_ALL,vec_Q50_J3)
# J3 quality.
      Lvec_qual_J3 <- sapply( X=1:length(DD),FUN=function(x) { sum( list_buoy_data1[[1]][[7]][ list_buoy_data1[[1]][[6]][[x]] ] ) > 0 } )
      Lvec_qual_J3_ALL <- c(Lvec_qual_J3_ALL,Lvec_qual_J3)
# S6 LRM.
      vec_Q50_S6_LRM <- sapply( X=1:length(DD),FUN=function(x) { median( list_buoy_data1[[3]][[5]][ unlist( list_buoy_data1[[3]][[6]][mat_DF[,x]] ) ], na.rm=T ) } )
      vec_Q50_S6_LRM_ALL1 <- c(vec_Q50_S6_LRM_ALL1,vec_Q50_S6_LRM)
# S6 LRM quality.
      Lvec_qual_S6_LRM <- sapply( X=1:length(DD),FUN=function(x) { sum( list_buoy_data1[[3]][[7]][ unlist( list_buoy_data1[[3]][[6]][mat_DF[,x]] ) ] ) > 0 } )
      Lvec_qual_S6_LRM_ALL <- c(Lvec_qual_S6_LRM_ALL,Lvec_qual_S6_LRM)
# S6 SAR.
      vec_Q50_S6 <- sapply( X=1:length(DD),FUN=function(x) { median( list_buoy_data1[[2]][[5]][ unlist( list_buoy_data1[[2]][[6]][mat_DE[,x]] ) ], na.rm=T ) } )
      vec_Q50_S6_ALL <- c(vec_Q50_S6_ALL,vec_Q50_S6)
# S6 SAR quality.
      Lvec_qual_S6 <- sapply( X=1:length(DD),FUN=function(x) { sum( list_buoy_data1[[2]][[7]][ unlist( list_buoy_data1[[2]][[6]][mat_DE[,x]] ) ] ) > 0 } )
      Lvec_qual_S6_ALL <- c(Lvec_qual_S6_ALL,Lvec_qual_S6)

# Plot.
#      plot(vec_Q50_J3,vec_Q50_S6,xlim=c(0,12),ylim=c(0,12),main=paste(lab_month[m_idx]," N=",length(vec_Q50_J3),sep=""))
#      abline(a=0,b=1)
   }
# Fix for NULL list output.
   #vec_Q50_S6_LRM_ALL <- numeric(length(vec_Q50_J3_ALL))
   #AA <- sapply(X=1:length(vec_Q50_S6_LRM_ALL),FUN=function(x) if ( is.null(vec_Q50_S6_LRM_ALL1[[x]]) ) { vec_Q50_S6_LRM_ALL[x] <- NA  } else { vec_Q50_S6_LRM_ALL[x] <- vec_Q50_S6_LRM_ALL1[[x]] } )
   vec_Q50_S6_LRM_ALL <- sapply(X=1:length(vec_Q50_J3_ALL),FUN=function(x) if ( is.null(vec_Q50_S6_LRM_ALL1[[x]]) ) { NA  } else { vec_Q50_S6_LRM_ALL1[[x]] } )

#=================================================================================================#
# Plotting.
#-------------------------------------------------------------------------------------------------#
# Data frame.
   df_plot <- data.frame(buoy=vec_buoy_hs_coloc_ALL,J3=vec_Q50_J3_ALL,S6LRM=vec_Q50_S6_LRM_ALL,S6SAR=vec_Q50_S6_ALL)
#-------------------------------------------------------------------------------------------------#
# File name.
   #fig_file_name <- paste("./figures/swell_test/multiscatter_",buoy_list[buoy_idx],".png",sep="")
   fig_file_name <- paste("./figures/multiscatter_",buoy_list[buoy_idx],"_2.png",sep="")

   png(fig_file_name, width = 2800, height = 2800)
   par(mfrow=c(4,4),oma=c(8,8,12,9),mar=c(12,14,9,9),mgp=c(9,4,0))

#-------------------------------------------------------------------------------------------------#
# Histogram: buoy
   hist(vec_buoy_hs_coloc_ALL,breaks=10,
        main="Buoy",xlab="Hs (m)",cex.lab=5,cex.axis=5,cex.main=6)
#-------------------------------------------------------------------------------------------------#
# Plot Buoy/J3 ALL.
   vec_J3_cols <- rep("black",length(Lvec_qual_J3_ALL))
   vec_J3_cols[Lvec_qual_J3_ALL] <- "red"
   vec_J3_pch <- rep(1,length(Lvec_qual_J3_ALL))
   vec_J3_pch[Lvec_qual_J3_ALL] <- 19
# Pairs:
   Lvec_pair_idx <- sapply( X=1:length(vec_buoy_hs_coloc_ALL), FUN=function(x) all(!is.na(cbind(vec_buoy_hs_coloc_ALL[x],vec_Q50_J3_ALL[x]))) )
   i_N <- sum( Lvec_pair_idx )

   plot(vec_buoy_hs_coloc_ALL,vec_Q50_J3_ALL,xlim=c(0,12),ylim=c(0,12),pch=vec_J3_pch,col=vec_J3_cols,
        main=paste("N=",sum(!is.na(vec_buoy_hs_coloc_ALL)),sep=""),
        xlab="Buoy",ylab="J-3",cex=4,cex.lab=5,cex.axis=5,cex.main=6)
   abline(a=0,b=1)

   mtext(text=paste("Correlation: ",format(cor(df_plot$buoy,df_plot$J3,use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("Mean bias: ",format(mean(-df_plot$buoy[Lvec_pair_idx],na.rm=T)+mean(df_plot$J3[Lvec_pair_idx],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("RMSE: ",format(mean(sqrt(lm(J3 ~ buoy,data=df_plot)$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)

#-------------------------------------------------------------------------------------------------#
# Plot Buoy/S6 LRM ALL.
   vec_S6_cols <- rep("black",length(Lvec_qual_S6_LRM_ALL))
   vec_S6_cols[Lvec_qual_S6_LRM_ALL] <- "red"
   vec_S6_pch <- rep(1,length(Lvec_qual_S6_LRM_ALL))
   vec_S6_pch[Lvec_qual_S6_LRM_ALL] <- 19
# Pairs:
   Lvec_pair_idx <- sapply( X=1:length(vec_buoy_hs_coloc_ALL), FUN=function(x) all(!is.na(cbind(vec_buoy_hs_coloc_ALL[x],vec_Q50_S6_LRM_ALL[x]))) )
   i_N <- sum( Lvec_pair_idx )

   plot(vec_buoy_hs_coloc_ALL,vec_Q50_S6_LRM_ALL,xlim=c(0,12),ylim=c(0,12),pch=vec_S6_pch,col=vec_S6_cols,
        main=paste("N=",i_N,sep=""),
        xlab="Buoy",ylab="S-6 LRM",cex=4,cex.lab=5,cex.axis=5,cex.main=6)
   abline(a=0,b=1)

   mtext(text=paste("Correlation: ",format(cor(df_plot$buoy,df_plot$S6LRM,use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("Mean bias: ",format(-mean(df_plot$buoy[Lvec_pair_idx],na.rm=T)+mean(df_plot$S6LRM[Lvec_pair_idx],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("RMSE: ",format(mean(sqrt(lm(S6LRM ~ buoy,data=df_plot)$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)

#-------------------------------------------------------------------------------------------------#
# Plot Buoy/S6 SAR ALL.
   vec_S6_cols <- rep("black",length(Lvec_qual_S6_ALL))
   vec_S6_cols[Lvec_qual_S6_ALL] <- "red"
   vec_S6_pch <- rep(1,length(Lvec_qual_S6_ALL))
   vec_S6_pch[Lvec_qual_S6_ALL] <- 19
# Pairs:
   Lvec_pair_idx <- sapply( X=1:length(vec_buoy_hs_coloc_ALL), FUN=function(x) all(!is.na(cbind(vec_buoy_hs_coloc_ALL[x],vec_Q50_S6_ALL[x]))) )
   i_N <- sum( Lvec_pair_idx )

   plot(vec_buoy_hs_coloc_ALL,vec_Q50_S6_ALL,xlim=c(0,12),ylim=c(0,12),pch=vec_S6_pch,col=vec_S6_cols,
        main=paste("N=",i_N,sep=""),
        xlab="Buoy",ylab="S-6 SAR",cex=4,cex.lab=5,cex.axis=5,cex.main=6)
   abline(a=0,b=1)

   mtext(text=paste("Correlation: ",format(cor(df_plot$buoy,df_plot$S6SAR,use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("Mean bias: ",format(-mean(df_plot$buoy[Lvec_pair_idx],na.rm=T)+mean(df_plot$S6SAR[Lvec_pair_idx],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("RMSE: ",format(mean(sqrt(lm(S6SAR ~ buoy,data=df_plot)$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)

#-------------------------------------------------------------------------------------------------#
# Line 2 (J3)
#-------------------------------------------------------------------------------------------------#
# Plot blank.
   plot(NULL,xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="")

#-------------------------------------------------------------------------------------------------#
# Histogram: J3
   hist(vec_Q50_J3_ALL,breaks=10,
        main="J3",xlab="Hs (m)",cex.lab=5,cex.axis=5,cex.main=6)
#-------------------------------------------------------------------------------------------------#
# Plot J3/S6 LRM ALL.
# Pairs:
   Lvec_pair_idx <- sapply( X=1:length(vec_Q50_J3_ALL), FUN=function(x) all(!is.na(cbind(vec_Q50_J3_ALL[x],vec_Q50_S6_LRM_ALL[x]))) )
   i_N <- sum( Lvec_pair_idx )

   plot(vec_Q50_J3_ALL,vec_Q50_S6_LRM_ALL,xlim=c(0,12),ylim=c(0,12),pch=vec_J3_pch,col=vec_J3_cols,
        main=paste("N=",i_N,sep=""),
        xlab="J-3",ylab="S-6 LRM",cex=4,cex.lab=5,cex.axis=5,cex.main=6)
   abline(a=0,b=1)

   mtext(text=paste("Correlation: ",format(cor(df_plot$J3,df_plot$S6LRM,use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("Mean bias: ",format(-mean(df_plot$J3[Lvec_pair_idx],na.rm=T)+mean(df_plot$S6LRM[Lvec_pair_idx],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("RMSE: ",format(mean(sqrt(lm(S6LRM ~ J3,data=df_plot)$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)

#-------------------------------------------------------------------------------------------------#
# Plot J3/S6 SAR ALL.
   Lvec_pair_idx <- sapply( X=1:length(vec_Q50_J3_ALL), FUN=function(x) all(!is.na(cbind(vec_Q50_J3_ALL[x],vec_Q50_S6_ALL[x]))) )
   i_N <- sum( Lvec_pair_idx )

   plot(vec_Q50_J3_ALL,vec_Q50_S6_ALL,xlim=c(0,12),ylim=c(0,12),pch=vec_J3_pch,col=vec_J3_cols,
        main=paste("N=",length(vec_Q50_J3_ALL),sep=""),
        xlab="J-3",ylab="S-6 SAR",cex=4,cex.lab=5,cex.axis=5,cex.main=6)
   abline(a=0,b=1)

   mtext(text=paste("Correlation: ",format(cor(df_plot$J3,df_plot$S6SAR,use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("Mean bias: ",format(-mean(df_plot$J3[Lvec_pair_idx],na.rm=T)+mean(df_plot$S6SAR[Lvec_pair_idx],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("RMSE: ",format(mean(sqrt(lm(S6SAR ~ J3,data=df_plot)$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)

#-------------------------------------------------------------------------------------------------#
# Line 3 (S6 LRM)
#-------------------------------------------------------------------------------------------------#
# Plot blank.
   plot(NULL,xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="")

#-------------------------------------------------------------------------------------------------#
# Plot blank.
   plot(NULL,xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="")

#-------------------------------------------------------------------------------------------------#
# Histogram: S6 LRM
   hist(vec_Q50_S6_ALL,breaks=10,
        main="S6 LRM",xlab="Hs (m)",cex.lab=5,cex.axis=5,cex.main=6)
#-------------------------------------------------------------------------------------------------#
# Plot S6 LRM/S6 SAR ALL.
# Pairs:
   Lvec_pair_idx <- sapply( X=1:length(vec_Q50_S6_LRM_ALL), FUN=function(x) all(!is.na(cbind(vec_Q50_S6_LRM_ALL[x],vec_Q50_S6_ALL[x]))) )
   i_N <- sum( Lvec_pair_idx )

   plot(vec_Q50_S6_LRM_ALL,vec_Q50_S6_ALL,xlim=c(0,12),ylim=c(0,12),pch=vec_J3_pch,col=vec_J3_cols,
        main=paste("N=",i_N,sep=""),
        xlab="S-6 LRM",ylab="S-6 SAR",cex=4,cex.lab=5,cex.axis=5,cex.main=6)
   abline(a=0,b=1)

   mtext(text=paste("Correlation: ",format(cor(df_plot$S6LRM,df_plot$S6SAR,use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("Mean bias: ",format(-mean(df_plot$S6LRM[Lvec_pair_idx],na.rm=T)+mean(df_plot$S6SAR[Lvec_pair_idx],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
   mtext(text=paste("RMSE: ",format(mean(sqrt(lm(S6SAR ~ S6LRM,data=df_plot)$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)

#-------------------------------------------------------------------------------------------------#
# Plot blank.
   plot(NULL,xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="")

#-------------------------------------------------------------------------------------------------#
# Plot blank.
   plot(NULL,xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="")

#-------------------------------------------------------------------------------------------------#
# Plot blank.
   plot(NULL,xlim=c(0,1),ylim=c(0,1),axes=F,xlab="",ylab="")

#-------------------------------------------------------------------------------------------------#
# Histogram: S6 SAR
   hist(vec_Q50_S6_LRM_ALL,breaks=10,
        main="S6 SAR",xlab="Hs (m)",cex.lab=5,cex.axis=5,cex.main=6)

#-------------------------------------------------------------------------------------------------#
# Top title.
   mtext(text=paste("2020/12 - 2021/10 Buoy: ",buoy_list[buoy_idx],sep=""), side=3, line=3, adj=0.05, cex=5, outer=TRUE)

   dev.off()
   system(paste("okular",fig_file_name,"&> /dev/null &"))

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
