# source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/buoy_coverage/show_buoy_coverage.R")

#=================================================================================================#
# Load buoy data.
# Bdduoy time offset: 946684800
#-------------------------------------------------------------------------------------------------#
   buoy_list <- c("46072","46001","46002","46005")

   year_range <- 2000:2021
   year_range <- 2017:2022
   vec_time_master <- c(0,3600*24*(1:(365 * length(year_range) + 5)))
   vec_time_master_Ym <- format( as.POSIXct( vec_time_master, tz='GMT', origin=paste0(year_range[1],"-01-01")), "%Y%m" )
   vec_time_master_Ymd <- format( as.POSIXct( vec_time_master, tz='GMT', origin=paste0(year_range[1],"-01-01")), "%Y%m%d" )
   Lmat_master <- matrix(FALSE,nrow=length(vec_time_master),ncol=length(buoy_list))

   for ( b_idx in 1:length(buoy_list) ) {

   buoy_name <- buoy_list[b_idx]
   buoy_data_file <- list.files(path = "/home/ben/research/waves/buoy_data/NDBC_complete_records/", pattern = paste("^",buoy_name,".*hs.csv",sep="") )
   #buoy_data_file <- "46005_1976-2021_hs.csv"
   mat_buoy_csv1 <- read.csv(paste("/home/ben/research/waves/buoy_data/NDBC_complete_records/",buoy_data_file,sep=""))
   mat_buoy_csv <- mat_buoy_csv1[!is.na(mat_buoy_csv1$hs),]
   vec_buoy_time <- strptime(as.character(mat_buoy_csv[,1]),format="%Y-%m-%d %H:%M:%S",tz="GMT")
   vec_buoy_time_Ym <- format( vec_buoy_time, "%Y%m" )
   vec_buoy_time_Ymd <- format( vec_buoy_time, "%Y%m%d" )
# Loop over specified date range (e.g. 2000-2021).
   for ( y_idx in 1:length(year_range) ) {
      print(paste("Buoy ID:",buoy_name,"year",y_idx))
      for ( m_idx in 1:12 ) {
         str_date <- paste(year_range[y_idx],c("01","02","03","04","05","06","07","08","09","10","11","12")[m_idx],sep='')
         vec_buoy_time_m_idx <-  which(vec_buoy_time_Ym == str_date)
         #str_date <- paste(year_range[y_idx])
         vec_date_idx <- which( vec_time_master_Ym %in% str_date )
         for ( d_idx in 1:length(vec_date_idx) ) {
            #print(paste("day",d_idx))
            Lmat_master[vec_date_idx[d_idx],b_idx] <- any( vec_time_master_Ymd[vec_date_idx[d_idx]] == vec_buoy_time_Ymd[vec_buoy_time_m_idx] )
         }
      }
   }

   }
# Plotting.
   mat_plot <- matrix(NA,nrow=length(vec_time_master),ncol=length(buoy_list))
   vec_plot_val <- seq(0.05,,0.05,length(buoy_list))
   for (kk in 1:length(buoy_list)) {
      mat_plot[Lmat_master[,kk],kk] <- vec_plot_val[kk]
   }

   X11()
   plot(1:dim(mat_plot)[1],mat_plot[,1],pch="|",ylim=c(0,1),col="red",xlab="Date",ylab="",axes=F)
   for (kk in 2:length(buoy_list)) {
      points(1:dim(mat_plot)[1],mat_plot[,kk],pch="|",col="orange")
   }
# X-axis.
   #100 * floor( length(vec_time_master) / 100 )
   axis(side=1,at=seq(0,2000,500)+1,labels=format( as.POSIXct( vec_time_master[seq(0,2000,500)+1], tz='GMT', origin='2017-01-01'), "%Y"))
# Y-axis.
   axis(side=2,at=seq(0,1,0.05),labels=c(NA,buoy_list,rep(NA,20-length(buoy_list))),las=1)
   abline(h=seq(0,1,0.05),col="grey")

#   print(paste("Buoy ID:",buoy_idx,"[",buoy_list[buoy_idx],"] date_start_idx:",date_start_idx))
#   if ( !is.na(date_start_idx) ) {
#      date_idx <- date_start_idx:length(vec_buoy_time)
#
#      vec_buoy_time_num <- as.numeric( vec_buoy_time[date_idx] ) - 946684800
#      vec_buoy_hs <- mat_buoy_csv$hs[date_idx]
#      vec_buoy_ap <- mat_buoy_csv$ap[date_idx]


