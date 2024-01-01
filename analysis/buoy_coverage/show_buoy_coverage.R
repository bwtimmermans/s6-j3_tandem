# source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/buoy_coverage/show_buoy_coverage.R")

#=================================================================================================#
# Load buoy data.
# Buoy time offset: 946684800
#-------------------------------------------------------------------------------------------------#
# Load buoys.
   source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/load_buoy_locs.R")
   buoy_list <- c(buoy_list_PAC_OS,buoy_list_PAC_NS)

# Identify lat and lon indices surrounding buoy locations (3x3).
   mat_buoy_lim <- matrix(NA,nrow=length(buoy_list),ncol=4)
   df_buoy_data <- NULL
   colnames(mat_buoy_lim) <- c("lat_low","lat_high","lon_low","lon_high")
   for ( b_idx in 1:length(buoy_list) ) {
# Data frame of buoy information.
      df_buoy_data <- eval(parse(text=paste("rbind(df_buoy_data,buoy_",buoy_list[b_idx],")",sep="")))
# Lat / lon values.
      eval(parse(text=paste("b_lat <- buoy_",buoy_list[b_idx],"$buoy_lat + c(-1.5,1.5)",sep="")))
      eval(parse(text=paste("b_lon <- buoy_",buoy_list[b_idx],"$buoy_lon + c(-1.5,1.5)",sep="")))
      mat_buoy_lim[b_idx,] <- c(b_lat,b_lon+360)
   }

# Redefine buoy_list for processing.
   buoy_list <- c("46246","46066","46078","46001","46085","46005","46002","46006","46059",buoy_list_PAC_NS[c(1:30)])
   buoy_list <- c("46246","46066","46078","46001","46085","46005","46002","46006","46059",buoy_list_PAC_NS)

   vec_operator <- unique(df_buoy_data$operator)
   vec_cols <- c("yellow2","grey","cyan4","grey","mediumpurple1","grey")
   fig_lab_region <- "OS"

## Nearshore.
#   buoy_list <- c(46077,46080,46076,46082,46083,46084,46205,46145,46183,46208,46185,46204,46147,46207,46100,46041,46099,46211,46248,46029,46243,46089,46098,46050,46097,46229,46015,46027,46244,46022,46213,46014,46013,46214,46026,46012,46042,46114,46239,46011,46259,46218,46054,46053,46069,46251,46025,46219,46047,46086,46258,46232)
#   vec_cols <- c( rep("cyan4",6),
#                  rep("darkorange1",8),
#                  rep("grey",1),
#                  rep("cyan4",37) )
#   fig_lab_region <- "NS"

   year_range <- 2000:2021
   year_range <- 2017:2022
   vec_time_master <- c(0,3600*24*(1:(365 * length(year_range) + 5)))
   vec_time_master_Ym <- format( as.POSIXct( vec_time_master, tz='GMT', origin=paste0(year_range[1],"-01-01")), "%Y%m" )
   vec_time_master_Ymd <- format( as.POSIXct( vec_time_master, tz='GMT', origin=paste0(year_range[1],"-01-01")), "%Y%m%d" )
   Lvec_flag_USACE <- rep(FALSE,length(buoy_list))
   Lmat_master <- matrix(FALSE,nrow=length(vec_time_master),ncol=length(buoy_list))
   Lmat_master_USACE <- matrix(FALSE,nrow=length(vec_time_master),ncol=length(buoy_list))
   list_master_payload <- list()

   for ( b_idx in 1:length(buoy_list) ) {
      buoy_name <- buoy_list[b_idx]
      print(paste("Buoy ID:",buoy_name))

      buoy_data_path_NOAA <- "/home/ben/research/waves/buoy_data/NDBC_complete_records/"
      buoy_data_path_USACE <- "/backup/datasets/buoys/USACE/NDBC_complete_records/"
# Acquire NDBC data.
      buoy_data_path <- buoy_data_path_NOAA
      hs_lab <- "hs"

      buoy_data_file <- list.files(path = buoy_data_path, pattern = paste0("^",buoy_name,".*hs.csv") )
      mat_buoy_csv1 <- read.csv(paste0(buoy_data_path,buoy_data_file))
      mat_buoy_csv <- eval(parse(text=paste0("mat_buoy_csv1[!is.na(mat_buoy_csv1$",hs_lab,"),]")))
      vec_buoy_time <- strptime(as.character(mat_buoy_csv[,1]),format="%Y-%m-%d %H:%M:%S",tz="GMT")
      vec_buoy_time_Y <- format( vec_buoy_time, "%Y" )
      vec_buoy_time_Ym <- format( vec_buoy_time, "%Y%m" )
      vec_buoy_time_Ymd <- format( vec_buoy_time, "%Y%m%d" )

# Check for, and acquire, USACE data.
      Lvec_flag_USACE[b_idx] <- length( list.files(path = buoy_data_path_USACE, pattern = paste0("^",buoy_name,"_") ) ) == 1

      if ( Lvec_flag_USACE[b_idx] ) {
         buoy_data_file <- list.files(path = buoy_data_path_USACE, pattern = paste0("^",buoy_name,"_") )
         mat_buoy_csv2 <- read.csv(paste0(buoy_data_path_USACE,buoy_data_file))
         mat_buoy_csv_USACE <- mat_buoy_csv2[!is.na(mat_buoy_csv2$waveHs) & (mat_buoy_csv2$waveHsFlag == 1),]
         vec_buoy_time_USACE <- strptime(as.character(mat_buoy_csv_USACE[,1]),format="%Y-%m-%d %H:%M:%S",tz="GMT")
         vec_buoy_time_USACE_Y <- format( vec_buoy_time_USACE, "%Y" )
         vec_buoy_time_USACE_Ym <- format( vec_buoy_time_USACE, "%Y%m" )
         vec_buoy_time_USACE_Ymd <- format( vec_buoy_time_USACE, "%Y%m%d" )

         list_buoy_range_idx <- sapply( X=1:length(year_range), FUN=function(x) { which( vec_buoy_time_USACE_Y == year_range[x] ) } )
         vec_payloads <- unique( mat_buoy_csv_USACE$payload[unlist(list_buoy_range_idx)] )
         vec_payload_first_idx <- sapply( X=1:length(vec_payloads), FUN=function(x) { which( mat_buoy_csv_USACE$payload[unlist(list_buoy_range_idx)] == vec_payloads[x])[1] } )
         vec_time_payload_idx <- sapply( X=1:length(vec_payload_first_idx), FUN=function(x) { which( vec_time_master_Ymd == vec_buoy_time_USACE_Ymd[unlist(list_buoy_range_idx)][vec_payload_first_idx][x] ) } )
         list_master_payload[[b_idx]] <- data.frame(vec_time_payload_idx=vec_time_payload_idx,vec_payloads=vec_payloads)
      }

# Loop over specified date range (e.g. 2017-2021) to find observations that match daily slots.
      for ( y_idx in 1:length(year_range) ) {
      #print(paste("Buoy ID:",buoy_name,"year",y_idx))
         for ( m_idx in 1:12 ) {
            str_date <- paste0(year_range[y_idx],c("01","02","03","04","05","06","07","08","09","10","11","12")[m_idx])
            vec_buoy_time_m_idx <-  which(vec_buoy_time_Ym == str_date)
            vec_date_idx <- which( vec_time_master_Ym %in% str_date )
            for ( d_idx in 1:length(vec_date_idx) ) {
               #print(paste("day",d_idx))
               Lmat_master[vec_date_idx[d_idx],b_idx] <- any( vec_time_master_Ymd[vec_date_idx[d_idx]] == vec_buoy_time_Ymd[vec_buoy_time_m_idx] )
            }
            if ( Lvec_flag_USACE[b_idx] ) {
               vec_buoy_time_USACE_m_idx <-  which(vec_buoy_time_USACE_Ym == str_date)
               for ( d_idx in 1:length(vec_date_idx) ) {
                  Lmat_master_USACE[vec_date_idx[d_idx],b_idx] <- any( vec_time_master_Ymd[vec_date_idx[d_idx]] == vec_buoy_time_USACE_Ymd[vec_buoy_time_USACE_m_idx] )
               }
            }
         }
      }
# Close buoy loop.
   }
#-------------------------------------------------------------------------------------------------#
# Plotting.
# Create a matrix for buoy active period.
   mat_plot <- matrix(NA,nrow=length(vec_time_master),ncol=length(buoy_list))
   mat_plot_USACE <- matrix(NA,nrow=length(vec_time_master),ncol=length(buoy_list))
   vec_plot_val <- seq(0.05,,0.05,length(buoy_list))
   for (kk in 1:length(buoy_list)) {
      mat_plot[Lmat_master[,kk],kk] <- vec_plot_val[kk]
      mat_plot_USACE[Lmat_master_USACE[,kk],kk] <- vec_plot_val[kk]
   }
# Find all different payloads and assign plotting character.
   vec_buoy_payload_unique <- unique( unlist( sapply(X=1:length(list_master_payload),FUN=function(x) { list_master_payload[[x]]$vec_payloads }) ) )
   vec_buoy_payload_lab <- paste0("p",1:length(vec_buoy_payload_unique))
   vec_buoy_payload_lab <- c("A","B","C","D","E")

   pl_mfrow <- c(2,2)
   pl_oma <- c(2,2,2,2)
   pl_mar <- c(20,12,8,5)
   pl_mgp <- c(9,3,0)
   pl_cex <- 4.0; pl_cex_main <- 6; pl_cex_lab <- 6; pl_cex_axis <- 4; pl_cex_leg <- 5

   fig_buoy_coverage <- paste0("./figures/",fig_lab_region,".png")
   png(fig_buoy_coverage, width = 3000, height = 4000)
   par(oma=pl_oma,mar=pl_mar,mgp=pl_mgp)

   #X11()
   #par(mar=c(6,4,3,1))

   #plot(1:dim(mat_plot)[1],mat_plot[,1],pch="|",ylim=c(0,max(mat_plot,na.rm=T)),col=vec_cols[1],xlab="",ylab="",cex=pl_cex,cex.axis=pl_cex_axis,axes=F)
   plot(NULL,xlim=c(0,dim(mat_plot)[1]),ylim=c(0.05,max(mat_plot,na.rm=T)),xlab="",ylab="",axes=F)


# X-axis.
   #100 * floor( length(vec_time_master) / 100 )
   x_at <- sapply(X=1:(1+length(year_range)),FUN=function(x) { which( format( as.POSIXct( vec_time_master, tz='GMT', origin='2017-01-01'), "%Y%m%d") == paste0(c(year_range,1+(year_range[length(year_range)])),"0101")[x] ) })
   axis(side=1,at=x_at,labels=paste0(c(year_range,1+(year_range[length(year_range)])),"-01-01"),las=3,cex.axis=pl_cex_axis)
# Y-axis.
   axis(side=2,at=seq(0,max(mat_plot,na.rm=T),0.05),labels=c(NA,buoy_list),las=1,cex.axis=pl_cex_axis)

   abline(v=x_at,col="grey",lwd=6)
   abline(h=seq(0,max(mat_plot,na.rm=T),0.05),col="darkgrey",lwd=4)
   abline(h=0.475,col="black",lwd=10)

# Payload metadata.
#   if ( Lvec_flag_USACE[1] ) {
#      points((1:dim(mat_plot)[1])[list_master_payload[[1]]$vec_time_payload_idx],rep(0.05,length(list_master_payload[[1]]$vec_time_payload_idx)),pch=vec_buoy_payload_lab[sapply(X=1:length(list_master_payload[[1]]$vec_payloads),FUN=function(x) { which(vec_buoy_payload_unique == list_master_payload[[1]]$vec_payloads[x]) })],cex=3)
#   }
   #points((1:dim(mat_plot)[1])[list_master_payload[[1]]$vec_time_payload_idx],rep(0.05,length(list_master_payload[[1]]$vec_time_payload_idx)),pch="p1",cex=3)
   if (length(buoy_list) > 1) {
      for (b_idx in 1:length(buoy_list)) {
         buoy_name <- buoy_list[b_idx]
         i_col <- which( vec_operator == df_buoy_data$operator[which(df_buoy_data$name == buoy_name)] )
         #print(paste("i_col",i_col))
         points(1:dim(mat_plot)[1],mat_plot[,b_idx],pch="|",col=vec_cols[i_col],cex=pl_cex)
         if ( Lvec_flag_USACE[b_idx] ) {
            points(1:dim(mat_plot_USACE)[1],mat_plot_USACE[,b_idx],pch="|",col="darkorange1",cex=2.0)
            points((1:dim(mat_plot)[1])[list_master_payload[[b_idx]]$vec_time_payload_idx],rep(unique(mat_plot[,b_idx])[1],length(list_master_payload[[b_idx]]$vec_time_payload_idx)),
		pch=vec_buoy_payload_lab[sapply(X=1:length(list_master_payload[[b_idx]]$vec_payloads),FUN=function(x) { which(vec_buoy_payload_unique == list_master_payload[[b_idx]]$vec_payloads[x]) })],
		cex=3)
         }
      }
   }
# Rectangle for tandem period.
#   which( format( as.POSIXct( vec_time_master, tz='GMT', origin='2017-01-01'), "%Y%m%d") == "20201218" )
# 1448
#   which( format( as.POSIXct( vec_time_master, tz='GMT', origin='2017-01-01'), "%Y%m%d") == "20220427" )
# 1943
   rect(1448, 0, 1943, 0.05*length(buoy_list), density = 5)

   dev.off()
   system(paste("okular",fig_buoy_coverage,"&> /dev/null &"))

