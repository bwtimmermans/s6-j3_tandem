# source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/buoy_coverage/show_buoy_coverage.R")

#=================================================================================================#
# Load buoy data.
# Bdduoy time offset: 946684800
#-------------------------------------------------------------------------------------------------#
# Offshore.
   buoy_list <- c("46035","46073","46072","46075","46066","46078","46246","46085","46001","46002","46005","46006","46059")
   buoy_list <- c("46246","46066","46078","46001","46085","46005","46002","46006","46059")
   vec_cols <- rep("yellow",length(buoy_list))
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
   list_master_payload <- list()

   for ( b_idx in 1:length(buoy_list) ) {
      buoy_name <- buoy_list[b_idx]
      print(paste("Buoy ID:",buoy_name))

      buoy_data_path_NOAA <- "/home/ben/research/waves/buoy_data/NDBC_complete_records/"
      buoy_data_path_USACE <- "/backup/datasets/buoys/USACE/NDBC_complete_records/"
# Check for USACE data.
      Lvec_flag_USACE[b_idx] <- length( list.files(path = buoy_data_path_USACE, pattern = paste0("^",buoy_name,"_") ) ) == 1
      if ( Lvec_flag_USACE[b_idx] ) {
# USACE.
         buoy_data_path <- buoy_data_path_USACE
         hs_lab <- "waveHs"
      } else {
# NOAA.
         buoy_data_path <- buoy_data_path_NOAA
         hs_lab <- "hs"
      }

      buoy_data_file <- list.files(path = buoy_data_path, pattern = paste0("^",buoy_name,"_") )
      mat_buoy_csv1 <- read.csv(paste0(buoy_data_path,buoy_data_file))
      mat_buoy_csv <- eval(parse(text=paste0("mat_buoy_csv1[!is.na(mat_buoy_csv1$",hs_lab,"),]")))
      vec_buoy_time <- strptime(as.character(mat_buoy_csv[,1]),format="%Y-%m-%d %H:%M:%S",tz="GMT")
      vec_buoy_time_Y <- format( vec_buoy_time, "%Y" )
      vec_buoy_time_Ym <- format( vec_buoy_time, "%Y%m" )
      vec_buoy_time_Ymd <- format( vec_buoy_time, "%Y%m%d" )
      if ( Lvec_flag_USACE[b_idx] ) {
         list_buoy_range_idx <- sapply( X=1:length(year_range), FUN=function(x) { which( vec_buoy_time_Y == year_range[x] ) } )
         vec_payloads <- unique( mat_buoy_csv$payload[unlist(list_buoy_range_idx)] )
         vec_payload_first_idx <- sapply( X=1:length(vec_payloads), FUN=function(x) { which( mat_buoy_csv$payload[unlist(list_buoy_range_idx)] == vec_payloads[x])[1] } )
         vec_time_payload_idx <- sapply( X=1:length(vec_payload_first_idx), FUN=function(x) { which( vec_time_master_Ymd == vec_buoy_time_Ymd[unlist(list_buoy_range_idx)][vec_payload_first_idx][x] ) } )
         list_master_payload[[b_idx]] <- data.frame(vec_time_payload_idx=vec_time_payload_idx,vec_payloads=vec_payloads)
      }
# Loop over specified date range (e.g. 2017-2021) to find observations that match daily slots.
      for ( y_idx in 1:length(year_range) ) {
      #print(paste("Buoy ID:",buoy_name,"year",y_idx))
         for ( m_idx in 1:12 ) {
            str_date <- paste0(year_range[y_idx],c("01","02","03","04","05","06","07","08","09","10","11","12")[m_idx])
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
# Create a matrix for buoy active period.
   mat_plot <- matrix(NA,nrow=length(vec_time_master),ncol=length(buoy_list))
   vec_plot_val <- seq(0.05,,0.05,length(buoy_list))
   for (kk in 1:length(buoy_list)) {
      mat_plot[Lmat_master[,kk],kk] <- vec_plot_val[kk]
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
   png(fig_buoy_coverage, width = 3000, height = 1000)
   par(oma=pl_oma,mar=pl_mar,mgp=pl_mgp)

   #X11()
   #par(mar=c(6,4,3,1))

   plot(1:dim(mat_plot)[1],mat_plot[,1],pch="|",ylim=c(0,max(mat_plot,na.rm=T)),col=vec_cols[1],xlab="",ylab="",cex=pl_cex,cex.axis=pl_cex_axis,axes=F)
# Rectangle for tandem period.
#   which( format( as.POSIXct( vec_time_master, tz='GMT', origin='2017-01-01'), "%Y%m%d") == "20201218" )
# 1448
#   which( format( as.POSIXct( vec_time_master, tz='GMT', origin='2017-01-01'), "%Y%m%d") == "20220427" )
# 1943
   rect(1448, 0, 1943, length(0.05*length(buoy_list)), density = 5)
# 
   if ( Lvec_flag_USACE[1] ) {
      points((1:dim(mat_plot)[1])[list_master_payload[[1]]$vec_time_payload_idx],rep(0.05,length(list_master_payload[[1]]$vec_time_payload_idx)),pch=vec_buoy_payload_lab[sapply(X=1:length(list_master_payload[[1]]$vec_payloads),FUN=function(x) { which(vec_buoy_payload_unique == list_master_payload[[1]]$vec_payloads[x]) })],cex=3)
   }
   #points((1:dim(mat_plot)[1])[list_master_payload[[1]]$vec_time_payload_idx],rep(0.05,length(list_master_payload[[1]]$vec_time_payload_idx)),pch="p1",cex=3)
   if (length(buoy_list) > 1) {
      for (b_idx in 2:length(buoy_list)) {
         points(1:dim(mat_plot)[1],mat_plot[,b_idx],pch="|",col=vec_cols[b_idx],cex=pl_cex)
         if ( Lvec_flag_USACE[b_idx] ) {
            points((1:dim(mat_plot)[1])[list_master_payload[[b_idx]]$vec_time_payload_idx],rep(unique(mat_plot[,b_idx])[1],length(list_master_payload[[b_idx]]$vec_time_payload_idx)),
		pch=vec_buoy_payload_lab[sapply(X=1:length(list_master_payload[[b_idx]]$vec_payloads),FUN=function(x) { which(vec_buoy_payload_unique == list_master_payload[[b_idx]]$vec_payloads[x]) })],
		cex=3)
         }
      }
   }
# X-axis.
   #100 * floor( length(vec_time_master) / 100 )
   x_at <- sapply(X=1:length(year_range),FUN=function(x) { which( format( as.POSIXct( vec_time_master, tz='GMT', origin='2017-01-01'), "%Y%m%d") == paste0(year_range,"0101")[x] ) })
   axis(side=1,at=x_at,labels=paste0(year_range,"-01-01"),las=3,cex.axis=pl_cex_axis)
   abline(v=x_at,col="grey",lwd=6)
# Y-axis.
   axis(side=2,at=seq(0,max(mat_plot,na.rm=T),0.05),labels=c(NA,buoy_list),las=1,cex.axis=pl_cex_axis)
   abline(h=seq(0,max(mat_plot,na.rm=T),0.05),col="darkgrey",lwd=4)

   dev.off()
   system(paste("okular",fig_buoy_coverage,"&> /dev/null &"))
