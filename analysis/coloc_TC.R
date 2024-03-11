# source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/coloc_TC.R")
   set.seed(23458892)
   require(lmodel2)
   require(viridis)

#=================================================================================================#
   flag_plot_TC_bar <- FALSE
   flag_period_thresh <- FALSE
   flag_plot_series <- TRUE

   min_bin_1Hz <- c(1,3,5,7)
   lab_min_bin_1Hz <- paste0("min",min_bin_1Hz)
   
#=================================================================================================#
# Triple collocation:
   require(ggplot2)
# Get all datasets.
# list_df_reg_radius: 1 = J3
#                     2 = S6LRM
#                     3 = S6SAR

   list_TC_stats_rad <- list()
   list_TC_stats_min_bin <- list()

   #for ( min_bin_1Hz_idx in 1 ) {
   for ( min_bin_1Hz_idx in 1:length(min_bin_1Hz) ) {

   #for ( i_b_rad in 4:4 ) {
   for ( i_b_rad in 1:length(vec_buoy_radius) ) {
      buoy_radius <- vec_buoy_radius[i_b_rad]

      if ( !is.null(list_df_reg_radius[[i_b_rad,1]]) ) {
         df_reg_J3 <- list_df_reg_radius[[i_b_rad,1]]
      }
      if ( !is.null(list_df_reg_radius[[i_b_rad,2]]) ) {
         df_reg_S6LRM <- list_df_reg_radius[[i_b_rad,2]]
         mat_list_slot_J3S6LRM <- sapply( X=1:length(df_reg_J3$sat_mean_time),FUN=function(x) { df_reg_S6LRM$sat_mean_time - df_reg_J3$sat_mean_time[x] < 35 & df_reg_S6LRM$sat_mean_time - df_reg_J3$sat_mean_time[x] > 0 } )
         vec_time_match <- apply(X=mat_list_slot_J3S6LRM,MAR=2,FUN=function(x) { if ( any(x) ) { DD <- which(x) } else { DD <- NA } })
         mat_time_match <- cbind(1:length(vec_time_match),vec_time_match)
         mat_time_match <- mat_time_match[!is.na(mat_time_match[,2]),]
# Crate new df including S6LRM.
         df_reg_J3 <- cbind( df_reg_J3[mat_time_match[,1],], S6LRM_hs=df_reg_S6LRM$sat_hs[mat_time_match[,2]])
      }
      if ( !is.null(list_df_reg_radius[[i_b_rad,3]]) ) {
         df_reg_S6SAR <- list_df_reg_radius[[i_b_rad,3]]
         mat_list_slot_J3S6SAR <- sapply( X=1:length(df_reg_J3$sat_mean_time),FUN=function(x) { df_reg_S6SAR$sat_mean_time - df_reg_J3$sat_mean_time[x] < 35 & df_reg_S6SAR$sat_mean_time - df_reg_J3$sat_mean_time[x] > 0 } )
         vec_time_match <- apply(X=mat_list_slot_J3S6SAR,MAR=2,FUN=function(x) { if ( any(x) ) { DD <- which(x) } else { DD <- NA } })
         mat_time_match <- cbind(1:length(vec_time_match),vec_time_match)
         mat_time_match <- mat_time_match[!is.na(mat_time_match[,2]),]
# Crate new df including S6SAR.
         df_reg_J3 <- cbind( df_reg_J3[mat_time_match[,1],], S6SAR_hs=df_reg_S6SAR$sat_hs[mat_time_match[,2]])
      }
      df_reg <- eval(parse(text=paste0("cbind(df_reg_J3,sat_hs_CAL_PRE=df_reg_J3$sat_hs_min",min_bin_1Hz[min_bin_1Hz_idx],")")))
      #mat_list_slot_J3B <- sapply( X=1:length(mat_list_mean_time[[m_idx,1]]),FUN=function(x) { abs(mat_list_mean_time[[m_idx,1]][x] - vec_buoy_time_num) < 1800 & abs(mat_list_mean_time[[m_idx,1]][x] - vec_buoy_time_num) == min( abs(mat_list_mean_time[[m_idx,1]][x] - vec_buoy_time_num) ) } )

#=================================================================================================#
# Calibration.
# Assume buoys = truth, and use linear model to remove linear dependency.
      df_reg1 <- df_reg[!is.na(df_reg$buoy_hs) & !is.na(df_reg$sat_hs_CAL_PRE) & !is.na(df_reg$ERA5_hs),]

      lm_J3B = lm(buoy_hs ~ sat_hs_CAL_PRE, data=df_reg1)
      lm_ERA5B = lm(buoy_hs ~ ERA5_hs, data=df_reg1)
      #lm_J3B = lm(sat_hs ~ buoy_hs, data=df_reg1)
      #lm_ERA5B = lm(sat_hs ~ ERA5_hs, data=df_reg1)

      if ( !is.null(list_df_reg_radius[[i_b_rad,2]]) ) {
         lm_S6LRMB = lm(buoy_hs ~ S6LRM_hs, data=df_reg1)
         df_reg1 <- cbind(df_reg1,S6LRM_hs_CAL=lm_S6LRMB$fitted.values)
      }
      if ( !is.null(list_df_reg_radius[[i_b_rad,3]]) ) {
         lm_S6SARB = lm(buoy_hs ~ S6SAR_hs + I(S6SAR_hs^(1/2)), data=df_reg1)
         df_reg1 <- cbind(df_reg1,S6SAR_hs_CAL=lm_S6SARB$fitted.values)
      }

      df_reg2 <- cbind(df_reg1,sat_hs_CAL=lm_J3B$fitted.values,ERA5_hs_CAL=lm_ERA5B$fitted.values)
      #df_reg2 <- cbind(df_reg1,buoy_hs_CAL=lm_J3B$fitted.values,ERA5_hs_CAL=lm_ERA5B$fitted.values)


#-------------------------------------------------------------------------------------------------#
# Find coloc data by removing all NA colocs.
      #Lvec_coloc_idx_master <- sapply( X=1:length(vec_Q50_J3_ALL), FUN=function(x) all(!is.na(cbind(vec_Q50_J3_ALL[x],vec_Q50_S6_SAR_ALL[x],vec_buoy_hs_coloc_ALL[x],vec_ERA5_hs_coloc_ALL[x]))) )
      Lvec_coloc_idx_master <- sapply( X=1:length(df_reg2$buoy_hs), FUN=function(x) all(!is.na(cbind(df_reg2$buoy_hs[x],df_reg2$ERA5_hs[x],df_reg2$sat_hs[x],df_reg2$S6SAR_hs[x]))) )
      Lvec_coloc_idx_master <- rep(TRUE,length(df_reg2$buoy_hs))

# Labels for the tandem data.
      #vec_data_var <- c("vec_Q50_J3_ALL_CAL","vec_Q50_S6_LRM_ALL_CAL","vec_Q50_S6_SAR_ALL_CAL")
      vec_data_var <- c("sat_hs_CAL","S6LRM_hs_CAL","S6SAR_hs_CAL")
      vec_data_lab <- c("J-3","S-6_LRM","S-6_SAR","Buoys","ERA5")

# Loop over omission of tandem datasets.
      df_plot_data_TC <- NULL
      for (i_data in c(1)) {
# Bootstrap uncertainty.
# Get the indices of the triplets.
         vec_coloc_idx <- which(Lvec_coloc_idx_master)
         mat_D <- matrix(NA,ncol=3,nrow=length(Lvec_coloc_idx_master))

         n_samp <- 5000
         mat_sqrt <- matrix(NA,ncol=3,nrow=n_samp)

         for (ii in 1:n_samp) {
            vec_idx <- sample(vec_coloc_idx,replace=TRUE)
            D1 <- eval(parse(text=paste0("df_reg2$",vec_data_var[i_data],"[vec_idx]")))
            #D2 <- vec_Q50_J3_ALL[vec_idx]
            D2 <- df_reg2$ERA5_hs_CAL[vec_idx]
            D3 <- df_reg2$buoy_hs[vec_idx]

            V12 <- var(D1-D2,na.rm=T)
            V31 <- var(D3-D1,na.rm=T)
            V23 <- var(D2-D3,na.rm=T)

            mat_sqrt[ii,1] <- sqrt( (V12+V31-V23)/2 )
            mat_sqrt[ii,2] <- sqrt( (V23+V12-V31)/2 )
            mat_sqrt[ii,3] <- sqrt( (V31+V23-V12)/2 )
         }
         df_plot_data_TC_temp <- rbind(
				    # Tandem
			            data.frame(mean_e=mean(mat_sqrt[,1],na.rm=T),sd_e=sqrt(var(mat_sqrt[,1],na.rm=T)),mission=vec_data_lab[i_data],group=i_data,n_coloc=sum(Lvec_coloc_idx_master,na.rm=T)),
				    # ERA5
			            data.frame(mean_e=mean(mat_sqrt[,2],na.rm=T),sd_e=sqrt(var(mat_sqrt[,2],na.rm=T)),mission=vec_data_lab[5],group=i_data,n_coloc=sum(Lvec_coloc_idx_master,na.rm=T)),
				    # Buoys
			            data.frame(mean_e=mean(mat_sqrt[,3],na.rm=T),sd_e=sqrt(var(mat_sqrt[,3],na.rm=T)),mission=vec_data_lab[4],group=i_data,n_coloc=sum(Lvec_coloc_idx_master,na.rm=T)) )
         df_plot_data_TC <- rbind(df_plot_data_TC,df_plot_data_TC_temp)
         #array_mean_e[,i_data,JJ] <- df_plot_data_temp$mean_e
      }

#-------------------------------------------------------------------------------------------------#
# Plot bar charts.
   if ( flag_plot_TC_bar ) {
      if ( flag_period_thresh ) {
         if ( flag_swell_only ) {
            fig_file_name <- paste0("./figures/bar_plots/",str_region,"_",buoy_radius,"km_numval_SWELL.png")
         } else {
            fig_file_name <- paste0("./figures/bar_plots/",str_region,"_",buoy_radius,"km_numval_NOSWELL.png")
         }
      } else {
         fig_file_name <- paste0("./figures/bar_plots/",str_region,"_M",m_limit,"_",buoy_radius,"km_",lab_min_bin_1Hz[min_bin_1Hz_idx],".png")
      }
# Plotting.
# https://www.statology.org/ggplot-default-colors/
# Default colours: "#F8766D" "#A3A500" "#00BF7D" "#00B0F6" "#E76BF3"
      p1 <- ggplot(df_plot_data_TC,aes(x = as.factor(group), y = mean_e, fill = as.factor(mission))) + 
      geom_col(position = "dodge") +
      geom_errorbar(aes(ymin = mean_e-sd_e, ymax = mean_e+sd_e), width=0.2,
                    position=position_dodge(0.9)) +
      ylim(0,0.6) +
      ggtitle(paste0(str_region," ",buoy_radius," km [N_coloc=",df_plot_data_TC$n_coloc[1],"]")) +
      labs(y="Mean error (m)",fill='Dataset') +
      
      theme(
	    plot.title = element_text(size = 70,hjust = 0.5),
	    axis.title.x=element_blank(),
            axis.title.y=element_text(size = 50),
            #panel.grid.minor = element_blank(),
            #panel.grid.major = element_blank(),
            #panel.background = element_rect(fill = "black"),

            strip.text = element_text(size = 50, margin = margin(25,0,25,0)),
            strip.background = element_rect(fill = "white"),
            panel.spacing.x = unit(1, "lines"),
            panel.spacing.y = unit(2, "lines"),
            axis.text.y = element_text(size = 50),
            axis.text.x = element_text(size = 50),
            axis.ticks.x = element_blank(),

            legend.position = "left",
            legend.margin = margin(0,75,0,0),
            legend.key.width = unit(1.5, "inch"),
            legend.key.height = unit(2, "inch"),
            legend.title = element_text(size = 50, margin = margin(25,0,0,0)),
            legend.title.align = 0.5,
            legend.text = element_text(size = 40, margin = margin(0,0,0,25))
         )

      png(fig_file_name, width = 2200, height = 1800)
      #grid.arrange(p2,p1,ncol=2)
      plot(p1)
      dev.off()
      system(paste("okular",fig_file_name,"&> /dev/null &"))

   }
## Plot histograms.
#      X11()
#      par(mfrow=c(1,3))
#      hist(mat_sqrt[,1])
#      hist(mat_sqrt[,2])
#      hist(mat_sqrt[,3])
# Mean uncertainty.
      print(paste("MEAN 1:",mean(mat_sqrt[,1],na.rm=T)))
      print(paste("MEAN 2:",mean(mat_sqrt[,2],na.rm=T)))
      print(paste("MEAN 3:",mean(mat_sqrt[,3],na.rm=T)))
# S.D. for variance uncertainty.
      print(paste("SQRT 1:",sqrt(var(mat_sqrt[,1],na.rm=T))))
      print(paste("SQRT 2:",sqrt(var(mat_sqrt[,2],na.rm=T))))
      print(paste("SQRT 3:",sqrt(var(mat_sqrt[,3],na.rm=T))))

# Assign TC data for each sampling radius to list.
      list_TC_stats_rad[[i_b_rad]] <- df_plot_data_TC
   }
      list_TC_stats_min_bin[[min_bin_1Hz_idx]] <- df_plot_data_TC
   }
# Save list_TC_stats_min_bin for multiple plotting.
   TC_stats_file_name <- paste0("./R_data_saved/list_TC_stats_min_bin_",str_region,"_S",paste(Sidx,collapse=''),"_M",m_limit,"_",buoy_radius,"km_",lab_min_bin_1Hz[min_bin_1Hz_idx],".Robj")
   save(list_TC_stats_min_bin,file = TC_stats_file_name)

#=================================================================================================#
# Plot TC error gainst parameters (like sampling raduis, min1 etc).
   if ( flag_plot_series ) {
# Plotting parameters.
      pl_mfrow <- c(1,1)
      pl_oma <- c(2,2,2,2)
      pl_mar <- c(12,14,8,12)
      pl_mgp <- c(9,3,0)
      pl_cex <- 4; pl_cex_main <- 6; pl_cex_lab <- 6; pl_cex_axis <- 5; pl_cex_leg <- 5
      #cex_mtext <- 4

      plot_title <- paste0(fig_lab_region," ",vec_tandem_labs[S_idx],"[M",m_limit,"] Mean bias with sampling radius")

      #fig_radius_stats_file_name <- paste0("./figures/TC_series/error_radius_",fig_lab_region,"_","M",m_limit,"_",vec_tandem_labs[S_idx],".png")
      fig_radius_stats_file_name <- paste0("./figures/TC_series/error_radius_",fig_lab_region,"_","M",m_limit,".png")

# Open file.
      png(fig_radius_stats_file_name, width = 2400, height = 2400)
      par(mfrow=pl_mfrow,oma=pl_oma,mar=pl_mar,mgp=pl_mgp)
      #X11()
# Create the plot.
      plot(NULL,xlim=c(0,8),ylim=c(0,0.3),xlab="Along-track bins",ylab="TC error (m)",main="",axes=FALSE,cex=pl_cex, cex.main=pl_cex_main, cex.lab=pl_cex_lab, cex.axis=pl_cex_axis)
      axis(side=1,at=min_bin_1Hz,labels=lab_min_bin_1Hz,cex=pl_cex_axis,cex.lab=pl_cex_lab,cex.axis=pl_cex_axis)
      axis(side=2,at=seq(0,0.4,0.1),cex=pl_cex_axis,cex.lab=pl_cex_lab,cex.axis=pl_cex_axis)

      #vec_cols <- c("#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3")
      vec_cols <- c("#00BF7D","#00B0F6","#E76BF3","#F8766D","#A3A500")
      vec_pch <- c(15,17,18,19)
      vec_pt_cex <- c(7,5)
      vec_pt_offset <- c(0,0.2)
# Plot TC errors.
# Load data files for different runs.
      vec_stat_files <- c("./R_data_saved/list_TC_stats_min_bin_PAC_OS_S1_M60_50km_min7.Robj","./R_data_saved/list_TC_stats_min_bin_PAC_OS_S1_M60_50km_min7_46246.Robj")
      for ( STAT_idx in 1:2 ) {
         load(vec_stat_files[STAT_idx])
         for ( S_idx in c(Sidx,4) ) {
            mat_TC_stats_temp <- matrix(NA,nrow=4,ncol=2)
            mat_TC_stats_temp[,1] <- sapply(X=1:length(list_TC_stats_min_bin),FUN=function(x) { FF <- list_TC_stats_min_bin[[x]]; FF$mean_e[which(FF$mission == vec_data_lab[S_idx])[1]] })
            mat_TC_stats_temp[,2] <- sapply(X=1:length(list_TC_stats_min_bin),FUN=function(x) { FF <- list_TC_stats_min_bin[[x]]; FF$sd_e[which(FF$mission == vec_data_lab[S_idx])[1]] })

            points(min_bin_1Hz + vec_pt_offset[STAT_idx],mat_TC_stats_temp[,1],pch=vec_pch[S_idx],cex=vec_pt_cex[STAT_idx],col=vec_cols[S_idx])
            # https://stackoverflow.com/questions/13032777/scatter-plot-with-error-bars
            arrows(min_bin_1Hz + vec_pt_offset[STAT_idx], mat_TC_stats_temp[,1]-mat_TC_stats_temp[,2], min_bin_1Hz + vec_pt_offset[STAT_idx], mat_TC_stats_temp[,1]+mat_TC_stats_temp[,2], length=0.05, angle=90, code=3, lwd=6)
# Legend.
            legend(x=0,y=0.3, legend=c(vec_data_lab[c(Sidx,4)],paste(vec_data_lab[c(Sidx,4)],"[inc 46246]")),pch=vec_pch[c(Sidx,4)],pt.cex=rep(vec_pt_cex,each=2),col=vec_cols[c(Sidx,4)],cex=pl_cex_leg)
         }
      }
      dev.off()
      system(paste("okular",fig_radius_stats_file_name,"&> /dev/null &"))
   }

## 1 Hz samples.
#      vec_x_samp_1Hz_poly <- c(rev(vec_buoy_radius),vec_buoy_radius)
#      vec_y_samp_1Hz_poly <- c(rep(0,length(vec_buoy_radius)),array_radius_stats[7,1,1:length(vec_buoy_radius)])
#      polygon(vec_x_samp_1Hz_poly,vec_y_samp_1Hz_poly,border = NA,col="lightsteelblue3")
#      text(x=vec_buoy_radius[1], y=array_radius_stats[7,1,1], labels=paste0("N[1Hz] = ",array_radius_stats[7,1,1]), pos=2, cex=pl_cex_axis)
#      text(x=rev(vec_buoy_radius)[1], y=array_radius_stats[7,1,length(vec_buoy_radius)], labels=paste0("N[1Hz] = ",array_radius_stats[7,1,length(vec_buoy_radius)]), pos=3, cex=pl_cex_axis)
## N samples.
#      vec_x_samp_N_poly <- c(rev(vec_buoy_radius),vec_buoy_radius)
#      vec_y_samp_N_poly <- c(rep(0,length(vec_buoy_radius)),array_radius_stats[6,1,1:length(vec_buoy_radius)])
#      polygon(vec_x_samp_N_poly,vec_y_samp_N_poly,border = NA,col="steelblue3")
#      text(x=vec_buoy_radius[1], y=array_radius_stats[6,1,1], labels=paste0("N = ",array_radius_stats[6,1,1]), pos=2, cex=pl_cex_axis)
#      text(x=rev(vec_buoy_radius)[1], y=array_radius_stats[6,1,length(vec_buoy_radius)], labels=paste0("N = ",array_radius_stats[6,1,length(vec_buoy_radius)]), pos=1, cex=pl_cex_axis)
#      par(new=TRUE)
## Plot mean bias by buoy.
#      plot(NULL,xlim=c(20,100),ylim=c(-0.1,0.1),xlab="",ylab="Hs Mean Bias (m)",cex.lab=pl_cex_lab,cex.axis=pl_cex_axis)
#      abline(h=c(-0.02,-0.01,0.01,0.02),col="grey",lwd=3)
#      abline(h=0,col="darkgrey",lwd=5)
#      for ( b_idx in 1:length(Bidx_max) ) {
#         lines(vec_buoy_radius,mat_buoy_bias_rad[b_idx,],lwd=10,col="coral1")
#         #points(vec_buoy_radius,mat_buoy_bias_rad[b_idx,],cex=1,col="coral1")
#      }      
## Legend.
#      legend(x=20,y=-0.05, legend=c("Bias (all buoys)","Bias (single buoy)"),lwd=c(18,10),col=c("firebrick2","coral1"),cex=pl_cex_leg)
## Plot mean Hs.
#      par(new=TRUE)
#      plot(vec_buoy_radius,array_radius_stats[1,1,1:length(vec_buoy_radius)],xlim=c(20,100),ylim=c(0,3),xlab="",ylab="",pch=19,col="blue",cex=5,axes=FALSE)
#      lines(c(vec_buoy_radius),array_radius_stats[1,1,1:length(vec_buoy_radius)],pch=19,col="white",cex=3)
#      axis(side=4,at=c(2,2.25,2.5,2.75,3,3.25),cex=pl_cex_axis,cex.lab=pl_cex_lab,cex.axis=pl_cex_axis)
#      mtext("Mean Hs (m)", side=4, line=8, adj=0.75, cex=pl_cex_axis)

