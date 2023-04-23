# source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/synthetic_cases/triple_collocation.R")
# Code to generate synthetic data for TC examples.
   set.seed(3903433)
   require(MASS)
   require(pracma)

#-----------------------------------------------#
   flag_scatter <- FALSE
   flag_coloc <- TRUE
#-----------------------------------------------#
# Definte datasets.
   n_samp <- 20000

# "Truth".
   mat_data <- matrix(NA,ncol=6,nrow=n_samp)

# Draw truth from gamme dsitribution (like Hs).
   mat_data[,1] <- rgamma(n_samp,3)

# Draw data set #2, moderate independent noise (zero mean).
   mat_data[,2] <- mat_data[,1] + rnorm(n_samp,0,0.15)

# Draw data set #3, moderate independent noise (non-zero mean).
   mat_data[,3] <- mat_data[,1] + (0.2*mat_data[,1]*rnorm(n_samp,0,0.5))

# Draw data set #4, low noise, highly dependent on #5.
   vec_bi_mean <- c(0, 0, 0)
   #mat_cov <- matrix(c(0.95, 1, 0.95, 1), ncol = 2)
   #mat_cov <- 0.1*matrix(c(1, 0.99, 0.99, 1), ncol = 2)
   mat_cov <- 0.1*rbind( cbind(1,    0.99, 0.99),
		         cbind(0.99, 1,    0.99),
			 cbind(0.99, 0.99, 1) )
   #mat_cov <- matrix(c(1, 1.1, 1, 1.1), ncol = 2)
   mat_bi <- mvrnorm(n=n_samp,mu=vec_bi_mean,Sigma=mat_cov)
   #X11(); plot(mat_bi)
   mat_data[,4] <- mat_data[,1] + mat_bi[,1]
   mat_data[,5] <- mat_data[,1] + mat_bi[,2]
   #mat_data[sample(1:n_samp)[1:4000],5] <- rgamma(4000,3)
   mat_data[,5] <- rep(0,n_samp)

# Draw data set #6, low noise, highly dependent on #5.
   mat_data[,6] <- mat_data[,1] + mat_bi[,3] + sapply(X=1:n_samp,FUN=function(x) { ( (mat_data[x,1]/5)^(1/4) - 0.5) } )
   #mat_data[,6] <- mat_data[,1] + mat_bi[,3]
   #mat_data[,6] <- mat_data[,1] + sapply(X=1:n_samp,FUN=function(x) { ( (mat_data[x,1]/5)^(1/4) - 0.5) } )

# Scatterplot.
   df_plot <- as.data.frame(mat_data)
   colnames(df_plot) <- c("Truth","Buoy","ERA5","J3","S6_LRM","S6_SAR")

#-----------------------------------------------#
   if (flag_scatter) {
      fig_file_name <- paste("./figures/scatterplots/synthetic.png",sep="")
      png(fig_file_name, width = 3000, height = 3000)
      par(mfrow=c(3,3),oma=c(0,0,12,0),mar=c(12,14,9,7),mgp=c(9,4,0))

# Line #1 (S6 LRM)
      plot(mat_data[,2],mat_data[,4],xlim=c(0,10),ylim=c(0,10),xlab="Buoy*",ylab="J3 LRM*",cex=4,cex.lab=5,cex.axis=5,cex.main=6); abline(0,1)
      plot(mat_data[,2],mat_data[,5],xlim=c(0,10),ylim=c(0,10),xlab="Buoy*",ylab="S6 LRM*",cex=4,cex.lab=5,cex.axis=5,cex.main=6); abline(0,1)
      plot(mat_data[,2],mat_data[,6],xlim=c(0,10),ylim=c(0,10),xlab="Buoy*",ylab="S6 SAR*",cex=4,cex.lab=5,cex.axis=5,cex.main=6); abline(0,1)
# Line #2 (J3 LRM)
      plot(NULL,xlim=c(0,10),ylim=c(0,10),xlab="",ylab="",axes=F)
      plot(mat_data[,4],mat_data[,5],xlim=c(0,10),ylim=c(0,10),xlab="J3 LRM*",ylab="S6 LRM*",cex=4,cex.lab=5,cex.axis=5,cex.main=6); abline(0,1)

      mtext(text=paste("Correlation: ",format(cor(mat_data[,4],mat_data[,5],use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
      mtext(text=paste("Mean bias: ",format(-mean(mat_data[,4],na.rm=T)+mean(mat_data[,5],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
      mtext(text=paste("RMSE: ",format(mean(sqrt(lm(J3 ~ S6_LRM,data=df_plot)$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)

      plot(mat_data[,4],mat_data[,6],xlim=c(0,10),ylim=c(0,10),xlab="Buoy*",ylab="S6 SAR*",cex=4,cex.lab=5,cex.axis=5,cex.main=6); abline(0,1)

      mtext(text=paste("Correlation: ",format(cor(mat_data[,4],mat_data[,6],use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
      mtext(text=paste("Mean bias: ",format(-mean(mat_data[,4],na.rm=T)+mean(mat_data[,6],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
      mtext(text=paste("RMSE: ",format(mean(sqrt(lm(J3 ~ S6_LRM,data=df_plot)$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)
# Line #3 (S6 LRM)
      plot(NULL,xlim=c(0,10),ylim=c(0,10),xlab="",ylab="",axes=F)
      plot(NULL,xlim=c(0,10),ylim=c(0,10),xlab="",ylab="",axes=F)
      plot(mat_data[,5],mat_data[,6],xlim=c(0,10),ylim=c(0,10),xlab="S6 LRM*",ylab="S6 SAR*",cex=4,cex.lab=5,cex.axis=5,cex.main=6); abline(0,1)

      mtext(text=paste("Correlation: ",format(cor(mat_data[,5],mat_data[,6],use="pairwise.complete.obs"),digits=3),sep=''), side=3, line=-3, adj=0.03, cex=3, outer=FALSE)
      mtext(text=paste("Mean bias: ",format(-mean(mat_data[,5],na.rm=T)+mean(mat_data[,6],na.rm=T),digits=3),sep=''), side=3, line=-6, adj=0.03, cex=3, outer=FALSE)
      mtext(text=paste("RMSE: ",format(mean(sqrt(lm(S6_SAR ~ S6_LRM,data=df_plot)$residuals^2)),digits=3),sep=''), side=3, line=-9, adj=0.03, cex=3, outer=FALSE)

# Top title.
      mtext(text=paste("Synthetic data",sep=""), side=3, line=3, adj=0.05, cex=5, outer=TRUE)

      dev.off()
      system(paste("okular",fig_file_name,"&> /dev/null &"))
   }

#-----------------------------------------------#
   if (flag_coloc) {
# Tiple collocation.
      require(ggplot2)
      #vec_data_lab <- c("Truth","Buoy","ERA5","J-3","S-6_LRM","S-6_SAR")
      vec_data_lab <- c("Truth","** Buoy","** ERA5","** J-3","ZERO","S-6_SAR")

# Calibration.
      fl_calib_thresh <- 0.5
      vec_med_diff <- abs( median(mat_data[,2]) - median(mat_data[,6]) )
      if ( vec_med_diff > fl_calib_thresh ) {
         mat_data[,6] <- mat_data[,6] - vec_med_diff
      }

      df_plot_data <- NULL
      for (i_data in 1:3) {
         #i_data1 <- c(4,5,6)[i_data]
         i_data1 <- c(3,5,6)[i_data]
# Bootstrap uncertainty.
# Get the indices of the triplets.
         vec_coloc_idx <- 1:n_samp
         mat_D <- matrix(NA,ncol=3,nrow=n_samp)

         n_boot <- 2000
         mat_sqrt <- matrix(NA,ncol=3,nrow=n_boot)

         for (ii in 1:n_boot) {
            vec_idx <- sample(vec_coloc_idx,replace=TRUE)
            D1 <- mat_data[vec_idx,i_data1]
            #D1 <- mat_data[vec_idx,2]
            D2 <- mat_data[vec_idx,4]
            D3 <- mat_data[vec_idx,2]
            #D3 <- mat_data[vec_idx,i_data1]

            V12 <- var(D1-D2,na.rm=T)
            V31 <- var(D3-D1,na.rm=T)
            V23 <- var(D2-D3,na.rm=T)

            mat_sqrt[ii,1] <- sqrt( (V12+V31-V23)/2 )
            mat_sqrt[ii,2] <- sqrt( (V23+V12-V31)/2 )
            mat_sqrt[ii,3] <- sqrt( (V31+V23-V12)/2 )
         }
         df_plot_data_temp <- rbind(
                                    #data.frame(mean_e=mean(mat_sqrt[,1],na.rm=T),sd_e=sqrt(var(mat_sqrt[,1],na.rm=T)),mission=vec_data_lab[2],group=i_data),
                                    data.frame(mean_e=mean(mat_sqrt[,1],na.rm=T),sd_e=sqrt(var(mat_sqrt[,1],na.rm=T)),mission=vec_data_lab[i_data1],group=i_data),
                                    data.frame(mean_e=mean(mat_sqrt[,2],na.rm=T),sd_e=sqrt(var(mat_sqrt[,2],na.rm=T)),mission=vec_data_lab[4],group=i_data),
                                    data.frame(mean_e=mean(mat_sqrt[,3],na.rm=T),sd_e=sqrt(var(mat_sqrt[,3],na.rm=T)),mission=vec_data_lab[2],group=i_data) )
                                    #data.frame(mean_e=mean(mat_sqrt[,3],na.rm=T),sd_e=sqrt(var(mat_sqrt[,3],na.rm=T)),mission=vec_data_lab[i_data1],group=i_data) )
         df_plot_data <- rbind(df_plot_data,df_plot_data_temp)
      }

#-----------------------------------------------#
# Plotting.
      #fig_file_name <- paste("./figures/bar_plots/plot_ALL_JTEX_",n_samp,".png",sep="")
      fig_file_name <- paste("./figures/bar_plots/plot_J3_",n_samp,"_25_1.png",sep="")

      p1 <- ggplot(df_plot_data,aes(x = as.factor(group), y = mean_e, fill = as.factor(mission))) + 
      geom_col(position = "dodge") +
      geom_errorbar(aes(ymin = mean_e-sd_e, ymax = mean_e+sd_e), width=0.2,
                    position=position_dodge(0.9)) +
      ylim(0,0.5) +
      #ggtitle(paste(str_region," ",buoy_radius," km [N_coloc=",df_plot_data$n_coloc[1],"]",sep="")) +
      labs(y="Mean error (m)",fill='** Synthetic Dataset') +
      
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

## Regression methods
#   df_test <- data.frame(x=((1:1000)/100)+rnorm(1000),y=((1:1000)/100)+rnorm(1000))
#   lm_fit <- lm(y ~ x, data=df_test)
#   odr <- odregress(df_test$x,df_test$y)
#   X11(); plot(df_test,xlim=c(0,13),ylim=c(0,13)); abline(lm_fit,col="blue",lwd=2); abline(a=odr$coeff[2],b=odr$coeff[1],col="red",lwd=2)
#
#   #TT <- (1:1000)/100
#   #AA <- rnorm(1000)
#   #BB <- rnorm(1000)
#   #var( TT ) + var( AA ) + 2*cov( TT, AA ) + var( TT ) + var( BB ) + 2*cov( TT, BB ) - var( AA ) - var( BB ) - 2*cov( AA, BB )
#   #2*var( TT ) + 2*cov( TT, AA ) + 2*cov( TT, BB ) - 2*cov( AA, BB )

