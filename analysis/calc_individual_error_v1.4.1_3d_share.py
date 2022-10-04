
import math
import numpy as np
#import rpy2.robjects.numpy2ri


import matplotlib.pyplot as plt
plt.rcParams['figure.dpi']= 300 # set resolution of inline plots

import scipy.io as sp
from scipy import stats

import seaborn as sns
sns.set_style("white")


##import data
s3tc_data_array = sp.loadmat('e5_natl_05_04.mat')
col_headers=s3tc_data_array['col_head_swh']
swh_data=s3tc_data_array['swh_table_e5_natl_mat']
swh_medians=swh_data[:,3:9] #medians -Asar,Aplrm,Bsar,Bplrm,Blrm,E5

##create output array
error_out_table=np.empty((3,4,3)) #3 obs groups, 4 error assesments , 3 in each group
num_records=np.empty((3,1)) #3 obs groups

obs_names=("S3A SAR","S3A PLRM","S3B SAR","S3B PLRM","S3B LRM","ERA5",)
error_out_table[:] = np.NaN
num_records[:] = np.NaN

obs_groups=np.array([[5,0,2],[5,1,3],[5,1,4]])

##Divide regions

for cc in range(0,3):
    ##subset by range
    #rename variables for function cores
    curr_obs_set=obs_groups[cc,:]
    print("--------",obs_names[curr_obs_set[0]],"/",obs_names[curr_obs_set[1]],"/",obs_names[curr_obs_set[2]],"--------")
    x1=swh_medians[:,curr_obs_set[0]]
    x2=swh_medians[:,curr_obs_set[1]]
    x3=swh_medians[:,curr_obs_set[2]]
    
    ##Sort nans in data
    nan_idx=np.isnan(x1) | np.isnan(x2) | np.isnan(x3) #idx nans in any 
    x1=x1[~nan_idx]#remove nans
    x2=x2[~nan_idx]
    x3=x3[~nan_idx]

    ###do some basic distributions
    kwargs = dict(hist_kws={'alpha':.6}, kde_kws={'linewidth':2})
    
    plt.figure(figsize=(10,7), dpi= 80)
    sns.distplot(x1, color="dodgerblue", label=obs_names[curr_obs_set[0]], **kwargs)
    sns.distplot(x2, color="orange", label=obs_names[curr_obs_set[1]], **kwargs)
    sns.distplot(x3, color="deeppink", label=obs_names[curr_obs_set[2]], **kwargs)
    plt.legend();
    
    ###do some basic qq plots to justify filtering
    fig = plt.figure()
    ax1 = plt.subplot(221)
    res = stats.probplot(x1, plot=plt)
    ax2 = plt.subplot(222)
    res = stats.probplot(x2, plot=plt)
    ax3 = plt.subplot(223)
    res = stats.probplot(x3, plot=plt)
    plt.show()
    
    ##remove outliers (high ones - because of swh distributions)
    x1[x1>(np.nanmean(x1)+3*np.nanstd(x1))]=float("NaN")
    x2[x2>(np.nanmean(x2)+3*np.nanstd(x2))]=float("NaN")
    x3[x3>(np.nanmean(x3)+3*np.nanstd(x3))]=float("NaN") 
    
       
    #bias_correction - get offsets
    mdn_x1=np.nanmedian(x1)
    mdn_x2=np.nanmedian(x2)
    mdn_x3=np.nanmedian(x3)
    
    #offset if sufficiently offset (buoys = reference, arbitrary valu eof 0.2m)
    if math.fabs(mdn_x2-mdn_x1)>0.2:
        x2=x2-(mdn_x2-mdn_x1)
        
    if math.fabs(mdn_x3-mdn_x1)>0.2:
        x3=x3-(mdn_x3-mdn_x1)
            
        
    ##Sort nans in data
    nan_idx=np.isnan(x1) | np.isnan(x2) | np.isnan(x3) ##idx nans in any 
    x1=x1[~nan_idx]#remove nans
    x2=x2[~nan_idx]
    x3=x3[~nan_idx]

    num_records[cc]=np.size(x1)
    ###replot some basic distributions
    kwargs = dict(hist_kws={'alpha':.6}, kde_kws={'linewidth':2})
    
    plt.figure(figsize=(10,7), dpi= 80)
    sns.distplot(x1, color="dodgerblue", label=obs_names[curr_obs_set[0]], **kwargs)
    sns.distplot(x2, color="orange", label=obs_names[curr_obs_set[1]], **kwargs)
    sns.distplot(x3, color="deeppink", label=obs_names[curr_obs_set[2]], **kwargs)
    #plt.xlim(50,75)
    plt.legend();
    
    ###replot some basic qq 
    fig = plt.figure()
    ax1 = plt.subplot(221)
    res = stats.probplot(x1, plot=plt)
    ax2 = plt.subplot(222)
    res = stats.probplot(x2, plot=plt)
    ax3 = plt.subplot(223)
    res = stats.probplot(x3, plot=plt)
    plt.show()
    
    
    ###CalcThreePoint(self, x1, x2, x3, txt, bDayNight):
            
    ##CalcThreePointOCArrol(x1, x2, x3, txt, bDayNight)
    print( "----  O'Carrol  ----"    )
    xn1 = np.array(x1)
    xn2 = np.array(x2)
    xn3 = np.array(x3)    
               
    v12 = np.var(xn1-xn2)
    v31= np.var(xn3-xn1)
    v23 = np.var(xn2-xn3)
    print("Number of records")
    print(num_records[cc])
    print("Variances of differences between datasets")
    print("Obs1/Obs2           Obs1/Obs3          Obs2/Obs3")
    print( v12, v31, v23)
    
    s1 = math.sqrt(0.5*(math.fabs(v12 + v31 - v23)))   
    s2 = math.sqrt(0.5*(math.fabs(v23 + v12 - v31)))  
    s3 = math.sqrt(0.5*(math.fabs(v31 + v23 - v12)))    
    print("Standard deviation of individual datasets' error") # note should be almost identical to Challenor 1 std.
    print("Obs1                Obs2                Obs3")
    print( s1, s2, s3)
    print( "----  O'Carrol End  ----")
            
    #save O'Carrol
    error_out_table[cc,0,:]=(v12,v31,v23)
    error_out_table[cc,1,:]=(s1,s2,s3)
    
    # ## R code to estimate errors in three r.v.'s using Tokmakian and Challenor
    # ## (1999) On the joint estimation of model and satellite sea surface
    # ## height anomaly errors Ocean Modelling, 1, 39-52
    
    # #reshape three separate (n,) arrays into a (3,n) array (surely there's a more direct way? i.e. x= np.array(xn1, xn2, xn3).T)
    # x0 = np.append(xn1, xn2)
    # x = np.append(x0, xn3)
    # x= x.reshape(3, len(x1))
    
    # ##  now calculate the covariance matrix
    # cr = np.cov(x)
    
    # ## calculate the errors
    # error1 = cr[0,0]-cr[0,1]+cr[1,2]-cr[0,2]
    # error2 = cr[1,1]-cr[0,1]+cr[0,2]-cr[1,2]
    # error3 = cr[2,2]-cr[0,2]+cr[0,1]-cr[1,2]
    #    # error4 = c[3,3]-c[0,2]+c[0,1]-c[1,2]
       
       
    # #error1<-c[1,1]-c[1,2]+c[2,3]-c[1,3]
    # #error2<-c[2,2]-c[1,2]+c[1,3]-c[2,3]
    # #error3<-c[3,3]-c[1,3]+c[1,2]-c[2,3]
       
    # print( '----  Challenor 1 Var ----')
    # print("Obs1                Obs2               Obs3")
    # print( error1,error2,error3) #,error4)
    # if error1>=0 and error2>=0 and error3>=0:
    #     print( '----  Challenor 1 Std ----')
    #     print("Obs1                Obs2                Obs3")
    #     print( math.sqrt(error1), math.sqrt(error2), math.sqrt(error3))
    #     error_out_table[cc,3,:]=(math.sqrt(error1), math.sqrt(error2), math.sqrt(error3))
    # else:
    #     print('Negative errors, cannot calc sqrt') #likely occurs as result of lack of filtering / QC
    # print( '----  Challenor 1 end ----')
    
    # #save Challenor
    # error_out_table[cc,2,:]=(error1,error2,error3)


#plot O'carrol 2 - individual datasets
# Create bars
barWidth = 0.9
 
# The X position of bars
r1 = [1,5,9]
r2 = [2]
r3 = [6,10]
r4 = [3]
r5 = [7]
r6 = [11] 
# Create barplot
fig = plt.figure()
ax = fig.add_subplot(1, 1, 1)
plt.barh(r1, np.ndarray.tolist(error_out_table[(0,1,2),1,0]), height = barWidth, color = (0.55,0.55,0.55), label='ERA-5')
plt.barh(r2, (error_out_table[(0),1,1]), height = barWidth, color = (0.22,0.49,0.72), label='$A_{SAR}$')
plt.barh(r3, (error_out_table[(1,2),1,(1)]), height = barWidth, color = (1,1,1), label='$A_{PLRM}$',edgecolor=(0.22,0.49,0.72), hatch="/")
plt.barh(r4, (error_out_table[(0),1,2]), height = barWidth, color = (0.89,0.10,0.11), label='$B_{SAR}$')
plt.barh(r5, (error_out_table[(1),1,2]), height = barWidth, color = (1,1,1), label='$B_{PLRM}$',edgecolor=(0.89,0.10,0.11), hatch="/")
plt.barh(r6, (error_out_table[(2),1,2]), height = barWidth, color = (0.89,0.10,0.11), label='$B_{LRM}$',edgecolor=(0.89,0.10,0.11), fill=False)


#xlabel
plt.xlabel(r'$SWH\ Standard\ Deviation\ of\ Error\ (m)$')
#grid
yt_l = [2,6,10]
ax.set_yticks(yt_l)
plt.grid(b=None, which='major', axis='both')
# Create legend
plt.legend(bbox_to_anchor=(1.05, 1), loc=2)

plt.title( '%i records' % num_records[0,:])
# Region ticks on y axis
r_test = [2,6,10,1,5,9]
plt.yticks([r + barWidth for r in r_test], ['$ERA-5/A_{SAR}/B_{SAR}$','$ERA-5/A_{PLRM}/B_{PLRM}$','$ERA-5/A_{PLRM}/B_{LRM}$',num_records[0],num_records[1],num_records[2]], rotation=0)
# Adjust the margins
plt.subplots_adjust(bottom= 0.2, top = 0.98)




