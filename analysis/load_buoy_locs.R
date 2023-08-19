# source("/home/ben/research/NOC/projects/s6-j3_tandem/analysis/load_buoy_locs.R")

# Script to load buoy locations.
# BT 04/2022

# ================================================================= #
# Buoy location.
# ----------------------------------------------------------------- #
# North Atlantic (West).
   buoy_44137 <- data.frame(buoy_lat=42.262,buoy_lon=-62.03, name=44137)
   buoy_44139 <- data.frame(buoy_lat=44.24,buoy_lon=-57.1, name=44139)
   buoy_44005 <- data.frame(buoy_lat=43.201,buoy_lon=-69.128, name=44005)
   buoy_44007 <- data.frame(buoy_lat=43.525,buoy_lon=-70.141, name=44007)
   buoy_44020 <- data.frame(buoy_lat=41.493,buoy_lon=-70.279, name=44020)
   buoy_44008 <- data.frame(buoy_lat=40.504,buoy_lon=-69.248, name=44008)
   buoy_41110 <- data.frame(buoy_lat=34.141,buoy_lon=-77.717, name=41110)
   buoy_41113 <- data.frame(buoy_lat=28.400,buoy_lon=-80.534, name=41113)
   buoy_44011 <- data.frame(buoy_lat=41.093,buoy_lon=-66.562, name=44011)
   buoy_44014 <- data.frame(buoy_lat=36.601,buoy_lon=-74.834, name=44014)
   buoy_41001 <- data.frame(buoy_lat=34.719,buoy_lon=-72.33, name=41001)
   buoy_41002 <- data.frame(buoy_lat=31.973,buoy_lon=-74.958, name=41002)
   buoy_41010 <- data.frame(buoy_lat=28.878,buoy_lon=-78.485, name=41010)
   buoy_41040 <- data.frame(buoy_lat=14.542,buoy_lon=-53.341, name=41040)
   buoy_41041 <- data.frame(buoy_lat=14.453,buoy_lon=-46.327, name=41041)
   buoy_41043 <- data.frame(buoy_lat=21.026,buoy_lon=-64.793, name=41043)
   buoy_41044 <- data.frame(buoy_lat=21.582,buoy_lon=-58.63, name=41044)
   buoy_41047 <- data.frame(buoy_lat=27.465,buoy_lon=-71.452, name=41047)
   buoy_41048 <- data.frame(buoy_lat=31.831,buoy_lon=-69.573, name=41048)
   buoy_41049 <- data.frame(buoy_lat=27.490,buoy_lon=-62.938, name=41049)
## Atlantic (south).
#   buoy_42060 <- data.frame(lat=16.406, lon=-63.188, name="42060", nudge=0)
#   buoy_41002 <- data.frame(lat=31.760, lon=-74.84, name="41002", nudge=0)
#   buoy_41040 <- data.frame(lat=14.559, lon=-53.073, name="41040", nudge=0)
#   buoy_41043 <- data.frame(lat=21.132, lon=-64.856, name="41043", nudge=0)
#   buoy_41044 <- data.frame(lat=21.575, lon=-58.625, name="41044", nudge=0)
#   buoy_41046 <- data.frame(lat=23.832, lon=-68.417, name="41046", nudge=0)
#   buoy_41047 <- data.frame(lat=27.520, lon=-71.53, name="41047", nudge=0)
#   buoy_41048 <- data.frame(lat=31.831, lon=-69.573, name="41048", nudge=0)
#   buoy_41049 <- data.frame(lat=27.490, lon=-62.938, name="41049", nudge=0)
#   buoy_41110 <- data.frame(lat=34.141, lon=-77.717, name="41110", nudge=0)
#   buoy_41112 <- data.frame(lat=30.709, lon=-81.292, name="41112", nudge=0)
#   buoy_41113 <- data.frame(lat=28.400, lon=-80.534, name="41113", nudge=0)
#   buoy_41114 <- data.frame(lat=27.550, lon=-80.217, name="41114", nudge=0)

# ----------------------------------------------------------------- #
# North Atlantic (East).
# ----------------------------------------------------------------- #
# Gulf of Mexico.
   #buoy_42002 <- data.frame(buoy_lat=26.055,buoy_lon=-93.646, name=42002)
   #buoy_42003 <- data.frame(buoy_lat=25.925,buoy_lon=-85.615, name=42003)
   buoy_42001 <- data.frame(buoy_lat=25.897, buoy_lon=-89.668, name="42001")
   buoy_42002 <- data.frame(buoy_lat=26.091, buoy_lon=-93.758, name="42002")
   buoy_42003 <- data.frame(buoy_lat=26.007, buoy_lon=-85.648, name="42003")
   buoy_42019 <- data.frame(buoy_lat=27.907, buoy_lon=-95.352, name="42019")
   buoy_42036 <- data.frame(buoy_lat=28.501, buoy_lon=-84.516, name="42036")
   buoy_42035 <- data.frame(buoy_lat=29.232, buoy_lon=-94.413, name="42035")
   buoy_42039 <- data.frame(buoy_lat=28.788, buoy_lon=-86.008, name="42039")
   buoy_42040 <- data.frame(buoy_lat=29.208, buoy_lon=-88.226, name="42040")
   buoy_42055 <- data.frame(buoy_lat=22.120, buoy_lon=-93.96, name="42055")
   buoy_42059 <- data.frame(buoy_lat=15.252, buoy_lon=-67.483, name="42059")
   buoy_42085 <- data.frame(buoy_lat=17.869, buoy_lon=-66.532, name="42085")
# ----------------------------------------------------------------- #
# North Pacific (Offshore).
   buoy_46035 <- data.frame(buoy_lat=57.016,buoy_lon=-177.703, name="46035", situ="OS")
   buoy_46073 <- data.frame(buoy_lat=55.008,buoy_lon=-172.012, name="46073", situ="OS")
   buoy_46072 <- data.frame(buoy_lat=51.666,buoy_lon=-172.114, name="46072", situ="OS")
   buoy_46075 <- data.frame(buoy_lat=53.969,buoy_lon=-160.794, name="46075", situ="OS")
   buoy_46066 <- data.frame(buoy_lat=52.765,buoy_lon=-155.009, name="46066", situ="OS")
   buoy_46078 <- data.frame(buoy_lat=55.557,buoy_lon=-152.641, name="46078", situ="OS")
   buoy_46246 <- data.frame(buoy_lat=50.017,buoy_lon=-145.170, name="46246", situ="OS")
   buoy_46085 <- data.frame(buoy_lat=55.883,buoy_lon=-142.882, name="46085", situ="OS")
   buoy_46001 <- data.frame(buoy_lat=56.232,buoy_lon=-147.949, name="46001", situ="OS")
# No data 2021.
   #buoy_46004 <- data.frame(buoy_lat=50.930,buoy_lon=-136.100, name="46004", situ="OS")
   #buoy_46036 <- data.frame(buoy_lat=48.360,buoy_lon=-133.940, name="46036, situ="OS")
   buoy_46002 <- data.frame(buoy_lat=42.658,buoy_lon=-130.502, name="46002", situ="OS")
   buoy_46005 <- data.frame(buoy_lat=46.134,buoy_lon=-131.079, name="46005", situ="OS")
   buoy_46006 <- data.frame(buoy_lat=40.766,buoy_lon=-137.379, name="46006", situ="OS")
   buoy_46059 <- data.frame(buoy_lat=38.094,buoy_lon=-129.951, name="46059", situ="OS")
   buoy_32012 <- data.frame(buoy_lat=-19.425,buoy_lon=-85.078, name="32012", situ="OS")
# ----------------------------------------------------------------- #
# North Pacific (Nearshore).
   buoy_46077 <- data.frame(buoy_lat=57.869,buoy_lon=-154.211, name="46077", situ="NS")
   buoy_46080 <- data.frame(buoy_lat=57.947,buoy_lon=-150.042, name="46080", situ="NS")
   buoy_46076 <- data.frame(buoy_lat=59.471,buoy_lon=-148.009, name="46076", situ="NS")
   buoy_46082 <- data.frame(buoy_lat=59.681,buoy_lon=-143.372, name="46082", situ="NS")
   buoy_46083 <- data.frame(buoy_lat=58.270,buoy_lon=-138.019, name="46083", situ="NS")
   buoy_46084 <- data.frame(buoy_lat=56.622,buoy_lon=-136.102, name="46084", situ="NS")
   buoy_46205 <- data.frame(buoy_lat=54.180,buoy_lon=-134.320, name="46205", situ="NS")
   buoy_46145 <- data.frame(buoy_lat=54.370,buoy_lon=-132.420, name="46145", situ="NS")
   buoy_46183 <- data.frame(buoy_lat=53.620,buoy_lon=-131.100, name="46183", situ="NS")
   buoy_46208 <- data.frame(buoy_lat=52.520,buoy_lon=-132.690, name="46208", situ="NS")
   buoy_46185 <- data.frame(buoy_lat=52.420,buoy_lon=-129.790, name="46185", situ="NS")
   buoy_46204 <- data.frame(buoy_lat=51.380,buoy_lon=-128.770, name="46204", situ="NS")
   buoy_46147 <- data.frame(buoy_lat=51.830,buoy_lon=-131.220, name="46147", situ="NS")
   buoy_46207 <- data.frame(buoy_lat=50.870,buoy_lon=-129.920, name="46207", situ="NS")
   buoy_46100 <- data.frame(buoy_lat=46.851,buoy_lon=-124.972, name="46100", situ="NS")
   buoy_46041 <- data.frame(buoy_lat=47.353,buoy_lon=-124.742, name="46041", situ="NS")
   buoy_46099 <- data.frame(buoy_lat=46.986,buoy_lon=-124.566, name="46099", situ="NS")
   buoy_46211 <- data.frame(buoy_lat=46.857,buoy_lon=-124.244, name="46211", situ="NS")
   buoy_46248 <- data.frame(buoy_lat=46.133,buoy_lon=-124.640, name="46248", situ="NS")
   buoy_46029 <- data.frame(buoy_lat=46.163,buoy_lon=-124.487, name="46029", situ="NS")
   buoy_46243 <- data.frame(buoy_lat=46.216,buoy_lon=-124.128, name="46243", situ="NS")
   buoy_46089 <- data.frame(buoy_lat=45.936,buoy_lon=-125.793, name="46089", situ="NS")
   buoy_46098 <- data.frame(buoy_lat=44.381,buoy_lon=-124.956, name="46098", situ="NS")
   buoy_46050 <- data.frame(buoy_lat=44.669,buoy_lon=-124.546, name="46050", situ="NS")
   buoy_46097 <- data.frame(buoy_lat=44.639,buoy_lon=-124.304, name="46097", situ="NS")
   buoy_46229 <- data.frame(buoy_lat=43.772,buoy_lon=-124.549, name="46229", situ="NS")
   buoy_46015 <- data.frame(buoy_lat=42.752,buoy_lon=-124.844, name="46015", situ="NS")
   buoy_46027 <- data.frame(buoy_lat=41.840,buoy_lon=-124.382, name="46027", situ="NS")
   buoy_46244 <- data.frame(buoy_lat=40.896,buoy_lon=-124.357, name="46244", situ="NS")
   buoy_46022 <- data.frame(buoy_lat=40.701,buoy_lon=-124.55, name="46022", situ="NS")
   buoy_46213 <- data.frame(buoy_lat=40.295,buoy_lon=-124.732, name="46213", situ="NS")
   buoy_46014 <- data.frame(buoy_lat=39.231,buoy_lon=-123.974, name="46014", situ="NS")
   buoy_46013 <- data.frame(buoy_lat=38.253,buoy_lon=-123.303, name="46013", situ="NS")
   buoy_46214 <- data.frame(buoy_lat=37.937,buoy_lon=-123.463, name="46214", situ="NS")
   buoy_46026 <- data.frame(buoy_lat=37.754,buoy_lon=-122.839, name="46026", situ="NS")
   buoy_46012 <- data.frame(buoy_lat=37.356,buoy_lon=-122.881, name="46012", situ="NS")
   buoy_46042 <- data.frame(buoy_lat=36.785,buoy_lon=-122.396, name="46042", situ="NS")
   buoy_46114 <- data.frame(buoy_lat=36.700,buoy_lon=-122.343, name="46114", situ="NS")
   buoy_46239 <- data.frame(buoy_lat=36.335,buoy_lon=-122.104, name="46239", situ="NS")
   buoy_46011 <- data.frame(buoy_lat=34.937,buoy_lon=-121.000, name="46011", situ="NS")
   buoy_46259 <- data.frame(buoy_lat=34.767,buoy_lon=-121.498, name="46259", situ="NS")
   buoy_46218 <- data.frame(buoy_lat=34.452,buoy_lon=-120.780, name="46218", situ="NS")
   buoy_46054 <- data.frame(buoy_lat=34.274,buoy_lon=-120.468, name="46054", situ="NS")
   buoy_46053 <- data.frame(buoy_lat=34.241,buoy_lon=-119.839, name="46053", situ="NS")
   buoy_46069 <- data.frame(buoy_lat=33.677,buoy_lon=-120.213, name="46069", situ="NS")
   buoy_46251 <- data.frame(buoy_lat=33.769,buoy_lon=-119.565, name="46251", situ="NS")
   buoy_46025 <- data.frame(buoy_lat=33.755,buoy_lon=-119.045, name="46025", situ="NS")
   buoy_46219 <- data.frame(buoy_lat=33.219,buoy_lon=-119.872, name="46219", situ="NS")
   buoy_46047 <- data.frame(buoy_lat=32.388,buoy_lon=-119.525, name="46047", situ="NS")
   buoy_46086 <- data.frame(buoy_lat=32.499,buoy_lon=-118.052, name="46086", situ="NS")
   buoy_46258 <- data.frame(buoy_lat=32.749,buoy_lon=-117.502, name="46258", situ="NS")
   buoy_46232 <- data.frame(buoy_lat=32.517,buoy_lon=-117.425, name="46232", situ="NS")
# ----------------------------------------------------------------- #
# Hawaii (Offshore).
   buoy_51000 <- data.frame(buoy_lat=23.528,buoy_lon=-153.792, name="51000", situ="OS")
   buoy_51001 <- data.frame(buoy_lat=24.453,buoy_lon=-162.0, name="51001", situ="OS")
   buoy_51101 <- data.frame(buoy_lat=24.359,buoy_lon=-162.081, name="51101", situ="OS")
   buoy_51002 <- data.frame(buoy_lat=17.042,buoy_lon=-157.746, name="51002", situ="OS")
   buoy_51003 <- data.frame(buoy_lat=19.196,buoy_lon=-160.639, name="51003", situ="OS")
   buoy_51004 <- data.frame(buoy_lat=17.533,buoy_lon=-152.255, name="51004", situ="OS")
# ----------------------------------------------------------------- #
# Hawaii (Nearshore).
   buoy_51208 <- data.frame(buoy_lat=22.285,buoy_lon=-159.574, name="51208", situ="NS")
   buoy_51201 <- data.frame(buoy_lat=21.671,buoy_lon=-158.117, name="51201", situ="NS")
   buoy_51212 <- data.frame(buoy_lat=21.323,buoy_lon=-158.149, name="51212", situ="NS")
   buoy_51211 <- data.frame(buoy_lat=21.297,buoy_lon=-157.959, name="51211", situ="NS")
   buoy_51207 <- data.frame(buoy_lat=21.477,buoy_lon=-157.752, name="51207", situ="NS")
   buoy_51202 <- data.frame(buoy_lat=21.417,buoy_lon=-157.680, name="51202", situ="NS")
   buoy_51213 <- data.frame(buoy_lat=20.750,buoy_lon=-157.003, name="51213", situ="NS")
   buoy_51205 <- data.frame(buoy_lat=21.018,buoy_lon=-156.427, name="51205", situ="NS")
   buoy_51206 <- data.frame(buoy_lat=19.779,buoy_lon=-154.970, name="51206", situ="NS")
# ----------------------------------------------------------------- #
# West Pacific (General).
# (52212)
   buoy_52200 <- data.frame(buoy_lat=13.354,buoy_lon=144.788, name="52200", situ="OS")
   buoy_52202 <- data.frame(buoy_lat=13.682,buoy_lon=144.806, name="52202", situ="OS")
   buoy_52211 <- data.frame(buoy_lat=15.268,buoy_lon=145.662, name="52211", situ="OS")
   buoy_52201 <- data.frame(buoy_lat=7.083,buoy_lon=171.392, name="52201", situ="OS")
   buoy_51209 <- data.frame(buoy_lat=-14.273,buoy_lon=-170.500, name="51209", situ="OS")
# ----------------------------------------------------------------- #

# List of buoys to use.
   buoy_list <- c(46246,46085,51004,51001,32012,44011,44137,44139,41010,41040,41044,41048,41049,46006,46082,46083,46084,46059,46002,46005,46001)
# Pacific Offshore.
   str_region <- "PAC_OS"
   buoy_list_PAC_OS <- c(46035,46073,46072,46075,46066,46078,46246,46085,46001,46002,46005,46006,46059)
# Pacific Nearshore.
   str_region <- "PAC_NS"
   buoy_list_PAC_NS <- c(46077,46080,46076,46082,46083,46084,46205,46145,46183,46208,46185,46204,46147,46207,46100,46041,46099,46211,46248,46029,46243,46089,46098,46050,46097,46229,46015,46027,46244,46022,46213,46014,46013,46214,46026,46012,46042,46114,46239,46011,46259,46218,46054,46053,46069,46251,46025,46219,46047,46086,46258,46232)
# Hawaii Offshore.
   str_region <- "Hawaii_OS"
   buoy_list_Hawaii_OS <- c(51000,51001,51101,51002,51003,51004)
# Hawaii Nearshore.
   str_region <- "Hawaii_NS"
   buoy_list_Hawaii_NS <- c(51208,51201,51212,51211,51207,51202,51213,51205,51206)
# West Pacific (General).
   str_region <- "PAC_west"
   buoy_list_PAC_west <- c(52200,52202,52211,52201,51209)

   #str_region <- "PAC_ALL"
   #buoy_list <- c(buoy_list_PAC_NS,buoy_list_PAC_OS)
   str_region <- "PAC_OS"
   buoy_list <- c(buoy_list_PAC_OS)

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

