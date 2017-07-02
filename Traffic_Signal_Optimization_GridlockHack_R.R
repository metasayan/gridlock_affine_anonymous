
#####################################################################################################
############
##                  FLIPKART GRIDLOCK HACKATHON - TRAFFIC SIGNAL OPTIMIZATION 
############
#####################################################################################################

###########################
##### OBJECTIVE - 
###########################

## solve Bangalore's Traffic Problem 

###########################
##### OUR SOLUTION -
###########################

## Optimization of Traffic Signals based on real-time Google and Bing Map data
  # => Currently applied to the SILK BOARD Junction [Busiest Junction of Bangalore, 7th Busiest across the nation]

###########################
##### Inputs - 
###########################

## Google Maps Traffic Data [Using Google Map API] - 
  # 1. Wait Times at each signal 

## Bing Maps Traffic Data [Using Bing Map API] - Traffic Map Image at a given instant 
  # 1. Using Image Processing, extract the length of immediate heavy/moderate traffic at the signal
  # 2. Using Image Processing, extract the length of heavy/moderate traffic at the complete stretch of road

###########################
##### Approach - 
###########################

## The current system installed at Silk Board Junction switches cycle times 6 times a day.
# But in our Exploratory Data Analysis, we found that this system is not efficient enough, as it is unable to take care of the traffic changes during the span of the day
  # For e.g. 
      # Traffic from HSR side to Silk Board remains at peaks at around 10:00AM and then significantly drops till 12PM 
      # While the traffic from BTM has already peaked at 9AM and significantly drops afterwards
      # The traffic signal time remains the same between 8AM and 12PM leading to huge jam in the morning from HSR to Silk Board

## Now using the above mentioned Inputs, we have 3 variables (mentioned below)
  # List of Variables -
      ## Wait Time at the signal [ Taken 500 metres in all the 4 directions ]
      ## Length of Immediate Heavy/Moderate Traffic at all the 4 traffic signal [ Using Image Processing ]
      ## Length of Heavy/Moderate Traffic on the whole stretch on all the 4 roads [ Nearly 3 Kms in every directions]

## using the above 3 Inputs, a single Traffic Congestion Index is calculated
  # A weighted combination of all 3 variables is taken for creation of this new metric
    ## The Weights are ascertained using a simple Linear Regression on multiple days of data
      ## Weights => Wait Time = 0.6, Length of Immediate Traffic = 0.25, Length of Traffic on complete stretch = 0.15

## Now there are 2 things that can be optimized - 
  # 1. Total cycle time [ The total time in which all the traffic signals become Green once]
  # 2. Individual Traffic Signal Time 
    ## This is done using the Traffic Congestion Index calculated using the above variables and corresponding weights

## Other Assumptions -
  # The current signal cycle time varies between 110 and 280 seconds
    ## We have taken a little little liberty and optimized the total cycle time between 90 and 360 seconds  [ ~20% buffer on each extreme ] 
    ## Also, the individual traffic signals can never be less than 15 seconds

## Implementation Logic - 
  # The total Traffic Congestion Index at the Silk Board Junction will decide the Total Cycle Time
    ## The total is calculated as a sum of individual Signals [3 in our case as Bommanahallli & Madiwala Signals go Green at the same instant]
  # The individual Traffic Congestion Indices are then used to divide the Total Cycle Time calculated above

## Impact Measurement - 
  # Impact of this whole logic has been tested and measured - The change in wait times can be simply calculated based on modified signal times
  # Based on EDA, we have been able to establish the Man Hours Saved [ ~17% of every second at the signal ]
  # Based on Statistics, we have been able to find the Reduction in CO2 Emissions and Fuel Consumption


###########################
##### CODE - 
###########################


### Installing all the relevant libraries - [Uncomment only if a Library is not installed]

## Comment and Ctrl+Shift+C to Uncomment and Then Comment again once done with module installations

# install.packages("plyr")
# install.packages("jpeg")
# install.packages("png")
# install.packages("dplyr")
# install.packages("gmapsdistance")
# install.packages("ggmap")
# install.packages("XML")
# install.packages("RCurl")
# install.packages("methods")
# install.packages("stats")
# install.packages("RgoogleMaps")
# install.packages("svDialogs")
# install.packages("jsonlite")

###########################
### Initializing all the required packages
###########################

library(plyr)
library(jpeg)
library(png)
library(dplyr)
library(gmapsdistance)
library(ggmap)
library(XML)
library(RCurl)
library(methods)
library(stats)
library(RgoogleMaps)
library(svDialogs)
library(jsonlite)

### Setting the Working Directory - [User Input] 

## The shared R project should be placed in this directory
    ## It should contain the following files and codes - 
          # 1. silkboard_coordinates.csv
          # 2. silk_board_road.jpeg
          # 3. Traffic_Signal_Optimization_GridlockHack_R.R

setwd(dlgDir(default = getwd(), title = "Select the Working Directory")$res)

###########################
### Setting all the keys, constants and constraints in the code
###########################

# The below Google and Bing APIs are specially created for the Hackathon
# Both these APIs correspond to the free versions
# Ping Limits ==> Google - 2500 per day, Bing  - 125000 per month

google_api_key = "AIzaSyAg1AeMusLDvBNxUIr-_clMeh84EE5izK0"
bing_api_key = "AlMeeGk8ljiG13V9zVgpSlpl7WeNt1NzyODT648buA4x9dhBF0nFkY4G9DGqFZIt"

### Latitude and Longitude for All 4 traffic Signals at the Silk Board Junction 

hsr_btm_lat = 12.9172365
hsr_btm_long = 77.6230248
btm_hsr_lat = 12.9172574
btm_hsr_long = 77.6224830 
madiwala_ec_lat = 12.9175868
madiwala_ec_long = 77.6227727 
ec_madiwala_lat = 12.9170901
ec_madiwala_long = 77.6226815 


### Weights as described in the Approach above [Computed using Regression]

alpha_wt = 0.6 
alpha_ri = 0.25
alpha_rw = 0.15
constant = 0

### Impact Measurement Metrics -

manhours_saved = 0.173611
people_per_vehicle = 2.2
average_speed = 5 
lanes = 4
vehicle_length = 3.5
fuel_wasted = 5000  # KG fuel wasted per day per signal [this number has been taken on the lower side - on busiest junction in Delhi this is around 12000]
fuel_density = 0.75 # Fuel Density - To Convert KG fuel to Litre
co2_emissions = 14451.25 # C02 Emissions per day per signal 

### Min and Max are Hard-coded based on 7 days of run-time [after removing the outliers]

tci_min = 200
tci_max = 900

## Hard-Coded to take the liberty of finding the best cycle time [as explained above in the assumptions]

cycle_time_min = 90
cycle_time_max = 360

df = data.frame()
df_raw = data.frame()
gt_data = data.frame()

sb_coords = read.csv("silkboard_coordinates.csv")

### Plotting Latitude and Longitude for every pixel in the image

lat_max = 12.94462
long_min = 77.59711

silk_board_lat = 12.917349
silk_board_long  = 77.622759

geomap_latitude = data.frame()
geomap_longitude = data.frame()
geomap = data.frame()


###########################
### Reading the Road data and corresponding manipulations
###########################

up_down_roads = readPNG("silk_board_roads.jpeg")
up_down_roads = up_down_roads*255

madiwala_ec_up = data.frame()
madiwala_ec_down = data.frame()
btm_hsr_down = data.frame()
btm_hsr_up = data.frame()
up_down_roads_map = data.frame()

for (i in 1:640)
{
  for (j in 1:640)
  {
    up_down_roads_map[i,j] = paste(up_down_roads[i,j,1],up_down_roads[i,j,2],up_down_roads[i,j,3],sep = ",")
  }
}
madiwala_ec_up = up_down_roads_map
madiwala_ec_down = madiwala_ec_up
btm_hsr_down = madiwala_ec_up
btm_hsr_up = madiwala_ec_up

madiwala_ec_up[madiwala_ec_up != "100,0,0"] = ""
madiwala_ec_down[madiwala_ec_down != "0,0,0"] = ""
btm_hsr_up[btm_hsr_up != "0,100,0"] = ""
btm_hsr_down[btm_hsr_down != "0,0,100"] = ""

index_madi_ec = data.frame()
index_ec_madi = data.frame()
index_hsr_btm = data.frame()
index_btm_hsr = data.frame()

for ( i in 1:640)
{
  for ( j in 1:640 )
  {
    if(up_down_roads_map[i,j] == "100,0,0")
    {
      index_madi_ec = rbind.fill(index_madi_ec,data.frame(i,j))    
    }
    if(up_down_roads_map[i,j] == "0,0,0")
    {
      index_ec_madi = rbind.fill(index_ec_madi,data.frame(i,j))    
    }
    if(up_down_roads_map[i,j] == "0,100,0")
    {
      index_btm_hsr = rbind.fill(index_btm_hsr,data.frame(i,j))    
    }
    if(up_down_roads_map[i,j] == "0,0,100")
    {
      index_hsr_btm = rbind.fill(index_hsr_btm,data.frame(i,j))    
    }
  }
}

madi_ec_x = 327
madi_ec_y = 302
ec_madi_x = 332
ec_madi_y = 300
hsr_btm_x = 329
hsr_btm_y = 304
btm_hsr_x = 327
btm_hsr_y = 295

index_madi_ec$distance = sqrt((index_madi_ec[1:nrow(index_madi_ec),1]-madi_ec_x)^2 + (index_madi_ec[1:nrow(index_madi_ec),2]-madi_ec_y)^2)
index_dist_ec_madi = sqrt((index_madi_ec[1:nrow(index_madi_ec),1]-madi_ec_x)^2 + (index_madi_ec[1:nrow(index_madi_ec),2]-madi_ec_y)^2)

index_madi_ec$distance = sqrt((index_madi_ec$i-madi_ec_x)^2 + (index_madi_ec$j-madi_ec_y)^2)
index_ec_madi$distance = sqrt((index_ec_madi$i-ec_madi_x)^2 + (index_ec_madi$j-ec_madi_y)^2)
index_hsr_btm$distance = sqrt((index_hsr_btm$i-hsr_btm_x)^2 + (index_hsr_btm$j-hsr_btm_y)^2)
index_btm_hsr$distance = sqrt((index_btm_hsr$i-btm_hsr_x)^2 + (index_btm_hsr$j-btm_hsr_y)^2)

index_ec_madi = index_ec_madi[order(index_ec_madi$distance),]
index_madi_ec = index_madi_ec[order(index_madi_ec$distance),]
index_hsr_btm = index_hsr_btm[order(index_hsr_btm$distance),]
index_btm_hsr = index_btm_hsr[order(index_btm_hsr$distance),]

###########################
#### Putting everything in one infinite loop - [ Everything below the loop is 1 time activity and need not run again]
###########################
i = 1
while (i>0)
{
  time1 = Sys.time()

  ## Getting all the 4 current wait times for the 500 metre distance - 
  
  hsr_btm_time = gmapsdistance(origin = sb_coords$origin_lat_long[sb_coords$Origin == "HSR"],destination =  sb_coords$dest_lat_long[sb_coords$Origin == "HSR"],mode = "driving",key = google_api_key)$Time
  btm_hsr_time = gmapsdistance(origin = sb_coords$origin_lat_long[sb_coords$Origin == "BTM"],destination =  sb_coords$dest_lat_long[sb_coords$Origin == "BTM"],mode = "driving",key = google_api_key)$Time
  ec_madiwala_time = gmapsdistance(origin = sb_coords$origin_lat_long[sb_coords$Origin == "EC"],destination =  sb_coords$dest_lat_long[sb_coords$Origin == "EC"],mode = "driving",key = google_api_key)$Time
  madiwala_ec_time = gmapsdistance(origin = sb_coords$origin_lat_long[sb_coords$Origin == "Madiwala"],destination =  sb_coords$dest_lat_long[sb_coords$Origin == "Madiwala"],mode = "driving",key = google_api_key)$Time
  
  time = Sys.time()
  
  ## Reading the current Bing Map Image and storing it at the specified location
  
  filename = paste("outputs/mapsbj",as.integer(Sys.time()),".jpeg",sep = "_")

  image = GetBingMap(center="silk%20%board%20flyover", zoom=14,mapArea = c(12.8979,77.6045,12.9379,77.6445),size = c(640,640), extraURL="&mapLayer=TrafficFlow",apiKey=bing_api_key,verbose=1, destfile=filename)
  image = readPNG(filename)
  image = image*255
  
  ## Processing the Image using RGBs for each of the pixels in 640x640 matrix 
  
  image_rgb = data.frame()
  
  for (i in 1:640)
  {
    for (j in 1:640)
    {
      image_rgb[i,j] = paste(image[i,j,1],image[i,j,2],image[i,j,3],sep = ",")
    }
  }
  
  ## Dividing the image into 4 roads coming from Madiwala, Electronic City, HSR and BTM correspondingly
  
  index_madi_ec = index_madi_ec[,c(1:2)]
  index_ec_madi = index_ec_madi[,c(1:2)]
  index_hsr_btm = index_hsr_btm[,c(1:2)]
  index_btm_hsr = index_btm_hsr[,c(1:2)]
  
  for ( i in 1:nrow(index_madi_ec))
  {
    index_madi_ec$rgb1[i] = image_rgb[index_madi_ec[i,1],index_madi_ec[i,2]]
  }
  
  for ( i in 1:nrow(index_ec_madi))
  {
    index_ec_madi$rgb1[i] = image_rgb[index_ec_madi[i,1],index_ec_madi[i,2]]
  }
  
  for ( i in 1:nrow(index_btm_hsr))
  {
    index_btm_hsr$rgb1[i] = image_rgb[index_btm_hsr[i,1],index_btm_hsr[i,2]]
  }
  
  for ( i in 1:nrow(index_hsr_btm))
  {
    index_hsr_btm$rgb1[i] = image_rgb[index_hsr_btm[i,1],index_hsr_btm[i,2]]
  }
  
  index_ec_madi = cbind(index_ec_madi[,c(1:2)],data.frame(do.call('rbind', strsplit(as.character(index_ec_madi$rgb1),',',fixed=TRUE))))
  colnames(index_ec_madi) = c("x","y","r","g","b") 
  
  index_madi_ec = cbind(index_madi_ec[,c(1:2)],data.frame(do.call('rbind', strsplit(as.character(index_madi_ec$rgb1),',',fixed=TRUE))))
  colnames(index_madi_ec) = c("x","y","r","g","b") 
  
  index_hsr_btm = cbind(index_hsr_btm[,c(1:2)],data.frame(do.call('rbind', strsplit(as.character(index_hsr_btm$rgb1),',',fixed=TRUE))))
  colnames(index_hsr_btm) = c("x","y","r","g","b") 
  
  index_btm_hsr = cbind(index_btm_hsr[,c(1:2)],data.frame(do.call('rbind', strsplit(as.character(index_btm_hsr$rgb1),',',fixed=TRUE))))
  colnames(index_btm_hsr) = c("x","y","r","g","b") 
  
  ## Assigning Red Flag to the roads with Heavy Traffic
  
  index_ec_madi$red_flag = ifelse(as.numeric(paste(index_ec_madi$r)) > 200 & as.numeric(paste(index_ec_madi$g)) < 120 ,1,0)
  index_madi_ec$red_flag = ifelse(as.numeric(paste(index_madi_ec$r)) > 200 & as.numeric(paste(index_madi_ec$g)) < 120 ,1,0)
  index_hsr_btm$red_flag = ifelse(as.numeric(paste(index_hsr_btm$r)) > 200 & as.numeric(paste(index_hsr_btm$g)) < 120 ,1,0)
  index_btm_hsr$red_flag = ifelse(as.numeric(paste(index_btm_hsr$r)) > 200 & as.numeric(paste(index_btm_hsr$g)) < 120 ,1,0)
  
  ## Assigning Amber Flag to the roads with Moderate Traffic
  
  index_ec_madi$amber_flag = ifelse(as.numeric(paste(index_ec_madi$r)) > 200 & as.numeric(paste(index_ec_madi$g)) > 140 & as.numeric(paste(index_ec_madi$b))<100,1,0)
  index_madi_ec$amber_flag = ifelse(as.numeric(paste(index_madi_ec$r)) > 200 & as.numeric(paste(index_madi_ec$g)) > 140 & as.numeric(paste(index_madi_ec$b))<100,1,0)
  index_hsr_btm$amber_flag = ifelse(as.numeric(paste(index_hsr_btm$r)) > 200 & as.numeric(paste(index_hsr_btm$g)) > 140 & as.numeric(paste(index_hsr_btm$b))<100,1,0)
  index_btm_hsr$amber_flag = ifelse(as.numeric(paste(index_btm_hsr$r)) > 200 & as.numeric(paste(index_btm_hsr$g)) > 140 & as.numeric(paste(index_btm_hsr$b))<100 ,1,0)
  
  ### Calculation of all 4 distances 
  
  latlon_dist <- function(origin,destination){
    xml.url <- paste0('http://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,'&mode=walking&sensor=false')
    xmlfile <- xmlParse(getURL(xml.url))
    dist <- xmlValue(xmlChildren(xpathApply(xmlfile,"//distance")[[1]])$value)
    distance <- as.numeric(sub(" km","",dist))
    return(distance)
  }

  index_hsr_btm$row_num = seq.int(nrow(index_hsr_btm))
  index_btm_hsr$row_num = seq.int(nrow(index_btm_hsr))
  index_madi_ec$row_num = seq.int(nrow(index_madi_ec))
  index_ec_madi$row_num = seq.int(nrow(index_ec_madi))
  
  ## Getting Immediate Red Pixels [Since the Image is marked Pixel by Pixel , the number of pixels will be directly proportional to the length of the corresponding road]
  
  hsr_btm_ri = ifelse(is.na(index_hsr_btm$row_num[index_hsr_btm$red_flag  == 0][1]),nrow(index_hsr_btm),index_hsr_btm$row_num[index_hsr_btm$red_flag  == 0][1]-1)
  btm_hsr_ri = ifelse(is.na(index_btm_hsr$row_num[index_btm_hsr$red_flag  == 0][1]),nrow(index_btm_hsr),index_btm_hsr$row_num[index_btm_hsr$red_flag  == 0][1]-1)
  madi_ec_ri = ifelse(is.na(index_madi_ec$row_num[index_madi_ec$red_flag  == 0][1]),nrow(index_madi_ec),index_madi_ec$row_num[index_madi_ec$red_flag  == 0][1]-1)
  ec_madi_ri = ifelse(is.na(index_ec_madi$row_num[index_ec_madi$red_flag  == 0][1]),nrow(index_ec_madi),index_ec_madi$row_num[index_ec_madi$red_flag  == 0][1]-1)
  
  ## Getting Immediate Amber Pixels for all the 4 individual roads
  
  hsr_btm_ai = ifelse(is.na(index_hsr_btm$row_num[index_hsr_btm$amber_flag  == 0][1]),nrow(index_hsr_btm),index_hsr_btm$row_num[index_hsr_btm$amber_flag  == 0][1]-1)
  btm_hsr_ai = ifelse(is.na(index_btm_hsr$row_num[index_btm_hsr$amber_flag  == 0][1]),nrow(index_btm_hsr),index_btm_hsr$row_num[index_btm_hsr$amber_flag  == 0][1]-1)
  madi_ec_ai = ifelse(is.na(index_madi_ec$row_num[index_madi_ec$amber_flag  == 0][1]),nrow(index_madi_ec),index_madi_ec$row_num[index_madi_ec$amber_flag  == 0][1]-1)
  ec_madi_ai = ifelse(is.na(index_ec_madi$row_num[index_ec_madi$amber_flag  == 0][1]),nrow(index_ec_madi),index_ec_madi$row_num[index_ec_madi$amber_flag  == 0][1]-1)
  
  ## Getting the Number of Red Pixels [ Heavy traffic ] on the entire stretch of the road
  
  hsr_btm_rw = sum(index_hsr_btm$red_flag)
  btm_hsr_rw = sum(index_btm_hsr$red_flag)
  madi_ec_rw = sum(index_madi_ec$red_flag)
  ec_madi_rw = sum(index_ec_madi$red_flag)
  
  ## Getting the number of Amber Pixels [ Moderate Traffic ] on the entire stretch of the road
  
  hsr_btm_aw = sum(index_hsr_btm$amber_flag)
  btm_hsr_aw = sum(index_btm_hsr$amber_flag)
  madi_ec_aw = sum(index_madi_ec$amber_flag)
  ec_madi_aw = sum(index_ec_madi$amber_flag)
  
  df_1 = data.frame(hsr_btm_time,btm_hsr_time,madiwala_ec_time,ec_madiwala_time,hsr_btm_ri,hsr_btm_ai,btm_hsr_ri,btm_hsr_ai,madi_ec_ri,madi_ec_ai,ec_madi_ri,ec_madi_ai,hsr_btm_rw,hsr_btm_aw,btm_hsr_rw,btm_hsr_aw,madi_ec_rw,madi_ec_aw,ec_madi_rw,ec_madi_aw,time)
  df_raw  = rbind.fill(df_raw,df_1)
  
  ## Creating the Traffic Congestion Index [ Based on the Weights mentioned in Approach ]
  
  hsr_btm_tci = constant + alpha_wt*(hsr_btm_time) + alpha_ri*(hsr_btm_ri+0.4*hsr_btm_ai) + alpha_rw*(hsr_btm_rw+0.4*hsr_btm_aw) 
  btm_hsr_tci = constant + alpha_wt*(btm_hsr_time) + alpha_ri*(btm_hsr_ri+0.4*btm_hsr_ai) + alpha_rw*(btm_hsr_rw+0.4*btm_hsr_aw) 
  madi_ec_tci = constant + alpha_wt*(madiwala_ec_time) + alpha_ri*(madi_ec_ri+0.4*madi_ec_ai) + alpha_rw*(madi_ec_rw+0.4*madi_ec_aw) 
  ec_madi_tci = constant + alpha_wt*(ec_madiwala_time) + alpha_ri*(ec_madi_ri+0.4*ec_madi_ai) + alpha_rw*(ec_madi_rw+0.4*ec_madi_aw) 
  
  tci_total = hsr_btm_tci +  btm_hsr_tci +  max(madi_ec_tci,ec_madi_tci)
  
  df = rbind.fill(df,data.frame(tci_total,hsr_btm_tci,btm_hsr_tci,madi_ec_tci,ec_madi_tci,time))
  write.csv(df_raw,"outputs/All_Variables.csv",row.names = F)
  write.csv(df,"outputs/TCI.csv")
  
  ## Calculating the individual Green times and the total cycle time for all the 4 signals
  
  hsr_btm_gt = round((hsr_btm_tci/tci_total)*(((cycle_time_max-cycle_time_min)/(tci_max-tci_min))*tci_total + 90)/5,0)*5
  hsr_btm_gt = ifelse(hsr_btm_gt < 15, 15, hsr_btm_gt)
  
  btm_hsr_gt = round((btm_hsr_tci/tci_total)*(((cycle_time_max-cycle_time_min)/(tci_max-tci_min))*tci_total + 90)/5,0)*5
  btm_hsr_gt = ifelse(btm_hsr_gt < 15, 15, btm_hsr_gt)
  
  madi_ec_gt = round((max(madi_ec_tci,ec_madi_tci)/tci_total)*(((cycle_time_max-cycle_time_min)/(tci_max-tci_min))*tci_total + 90)/5,0)*5
  madi_ec_gt = ifelse(madi_ec_gt < 15, 15, madi_ec_gt)
  
  ec_madi_gt = round((max(madi_ec_tci,ec_madi_tci)/tci_total)*(((cycle_time_max-cycle_time_min)/(tci_max-tci_min))*tci_total + 90)/5,0)*5
  ec_madi_gt = ifelse(ec_madi_gt < 15, 15, ec_madi_gt)
  
  tct = hsr_btm_gt + btm_hsr_gt + madi_ec_gt
  
  gt_data = rbind.fill(gt_data, data.frame(hsr_btm_gt,btm_hsr_gt,madi_ec_gt,ec_madi_gt,tct,time))
  write.csv(gt_data,"outputs/Tgt_data.csv")
  
  ## Writing the JSON FIles for the Front End
  
  df_2 = cbind(df_1[,c("hsr_btm_time","btm_hsr_time","madiwala_ec_time","ec_madiwala_time")],data.frame(hsr_btm_gt,btm_hsr_gt,madi_ec_gt,ec_madi_gt,tct))
  out1 = toJSON(df_2)
  write(out1,"outputs/json_1.txt")
  
  df_3 = cbind(df_raw[,c("time","hsr_btm_time","btm_hsr_time","madiwala_ec_time","ec_madiwala_time")],gt_data[,c("hsr_btm_gt","btm_hsr_gt","madi_ec_gt","ec_madi_gt")])
  df_3$hsr_btm_cst = 70
  df_3$btm_hsr_cst = 80
  df_3$madi_ec_cst = 40
  df_3$ec_madi_cst = 40
  df_3$tct = df_3$hsr_btm_gt+df_3$btm_hsr_gt+df_3$madi_ec_gt
  
  ## Calculating the Impact Metrics [ Man Hours Saved , Fuel Saved and CO2 Emissions Reduced ]
  
  df_3$man_hours_saved = (((manhours_saved*df_3$tct*average_speed)/vehicle_length)*lanes)*people_per_vehicle*manhours_saved*df_3$tct/3600
  df_3$fuel_saved = (df_3$tct*manhours_saved)*fuel_wasted/24/60/60/fuel_density
  df_3$co2_reduced = (df_3$tct*manhours_saved)*co2_emissions/24/60/60
  df_3 = toJSON(df_3)
  write(df_3,"outputs/json_time_series_trends.txt")
  
  ## Keeping the system idle before the next cycle - 
    ## This will be dynamic as the cycle time is dynamic [deltime here is the code run time ]
        # (During our runs on local laptops this was always found to be less than 60 seconds - Way less than min cycle time)
  
  cat("Total Cycle Time :",gt_data$tct,"\n Green Times : \n","HSR Green Time : ",gt_data$hsr_btm_gt[nrow(gt_data)],"\n BTM Green Time : ",gt_data$btm_hsr_gt[nrow(gt_data)],"\n Madiwala & Bommanahalli Green Time : ",gt_data$madi_ec_gt[nrow(gt_data)])
  
  time2 = Sys.time()
  deltime = difftime(time2,time1,units = "secs") ## This is the code run time in seconds
  Sys.sleep(tct-deltime)
   
}
