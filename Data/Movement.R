#Packages
require(sf)
require(raster)
require(rgdal)
require(terra)
require(gdistance)
require(tigris)
require(tidyterra)
require(dplyr)

#Reading in Neely Henry boundary and fish locations
Neely = st_read("NeelySHP_NAD_1983_2011.shp")
Control = st_read("ControlFish_3_4_24.shp")
Dispersal = st_read("DispersalFish_3_4_24.shp")
NeelyRastHalfM = rast("NeelyHalfMReclassed.tif")#Raster created in arcgispro, grid cells are 0.5m X 0.5m
plot(NeelyRastHalfM)#looks right

#Reclassifying zero values to be NAs
NeelyRastHalfM = NeelyRastHalfM[NeelyRastHalfM == 0] = NA
plot(NeelyRastHalfM)

#Map of study area
studyArea = st_bbox(Neely)
studyArea = st_as_sfc(studyArea)

#State of Alabama
alabama = tigris::states() %>% 
  filter(NAME=='Alabama')
plot(st_geometry(alabama))
st_crs(alabama)$proj4string
alabama = st_transform(alabama, st_crs(studyArea))

#Plot of where Neely is located in AL
plot(st_geometry(alabama))
plot(studyArea, col="red", add=T)

#Plot of Neely within bounding box
plot(studyArea)
plot(st_geometry(Neely),col="blue",add=T)


#Boat ramp locations
CL = st_read("CL.shp")
SS = st_read("SS.shp")
RL = st_read("RL.shp")
CC = st_read("CC.shp")

#Separating
CL_Fish = Dispersal[Dispersal$Ramp == "CL",]
SS_Fish = Dispersal[Dispersal$Ramp == "SS",]
RL_Fish = Dispersal[Dispersal$Ramp == "RL",]
CC_Fish = Dispersal[Dispersal$Ramp == "CC",]

#Checking to make sure they show up in the right place
plot(st_geometry(Neely))
plot(Dispersal, add=T)
plot(Control, add=T)

#### Getting distances between dispersal locations and boat launch release point ####

  #First need to convert Neely shapefile into "cost" raster
  NeelyVec=vect(Neely)
  NeelyR=rast(NeelyVec, res=0.0001)
  NeelyRast=rasterize(NeelyVec,NeelyR)#value of 1 is Neely, value of NA is land
  plot(NeelyRast)#looks correct
  
  # Coosa landing
    #Giving the CL launch a cost of 2 (unique value in the raster) 
    CL.launch.loc = terra::cellFromXY(NeelyRast, sf::st_coordinates(CL))#Selects Coosa landing boat launch
    terra::values(NeelyRast)[CL.launch.loc] = 2 # Gives Coosa landing a unique raster cell value of 2
    all.dists = terra::gridDist(NeelyRast,target=2) # Calculates distances between all raster cells and our unique cell
  
    
    CL_distances = numeric(nrow(CL_Fish))#Container for CL point data
    tail(CL_Fish)
    #Looping each individual location
    for (i in 1:nrow(CL_Fish)){
      CL.loc = terra::cellFromXY(NeelyRast,sf::st_coordinates(CL_Fish[i,]))
      CL_distances[i] = values(all.dists)[CL.loc]
    }
    
    CL_dist__ramp_final = cbind(CL_distances,CL_Fish)
    
#############################################SOME NAs here?????, raster not fine enough?###############################################################    
#Would like to use a transition layer to do true least cost but couldn't get the transition function to run 
#so found this secondary way to do what I am trying to do. 
      
  # Rainbow landing
    RL.launch.loc = terra::cellFromXY(NeelyRast, sf::st_coordinates(RL))#Selects Coosa landing boat launch
    terra::values(NeelyRast)[RL.launch.loc] = 3 # Gives Coosa landing a unique raster cell value of 2
    all.dists = terra::gridDist(NeelyRast,target=3) # Calculates distances between all raster cells and our unique cell
    plot(all.dists)
    
    RL_distances = numeric(nrow(RL_Fish))#Container for CL point data
    tail(RL_Fish)
    #Looping each individual location
    for (i in 1:nrow(RL_Fish)){
      RL.loc = terra::cellFromXY(NeelyRast,sf::st_coordinates(RL_Fish[i,]))
      RL_distances[i] = values(all.dists)[RL.loc]
    }
    
    RL_dist__ramp_final = cbind(RL_distances,RL_Fish)
    
  # Southside
    
    SS.launch.loc = terra::cellFromXY(NeelyRast, sf::st_coordinates(SS))#Selects SS boat launch
    terra::values(NeelyRast)[SS.launch.loc] = 4 # Gives Coosa landing a unique raster cell value of 2
    all.dists = terra::gridDist(NeelyRast,target=4) # Calculates distances between all raster cells and our unique cell
    plot(all.dists)
    
    SS_distances = numeric(nrow(SS_Fish))#Container for CL point data
    tail(RL_Fish)
    #Looping each individual location
    for (i in 1:nrow(SS_Fish)){
      SS.loc = terra::cellFromXY(NeelyRast,sf::st_coordinates(SS_Fish[i,]))
      SS_distances[i] = values(all.dists)[SS.loc]
    }
    
    SS_dist_ramp_final = cbind(SS_distances,SS_Fish)
    

  # Canoe Creek

    CC.launch.loc = terra::cellFromXY(NeelyRast, sf::st_coordinates(CC))#Selects SS boat launch
    terra::values(NeelyRast)[CC.launch.loc] = 5 # Gives Coosa landing a unique raster cell value of 2
    all.dists = terra::gridDist(NeelyRast,target=5) # Calculates distances between all raster cells and our unique cell
    plot(all.dists)
    
    CC_distances = numeric(nrow(CC_Fish))#Container for CL point data
    #Looping each individual location
    for (i in 1:nrow(CC_Fish)){
      CC.loc = terra::cellFromXY(NeelyRast,sf::st_coordinates(CC_Fish[i,]))
      CC_distances[i] = values(all.dists)[CC.loc]
    }
    
   CC_dist_ramp_final = cbind(CC_distances,CC_Fish)
    
#Distances are bigger than what I calculated in arcgispro? Too big of grid size????? import from arcgispro? 
   
#### Movement between successive time-steps for dispersal ####
DispersalFilt = Dispersal %>% group_by(RewardTag1) %>% filter(n()>1)#Filtering to only fish that have more than 1 observation
   
      
tag_numbers = unique(DispersalFilt$RewardTag1)#unique tag numbers
n_fish = length(tag_numbers)#number of fish total
output=numeric()


#Outside loop that selects tag-number 
for(i in 1:n_fish){
  
  new_tag = tag_numbers[i]
  new_dat = subset(DispersalFilt,RewardTag1 == new_tag)
  tmax = nrow(new_dat)
  DispersalDist = numeric(tmax)
  
  
    for(t in 2:tmax){
  
      past_loc = terra::cellFromXY(NeelyRast, sf::st_coordinates(new_dat[t-1,]))#Defining what cell of past location is in
      target_cell_value = terra::values(NeelyRast)[past_loc] = t+new_tag #Setting unique cell value to past_location (which will change ever time), may need to change.
      all_dists = terra::gridDist(NeelyRast,target=target_cell_value)#Getting all distances from our given target cell
      current_loc = terra::cellFromXY(NeelyRast, sf::st_coordinates(new_dat[t,]))#Defining what cell current location is in
      DispersalDist[t] = values(all_dists)[current_loc]#this needs reworked
    }
  output = append(output,DispersalDist)
}
output

#Need to make it that if # of obs is <=1 then skip


  
#### Movement between successive time-steps for control #### 
    
    