
library(ncdf4)
library(raster)
library(rgdal)
library(exactextractr)
library(data.table)
library(sf)
library(dplyr)
gc()

#---------- SETTINGS ------------------------------------------------------------------------------------------
#- Set path to input ncdf file
path <- c("F:/NetCDFs_CCI/Volta/2018/2018_Chla_Volta/")
list <- list.files(path, pattern="*.nc$", full.names=TRUE,recursive=TRUE) #- list all files in the directory with extension .nc

path_out <- c("F:/NetCDFs_CCI/out/Volta_outs/Volta_Chla_2018_out/")


#- Parameters of interest
# In the param list you find names for parameters
# while in the qual list you find names for quality layers, these names are slightly different
# there MUST BE correspondence to avoid associating wrong quality level to a given parameter 
param <- c(#"water_surface_height_above_reference_datum",
           #"lake_surface_water_extent",
           #"lake_surface_water_temperature",
           #"lake_ice_cover_class",# old name  lake_ice_cover,
           #"turbidity_mean" 
           "chla_mean")

qual <- c(#"water_surface_height_uncertainty",
          #"lake_surface_water_extent_uncertainty",
          #"lswt_quality_level",
          #"lake_ice_cover_uncertainty", # old name  lake_ice_cover,
          #"turbidity_uncertainty"
          "chla_uncertainty")

#- REad input shapefile with lakes extent (if avaialble)
v1    <- readOGR('F:/NetCDFs_CCI/out/shps/Volta_shp/Lake_Volta.shp')  #- path to file on the disk
v1    <- st_as_sf(v1) #- To simple feature

#---------- END SETTINGS ------------------------------------------------------------------------------------------

print('Start Processing...')

#- Extract dates from filename
name  <- unlist(lapply(strsplit(as.character(list), "[-]"), '[[', 6))
dates <- as.Date(name, format='%Y%m%d')  #- Convert to dates (this is not used later)

print('Start extracting subset over Lake Baikal')

t1 <- Sys.time()

for (nm in seq_along(param)) {
  
  #- Stack for output raster files (create one raster stack for each parameter)
  clim_output_st         <- stack()
  clim_output_quality_st <- stack()
  
  print(paste0('Now processing parameter: ', param[nm]))
  
  for (i in seq_along(list)){  #- This loop runs over all dates fior the selected parmeter param[nm]
    
    print(paste0('Now processing file ', 'number ', i, ' ',  'out of ', length(list)))
    clim_output          <- brick(list[i], varname = param[nm]) #- Extract variable of interest for each file in the list
    clim_output_quality  <- brick(list[i], varname = qual[nm])  #- Extract quality of the variable of interest 
    
    #- Crop over Lake Baikal extent
    clim_output         <- crop(clim_output, v1)               
    clim_output_quality <- crop(clim_output_quality, v1)       
    
    #- Assign name to raster
    names(clim_output)         <- paste0(param[nm], "_", name[i])
    names(clim_output_quality) <- paste0(qual[nm], "_", name[i])
    
    #- Stack over previous raster files/dates
    clim_output_st         <- stack(clim_output_st, clim_output)
    clim_output_quality_st <- stack(clim_output_quality_st, clim_output_quality)
    
  } 
  
  filename_out_param <- paste0(path_out, as.character(name[1]), "_", as.character(name[length(name)]), "_", param[nm]) 
  filename_out_qual  <- paste0(path_out, as.character(name[1]), "_", as.character(name[length(name)]), "_", qual[nm]) 
  
  writeRaster(brick(clim_output_st), filename_out_param, format="GTiff", overwrite=TRUE)        #- Write output stack raster for the given parameter
  writeRaster(brick(clim_output_quality_st), filename_out_qual, format="GTiff", overwrite=TRUE) #- Write output raster for the given quality parameter
  
}

writeRaster(brick(clim_output_quality_st), filename_out_qual, format="GTiff", overwrite=TRUE) #- Write output raster for the given quality parameter
if (file.exists(paste0(filename_out_qual, ".tif"))) print("Good")


#- Time for computing
print(paste0('Stop creating stack raster - Computing time:  ', as.character(Sys.time()-t1)))

print('End Processing...')
