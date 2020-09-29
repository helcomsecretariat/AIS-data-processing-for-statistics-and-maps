#-------------------------------------------------------------
# Name:             Split tracks by ship type
# Purpose:
# Authors:          Andzej Milos
# Created:			2017
# Copyright:        (c) HELCOM Secretariat
# ArcGIS Version:   10.2
# Python Version:   2.7
#-------------------------------------------------------------

import arcpy
import time
from arcpy.sa import *
import os
import multiprocessing
from datetime import datetime

year = "2019"
# CARGO CONTAINER PASSENGER TANKER ROROCARGO SERVICE FISHING OTHER UNKNOWN
shipType = "ROROCARGO"
grids_folder = r"E:/DensityMaps_V3/grid"
grid = r"E:/DensityMaps_V3/grid/BSII_grid.shp"
#clippingArea = r"E:\DensityMaps\DensityMaps1Km_GRIDSandTables.gdb\BalticScope_StudyArea"

in_folder = r"E:/DensityMaps_V3/" + year + "/03_lines_by_shiptype/" + shipType
out_folder = r"E:/DensityMaps_V3/" + year + "/04_rasters"

def getMonthFromFileName(filename):
	monthsList = ['january', 'february', 'march', 'april', 'may', 'june', 'july', 'august', 'september', 'october', 'november', 'december']
	result = False
	for month in monthsList:
		if ((filename.find(month) > -1) and (filename.find(year) > -1) and (filename.find(shipType) > -1)):
			result = month
			break
	return result

def multiProcessing_function(data):
	month = getMonthFromFileName(data)
	print "Start processing " + month + " at: " + str(datetime.now()) + "\n"
	#worktempfolder = out_folder + "\\" + shipType + "\\temp_" + month
	worktempfolder = out_folder + "\\DensityMaps1Km_" + year + "_IMO_" + shipType + "\\temp_" + month
	if not arcpy.Exists(worktempfolder):
		#arcpy.CreateFolder_management(out_folder + "\\" + shipType, "temp_" + month)
		arcpy.CreateFolder_management(out_folder + "\\DensityMaps1Km_" + year + "_IMO_" + shipType, "temp_" + month)
	
	grid_inverse_sel_lyrs = []
	for i in range(1, 6):
		print "--- Started Spatial join " + month + " gridDivision " + str(i) + " at: " + str(datetime.now()) + "\n"
	
		# Choose next grid division
		gridDivision = grids_folder + "\\grid_division" + str(i) + ".shp"
		
		arcpy.MakeFeatureLayer_management(gridDivision, "gridDivision_lyr_" + month)
		arcpy.MakeFeatureLayer_management(grid, "grid_lyr_" + month + "_" + str(i))
		arcpy.MakeFeatureLayer_management(data, "line_lyr")
		
		# Select grids in grid division
		arcpy.SelectLayerByLocation_management("grid_lyr_" + month + "_" + str(i), "WITHIN", "gridDivision_lyr_" + month)
		arcpy.MakeFeatureLayer_management("grid_lyr_" + month + "_" + str(i), "grid_lyr_" + month)
		
		# Select lines in grid division
		arcpy.SelectLayerByLocation_management("line_lyr", "INTERSECT", "gridDivision_lyr_" + month, "", "NEW_SELECTION")
		arcpy.MakeFeatureLayer_management("line_lyr", "selected_line_lyr")
		
		# Select grids intersecting lines
		arcpy.SelectLayerByLocation_management("grid_lyr_" + month, "INTERSECT", "line_lyr", "", "NEW_SELECTION")
		arcpy.MakeFeatureLayer_management("grid_lyr_" + month, "selected_grid_lyr_" + month)
				
		# Spatial join selected lines and grids
		arcpy.SpatialJoin_analysis("selected_grid_lyr_" + month, "selected_line_lyr", worktempfolder + "\\" + month + "_SpJoin_" + str(i) + ".shp", "JOIN_ONE_TO_MANY", "", "", "INTERSECT")
		
		# Select grids not intersecting lines and store them for later raster creation. 1 grid layer per grid division
		arcpy.SelectLayerByAttribute_management("grid_lyr_" + month, "SWITCH_SELECTION")
		arcpy.MakeFeatureLayer_management("grid_lyr_" + month, "grid_inverse_sel_lyr_" + month + "_" + str(i))
		grid_inverse_sel_lyrs.append("grid_inverse_sel_lyr_" + month + "_" + str(i))
				
		arcpy.Delete_management("gridDivision_lyr_" + month)
		arcpy.Delete_management("grid_lyr_" + month + "_" + str(i))
		arcpy.Delete_management("grid_lyr_" + month)
		arcpy.Delete_management("selected_grid_lyr_" + month)
		arcpy.Delete_management("line_lyr")
		arcpy.Delete_management("selected_line_lyr")
		
		print "--- Ended Spatial join " + month + " gridDivision " + str(i) + " at: " + str(datetime.now()) + "\n"
	
	# Store spatial join from 2 to 5 
	spjoinList = []
	for i in range(2, 6):
		if arcpy.Exists(worktempfolder + "\\" + month + "_SpJoin_" + str(i) + ".shp"):
			spjoinList.append(worktempfolder + "\\" + month + "_SpJoin_" + str(i) + ".shp")
	
	if len(spjoinList) > 0:
		# Append 2 - 5 spatial joins to 1
		print "+++ Started spjoin Append " + month + " at: " + str(datetime.now()) + "\n"
		arcpy.Append_management(spjoinList, worktempfolder + "\\" + month + "_SpJoin_1.shp", "TEST","","")
		print "+++ Ended spjoin Append " + month + " at: " + str(datetime.now()) + "\n"
				
		# Dissolve spatial join 1
		print "+++ Started dissolve " + month + " at: " + str(datetime.now()) + "\n"
		arcpy.Dissolve_management(worktempfolder + "\\" + month + "_SpJoin_1.shp", worktempfolder + "\\" + month + "_Dissolve.shp", "TARGET_FID", [["Join_Count", "SUM"]])
		print "+++ Ended dissolve " + month + " at: " + str(datetime.now()) + "\n"

		# Create shp of 1 grid division grids not intersecting lines layer
		print "+++ Started CopyFeatures " + month + " at: " + str(datetime.now()) + "\n"
		arcpy.CopyFeatures_management(grid_inverse_sel_lyrs[0], worktempfolder + "\\" + month + "_Inv_Merged.shp")
		del grid_inverse_sel_lyrs[0]
		print "+++ Ended CopyFeatures " + month + " at: " + str(datetime.now()) + "\n"

		# Append rest grids not intersecting lines layers to first division layer
		print "+++ Started inverse Append " + month + " at: " + str(datetime.now()) + "\n"
		arcpy.Append_management(grid_inverse_sel_lyrs, worktempfolder + "\\" + month + "_Inv_Merged.shp", "TEST","","")
		print "+++ Ended inverse Append " + month + " at: " + str(datetime.now()) + "\n"
		
		# Merge Dissolve result and grids not intersecting lines
		print "+++ Started Merge dissolve + inverse " + month + " at: " + str(datetime.now()) + "\n"
		arcpy.Merge_management([worktempfolder + "\\" + month + "_Dissolve.shp", worktempfolder + "\\" + month + "_Inv_Merged.shp"], worktempfolder + "\\" + month + "_ForRaster.shp")
		print "+++ Ended Merge dissolve + inverse " + month + " at: " + str(datetime.now()) + "\n"
		
		# Create raster out of merged grids
		print "+++ Started FeatureToRaster " + month + " at: " + str(datetime.now()) + "\n"
		#arcpy.FeatureToRaster_conversion(worktempfolder + "\\" + month + "_ForRaster.shp","SUM_Join_C", out_folder + "\\" + shipType + "\\" + shipType + "_" + year + "_" + month + "_Raster" + ".tif", 1000)
		arcpy.FeatureToRaster_conversion(worktempfolder + "\\" + month + "_ForRaster.shp","SUM_Join_C", out_folder + "\\DensityMaps1Km_" + year + "_IMO_" + shipType + "\\" + shipType + "_" + year + "_" + month + "_Raster" + ".tif", 1000)
		print "+++ Ended FeatureToRaster " + month + " at: " + str(datetime.now()) + "\n"
				
		arcpy.Delete_management(worktempfolder)
	print "End processing " + month + " at: " + str(datetime.now()) + "\n"

if __name__ == '__main__':
	startTime_script = datetime.now()
	print "Started script for year " + year + " " + shipType + " at: " + str(startTime_script)
	print "Started create months rasters at: " + str(datetime.now()) + "\n"

	linesfiles = []
	for filename in os.listdir(in_folder):
		if filename.endswith(".shp"):
			linesfiles.append(in_folder + "\\" + filename)
	
	if len(linesfiles) == 0:
		print "WARNING: No files for the analysis..."
	else:
		starTime_task = datetime.now()
		#shipTypeFolder = out_folder + "\\" + shipType
		shipTypeFolder = out_folder + "\\DensityMaps1Km_" + year + "_IMO_" + shipType
		if not arcpy.Exists(shipTypeFolder):
			#arcpy.CreateFolder_management(out_folder, shipType)
			arcpy.CreateFolder_management(out_folder, "DensityMaps1Km_" + year + "_IMO_" + shipType)
		
		pool = multiprocessing.Pool(6)
		pool.map(multiProcessing_function, linesfiles)
		pool.close()
		pool.join()
	
	print "Ended create months rasters at: " + str(datetime.now())
		
	#Join all months rasters into one year raster
	print "Started create year " + year + " " + shipType + " raster at: " + str(datetime.now()) + "\n"
	if arcpy.CheckExtension("Spatial") == "Available":
		arcpy.CheckOutExtension("Spatial")
		#arcpy.env.workspace = out_folder + "\\" + shipType
		arcpy.env.workspace = out_folder + "\\DensityMaps1Km_" + year + "_IMO_" + shipType
		rasterList = arcpy.ListRasters("*_Raster.tif")
		sumRasters = arcpy.sa.CellStatistics(rasterList, "SUM", "NODATA")
		#sumRasters.save(out_folder + "\\" + shipType + "\\" + shipType + "_" + year + "_Year_Raster" + ".tif")
		sumRasters.save(out_folder + "\\DensityMaps1Km_" + year + "_IMO_" + shipType + "\\" + shipType + "_" + year + "_Year_Raster" + ".tif")
		arcpy.CheckInExtension("Spatial")
	else:
		print("Spatial Analyst license is unavailable")
	print "Ended create year " + year + " " + shipType + " raster at: " + str(datetime.now())
	print "Ended script for year " + year + " " + shipType + " at: " + str(datetime.now())
	print "Duration: " + str(datetime.now() - startTime_script) + "\n"