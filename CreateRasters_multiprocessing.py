#-------------------------------------------------------------
# Name:             Create rasters in multiprocessing
# Purpose:			Creates a raster file for each ship type category
# Authors:          Manuel Frias, Andzej Milos
# Copyright:        GNU General Public License V3
# ArcGIS Version:   10.2
# Python Version:   2.7
#-------------------------------------------------------------

import arcpy
import time
from arcpy.sa import *
import os
import multiprocessing
from datetime import datetime

year = "2010"
shipType = "TANKER"
grids_folder = r"E:\DensityMaps_V2\grid"
grid = r"E:\DensityMaps_V2\grid\Grid1km_BalticSea.shp"
#clippingArea = r"E:\DensityMaps\DensityMaps1Km_GRIDSandTables.gdb\BalticScope_StudyArea"

in_folder = r"E:\DensityMaps_V2\2013\03_lines_by_shiptype" + "\\" + shipType
out_folder = r"E:\DensityMaps_V2\2013\04_rasters"

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
	worktempfolder = out_folder + "\\" + shipType + "\\temp_" + month
	if not arcpy.Exists(worktempfolder):
		arcpy.CreateFolder_management(out_folder + "\\" + shipType, "temp_" + month)

	for i in range(1, 6):
		print "--- Spatial join " + month + " gridDivision " + str(i) + "..."

		gridDivision = grids_folder + "\\grid_division" + str(i) + ".shp"

		arcpy.MakeFeatureLayer_management(gridDivision, "gridDivision_lyr_" + month)
		arcpy.MakeFeatureLayer_management(grid, "grid_lyr_" + month + "_" + str(i))
		arcpy.MakeFeatureLayer_management(data, "line_lyr")

		arcpy.SelectLayerByLocation_management("grid_lyr_" + month + "_" + str(i), "WITHIN", "gridDivision_lyr_" + month)
		arcpy.MakeFeatureLayer_management("grid_lyr_" + month + "_" + str(i), "grid_lyr_" + month)


		#result = arcpy.GetCount_management("line_lyr")
		#count = int(result.getOutput(0))
		#print " lines total count: " + str(count) + " " + month

		# Select lines in grid division
		arcpy.SelectLayerByLocation_management("line_lyr", "INTERSECT", "gridDivision_lyr_" + month, "", "NEW_SELECTION")

		#if count > 0:
		# Select grids intersecting lines
		arcpy.SelectLayerByLocation_management("grid_lyr_" + month, "INTERSECT", "line_lyr", "", "NEW_SELECTION")

		# Spatial join selected lines and grids
		arcpy.SpatialJoin_analysis("grid_lyr_" + month, "line_lyr", worktempfolder + "\\" + month + "_SpJoin_" + str(i) + ".shp", "JOIN_ONE_TO_MANY", "", "", "INTERSECT")

		arcpy.Delete_management("gridDivision_lyr_" + month)
		arcpy.Delete_management("grid_lyr_" + month + "_" + str(i))
		arcpy.Delete_management("grid_lyr_" + month)
		arcpy.Delete_management("line_lyr")

		print "--- End spatial join: " + month + " gridDivision " + str(i) + "..."

	spjoinList = []
	for spjoin in os.listdir(worktempfolder):
		if spjoin.endswith(".shp"):
			spjoinList.append(worktempfolder + "\\" + spjoin)

	if len(spjoinList) > 0:
		# Merge Spatial Joins
		print "--- Merge " + month + "..."
		arcpy.Merge_management(spjoinList, worktempfolder + "\\" + month + "_Merged.shp")
		print "--- End merge " + month + "..."

		# Dissolve merged
		print "--- Dissolve " + month + "..."
		arcpy.Dissolve_management(worktempfolder + "\\" + month + "_Merged.shp", worktempfolder + "\\" + month + "_Dissolve.shp", "TARGET_FID", [["Join_Count", "SUM"]])
		print "--- End dissolve " + month + "..."

		# Make raster out of dissolved
		print "--- FeatureToRaster " + month + "..."
		arcpy.FeatureToRaster_conversion(worktempfolder + "\\" + month + "_Dissolve.shp","SUM_Join_C", out_folder + "\\" + shipType + "\\" + month + "_" + year + "_" + shipType + "_Raster" + ".tif", 1000)
		print "--- End FeatureToRaster " + month + "..."

		arcpy.Delete_management(worktempfolder)
	print "End processing " + month + " at: " + str(datetime.now()) + "\n"

if __name__ == '__main__':
	startTime_script = datetime.now()
	print "Started at: " + str(startTime_script) + "\n"

	linesfiles = []
	for filename in os.listdir(in_folder):
		if filename.endswith(".shp"):
			linesfiles.append(in_folder + "\\" + filename)

	if len(linesfiles) == 0:
		print "WARNING: No files for the analysis..."
	else:
		starTime_task = datetime.now()
		shipTypeFolder = out_folder + "\\" + shipType
		if not arcpy.Exists(shipTypeFolder):
			arcpy.CreateFolder_management(out_folder, shipType)


		pool = multiprocessing.Pool(6)
		pool.map(multiProcessing_function, linesfiles)
		pool.close()
		pool.join()

	print "Ended at: " + str(datetime.now()) + ".\nDuration: " + str(datetime.now() - startTime_script) + "\n"

		#Join all rasters into one
		#rasterList = arcpy.ListRasters("*_Raster.tif")
		#arcpy.CheckOutExtension("Spatial")

		#print "Joining and summing up values of all months into one..."
		#sumRasters = arcpy.sa.CellStatistics(rasterList, "SUM", "NODATA")
		#sumRasters.save(os.path.join(env, yearRaster))
		#print "Yearly raster file done!\n"

		#Clip Baltic Scope area
		#print "Clipping Baltic Scope area and creating new raster..."
		#areaClip = arcpy.Clip_management(yearRaster, "", yearRaster+"_Scope.tif", clippingArea, "", "ClippingGeometry")
		#print "Baltic Scope clipping done!\n"



		#Delete IMO files to save space in disk
		#print "Deleting IMO files in "+env+" to save space..."
		#for IMOFiles in checkIMOFiles:
		#	print(clearWSLocks(env))
		#	arcpy.Delete_management(IMOFiles)

		#Print end time
		#print "Script ended! Time now: " +str(datetime.now())
		#print "Finished in "+str(datetime.now() - startTime)

