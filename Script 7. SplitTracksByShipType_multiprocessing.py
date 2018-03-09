#-------------------------------------------------------------
# Name:             Split tracks by ship type
# Purpose:	    Split lines features into different ship type categories
# Authors:          Manuel Frias, Andzej Milos
# Created:	    2017
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

spatialRef = arcpy.SpatialReference(3035)
year = "2011"
shipTypes = ["CARGO", "CONTAINER", "FISHING", "OTHER", "PASSENGER", "SERVICE", "TANKER", "UNKNOWN", "ROROCARGO"]
shipList = r"E:/DensityMaps_V3/ship_list_dbf/imo/" + year + ".dbf"
fields = ["HELCOM_Gro"]

in_folder = r"E:/DensityMaps_V3/" + year + "/02_lines"
out_folder = r"E:/DensityMaps_V3/" + year + "/03_lines_by_shiptype"

def getMonthFromFileName(filename):
	monthsList = ['january', 'february', 'march', 'april', 'may', 'june', 'july', 'august', 'september', 'october', 'november', 'december']
	result = False
	for month in monthsList:
		if ((filename.find(month) > -1) and (filename.find(year) > -1) and (filename.find("lines") > -1)):
			result = month
			break
	return result

def checkDefineProjection(data_, spatialRef_):
	data_desc = arcpy.Describe(data_)
	if data_desc.spatialReference.name != spatialRef_.name:
		print "--- Projecting " + data_desc.file + " in " + spatialRef_.name + "..."
		arcpy.Project_management(data_, in_folder + "\\pr_" + data_desc.file, spatialRef_)
		arcpy.Delete_management(data_)
		arcpy.Rename_management(in_folder + "\\pr_" + data_desc.file, in_folder + "\\" + data_desc.file)
		
def checkAddFields(data_, fields_):
	data_desc = arcpy.Describe(data_)
	data_fieldList = arcpy.ListFields(data_)
	data_fieldListNames = [field.name for field in data_fieldList]
	fieldsToAdd = [field for field in fields_ if field not in data_fieldListNames]
				
	if ((len(fieldsToAdd) > 0) and ("imo" in data_fieldListNames)):
		print "--- Adding fields " + str(fieldsToAdd) + " " + data_desc.file + "..."
		month = getMonthFromFileName(data_desc.file)
		arcpy.MakeTableView_management(shipList, "shipList_view_" + month)
		try:
			arcpy.JoinField_management(data_, "imo", "shipList_view_" + month, "imo", fieldsToAdd)
		except:
			print "error processing join " + "shipList_view_" + month
		arcpy.Delete_management("shipList_view_" + month)
		
def splitDataByType(data_, shipTypes_):
	data_desc = arcpy.Describe(data_)
	print "--- Extracting ship types out of " + data_desc.file + "..."
	for shipType in shipTypes_:
		shipTypeFolder = out_folder + "\\" + shipType
		if not arcpy.Exists(shipTypeFolder):
			arcpy.CreateFolder_management(out_folder, shipType)
		shipTypeData = shipTypeFolder + "\\" + shipType + "_" + data_desc.file
		if not arcpy.Exists(shipTypeData):
			query = "HELCOM_Gro = '" + shipType.title() + "'"
			arcpy.FeatureClassToFeatureClass_conversion(data_, shipTypeFolder, shipType + "_" + data_desc.file, query)
		else:
			print "--- WARNING: " + shipType + " " + getMonthFromFileName(data_desc.file) + " " + year + " already exists..."
			
def multiProcessing_function(data):
	month = getMonthFromFileName(data)
	if not month:
		print "Cannot process file " + data
	else:
		print "Start processing " + month + " at: " + str(datetime.now()) + "\n"
		checkDefineProjection(data, spatialRef)
		checkAddFields(data, fields)
		splitDataByType(data, shipTypes)
		print "End processing " + month + " at: " + str(datetime.now()) + "\n"
	
if __name__ == '__main__':
	startTime_script = datetime.now()
	print "Started at: " + str(startTime_script) + "\n"
	
	#monthsList = ['january', 'february', 'march', 'april', 'may', 'june', 'july', 'august', 'september', 'october', 'november', 'december']
	linesfiles = []
	for filename in os.listdir(in_folder):
		if filename.endswith(".shp"): 
			#m = getMonthFromFileName(filename)
			#print "+++ " + str(m) + " " + filename
			if getMonthFromFileName(filename):
				linesfiles.append(in_folder + "\\" + filename)
			else:
				print "WARNING: " + in_folder + "\\" + filename + " is not included in analysis...\n"
			
	if len(linesfiles) == 0:
		print "WARNING: No files for the analysis..."
	else:
		pool = multiprocessing.Pool(6)
		pool.map(multiProcessing_function, linesfiles)
		pool.close()
		pool.join()
		
	print "Ended at: " + str(datetime.now()) + ".\nDuration: " + str(datetime.now() - startTime_script) + "\n"
		
		
