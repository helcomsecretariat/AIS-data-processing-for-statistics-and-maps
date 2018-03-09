#-------------------------------------------------------------
# Name:             Track Builder From CSV
# Purpose:	    Creates lines features from the monthly csv files
# Author:           Andzej Milos
# Created:	    2017
# Copyright:        GNU General Public License V3
# ArcGIS Version:   10.2
# Python Version:   2.7
#-------------------------------------------------------------

import os
import csv
import arcpy
import multiprocessing
from datetime import datetime

# Change input values according to the data you want to process
year = "2011"
inFolder = r"E:/DensityMaps_V3/" + year + "/01_trips"
outFolder = r"E:/DensityMaps_V3/" + year + "/02_lines"

# Check if file name contains "tracks", name of month and year
def getMonthFromFileName(filename):
	monthsList = ['january', 'february', 'march', 'april', 'may', 'june', 'july', 'august', 'september', 'october', 'november', 'december']
	result = False
	for month in monthsList:
		#if filename.find(month) > -1:
		if ((filename.find(month) > -1) and (filename.find(year) > -1) and (filename.find("tracks") > -1)):
			result = month
			break
	return result

def readCSV(filename):
	month = getMonthFromFileName(filename)
	if not month:
		print "Cannot process file " + filename
	else:
		print "--- Started processing " + month + " at: " + str(datetime.now()) + "\n"
			
		#attrnames = ["imo", "trip_id", "lat", "long"]
		inCSV = open(inFolder + "\\" + filename,  'r')
		csvreader = csv.reader(inCSV, delimiter = ';')
		
		headers = csvreader.next()
		#print str(headers)
		attrnameindexes = {"imo": headers.index("imo"), "trip_id": headers.index("trip_id"), "new_trip_id": headers.index("new_trip_id"), "lat": headers.index("lat"), "long": headers.index("long") }
		
		i = 0
		point = arcpy.Point()
		pointsarray = arcpy.Array()
		
		
		firstrow = csvreader.next()
		#print str(firstrow)
		trip_id = firstrow[attrnameindexes["trip_id"]]
		new_trip_id = firstrow[attrnameindexes["new_trip_id"]]
		imo = firstrow[attrnameindexes["imo"]]
		point.X = float(firstrow[attrnameindexes["long"]])
		point.Y = float(firstrow[attrnameindexes["lat"]])
		pointsarray.add(point)
		
		#filenameparts = filename.split(".")[0].split("_")
		#outputfilename = "ais_"+filenameparts[2]+"_"+filenameparts[1]+"_lines.shp"
		outputfilename = "lines_" + year + "_" + month + ".shp"
		outputfilepath = outFolder + "\\" + outputfilename
		
		arcpy.CreateFeatureclass_management(outFolder, outputfilename, "POLYLINE", None, None, None, arcpy.SpatialReference(4326))
		arcpy.AddField_management(outputfilepath, "trip_id", "LONG")
		arcpy.AddField_management(outputfilepath, "new_tr_id", "LONG")
		arcpy.AddField_management(outputfilepath, "imo", "LONG")
		
		insertcursor = arcpy.da.InsertCursor(outputfilepath, ["imo", "trip_id", "new_tr_id", "SHAPE@"])
		
		for row in csvreader:
			
			if i % 50000 == 0:
				print "processing row " + str(i) + " " + month
				#print str(row)
			#if i > 500000:
			#	print str(row)
			
			#print "imo: " + str(row[attrnameindexes["imo"]]) + ", new_tr_id: " + str(row[attrnameindexes["new_trip_id"]])
			#print str(row) + " " + month
			if new_trip_id == row[attrnameindexes["new_trip_id"]]:
				point.X = float(row[attrnameindexes["long"]])
				point.Y = float(row[attrnameindexes["lat"]])
				pointsarray.add(point)
			else:
				polyline = arcpy.Polyline(pointsarray)
				try:
					insertcursor.insertRow([imo, trip_id, new_trip_id, polyline])
				except:
					print "error insertcursor.insertRow " + str(row) + " " + month
					break
				pointsarray.removeAll()
				trip_id = row[attrnameindexes["trip_id"]]
				new_trip_id = row[attrnameindexes["new_trip_id"]]
				imo = row[attrnameindexes["imo"]]
				point.X = float(row[attrnameindexes["long"]])
				point.Y = float(row[attrnameindexes["lat"]])
				pointsarray.add(point)
			i += 1
			
		polyline = arcpy.Polyline(pointsarray)
		insertcursor.insertRow([imo, trip_id, new_trip_id, polyline])
		pointsarray.removeAll()
				
		del insertcursor
		print "--- Ended processing " + month + " at: " + str(datetime.now()) + "\n"
	
if __name__ == '__main__':
	startTime_script = datetime.now()
	print "Started Track Builder at: " + str(startTime_script)
	filenames = []
	for filename in os.listdir(inFolder):
		if filename.endswith(".csv"):
			filenames.append(filename)
			
	pool = multiprocessing.Pool(6)
	pool.map(readCSV, filenames)
	pool.close()
	pool.join()
	print "Ended Track Builder at: " + str(datetime.now())
	print "Duration: " + str(datetime.now() - startTime_script) + "\n"
