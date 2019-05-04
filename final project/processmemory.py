import csv
import re
import string
import datetime
import pandas as pd
import numpy as np
import math

# File locations
inputpath = '/Users/raider/Dropbox/backup/grad school/Lab work/Eye-Tracking Causal/Results Memory/'
outputpath = '/Users/raider/Dropbox/backup/grad school/Lab work/Eye-Tracking Causal/Results Memory/'
condinput = '/Users/raider/Dropbox/backup/grad school/Lab work/Eye-Tracking Causal/eye tracker files/ExpBuilderStimUpload.csv'

fileprefix = 'CausalMem'
filesuffix = '.csv'
testfilepath = outputpath + 'CausalMem_py.csv'
Demofilepath = outputpath + 'CausalMemDemo_py.csv'

#Columns of interest
StartDateCol = 0
EndDateCol = 1
ProgressCol = 4
DurationCol = 5
ListCol = 17
IDCol = 18
ExName = 19
Debriefing = range(260, 279) 
Race = range(278,284)
InferenceCol = range(20,140)
InferenceStart = 20
DetailCol = range(140,260)
DetailStart = 140
Cond = 2
DetailLoc = 5
# Start the output file:
outfile = open(testfilepath, 'w')
outfile2 = open(Demofilepath, 'w')
outfile2.write('StartMonth,StartDay,StartTime,EndMonth,EndDay,EndTime,Progress,DurationSec,List,ID')
outfile2.write(',ReadingStrat,ReadingStratTxt,MemoryStrat,MemoryStratTxt,PurposeTxt,OtherCmntsTxt,Age,Gender,SAT,SATTxt,ACT,ACTtxt,OtherTest,Lang1,LangChild,Country,LangSchool,Ethnicity,Race')
outfile.write('ID,InferAns,InferJOL,LogInferJOL,IJolCalibrAns,QuestNum,LogQuestNum,DetAns,DetJOL,LogDetJOL,DJolCalibrAns,OrderofClauses,ConnectLoc,DetailLoc')

# Open the Qualtrics file and begin reading
csvfile = open(fileprefix+filesuffix, 'r')
csvreader = csv.reader(csvfile)
next(csvreader)
next(csvreader)
next(csvreader)

#open the conditions file
condfile = open(condinput, 'r')
condreader = csv.reader(condfile)
CondList = []
DetLocList = []
#get conditions and detail location
for line in condreader:
	CondList.append(str(line[Cond]).strip('\"'))
	DetLocList.append(str(line[DetailLoc]).strip('\"'))
CondList = [x for x in CondList if x != 'F']
CondList = [x for x in CondList if x != '$Cond']
DetLocList = [x for x in DetLocList if x != 'empty']
DetLocList = [x for x in DetLocList if x != '$DetailPlace']
DetLocList = [x for x in DetLocList if x != 'note: this is from a story with two details']
#get cond and detail by list
List1 = CondList[0:60] + DetLocList[0:60]
List2 = CondList[60:120] + DetLocList[0:60]
List3 = CondList[120:180] + DetLocList[0:60]
List4 = CondList[180:240] + DetLocList[0:60]
List5 = CondList[240:300] + DetLocList[0:60]
List6 = CondList[300:360] + DetLocList[0:60]


# Handle each subject (= 1 line)

for line in csvreader:
	# calculate the start & end time
	try:
		starttime = datetime.datetime.strptime(line[StartDateCol], '%Y-%m-%d %H:%M:%S')
		endtime = datetime.datetime.strptime(line[EndDateCol], '%Y-%m-%d %H:%M:%S')
		startday = starttime.day
		startmonth = starttime.month
		starttime = str(starttime.hour+2) + ":" + str(starttime.minute)  # was in Central time
		endday = endtime.day
		endmonth = endtime.month
		endtime = str(endtime.hour+2) + ":" + str(endtime.minute)
	except ValueError:
		# couldn't process the start or end time
		starttime = ''
		endtime = ''
		startday = ''
		startmonth = ''
		starttime = ''
		endday = ''
		endmonth = ''
		endtime = ''		

	I=InferenceStart #Inference Question
	IJOL=I+1 #Inference JOL
	D = DetailStart #Detail Question
	DJOL = D + 1 #Detail JOL
	C = 1 #Counter for questions #
	e = 0  #element in list
	d = 60
	
	#do participant level data
	if line[IDCol].isdigit():
		#fill demographics file
		outfile2.write('\n')
		# print the subject data
		if line[ListCol] == '4' and line[IDCol]=='17':
			outfile2.write(','.join([str(startmonth),str(startday),starttime,str(endmonth),str(endday),endtime,str(line[ProgressCol]),str(line[DurationCol]),str('5'),line[IDCol]]))	
		else:
			outfile2.write(','.join([str(startmonth),str(startday),starttime,str(endmonth),str(endday),endtime,str(line[ProgressCol]),str(line[DurationCol]),line[ListCol],line[IDCol]]))	
		for Data in Debriefing[:]:
			outfile2.write(',' + str(line[Data]).replace(',',' '))
		for Data in Race[:]:
			outfile2.write(str(line[Data]).replace(',',' ').replace('','.'))	
			
	# do the multiple-choice questions and JOLs		
		for answer in range(0,60):
			outfile.write('\n')
			outfile.write(str(line[IDCol]))
		# print results
			if line[I] == "":
				outfile.write(',' + str("NA"))
			else:
				outfile.write(',' + str(line[I]).replace('2','0')) #InfAns
			if line[IJOL] == "":
				outfile.write(',' + str("NA"))
				outfile.write(',' + str("NA"))
				outfile.write(',' + str("NA"))
			else:
				outfile.write(',' + str(line[IJOL])) #InfJOL
				outfile.write(',' + str(math.log10(int(line[IJOL]))))
				if line[I] == str("1"):
					outfile.write(',' + str(line[IJOL])) #take into account whether or not they're guessing
				else:
					outfile.write(',' + str(100 - int(line[IJOL]))) #confidence of getting it right
			outfile.write(',' + str(C)) # id for question
			outfile.write(',' + str(math.log10(C)))
			if line[D] == "":
				outfile.write(',' + str("NA"))
			else:
				outfile.write(',' + str(line[D]).replace('2','0')) #DetailAns
			if line[DJOL] == "":
				outfile.write(',' + str("NA"))
				outfile.write(',' + str("NA"))
				outfile.write(',' + str("NA"))
			else:
				outfile.write(',' + str(line[DJOL])) #DetailJOL
				outfile.write(',' + str(math.log10(int(line[DJOL]))))
				if line[D] == str("1"):
					outfile.write(',' + str(line[DJOL])) #take into account whether or not they're guessing
				else:
					outfile.write(',' + str(100 - int(line[DJOL]))) #confidence of getting it right
			#Add condition
			if line[ListCol] == '1':
				outfile.write(',' + List1[e].replace('.',','))
				outfile.write(',' + List1[d])
			elif line[ListCol] == '2':
				outfile.write(',' + List2[e].replace('.',','))
				outfile.write(',' + List2[d])
			elif line[ListCol] == '3':
				outfile.write(',' + List3[e].replace('.',','))
				outfile.write(',' + List3[d])
			#fix an error in list coding
			elif line[ListCol] == '4' and line[IDCol]=='17':
				outfile.write(',' + List5[e].replace('.',','))
				outfile.write(',' + List5[d])
			elif line[ListCol] == '4':
				outfile.write(',' + List4[e].replace('.',','))
				outfile.write(',' + List4[d])
			elif line[ListCol] == '5':
				outfile.write(',' + List5[e].replace('.',','))
				outfile.write(',' + List5[d])
			elif line[ListCol] == '6':
				outfile.write(',' + List6[e].replace('.',','))
				outfile.write(',' + List6[d])
			else:
				pass
			if line[DetailLoc] == "":
				outfile.write(',' + str("NA"))
			
			I+=2
			IJOL+=2
			D+=2
			DJOL+=2
			e+=1
			C+=1
			d+=1

	else:
		pass
csvfile.close()
outfile.close()
outfile2.close()
print('Done!')
#check for even numbers of conditions in lists
BigList = List1 + List2 + List3 + List4 + List5 + List6
counts = {}
for item in BigList:
	counts[item] = counts.get(item, 0)+1
print(counts)
#open the conditions file
checkfile = open(testfilepath, 'r')
checkfilereader = csv.reader(checkfile)
CheckList = []
countscheck = {}
#get conditions and detail location
for line in checkfilereader:
	CheckList.append(str(line[0]))
	CheckList.append(str(line[5]))
	CheckList.append(str(line[11]))
	CheckList.append(str(line[12]))
	CheckList.append(str(line[13]))
for item in CheckList:
	countscheck[item] = countscheck.get(item, 0)+1
