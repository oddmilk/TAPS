'''

TAPS Graphical Analysis
@Author: Oddmilk
Data Source: Ricardo Godoy @ Brandeis University
Time: Jan.19.2017

'''

import pandas as pd 
import numpy as np 

raw = pd.read_excel("TAPS.xlsx")
i_vars = pd.read_excel("TAPS_Variables.xlsx", sheetname = 'individual')
i_vars = list(i_vars.Variables)
i_data = raw[i_vars]

h_vars = pd.read_excel("TAPS_Variables.xlsx", sheetname = 'household')
h_vars = list(h_vars.Variables)
h_data = raw[h_vars]

v_vars = pd.read_excel("TAPS_Variables.xlsx", sheetname = 'village')
v_vars = list(v_vars.Variables)
v_data = raw[v_vars]

ihv_data = [i_data, h_data, v_data]
ihv_data_names = ['i_data', 'h_data', 'v_data']

from pandas import ExcelWriter

for i in range(len(ihv_data)):
	writer = pd.ExcelWriter(ihv_data_names[i]+'.xlsx')
	ihv_data[i].to_excel(writer, 'sheet1')
	writer.save()

# Import Publication Overview 
pub = pd.read_excel("Publication_Overview.xlsx")
# Text analysis with publication names 
library(nltk)
corpus = pub.Name 


#########################
########## Round 1 ######
# stunt & growth ######
#########################
# Variables in use #

# Time
	# yearY234567890: Year of survey, 2002-2010, inclusive
	# idssnY234567890: Unique subject id #; remains fixed across years;note

# Demographic
	# idmaleY234567890: Subject's sex: 1=male; 0=female 
	# idage_TAPS_Y234567: Best estimate of subject's age in whole years made by TAPS team

# Growth determinants
	# ianstatureY234567890: Stature (cm)

r1 = i_data[['yearY234567890','idssnY234567890','idmaleY234567890','idage_TAPS_Y234567','ianstatureY234567890']]
r1.to_csv('round_1.csv')


################
# dinner table #
################

# Variables of interest
# Time
	# yearY234567890
	# hhidY234567890
# Village
	# vawalkacY234567890
# Household harvest
	# hagcornstockY4567890
	# hagricestockY4567890
	# hagcornsaleY24567890
	# hagricesaleY24567890


raw = pd.read_excel('TAPS.xlsx')
dinner_table = ['yearY234567890','vidY234567890','vawalkacY234567890','hhidY234567890',
'hagcassstockY234567890','hagcornstockY4567890','hagricestockY4567890',
'hagcorntarY4567890','hagricetarY4567890',
'hcnoodleY234567890','hcsardinesY234567890','hcpigeonpeaY234567890','hcplantainY234567890','hcriceY234567890','hagpigeonparY567']
dinner = raw[dinner_table].drop_duplicates()
dinner.to_csv('dinner.csv', index = False)



