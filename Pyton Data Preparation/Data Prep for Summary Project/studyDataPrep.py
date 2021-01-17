import xlrd
import pandas as pd
from collections import OrderedDict
import simplejson as json
import re;
import datetime as dt
fmt= '%m/%d/%y'


# Open the workbook and select the first worksheet
wb = xlrd.open_workbook('924_truncated.xlsx')
sh = wb.sheet_by_index(0)
# List to hold dictionaries
cases_list = []
# Iterate through each row in worksheet and fetch values into dict
cases_list = []
for rownum in range(1, sh.nrows):
    cases = OrderedDict()
    row_values = sh.row_values(rownum)
    cases['CaseNo'] = row_values[0]
    cases['ReportType'] = row_values[1]
    cases['Age'] =  row_values[2]
    cases['Gender'] = row_values[3]
    cases['History'] = row_values[4]
    cases['Reporter'] = row_values[5]
    # cases['Narrative'] = row_values[6].replace('\r', ' ').replace('\n', ' ')
    cases['AdrDate'] =  pd.to_datetime(row_values[7], errors='coerce').date() #row_values[7]
    cases['Adrs'] = row_values[8]
    cases['SeriousOutcome'] = row_values[9]
    cases['Outcome'] =row_values[10]
    cases['PS'] = row_values[11]
    cases['Concomitants'] = row_values[12]
    cases['OnsetE'] = row_values[13]
    cases['Dechal'] = row_values[14]
    cases['Rechal'] = row_values[15]
    cases['DrugDate'] = pd.to_datetime(row_values[16], errors='coerce').date()
    cases['Length'] = int(row_values[17])
    cases['Treatment'] = row_values[18]
    cases['DrugChanged'] = row_values[19]
    cases['NewDrug'] = row_values[20]
    cases['EventOutcome'] = row_values[21]
    cases['Id'] = int(row_values[22])
    cases['Indication'] = row_values[23]
    cases_list.append(cases)

# print(cases_list)

############# to see distribution of narrative length
narrative_len_count = []
for case in cases_list:
    # print (case, len(case["Narrative"]))
    if len(case["Narrative"]) > 200 and len(case["Narrative"]) <= 3500:
        narrative_len_count.append(len(case["Narrative"]))

# print(narrative_len_count, len(narrative_len_count))


####### For plotting the length of narrative
import matplotlib
matplotlib.use('TkAgg')
import pandas as pd
import matplotlib.pyplot as plt;
import numpy as np
plt.rcdefaults()


narrative_len_count.sort();
N = len(narrative_len_count)
x = range(N)
plt.bar(x, narrative_len_count)

####### Selecting the right sample of narrative length ####### 
median_narrative = np.median(narrative_len_count)
mean_narrative = np.mean(narrative_len_count)

std_narrative = np.std(narrative_len_count)
print ("median",median_narrative)
print ("std", std_narrative)
print ("mean", np.mean(narrative_len_count))
print("min-max", min(narrative_len_count), max(narrative_len_count))


q75, q25 = np.percentile(narrative_len_count, [75 ,25])
iqr = q75 - q25

# ********  For asymmetric distribution with outliers
print ("range", median_narrative - 0.25* iqr ,  median_narrative + 0.25 * iqr)


# ********  For symmetric distribution with outliers
print ("Symm range", mean_narrative - 0.25* std_narrative ,  mean_narrative + 0.25 * std_narrative)


sym_range =[]
for item in narrative_len_count:
        if item in range (1296, 1704):
            sym_range.append(item)

print "Range", len(sym_range)



# ********** To check the summary statistics of the narrative length ********
df = pd.DataFrame (narrative_len_count)
# print df[1:10]
print "skew", df.skew()
print  "kurt", df.kurt()
print "desc", df.describe()


# ********** To see the most frequent elements of a list ********
print "mode,", max(set(narrative_len_count), key=narrative_len_count.count)

matplotlib.style.use('ggplot')
df.hist( alpha=0.5, figsize=(16, 10))

plt.bar(x, narrative_len_count, align='center', alpha=0.5)
plt.ylabel("Narrative Length -- # of Characters in Narrative")
plt.xlabel("Report#")
plt.show()


################ Data Cleaning ##########
def unique_list(l):
    ulist = []
    [ulist.append(x) for x in l if x not in ulist]
    return ulist


#####import list of otc products
f = open('drugs.txt', 'r')
drugs_list = re.findall(r"\S+", f.read())

# print (drugs_list)


## fixing new lines
for i in drugs_list:
    i.replace('\\n','');
    i = i.strip("\n")

# print (len(cases_list))
# print (drugs_list)
# print (cases_list[1]["products"])


## selecting cases with few products
unfamiliar_PS =[]
for case in cases_list:
    if case["PS"].count(":") < 1:
        if case["PS"].title() not in drugs_list:
            unfamiliar_PS.append(case)

# print ('Carisoprodol' in drugs_list)
print ("unfamiliar",len(unfamiliar_PS))

### selecting narratives with specific length
proper_length_cases =[];
for case in unfamiliar_PS:
    if len(case["Narrative"]) > (median_narrative - std_narrative)  and  len(case["Narrative"])  < (median_narrative + std_narrative):
        proper_length_cases.append(case)


print ("proper",len(proper_length_cases))

# # selecting cases with full information
info_cases =[];
for case in proper_length_cases:
    # print (len(case["Narrative"]))
    if case["Age"] != "" and case["Gender"] != "" and case["Conc"] != "" and case["Indication"] != "" and case["PS"] != "":# and case['Outcome'] !='':
        info_cases.append(case)

print ("info", len(info_cases))

####### selecting cases with few products
less_data =[]
for case in info_cases:
    if case["PTS"].count(":") <= 10 and case["Conc"].count(":") < 5 and case["PS"].count(":") < 3:
        less_data.append(case)
print ("count",len(less_data))


# ******** Removing duplicate values in a cell/field **************
for case in less_data:
    for key, value in case.iteritems():
        if key=="Indication" or key=="PS" or key=="Adrs":
            if ':' in str(value) or ';' in str(value):
                value = str(value).replace(';',':')
                value =':'.join(unique_list(str(value).split(':')))
                case[key] = value
        if key=="Dechal" or key=="Rechal":
            if value != 'Y' and  value != 'N':
                case[key] = '';

        ##### Calculating Onset
        if key=="DrugDate":
            case['Onset'] = case['AdrDate'] - value;
            case['Onset'] = str(case['Onset']).replace(', 0:00:00', '')
            if pd.isnull(case['AdrDate']):
               case['AdrDate'] = ""
            else:
               case['AdrDate'] =  case['AdrDate'].strftime("%b-%d-%Y")

            if pd.isnull(case['DrugDate']):
               case['DrugDate'] = ""
            else:
               case['DrugDate'] =  case['DrugDate'].strftime("%b-%d-%Y")


# ******** Adding missing fields as number to the data **************
missing =0;
for case in less_data:
    for key, value in case.iteritems():
        if value=='' or value ==' ':
            missing = missing+1;
    case['Missing'] = missing;
    missing =0;
    # print case['Missing']



############ labeling data as complete/incomplete for triageStudy ############
inCom=0;com=0
study1Cases = []
for case in less_data:
    if case["Length"] in range (2500, 3000) or case["Length"] in range (200, 1100) :
        if case["Missing"] > 11:
            case["Status"] = "Incomplete"
            inCom=inCom+1;
            study1Cases.append(case)
        elif case["Missing"] in range(4, 7):
            case["Status"] =  "Complete"
            com=com+1;
            study1Cases.append(case)

print "I & C", inCom, com

missing=[];
equalSet=[];
for case in study1Cases:
    if case['Status'] == 'Incomplete':
         missing.append(case['Missing'])
    else:
        equalSet.append(case)

target = len(equalSet)
# print list(set(missing)), len(missing)
# print len(study1Cases), target

uniqueMissingNo  = list(set(missing))
it = 0;


# /********** Making both conditions equal **********//
for case in study1Cases:
    if case['Missing'] in uniqueMissingNo:
        equalSet.append(case)
        it = it+1;
    if it == target:
        break;

print len(equalSet)


# /********** output **********//
j = json.dumps(equalSet)
with open('triageStudy.json', 'w') as f:
    f.write(j)

