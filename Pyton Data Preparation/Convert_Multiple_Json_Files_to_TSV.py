import json
import csv
import ast
import os


###### Getting all json files paths
path = r'expfactory/'
filesPath=[]

# r=root, d=directories, f = files
for r, d, f in os.walk(path):
    for myFile in f:
        if myFile.endswith(".json"):
            filesPath.append(os.path.join(r, myFile))

# print(filesPath)

#### input data is in the form [{a:3, b:4},{a:6, b:22},{a:7,b:6}]

for myFile in filesPath:
    json_file = open(myFile, "r")
    json_data = json.load(json_file)
    json_file.close()
    # data.append(json.dumps(json_data, indent=4))
    data = json.dumps(json_data, indent=4)
    # tsv_file = open(f"{myFile}.tsv", "w")

    tsv_file = open(myFile+".tsv", "w")
    tsv_writer = csv.writer(tsv_file, delimiter='\t')
    # print (tsv_file)

    my_dict = ast.literal_eval(data)
    myDictValues = my_dict['data']

    ##### Convert string object to dictionary object
    jsonObject = json.loads(myDictValues)


    ######## Json Objects had missing keys (columns), we need all the columns to avoid irregular cell allocation
    columns = list({column for row in jsonObject for column in row.keys()})
    tsv_writer.writerow(columns)

    for row in jsonObject:
        tsv_writer.writerow([None if column not in row else row[column] for column in columns])
    tsv_file.close()









