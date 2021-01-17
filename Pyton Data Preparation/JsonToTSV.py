import json
import csv
import ast

#### input data is in the form [{a:3, b:4},{a:6, b:22},{a:7,b:6}]
json_file = open("set1.json", "r")
json_data = json.load(json_file)
json_file.close()

data = json.dumps(json_data, indent=4)
# print(data)

tsv_file = open("data.tsv", "w")
tsv_writer = csv.writer(tsv_file, delimiter='\t')  #### change delimeter for other files


# convert string (data is string) to a dictionary object
my_dict = ast.literal_eval(data)

# write the header
tsv_writer.writerow(my_dict[0].keys())
for row in my_dict:
    # write data rows
    tsv_writer.writerow(row.values())

tsv_file.close()