import csv
import sys

csv.field_size_limit(sys.maxint)
reader = csv.reader(open(sys.argv[1]), delimiter='\t')
col = int(sys.argv[2])
#minsize = int(sys.argv[3])+1

for row in reader:
  size = len(row[col].split("  ")) 
  #if size >= minsize:
  field = "\t"+str(size)
  print "\t".join(row)+field
  #print "\t".join(row[0:col])+"\t"+rep+("\t".join(row[col+1:]))


