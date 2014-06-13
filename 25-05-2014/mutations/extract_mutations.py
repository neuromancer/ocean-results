import csv
import sys

csv.field_size_limit(sys.maxint)
reader = csv.reader(open(sys.argv[1]), delimiter='\t')
typ = sys.argv[2]
#minsize = int(sys.argv[3])+1

for row in reader:
  mutations = row[1].split(" ")
  res = row[-1]

  if ("type="+typ) in mutations:
    for mutation in mutations:
      print mutation.split("=")[1],
    print res
    #print ""

  #assert(0)
  #size = len(row[col].split("  ")) 
  #if size >= minsize:
  #rep = str(size)+"\t"
  #print "\t".join(row[0:col])+"\t"+rep+("\t".join(row[col+1:]))


