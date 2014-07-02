#! /usr/bin/python2.7
import sys
import csv

csv.field_size_limit(sys.maxsize)

programs = dict()
tmp = []

with open(sys.argv[1], 'rb') as csvfile1:
    reader1 = csv.reader(csvfile1, delimiter='\t')
  
    for row in reader1:
      if (len(row) <> 3):
        continue
        #assert(0)

      assert(not (" " in row[0]))
      if row[0] in programs:
        n = programs[row[0]]
        if n < 3:
          tmp.append("\t".join(row))
          programs[row[0]] = n + 1
        
        if n == 2:
          if not ("vulnerable" in tmp[0] or "vulnerable" in tmp[2] or "vulnerable" in tmp[1]):
            for line in tmp:
              print line
          tmp = []
      else:
        tmp.append("\t".join(row))
        programs[row[0]] = 1
 

