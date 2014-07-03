#! /usr/bin/python2.7
import sys
import csv
import gzip
import random

csv.field_size_limit(sys.maxsize)

norm_programs = dict()
vuln_programs = dict()

tmp = []

data_filename = sys.argv[1]
vuln_filename = sys.argv[2]

buggy_train_filename = "buggy_train.csv.gz"
robust_train_filename = "robust_train.csv.gz"

buggy_test_filename = "buggy_test.csv.gz"
robust_test_filename = "robust_test.csv.gz"

# first, we read the list of vulnerable programs
with gzip.open(vuln_filename, 'rb') as csvfile1:
    reader1 = csv.reader(csvfile1, delimiter='\t')

    for row in reader1:
      vuln_programs[row[0]] = 1 

# we read the list of all programs
with gzip.open(data_filename, 'rb') as csvfile1:
    reader1 = csv.reader(csvfile1, delimiter='\t')
    
    for row in reader1:
      if (len(row) <> 3):
        continue
      if not (row[0] in vuln_programs):
        norm_programs[row[0]] = 1

train_prop = 0.75
#test_prop  = 1.0 - train_prop

vuln_size = len(vuln_programs)
norm_size = len(norm_programs)

vuln_sample = random.sample(vuln_programs.keys(), vuln_size)
norm_sample = random.sample(norm_programs.keys(), norm_size)

train_vuln_programs = vuln_sample[:int(train_prop*vuln_size)]
test_vuln_programs  = vuln_sample[int(train_prop*vuln_size):] 

train_norm_programs = norm_sample[:int(train_prop*norm_size)]
test_norm_programs  = norm_sample[int(train_prop*norm_size):] 

#print train_vuln_programs
#print train_norm_programs

# train data
# we read the list of all programs and traces
with gzip.open(data_filename, 'rb') as csvfile1:
  with gzip.open(buggy_train_filename, 'wb') as csvfile2:
    with gzip.open(robust_train_filename, 'wb') as csvfile3:
      reader1 = csv.reader(csvfile1, delimiter='\t')

      for row in reader1:
        if (len(row) <> 3):
          continue

        if (row[0] in train_vuln_programs):
          csvfile2.write("\t".join(row)+"\n")
       
        elif (row[0] in train_norm_programs):
          csvfile3.write("\t".join(row)+"\n")


# test data

count = 3
tmp = []
program_count = dict() 

with gzip.open(data_filename, 'rb') as csvfile1:
  with gzip.open(buggy_test_filename, 'wb') as csvfile2:
    with gzip.open(robust_test_filename, 'wb') as csvfile3:
      reader1 = csv.reader(csvfile1, delimiter='\t')

      for row in reader1:
        if (len(row) <> 3):
          continue
          #assert(0)

        if (row[0] in test_vuln_programs):
          f = csvfile2
        elif (row[0] in test_norm_programs):
          f = csvfile3
        else:
          continue
     
        if row[0] in program_count:
          n = program_count[row[0]]
          if n < count:
            tmp.append("\t".join(row))
            program_count[row[0]] = n + 1
          if n == count - 1:
            if not ("vulnerable" in tmp[0] or "vulnerable" in tmp[2] or "vulnerable" in tmp[1]): # FIXME: count should be 3
              for line in tmp:
                f.write(line+"\n")
            tmp = []
        else:
          tmp.append("\t".join(row))
          program_count[row[0]] = 1

 
#print(vuln_sample)
#print(norm_sample)


#print programs.keys()
#print len(programs)

assert(0)

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
 

