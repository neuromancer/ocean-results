import csv
import sys

csv.field_size_limit(sys.maxint)
reader = csv.reader(open(sys.argv[1]), delimiter='\t')
col = int(sys.argv[2])

for row in reader:
  event_sizes = len(row[col].split("  "))-1
  res = row[-1]
  print event_sizes, res


