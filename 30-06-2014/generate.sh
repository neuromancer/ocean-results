for i in $(seq 1 10); do
  ./generate_datasets.py data.csv.gz vulnerable_programs.csv.gz
  Rscript --no-restore --no-save ../Rscripts/exp_120.R 64 weightTf
  #Rscript --no-restore --no-save ../Rscripts/exp_120.R 256 weightTf
  #Rscript --no-restore --no-save ../Rscripts/exp_120.R 512 weightTf
  #Rscript --no-restore --no-save ../Rscripts/exp_120.R 1024 weightTf
done
