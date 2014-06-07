library("e1071")
library("caret")
library("tm")

options(mc.cores=1)

dir = "../25-05-2014"

options(stringsAsFactors=F)

mycon = gzcon(gzfile(paste(dir, "filtered_traces.csv.gz", sep="/"), open="r"))
buggy_program_events = read.csv(textConnection(readLines(mycon)), sep="\t", header = F)


x = factor(c(buggy_program_events[,4]), levels = c("R","B"))
buggy_program_events = buggy_program_events[!is.na(x),]

cats = factor(c(buggy_program_events[,4]), levels = c("R","B"))
programs = buggy_program_events[,1]
uniq_programs = levels(factor(programs))
sizes = buggy_program_events[,3]

#print(nrow(buggy_program_events))
#print(sizes)

mut_corpus = Corpus(VectorSource(c(buggy_program_events[,2])))
print(mut_corpus)

mut_dm = DocumentTermMatrix(mut_corpus)

sink("/dev/null")

mut_dm_df =  as.data.frame(inspect(mut_dm))
mut_dm_df["program"] = programs
mut_dm_df["size"] = sizes
mut_dm_df["class"] = cats

sink()

msizes = rev(c(1, 50, 100, 150, 200, 250, 300))
res = c()

mut_robust_cases = mut_dm_df[mut_dm_df$class == "R",]
mut_buggy_cases  = mut_dm_df[mut_dm_df$class == "B",]

for (n in msizes){
  
  print(c("n:",n))

  mut_robust_cases = mut_robust_cases[mut_robust_cases$size >= n,]
  mut_buggy_cases = mut_buggy_cases[mut_buggy_cases$size >= n,]
  
  robust_cases = mut_robust_cases[,names(mut_robust_cases) != "size"]
  buggy_cases  = mut_buggy_cases[,names(mut_buggy_cases) != "size"]

  n = length(uniq_programs)
  psample = sample(n)
  train_programs = uniq_programs[psample[1:as.integer(n*0.75)]]
  test_programs = uniq_programs[psample[(as.integer(n*0.75)+1):n]]

  buggy_train = buggy_cases[buggy_cases$program %in% train_programs,names(buggy_cases) != "program"]
  buggy_test  = buggy_cases[buggy_cases$program %in% test_programs,names(buggy_cases) != "program"]

  robust_train = robust_cases[robust_cases$program %in% train_programs,names(robust_cases) != "program"]
  robust_test  = robust_cases[robust_cases$program %in% test_programs,names(robust_cases) != "program"]

  train_size = min(nrow(buggy_train), nrow(robust_train))
  test_size = min(nrow(buggy_test), nrow(robust_test))

  print(train_size)
  print(test_size)

  #print(levels(factor(robust_cases$program)))
  #print(levels(factor(buggy_cases$program)))
  #aaaa


  #train_size = min(500,as.integer(nrow(buggy_cases)*0.75))
  #test_size = min(250,nrow(buggy_cases) - train_size)

  #print(train_size)
  #print(test_size)

  #n = nrow(buggy_cases)
  #rsample = 1:n
  #rsample = sample(n)

  #train_sample = rsample[1:(train_size)] 
  #test_sample = rsample[(train_size+1):(train_size+test_size)]

  #print(rsample)

  #buggy_train = buggy_cases[train_sample,]
  #buggy_test  = buggy_cases[test_sample,]

  #print(nrow(buggy_train))
  #print(nrow(buggy_test))

  # robust train and test

  #n = nrow(robust_cases)
  #rsample = 1:n
  #rsample = sample(n)

  #print(rsample)

  # n cases are selected to keep the train dataset balanced
  #train_sample = rsample[1:(train_size)]
  #test_sample =  rsample[(train_size+1):(train_size+test_size)]
  #more_test_sample = rsample[(train_size+test_size+1):n]

  #robust_train = robust_cases[train_sample,]
  #robust_test  = robust_cases[test_sample,]
  #robust_more_test = robust_cases[more_test_sample,]

  #print(nrow(robust_train))
  #print(nrow(robust_test))
  
  train = rbind(buggy_train, robust_train)
  test  = rbind(buggy_test, robust_test)

  varnot0 = names(train)[unlist(lapply(train,function(x) 0 != var(x)))]

  xy_train = train[,varnot0]
  xy_test  = test[,varnot0]

  x_test = test[,names(test) != "class"]
  y_test  = test[,"class"]

  tcont = tune.control(sampling='fix')
  #print(names(xy_train))
  m = tune.svm(class~., data = xy_train, validation.x = x_test, validation.y = y_test, gamma = 10^(-5:-1), cost = 10^(-1:3), tunecontrol=tcont)
  
  model = m$best.model
  z = predict(model,x_test)
  print(summary(m))
  res = c(res,confusionMatrix(table(pred=z, true=y_test))$overall[1])

}

print(data.frame(n=msizes, acc=res))
