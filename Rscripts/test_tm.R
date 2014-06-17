options(warn=-1)

msg.trap <- capture.output( suppressMessages( library("e1071") ))
msg.trap <- capture.output( suppressMessages( library("tm") ))
msg.trap <- capture.output( suppressMessages( library("gbm") ))
#library("RWeka")
options(mc.cores=10)
#options(mc.cores=1)

confusion <- function(a, b){ # instead of caret
  tbl <- table(pred=a, true=b)
  mis <- 1 - sum(diag(tbl))/sum(tbl)
  list(table = tbl, misclass.prob = mis)
  }


# Argument handling

wfs = data.frame(1)

wfs$weightBin   = weightBin
wfs$weightTf    = weightTf
wfs$weightTfIdf = weightTfIdf

args <- commandArgs(trailingOnly = TRUE)
msize = abs(as.numeric(args[1]))
wf = wfs[,args[2]]

dir = "../25-05-2014"

options(stringsAsFactors=F)

if (! ("mycon" %in% ls())) {
  mycon = gzcon(gzfile(paste(dir, "sized_buggy_traces.csv.gz", sep="/"), open="r"))  
  buggy_program_events = read.csv(textConnection(readLines(mycon)), sep="\t", header = F)

  x = factor(c(buggy_program_events[,5]), levels = c("R","B"))
  buggy_program_events = buggy_program_events[!is.na(x),]

}

#quit()

programs = buggy_program_events[,1]
mutations = buggy_program_events[,2]
events = buggy_program_events[,3]
sizes = buggy_program_events[,4]
cats = factor(c(buggy_program_events[,5]), levels = c("R","B"))

uniq_programs = levels(factor(programs))

#print(nrow(buggy_program_events))
#print(sizes)

mut_corpus = Corpus(VectorSource(c(mutations)))
evs_corpus = Corpus(VectorSource(c(events)))

#print(mut_corpus)
#print(evs_corpus)

mut_dm = DocumentTermMatrix(mut_corpus)

sink("/dev/null")

mut_dm_df =  as.data.frame(inspect(mut_dm))
mut_dm_df["class"] = cats
mut_vars = names(mut_dm_df) #it includes "class"!

mut_dm_df["program"] = programs
mut_dm_df["size"] = sizes

sink()

#BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 5, delimiters="  "))}
evs_dm = DocumentTermMatrix(evs_corpus, control = list(bounds = list(global = c(1,Inf)), weighting = wf))

sink("/dev/null")

evs_dm_df =  as.data.frame(inspect(evs_dm))

#evs_vars = names(sort(colSums(evs_dm_df), decreasing=T)[1:3000])
#evs_dm_df = evs_dm_df[,evs_vars]

evs_dm_df["class"] = cats
evs_vars = names(evs_dm_df) # unused

sink()

#print(evs_dm)
#print(names(evs_dm_df))

mut_robust_cases = mut_dm_df[mut_dm_df$class == "R",]
mut_buggy_cases  = mut_dm_df[mut_dm_df$class == "B",]

evs_robust_cases = evs_dm_df[evs_dm_df$class == "R",]
evs_buggy_cases  = evs_dm_df[evs_dm_df$class == "B",]

aug_robust_cases = cbind(mut_robust_cases[,names(mut_robust_cases) != "class"], evs_robust_cases) 
aug_buggy_cases  = cbind(mut_buggy_cases[,names(mut_buggy_cases) != "class"], evs_buggy_cases)

#print(names(aug_buggy_cases))

#msizes = seq(600,0,-100)
msizes = c(msize) #seq(0,600,100)

res_mut_only = c()
res_mut_evs  = c()

for (n in msizes) {

  nrep = 10
  mut_only_err = 0.0
  mut_evs_err  = 0.0 
  
  #print(c("n:",n))

  #mut_robust_cases = mut_robust_cases[mut_robust_cases$size >= n,]
  #mut_buggy_cases = mut_buggy_cases[mut_buggy_cases$size >= n,]
  
  robust_cases = aug_robust_cases[aug_robust_cases$size >= n, names(aug_robust_cases) != "size"]
  buggy_cases  = aug_buggy_cases[aug_buggy_cases$size >= n,   names(aug_buggy_cases) != "size"]

  #print(c(aug_robust_cases$program, aug_buggy_cases$program))
  #aaaa
  uniq_programs = levels(factor(c(aug_robust_cases$program, aug_buggy_cases$program)))

  np = length(uniq_programs)
  #print(paste("number of programs",np,"selected with at most",n,"events"))

  for (r in 1:nrep) {
    
    gc() 

    psample = sample(np)
    train_programs = uniq_programs[psample[1:as.integer(np*0.75)]]
    test_programs = uniq_programs[psample[(as.integer(np*0.75)+1):np]]

    #print(paste("intersection train and test:", intersect(train_programs,test_programs)))

    buggy_train = buggy_cases[buggy_cases$program %in% train_programs,names(buggy_cases) != "program"]
    buggy_test  = buggy_cases[buggy_cases$program %in% test_programs,names(buggy_cases) != "program"]

    robust_train = robust_cases[robust_cases$program %in% train_programs,names(robust_cases) != "program"]
    robust_test  = robust_cases[robust_cases$program %in% test_programs,names(robust_cases) != "program"]
 
    buggy_train = sample(buggy_train)
    buggy_test = sample(buggy_test)

    robust_train = sample(robust_train)
    robust_test = sample(robust_test)

    train_size = min(nrow(buggy_train), nrow(robust_train))
    test_size = min(nrow(buggy_test), nrow(robust_test))

    #print(c(train_size, test_size))

    train = rbind(buggy_train[1:train_size,], robust_train[1:train_size,])
    test  = rbind(buggy_test[1:test_size,], robust_test[1:test_size,])

    varnot0 = names(train)[unlist(lapply(train,function(x) 0 != var(x)))]

    xy_train = train[,varnot0]
    xy_test  = test[,varnot0]

    x_test = xy_test[,names(xy_test) != "class"]
    y_test  = xy_test[,"class"]

    tcont = tune.control(sampling='fix')
    
    #print(names(xy_train))
    # mutation only variables    

    to_train = xy_train[,intersect(names(xy_train),mut_vars)]
    to_test = x_test[,intersect(names(x_test),mut_vars)]
   
    m = tune.knn(to_train[,names(to_train) != "class"], to_train[,"class"], validation.x = to_test, validation.y = y_test, k = 1:10, tunecontrol=tcont)
    #m = tune.svm(class~., data = to_train, validation.x = x_test, validation.y = y_test, gamma = 10^(-5:-1), cost = 10^(-1:3), tunecontrol=tcont)
   
    mut_only_err = mut_only_err + m$best.performance
    #print(mut_only_err / r)
    # mutation and event variables  
 
    to_train = xy_train
    to_test = x_test

    #print("mutation + events")

    m = tune.knn(to_train[,names(to_train) != "class"], to_train[,"class"], validation.x = to_test, validation.y = y_test, k = 1:10, tunecontrol=tcont)
    #m = tune.svm(class~., data = to_train, validation.x = x_test, validation.y = y_test, gamma = 10^(-5:-1), cost = 10^(-1:3), tunecontrol=tcont)
   
    mut_evs_err = mut_evs_err + m$best.performance
    #print(mut_evs_err / r)

  }
  
  res_mut_only = c(res_mut_only, mut_only_err / nrep)
  res_mut_evs = c(res_mut_evs, mut_evs_err / nrep)

}

print(data.frame(n=msizes, MutationOnly=res_mut_only, MutationEvents=res_mut_evs))
