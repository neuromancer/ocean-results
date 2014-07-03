options(warn=-1)

msg.trap <- capture.output( suppressMessages( library("e1071") ))
msg.trap <- capture.output( suppressMessages( library("tm") ))
#msg.trap <- capture.output( suppressMessages( library("gbm") ))
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

dir = "../30-06-2014"

options(stringsAsFactors=F)


mycon = gzcon(gzfile(paste(dir, "vulnerable_programs.csv.gz", sep="/"), open="r"))
vulnerable_programs = read.csv(textConnection(readLines(mycon)), sep="\t", header = F)

#if (! ("mycon" %in% ls())) {
mycon = gzcon(gzfile(paste(dir, "buggy_train.csv.gz", sep="/"), open="r"))  
buggy_program_events = read.csv(textConnection(readLines(mycon)), sep="\t", header = F)

buggy_program_events = na.omit(buggy_program_events)


# real test

#dir = "../30-06-2014"

#mycon = gzcon(gzfile(paste(dir, "test_sample.csv.gz", sep="/"), open="r"))  
#sample_program_events = read.csv(textConnection(readLines(mycon)), sep="\t", header = F)
#sample_program_events = na.omit(sample_program_events)

#sample_programs = sample_program_events[,1]
#unique_sample_programs = levels(factor(sample_programs))

#test_sample = sample(lenght(unique_sample_programs))

#robust_programs = unique_sample_programs(test_sample[1:22]) 

#print(robust_programs)
#quit()

#sample_program_events = sample_program_events[,3]
#sample_program_sizes = sapply(FUN = length, strsplit( sample_program_events , split = " "))

#quit()

mycon = gzcon(gzfile(paste(dir, "robust_train.csv.gz", sep="/"), open="r"))
robust_program_events = read.csv(textConnection(readLines(mycon)), sep="\t", header = F)

buggy_program_events = na.omit(buggy_program_events)

robust_programs = robust_program_events[,1]
robust_events = robust_program_events[,3]

print(dim(buggy_program_events))
print(dim(robust_program_events))

buggy_programs = buggy_program_events[,1]
buggy_events = buggy_program_events[,3]

evs_corpus = Corpus(VectorSource(c(buggy_events,robust_events)))

print(evs_corpus)
#quit()

BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 3, delimiters="  "))}
#cl = list(tokenize = BigramTokenizer, bounds = list(global = c(1,Inf)), weighting = wf) 
cl = list(bounds = list(global = c(1,Inf)), weighting = wf)#, tokenize = BigramTokenizer) 

evs_dm = DocumentTermMatrix(evs_corpus, control = cl)

sink("/dev/null")
evs_dm_df =  as.data.frame(inspect(evs_dm))

evs_dm_df["program"] = c(buggy_programs, robust_programs)
evs_dm_df["class"] = factor( c(rep("B",length(buggy_programs)), rep("R",length(robust_programs))), levels = c("R","B"))
evs_dm_df["size"] = sapply(FUN = length, strsplit( c(buggy_events,robust_events) , split = " ")) 

evs_vars = names(evs_dm_df)

# variable removal
sink()

evs_vars_to_remove = c()

evs_vars_to_remove = c(evs_vars_to_remove, evs_vars[grepl("crash", evs_vars)])
evs_vars_to_remove = c(evs_vars_to_remove, evs_vars[grepl("exit", evs_vars)])
evs_vars_to_remove = c(evs_vars_to_remove, evs_vars[grepl("sigsegv", evs_vars)])
#evs_vars_to_remove = c(evs_vars_to_remove, evs_vars[grepl("sigabrt", evs_vars)])
#evs_vars_to_remove = c(evs_vars_to_remove, evs_vars[grepl("abort", evs_vars)])

print(evs_vars_to_remove)

#quit()
evs_dm_df = evs_dm_df[,setdiff(evs_vars,evs_vars_to_remove)]
evs_vars = names(evs_dm_df)

evs_robust_cases = evs_dm_df[evs_dm_df$class == "R",]
evs_buggy_cases  = evs_dm_df[evs_dm_df$class == "B",]

aug_robust_cases = evs_robust_cases#[,names(evs_robust_cases) != "class"] 
aug_buggy_cases  = evs_buggy_cases#[,names(evs_buggy_cases) != "class"]

#quit()

mycon = gzcon(gzfile(paste(dir, "test.csv.gz", sep="/"), open="r"))  
sample_program_events = read.csv(textConnection(readLines(mycon)), sep="\t", header = F)
sample_program_events = na.omit(sample_program_events)


sample_events = sample_program_events[,3]
sample_programs = sample_program_events[,1]
unique_sample_programs = levels(factor(sample_programs))

sample_program_sizes = sapply(FUN = length, strsplit( sample_events , split = " "))

print(sample_programs %in% vulnerable_programs[,1])

#test_sample = sample(lenght(unique_sample_programs))

#robust_programs = unique_sample_programs(test_sample[1:22])

evs_corpus = Corpus(VectorSource(sample_events))

r_evs_dm = DocumentTermMatrix(evs_corpus, control = cl)

sink("/dev/null")
r_evs_dm_df =  as.data.frame(inspect(r_evs_dm))
r_evs_dm_df["size"] = sample_program_sizes
r_evs_vars = names(r_evs_dm_df)

# variable removal
sink()

r_evs_dm_df[,setdiff(evs_vars,r_evs_vars)] = 0
#print(names(r_evs_dm_df))
#r_evs_dm_df = r_evs_dm_df[,setdiff(r_evs_vars,evs_vars_to_remove)]
real_test = r_evs_dm_df

print(dim(real_test))

#quit()

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
  
  robust_cases = aug_robust_cases#[aug_robust_cases$size >= n,]#, names(aug_robust_cases) != "size"]
  buggy_cases  = aug_buggy_cases#[aug_buggy_cases$size >=n,]

  #print(c(aug_robust_cases$program, aug_buggy_cases$program))
  #aaaa
  #buggy_uniq_programs = setdiff(levels(factor(c(aug_buggy_cases$program))) , sample_programs)
  #buggy_np = length(buggy_uniq_programs)

  #print(buggy_uniq_programs)

  #robust_uniq_programs = setdiff( levels(factor(c(aug_robust_cases$program))), sample_programs)
  #robust_np = length(robust_uniq_programs)

  #print(robust_uniq_programs)

  #print(c(buggy_np,robust_np))

  #print(paste("number of programs",np,"selected with at most",n,"events"))

  for (r in 1:nrep) {
    
    gc() 

    #cont = TRUE
    #while (cont) {

    #buggy_psample = sample(buggy_np)
    #robust_psample = sample(robust_np)

    #ts = 1
    #train_programs = c( buggy_uniq_programs[buggy_psample[1:(buggy_np-ts)]] , robust_uniq_programs[robust_psample[1:(robust_np-ts)]] )
    #test_programs = c( buggy_uniq_programs[buggy_psample[(buggy_np-ts+1):buggy_np]] , robust_uniq_programs[robust_psample[(robust_np-ts+1):robust_np]] )
   
    #print("selected programs:")
    #print(c(train_programs))
    #print(c(test_programs))

    #print(paste("intersection train and test:", intersect(train_programs,test_programs)))i

    buggy_train = buggy_cases#[buggy_cases$program %in% train_programs,]#,names(buggy_cases) != "program"]
    #buggy_test  = buggy_cases#[buggy_cases$program %in% test_programs,]#,names(buggy_cases) != "program"]

    robust_train = robust_cases#[robust_cases$program %in% train_programs,]#,names(robust_cases) != "program"]
    #robust_test  = robust_cases#[robust_cases$program %in% test_programs,]#,names(robust_cases) != "program"]
  
    #rownames(buggy_train)
    #buggy_train = buggy_train[sample(rownames(buggy_train)),]
    #buggy_test = buggy_test[sample(rownames(buggy_test)),]

    #robust_train = robust_train[sample(rownames(robust_train)),]
    #robust_test = robust_test[sample(rownames(robust_test)),]
 
    #print(buggy_train)
    #print(nrow(buggy_train))
    #print(robust_train)
    #print(nrow(robust_train))

    train_size = min(nrow(buggy_train), nrow(robust_train))
    #test_size = min(nrow(buggy_test), nrow(robust_test))

    #if (test_size >= 50)
    #   cont = FALSE
    #}

    print(c(train_size))

    extravs = c("program","trace")

    train = rbind(buggy_train[1:train_size,setdiff(names(buggy_train),extravs)], robust_train[1:train_size,setdiff(names(robust_train),extravs)])
    #test  = rbind(buggy_test[1:test_size,setdiff(names(buggy_test),extravs)], robust_test[1:test_size,setdiff(names(robust_test),extravs)])

    #test_programs = c(buggy_test[1:test_size,"program"], robust_test[1:test_size,"program"])
    #test_traces = c(buggy_test[1:test_size,"trace"], robust_test[1:test_size,"trace"])
    #print(test_programs)
    #quit()

    #print(any(is.na(train)))
    varnot0 = names(train)[unlist(lapply(train,function(x) 0 != var(x)))]
    print("size" %in% varnot0)
    #quit()
    #varnot0 = c(na.omit(varnot0), "class")
    #print

    xy_train = train[,varnot0]
    #xy_test  = test[,varnot0]

    print(paste("xy_train dim:",dim(xy_train)))


    #real_test[setdiff(varnot0,names(real_test)),] = 0
    #print(1)
    #print(varnot0[!(varnot0 %in% names(real_test))])
    #quit() 
    x_real_test = real_test[,setdiff(varnot0,c("class"))]


    #x_test = xy_test[,names(xy_test) != "class"]
    #y_test  = xy_test[,"class"]

    tcont = tune.control(sampling='fix')
    
    to_train = xy_train
    #to_test = x_test

    m = tune.randomForest(to_train[,names(to_train) != "class"], to_train[,"class"], importance=TRUE)
    #m = tune.knn(to_train[,names(to_train) != "class"], to_train[,"class"], validation.x = to_test, validation.y = y_test, k = 1:10, tunecontrol=tcont)
    #m = tune.randomForest(to_train[,names(to_train) != "class"], to_train[,"class"], validation.x = to_test, validation.y = y_test, tunecontrol=tcont, importance=TRUE)
    #m = tune.svm(class~., data = to_train, validation.x = x_test, validation.y = y_test, gamma = 10^(-5:-1), cost = 10^(-1:3), tunecontrol=tcont)
    #print(m)

    model = m$best.model
    print(model)

    imp =  round(importance(model),2)
    inds = order(imp[,"B"], decreasing=T)
    rownames(imp) = rownames(imp)[inds]
    print(head(data.frame(imp[inds,"B"]),20)) 
    
    #scores = t(abs(t(model$coefs) %*% model$SV))
    #inds = sort(scores, decreasing=TRUE, index.return = TRUE)$i
    #print(names(scores[inds,][1:10]))

    #z = predict(model,x_test)
    #result = data.frame(program = test_programs, trace=test_traces, pred=z)
    #print(result)
    #print(confusion(z,y_test)) 

    z = predict(model,x_real_test)
    #print(z)
    #print(sample_programs)   

    result = data.frame(program = sample_programs, pred=z)
    scores = data.frame(score=c())
    sizes = data.frame(size=c())
    #types = data.frame(size=c())

    for (program in levels(factor(sample_programs))) {
      #print(program)
      votes = table(result[result$program == program,"pred"])
      votes = votes / sum(votes)
      #print(votes$B)
      scores[program,"score"] = votes[2][1]

      rsizes = sample_program_sizes[sample_programs == program]
      sizes[program,"size"] = mean(rsizes)
      

      #print(
      #print(table(result[result$program == program,"pred"]))

    }
 
    print(confusion(scores[,"score"]>0,rownames(scores) %in% vulnerable_programs[,1]))
    #print(scores)
    #print(sizes)
    ids = order(scores, decreasing=T)
    #print(ids)
    rownames(scores) = rownames(scores)[ids]
    scores[,"score"] = scores[ids,"score"]
    sizes[,"size"] = sizes[ids,"size"]
    ext_result = data.frame(score=scores, size=sizes)
    #ext_result = ext_result[ext_result$size > 8,]
    print(ext_result)
    #print(z == "B")
    #print(sample_programs %in% vulnerable_programs[,1])

    
    #print(scores[order(scores, decreasing=T),])
   
    mut_evs_err = mut_evs_err + m$best.performance
    #print(mut_evs_err / r)

  }
  
  res_mut_only = c(res_mut_only, mut_only_err / nrep)
  res_mut_evs = c(res_mut_evs, mut_evs_err / nrep)

}

print(data.frame(n=msizes, MutationOnly=res_mut_only, MutationEvents=res_mut_evs))
