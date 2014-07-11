options(warn=-1)

msg.trap <- capture.output( suppressMessages( library("e1071") ))
msg.trap <- capture.output( suppressMessages( library("tm") ))
msg.trap <- capture.output( suppressMessages( library("randomForest") ))
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
cutoff = abs(as.numeric(args[1]))
wf = wfs[,args[2]]

dir = "../30-06-2014"

options(stringsAsFactors=F)

# train data

mycon = gzcon(gzfile(paste(dir, "vulnerable_programs.csv.gz", sep="/"), open="r"))
vulnerable_programs = read.csv(textConnection(readLines(mycon)), sep="\t", header = F)

#if (! ("mycon" %in% ls())) {
mycon = gzcon(gzfile(paste(dir, "buggy_train.csv.gz", sep="/"), open="r"))  
buggy_program_events = read.csv(textConnection(readLines(mycon)), sep="\t", header = F)

buggy_program_events = na.omit(buggy_program_events)

mycon = gzcon(gzfile(paste(dir, "robust_train.csv.gz", sep="/"), open="r"))
robust_program_events = read.csv(textConnection(readLines(mycon)), sep="\t", header = F)

buggy_program_events = na.omit(buggy_program_events)

robust_programs = robust_program_events[,1]
robust_events = robust_program_events[,3]

#print(dim(buggy_program_events))
#print(dim(robust_program_events))

buggy_programs = buggy_program_events[,1]
buggy_events = buggy_program_events[,3]

evs_corpus = Corpus(VectorSource(c(buggy_events,robust_events)))

#print(evs_corpus)
#quit()

BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3, delimiters="  "))}
cl = list(tokenize = BigramTokenizer, bounds = list(global = c(1,Inf)), weighting = wf) 
#cl = list(bounds = list(global = c(1,Inf)), weighting = wf)#, tokenize = BigramTokenizer) 

evs_dm = DocumentTermMatrix(evs_corpus, control = cl)

sink("/dev/null")
evs_dm_df =  as.data.frame(inspect(evs_dm))
# cutoff
evs_dm_df[evs_dm_df>cutoff] = cutoff

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

#print(evs_vars_to_remove)

#quit()
evs_dm_df = evs_dm_df[,setdiff(evs_vars,evs_vars_to_remove)]
evs_vars = names(evs_dm_df)

evs_robust_cases = evs_dm_df[evs_dm_df$class == "R",]
evs_buggy_cases  = evs_dm_df[evs_dm_df$class == "B",]

aug_robust_cases = evs_robust_cases#[,names(evs_robust_cases) != "class"] 
aug_buggy_cases  = evs_buggy_cases#[,names(evs_buggy_cases) != "class"]

# test data

mycon = gzcon(gzfile(paste(dir, "test.csv.gz", sep="/"), open="r"))  
sample_program_events = read.csv(textConnection(readLines(mycon)), sep="\t", header = F)
sample_program_events = na.omit(sample_program_events)


sample_programs = sample_program_events[,1]
sample_events = sample_program_events[,3]
unique_sample_programs = levels(factor(sample_programs))

sample_program_sizes = sapply(FUN = length, strsplit( sample_events , split = " "))

#print(sample_programs %in% vulnerable_programs[,1])

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
# cutoff
r_evs_dm_df[r_evs_dm_df>cutoff] = cutoff

#print(names(r_evs_dm_df))
#r_evs_dm_df = r_evs_dm_df[,setdiff(r_evs_vars,evs_vars_to_remove)]
real_test = r_evs_dm_df

#print(dim(real_test))

#quit()

#msizes = seq(600,0,-100)
#msizes = c(msize) #seq(0,600,100)

res_mut_only = c()
res_mut_evs  = c()

#for (n in msizes) {

#nrep = 1
mut_only_err = 0.0
mut_evs_err  = 0.0 
  
 
robust_cases = aug_robust_cases#[aug_robust_cases$size >= n,]#, names(aug_robust_cases) != "size"]
buggy_cases  = aug_buggy_cases#[aug_buggy_cases$size >=n,]

#for (r in 1:nrep) {
    
#gc() 

buggy_train = buggy_cases#[buggy_cases$program %in% train_programs,]#,names(buggy_cases) != "program"]

robust_train = robust_cases#[robust_cases$program %in% train_programs,]#,names(robust_cases) != "program"]
  
train_size = min(nrow(buggy_train), nrow(robust_train))
extravs = c("program","trace","size")

train = rbind(buggy_train[1:train_size,setdiff(names(buggy_train),extravs)], robust_train[1:train_size,setdiff(names(robust_train),extravs)])
varnot0 = names(train)[unlist(lapply(train,function(x) 0 != var(x)))]
xy_train = train[,varnot0]

x_real_test = real_test[,setdiff(varnot0,c("class"))]


tcont = tune.control(sampling='fix')
    
to_train = xy_train
#for (ntree in seq(500,500,500)) {
ntree = 500
#print(paste("ntree:", ntree))

#m = randomForest(to_train[,names(to_train) != "class"], to_train[,"class"], importance=TRUE, ntree = ntree)
#m = tune.knn(to_train[,names(to_train) != "class"], to_train[,"class"], k = 1:10)
#m = tune.randomForest(to_train[,names(to_train) != "class"], to_train[,"class"], validation.x = to_test, validation.y = y_test, tunecontrol=tcont, importance=TRUE)
m = tune.svm(class~., data = to_train, gamma = 10^(-5:-1), cost = 10^(-1:3))
print(m)

#model = knn(to_train[,names(to_train) != "class"], to_train[,"class"], k = 1)
model = m$best.model
#print(model)

#imp =  round(importance(model),2)
#inds = order(imp[,"B"], decreasing=T)
#rownames(imp) = rownames(imp)[inds]
#print(head(data.frame(imp[inds,"B"]),20)) 
#print(head(data.frame(imp[inds,"R"]),20)) 
    
    
tmp = t(abs(t(model$coefs) %*% model$SV))
inds = sort(tmp, decreasing=TRUE, index.return = TRUE)$i
print(names(tmp[inds,][1:25]))

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
 
#print(confusion(scores[,"score"]>0,rownames(scores) %in% vulnerable_programs[,1]))
#print(scores)
#print(sizes)
ids = order(scores, decreasing=T)
#print(ids)
rownames(scores) = rownames(scores)[ids]
scores[,"score"] = scores[ids,"score"]
sizes[,"size"] = sizes[ids,"size"]
ext_result = data.frame(score=scores, size=sizes)
print(ext_result)

output = c()

for (sc in c(0,1/3,2/3,3/3)) {
  n_vuln = length(scores[(scores$score == sc) & (rownames(scores) %in% vulnerable_programs[,1]),])
  n = length(scores[(scores$score == sc),])

  #print(n)
  output = c(output, n_vuln, n)
  #print(output)
}

write.table(t(c(cutoff, output)), stdout(), ,row.names=FALSE, col.names=FALSE, sep = ",")
#print(c(cutoff, output))

#}
#print(z == "B")
#print(sample_programs %in% vulnerable_programs[,1])    
#print(scores[order(scores, decreasing=T),])
   
#mut_evs_err = mut_evs_err + m$best.performance
#print(mut_evs_err / r)

#}
  
#res_mut_only = c(res_mut_only, mut_only_err / nrep)
#res_mut_evs = c(res_mut_evs, mut_evs_err / nrep)

#}

#print(data.frame(n=msizes, MutationOnly=res_mut_only, MutationEvents=res_mut_evs))
