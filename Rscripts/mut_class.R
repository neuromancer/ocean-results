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

msizes = (rev(c(1, 50, 100, 150, 200, 250, 300)))
res = c()

mut_robust_cases = mut_dm_df[mut_dm_df$class == "R",]
mut_buggy_cases  = mut_dm_df[mut_dm_df$class == "B",]

aug_robust_cases = mut_robust_cases 
aug_buggy_cases  = mut_buggy_cases

for (n in msizes) {

  nrep = 3
  mut_only_acc = 0.0 
  
  #print(c("n:",n))

  #mut_robust_cases = mut_robust_cases[mut_robust_cases$size >= n,]
  #mut_buggy_cases = mut_buggy_cases[mut_buggy_cases$size >= n,]
  
  robust_cases = aug_robust_cases[aug_robust_cases$size >= n, names(aug_robust_cases) != "size"]
  buggy_cases  = aug_buggy_cases[aug_buggy_cases$size >= n,   names(aug_buggy_cases) != "size"]

  np = length(uniq_programs)
  #print(paste("number of programs",,"selected with at most", n,"events"))

  for (r in 1:nrep) {
    
    gc() 

    psample = sample(np)
    train_programs = uniq_programs[psample[1:as.integer(np*0.75)]]
    test_programs = uniq_programs[psample[(as.integer(np*0.75)+1):np]]

    print(c("intersection train and test:", intersect(train_programs,test_programs)))

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

    print(c(train_size, test_size))

    train = rbind(buggy_train[1:train_size,], robust_train[1:train_size,])
    test  = rbind(buggy_test[1:test_size,], robust_test[1:test_size,])

    varnot0 = names(train)[unlist(lapply(train,function(x) 0 != var(x)))]

    xy_train = train[,varnot0]
    xy_test  = test[,varnot0]

    x_test = test[,names(test) != "class"]
    y_test  = test[,"class"]

    tcont = tune.control(sampling='fix')
    #print(names(xy_train))
    m = tune.svm(class~., data = xy_train, validation.x = x_test, validation.y = y_test, gamma = 10^(-5:-1), cost = 10^(-1:3), tunecontrol=tcont)
    model = m$best.model
    #model = svm(class ~., data=xy_train, gamma=1e-05, cost=0.1)    

    z = predict(model,x_test)
    #print(summary(m))
    print(confusionMatrix(table(pred=z, true=y_test)))
    mut_only_acc = mut_only_acc + as.numeric(confusionMatrix(table(pred=z, true=y_test))$overall[1])
    print(mut_only_acc / r)
  }
  
  res = c(res, mut_only_acc / nrep)    

}

print(data.frame(n=msizes, acc=res))
