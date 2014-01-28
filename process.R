mycon<-gzcon(gzfile("28-01-2014/traces.csv.gz", open="r"))
traces = read.csv(textConnection(readLines(mycon)))[,-1]

#library("kohonen")

#training <- sample(nrow(traces), 100)
#Xtraining <- scale(traces[training,])

#grid = somgrid(100, 100, "hexagonal")
#som.traces <- som(Xtraining, grid)

#kohonen::plot.kohonen(som.traces,  type = "mapping", label = traces[training,1], main = "")

#library("kohonen")

#training <- sample(nrow(traces), 300)
#Xtraining <- traces[training,-1]

#d <- dist(Xtraining)
#fit <- cmdscale(d,eig=TRUE, k=2)

#plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="Metric MDS", type="n")
#text(x, y, labels = traces[training,1], cex=.7) 
