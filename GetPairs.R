
month<-c(rep(6,19),6,6,6,6,7,7,1,1,1)

pairs<-matrix(0,nrow=1,ncol=4)

for(j in 1:length(years)){

for(i in 1:length(groups[[j]][,1])){
g<-groups[[j]][i,]
pairs<-rbind(pairs,
            matrix(c(t(combn(g, 2)),
                     rep(c(years[j]),
                         length(t(combn(g, 2))[,1])),
                     rep(c(month[j]),
                         length(t(combn(g, 2))[,1]))),
                   ncol=4))}
}

pairs<-pairs[-1,]

pairs<-pairs[-unique(c(which(pairs[,1]=="NONE"),
                      which(pairs[,2]=="NONE"))),]

for(i in 1:length(pairs[,1])){
  pairs[i,1:2]<-sort(unlist(pairs[i,1:2]))}