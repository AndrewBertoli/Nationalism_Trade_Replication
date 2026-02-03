perm_pairs=matrix(0,nrow=1,ncol=4)

for(j in 1:length(years)){

for(i in 1:length(perm_groups[[j]][,1])){
g=perm_groups[[j]][i,]
perm_pairs=rbind(perm_pairs,matrix(c(as.character(t(combn(g, 2))),rep(years[j],length(t(combn(g, 2))[,1])),rep(month[j],length(t(combn(g, 2))[,1]))),ncol=4))}
}

perm_pairs=perm_pairs[-1,]

perm_pairs=perm_pairs[-unique(c(which(perm_pairs[,1]=="NONE"),which(perm_pairs[,2]=="NONE"))),]

perm_pairs[,3]=as.numeric(perm_pairs[,3])

for(i in 1:length(perm_pairs[,1])){perm_pairs[i,1:2]=sort(perm_pairs[i,1:2])}