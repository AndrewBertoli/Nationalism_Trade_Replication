shuffled_countries1992<-sample(unlist(euro1992[,2:4]))

perm_groups1992<-cbind(euro1992[,1],shuffled_countries1992[1:2],
                       shuffled_countries1992[3:4],shuffled_countries1992[5:6])


shuffled_countries1996<-sample(unlist(euro1996[,2:4]))

perm_groups1996<-cbind(euro1996[,1],shuffled_countries1996[1:4],
                       shuffled_countries1996[5:8],shuffled_countries1996[9:12])
                       
perm_groups2000<-cbind(euro2000[,1],sample(euro2000[,2]),
                       sample(euro2000[,3]),sample(euro2000[,4]))   
                       
perm_groups2004<-cbind(euro2004[,1],sample(euro2004[,2]),
                       sample(euro2004[,3]),sample(euro2004[,4]))                          
                                           
perm_groups2008<-cbind(euro2008[,1],sample(euro2008[,2]),
                       sample(euro2008[,3]),sample(euro2008[,4]))   
                       
perm_groups2012<-cbind(euro2012[,1],sample(euro2012[,2]),
                       sample(euro2012[,3]),sample(euro2012[,4]))     
                       
perm_groups2016<-cbind(euro2016[,1],sample(euro2016[,2]),
                       sample(euro2016[,3]),sample(euro2016[,4]))    
                       
perm_groups2021<-euro2021

perm_groups2021[c(1,4:6),2]<-sample(perm_groups2021[c(1,4:6),2])

perm_groups2021[c(1,3:6),3]<-sample(perm_groups2021[c(1,3:6),3])

perm_groups2021[1:2,4]<-sample(perm_groups2021[1:2,4])
                                                                    



perm_groups<-list(perm_groups1992,perm_groups1996,
                  perm_groups2000,perm_groups2004,perm_groups2008,
                  perm_groups2012,perm_groups2016,perm_groups2021)