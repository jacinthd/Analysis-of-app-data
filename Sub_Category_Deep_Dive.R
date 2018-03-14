test<-as.data.frame.matrix(table(DL_REV_FULL$Category,DL_REV_FULL$SubCategoryName))#,exclude=NULL))

cat.with.sub<-names(rowSums(test)[rowSums(test)>0])

cat.with.sub

test2<-test[row.names(test) %in% cat.with.sub,]

#check all the categories with subcategories
cat.testdf<-subset(DL_REV_FULL,Category=='Travel')

cat.testdf<-droplevels(cat.testdf)
                                                             
table(cat.testdf$Category,cat.testdf$SubCategoryName,exclude=NULL)

#find out which types of apps earn money in each subcategory and are popular
#make dataframe
cat.testdf2<-cat.testdf[,c(1:2,6:9,20:21,12:14)]

cat.testdf2$DTO_IAP_Revenue<-
  ifelse(is.na(cat.testdf2$DTO_NetRevenue),0,cat.testdf2$DTO_NetRevenue)+
  ifelse(is.na(cat.testdf2$IAP_NetRevenue),0,cat.testdf2$IAP_NetRevenue)

#find top apps by revenue
cat.testdf2<-cat.testdf2[order(-cat.testdf2$DTO_IAP_Revenue),]

#find top dev countries by revenue
test3<-table(droplevels(subset(cat.testdf2,DTO_IAP_Revenue>0))$DeveloperCountry)

test3<-table(cat.testdf2$DeveloperCountry)
test3<-cbind(test3,test3)
test3<-as.data.frame(test3)

#initialzie list
list.cat.rev<-NULL
list.cat.DL<-NULL



#make a df for each subcategory
for (i in levels(cat.testdf2$SubCategoryName)){
  loopdf<-subset(cat.testdf2,SubCategoryName==i)
  loopdf<-droplevels(loopdf)
  list.cat.rev[[length(list.cat.rev)+1]]<-loopdf[order(-loopdf$DTO_IAP_Revenue),]
  list.cat.DL[[length(list.cat.DL)+1]]<-loopdf[order(-loopdf$DTO_DL),]
}

testdf.rev<-list.cat.rev[[8]]
testdf.DL<-list.cat.DL[[8]]


