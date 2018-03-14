top.1000.apps.by.DL<-head(category_dataset[order(-category_dataset$DTO_DL),])


#top 100 apps from each category
test<-subset(DL_REV_FULL,Category=="KidsAndFamily")

test2<-head(test[order(-test$DTO_DL),],100)

test<-subset(category_data_DLs_morethan10,Category=="Social")

test2<-head(test[order(-test$DTO_DL),],100)


#get number of apps from each category
test<-as.data.frame(table(DL_REV_FULL$Category))
test$contr_to_total<-test$Freq/sum(test$Freq)


#Total DLs
total.category<-aggregate(DTO_DL~Category,data=DL_REV_FULL,FUN=sum)
total.category$contr_to_total<-total.category$DTO_DL/sum(total.category$DTO_DL)


#get the means by each category
means.category.all<-aggregate(DTO_DL~Category,data=DL_REV_FULL,FUN=mean)
means.category.all<-means.category.all[order(-means.category.all$DTO_DL),]

