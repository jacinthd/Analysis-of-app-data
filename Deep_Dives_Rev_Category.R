test<-subset(category_dataset,Category=="NewsAndWeather")

test2<-head(test[order(-test$DTO_IAP_Revenue),],100)

cor(category_dataset$DTO_IAP_Revenue,category_dataset$DTO_DL,use="complete.obs")

plot(log(log(testdf$DTO_DL)+1),testdf$DTO_IAP_Revenue)

testdf<-subset(category_data_rev_above_0,DTO_DL>1,select=c(DTO_IAP_Revenue,DTO_DL))

outliers2<-merge(category_data_rev_above_0,DL_REV_FULL[,c(1,3:5)],by="ProductId")

summary(outliers2[,9:12])

summary(category_data_rev_above_0)

#no of apps with only DTO enabled
nrow(subset(outliers2,(Paid.DTO=='Yes') & ((Paid_IAP_enabled=='No')|is.na(Paid_IAP_enabled)) & ((AiA.Enabled=='No')|is.na(AiA.Enabled))))

#no of apps with IAP not enabled and DTO and AiA enabled
nrow(subset(outliers2,(Paid.DTO=='Yes') & ((Paid_IAP_enabled=='No')|is.na(Paid_IAP_enabled)) & (AiA.Enabled=='Yes')))

#no of apps with IAP enabled
nrow(subset(outliers2,(Paid_IAP_enabled=='Yes')))

#% of revenue from DTO and IAP
sum(category_dataset$DTO_NetRevenue,na.rm=T)/
(sum(category_dataset$IAP_NetRevenue,na.rm=T)+sum(category_dataset$DTO_NetRevenue,na.rm=T))

total.category<-aggregate(DTO_IAP_Revenue~Category,data=category_dataset,FUN=sum)
total.category$contr_to_total<-100*(total.category$DTO_IAP_Revenue/sum(total.category$DTO_IAP_Revenue))
