#make category dataset for testing
category_dataset<-DL_REV_FULL[,c(1:2,6:9,20:21)]

library(ggplot2)

#get DTO+IAP Rev
category_dataset$DTO_IAP_Revenue<-
ifelse(is.na(category_dataset$DTO_NetRevenue),0,category_dataset$DTO_NetRevenue)+
ifelse(is.na(category_dataset$IAP_NetRevenue),0,category_dataset$IAP_NetRevenue)


#understand how rev is distributed
quantile(category_dataset$DTO_IAP_Revenue,na.rm=T)

#exclude apps with rev below 1
category_data_rev_above_0<-subset(category_dataset,DTO_IAP_Revenue>2)

#understand how rev is distributed for these apps
quantile(category_data_rev_above_0$DTO_IAP_Revenue,na.rm=T)

#the number of apps for each category
table(category_data_rev_above_0$Category)
ggplot(data=category_data_rev_above_0,aes(x=Category))+geom_bar()

#transformation to normalize and reduce variance
category_data_rev_above_0$DTO_IAP_Revenue<-log(log(category_data_rev_above_0$DTO_IAP_Revenue)+1)

category_data_rev_above_0$DTO_IAP_Revenue<-log(category_data_rev_above_0$DTO_IAP_Revenue,100)

#Classify categories as others for categories with n<300
temp_vec<-as.character(category_data_rev_above_0$Category)
category_data_rev_above_0$category.new<-as.factor(ifelse(temp_vec=="GovernmentAndPolitics"|temp_vec=="Business"|temp_vec=="Finance"|temp_vec=="Lifestyle"|temp_vec=="NewsAndWeather"|temp_vec=="Sports","Others",temp_vec))

#visualize the distribution for each category
table(category_data_rev_above_0$category.new)
ggplot(data=category_data_rev_above_0,aes(x=DTO_IAP_Revenue))+geom_histogram()+facet_wrap(~category.new,nrow=4,ncol=3,scales="free")

#var by category, there's high variance for 'games'
aggregate(DTO_IAP_Revenue~category.new,data=category_data_rev_above_0,FUN=var)

#lm output
summary(lm(DTO_IAP_Revenue~category.new,data=category_data_rev_above_0))

#anova test results
aov.rev<-aov(DTO_IAP_Revenue~category.new,data=category_data_rev_above_0)

summary(aov.rev)
  
#the means of each category visualized
ggplot(data=category_data_rev_above_0,aes(category.new,DTO_IAP_Revenue))+geom_boxplot()

#post-hoc analysis
TukeyHSD(aov.rev)
#outliers<-subset(category_data_rev_above_0,DTO_IAP_Revenue>2 & Category!="Games")

#get median revenue of each category as output
medians.category<-aggregate(DTO_IAP_Revenue~category.new,data=category_data_rev_above_0,FUN=median)
medians.category<-medians.category[order(-medians.category$DTO_IAP_Revenue),]

plot(lm(DTO_IAP_Revenue~category.new,data=category_data_rev_above_0))



