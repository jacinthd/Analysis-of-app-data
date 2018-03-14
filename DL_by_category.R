#make category dataset for testing
category_dataset<-DL_REV_FULL[,c(1:2,6:9,20:21)]

summary(category_dataset)

as.data.frame(table(category_dataset$Category))

#look at the top 6 apps by net IAP revenue
head(category_dataset[order(-category_dataset$IAP_NetRevenue),])


#visualize category distribution with DL and rev
library(ggplot2)

ggplot(data=category_dataset,aes(Category,DTO_DL))+geom_boxplot()

ggplot(data=category_dataset,aes(Category,DTO_NetRevenue))+geom_boxplot()
#the graphs tell us that there are lots of outliers


#understand how downloads are distributed
quantile(category_dataset$DTO_DL,na.rm=T)

head(table(category_dataset$DTO_DL))


#the number of apps for each category
ggplot(data=category_dataset,aes(x=Category))+geom_bar()

#visualize the distribution for each category
ggplot(data=category_dataset,aes(x=DTO_DL))+geom_histogram()+facet_wrap(~Category,nrow=6,ncol=3)


#consider apps with more than 10 DLs
category_data_DLs_morethan10<-subset(category_dataset,DTO_DL>10)

#understand their distribution
summary(category_data_DLs_morethan10$Category)

ggplot(data=category_data_DLs_morethan10,aes(x=Category))+geom_bar()


#transform to get it normal like(this transformation also reduces variability)
category_data_DLs_morethan10$DTO_DL<-log(log(category_data_DLs_morethan10$DTO_DL))
#category_data_DLs_morethan10<-subset(category_data_DLs_morethan10,DTO_DL<2.5)

ggplot(data=category_data_DLs_morethan10,aes(x=DTO_DL))+geom_histogram()+facet_wrap(~Category,nrow=6,ncol=3,scales="free")


#the graph show outliers with DL above 2.5, this dataset below just has these outliers
outliers<-category_data_DLs_morethan10[which(category_data_DLs_morethan10$DTO_DL>2.5),]

#the difference in variance among categories is also not much now
var_categories<-aggregate(DTO_DL~Category,data=category_data_DLs_morethan10,FUN=mean)

var_categories$DTO_DL<-exp(exp(var_categories$DTO_DL))

var_categories<-var_categories[order(-var_categories$DTO_DL),]

#visualize their distribution
ggplot(data=category_data_DLs_morethan10,aes(Category,DTO_DL))+geom_boxplot()


#Proceed with anova
aov.category<-aov(DTO_DL~Category,data=category_data_DLs_morethan10)

summary(aov.category)


#post-hoc to see which differences are significant
pairwise.t.test(category_data_DLs_morethan10$DTO_DL,category_data_DLs_morethan10$Category,p.adjust="bonferroni")

posthoc.category<-TukeyHSD(aov.category,conf.level=0.95)$Category
posthoc.category<-as.data.frame(posthoc.category)

results.category<-summary(lm(DTO_DL~Category,data=category_data_DLs_morethan10))$coefficients
results.category<-as.data.frame(results.category)


#get median downloads of each category as output
medians.category<-aggregate(DTO_DL~Category,data=category_data_DLs_morethan10,FUN=median)
medians.category<-medians.category[order(-medians.category$DTO_DL),]

#mean DLs per category is the prediction
means.category<-aggregate(DTO_DL~Category,data=category_data_DLs_morethan10,FUN=mean)
means.category<-means.category[order(-means.category$DTO_DL),]

#mean DLs per category is the prediction
means.category2<-aggregate(DTO_DL~Category,data=category_data_DLs_morethan10,FUN=mean)
means.category2<-means.category2[order(-means.category2$DTO_DL),]
means.category2$DTO_DL<-exp(exp(means.category2$DTO_DL))


#Diagnostics of the test
plot(lm(DTO_DL~Category,data=category_data_DLs_morethan10))

anova(lm(DTO_DL~Category,data=category_data_DLs_morethan10))

summary(lm(DTO_DL~Category,data=category_dataset))

category_data_DLs_morethan10[135511,]


