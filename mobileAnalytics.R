#Mobile Analytics
#Data Preparation
library(dplyr)
library(DescTools)
library(psych)
library(corrplot)
library(RColorBrewer)
library(ggplot2)
setwd('/Users/mason/Desktop/MSBA/Fall18/BANA277_f18/Mobile_Analytics/')
data <- read.csv('Geo-Fence Analytics.csv')
options(scipen=999)
#CreateDummy Variables
attach(data)
data$imp_large <- ifelse(imp_size == "728x90",1,0)
data$cat_entertainment <- ifelse(app_topcat == "IAB1" | app_topcat =="IAB1-6",1,0)
data$cat_social <- ifelse(app_topcat == "IAB14",1,0)
data$cat_tech <- ifelse(app_topcat == "IAB19-6",1,0)
data$as_ios <- ifelse(device_os == "iOS",1,0)
data$weight<- ifelse(data$didclick == 1,1,.001)
#Other variables
distance_LatLong <- function(Lat1,Lon1,Lat2,Lon2){
  6371 * acos(cos(DegToRad(Lat1)) * cos(DegToRad(Lat2)) * cos(DegToRad(Lon1) - DegToRad(Lon2)) + sin(DegToRad(Lat1)) * sin(DegToRad(Lat2)))
}
for (i in 1:nrow(data)){
  data$distance[i] <- distance_LatLong(device_lat[i],device_lon[i],geofence_lat[i],geofence_lon[i])
}
data$distance_squared <- data$distance^2
data$ln_app_review_vol <-log(app_review_vol)
detach(data)
head(data)

  
#Descriptive Stats
##Summary Statistics
ds_vars <- c('didclick','distance','imp_large','cat_entertainment','cat_social', 
             'cat_tech','as_ios','ln_app_review_vol','app_review_val')
df_vars <- data[ds_vars]
x<- describe(df_vars)
attach(x)
newx<- cbind(mean,median,sd,min,max);
rownames(newx)<-ds_vars
newx
detach(x)
##Correlations
thresh <- 0
correlations <- cor(df_vars[,-1])
correlations0 <- correlations
diag(correlations0) <- 0 
new<- sort(unique(c(which(abs(correlations0)> thresh,arr = TRUE))))
correlations <- correlations[new,new]
corrplot(correlations[new,new], method = "circle", type = "upper", col = brewer.pal(n=8, name = "RdYlBu"))

#Interpretations
# For variables with a strong correlation, either positive or negative, the pairings are included as follows:
# Category: Technology vs. Category: Entertainment, r = ~ (-0.653)
# Category: Technology vs. iOS Device, r = ~ (-0.599)
# Category: Technology vs. App Review Valance, r = ~ (-0.732)
# Category Entertainment vs. App Review Valance, r = ~ 0.642

  
#Click Through Rate by Distance (Binned)
data <- 
  data %>% 
  mutate(dist_bin=cut(distance, breaks=c(0, 0.5, 1,2,4,7,10, Inf), labels=c("1","2","3","4","5","6","7")))
dist_bin_freq <- as.data.frame.matrix(table(data$dist_bin, data$didclick))
dist_bin_freq$total <- dist_bin_freq$`0` + dist_bin_freq$`1`
dist_bin_freq$ctr <- dist_bin_freq$`1`/dist_bin_freq$total
colnames(dist_bin_freq)<- c("No Click","Did Click","Total", "Click Rate"); 
rownames(dist_bin_freq) <- c('0-0.5','0.5-1','1-2','2-4','4-7','7-10','>10')
dist_bin_freq

#Click Through Rate by Binned-'App Review Volume'
data<-
  data %>%
  mutate(vol_bin= cut(app_review_vol, breaks=1000*c(0,60,120,180,240,300,360,Inf), labels = c('1','2','3','4','5','6','7')))
vol_bin_freq<- as.data.frame.matrix(table(data$vol_bin,data$didclick))
vol_bin_freq$total <- vol_bin_freq$`0` + vol_bin_freq$`1`
vol_bin_freq$vol_ctr <-vol_bin_freq$`1`/vol_bin_freq$total
colnames(vol_bin_freq)<- c("No Click","Did Click","Total", "Click Rate"); 
rownames(vol_bin_freq) <- c('0-60','60-120','120-180','180-240','240-300','300-360','>360')
vol_bin_freq

#Click Through Rate by Binned - 'App Review Valance'
data<-
  data %>%
  mutate(val_bin= cut(app_review_val, breaks=c(1,2,3,4,5), labels = c('1','2','3','4')))
val_bin_freq<- as.data.frame.matrix(table(data$val_bin,data$didclick))
val_bin_freq$total <- val_bin_freq$`0` + val_bin_freq$`1`
val_bin_freq$val_ctr <-val_bin_freq$`1`/val_bin_freq$total
colnames(val_bin_freq)<- c("No Click","Did Click","Total", "Click Rate"); 
rownames(val_bin_freq) <- c('1-2','2-3','3-4','4-5')
val_bin_freq

  
#Plots of Interest
#Boxplot

didclick.sum<- aggregate(data$distance~data$didclick,data,mean)
ggplot(data, aes(x=as.factor(data$didclick),y=data$distance,fill=as.factor(data$didclick)))+
  geom_boxplot(outlier.colour = "red")+
  labs(x="Did Click",y="Distance")+
  scale_fill_discrete(name= "Did Click")



## Click Through Rate by Distance (Binned)

barplot(dist_bin_freq$`Click Rate`,
        xlab = "Distance (in km)", ylab = "Click Trhough Rate",
        main = "Click Through Rate vs Distance",
        col = "RoyalBlue",
        axis.lty = 1,
        names.arg = c('0-0.5','0.5-1','1-2','2-4','4-7','7-10','>10'),
        cex.axis = .8)



#Interpretations
# Impressions with smallest distance between device and impression location of interest perform best in click through rate. 
# For distances of (0 to 0.5) kilometers, the click through rate was 0.008192 and for distances (0.5 to 1) kilometers, the click through rate was 0.008195. Thus, it appears that the highest values of click through rates occur among those impressions that had the smallest distances between the device and the impressions' geofence location of interest.


#Click Through Rate by App Review Valance (Binned)
barplot(val_bin_freq$`Click Rate`,
xlab = "Review Valance", ylab = "Click Trhough Rate",
main = "Click Through Rate vs Review Valance",
col = "RoyalBlue",
axis.lty = 1,
names.arg = c('1-2','2-3','3-4','4-5'),
cex.axis = .8)
#Click Through Rate by App Review Volume (Binned)
barplot(vol_bin_freq$`Click Rate`,
xlab = "Review Counts (in 10,000s)", ylab = "Click Trhough Rate",
main = "Click Through Rate vs Review Count",
col = "RoyalBlue",
axis.lty = 1,
names.arg = c('0-6','6-12','12-18','18-24','24-30','30-36','>36'),
cex.axis = .8)

#Imp Large
imp_large_freq<- as.data.frame.matrix(table(data$dist_bin,data$imp_large))
colnames(imp_large_freq)<- c("Small","Large"); 
rownames(imp_large_freq) <- c('0-0.5','0.5-1','1-2','2-4','4-7','7-10','>10')
imp_large_freq
ggplot(data,aes(x=data$dist_bin,fill = as.factor(imp_large)))+
geom_bar(position = "dodge")+
labs(title= "Impression Size by Distance",x = 'Distance (in km)', y = "Frequency")+
theme_minimal()
#as_ios
as_ios_freq<- as.data.frame.matrix(table(data$dist_bin,data$as_ios))
colnames(as_ios_freq)<- c("Android/Other","iOS"); 
rownames(as_ios_freq) <- c('0-0.5','0.5-1','1-2','2-4','4-7','7-10','>10')
as_ios_freq
ggplot(data,aes(x=data$dist_bin,fill = as.factor(as_ios)))+
geom_bar(position = "dodge")+
labs(title = "Device Type by Distance", x = 'Distance (in km)', y = "Frequency")+
theme_minimal()
#cat_tech
ggplot(data,aes(x=dist_bin,fill = as.factor(cat_tech)))+
geom_bar(position = "dodge")+
labs(title= "Category: Technology (Y/N) By Distance",x = 'Distance (in km)', y = "Frequency")+
theme_minimal()

# Logistic Regression

mod2<- glm(didclick~ distance + distance_squared+imp_large+cat_entertainment+
cat_social+cat_tech+as_ios+ln_app_review_vol+
app_review_val,family = "binomial", data = data)
summary(mod2)
sig_coeff<-mod2$coefficients[c(2,4,7,8)]
odds<- exp(sig_coeff);odds

### Results from the logistic regression: ###
#Significant factors: 
# + Distance and Distance Squared
# + Large Impressions ("728x90")
# + Category: Technology
# + iOS Devices
# Distance and impression size have negative coefficients. 


# Odds Ratios
#  As distance increases by 1 km, the probability of a customer clicking decreases by ~ 11%. 
#  If the impression is large ("728x90"), then the probability of a customer clicking decreases by ~ 29.7%.
#  If the category is technology, then the probability of a customer clicking increases by ~98.9%.
#  If the device is iOS, the probability of a customer clicking increases by ~47%.

#Conclusions
# For a company or app developer that wants to reach potential customers by advertising impressions, they should focus on customers that are a) within close proximity to the business, b) focus on smaller impressions, c) focus on ads that are in the technology category, and d) for those using an iOS device. 



