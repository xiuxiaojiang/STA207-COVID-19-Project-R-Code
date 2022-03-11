library(countrycode)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(MASS)
library(car)
global_data <- read.csv("C:/Users/xiuxi/OneDrive/Desktop/School/Winter 2022/STA 207/Final project/WHO-COVID-19-global-data.csv")
data_new<-global_data
names(data_new)[names(data_new) == "ï..Date_reported"] <- "Date"
data_new$Date<- as.Date(data_new$Date)                                    
# Usual area chart
p <- data_new %>%
  ggplot( aes(x=Date, y=New_cases)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("New cases") +
  theme_ipsum()
p <- ggplotly(p)
p
vac.data<- read.csv("C:/Users/xiuxi/OneDrive/Desktop/School/Winter 2022/STA 207/Final project/vaccination-data.csv")
vac<-na.omit(vac.data)
cor<-cor(vac$PERSONS_VACCINATED_1PLUS_DOSE_PER100,vac$PERSONS_FULLY_VACCINATED_PER100)
plot(vac$PERSONS_VACCINATED_1PLUS_DOSE_PER100,vac$PERSONS_FULLY_VACCINATED_PER100,pch = 15, col = "blue",main="One-plus dose vaccinated vs. fully vaccinated",xlab="One-plus dose vaccinated",ylab="Fully vaccinated",)
## highly correlated, can just use one variable instead using both
names(vac.data)[names(vac.data) == "COUNTRY"] <- "country"
vac.data1<- vac.data[order(vac.data$country),]
vac.data2<-vac.data1[,c("country","PERSONS_VACCINATED_1PLUS_DOSE_PER100","PERSONS_FULLY_VACCINATED_PER100")]
colnames(vac.data2) <- c('country','VR_1PLUS','VR_FULL')
##create a death rate dataset for Delta which from 5/11/2021-11/23/2021 by country.
delta<-global_data[global_data$ï..Date_reported >= "2021-05-11"& global_data$ï..Date_reported <= "2021-11-23",]
delta1<-delta[,c("Country_code","New_cases","New_deaths")]
delta1<-na.omit(delta1)
delta2<-tapply(delta1$New_cases,delta1$Country_code,sum)
delta3<-tapply(delta1$New_deaths,delta1$Country_code,sum)
death_rate<-delta3/delta2
delta4<-as.data.frame(death_rate)
delta4$country_code <- row.names(delta4)
rownames(delta4) <- NULL
del<-rep("Delta",times=236)
delta4$variant<- del
codes <- delta4$country_code
delta4$country_code<-countrycode(codes, origin = "iso2c", destination = "iso3c")
delta5<-na.omit(delta4)
##create a death rate dataset for Omicron which from 11/24/2021-today by country
omic<-global_data[global_data$ï..Date_reported >= "2021-11-24",]
omic1<-omic[,c("Country_code","New_cases","New_deaths")]
omic1<-na.omit(omic1)
omic2<-tapply(omic1$New_cases,omic1$Country_code,sum)
omic3<-tapply(omic1$New_deaths,omic1$Country_code,sum)
death_rate1<-omic3/omic2
omic4<-as.data.frame(death_rate1)
omic4$country_code <- row.names(omic4)
rownames(omic4) <- NULL
omi<-rep("Omicron",times=236)
omic4$variant<- omi
names(omic4)[names(omic4) == "death_rate1"] <- "death_rate"
codes2 <- omic4$country_code
omic4$country_code<-countrycode(codes2, origin = "iso2c", destination = "iso3c")
omic5<-na.omit(omic4)
## the vaccination rate for Delta
covid.data <- read.csv("C:/Users/xiuxi/OneDrive/Desktop/School/Winter 2022/STA 207/Final project/owid-covid-data.csv")
covid.data1<-covid.data[covid.data$date <= "2021-11-09",]
covid.data2<-covid.data1[,c("iso_code","date","people_fully_vaccinated","population")]
covid.data3<-na.omit(covid.data2)
fv<-tapply(covid.data3$people_fully_vaccinated, covid.data3$iso_code, max)
pop<-tapply(covid.data3$population, covid.data3$iso_code, mean)
v_rate<-fv/pop
v_rate<-as.data.frame(v_rate)
v_rate$country_code <- row.names(v_rate)
rownames(v_rate) <- NULL
v_rate$v_rate<-100*v_rate$v_rate
v1<-v_rate[v_rate$v_rate >= 0 & v_rate$v_rate <= 20,]
v2<-v_rate[v_rate$v_rate >20 & v_rate$v_rate <= 40,]
v3<-v_rate[v_rate$v_rate >40 & v_rate$v_rate <= 60,]
v4<-v_rate[v_rate$v_rate >60 & v_rate$v_rate <= 80,]
v5<-v_rate[v_rate$v_rate >80,]
vv1<-rep("0-20%",times=47)
v1$v_level<- vv1
vv2<-rep("21-40%",times=32)
v2$v_level<- vv2
vv3<-rep("41-60%",times=48)
v3$v_level<- vv3
vv4<-rep("61-80%",times=81)
v4$v_level<- vv4
vv5<-rep("80%+",times=17)
v5$v_level<- vv5
v_rate1<-rbind(v1,v2,v3,v4,v5)
##v_rate1 is the vaccination rate for Delta
##v_r1 is the vaccination rate for Omicron
cd1<-covid.data[,c("iso_code","date","people_fully_vaccinated","population")]
cd2<-na.omit(cd1)
fv1<-tapply(cd2$people_fully_vaccinated, cd2$iso_code, max)
pop1<-tapply(cd2$population, cd2$iso_code, mean)
v_r<-fv1/pop1
v_r<-as.data.frame(v_r)
v_r$country_code <- row.names(v_r)
rownames(v_r) <- NULL
v_r$v_r<-100*v_r$v_r
names(v_r)[names(v_r) == "v_r"] <- "v_rate"
w1<-v_r[v_r$v_rate >= 0 & v_r$v_rate <= 20,]
w2<-v_r[v_r$v_rate >20 & v_r$v_rate <= 40,]
w3<-v_r[v_r$v_rate >40 & v_r$v_rate <= 60,]
w4<-v_r[v_r$v_rate >60 & v_r$v_rate <= 80,]
w5<-v_r[v_r$v_rate >80,]
wv1<-rep("0-20%",times=46)
w1$v_level<- wv1
wv2<-rep("21-40%",times=32)
w2$v_level<- wv2
wv3<-rep("41-60%",times=51)
w3$v_level<- wv3
wv4<-rep("61-80%",times=83)
w4$v_level<- wv4
wv5<-rep("80%+",times=18)
w5$v_level<- wv5
v_r1<-rbind(w1,w2,w3,w4,w5)
##v_r1 is the vaccination rate for Omicron
##delta data
dd <- merge(delta5,v_rate1, by="country_code",all.x=T)
##Omicron data
od<-merge(omic5,v_r1, by="country_code",all.x=T)
## master data
master<-rbind(dd,od)
master$variant<-as.factor(master$variant)
master$v_level<-as.factor(master$v_level)
summary(master[,c(-1,-4)])
master<-na.omit(master)
summary(master[,c(-1,-4)])
table(master$variant,master$v_level)
hist(master$death_rate,
     main="Death Rate by Country",
     xlab="Death Rate",
     xlim=c(0.00,0.20),
     col="darkmagenta",
     freq=FALSE
)
par(mfrow = c(1, 2))
boxplot(master$death_rate ~ master$variant,
        col='steelblue',
        main='Death Rate by Variants',
        xlab='Variants',
        ylab='Death Rate') 
boxplot(master$death_rate ~ master$v_level,
        col='pink',
        main='Death Rate by Vaccination Level',
        xlab='Vaccination Level',
        ylab='Death Rate') 
my.bp <-ggplot(data=master, aes(y= death_rate, x=variant, fill=v_level ) ) 
my.bp <- my.bp + geom_boxplot() 
my.bp <- my.bp + ggtitle("Death rate by variants") 
my.bp <- my.bp +  ylab("Death rate") + xlab("Variants") 
my.bp
interaction.plot(master$variant, master$v_level, master$death_rate
                 ,cex.lab=1.5,ylab="Death rate",xlab='Variant : Vaccination level',trace.label = "Vaccination level",
                 col = c("red", "yellow","blue","green","black",lyt=1,lwd=3), main="Interaction Plot")
new_death_rate<-(master$death_rate^0.3030303-1)/0.3030303
master$ndr<-new_death_rate
anova.fit2<-aov(lm(ndr~variant+v_level+variant*v_level, data=master))
Anova(anova.fit2, type="III")
anova.fit<-aov(lm(ndr~variant+v_level, data=master))
Anova(anova.fit, type="III")
#Tukey method
tkm<-TukeyHSD(anova.fit, conf.level=.95)
tkm$variant
## Outlying Y observations
fit<-lm(death_rate~variant+v_rate, data=master)
## studentized deleted residuals
stu.res.del <- studres(fit)
head(sort(abs(stu.res.del), decreasing = TRUE))
##Bonferroni's threshold
bt<-qt(1-.1/(2*402), 402-3-1)
## Outlying X observations
h <- influence(fit)$hat
p <- 3
sort(h[which(h > 2 * p/402)], decreasing = TRUE)
## Cook's distance
res <- fit$residuals
mse <- anova(fit)["Residuals", 3]
cook.d <- res^2 * h/(p * mse * (1 - h)^2)
cd<-sort(cook.d[which(cook.d > 1)], decreasing = TRUE)
plot(fit, which=4)
par(mfrow = c(1, 2))
death_rate_p<-master$death_rate+0.0000001 ## add a constant to make all value positive
bc<-boxcox(death_rate_p~variant+v_level, data = master)
lambda <- bc$x[which.max(bc$y)]
new_death_rate<-(master$death_rate^lambda-1)/lambda
par(mfrow = c(1, 2))
hist(master$death_rate,
     main="Death Rate by Country",
     xlab="Death Rate",
     xlim=c(0.00,0.20),
     col="darkmagenta",
     freq=FALSE
)
hist(new_death_rate,
     main="Histogram of Transformed Data ",
     xlab="Transformed Death Rate",
     col="darkmagenta",
     freq=FALSE
)
master$ndr<-new_death_rate
fit2<-lm(ndr~variant+v_level,data=master)
durbinWatsonTest(fit2)
print("Levene test using original data")
result10 <- leveneTest(death_rate ~ interaction(variant, v_level), 
                       data = master)
print(result10)
print("Levene test using transformed data")
result <- leveneTest(ndr ~ interaction(variant, v_level), 
                     data = master)
print(result) 
par(mfrow = c(2, 2))
plot(anova.fit)

