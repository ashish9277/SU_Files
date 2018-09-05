
library(data.table)
temp_df <- data.frame()
clean_data <- function(read_file)
{
  df <- fread(read_file , select = c('ROOM_TYPE_CODE_C','LENGTH_OF_STAY_C','Guest_Checkin_Date_H','Length_Stay_H','Guest_Country_H','Gender_H','POV_H','Likelihood_Recommend_H','Overall_Sat_H','Guest_Room_H','Condition_Hotel_H','Customer_SVC_H','Staff_Cared_H','Internet_Sat_H','Check_In_H','F&B_Overall_Experience_H','City_PL','State_PL','Brand_PL','NPS_Type' , 'Tranquility_H' , 'Spa_Used_H' , 'Clublounge_Used_H' , 'Age_Range_H' ))
  
  df <- na.omit(df)
  temp_df <- rbind(temp_df,df)
  return(temp_df)
}

final_df <- clean_data('out-201501.csv')
final_df <- clean_data('data/out-201402.csv')
final_df <- clean_data('data/out-201402.csv')
final_df <- clean_data('data/out-201402.csv')
final_df <- clean_data('data/out-201402.csv')
final_df <- clean_data('data/out-201402.csv')
final_df <- clean_data('data/out-201402.csv')
df_hotel_1 <- fread('out-201501.csv' , select = c('ROOM_TYPE_CODE_C','LENGTH_OF_STAY_C','Guest_Checkin_Date_H','Length_Stay_H','Guest_Country_H','Gender_H','POV_H','Likelihood_Recommend_H','Overall_Sat_H','Guest_Room_H','Condition_Hotel_H','Customer_SVC_H','Staff_Cared_H','Internet_Sat_H','Check_In_H','F&B_Overall_Experience_H','City_PL','State_PL','Brand_PL','NPS_Type' , 'Tranquility_H' , 'Spa_Used_H' , 'Clublounge_Used_H' , 'Age_Range_H' ))


############ Reading other months ##########################
df_hotel_2 <- fread('data/out-201402.csv' , select = c('ROOM_TYPE_CODE_C','LENGTH_OF_STAY_C','Guest_Checkin_Date_H','Length_Stay_H','Guest_Country_H','Gender_H','POV_H','Likelihood_Recommend_H','Overall_Sat_H','Guest_Room_H','Condition_Hotel_H','Customer_SVC_H','Staff_Cared_H','Internet_Sat_H','Check_In_H','F&B_Overall_Experience_H','City_PL','State_PL','Brand_PL','NPS_Type' , 'Tranquility_H' , 'Spa_Used_H' , 'Clublounge_Used_H' , 'Age_Range_H' ))
df_hotel_2 = na.omit(df_hotel_2)
df_hotel_2 <- fread('data/out-201402.csv' , select = c('ROOM_TYPE_CODE_C','LENGTH_OF_STAY_C','Guest_Checkin_Date_H','Length_Stay_H','Guest_Country_H','Gender_H','POV_H','Likelihood_Recommend_H','Overall_Sat_H','Guest_Room_H','Condition_Hotel_H','Customer_SVC_H','Staff_Cared_H','Internet_Sat_H','Check_In_H','F&B_Overall_Experience_H','City_PL','State_PL','Brand_PL','NPS_Type' , 'Tranquility_H' , 'Spa_Used_H' , 'Clublounge_Used_H' , 'Age_Range_H' ))
df_hotel_2 = na.omit(df_hotel_2)
df_hotel_2 <- fread('data/out-201402.csv' , select = c('ROOM_TYPE_CODE_C','LENGTH_OF_STAY_C','Guest_Checkin_Date_H','Length_Stay_H','Guest_Country_H','Gender_H','POV_H','Likelihood_Recommend_H','Overall_Sat_H','Guest_Room_H','Condition_Hotel_H','Customer_SVC_H','Staff_Cared_H','Internet_Sat_H','Check_In_H','F&B_Overall_Experience_H','City_PL','State_PL','Brand_PL','NPS_Type' , 'Tranquility_H' , 'Spa_Used_H' , 'Clublounge_Used_H' , 'Age_Range_H' ))
df_hotel_2 = na.omit(df_hotel_2)
df_hotel_2 <- fread('data/out-201402.csv' , select = c('ROOM_TYPE_CODE_C','LENGTH_OF_STAY_C','Guest_Checkin_Date_H','Length_Stay_H','Guest_Country_H','Gender_H','POV_H','Likelihood_Recommend_H','Overall_Sat_H','Guest_Room_H','Condition_Hotel_H','Customer_SVC_H','Staff_Cared_H','Internet_Sat_H','Check_In_H','F&B_Overall_Experience_H','City_PL','State_PL','Brand_PL','NPS_Type' , 'Tranquility_H' , 'Spa_Used_H' , 'Clublounge_Used_H' , 'Age_Range_H' ))
df_hotel_2 = na.omit(df_hotel_2)
df_hotel_2 <- fread('data/out-201402.csv' , select = c('ROOM_TYPE_CODE_C','LENGTH_OF_STAY_C','Guest_Checkin_Date_H','Length_Stay_H','Guest_Country_H','Gender_H','POV_H','Likelihood_Recommend_H','Overall_Sat_H','Guest_Room_H','Condition_Hotel_H','Customer_SVC_H','Staff_Cared_H','Internet_Sat_H','Check_In_H','F&B_Overall_Experience_H','City_PL','State_PL','Brand_PL','NPS_Type' , 'Tranquility_H' , 'Spa_Used_H' , 'Clublounge_Used_H' , 'Age_Range_H' ))
df_hotel_2 = na.omit(df_hotel_2)







#Checking columns
colnames(df_hotel_1)

#Removing the NA values
df_hotel_1 <- na.omit(df_hotel_1)

View(df_hotel_1)

#Converting the values in the table
df_hotel_1$ROOM_TYPE_CODE_C <- as.factor(df_hotel_1$ROOM_TYPE_CODE_C)
df_hotel_1$LENGTH_OF_STAY_C <- as.numeric(df_hotel_1$Length_Stay_H)
df_hotel_1$Guest_Checkin_Date_H <- as.Date(df_hotel_1$Guest_Checkin_Date_H)
df_hotel_1$Length_Stay_H <- as.numeric(df_hotel_1$Length_Stay_H)
df_hotel_1$Guest_Country_H <- as.factor(df_hotel_1$Guest_Country_H)
df_hotel_1$Gender_H <- as.factor(df_hotel_1$Gender_H)
df_hotel_1$POV_H <- as.factor(df_hotel_1$POV_H)
df_hotel_1$Clublounge_Used_H <- as.factor(df_hotel_1$Clublounge_Used_H)
df_hotel_1$Spa_Used_H <- as.factor(df_hotel_1$Spa_Used_H)
df_hotel_1$Likelihood_Recommend_H <- as.numeric(df_hotel_1$Likelihood_Recommend_H)
df_hotel_1$Overall_Sat_H <- as.numeric(df_hotel_1$Overall_Sat_H)
df_hotel_1$Guest_Room_H <- as.numeric(df_hotel_1$Guest_Room_H)
df_hotel_1$Condition_Hotel_H <- as.numeric(df_hotel_1$Condition_Hotel_H)
df_hotel_1$Staff_Cared_H <- as.numeric(df_hotel_1$Staff_Cared_H)
df_hotel_1$Internet_Sat_H <- as.numeric(df_hotel_1$Internet_Sat_H)
df_hotel_1$Check_In_H <- as.numeric(df_hotel_1$Check_In_H)
df_hotel_1$`F&B_Overall_Experience_H` <- as.numeric(df_hotel_1$`F&B_Overall_Experience_H`)
df_hotel_1$City_PL <- as.factor(df_hotel_1$City_PL)
df_hotel_1$State_PL <- as.factor(df_hotel_1$State_PL)
df_hotel_1$Brand_PL <- as.factor(df_hotel_1$Brand_PL)
df_hotel_1$NPS_Type <- as.factor(df_hotel_1$NPS_Type)
df_hotel_1$Age_Range_H <- as.factor(df_hotel_1$Age_Range_H)
#########################################################
######## Cleanup complete ###############################
##########################################################


#Descriptive analysis
tapply(df_hotel_1$NPS_Type , df_hotel_1$NPS_Type , length)
tapply(df_hotel_1$Gender_H , df_hotel_1$Gender_H , length)
tapply(df_hotel_1$Gender_H , df_hotel_1$Age_Range_H , length)
tapply(df_hotel_1$Gender_H , df_hotel_1$POV_H , length)

barplot(tapply(df_hotel_1$Gender_H , df_hotel_1$State_PL , length))
library(ggplot2)





###########################################################################
###########################################################################
colnames(df_hotel_1)






#Linear Modeling
linear_model <- lm(data = df_hotel_1 , Likelihood_Recommend_H~Condition_Hotel_H+Staff_Cared_H + Customer_SVC_H + Guest_Room_H + Overall_Sat_H + Brand_PL + POV_H + Internet_Sat_H + Age_Range_H +Length_Stay_H + Guest_Country_H + `F&B_Overall_Experience_H` + ROOM_TYPE_CODE_C + City_PL + Gender_H)

summary(linear_model)




##########################################################################
# Analysis part...Need more work here

tapply(df_hotel_1$Likelihood_Recommend_H , df_hotel_1$Tranquility_H , length)


#Splitting the data by the date month and year
datetxt <- as.Date(df_hotel_1$Guest_Checkin_Date_H)
df <- data.frame(date = datetxt,
                 year = as.numeric(format(datetxt, format = "%Y")),
                 month = as.numeric(format(datetxt, format = "%m")),
                 day = as.numeric(format(datetxt, format = "%d")),
                 likelihood = df_hotel_1$Likelihood_Recommend_H)
View(df)

tapply(df$month , df$likelihood , length)

ggplot(df) + geom_bar(aes(x=likelihood) , col='red') 

unique(df$month)

tapply(df$likelihood , df$month , length)




##############################################################################


#Implementing arules
str(df_hotel_1)

library(arulesViz)
library(arules)

mycol<- names(df_hotel_data) %in% c(
  "Likelihood_Recommend_H",
  "Overall_Sat_H",
  "Guest_Room_H",
  "Tranquility_H",
  "Condition_Hotel_H",
  "Customer_SVC_H",
  "Staff_Cared_H",
  "Internet_Sat_H",
  "Check_In_H"
)



##########################################################################
#Adding columns for arules


df_hotel_data <- data.frame(df_hotel_1)

df_survey_data <- df_hotel_data[mycol]

library(kernlab)

survey_LRH6<- df_survey_data[which(df_survey_data$Likelihood_Recommend_H < 6,arr.ind = TRUE),]

survey_LRH6$Likelihood_Recommend_H <- as.factor(survey_LRH6$Likelihood_Recommend_H)
survey_LRH6$Overall_Sat_H <- as.factor(survey_LRH6$Overall_Sat_H)
survey_LRH6$Guest_Room_H <- as.factor(survey_LRH6$Guest_Room_H)
survey_LRH6$Tranquility_H <- as.factor(survey_LRH6$Tranquility_H)
survey_LRH6$Condition_Hotel_H <- as.factor(survey_LRH6$Condition_Hotel_H)
survey_LRH6$Customer_SVC_H <- as.factor(survey_LRH6$Customer_SVC_H)
survey_LRH6$Staff_Cared_H <- as.factor(survey_LRH6$Staff_Cared_H)
survey_LRH6$Internet_Sat_H <- as.factor(survey_LRH6$Internet_Sat_H)
survey_LRH6$Check_In_H <- as.factor(survey_LRH6$Check_In_H)



rule1 <- apriori(survey_LRH6,parameter=list(support=0.05, confidence=0.8))
rule1 <- sort(rule1, decreasing = TRUE, by= "confidence")
inspect(rule1)
plot(rule1)

#####################################################################












df_hotel_data$willRecommend <- as.factor(as.numeric(df_hotel_data$Likelihood_Recommend_H > 7))





#KSVM

library(kernlab)

splitData <- function(dataset){
  randIndex <- sample(1:nrow(dataset))
  cutPoint <- floor(2 * nrow(dataset) / 3)
  trainingData <- dataset[randIndex[1:cutPoint],]
  testingData <- dataset[randIndex[(cutPoint + 1): nrow(dataset)],]
  return(list(trainingData, testingData))
}
colnames(df_hotel_data)
tmp <- splitData(df_hotel_data[,c(-5)])
trainingData <- as.data.frame(tmp[1])
testingData <- as.data.frame(tmp[2])
rm(tmp)

ksvmModel <- ksvm(trainingData[,"willRecommend"] ~ ., data = trainingData, kernel = "rbfdot",kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
ksvmPred <- predict(ksvmModel, testingData, type = "response")
ksvmErr <- sum(testingData[,'willRecommend'] != ksvmPred) * 100 /
  length(ksvmPred)
# Error of 3 percent 


colnames(trainingData)

library(e1071)
svmModel <- svm(trainingData[,"willRecommend"] ~ ., data = trainingData, kernel = "linear", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
svmPred <- predict(svmModel, testingData, type = "response")
svmErr <- sum(testingData[,'willRecommend'] != svmPred) * 100 /
  length(svmPred)


svmErr

##############################################

colnames(trainingData)
colnames(testingData)

nbModel <- naiveBayes(trainingData[,'willRecommend'] ~ ., data = trainingData, kernel = "linear", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
nbPred <- predict(nbModel, testingData, type = "class")
nbErr <- sum(testingData[,'willRecommend'] != nbPred) * 100 /length(nbPred)


colnames(trainingData)

predictors <- colnames(TrainingData)[-19]


svmModel$SV


t(svmModel$coefs) %*% svmModel$SV

#########################################################################

library(ggplot2)
projscattr = ggplot(data = surveydatanew,aes(x=as.numeric(surveydatanew$Likelihood_Recommend_H), y=as.numeric(surveydatanew$Length_Stay_H))) + geom_point(data=surveydatanew, aes(size=Tranquility_H,color= Condition_Hotel_H)) + labs(title = "Factors Affecting Likelihood to Recommend",y="Length of Stay",x="Likelihood To Recommend")






########################################################################
#Maps section here

# create a map of countries colored using the likelihood to recommend
# install the rworldmap and ggplot packages
install.packages("rworldmap")
install.packages("ggplot2")
# load the 2 packages
library("rworldmap")
library("ggplot2")
worldMap <- map_data(map = "world")

surveyData <- df_hotel_data
# first compute the average for each country
countryRating <- as.data.frame(tapply(surveyData$Likelihood_Recommend_H,
                                      surveyData$Guest_Country_H, mean))
colnames(countryRating) <- c("rating")
countryRating$country <- row.names(countryRating)
rownames(countryRating) <- NULL
# change Iran (republic of) to Iran
# and United Kingdom to UK
countryRating$country[countryRating$country == "Iran (islamic Republic Of)"] <- "Iran"
countryRating$country[countryRating$country == "United Kingdom"] <- "UK"
worldMap$rating <- sapply(1:nrow(worldMap), function(i) 
  countryRating$rating[countryRating$country == worldMap$region[i]])
worldMap$rating <- as.numeric(worldMap$rating)

ggplot() + geom_map(data = worldMap, map = worldMap,
                    aes(map_id = region, fill = rating)) +
  scale_fill_gradient(low = "yellow", high = "red", guide = "colorbar") +
  coord_equal() + xlab("") + ylab("")


unique(df_hotel_1$Brand_PL)


ggplot(df_hotel_1) + geom_bar(aes(x=Brand_PL), fill=Likelihood_Recommend_H)


data.m <- melt(df_hotel_1 , id.vars='Brand_PL')
ggplot(data.m, aes(Brand_PL, value)) +   geom_bar(aes(fill = variable), position = "dodge", stat="identity")

head(data.m)





tapply(df_hotel_1$Brand_PL , df_hotel_1$Gender_H , length)



plot(linear_model)

df_hotel_usa <- match(df_hotel_1$State_PL, c('Indiana'))
head(df_hotel_usa)



ggplot(subset(df_hotel_1,NPS_Type=="Promoter" & State_PL!=''),aes(x=State_PL)) +
  
  geom_bar() + ggtitle("State wise Promoter Score") +
  
  labs(x = "State", y = "Promoter Count") + coord_flip()



X <- data.frame(coef(linear_model))
X


df_hotel_Detractors = df_hotel_1[df_hotel_1$NPS_Type=='Detractor']

linear_model_detractors <- lm(data = df_hotel_Detractors , Likelihood_Recommend_H~Condition_Hotel_H+Staff_Cared_H + Customer_SVC_H + Guest_Room_H + Overall_Sat_H + Brand_PL + POV_H + Internet_Sat_H + Age_Range_H +Length_Stay_H + Guest_Country_H + `F&B_Overall_Experience_H` + ROOM_TYPE_CODE_C + City_PL + Gender_H)
summary(linear_model_detractors)


#Important scores for detractors
Overall_Sat_H (2.35e-11)


summary(lm(data=df_hotel_Detractors , Likelihood_Recommend_H~Condition_Hotel_H))

library(data.table)
df_hotel_final <- fread('mydata.csv' , select = c('ROOM_TYPE_CODE_C','LENGTH_OF_STAY_C','Guest_Checkin_Date_H','Length_Stay_H','Guest_Country_H','Gender_H','POV_H','Likelihood_Recommend_H','Overall_Sat_H','Guest_Room_H','Condition_Hotel_H','Customer_SVC_H','Staff_Cared_H','Internet_Sat_H','Check_In_H','F&B_Overall_Experience_H','City_PL','State_PL','Brand_PL','NPS_Type' , 'Tranquility_H' , 'Spa_Used_H' , 'Clublounge_Used_H' , 'Age_Range_H' ))




ggplot(df_hotel_final, aes(x=Tranquility_H , y=Likelihood_Recommend_H))+ geom_bar(stat = 'identity')


tapply(df_hotel_final$Gender_H , df_hotel_final$Gender_H , length)
tapply(df_hotel_final$POV_H , df_hotel_final$POV_H , length)
tapply(df_hotel_final$Age_Range_H , df_hotel_final$Age_Range_H , length)

colnames(df_hotel_final)


df_hotel_final

ggplot(data=df_hotel_final, aes(x=Brand_PL)) + geom_bar() + geom_text(stat='count',aes(label=..count..),vjust=-1)


hist(as.numeric(df_hotel_final$Brand_PL))


library(gridExtra)
test_df <- table(df_hotel_final$LENGTH_OF_STAY_C , df_hotel_final$Likelihood_Recommend_H)
barplot(test_df, main="Car Distribution by Gears and VS",
        xlab="Number of Days Stayed", col=rownames(test_df))

t1<- ggplot(data=df_hotel_final, aes(x=as.numeric(Tranquility_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb")  +
  labs(y="LRH",x="Tranquility")
t1
t2<- ggplot(data=df_hotel_final, aes(x=as.numeric(Condition_Hotel_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb")  +
  labs(y="LRH",x="Condition of Hotel")
t2
t3<- ggplot(data=df_hotel_final, aes(x=as.numeric(Internet_Sat_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb") +
  labs(y="LRH",x="Internet Satisfaction")
t3
t4<- ggplot(data=df_hotel_final, aes(x=as.numeric(Check_In_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb")  +
  labs(y="LRH",x="Check in Procedure")
t4
t5<- ggplot(data=df_hotel_final, aes(x=as.numeric(Customer_SVC_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb") +
  labs(y="LRH",x="Customer Service")
t5
t6<- ggplot(data=df_hotel_final, aes(x=as.numeric(Staff_Cared_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se",color="blue", fill="#87ceeb") +
  labs(y="LRH",x="Staff_Cared_H")
t6
grid.arrange(t1,t2,t3,t4,t5,t6)



df_test <- fread('out-201501.csv' , select = c('Pool-Indoor_PL','Spa_PL','Business Center_PL','Golf_PL','Limo Service_PL	Mini-Bar_PL','Fitness Center_PL','Likelihood_Recommend_H'))


t1<- ggplot(data=df_hotel_final, aes(x=as.numeric(Tranquility_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb")  +
  labs(y="LRH",x="Tranquility")
t1
t2<- ggplot(data=df_hotel_final, aes(x=as.numeric(Condition_Hotel_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb")  +
  labs(y="LRH",x="Condition of Hotel")
t2
t3<- ggplot(data=df_hotel_final, aes(x=as.numeric(Internet_Sat_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb") +
  labs(y="LRH",x="Internet Satisfaction")
t3
t4<- ggplot(data=df_hotel_final, aes(x=as.numeric(Check_In_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb")  +
  labs(y="LRH",x="Check in Procedure")
t4
t5<- ggplot(data=df_hotel_final, aes(x=as.numeric(Customer_SVC_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb") +
  labs(y="LRH",x="Customer Service")
t5
t6<- ggplot(data=df_hotel_final, aes(x=as.numeric(Staff_Cared_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se",color="blue", fill="#87ceeb") +
  labs(y="LRH",x="Staff_Cared_H")
t6
grid.arrange(t1,t2,t3,t4,t5,t6)



#######################################
#### Checking useless amenitites
########################################

df_test <- fread('out-201501.csv' , select = c('Pool-Indoor_PL','Spa_PL','Business Center_PL','Golf_PL','Limo Service_PL	Mini-Bar_PL','Fitness Center_PL','Likelihood_Recommend_H'))

df_test <- na.omit(df_test)



colnames(df_test)
t7<- ggplot(data=(df_test[df_test$`Pool-Indoor_PL`!='']), aes(x=`Pool-Indoor_PL`,y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb" ,  width = 0.3)  +
  labs(y="LRH",x="Pool-Indoor")
t7

t8<- ggplot(data=df_test[df_test$Spa_PL!=''], aes(x=Spa_PL,y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb" , width = 0.3)  +
  labs(y="LRH",x="Spa")
t8

t9<- ggplot(data=df_test[df_test$`Business Center_PL`!=''], aes(x=`Business Center_PL`,y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb" , width = 0.3) +
  labs(y="LRH",x="Business Center")
t9

t10<- ggplot(data=df_test[df_test$Golf_PL!=''], aes(x=Golf_PL,y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb" , width = 0.3)  +
  labs(y="LRH",x="Golf")
t10

grid.arrange(t7,t8,t9,t10)

 

#################################################################







install.packages("rworldmap")
install.packages("ggplot2")
# load the 2 packages
library("rworldmap")
library("ggplot2")
worldMap <- map_data(map = "world")

SurveyData <- df_hotel_final
# first compute the average for each country
countryRating <- as.data.frame(tapply(SurveyData$Likelihood_Recommend_H,
                                      SurveyData$Guest_Country_H, mean))
colnames(countryRating) <- c("rating")
countryRating$country <- row.names(countryRating)
rownames(countryRating) <- NULL
# change Iran (republic of) to Iran
# and United Kingdom to UK
countryRating$country[countryRating$country == "Iran (islamic Republic Of)"] <- "Iran"
countryRating$country[countryRating$country == "United Kingdom"] <- "UK"
worldMap$rating <- sapply(1:nrow(worldMap), function(i) 
  countryRating$rating[countryRating$country == worldMap$region[i]])
worldMap$rating <- as.numeric(worldMap$rating)

ggplot() + geom_map(data = worldMap, map = worldMap,
                    aes(map_id = region, x = long, y = lat, fill = rating)) +
  scale_fill_gradient(low = "yellow", high = "red", guide = "colorbar") +
  coord_equal() + xlab("") + ylab("")




