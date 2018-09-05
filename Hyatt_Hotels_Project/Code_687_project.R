
#cleaning the environment
rm(list=ls())

# Collecting the data
library(data.table)
temp_df <- data.frame()
clean_data <- function(read_file)
{
  df <- fread(read_file , select = c('ROOM_TYPE_CODE_C','LENGTH_OF_STAY_C','Guest_Checkin_Date_H','Length_Stay_H','Guest_Country_H','Gender_H','POV_H','Likelihood_Recommend_H','Overall_Sat_H','Guest_Room_H','Condition_Hotel_H','Customer_SVC_H','Staff_Cared_H','Internet_Sat_H','Check_In_H','F&B_Overall_Experience_H','City_PL','State_PL','Brand_PL','NPS_Type' , 'Tranquility_H' , 'Spa_Used_H' , 'Clublounge_Used_H' , 'Age_Range_H' ))
  
  df <- na.omit(df)
  temp_df <- rbind(temp_df,df)
  return(temp_df)
}

#Reading the files
final_df <- clean_data('data/out-201501.csv')
final_df <- clean_data('data/out-201402.csv')
final_df <- clean_data('data/out-201403.csv')
final_df <- clean_data('data/out-201404.csv')
final_df <- clean_data('data/out-201405.csv')
final_df <- clean_data('data/out-201406.csv')

final_df <- clean_data('data/out-201407.csv')
final_df <- clean_data('data/out-201408.csv')
final_df <- clean_data('data/out-201409.csv')
final_df <- clean_data('data/out-201410.csv')
final_df <- clean_data('data/out-201411.csv')
final_df <- clean_data('data/out-201412.csv')

#Saving a copy for the backup
df_hotel_final <- final_df
#Checking columns
colnames(df_hotel_final)



#Converting the values in the table
df_hotel_final$ROOM_TYPE_CODE_C <- as.factor(df_hotel_final$ROOM_TYPE_CODE_C)
df_hotel_final$LENGTH_OF_STAY_C <- as.numeric(df_hotel_final$Length_Stay_H)
df_hotel_final$Guest_Checkin_Date_H <- as.Date(df_hotel_final$Guest_Checkin_Date_H)
df_hotel_final$Length_Stay_H <- as.numeric(df_hotel_final$Length_Stay_H)
df_hotel_final$Guest_Country_H <- as.factor(df_hotel_final$Guest_Country_H)
df_hotel_final$Gender_H <- as.factor(df_hotel_final$Gender_H)
df_hotel_final$POV_H <- as.factor(df_hotel_final$POV_H)
df_hotel_final$Clublounge_Used_H <- as.factor(df_hotel_final$Clublounge_Used_H)
df_hotel_final$Spa_Used_H <- as.factor(df_hotel_final$Spa_Used_H)
df_hotel_final$Likelihood_Recommend_H <- as.numeric(df_hotel_final$Likelihood_Recommend_H)
df_hotel_final$Overall_Sat_H <- as.numeric(df_hotel_final$Overall_Sat_H)
df_hotel_final$Guest_Room_H <- as.numeric(df_hotel_final$Guest_Room_H)
df_hotel_final$Condition_Hotel_H <- as.numeric(df_hotel_final$Condition_Hotel_H)
df_hotel_final$Staff_Cared_H <- as.numeric(df_hotel_final$Staff_Cared_H)
df_hotel_final$Internet_Sat_H <- as.numeric(df_hotel_final$Internet_Sat_H)
df_hotel_final$Check_In_H <- as.numeric(df_hotel_final$Check_In_H)
df_hotel_final$`F&B_Overall_Experience_H` <- as.numeric(df_hotel_final$`F&B_Overall_Experience_H`)
df_hotel_final$City_PL <- as.factor(df_hotel_final$City_PL)
df_hotel_final$State_PL <- as.factor(df_hotel_final$State_PL)
df_hotel_final$Brand_PL <- as.factor(df_hotel_final$Brand_PL)
df_hotel_final$NPS_Type <- as.factor(df_hotel_final$NPS_Type)
df_hotel_final$Age_Range_H <- as.factor(df_hotel_final$Age_Range_H)


#######################################################################
####### Data Cleanup complete ########################################

#Descriptive Analysis

#Checking the counts for NPS Type, Gender, Age-Range, etc.
tapply(df_hotel_final$NPS_Type , df_hotel_final$NPS_Type , length)
tapply(df_hotel_final$Gender_H , df_hotel_final$Gender_H , length)
tapply(df_hotel_final$Age_Range_H , df_hotel_final$Age_Range_H , length)
tapply(df_hotel_final$LENGTH_OF_STAY_C , df_hotel_final$LENGTH_OF_STAY_C , length)
barplot(tapply(df_hotel_final$Gender_H , df_hotel_final$State_PL , length))

#Checking according to region


#computing the average rating country-wise
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
worldMap$rating <- sapply(1:nrow(worldMap), function(i) 
  countryRating$rating[countryRating$country == worldMap$region[i]])
worldMap$rating <- as.numeric(worldMap$rating)


#Generating boxplots for countries whose guests have given lower ratings
#Getting the values for which rating is less than 7
countryRating_less <- countryRating[countryRating$rating<7]
ggplot(countryRating, aes(x = country, y = rating)) + geom_boxplot(fill='blue' , outcol="red")


#Generating the world map based on the ratings. Yellow represents low ratings and red represents high ratings.
ggplot() + geom_map(data = worldMap, map = worldMap,
                    aes(map_id = region, x = long, y = lat, fill = rating)) +
  scale_fill_gradient(low = "yellow", high = "red", guide = "colorbar") +
  coord_equal() + xlab("") + ylab("")


#Checking the barchart for brands
ggplot() + geom_bar(data = df_hotel_final , aes(x=Brand_PL))

#Generating stack barchart for brands
data.m <- melt(df_hotel_final , id.vars='Brand_PL')
ggplot(data.m, aes(Brand_PL, value)) +   geom_bar(aes(fill = variable), position = "dodge", stat="identity")

#Getting business users
df_hotel_business <- df_hotel_final[df_hotel_final$POV_H=='Business']


# Amenities plots
library(ggplot2)
t1<- ggplot(data=df_hotel_business, aes(x=as.numeric(Tranquility_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb")  +
  labs(y="LRH",x="Tranquility")
t1
t2<- ggplot(data=df_hotel_business, aes(x=as.numeric(Condition_Hotel_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb")  +
  labs(y="LRH",x="Condition of Hotel")
t2
t3<- ggplot(data=df_hotel_business, aes(x=as.numeric(Internet_Sat_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb") +
  labs(y="LRH",x="Internet Satisfaction")
t3
t4<- ggplot(data=df_hotel_business, aes(x=as.numeric(Check_In_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb")  +
  labs(y="LRH",x="Check in Procedure")
t4
t5<- ggplot(data=df_hotel_business, aes(x=as.numeric(Customer_SVC_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb") +
  labs(y="LRH",x="Customer Service")
t5
t6<- ggplot(data=df_hotel_business, aes(x=as.numeric(Staff_Cared_H),y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se",color="blue", fill="#87ceeb") +
  labs(y="LRH",x="Staff_Cared_H")
t6
library(gridExtra)
grid.arrange(t1,t2,t3,t4,t5,t6)

colnames(df_hotel_business)
#Reading data for extra amenities
amenities_data <- function(read_file)
{
  df <- fread(read_file , select = c('Pool-Indoor_PL','Spa_PL','Business Center_PL','Golf_PL','Limo Service_PL	Mini-Bar_PL','Fitness Center_PL','Likelihood_Recommend_H','POV_H'))
  
  df <- na.omit(df)
  temp_df <- rbind(temp_df,df)
  return(temp_df)
}

df <- amenities_data('data/out-201501.csv')
df <- amenities_data('data/out-201402.csv')
df <- amenities_data('data/out-201403.csv')
df <- amenities_data('data/out-201404.csv')
df <- amenities_data('data/out-201405.csv')
df <- amenities_data('data/out-201406.csv')
df <- amenities_data('data/out-201407.csv')
df <- amenities_data('data/out-201408.csv')
df <- amenities_data('data/out-201409.csv')
df <- amenities_data('data/out-201410.csv')
df <- amenities_data('data/out-201411.csv')
df <- amenities_data('data/out-201412.csv')

#Checking extra amenities which business users might not use at all.

#Getting business users
df_amenities_business <- df[df$POV_H=='Business']
t7<- ggplot(data=(df_amenities_business[df_amenities_business$`Pool-Indoor_PL`!='']), aes(x=`Pool-Indoor_PL`,y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb" ,  width = 0.3)  +
  labs(y="LRH",x="Pool-Indoor")
t7

t8<- ggplot(data=df_amenities_business[df_amenities_business$Spa_PL!=''], aes(x=Spa_PL,y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb" , width = 0.3)  +
  labs(y="LRH",x="Spa")
t8

t9<- ggplot(data=df_amenities_business[df_amenities_business$`Business Center_PL`!=''], aes(x=`Business Center_PL`,y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb" , width = 0.3) +
  labs(y="LRH",x="Business Center")
t9

t10<- ggplot(data=df_amenities_business[df_amenities_business$Golf_PL!=''], aes(x=Golf_PL,y=Likelihood_Recommend_H)) +
  geom_bar(stat="summary",fun.data="mean_se", color="blue", fill="#87ceeb" , width = 0.3)  +
  labs(y="LRH",x="Golf")
t10

grid.arrange(t7,t8,t9,t10)


#Modeling the data
#We will be checking which factors are contibuting more towards detractors
df_busines_detractors <- df_hotel_business[df_hotel_business$NPS_Type=="Detractor"]

#Linear Model
linear_model_detractors <- lm(data = df_busines_detractors , Likelihood_Recommend_H~Condition_Hotel_H + Staff_Cared_H + Customer_SVC_H + Guest_Room_H + Overall_Sat_H + Brand_PL  + Internet_Sat_H  + Age_Range_H +Length_Stay_H  + Guest_Country_H + `F&B_Overall_Experience_H`  + ROOM_TYPE_CODE_C + City_PL + Gender_H)
summary(linear_model_detractors)

#SVM Model

df_hotel_business$willRecommend <- as.factor(as.numeric(df_hotel_business$Likelihood_Recommend_H > 7))
library(kernlab)

splitData <- function(dataset){
  randIndex <- sample(1:nrow(dataset))
  cutPoint <- floor(2 * nrow(dataset) / 3)
  trainingData <- dataset[randIndex[1:cutPoint],]
  testingData <- dataset[randIndex[(cutPoint + 1): nrow(dataset)],]
  return(list(trainingData, testingData))
}
colnames(df_hotel_business)
df_hotel_business$LENGTH_OF_STAY_C <- as.numeric(df_hotel_business$LENGTH_OF_STAY_C)
tmp <- splitData(df_hotel_business[c(-1,-2)])
trainingData <- as.data.frame(tmp[1])
testingData <- as.data.frame(tmp[2])
rm(tmp)

trainingData <- trainingData[c(-1)]
testingData <- testingData[c(-1)]
library(kernlab)
colnames(df_hotel_business)

df_hotel_business_1 <- df_hotel_business[c(-1,-2)]
ksvmModel <- ksvm(df_hotel_business_1[,"willRecommend"] ~ ., data = trainingData, kernel = "rbfdot",kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
ksvmPred <- predict(ksvmModel, testingData, type = "response")
ksvmErr <- sum(testingData[,'willRecommend'] != ksvmPred) * 100 /
  length(ksvmPred)

#Error of 11 percent 

library(e1071)
svmModel <- svm(trainingData[,"willRecommend"] ~ ., data = trainingData, kernel = "linear", kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
svmPred <- predict(svmModel, testingData, type = "response")
svmErr <- sum(testingData[,'willRecommend'] != svmPred) * 100 /
  length(svmPred)
#svmError of 9 percent

#Naive Bayes
nbModel <- naiveBayes(trainingData[,'willRecommend'] ~ ., data = trainingData, kernel = "linear",
                      kpar = "automatic", C = 5, cross = 3, prob.model = TRUE)
nbPred <- predict(nbModel, testingData, type = "class")
nbErr <- sum(testingData[,'willRecommend'] != nbPred) * 100 /
  length(nbPred)


#Arules
library(arules)
library(arulesViz)

mycol<- names(df_hotel_business) %in% c(
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
df_survey_data <- df_hotel_business[mycol]
colnames(df_survey_data)
library(kernlab)

survey_LRH6 <- df_survey_data[which(df_survey_data$Likelihood_Recommend_H < 6,arr.ind = TRUE),]

survey_LRH6$Likelihood_Recommend_H <- as.factor(survey_LRH6$Likelihood_Recommend_H)
survey_LRH6$Overall_Sat_H <- as.factor(survey_LRH6$Overall_Sat_H)
survey_LRH6$Guest_Room_H <- as.factor(survey_LRH6$Guest_Room_H)
survey_LRH6$Tranquility_H <- as.factor(survey_LRH6$Tranquility_H)
survey_LRH6$Condition_Hotel_H <- as.factor(survey_LRH6$Condition_Hotel_H)
survey_LRH6$Customer_SVC_H <- as.factor(survey_LRH6$Customer_SVC_H)
survey_LRH6$Staff_Cared_H <- as.factor(survey_LRH6$Staff_Cared_H)
survey_LRH6$Internet_Sat_H <- as.factor(survey_LRH6$Internet_Sat_H)
survey_LRH6$Check_In_H <- as.factor(survey_LRH6$Check_In_H)

colnames(survey_LRH6)
rule1 <- apriori(survey_LRH6,parameter=list(support=0.05, confidence=0.8))
rule1 <- sort(rule1, decreasing = TRUE, by= "confidence")
inspect(rule1)
#> inspect(rule1)
#lhs                                          rhs                   support    confidence lift    
#[1] {Customer_SVC_H=10}                       => {Check_In_H=10}       0.06341463 1.0000000  5.125000
#[2] {Guest_Room_H=9,Check_In_H=9}             => {Condition_Hotel_H=9} 0.06341463 1.0000000  6.406250
#[3] {Customer_SVC_H=9,Staff_Cared_H=9}        => {Check_In_H=9}        0.05853659 0.9230769  3.710407
#[4] {Guest_Room_H=9,Tranquility_H=9}          => {Condition_Hotel_H=9} 0.06341463 0.8666667  5.552083
#[5] {Tranquility_H=9,Condition_Hotel_H=9}     => {Guest_Room_H=9}      0.06341463 0.8666667  7.106667
#[6] {Staff_Cared_H=9,Check_In_H=9}            => {Customer_SVC_H=9}    0.05853659 0.8571429  6.758242
#[7] {Likelihood_Recommend_H=5,Guest_Room_H=9} => {Condition_Hotel_H=9} 0.06341463 0.8125000  5.205078
#[8] {Guest_Room_H=9}                          => {Condition_Hotel_H=9} 0.09756098 0.8000000  5.125000
#Plotting the rule1
plot(rule1)




