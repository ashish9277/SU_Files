


library(data.table)


df_hotel <- fread('out-201501.csv' , select = c('ROOM_TYPE_CODE_C','LENGTH_OF_STAY_C','Guest_Checkin_Date_H','Length_Stay_H','Guest_Country_H','Gender_H',',POV_H','Likelihood_Recommend_H','Overall_Sat_H','Guest_Room_H','Condition_Hotel_H','Customer_SVC_H','Staff_Cared_H','Internet_Sat_H','Check_In_H','F&B_Overall_Experience_H','City_PL','State_PL','Brand_PL','NPS_Type'))



#converting all the variables as factor...no time series data

df_hotel1 = na.omit(df_hotel)

sapply(df_hotel1, function(x) as.factor(x))

df_hotel1 <- lapply(df_hotel1, factor)

df_hotel1 <- as.data.frame(df_hotel)



df_hotel1$Guest_Checkin_Date_H = as.Date(df_hotel1$Guest_Checkin_Date_H)

df_hotel1$NPS_Type <- as.factor(df_hotel1$NPS_Type)
df_hotel1$State_PL <- as.factor(df_hotel1$State_PL)
df_hotel1$City_PL <- as.factor(df_hotel1$City_PL)
df_hotel1$Gender_H <- as.factor(df_hotel1$Gender_H)

str(df_hotel1)

View(df_hotel1)
df_hotel1[df_hotel1$State_PL!='',]
levels(df_hotel1$State_PL)

max(df_hotel$Internet_Sat_H)

#Checking for USA hotels

df_hotel_usa <- df_hotel1[df_hotel1$State_PL!='',]

df_hotel$Internet_Sat_H

hist(as.numeric(df_hotel_usa$Staff_Cared_H))

colnames(df_hotel1)
library(ggplot2)


ggplot(df_hotel_usa, aes(x=State_PL)) + geom_bar()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

levels(df_hotel_usa$LENGTH_OF_STAY_C)

ggplot(df_hotel_usa , aes(x=df_hotel_usa$LENGTH_OF_STAY_C )) + geom_bar()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


max(df_hotel_usa$LENGTH_OF_STAY_C , na.rm = TRUE)


levels(df_hotel_usa$NPS_Type)
levels(df_hotel_usa$NPS_Type) <- c(0,1,2)


str(df_hotel_usa$NPS_Type)

length(df_hotel_usa$NPS_Type)

#barplot for checking no of promoters, passive and detractors
ggplot(df_hotel_usa , aes(x=df_hotel_usa$NPS_Type)) + geom_bar()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

df_hotels_usa_detractors = df_hotel_usa[df_hotel_usa$NPS_Type=='0']

df_hotel_usa_promoters <- df_hotel_usa[df_hotel_usa$NPS_Type=='2']

#Making a model using 4586 parameters

View(df_hotel_usa_promoters)


str(df_hotel_usa_promoters$Condition_Hotel_H)

df_hotel_usa_final = df_hotel_usa_promoters
#'' - 0 , F-1 , M-2 , Prefer not to answer -3
levels(df_hotel_usa_final$Gender_H) <- c(0,1,2,3)

str(df_hotel_usa_promoters)
df_hotel_usa_final$Gender_H <- as.factor(df_hotel_usa_final$Gender_H)
sum(df_hotel_usa_final$Gender_H=='')

df_hotel_usa_final$Guest_Checkin_Date_H <- as.Date(df_hotel_usa_final$Guest_Checkin_Date_H , '%Y-%m-%d')

View(df_hotel_usa_final)
colnames(df_hotel_usa_final)

df_hotel_usa_final$Check_In_H <-as.numeric(df_hotel_usa_final$Check_In_H)

head(df_hotel_usa_final$Guest_Checkin_Date_H)
df_hotel_usa_final$Guest_Checkin_Date_H <- as.Date(df_hotel_usa_final$Guest_Checkin_Date_H,'%y-%m-%d')

df_hotel_usa_final$Length_Stay_H <- as.numeric(df_hotel_usa_final$Length_Stay_H)

df_hotel_usa_final$Likelihood_Recommend_H = as.numeric(df_hotel_usa_final$Likelihood_Recommend_H)

levels(df_hotel_usa_final$NPS_Type)

df_hotel_usa_final$NPS_Type = as.numeric(df_hotel_usa_final$NPS_Type)



colnames(df_hotel_usa_final$`F&B_Overall_Experience_H`)
colnames(df_hotel_usa_final)
linear_model <- lm(data = df_hotel_usa_final, Likelihood_Recommend_H~Guest_Checkin_Date_H + Gender_H + NPS_Type + Guest_Room_H , Condition_Hotel_H+ Customer_SVC_H + Staff_Cared_H + df_hotel_usa_final`$F&B_Overall_Experience_H`)
summary(linear_model)

df_hotel_usa_final$Likelihood_Recommend_H = as.factor(df_hotel_usa_final$Likelihood_Recommend_H)
str(df_hotel_usa_final)


plot(linear_model)

##############################################################################
##############################################################################
##############  Linear Regression complete with 89.7% ########################
##############################################################################
##############################################################################

#KNN in R

library(class)



max(df_hotel_usa_final$Length_Stay_H)

count(df_hotel_usa$State_PL)
str(df_hotel_usa)
colnames(df_hotel)
df_hotel <- as.factor(df_hotel)
df_hotel$ROOM_TYPE_CODE_C <- as.factor(df_hotel$ROOM_TYPE_CODE_C)
levels(df_hotel$ROOM_TYPE_CODE_C)

df_new$Test= as.factor(df_new$STATUS_CALCULATION_R)
levels(df_new$Test)

#confirmed


#confirmed 
df_confirmed <- df_new[df_new$Test=='Confirmed',]

help("memory.size")
memory.size(TRUE)

memory.size(max = TRUE)

sum(is.na(df_new))


max(df_new$Age_Range_H)
?read.csv
sum(is.na(df))
colnames(df)

levels(df$City_PL)

levels(df$Length_Stay_H)

sapply(df, function(x)all(is.na(x)))

levels(df$Hotel.Inventory_PL)


na.omit(df[df$NUMBER_OF_ROOMS_C==1,"Spa_PL"])

df$NPS
library(e1071)

hist(df$NPS_Type)

as.numeric(as.character(df$NPS_Type))

levels(df$GUEST_COUNTRY_R)
na.omit(as.numeric(as.character(x)))

df[df$NPS_Type=="", "NPS_Type"]

rm(df[df$NPS_Type=="", ])


factor(df$NPS_Type)

levels(df$NPS_Type)

colnames(df)


levels(df$Elevators_P)

levels(df$Restaurant_PL)
factor(df$Restaurant_PL)


reg <- lm(df$NPS_Type~df$Restaurant_PL)


summary(reg)


plot(reg)



df$NPS_VALUE <- factor(df$NPS_Type)



df$NPS_VALUE



str(df$NPS_Type)



levels(df$NPS_Type)


is.na(df$NPS_Type)



factor(df$NPS_Type)

as.factor(df$NPS_Type)

contrasts(as.factor(df$NPS_Type))

df[df$NPS_Type=='Promoter',]

as.factor(levels(df$NPS_Type))

as.numeric(levels(df$NPS_Type))[df$NPS_Type]


install.packages("microbenchmark")
library(microbenchmark)


microbenchmark(
  as.numeric(levels(f))[f],
  as.numeric(levels(f)[f]),
  as.numeric(as.character(f)),
  paste0(x),
  paste(x),
  times = 1e5
)

par(mfrow=c(1,1))
barplot(table(df$Currency_H , df$NPS_Type) , col = brewer.pal(9,"Set1"))




hist(df$Guest.NPS.Goal_PL)

hist(df$Dom.Int.l_PL)


levels(df$Dom.Int.l_PL)


as.numeric(levels(df$Dom.Int.l_PL))

is.factor(df$NPS_Type)

is.factor(df$Restaurant_PL)

summary(reg)

reg <- lm(df$Guest.NPS.Goal_PL~df$Floors_PL)

summary(reg)

plot(reg)

heatmap(df$Guest.NPS.Goal_PL)


heatmap(df$ROOM_NUM_C, df$Guest.NPS.Goal_PL)

as.numeric(levels(df$Bell.Staff_PL))


is.factor(df$Bell.Staff_PL)

as.numeric(as.factor(df$Bell.Staff_PL))



levels(df$Bell.Staff_PL)
rm(df$Bell.Staff_PL=="")





hist(df$Guest_Country_H)



df$Guest_Country_H <- as.factor(df$Guest_Country_H)

df$ARRIVAL_DATE_R <- as.Date(df$ARRIVAL_DATE_R)


test_df <- df[rowSums(is.na(df) ==0)]

dim(df[apply(df == "", 1, all),])

as.numeric(df$Currency_PL)


hist(as.numeric(df$Currency_PL) , xlab = as.factor(df$Currency_PL))


levels(df$Currency_PL)
levels(as.numeric(df$Currency_PL))

dim(as.factor(df$Currency_PL))

max(as.numeric(df$Currency_PL))

currency_levels <- as.factor(df$Currency_PL)

hist(as.numeric(df$Currency_PL) , xlab = currency_levels)



hist(df$Currency_H)

ggplot(df) + geom_line(df , aes(x=))


levels(df$NPS_Type)

# In lab

head(data.frame(df$NPS_Type, df$Length_Stay_H))
df_reduced <- data.frame(df$NPS_Type, df$Room_Type_H , df$LENGTH_OF_STAY_C,df$POV_CODE_C , df$ARRIVAL_DATE_R, df$NT_RATE_R)

head(df_reduced)
View(df_reduced)

df$NPS_Type

levels(df$NPS_Type) <- c(NA,0,1,2)
levels(df$NPS_Type)

View(df$NPS_Type)

df$NPS_Type <- as.numeric(df$NPS_Type)



levels(df$Internet_Sat_H)
levels(df$Room_Dissat_Internet_H)

levels(df$Status_H)
levels(df$Overall_Sat_H)



View(df)



promoter_df <- df[df$NPS_Type=='Promoter',]


head(promoter_df)

View(promoter_df)

levels(promoter_df$Tranquility_H)












