
### Assessment - Hospital Cost Analysis ######

#The agency wants to analyze the data to research on healthcare costs and their utilization

getwd()

setwd("C:/Users/SujitSonar/Desktop/SimpliLearn/Assessment _R")
getwd()

library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)

hospital_df = read_excel("1555054100_hospitalcosts.xlsx")

########## basic data structure checks and transformation for analysis ##########

hospital_df
dim(hospital_df)
str(hospital_df)

# Converting FEMALE,RACE and APRDRG cols to factors

col = c('FEMALE','RACE','APRDRG')
col
hospital_df[col] =lapply(hospital_df[col],factor)
sapply(hospital_df,class)  

str(hospital_df)


# find if any NA's in the data set
summary(hospital_df)

# there is one row entry where the RACE data missing
unique(is.na(hospital_df)) # there is some data missing under Race column 

# checking how many data points under RACE column, data is missing
df_null = subset(hospital_df, is.na(hospital_df$RACE))
df_null


# Dropping the row entry where RACE data is missing #df <- df[!is.na(df$col_name), ]

hospital_df = hospital_df[!is.na(hospital_df$RACE),]
summary(hospital_df)
dim(hospital_df)
str(hospital_df)

# creating one new col for Age category and analyzing the data

Age_cat =c()
i=1

for (age in hospital_df$AGE){
   if (age <= 1){
      Age_cat = c(Age_cat,"Infants") 
   }else if (age <=3){
      Age_cat = c(Age_cat,"Toddlers")
      
   } else if (age <=5){
      Age_cat = c(Age_cat,"PreSchoolers")     
   }else if (age <=11){
      Age_cat = c(Age_cat,"Middle Childhood")     
   }else if (age<=14){
      Age_cat = c(Age_cat,"Young Teens")
   }else if (age <=17){
      Age_cat = c(Age_cat,"Teenagers")
   }else{
      Age_cat = c(Age_cat,"Adults")
   }
   i = i+1
} 

length(Age_cat)

# adding the newly created Age_cat col to the df
hospital_df = cbind(hospital_df,as.factor(Age_cat))

names(hospital_df)[7]='AGE_CAT'

str(hospital_df)

view(hospital_df)


#######  Data Interpretation / Visualization  #################

#1. To record the patient statistics, the agency wants to find the age category 
#of people who frequently visit the hospital and has the maximum expenditure.


#summary table by Age category, Gender, count of gender and Total charge

# create df to count the number of patients by Gender and Age category
ds1=table(hospital_df$AGE_CAT,hospital_df$FEMALE)
ds1=data.frame(ds1)
names(ds1)[1:3]=c('Age_category',"Gender","Count_Gender")
ds1

# create key col for lookup
ds1=mutate(ds1,new_col=paste(ds1$Age_category,ds1$Gender))
ds1


# create df to find the sum of Exp by Gender and Age category
ds2=aggregate(x=hospital_df$TOTCHG, by = list(hospital_df$AGE_CAT,hospital_df$FEMALE), FUN = sum)
names(ds2)[1:3]=c('Age_category',"Gender","Exp_by_age_cat")
ds2

# create key col for lookup
ds2=mutate(ds2,new_col=paste(ds2$Age_category,ds2$Gender))
ds2


# merging two data sets using the lookup key

ds3=merge(ds1,ds2,by="new_col",all.x=TRUE)
ds3

# drooping duplicate gender col

ds3=ds3[-1]
ds3
ds3=ds3[-2]
ds3
ds3=ds3[-3]
ds3

# renaming columns
names(ds3)[1:3]=c("Age_category","Gender_count","Gender")
ds3

#sorting df by Exp_by_age_cat in descending
ds3=ds3[order(-ds3$Exp_by_age_cat),]
ds3

# Calculating the average of Expenditure by age category and by gender

ds3=mutate(ds3,Mean_Exp_by_Age_cat=ds3$Exp_by_age_cat/ds3$Gender_count)
ds3

#sorting df by Mean of Exp_by_age_cat in descending
ds3=ds3[order(-ds3$Mean_Exp_by_Age_cat),]
ds3

##############Visualizing the data##############

p=ggplot(data = ds3, aes(x=Age_category, y = Gender_count,fill=Gender))+ 
   geom_bar(stat='identity')+
   geom_text(aes(label=Gender_count), position = position_stack(vjust = 0.5), color="white", size=3)+
   theme_minimal()+
   theme(axis.text.x = element_text(angle = 90))+
   scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"))


p


q=ggplot(data = ds3, aes(x=Age_category, y = Mean_Exp_by_Age_cat,fill=Gender))+ 
   geom_bar(stat='identity')+
   geom_text(aes(label=round(Mean_Exp_by_Age_cat/1000,2)), position = position_stack(vjust = 0.5), color="white", size=3)+
   theme_minimal()+
   theme(axis.text.x = element_text(angle = 90))+
   scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))

q



plots= list(p,q)

layout = rbind(c(1,2))

grid.arrange(grobs=plots,layout_matrix=layout,top="Expenditure by Age category and Gender",
             vp=viewport(width=0.8, height = 0.9))



# ans 1:From the above data out/put of ds3 and visualizing the data, we can clearly see that Infants age = 0 to 1 are
# age category visiting hospital most frequently, However the maximum expenditure is seen under age between 2 (toddlers 2-3) to
# 5 (preshoolers 4-5) when we take the mean of the expenditures,

#########################################################################################################################

#2. In order of severity of the diagnosis and treatments and to find out the expensive treatments,
#the agency wants to find the diagnosis-related group that has maximum hospitalization and expenditure.


str(hospital_df)

#create df to find count of patients by APRDRG and by Gender

df1=table(hospital_df$APRDRG,hospital_df$FEMALE)
df1= data.frame(df1)
class(df1)
head(df1)
names(df1)[1:3] =c("APRDRG","Gender","Patients_count")
head(df1)

# create key col for lookup
df1=mutate(df1,new_col=paste(df1$APRDRG,df1$Gender))
df1

#create df to find sum of exp by APRDRG and by Gender

df2 = aggregate(x=hospital_df$TOTCHG, by = list(hospital_df$APRDRG,hospital_df$FEMALE), FUN = sum)
names(df2)[1:3]=c("APRDRG","Gender","Exp_by_gender_APRDRG")
head(df2)

# create key col for lookup
df2=mutate(df2,new_col =paste(df2$APRDRG,df2$Gender))
df2
head(df2)

# merging df1 and df2
df3=merge(df1,df2,by="new_col",all.x=TRUE)
head(df3)
df3


#dropping unwanted col
df3=df3[-1]
df3=df3[-(4:5)]
names(df3)[1:2]=c("APRDRG","Gender")
names(df3)

#calculating mean of the Exp_by_gender

df3 = mutate(df3,Mean_Exp_by_gender_APRDRG =df3$Exp_by_gender/df3$Patients_count)
head(df3)

#sorting by Exp by APRDRG in descending order
df3=df3[order(-df3$Mean_Exp_by_gender),]
df3


#visualization

#Most number of hospitalization  and Top 10 expensive DRG and most

# we can see that maximum number of hospitalization falls under APRDRG = 640

df3 %>% slice_max(df3$Patients_count, n = 10)

ggplot(data=df3, aes(x=APRDRG,y=Patients_count, fill=Gender))+
   geom_bar(stat='identity')+
   theme_minimal()+
   theme(axis.text.x = element_text(angle = 90))


# # we can see that mean of maximum Expenditure falls under APRDRG = 911 however, the count of patients is just 1
df3 %>% slice_max(df3$Mean_Exp_by_gender, n = 10)

ggplot(data=df3, aes(x=APRDRG,y=Mean_Exp_by_gender_APRDRG, fill=Gender))+
   geom_bar(stat='identity')+
   theme_minimal()+
   theme(axis.text.x = element_text(angle = 90))+
   scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))


# ans: number of patients under PRDRG = 640 is the highest however,we cannot say that APRDRG = 640 is the
# most Expensive Treatment s compared to other APRDRG

#by looking at the Mean of each APRDRG, we see that, APRDRG = 911 is the most expensive treatment however, 
# the count of patients is just 1.

#############################################################################################################

#3. To make sure that there is no malpractice, the agency needs to analyze if the race of the patient is related to the hospitalization costs.

library(corrplot) # for corrplot

# finding correlation

hospital_df
str(hospital_df)

dataset1=lapply(hospital_df, as.numeric)

class(dataset1)

dataset2=data.frame(dataset1)

class(dataset2)

str(dataset2)

corr2=cor(dataset2[,1:6])
view(corr2)

corrplot(corr2, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  tl.col = "indianred4", addCoef.col = "black", number.digits = 2,
         number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))


# the correlation between patient race and hospitalization cost  = -0.02 which shows a very weak correlation between the two. 

# the relation is not uniform


# checking the data based on mean expenditure by Race and gender

str(hospital_df)

#create df to find count of patients by RACE and by Gender

df4=table(hospital_df$RACE,hospital_df$FEMALE)
df4= data.frame(df4)
class(df4)
head(df4)

#rename headers
names(df4)[1:3] =c("RACE","Gender","Patients_count")
head(df4)

#creating a lookup key new_col
df4=mutate(df4,new_col=paste(df4$RACE,df4$Gender))
df4

#create df to find sum of exp by RACE and by Gender
df5 = aggregate(x=hospital_df$TOTCHG, by = list(hospital_df$RACE,hospital_df$FEMALE), FUN = sum)
df5

#rename headers
names(df5)[1:3]=c("RACE","Gender","Exp_by_gender_RACE")
head(df5)

#creating a lookup key new_col
df5=mutate(df5,new_col =paste(df5$RACE,df5$Gender))
df5

# merging df4 and df5
df6=merge(df4,df5,by="new_col",all.x=TRUE)
df6

#removing unwanted cols
df6=df6[-1]
df6=df6[-(4:5)]
names(df6)[1:2]=c("RACE","Gender")
names(df6)

# calculating Mean of Expenditure by RACE

df6 = mutate(df6,Mean_Exp_by_gender_RACE =df6$Exp_by_gender_RACE/df6$Patients_count)
df6

#sorting by Exp by RACE
df6=df6[order(-df6$Mean_Exp_by_gender_RACE),]
df6


#visualization

#Top 10 expensive DRG

# we can see that maximum number of hospitalization falls under RACE = 1

df6 %>% slice_max(df6$Patients_count, n = 10)

g=ggplot(data=df6, aes(x=RACE,y=Patients_count, fill=Gender))+
   geom_bar(stat='identity')+
   theme_minimal()+
   theme(axis.text.x = element_text(angle = 90))

g


# # we can see that mean of maximum Expenditure falls under RACE = 2 however, the count of patients is just 4


df6 %>% slice_max(df6$Mean_Exp_by_gender_RACE, n = 10)

h= ggplot(data=df6, aes(x=RACE,y=Mean_Exp_by_gender_RACE, fill=Gender))+
   geom_bar(stat='identity')+
   theme_minimal()+
   theme(axis.text.x = element_text(angle = 90))+
   scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3))

h


plots= list(g,h)

layout = rbind(c(1,2))

grid.arrange(grobs=plots,layout_matrix=layout,top="Expenditure by RACE and Gender",
             vp=viewport(width=0.8, height = 0.9))


# ans: #from the above data frame output, we can see that RACE = 1, has most number of patients,
#  however the maximum expenditure falls under RACE =2 when we look at the mean of the expenditure by Race


################################################################################################################

# 4. To properly utilize the costs, the agency has to analyze the severity of the hospital costs by age 
#and gender for the proper allocation of resources.


ds3

178+138

# looking at the data we, see most of the patients are under Infants category (Age 0 -1)
# there are 316 (178 Male Infants and 138 Female infants) infants


#combo chart (col and line chart)

ggplot(ds3) +
   geom_col(aes(x = Age_category, y = Mean_Exp_by_Age_cat,fill=Gender), size = 1, color = "darkblue") +
   scale_fill_manual(values = c("#0073C2FF", "#EFC000FF"))+
   geom_text(aes(x = Age_category, y = Mean_Exp_by_Age_cat,label=round(Mean_Exp_by_Age_cat/1000,2)), position = position_stack(vjust = 0.5), color="white", size=3)+
   geom_line(aes(x = Age_category, y = 100*Gender_count, color=Gender,group=Gender),stat='identity',size=1)+
   scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-3),sec.axis = sec_axis(~./100, name = "Gender_count"))+
   theme_minimal()


# ans  - looking at the number of patients by gender and age, we see that most of the patients are under Infants category
# as shown by the line plot, however, hospital cost for age 2-3 and 3 - 4 are the highest



###########################################################################################################################
#5. Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay can
#be predicted from age, gender, and race.

head(hospital_df)

summary(hospital_df)

hospital_df
str(hospital_df)
dim(hospital_df)


#changing the df fields to numeric
dataset1=lapply(hospital_df, as.numeric)

class(dataset1)

dataset2=data.frame(dataset1)

class(dataset2)

str(dataset2)

corr2=cor(dataset2[,1:6])
view(corr2)

corrplot(corr2, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  tl.col = "indianred4", addCoef.col = "black", number.digits = 2,
         number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))


##### Correlation Notes ###################
#Negative correlation
# with increase in speed if the time decrease 
#if the decrease is uniform then the correlation is high 
#but if time decreases erratically then the co relation is low

# with increase in speed, chances of accident also increases
#if the increase is uniform then the correlation is high 
#but if increases erratically then the co relation is low
#correlation ranges

#     -1 < correlation <1

# If the uniform decrease then correlation is more towards -1
# If the uniform increase then correlation is more towards +1
# if there is correlation but erratic then it will towards Zero




#########################################################################


# dependent variable = LOS
# independent variable = age,gender and race


# Boxplot for AGE
boxplot(dataset2$LOS ~ dataset2$AGE, main = 'AGE', col=c("blue","red"))


# Boxplot for Gender
boxplot(dataset2$LOS ~ dataset2$FEMALE, main = 'Gender', col=c("blue","red"))


# Boxplot for RACE
boxplot(dataset2$LOS ~ dataset2$RACE, main = 'RACE', col=c("blue","red"))


# Dropping variables that are not needed for prediction.

names(dataset2)

dataset2 = subset(dataset2, select = -c(TOTCHG,APRDRG,AGE_CAT))

names(dataset2)

str(dataset2)

### creating dummy variables. 
AGE_factor <- as.factor(dataset2$AGE)
AGE_factor
dummy_AGE <- data.frame(model.matrix(~AGE_factor))[,-1]

head(dummy_AGE)


RACE_factor <- as.factor(dataset2$RACE)
dummy_RACE <- data.frame(model.matrix(~RACE_factor))[,-1]

head(dummy_RACE)


FEMALE_factor <- as.factor(dataset2$FEMALE)
dummy_FEMALE <- data.frame(model.matrix(~FEMALE_factor))[,-1]

head(dummy_FEMALE)


dataset3 <- cbind(dataset2,dummy_AGE,dummy_FEMALE,dummy_RACE)

names(dataset3)



### Drop original columns 

dataset4 = subset(dataset3, select = -c(AGE,FEMALE,RACE) )
names(dataset4)
View(dataset4)


# move columns in DF
#df %>%
#select(g, everything())
str(dataset5)


# Splitting dataset into training set and test set
library(caTools) # for splitting dataset

set.seed(123) # Seed initializes the randomness
sample = sample.split(dataset4, SplitRatio = 0.7) # Returns a vector with T for 70% of data
trainingSet = subset(dataset4, sample == T)
testSet = subset(dataset4, sample == F)




# Create model 
model = lm(formula = LOS ~ . , data = trainingSet)
summary(model)

y_pred_train = predict(model, newdata = trainingSet)

length(y_pred_train)
y_pred_train

# Visualizing the training set results
ggplot() + 
   geom_point(aes(x=trainingSet$LOS,y=y_pred_train)) +
   xlab('actual_LOS') +
   ylab('predicted_LOS')+
   ggtitle('comparison of train data')



# Visualizing the test set results

y_pred_test = predict(model, newdata = testSet)

y_pred_test
View(y_pred_test)

ggplot() + 
   geom_point(aes(x=testSet$LOS,y=y_pred_test)) +
   xlab('actual_LOS') +
   ylab('predicted_LOS')+
   ggtitle('comparison of test data')








########################################################################################################################


################################################################################################################





