

install.packages("dplyr")
library(dplyr)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

install.packages("plotly")
library(plotly)

df <- read.csv("C:/Users/Gabriel/OneDrive/Área de Trabalho/Data Analytics TSOM/Module 2. Data handling and decision making/Assignments/FINAL PROJECT/BurgundySip.csv")
summary(df)

str(df)



#STEP 1 - VARIABLE RECOGNITIONS - assumptions



# 'data.frame': 7500 obs. of  14 variables:

# $ PR  : num  23.7 41.9 64 172.5 78.7 ... [SD]

# $ RT  : num  4.06 4.21 4.31 4.43 4.32 4.37 4.34 4.32 4.09 4.31 ... [SD]

# $ SN  : chr  "W4339-20718" "W9737-31436" "W8398-63402" "W4418-44312" ... [SI]

# $ NAME: chr  "A Coroa" "Aalto" "Aalto" "Aalto" ... [SI]

# $ WINE: chr  "200 Cestos Godello" "Blanco de Parcela" "PS (Pagos Seleccionados) Ribera del Duero"... [SI]

# $ YR  : chr  "2020" "2019" "2011" "2015" ... [SI]

# $ REG : chr  "Valdeorras" "Ribera del Duero" "Ribera del Duero" "Ribera del Duero" ... [SI]

# $ TP  : chr  NA "Verdejo" "Ribera Del Duero Red" "Ribera Del Duero Red" ... [SI]

# $ NUMR: int  33 80 2207 2858 4411 3383 3239 1108 2844 1884 ... [SI]

# $ BD  : int  NA NA 5 5 5 5 5 5 NA 5 ... [SI]

# $ ACD : int  NA NA 3 3 3 3 3 3 NA 3 ... [SI]

# $ RSG : chr  " 9.9868 " " 8.9416 " " 8.2189 " " 6.9998 " ... [SI]

# $ AL  : chr  " 11.1057 " " 11.6007 " " 11.9328 " " 11.4113 " ... [SI]

# $ DN  : chr  " 0.9956 " " 0.9960 " " 0.9952 " " 0.9955 " ... [SI]





df$YR <- as.numeric(df$YR)

df$RSG <- as.numeric(df$RSG)

df$AL <- as.numeric(df$AL)

df$DN <- as.numeric(df$DN)

df$REG <- sapply(df$REG, factor)

df$TP <- sapply(df$TP, factor)

summary(df)

str(df)







#STEP 2 - TREATING DUPLICATES from bottom to top



anyDuplicated(df$SN)

duplicated(df$SN,fromlast = T)

df[duplicated(df$SN,fromlast = T),]

df <- df[!duplicated(df$SN,fromlast = T),]

df

summary(df)

str(df)



#removing the N.V. value because there were a lot of N.V. that were deleted as duplicated values but we were missing one.



df <- df[-693,] #we used fix(df) to first find the N.V.



#STEP 3 - TREATING MISSING VALUES - first we calculate the % of missing values for each variable



calculateMissProb <- function(x){
  
  return(sum(is.na(x))/length(x)*100);
  
}

apply(df,2,calculateMissProb);



#impute mode to the NA values of the wine types based on the region



# Group by TYPE and REGION, calculate mode for each combination

mode_df <- df %>%
  
  group_by(TP, REG) %>%
  
  summarize(Mode_Category = names(table(TP))[which.max(table(TP))])



# Merge the mode_df with the original data to impute missing values

df <- merge(df, mode_df, by = c("TP", "REG"), all.x = TRUE)



# Impute missing CATEGORY values with the calculated mode

df$TP[is.na(df$TP)] <- df$Mode_Category

df$Mode_Category



# Remove the auxiliary column

df$Mode_Category <- NULL

summary(df)

str(df)



#Adding an "others" category for categorical variables with a large number of observations that are not repeated frequently



topREG <- names(sort(table(df$REG), decreasing = T)[1:35]) #36th is way lower

topREG

df$REG <- as.character(df$REG)

df$REG[!df$REG%in%topREG] <- "others"

df$REG

table(df$REG)

sort(table(df$REG), decreasing = T)







df$REG <- factor(df$REG)





summary(df)

str(df)





#treating the variables with more than 0.5% of NA values: YR, PR, BD, ACD, RSG, AL, DN

#analyze if the categories influence the missing values of the numeric ones





#Identify numeric and categorical variables

numeric_vars <- names(df)[sapply(df, is.numeric)]

categorical_vars <- names(df)[sapply(df, is.factor)]



#Loop through each combination of numeric and categorical variables to check if they are related with ANOVA

for (numeric_var in numeric_vars) {
  
  for (categorical_var in categorical_vars) {
    
    anova_result <- anova(lm(paste(numeric_var, "~", categorical_var), data = df))
    
    print(paste("ANOVA result for", numeric_var, "and", categorical_var, ":"))
    
    print(anova_result)
    
  }
  
}



#CHI- SQUARE to check that REGION and TYPE the two factor categories are indeed associated with each other.

df %>%
  
  select_if(is.factor)%>%
  
  table()%>%
  
  summary()



#Imputing the mean of the combinations of region and type to the missing values of all numeric variables because they are related: p-value < 0.05



#Calculate the means of the variables BD, ACD, AL, YR, RSG, and DN for each combination of region and type

meanRegType <- aggregate(cbind(BD, ACD, AL, YR, DN, RSG) ~ REG + TP, data = df, FUN = mean, na.rm = TRUE)

meanRegType;

#Loop through the rows of the mean_combinations data frame

for (i in 1:nrow(meanRegType )) {
  
  reg_value <- meanRegType$REG[i]
  
  tp_value <- meanRegType$TP[i]
  
  bd_mean <- meanRegType$BD[i]
  
  acd_mean <- meanRegType$ACD[i]
  
  al_mean <- meanRegType$AL[i]
  
  yr_mean <- meanRegType$YR[i]
  
  dn_mean <- meanRegType$DN[i]
  
  rsg_mean <- meanRegType$RSG[i]
  
  
  
  
  
  # Impute missing values with the means of the corresponding combination
  
  df$BD[is.na(df$BD) & df$REG == reg_value & df$TP == tp_value] <- round(bd_mean, 0) #used round because should be an integer
  
  df$ACD[is.na(df$ACD) & df$REG == reg_value & df$TP == tp_value] <- round(acd_mean, 0) #used round because should be an integer
  
  df$AL[is.na(df$AL) & df$REG == reg_value & df$TP == tp_value] <- al_mean
  
  df$YR[is.na(df$YR) & df$REG == reg_value & df$TP == tp_value] <- round(yr_mean, 0)
  
  df$DN[is.na(df$DN) & df$REG == reg_value & df$TP == tp_value] <- dn_mean
  
  df$RSG[is.na(df$RSG) & df$REG == reg_value & df$TP == tp_value] <- rsg_mean
  
  
  
}

summary(df)



print(str(df));



#recalculating the % prob of all columns



calculateMissProb <- function(x){
  
  return(sum(is.na(x))/length(x)*100);
  
}

apply(df,2,calculateMissProb);



#we still have some missing values that can´t be predicted and imputed with this model, so we will omit all the leftovers at the end.





#STEP 4 - TREATING OUTLIERS of all numeric/integer variables

#First we check with boxplot if there are outliers



df %>%
  
  select_if(is.numeric)%>%
  
  boxplot()



#BASED ON THE CONTEXT ONLY PRICES ARE CONSIDERED OUTLIERS - maybe miss typing. The others are considered valid extreme values.





boxplot(df$YR)

boxplot(df$RT)

boxplot(df$PR) #OUTLIERS

boxplot(df$NUMR)

boxplot(df$RSG)

boxplot(df$AL)

boxplot(df$ACD)

boxplot(df$BD)

boxplot(df$DN)



#check the normality distribution of the numeric variables and correlation



df %>%
  
  select_if(is.numeric)%>%
  
  lapply(FUN = shapiro.test)



df %>%
  
  select_if(is.numeric)%>%
  
  chart.Correlation(method = "spearman")









# treating outliers in PRICE using quartiles and IQR because they are not normally distributed



# Create a copy of the original dataframe

treated_data <- df



# Calculate quartiles and IQR

Q1 <- quantile(df$PR, 0.25, na.rm = TRUE)

Q3 <- quantile(df$PR, 0.75, na.rm = TRUE)

IQR <- Q3 - Q1



# Calculate lower and upper bounds

lower_bound <- Q1 - 1.5 * IQR

upper_bound <- Q3 + 1.5 * IQR



# Replace outliers with lower or upper bounds

treated_data$PR[treated_data$PR < lower_bound] <- lower_bound

treated_data$PR[treated_data$PR > upper_bound] <- upper_bound





summary(treated_data)



calculateMissProb <- function(x){
  
  return(sum(is.na(x))/length(x)*100);
  
}

apply(treated_data,2,calculateMissProb);





#*STEP 3 CONTINUATION* - MISSING VALUES - after treating outliers - treat price  NA values with regression model



# Create the model for the multivariate linear regression model - we picked the top 3 correlated variables (RSG > AL > YR)

modelPR <- lm(PR ~ RSG + AL + YR, data = treated_data)

modelPR;







modelPR_2 <- loess(PR ~ RSG + AL + YR, data = treated_data)

modelPR_2;







# Print the summary of the model

summary(modelPR) #rsquared is 0.8 and the Residual Standard Error is lower that  means the model with lm is good and better than the model2 with loess

#we can also check the 3 variables we chose are highly significant because of the 3 stars in the summary

summary(modelPR_2)



# Impute missing "PR" values using the model

treated_data$PR[is.na(treated_data$PR)] <- predict(modelPR, newdata = treated_data[is.na(treated_data$PR), ])



# Print the updated dataset

print(treated_data)

str(treated_data)

summary(treated_data)



#finally, we can remove the NA values that are still in the data set after all treating process





treated_data <- na.omit(treated_data);

summary(treated_data)



#Step 5 - Descriptive objectives



#1) How many types of wine are there, how many regions are represented, and how many wines are there per type and per region?



unique(treated_data$TP)

sort(table(treated_data$TP), decreasing = T)

sort(table(treated_data$REG), decreasing = T)



#Aggregate the count of wines per type of wine

winebyTP <- aggregate(WINE ~ TP, data = treated_data, FUN = length)

winebyTP;



#Order the type with the higher count of wine

sorted_lengths <- winebyTP[order(-winebyTP$WINE), ]

sorted_lengths



#Aggregate the count of wines per region

winebyREG <- aggregate(WINE ~ REG, data = treated_data, FUN = length)

winebyREG;



#Order the region with the higher count of wine

sorted_lengths2 <- winebyREG[order(-winebyREG$WINE), ]

sorted_lengths2;



#2) How many different types by regions



# Count the number of types for each region

type_counts <- aggregate(TP ~ REG, data = treated_data, FUN = function(x) length(unique(x)))

type_counts;



#Order the Reg by the ones that has a higher count of unique types

sorted_lengths3 <- type_counts[order(-type_counts$TP), ]

sorted_lengths3







#3) What is the region and type of wine with the best/worst ratings?

topRT <- aggregate(RT ~ REG + TP, data = treated_data, FUN = mean)

sorted_topRT <- topRT[order(topRT$RT, decreasing = TRUE), ]

sorted_bottomRT <- topRT[order(topRT$RT), ]

sorted_topRT

sorted_bottomRT



#4) What is the region and type of wine with the highest/lowest prices?

topPR <- aggregate(PR ~ REG + TP, data = treated_data, FUN = mean)

sorted_topPR <- topPR[order(topPR$PR, decreasing = TRUE), ]

sorted_bottomPR <- topPR[order(topPR$PR), ]

sorted_topPR

sorted_bottomPR



#5 Relation between prices and ratings

plot(treated_data$RT,treated_data$PR)

cor.test(treated_data$RT, treated_data$PR, method = "spearman")







#6 Evolution of prices through the years



plot(treated_data$YR,treated_data$PR)

cor.test(treated_data$YR, treated_data$PR, method = "spearman")



#7 Relation between  of prices and RSG

plot(treated_data$RSG,treated_data$PR)

cor.test(treated_data$RSG, treated_data$PR, method = "spearman")





#8 Relation between prices and AL

plot(treated_data$AL,treated_data$PR)

cor.test(treated_data$AL, treated_data$PR, method = "spearman")



#9 Relation between RT an YR

plot(treated_data$YR,treated_data$RT)

cor.test(treated_data$YR, treated_data$RT, method = "spearman")



#10 Relation between RT and RSG

plot(treated_data$RSG,treated_data$RT)

cor.test(treated_data$RSG, treated_data$RT, method = "spearman")



#11 Relation between RT and AL

plot(treated_data$AL,treated_data$RT)

cor.test(treated_data$AL, treated_data$RT, method = "spearman")



# STEP 6 - PREDICTIVE OBJECTIVES



#first we check the correlation between price/ratings and the other numeric variables



df %>%
  
  select_if(is.numeric)%>%
  
  chart.Correlation(method = "spearman")



#both of Price and Ratings are more correlated to YR, RSG and AL - which makes sense because the correlation between both PR and RT is 1.00.





# Create the model for the multivariate linear regression model - we picked the top 3 correlated variables (RSG > AL > YR)



#Find de model that better predicts RT based on RSG + AL + YR



modelRT <- lm(RT ~ RSG + AL + YR, data = treated_data) #R-squared 0.8645, residual error 0.07342

modelRT_2 <- loess(RT ~ RSG + AL + YR, data = treated_data, control = loess.control(surface = "direct")) # residual error 0.06118

summary(modelRT)

summary(modelRT_2)



summary(treated_data)



#In this case we would use the model_2 to predict the future ratings of a given wine with certain characteristics



treated_data;



#Creating a new set of wines to try the prediction model

new_wine <- data.frame(YR=as.integer(runif(10,1910,2024)),AL=runif(10,0,14),RSG=runif(10,0,16))

new_wine;



new_wine2<-data.frame(YR=1900,AL=7,RSG=9)

new_wine2





#Testing dummy data

predicted_rating <- predict(modelRT_2, newdata = new_wine,allow.far = TRUE)

predicted_rating <- pmax(1, pmin(predicted_rating, 5))

predicted_rating





predicted_rating <- predict(modelRT_2, newdata = new_wine2,allow.far = TRUE)

predicted_rating <- pmax(1, pmin(predicted_rating, 5))

predicted_rating



#Ploting the regression RT

New_data <- data.frame(
  
  Actual = treated_data$RT,
  
  Predictive = predict(modelRT_2)
  
)



ggplot(New_data, aes(x = Actual, y = Predictive)) +
  
  geom_point() +
  
  geom_smooth(method = "loess", col = "red") +
  
  labs(title = "Regresion RT",
       
       x = "Actual RT",
       
       y = "Predictive RT")











#For predicting the price of a certain wine based on RSG + AL + YR, we will use the same model that we use to predict the missing values



modelPR <- lm(PR ~ RSG + AL + YR, data = treated_data)

modelPR;



#Testing dummy data

new_wine <- data.frame(YR=runif(10,1910,2024),AL=runif(10,0,14),RSG=runif(10,0,16))

new_wine;



new_wine3<- data.frame(YR=1920,AL=10,RSG=12)

new_wine3;



predicted_PR <- predict(modelPR, newdata = new_wine)

predicted_PR <- pmax(0, pmin(predicted_PR, 1000000000))

predicted_PR;





predicted_PR <- predict(modelPR, newdata = new_wine3)

predicted_PR <- pmax(0, pmin(predicted_PR, 1000000000))

predicted_PR;





##Ploting the regression PR



New_data <- data.frame(
  
  Actual = treated_data$PR,
  
  Predictive = predict(modelPR)
  
)



ggplot(New_data, aes(x = Actual, y = Predictive)) +
  
  geom_point() +
  
  geom_smooth(method = "lm", col = "red") +
  
  labs(title = "Regresion PR",
       
       x = "Actual PR",
       
       y = " Predictive PR")









#Clustering



#Cluster the wines by prices based on the top correlation variables: YEAR, AL and RSG.





fit <- kmeans(treated_data[c("PR","YR","AL","RSG")], 3)





fit$cluster;

fit$iter;

treated_data$PR_Cluster <- fit$cluster;







treated_data$PR_Cluster <- factor(treated_data$PR_Cluster,
                                  
                                  levels = 1:3,
                                  
                                  labels = c("Vintage Prestige Wines","Elegant Balanced","Youthful Delights Wines"))



summary(treated_data);





#Cluster the wines by ratings based on the top correlation variables: YEAR, AL and RSG.





fit2 <- kmeans(treated_data[c("RT","YR","AL","RSG")], 3)

fit2;



fit2$cluster;

fit$iter;



treated_data$RT_Cluster <- fit2$cluster;





treated_data$RT_Cluster <- factor(treated_data$RT_Cluster,
                                  
                                  levels = 1:3,
                                  
                                  labels = c("Premier Market Leaders","Balanced Market Appeal Wines","Niche Preference Selection"))



summary(treated_data);



#Ploting the clusters in the relation between PR and RT



#1 RT clusters based on RSG, AL, YR

plot_ly(x=treated_data$RT, y=treated_data$PR,
        
        color=I(fit2$cluster))



#2 PR clusters based on RSG, AL, YR

plot_ly(x=treated_data$RT, y=treated_data$PR,
        
        color=I(fit$cluster))




