#############################################################################
#                                                                           #
#                             Incognia Data Analysis                        #
#                                                                           #
#                           Hugo Roberto de Melo Daher                      #
#                                                                           #
#                                 Technical Case                            #
#                                                                           #
#                             Exploratory Analysis                          #
#                                                                           #
##############################################################################


# Data cleansing and data transformation

# Setting work folder
setwd("C:/Users/Toccato/OneDrive/Work Experience/Incognia/Case/r")
getwd()

# Installing packages
#install.packages("dplyr")
#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("Amelia")
#install.packages("caret")
#install.packages("reshape")
#install.packages("randomForest")
#install.packages("e1071")
#install.packages("tidyr")
#install.packages("DMwR")
#install.packages("GGally")
#install.packages("stringr")
#install.packages("viridis")
#install.packages("hrbrthemes")
#install.packages("corrplot")

# Loading packages
library(dplyr)
library(data.table)
library(ggplot2)
library(Amelia)
library(caret)
library(reshape)
library(randomForest)
library(e1071)
library(tidyr)
library(DMwR)
library(GGally)
library(stringr)
#library(tidyverse)
#library(readr)
#library(lubridate)
#library(ggmap)
library(psych)
library(gridExtra)
library(scales)
library(viridis)
library(hrbrthemes)
library(corrplot)

# Loading dataset
tb <- read.table("Dados/hugo_incognia_db_for_da_test.csv",
                    dec = ".",
                    sep = ",",
                    h = T,
                    fileEncoding = "windows-1252")

# Removing id column
tb$event_id <- NULL

# Shortening some column names
names(tb) <- c("event_timestamp","account_id","device",
               "distance_fl","device_age","is_emulator",
               "fake_location","root_permission","official_store")

# Converting variable to integer
tb$distance_fl <- 
  as.integer(tb$distance_fl)

# Converting miliseconds to timestamp
tb$event_timestamp <- as.POSIXct(tb$event_timestamp / 1000, 
                                 origin = "1970-01-01", 
                                 tz = "UTC")
summary(tb$event_timestamp)

# Creating variable Event Hour
tb$event_hour <- format(tb$event_timestamp,"%H")
tb$event_hour <- as.numeric(tb$event_hour)

# Creating variable Event Date
tb$event_date <- as.Date(tb$event_timestamp,format="%Y-%m-%d")

# Creating variable Weekday

Sys.setlocale("LC_ALL", "Portuguese")
#Sys.getlocale("LC_TIME")
#Sys.setlocale("LC_TIME","English")
tb$weekday <- weekdays(tb$event_date)
tb$wday <- as.POSIXlt(tb$event_date)$wday

# Missing values
sapply(tb, function(x) sum(is.na(x)))
missmap(tb, main = "Missing Values")
tb <- replace_na(tb,list(distance_fl = -1))

# Device Age Category
tb$age_category <- cut(tb$device_age, 
            breaks = c(0,1,7,30,365,Inf),
            labels = c("Day","Week","Month","Year","Year+"), right = FALSE)

# Distance to frequent location Category
tb$distance_group <- cut(tb$distance_fl, 
                              breaks = c(-1,0,1,10,Inf),
                              labels = c("No Location",
                                         "Freq Location",
                                         "Near FL",
                                         "Far FL"), right = FALSE)

# Creating Risk Score
tb$score_risk = 0
tb$score_risk <- tb$score_risk + ifelse(tb$event_hour>=0 & tb$event_hour<=6,1,0)
tb$score_risk <- tb$score_risk + ifelse(tb$distance_group=="No Location",3,0)
tb$score_risk <- tb$score_risk + ifelse(tb$distance_group=="Far FL",2,0)
tb$score_risk <- tb$score_risk + ifelse(tb$distance_group=="Near FL",1,0)
tb$score_risk <- tb$score_risk + ifelse(tb$age_category=="Day",3,0)
tb$score_risk <- tb$score_risk + ifelse(tb$is_emulator=="true",3,0)
tb$score_risk <- tb$score_risk + ifelse(tb$fake_location=="true",3,0)
tb$score_risk <- tb$score_risk + ifelse(tb$root_permission=="true",3,0)
tb$score_risk <- tb$score_risk + ifelse(tb$official_store=="false",3,0)

# Creating Risk Level
tb$risk_level <- cut(tb$score_risk, 
                              breaks = c(0,2,3,Inf),
                              labels = c("Low","Medium","High"),
                     right = FALSE)
str(tb)
# Creating a single column with all numeric variables
tb_pivot <- pivot_longer(select
                         (tb,c("distance_fl",
                               "device_age",
                               "event_hour",
                               "score_risk")),
                             cols = everything(), names_to = "numeric_var",
                             values_to = "values")
View(tb_pivot)
# Building a numeric only dataframe
tb_num <- select(tb,c("distance_fl",
                      "device_age",
                      "event_hour",
                      "score_risk"))

# Absolute frequency
freq1 <- table(tb[c('is_emulator')])
freq2 <- table(tb[c('fake_location')])
freq3 <- table(tb[c('root_permission')])
freq4 <- table(tb[c('official_store')])
freq5 <- table(tb[c('distance_group')])
freq6 <- table(tb[c('age_category')])
freq7 <- table(tb[c('event_hour')])
freq8 <- table(tb[c('risk_level')])


# Relative frequency
freq_rel1 <- round(prop.table(freq1),7)*100
freq_rel2 <- round(prop.table(freq2),7)*100
freq_rel3 <- round(prop.table(freq3),7)*100
freq_rel4 <- round(prop.table(freq4),7)*100
freq_rel5 <- round(prop.table(freq5),4)*100
freq_rel6 <- round(prop.table(freq6),4)*100
freq_rel7 <- round(prop.table(freq7),4)*100
freq_rel8 <- round(prop.table(freq8),4)*100

### CHARTS ###

# Evaluating correlation between variables
ggpairs(tb_num, upper = list(continuous = wrap("cor", size = 3))) +
  theme(
    axis.text = element_text(size = 6),
    axis.title = element_text(size = 6),
    legend.background = element_rect(fill = "white"),
    panel.grid.major = element_line(colour = NA),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "grey95"))


#### Heatmap ####
M = cor(tb_num)
corrplot(M, method = 'color', order = 'alphabet')


##### Barplot #####

# Risk Level
df = data.frame(freq8)
bar1 <- ggplot(data=df, aes(x=Var1, y=Freq, fill=Var1)) +
  geom_bar(stat="identity") +
  #scale_fill_manual("legend",values=c("Low"="turquoise","Medium"="khaki","High"=2)) +
  scale_fill_manual("legend",values=c("turquoise","khaki",2)) +
  geom_text(aes(label=Freq), vjust=0, size=3)+
  ggtitle("Risk Level") +
  labs(y = "Count", 
       x = "Risk Level") +  
  theme(title = element_text(size = 10, color = "turquoise4")) +
  scale_y_continuous(labels = comma)

# Is Emulator
df = data.frame(freq1)
bar2 <- ggplot(data=df, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill=c(2,3)) +
  geom_text(aes(label=Freq), vjust=0, size=3)+
  ggtitle("Is Emulator") +
  labs(y = "Count", 
       x = "Category") +  
  theme(title = element_text(size = 10, color = "turquoise4")) +
  scale_y_continuous(labels = comma)

# Has Fake Location
df = data.frame(freq2)
bar3 <- ggplot(data=df, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill=c(2,3)) +
  geom_text(aes(label=Freq), vjust=0, size=3)+
  ggtitle("Has Fake Location") +
  labs(y = "Count", 
       x = "Category") +  
  theme(title = element_text(size = 10, color = "turquoise4")) +
  scale_y_continuous(labels = comma)

# Has Root Permissions
df = data.frame(freq3)
bar4 <- ggplot(data=df, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill=c(2,3)) +
  geom_text(aes(label=Freq), vjust=0, size=3)+
  ggtitle("Has Root Permissions") +
  labs(y = "Count", 
       x = "Category") +  
  theme(title = element_text(size = 10, color = "turquoise4")) +
  scale_y_continuous(labels = comma)

# App is from official store
df = data.frame(freq4)
bar5 <- ggplot(data=df, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill=c(2,"turquoise")) +
  geom_text(aes(label=Freq), vjust=0, size=3)+
  ggtitle("App from official store") +
  labs(y = "Count", 
       x = "Category") +  
  theme(title = element_text(size = 10, color = "turquoise4")) +
  scale_y_continuous(labels = comma)

# Distance to FL Category
df = data.frame(freq5)
bar6 <- ggplot(data=df, aes(x=Var1, y=Freq, fill=Var1)) +
  geom_bar(stat="identity") +
  scale_fill_manual("legend",values=c("turquoise","khaki",2)) +
  geom_text(aes(label=Freq), vjust=0, size=3)+
  ggtitle("Distance to Frequent Location Category") +
  labs(y = "Count", 
       x = "Category") +  
  theme(title = element_text(size = 10, color = "turquoise4")) +
  scale_y_continuous(labels = comma)

# Device Age Category
df = data.frame(freq6)
bar7 <- ggplot(data=df, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill=c(2,"thistle","khaki","turquoise","turquoise4")) +
  geom_text(aes(label=Freq), vjust=0, size=3)+
  ggtitle("Device Age Category") +
  labs(y = "Count", 
       x = "Category") +  
  theme(title = element_text(size = 10, color = "turquoise4")) +
  scale_y_continuous(labels = comma)

# Hour analysis
df = data.frame(freq7)
bar8 <- ggplot(data=df, aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill=c(2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3)) +
#  geom_text(aes(label=Freq), vjust=-0.3, size=3.5)+
  ggtitle("Hour of the Day") +
  labs(y = "Count", 
       x = "Hour") +  
  theme(title = element_text(size = 10, color = "turquoise4")) +
  scale_y_continuous(labels = comma)

grid.arrange(bar1, bar6, nrow=2, ncol=1)
grid.arrange(bar7, bar8, nrow=2, ncol=1)
grid.arrange(bar2, bar3, bar4, bar5, nrow=2, ncol=2)


##### Pie Chart ##### 
# set the plotting area into a 1*3 array
par(mfrow=c(1,2))    

pc <- pie(freq_rel8, labels = freq_rel8 , 
          main = "Risk Level",
          col = c("turquoise","khaki",2))
legend("topright", names(freq_rel8), cex = .8,
       fill = c("turquoise","khaki",2))

pc <- pie(freq_rel6, labels = freq_rel6 , 
          main = "Device Age Analysis",
          col = c(2,"thistle","khaki","turquoise","turquoise4"))
legend("topright", names(freq_rel6), cex = .7,
       fill = c(2,"thistle","khaki","turquoise","turquoise4"))

par(mfrow=c(1,1))
pc <- pie(freq_rel5, labels = freq_rel5 , 
          main = "Distance to Frequent Location",
          col = c(2,"turquoise4","turquoise","khaki"))
legend("topright", names(freq_rel5), cex = .8,
       fill = c(2,"turquoise4","turquoise","khaki"),)

##### Boxplot ##### 
# Numeric variables
ggplot(tb_pivot, mapping = aes(x=numeric_var, y=values, fill="red")) +
  geom_boxplot(show.legend = FALSE) + 
  facet_wrap(.~numeric_var, ncol = 4, scales = "free") +
  ggtitle("Numeric Variables Analysis") +
  labs(x = "Numeric Variables") +
  theme(strip.text.x = element_blank(), 
        text = element_text(size = 12, color = "turquoise4")) +
  scale_y_continuous(labels = comma)

# Is Emulator vs Device Age
box1 <- ggplot(tb, aes(x = reorder(is_emulator, device_age), 
                       y = device_age, fill = is_emulator)) + 
  geom_boxplot() +
#  ggtitle("Is Emulator vs Device Age") +
  labs(y = "Device Age Days", 
       x = "Is Emulator") +
  theme(legend.position = "none", 
        title = element_text(size = 12, color = "turquoise4"))

# Has Fake Location vs Device Age
box2 <- ggplot(tb, aes(x = reorder(fake_location, device_age), 
                       y = device_age, fill = fake_location)) + 
  geom_boxplot() +
#  ggtitle("Has Fake Location vs Device Age") +
  labs(y = "Device Age Days", 
       x = "Has Fake Location") +
  theme(legend.position = "none", 
        title = element_text(size = 12, color = "turquoise4"))

# Has Root Permissions vs Device Age
box3 <- ggplot(tb, aes(x = reorder(root_permission, device_age), 
                       y = device_age, fill = root_permission)) + 
  geom_boxplot() +
#  ggtitle("Has Root Permissions vs Device Age") +
  labs(y = "Device Age Days", 
       x = "Has Root Permissions") +
  theme(legend.position = "none", 
        title = element_text(size = 12, color = "turquoise4"))

# App From Official Store vs Device Age
box4 <- ggplot(tb, 
               aes(x = reorder(official_store, device_age), 
                   y = device_age, fill = official_store)) + 
  geom_boxplot() +
#  ggtitle("App From Official Store vs Device Age") +
  labs(y = "Device Age Days", 
       x = "App From Official Store") +
  theme(legend.position = "none", 
        title = element_text(size = 12, color = "turquoise4"))

# GRid
grid.arrange(box1, box2, box3, box4, nrow=2, ncol=2)

# Distance to Frequent Location Category vs Device Age
ggplot(tb, aes(x = distance_group, 
               y = device_age, fill = distance_group)) + 
  geom_boxplot() +
  ggtitle("Distance to Frequent Location Category vs Device Age") +
  labs(y = "Device Age Days", 
       x = "Distance to Frequent Location category") +
  theme(legend.position = "none", 
        title = element_text(size = 12, color = "turquoise4"))


##### Scatter Plot ##### 
# showing relation between two variables and a logical variable
ggplot(data = tb, aes(y = distance_fl/1000, 
                      x = device_age)) +
  geom_point(aes(color = risk_level), size = 2) +
  ggtitle("Distance to frequent location vs Device Age") +
  scale_color_manual(values=c("turquoise4","gold",2)) +
  labs(y = "Distance to frequent location (Km)", 
       x = "Device Age (days)") +
  theme(title = element_text(size = 12, color = "turquoise4"))

##### Histogram #####
# Device Age
temp <- summarise(group_by(tb,device),max = max(device_age))
df = data.frame(value = temp$max)
ggplot(df, aes(x=value)) + 
  geom_histogram(col="white",aes(fill = ..count..)) +
  ggtitle("Histogram with distribution curve of Device Age") +
  labs(y = "Count", 
       x = "Device Age (days)") +
  theme(title = element_text(size = 12, color = "turquoise4"))

###### Area Chart ####
# Event Date
temp <- summarise(group_by(tb,risk_level,event_date),qtde = length(event_date))
df = data.frame(temp)
ggplot(df, aes(x=event_date,y=qtde, fill=risk_level)) + 
  geom_area(alpha=0.6 , size=1.0, colour="#ffffff") +
  scale_fill_manual(values=c("turquoise","khaki",2)) +
#  scale_fill_viridis(discrete = T) +
#  theme_ipsum() + 
  ggtitle("Distribution curve of Event Date") +
  labs(y = "Count", 
       x = "Event Date") +
  theme(legend.position='right',
        title = element_text(size = 12, color = "turquoise4"))

# Event Weekday
temp <- summarise(group_by(tb,risk_level,weekday,wday),qtde = length(weekday))
df = data.frame(temp)
ggplot(df, aes(x=reorder(weekday,wday),
               y=qtde, fill=risk_level)) + 
  geom_bar(stat="identity", position="stack") +
  scale_fill_manual(values=c("turquoise","khaki",2)) +
  ggtitle("Day of the Week") +
  labs(y = "Count", 
       x = "Weekday") +  
  theme(title = element_text(size = 10, color = "turquoise4")) +
  scale_y_continuous(labels = comma)


#################### Machine Learning Model####################

# Sample random rows in dataframe
df = data.frame(tb)
tb_ml <- df[sample(nrow(df), 3000), ]

# Suggesting a variable as a possible fraud 
# Risk Score greater than 3
tb_ml$is_fraud <- ifelse(tb_ml$score_risk>=4,1,0)

tb_ml$is_fraud <- as.factor(tb_ml$is_fraud)
tb_ml$account_id <- NULL
tb_ml$device <- NULL 
tb_ml$event_hour <- as.factor(tb_ml$event_hour)
tb_ml$score_risk <- NULL
tb_ml$event_timestamp <- NULL 
tb_ml$is_emulator <- as.factor(tb_ml$is_emulator)
tb_ml$fake_location <- as.factor(tb_ml$fake_location)
tb_ml$root_permission <- as.factor(tb_ml$root_permission)
tb_ml$official_store <- as.factor(tb_ml$official_store)
tb_ml$event_date <- NULL 
tb_ml$weekday <- as.factor(tb_ml$weekday)
tb_ml$wday <- NULL
tb_ml$age_category <- NULL
tb_ml$distance_group <- NULL
tb_ml$risk_level <- NULL

View(tb_ml)

# Set seed
set.seed(12345)

# Selecting rows according to variable IS_FRAUD
index <- createDataPartition(tb_ml$is_fraud, p = 0.75, list = FALSE)
dim(index)

# Setting training data as a subset
data_training <- tb_ml[index,]
dim(data_training)
table(data_training$is_fraud)

# Percentages between classes
prop.table(table(data_training$is_fraud))

# Percentage comparison between training classes and original dataset
data_comparison <- cbind(prop.table(table(data_training$is_fraud)), 
                       prop.table(table(tb_ml$is_fraud)))
colnames(data_comparison) <- c("Training", "Original")
data_comparison

# Melt Data - Convert columns to rows
melt_data_comparison <- melt(data_comparison)
melt_data_comparison

# Plot - Training vs original
ggplot(melt_data_comparison, aes(x = X1, y = value)) + 
  geom_bar( aes(fill = X2), stat = "identity", position = "dodge") + 
  ggtitle("Training vs original") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Everything not in the Training Dataset must be in the Test Dataset
data_test <- tb_ml[-index,]
dim(data_test)
dim(data_training)

# Building model version 1
View(data_training)
model_v1 <- randomForest(is_fraud ~ ., data = data_training)
model_v1
plot(model_v1)

# Predicting the test dataset
predict_v1 <- predict(model_v1, data_test)

# Confusion Matrix to calculate a cross-tabulation of observed and predicted classes
cm_v1 <- caret::confusionMatrix(predict_v1, data_test$is_fraud, positive = "1")
cm_v1

# Precision, Recall e F1-Score, measures to evaluate predict model
y <- data_test$is_fraud
y_pred_v1 <- predict_v1

precision <- posPredValue(y_pred_v1, y)
precision

recall <- sensitivity(y_pred_v1, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1

# SMOTE algorithm for unbalanced classification problems

table(data_training$is_fraud)
prop.table(table(data_training$is_fraud))
set.seed(9560)
#str(data_test)
#temp_data_training <- data_training 
#temp_data_training$official_store <- NULL

data_training_bal <- SMOTE(is_fraud ~ ., data  = data_training)                         
table(data_training_bal$is_fraud)
prop.table(table(data_training_bal$is_fraud))

# Building model version 2
model_v2 <- randomForest(is_fraud ~ ., data = data_training_bal)
model_v2
plot(model_v2)

# Predicting the test dataset
predict_v2 <- predict(model_v2, data_test)

# Confusion Matrix
cm_v2 <- caret::confusionMatrix(predict_v2, data_test$is_fraud, positive = "1")
cm_v2

# Precision, Recall e F1-Score
y <- data_test$is_fraud
y_pred_v2 <- predict_v2

precision <- posPredValue(y_pred_v2, y)
precision

recall <- sensitivity(y_pred_v2, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1

# Most important variables to predict
varImpPlot(model_v2)
imp_var <- importance(model_v2)
varImportance <- data.frame(Variables = row.names(imp_var), 
                            Importance = round(imp_var[ ,'MeanDecreaseGini'],2))
rankImportance <- varImportance %>% 
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))

# Ranking of Variables Importance
ggplot(rankImportance, 
       aes(x = reorder(Variables, Importance), 
           y = Importance, 
           fill = Importance)) + 
  geom_bar(stat='identity') + 
  ggtitle("Ranking of Variables Importance") +
  geom_text(aes(x = Variables, y = 0.5, label = Rank), 
            hjust = 0, 
            vjust = 0.55, 
            size = 4, 
            colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() 

# Building model version 3 with the most important variables
colnames(data_training_bal)
model_v3 <- randomForest(is_fraud ~ device_age + 
                           event_hour + 
                           distance_fl +
                           weekday +
                           official_store +
                           root_permission, 
                          data = data_training_bal)
model_v3
plot(model_v3)

# Predicting the test dataset
predict_v3 <- predict(model_v3, data_test)

# Confusion Matrix
cm_v3 <- caret::confusionMatrix(predict_v3, data_test$is_fraud, positive = "1")
cm_v3

# Precision, Recall e F1-Score
y <- data_test$is_fraud
y_pred_v3 <- predict_v3

precision <- posPredValue(y_pred_v3, y)
precision

recall <- sensitivity(y_pred_v3, y)
recall

F1 <- (2 * precision * recall) / (precision + recall)
F1

# Salve model file
saveRDS(model_v3, file = "Dados/model_v3_03.rds")

# Loading model
final_model <- readRDS("Dados/model_v3_03.rds")

# Predictions with new data from 3 events

# Events data
device_age <- c(0, 10, 100) 
event_hour <- c("4", "10", "11") 
distance_fl <- c(1000, 1, 0)
weekday <- c("Sunday","Monday","Sunday")
official_store <- c("true","true","true")
root_permission <- c("false","false","false")

new_events <- data.frame(device_age,
                         event_hour,
                         distance_fl,
                         weekday,
                         official_store,
                         root_permission)

new_events$weekday <- factor(new_events$weekday, levels = levels(data_training_bal$weekday))
new_events$official_store <- factor(new_events$official_store, levels = levels(data_training_bal$official_store))
new_events$root_permission <- factor(new_events$root_permission, levels = levels(data_training_bal$root_permission))
new_events$event_hour <- factor(new_events$event_hour, levels = levels(data_training_bal$event_hour))

# Predictions
pred_new_events <- predict(final_model, new_events)
df = data.frame(pred_new_events)
df$is_fraud <- ifelse(pred_new_events==0,"No","Yes")
View(df)

