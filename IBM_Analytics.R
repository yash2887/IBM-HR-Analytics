fname <- file.choose()
fname
ibm <- read.csv(fname, header = T, stringsAsFactors = F)
ibm <- ibm1

# checking the structure 
str(ibm)

#viewing the summary of variables
summary(ibm)

#checking for missing values, We found that there are no missing values in the data
colSums(is.na(ibm))

# Plot 1
#Loading necessary packages
suppressMessages(library(ggplot2))
suppressMessages(library(grid))
suppressMessages(library(gridExtra))
suppressMessages(library(plyr))
suppressMessages(library(rpart))
suppressMessages(library(rpart.plot))
suppressMessages(library(randomForest))
suppressMessages(library(pROC))
suppressMessages(library(scales))
install.packages('dplyr')
library(dplyr)



g1 <- ggplot(ibm, 
             aes(x = MonthlyIncome, fill = Attrition)) + 
  geom_density(alpha = 0.7) + 
  theme(text = element_text(size=7)) +
  scale_fill_manual(values = c("green","red")) +
  labs(title = "Distribution of Monthly Income", x = "Monthly Income", y = "Density")

g2 <- ggplot(ibm, 
             aes(x = HourlyRate, fill = Attrition)) + 
  geom_density(alpha = 0.7) + 
  theme(text = element_text(size=7)) +
  scale_fill_manual(values = c("green","red")) + 
  labs(title = "Distribution of Hourly Rate", x = "Hourly Rate", y = "Density")

g3 <- ggplot(ibm, 
             aes(x = DailyRate, fill = Attrition)) + 
  geom_density(alpha = 0.7) + 
  theme(text = element_text(size=7)) +
  scale_fill_manual(values = c("green","red")) +
  labs(title = "Distribution of Daily Rate", x = "Daily Rate", y = "Density")

g4 <- ggplot(ibm, 
             aes(x = MonthlyRate, fill = Attrition)) + 
  geom_density(alpha = 0.7) + 
  theme(text = element_text(size=7)) +
  scale_fill_manual(values = c("green","red")) + 
  labs(title = "Distribution of Monthly Rate", x = "Monthly Rate", y = "Density")

grid.arrange(g1, g2, g3, g4, ncol = 2, nrow = 2)



#PLOT2
################################################################################
attri <- as.data.frame(table(ibm$Attrition))
piepercent<- round(100*(attri$Freq)/sum(attri$Freq), 1)

pie(attri$Freq,piepercent, main = "Employee Attrition", col = rainbow(length(attri$Freq)))
legend("topright", c("YES", "NO"), cex = 0.8,
       fill = rainbow(length(attri$Freq)))





#PLOT 3
#################################################################################
names(ibm)[1]<-"Age"
ggplot(ibm,aes(x=Age,fill=Gender))+geom_bar()+facet_grid(".~Attrition") + scale_fill_manual(values=c("#e15759", "#A5CFE9"))


#PLOT 4
################################################################################

ggplot(ibm,aes(x=BusinessTravel,fill=Attrition))+geom_bar()+facet_grid(".~Gender")



#PLOT5
#################################################################################

ggplot(ibm, 
       aes(y = YearsSinceLastPromotion, x = YearsAtCompany, colour = OverTime)) + 
  geom_jitter(size = 1, alpha = 0.9) + 
  geom_smooth(method = "gam") + 
  facet_wrap(~ Attrition) + 
  ggtitle("Attrition") + 
  scale_colour_manual(values = c("#e15759", "#A5CFE9")) + 
  theme(plot.title = element_text(hjust = 0.5))

#PLOT6
##################################################################################
install.packages("pacman") 
pacman::p_load( tidyverse,caret, randomForest,rpart, rpart.plot, ggthemes, treemap, treemapify, repr,magrittr,
               RColorBrewer, tree)



suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(rpart))
suppressPackageStartupMessages(library(rpart.plot))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(treemap))
suppressPackageStartupMessages(library(treemapify))
suppressPackageStartupMessages(library(repr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(tree))

############################

# IMPORTING DATA TO "original_df" variable.
original_df <- ibm



set.seed(142)
# # SHUFFELING THE DATA BEFORE SPLITTING
original_df <- original_df[sample(nrow(original_df)),]

# ENCODING THE ORIGINAL VARIABLES
original_df$BusinessTravel = factor(original_df$BusinessTravel,
                                    levels = c('Travel_Frequently', 'Travel_Rarely', 'Non-Travel'),
                                    labels = c(1, 2, 3))



# Changing the datatype from integer to factors from the ordinal variables.
cols <- c("Education", "EnvironmentSatisfaction", "JobInvolvement", "JobLevel",
          "JobSatisfaction", "PerformanceRating", "RelationshipSatisfaction", 
          "StockOptionLevel", "TrainingTimesLastYear", "WorkLifeBalance")

original_df[cols] <- lapply(original_df[cols], factor)

# Deleting unwanted columns
cols <- c("Over18", "EmployeeNumber", "EmployeeCount")

original_df[cols] <- NULL


# Splitting  data
trainIndex <- createDataPartition(original_df$Attrition, p=0.8, 
                                  list=FALSE, times=1)

train <- original_df[trainIndex,]
test <- original_df[-trainIndex,]



# Checking that both the training and testing sets have the same label proportions.
prop_train <- train %>% select(Attrition) %>% group_by(Attrition) %>% summarize(n=n()) %>%
  mutate(pct=round(prop.table(n), 2))

prop_test <- test %>% select(Attrition) %>% group_by(Attrition) %>% summarize(n=n()) %>%
  mutate(pct=round(prop.table(n), 2))

prop_train
prop_test


options(repr.plot.width=10, repr.plot.height=8) 

rpart.tree <- rpart(Attrition ~ ., data=train)
plot(rpart.tree, uniform=TRUE, branch=0.6, margin=0.05)
text(rpart.tree, all=TRUE, use.n=TRUE)
title("Training Set's Classification Tree")


# VARIABLE IMPORTANCE
var_imp <- data.frame(rpart.tree$variable.importance)
var_imp$features <- rownames(var_imp)
var_imp <- var_imp[, c(2, 1)]
var_imp$importance <- round(var_imp$rpart.tree.variable.importance, 2)
var_imp$rpart.tree.variable.importance <- NULL

colorCount <- length(unique(var_imp$features))
feature_importance <- var_imp %>%
  ggplot(aes(x=reorder(features, importance), y=importance, fill=features)) + geom_bar(stat='identity') + coord_flip() + 
  theme_minimal() + theme(legend.position="none", strip.background = element_blank(), strip.text.x = element_blank(), 
                          plot.title=element_text(hjust=0.5, color="black"), plot.subtitle=element_text(color="black"), plot.background=element_rect(fill= "white"),
                          axis.text.x=element_text(colour="black"), axis.text.y=element_text(colour="black"),
                          axis.title=element_text(colour="black"), 
                          legend.background = element_rect(fill="white",
                                                           size=0.5, linetype="solid", 
                                                           colour ="black")) + scale_fill_manual(values = colorRampPalette(brewer.pal(24, "Set2"))(colorCount)) + 
  geom_label(aes(label=paste0(importance, "%")), colour = "white",  hjust=0.6) + 
  labs(title="Feature Importance for our Decision Tree Model", x="Features", y="Importance")




feature_importance

View(var_imp)
write.csv(var_imp,'Importnace.csv')

