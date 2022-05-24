
# Installing and loading the relevant packages

install.packages("mlr3")
install.packages("mlr3learners")
install.packages("mlr3viz")
install.packages("precrec")

library(precrec)
library(dplyr)
library("mlr3")
library("mlr3learners")
library("mlr3pipelines")
library("mlr3viz")
library(ggplot2)

setwd("Prog for DS/Coursework PG/sourcecode/") #Setting working directory

# Importing datasets

fullset <- read.csv('R fullset cleaned.csv')
planes <- read.csv('../datasets/plane data cleaned.csv')

# Subset of required columns
# At the same time, we sample out 800,000 rows for building predictive model
# (due to computational power limits, the model cannot be built using the entire dataset)

model1 <- sample_n(fullset[, c('Year', 'Month', 'DayofMonth', 'DayOfWeek', 'CRSDepTime', 
                    'TailNum', 'DepDelay', 'Origin', 'Dest')], 800000)
model2 <- planes[c('tailnum', 'year')]
colnames(model2) <- c('TailNum', 'YearOfMf') #Renaming columns to allow for merging

# Sampling out 2 million rows for building predictive model
# (due to computational power limits, the model cannot be built using the entire dataset)
# Creating dummy variable, if DepDelay>15 then 1 (DepDelayed), 0 otherwise
model1 <- model1 %>% mutate('DepDelayed' = if_else(model1$DepDelay > 15, 1, 0))

model <- merge(model1, model2, by='TailNum') #Merging with pane data to get year of manufacture

dplyr::count(model, YearOfMf, sort = TRUE) #Checking for consistency in values

model <- subset(model, YearOfMf != '0' & YearOfMf != 'None') #Removing inconsistent values

# Data selection
ml_data <- model[, c('Year', 'Month', 'DayofMonth', 'DayOfWeek', 'CRSDepTime', 'DepDelayed', 'YearOfMf', 'Origin', 'Dest')]
numerical_data <- c('CRSDepTime')
categorical_data <- c('Year', 'Month', 'DayofMonth', 'DayOfWeek', 'YearOfMf', 'DepDelayed', 'Origin', 'Dest')

# Clearing memory by removing variables not used anymore
rm(fullset)
rm(model)
rm(model1)
rm(model2)
rm(planes)

for(column in categorical_data) {
  ml_data[,column] <- as.factor(ml_data[[column]]) 
}
# We convert to factor because the pipeline treats factors as categorical variables
# and vectors as numerical variables.

rm(column)

# Model setup
task <- TaskClassif$new('Delay Prediction', backend=ml_data, target = 'DepDelayed', positive = "1")
measure <- msr('classif.auc') 

# Building a logistic regression model
learner_logreg <- lrn('classif.log_reg', predict_type = "prob")

# Pipeline setup

scale_data <- po("scale", affect_columns = selector_type("numeric")) 
ohencode <- po("encode", affect_columns = selector_type("factor"))

# Building pipeline
lr_pipe <- po("featureunion") %>>%  
  scale_data %>>%
  ohencode %>>%
  po(learner_logreg)

lr_pipe <- GraphLearner$new(lr_pipe)

# Splitting dataset into train and test sets

# Selecting 50% of the data as training set, 
# seed=1 allows the first sample to be replicated each time this code is run.

set.seed(1)
train_set <- sample(task$nrow, 0.5 * task$nrow) 
test_set <- setdiff(seq_len(task$nrow), train_set)

# Training the model
lr_pipe$train(task, row_ids = train_set)

# Evaluating model
prediction <- lr_pipe$predict(task, row_ids = test_set)
prediction$score(measure) # Gives AUC score

# Plotting ROC Curve
autoplot(prediction, type = "roc")
ggsave('R - roc.png', width=5, height=5)  

