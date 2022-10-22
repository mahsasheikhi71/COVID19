library("tidyverse")

library("DT")

cases <- read_csv("data_cleaned.csv")
cases
cases <- cases %>% mutate_if(is.character, factor)
dim(cases)
summary(cases)
cases_sel <- cases
#Create class variable##########################################################
cases_sel <- cases_sel %>% mutate(bad = as.factor(spread_rate  > -0.23209 ))

cases_sel %>% pull(bad) %>% table()


cases_sel %>% 
  summarize(bad_pct = sum(bad == TRUE)/n()) %>%
  arrange(desc(bad_pct))

cases_sel 


counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% 
  rename(c(county = subregion))

cases_TX_scaled <- cases_sel  %>% mutate(county = county_name %>% 
                                           str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
summary(cases_TX_scaled)
counties_TX_clust <- counties_TX %>% left_join(cases_TX_scaled)

ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))
################################################################################
library(caret)
set.seed(2000)

#Hold out Test Data#############################################################
inTrain <- createDataPartition(y = cases_sel$bad, p = .9, list = FALSE)
cases_train <- cases_sel %>% slice(inTrain)
cases_test <- cases_sel %>% slice(-inTrain)
cases_train
cases_test



#Testing: Confusion Matrix and Confidence Interval for Accuracy

pred <- predict(fit, newdata = cases_test)
pred

confusionMatrix(data = pred, ref = cases_test$bad)

###########################Feature Selection####################################

library(FSelector)
cases_train %>%  chi.squared(bad ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head(n = 17)


cases_train <- cases_train %>% select(-c(death_per_case,county_name,deaths_per_1000,spread_rate,spread_rate,cases_per_1000,avg_confirmed,avg_deaths))
cases_train %>%  chi.squared(bad ~ ., data = .) %>% 
  arrange(desc(attr_importance)) %>% head(n = 10)

#cases_train <- cases_train %>% select(-death_per_case, -cases_per_1000)

#cases_train %>%  chi.squared(bad ~ ., data = .) %>% 
#  arrange(desc(attr_importance)) %>% head(n = 10)



################################################################################



library(caret)

fit <- cases_train %>%
  train(bad ~ .  ,
        data = . ,
        #method = "rpart",
        method = "rf",
        #method = "nb",
        trControl = trainControl(method = "cv", number = 10),
        tuneLength = 5
  )
fit
ggplot(fit)
library(rpart.plot)
rpart.plot(fit$finalModel, extra = 2)

varImp(fit)
imp <- varImp(fit, compete = FALSE)
imp
ggplot(imp)

cases_test <- cases_test %>% na.omit

cases_test$bad_predicted <- predict(fit, cases_test)
#### confusionMatrix############################################################

confusionMatrix(data = cases_test$bad_predicted, ref = cases_test$bad)

#### Plot the training:#########################################################
cases_TX_scaled_train <- cases_train  %>% mutate(county = county_name %>% 
                                                   str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
summary(cases_TX_scaled_train)
counties_TX_clust <- counties_TX %>% left_join(cases_TX_scaled_train)

ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))

#### Plot the test:#############################################################
cases_TX_scaled_test <- cases_test%>% mutate(county = county_name %>% 
                                               str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
summary(cases_TX_scaled_test)
counties_TX_clust <- counties_TX %>% left_join(cases_TX_scaled_test)
#### Plot the ground truth :#############################################################
ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))

#### Plot the predict :#############################################################
ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = bad_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))

#Model Comparison###############################################################
library(caret)
train_index <- createFolds(cases_train$bad, k = 10)

rpartFit <- cases_train %>% train(bad ~ .,
                                  data = .,
                                  method = "rpart",
                                  tuneLength = 10,
                                  trControl = trainControl(method = "cv", indexOut = train_index)
)

knnFit <-cases_train %>% train(bad ~ .,
                               data = .,
                               method = "knn",
                               preProcess = "scale",
                               tuneLength = 10,
                               trControl = trainControl(method = "cv", indexOut = train_index)
)

resamps <- resamples(list(
  CART = rpartFit,
  kNearestNeighbors = knnFit
))
summary(resamps)

difs <- diff(resamps)
difs

summary(difs)


#PART (Rule-based classifier):

rulesFit <- cases_train %>% train(bad ~ .,
                                  method = "PART",
                                  data = .,
                                  tuneLength = 5,
                                  trControl = trainControl(method = "cv", indexOut = train_index))
rulesFit



