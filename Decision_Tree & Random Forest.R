# preparation growing trees ====
library("tidyverse")
library("rpart")
library("rpart.plot")
library("rattle")
library("partykit")
library("kernlab")

# growing simple trees =====

## Load data-----------

head(smoking) 
dim(smoking)

# tiny tree
tree_smoke <- rpart(intention_to_smoke ~ ., data = smoking)

# simple plots
plot(tree_smoke)
text(tree_smoke)

# fancy plots
fancyRpartPlot(tree_smoke, sub = NULL)
prp(tree_smoke, type = 4, extra = 6, faclen = 0)
plot(as.party(tree_smoke))

## A much bigger (unpruned) tree
controlpar <- rpart.control(minsplit = 2, minbucket = 1, cp = 0, 
                            maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
                            xval = 0, surrogatestyle = 0, maxdepth = 30)
fit_big <- rpart(intention_to_smoke ~ ., data = smoking, control = controlpar)
prp(fit_big, type = 4, extra = 6, faclen = 0)


# growing regression trees =====
library("plotly")
library("shiny")  
runGitHub("schoonees/trees")  

head(Hitters)
dim(Hitters)

## Remove some missings, plot
Hitters <- Hitters[!is.na(Hitters$Salary), ]
dim(Hitters)
ggplot(Hitters, aes(x = Years, y = Hits, colour = Salary)) + 
  geom_point()

## Grow and plot a simple tree
tree_hitters <- rpart(log(Salary)  ~ Years + Hits, data = Hitters, 
                      control = rpart.control(minsplit = 100))
fancyRpartPlot(tree_hitters, sub = NULL)
plot(as.party(tree_hitters))

## Grow a larger tree
set.seed(123)
tree_hitters_all <- rpart(log(Salary) ~ ., data = Hitters,
                          control = rpart.control(cp = 0.004))
prp(tree_hitters_all)


## Pruning information from cross-validation
plotcp(tree_hitters_all) 
printcp(tree_hitters_all)

## Simplest tree within 1 standard error of the best
ind_min <- which.min(tree_hitters_all$cptable[, "xerror"])
err_thres <- tree_hitters_all$cptable[ind_min, "xerror"] +
  tree_hitters_all$cptable[ind_min, "xstd"]
ind_1sd <- which(tree_hitters_all$cptable[, "xerror"] <= err_thres)[1]
tree_hitters_all_1sd <- prune(tree_hitters_all, 
                              cp = tree_hitters_all$cptable[ind_1sd, "CP"]) 
plot(as.party(tree_hitters_all_1sd))

## Best tree according to CV
cpmin <- which.min(tree_hitters_all$cptable[, "xerror"])
tree_hitters_all_min <- prune(tree_hitters_all, 
                              cp = tree_hitters_all$cptable[cpmin, "CP"]) 
prp(tree_hitters_all_min)

# growing classification trees =====

## Spam data
library("kernlab")
data("spam")
head(spam, 3)
dim(spam)

## Large tree
set.seed(1)
fit_spam <- rpart(type ~ ., data = spam, control = rpart.control(cp = 0.0001))

## CV information
printcp(fit_spam)
plotcp(fit_spam)

## Apply 1 standard error rule
ind_min <- which.min(fit_spam$cptable[, "xerror"])
err_1sd <- fit_spam$cptable[ind_min, "xerror"] + fit_spam$cptable[ind_min, "xstd"]
ind_1sd <- which(fit_spam$cptable[, "xerror"] < err_1sd)[1]
fit_spam_pruned <- prune(fit_spam, cp = fit_spam$cptable[ind_1sd, "CP"])
prp(fit_spam_pruned)

## Training set predictions for classification tree (use newdata = test_data for test predictions)
predict(fit_spam_pruned)
predict(fit_spam_pruned, type = "class")


# growing random forest trees ====

load("bookings.RData")
df<-bookings
y<-1
xvars<-c(2:18)


# Splits data in train/test (60/40)
set.seed(1)
smpl<-sample(nrow(df))
data.perm<-df[smpl,]

ntrain<-round(.6*nrow(df))
data.train<-data.perm[1:ntrain,]
data.test<-data.perm[(ntrain+1):nrow(df),]



# tuning random forest
library(caret)
set.seed(1)
ctrl<-trainControl(method = "repeatedcv",
                   number = 10, repeats = 10,
                   selectionFunction = 'best',
                   savePredictions = TRUE,
                   classProbs = TRUE,
                   summaryFunction = twoClassSummary)

grid_rf<-expand.grid(mtry=c(2,4,6,8,10,12,14,16))

m_rf_1<-train(is_canceled~., data=data.train, method="rf",
              metric= "ROC", trControl=ctrl,
              tuneGrid = grid_rf)
#choose based on highest ROC

## Bagging for the booking data
set.seed(1)
bagging <- randomForest(is_canceled ~ ., data = data.train, mtry = 17, 
                        ntree = 250, importance=TRUE, nPerm = 10, do.trace = FALSE)

# Optimal mtry Random forest
rf_optimal <- randomForest(is_canceled ~ ., data = data.train, mtry = 6, 
                           ntree = 250, importance=TRUE, nPerm = 10, do.trace = FALSE)


## Out-of-bag performance: plot
oob <- data.frame(bagging = bagging$err.rate[, "OOB"],
                  rf_optimal = rf_optimal$err.rate[, "OOB"], 
                  trees = 1:250)

oob_long <- gather(oob, method, error, -trees) # pivot_longer() is the newer version of gather()
ggplot(oob_long, aes(x = trees, y = error, colour = method)) +
  geom_line()



#prediction
rf_optimal_pred<-predict(rf_optimal, newdata=data.test)
bagging_pred<-predict(bagging, newdata=data.test)

performance <- data.frame(prediction_rf = rf_optimal_pred,
                          prediction_bg = bagging_pred,
                          actuals =data.test$is_canceled)



#results comparison
confusionMatrix(data=bagging_pred, reference=data.test$is_canceled)
bg_mis<-table(bagging_pred, data.test$is_canceled)
(err<-1-(sum(diag(bg_mis))/sum(bg_mis)))

confusionMatrix(data=rf_optimal_pred, reference=data.test$is_canceled)
rd_mis<-table(rf_optimal_pred, data.test$is_canceled)
(err<-1-(sum(diag(rd_mis))/sum(rd_mis)))


## Variable importance data frame
imp <- randomForest::importance(rf_optimal) 
imp <- tibble(Importance = imp[, "MeanDecreaseAccuracy"],
              Variable = rownames(imp)) 
imp <- arrange(imp, Importance)
imp <- mutate(imp, Variable = fct_inorder(Variable))

## Order from large to small
ggplot(imp, aes(x = Variable, y = Importance)) + geom_point() +
  theme() + coord_flip()

## Variable importance 
varImpPlot(rf_optimal, type = 2)
randomForest::importance(rf_optimal)  

## OOB error rates 1) misclassification rate, specificity and sensitivity
rf_optimal$err.rate
plot(rf_3, lwd = 2)
legend("topright", col = 1:3, lty = 1:3, legend = c("OOB", "No", "Yes"), lwd = 2)


