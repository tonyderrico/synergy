#silica

names(df)
table(df$silica_dic)
df$silica_dic = df$silica
df$silica_dic[df$silica_dic == 2] = 1
sensitivity(as.factor(df$silica_dic), as.factor(df$diagyr_dic))
specificity(as.factor(df$silica_dic), as.factor(df$diagyr_dic))
negPredValue(as.factor(df$silica_dic), as.factor(df$diagyr_dic))
confusionMatrix(as.factor(df$silica_dic), as.factor(df$diagyr_dic))

library(pROC) # install with install.packages("pROC")
library(randomForest) # install with install.packages("randomForest")

## plot the data
plot(x=df$silica_dic, y=df$diagyr_dic)

## fit a logistic regression to the data...
glm.fit=glm(diagyr_dic ~ silica_dic, family=binomial, data = df)
lines(df$diagyr_dic, glm.fit$fitted.values)


#######################################
##
## draw ROC and AUC using pROC
##
#######################################

## NOTE: By default, the graphs come out looking terrible
## The problem is that ROC graphs should be square, since the x and y axes
## both go from 0 to 1. However, the window in which I draw them isn't square
## so extra whitespace is added to pad the sides.
roc(df$diagyr_dic, glm.fit$fitted.values, plot=TRUE)

## Now let's configure R so that it prints the graph as a square.
##
par(pty = "s") ## pty sets the aspect ratio of the plot region. Two options:
##                "s" - creates a square plotting region
##                "m" - (the default) creates a maximal plotting region
roc(df$diagyr_dic, glm.fit$fitted.values, plot=TRUE)

## NOTE: By default, roc() uses specificity on the x-axis and the values range
## from 1 to 0. This makes the graph look like what we would expect, but the
## x-axis itself might induce a headache. To use 1-specificity (i.e. the 
## False Positive Rate) on the x-axis, set "legacy.axes" to TRUE.
roc(df$diagyr_dic, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE)

## If you want to rename the x and y axes...
roc(df$diagyr_dic, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage")

## We can also change the color of the ROC line, and make it wider...
roc(df$diagyr_dic, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4)

## If we want to find out the optimal threshold we can store the 
## data used to make the ROC graph in a variable...
roc.info <- roc(df$diagyr_dic, glm.fit$fitted.values, legacy.axes=TRUE)
str(roc.info)
## and then extract just the information that we want from that variable.
roc.df <- data.frame(
  tpp=roc.info$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - roc.info$specificities)*100, ## fpp = false positive precentage
  thresholds=roc.info$thresholds)

head(roc.df) ## head() will show us the values for the upper right-hand corner
## of the ROC graph, when the threshold is so low 
## (negative infinity) that every single sample is called "obese".
## Thus TPP = 100% and FPP = 100%

tail(roc.df) ## tail() will show us the values for the lower left-hand corner
## of the ROC graph, when the threshold is so high (infinity) 
## that every single sample is called "not obese". 
## Thus, TPP = 0% and FPP = 0%

## now let's look at the thresholds between TPP 60% and 80%...
roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]

## We can calculate the area under the curve...
roc(df$diagyr_dic, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

## ...and the partial area under the curve.
roc(df$diagyr_dic, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE, print.auc.x=45, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

#######################################
##
## Now let's fit the data with a random forest...
##
#######################################
rf.model <- randomForest(factor(df$diagyr_dic) ~ df$silica_dic)

## ROC for random forest
roc(df$diagyr_dic, rf.model$votes[,1], plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)


#######################################
##
## Now layer logistic regression and random forest ROC graphs..
##
#######################################
roc(obese, glm.fit$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)

plot.roc(obese, rf.model$votes[,1], percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)
legend("bottomright", legend=c("Logisitic Regression", "Random Forest"), col=c("#377eb8", "#4daf4a"), lwd=4)


#######################################
##
## Now that we're done with our ROC fun, let's reset the par() variables.
## There are two ways to do it...
##
#######################################
par(pty = "m")