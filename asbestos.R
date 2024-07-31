library(dplyr)
library(caret)
library("pROC")
library(randomForest)

#preprocessing
str(syn_all_occ_hist)
names(syn_all_subjects)
names(df)
df$isco

syn_all_subjects$diagyr_dic <- ifelse(is.na(syn_all_subjects$diagyr), 0, 1)
table(syn_all_subjects$diagyr_dic, syn_all_subjects$farmer)
df = syn_all_subjects %>% select(subjctid, diagyr_dic, diagyr)
df = merge(df, syn_all_subjects, by = "subjctid")
synergy.job.history_isco88.2 = synergy.job.history_isco88 %>% select('subjctid',
                                'isco68','ISCO88', 'year')
df = merge(syn_all_occ_hist,synergy.job.history_isco88.2, by = c("subjctid",'year'))
names(syn_all_occ_hist)
table(syn_all_subjects$diagyr_dic)
table(syn_all_subjects$sex)
table(syn_all_subjects$ever_asbestos0)

df = merge(df,ALOHA_ISCO_88_JEM_v2_0, by='ISCO88')
names(DOM_JEM_isco88_20210629)[1] = 'ISCO88'
#asbestos - all years
names(df)
table(df$asbestos.x)
df$asbestos_dic = df$
df$asbestos_dic[df$asbestos_dic == 2] = 1
tb = table(df$asbestos_dic,df$diagyr_dic)
confusionMatrix(tb)
hist(df)

#different test - 1980 SA
x = df %>% filter(subjctid == '101031034')
x = df %>% filter(subjctid == '101031034')
table(x$diagyr_dic,x$asbestos)

df1 = df %>% filter(year == '1980')
table(df1$asbestos_dic)
df1$asbestos_dic = df1$asbestos
df1$asbestos_dic[df1$asbestos_dic == 2] = 1
tb1 = table(df1$asbestos_dic,df1$diagyr_dic)
confusionMatrix(tb1)

roc_obj = roc(as.factor(df1$diagyr_dic),df1$asbestos_cum0)
x=as.data.frame(roc_obj$thresholds)
# Plot the ROC curve
plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve",
     xlab = "Specificity (False Positive Rate)", ylab = "Sensitivity (True Positive Rate)")

# Add diagonal reference line (Random Classifier)
abline(a = 0, b = 1, col = "red", lty = 2)

# Calculate and print the AUC
auc_value <- auc(roc_obj)
print(paste("AUC:", round(auc_value, 2)))

# Add AUC value to the plot
text(0.6, 0.2, paste("AUC =", round(auc_value, 2)), col = "blue", cex = 1.2)

## fit a logistic regression to the data...
glm.fit=glm(diagyr_dic ~ asbestos_cum0, family=binomial, data = df)
predprob = predict(glm.fit, type = 'response') #other option, same
lines(df$diagyr_dic, glm.fit$fitted.values)
roc_curve=roc(df$diagyr_dic, glm.fit$fitted.values, plot=TRUE)
optimal_threshold <- coords(roc_curve, "best", ret = "threshold", best.method = "youden")
print(paste("Optimal Threshold: ", optimal_threshold))
abline(v = optimal_threshold, col = "blue", lty = 2)
summary(roc_curve$thresholds > 0.44)
summary(glm.fit$fitted.values > 0.44)
#######################################
##
## draw ROC and AUC using pROC
##
#######################################

## NOTE: By default, the graphs come out looking terrible
## The problem is that ROC graphs should be square, since the x and y axes
## both go from 0 to 1. However, the window in which I draw them isn't square
## so extra whitespace is added to pad the sides.
rc = roc(df$diagyr_dic, predprob, plot=TRUE)
hist(rc$sensitivities)
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
table(roc_obj$thresholds)
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
rf.model <- randomForest(df$diagyr_dic ~ df$asbestos_cum0)

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

#thresholds!
roc.df <- data.frame(
  tpp=roc_obj$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - roc_obj$specificities)*100, ## fpp = false positive precentage
  thresholds=roc_obj$thresholds)

roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]
roc(df$diagyr_dic, df$smok_duration, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE, print.auc.x=45, partial.auc=c(80,70), auc.polygon = TRUE, auc.polygon.col = "#377eb822")


# Create a data frame for thresholds, TPR, and FPR
threshold_data <- data.frame(
  Threshold = thresholds,
  TPR = tpr,
  FPR = 1 - fpr # FPR is 1 - specificity
)

ggplot(threshold_data, aes(x = Threshold)) +
  geom_line(aes(y = TPR, color = "TPR")) +
  geom_line(aes(y = FPR, color = "FPR")) +
  geom_vline(xintercept = optimal_threshold, linetype = "dashed", color = "red") +
  labs(title = "TPR and FPR vs. Threshold",
       x = "Threshold",
       y = "Rate") +
  theme_minimal() +
  scale_color_manual(values = c("TPR" = "blue", "FPR" = "orange"), name = "Rate")
