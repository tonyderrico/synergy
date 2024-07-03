library(dplyr)
library(caret)
library("pROC")

str(syn_all_occ_hist)
names(syn_all_subjects)

syn_all_subjects$diagyr_dic <- ifelse(is.na(syn_all_subjects$diagyr), 0, 1)
table(syn_all_subjects$diagyr_dic, syn_all_subjects$farmer)
df = syn_all_subjects %>% select(subjctid, diagyr_dic, diagyr)
df = merge(syn_all_occ_hist, df, by = "subjctid")

#asbestos
names(df)
table(df1$asbestos_dic)
df1$asbestos_dic = df1$asbestos
df1$asbestos_dic[df1$asbestos_dic == 2] = 1
sensitivity(as.factor(df$asbestos_dic), as.factor(df$diagyr_dic))
specificity(as.factor(df$asbestos_dic), as.factor(df$diagyr_dic))
negPredValue(as.factor(df$asbestos_dic), as.factor(df$diagyr_dic))
confusionMatrix(as.factor(df1$asbestos_dic), as.factor(df1$diagyr_dic))

roc_obj = roc(df1$diagyr_dic,df1$asbestos_dic)
# Plot the ROC curve
plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve",
     xlab = "Specificity (False Positive Rate)", ylab = "Sensitivity (True Positive Rate)")

# Add diagonal reference line (Random Classifier)
abline(a = 0, b = 1, col = "red", lty = 2)

# Calculate and print the AUC
auc_value <- auc(roc_curve)
print(paste("AUC:", round(auc_value, 2)))

# Add AUC value to the plot
text(0.6, 0.2, paste("AUC =", round(auc_value, 2)), col = "blue", cex = 1.2)


x = df %>% filter(subjctid == '101031034')
x = df %>% filter(subjctid == '101031034')
table(x$diagyr_dic,x$asbestos)
table(df$year)

df1 = df %>% filter(year == '1980')
