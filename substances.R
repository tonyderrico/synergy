#pah

names(df)
table(df$pah_dic)
df$pah_dic = df$pah
df$pah_dic[df$pah_dic == 2] = 1
confusionMatrix(as.factor(df$pah_dic), as.factor(df$diagyr_dic))

table(df1$pah_dic)
df1$pah_dic = df1$pah
df1$pah_dic[df1$pah_dic == 2] = 1
confusionMatrix(as.factor(df1$pah_dic), as.factor(df1$diagyr_dic))

#nickel
names(df)
table(df$nickel)
df$nickel_dic = df$nickel
df$nickel_dic[df$nickel_dic == 2] = 1
confusionMatrix(as.factor(df$nickel_dic), as.factor(df$diagyr_dic))

#smoking
names(df)
table(df$smoking_dic)
df$smoking_dic = df$smoking
df$smoking_dic[df$smoking_dic == 2] = 1
confusionMatrix(as.factor(df$ever_smok), as.factor(df$diagyr_dic))

df$smoking[is.na(df$smoking)] <- 0
table(df$smok_duration)
plot(x=df$smok_duration, y=df$diagyr_dic)
glm.fit=glm(diagyr_dic ~ smok_duration, family=binomial, data = df)
lines(df$smok_duration, glm.fit$fitted.values)
roc.info=roc(df$diagyr_dic, df$smok_duration,plot=TRUE)

roc.df <- data.frame(
  tpp=roc.info$sensitivities*100, ## tpp = true positive percentage
  fpp=(1 - roc.info$specificities)*100, ## fpp = false positive precentage
  thresholds=roc.info$thresholds)

roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]
roc(df$diagyr_dic, df$smok_duration, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE, print.auc.x=45, partial.auc=c(80,70), auc.polygon = TRUE, auc.polygon.col = "#377eb822")

