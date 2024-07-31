#aloha jem

names(df)

table(df$minedust)
df$minedust_dic = df$minedust
df$minedust_dic[df$minedust_dic == 2] = 1
confusionMatrix(as.factor(df$minedust_dic), as.factor(df$diagyr_dic))

table(df$metals)
df$metals_dic = df$metals
df$metals_dic[df$metals_dic == 2] = 1
confusionMatrix(as.factor(df$metals_dic), as.factor(df$diagyr_dic))

table(df$pest_all)
df$pest_all_dic = df$pest_all
df$pest_all_dic[df$pest_all_dic == 2] = 1
confusionMatrix(as.factor(df$pest_all_dic), as.factor(df$diagyr_dic))

glm.fit=glm(status ~ vgdf + ever_smok + age + study_name.x + jobno, family=binomial, data = df)
summary(glm.fit)
exp(0.20)

x=df %>% filter(pest_all == 1 | pest_all == 2)
View(x)
table(x$desc)

table(df$gasfumes_dic)
df$gasfumes_dic = df$gasfumes
df$gasfumes_dic[df$gasfumes_dic == 2] = 1
confusionMatrix(as.factor(df$gasfumes_dic), as.factor(df$diagyr_dic))

table(df$vgdf_dic)
df$vgdf_dic = df$vgdf
df$vgdf_dic[df$vgdf_dic == 2] = 1
confusionMatrix(as.factor(df$vgdf_dic), as.factor(df$status))

table(df$sol_chlorinated)
df$sol_chlorinated_dic = df$sol_chlorinated
df$sol_chlorinated_dic[df$sol_chlorinated_dic == 2] = 1
confusionMatrix(as.factor(df$sol_chlorinated_dic), as.factor(df$status))
