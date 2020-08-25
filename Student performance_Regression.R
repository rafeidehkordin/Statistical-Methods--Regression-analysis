library(tidyverse)
library(ggcorrplot)
library(RColorBrewer)
library(lattice)
library(psych)
library(DataExplorer)
library(reshape2)
library(car)
library(caret)
library(data.table)
library(e1071) 
library(gridGraphics)
library(gridExtra)
library(cowplot)
library(lmtest)
library(gvlma)
library(dplyr)
library(packHV)
library(caTools)


df=read_csv("C:\\Users\\nazli.rafeidehkordi\\OneDrive - Northeastern University\\courses\\Summer 2020\\Project\\74977_169835_bundle_archive\\StudentsPerformance.csv")

df=df %>% mutate(eth=`race/ethnicity`,pr_edu=`parental level of education`,
                 prep=`test preparation course`,mscore=`math score`,
                 rscore=`reading score`,wscore=`writing score`)
df = df %>% select(eth, lunch, gender,pr_edu,prep,mscore,rscore, wscore)


# histogram and boxplot of th distribution of math score

hist_boxplot(df$mscore, main = "Math", xlab = "Math Scores");


# QQ-Plots for checking the normality of math score

qqnorm(df$mscore, main = "Normal Q-Q Plot of Math Scores");qqline(df$mscore); 

# Splitting into the Training set and Test set 

cols = c("gender","eth","pr_edu","lunch","prep" )
df[,cols] = df %>% select(all_of(cols)) %>% lapply(as.factor)

set.seed(123) # math scores
split = sample.split(df$mscore, SplitRatio = 0.75);
trainingdf = subset(df, split == TRUE);
testdf = subset(df, split == FALSE);



# Applying linear regressiion on the data

lin_reg=lm(mscore ~ wscore + eth + lunch +prep + gender,
         data = trainingdf);
summary(lin_reg);

predicted = predict(lin_reg, testdf[-6],interval = "prediction", level=0.95);
summary(predicted)


# Plotting prediction line and confidence intervals

df_bind = cbind(testdf, predicted)
p =ggplot(df_bind, aes( fit, mscore)) +
  geom_point() +
  stat_smooth(method = lm)
plot(p)

p + geom_line(aes(y = lwr))+ geom_line(aes(y = upr)) +
  xlab("Predicted math Scores") + ylab("Test math Scores")


# Applying Logistic regression to predict binary variable gender and test preparation 


gender_model = glm(gender ~., data = trainingdf, family = "binomial")
summary(gender_model)

prep_model= glm(prep ~., data = trainingdf, family = "binomial")
summary(gender_model)

# predict the test data to get the prediction power of logistic regression on gender and prepration
gender_pred = predict(gender_model, newdata = testdf, type = "response")
gender_pred_class =  factor(ifelse(gender_pred > 0.5, "female", "male"))
Conf_Matrix = addmargins(table(testdf$gender, gender_pred_class))
Conf_Matrix
caret::confusionMatrix(gender_pred_class, testdf$gender, positive="male")


prep_pred = predict(prep_model, newdata = testdf, type = "response")
pre_pred_class = factor(ifelse(prep_pred > 0.5, "completed", "none"))
Conf_Matrix = addmargins(table(testdf$prep, gender_pred_class))
Conf_Matrix
caret::confusionMatrix(pre_pred_class,testdf$prep, positive="completed")

