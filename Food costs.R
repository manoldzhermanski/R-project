library(ggplot2)
survey_data <- read.csv("D:\\R project\\Food costs.csv")

#Analyzing the categorial variables

#Showcasing the percentage ration between the genders

gender_distribution <- table(survey_data$Gender.)
gender_distribution_percentage <- prop.table(gender_distribution)
percent <- round(gender_distribution_percentage*100,1)
percent <-paste(percent,"%",sep = " ")

pie(gender_distribution,col = c("pink", "powderblue"),main ="Gender Distribution",labels = percent, border = 0)
legend("bottomright",c("Female","Male"), fill =c("pink", "powderblue"),cex = 0.8)

#Showcasing the labor status though a barplot

working_status_percentage <-prop.table(table(survey_data$Do.you.work..))

working_status <-barplot(working_status_percentage,main = "Do you work ?", xlab = "Answer", ylab = "Frequency",col = c("firebrick1","lightskyblue"),border = 0, ylim =c(0,1))
text(working_status,0, table(survey_data$Do.you.work..),cex = 1.5,pos=3) 

#Analyzing first numeric variable

boxplot(survey_data$Salary.Pocket.Money, las = 1, col = I("palegreen1"), main = "Salary / Pocket Money", horizontal = TRUE, xlab = "Lev")

summary(survey_data$Salary.Pocket.Money)

round(sd(survey_data$Salary.Pocket.Money),2)
mad(survey_data$Salary.Pocket.Money)
IQR(survey_data$Salary.Pocket.Money)

qqnorm(x, pch = 1, main = "Check for normal distribution", xlab = "Salary / Pocket Money")
qqline(x, col = "steelblue", lwd = 2)

shapiro.test(x)

#Analyzing second numeric variable

plot(density(survey_data$Groceries..), lwd = 2, main = "Money spent on groceries", xlab = "Lev", ylab = "Density",
     col = "lightblue", xlim = range(survey_data$Groceries..))

options(scipen=999)
abline(v = fivenum(survey_data$Groceries..), lwd = c(1.5, rep(2, 3), 1.5), 
       col = c("black","plum", "purple", "plum", "black"),lty = c(1, rep(3, 3), 1))

summary(survey_data$Groceries..)

round(sd(survey_data$Groceries..),2)
mad(survey_data$Groceries..)
IQR(survey_data$Groceries..)

qqnorm(survey_data$Groceries.., pch = 1, main = "Check for normal distribution", xlab = "Money spent on groceries")
qqline(survey_data$Groceries.., col = "steelblue", lwd = 2)

shap

#Analyzing third numeric variable

boxplot(survey_data$Eating.out.., las = 1, col = I("mediumorchid2"), main = "Money spent eating out", xlab = "Lev")

summary(survey_data$Eating.out..)

round(sd(survey_data$Eating.out..),2)
mad(survey_data$Eating.out..)
IQR(survey_data$Eating.out..)

qqnorm(survey_data$Eating.out.., pch = 1, main = "Check for normal distribution", xlab = "Money spent on eating out")
qqline(survey_data$Eating.out.., col = "steelblue", lwd = 2)

shapiro.test(survey_data$Eating.out..)


# Cathegorial vs numeric

# GENDER - SALARY
survey_data_frame = data.frame(survey_data)

salary_by_gender = boxplot(survey_data_frame$Salary.Pocket.Money[survey_data_frame$Gender. == "Male"],survey_data_frame$Salary.Pocket.Money[survey_data_frame$Gender. == "Female"],names = c("Male","Female"),col = c("powderblue","pink"), main = "Salary/ Pocket Money by Gender",ylab = "Lev",xlab = "Gender")
wilcox.test(survey_data_frame$Salary.Pocket.Money[survey_data_frame$Gender. == "Male"],survey_data_frame$Salary.Pocket.Money[survey_data_frame$Gender. == "Female"], paired = F,exact = T,conf.int = T)
#GENDER-GROCERIES

groceiries_by_gender = boxplot(survey_data_frame$Groceries..[survey_data_frame$Gender. == "Male"],survey_data_frame$Groceries..[survey_data_frame$Gender. == "Female"],names = c("Male","Female"),col = c("powderblue","pink"), main = "Money spent on groceries by Gender",ylab = "Lev",xlab = "Gender")
t.test(survey_data_frame$Groceries..[survey_data_frame$Gender. == "Male"],survey_data_frame$Groceries..[survey_data_frame$Gender. == "Female"], paired = F,exact = T,conf.int = T)

#GENDER-EATING OUT
eating_out_by_gender = boxplot(survey_data_frame$Eating.out..[survey_data_frame$Gender. == "Male"],survey_data_frame$Eating.out..[survey_data_frame$Gender. == "Female"],names = c("Male","Female"),col = c("powderblue","pink"), main = "Money spent on eating out by Gender",ylab = "Lev",xlab = "Gender")
wilcox.test(survey_data_frame$Eating.out..[survey_data_frame$Gender. == "Male"],survey_data_frame$Eating.out..[survey_data_frame$Gender. == "Female"], paired = F,exact = T,conf.int = T)

# DO_YOU_WORK - SALARY

salary_by_working_status = boxplot(survey_data_frame$Salary.Pocket.Money[survey_data_frame$Do.you.work.. == "Yes"],survey_data_frame$Salary.Pocket.Money[survey_data_frame$Do.you.work.. == "No"],names = c("Yes","No"),col = c("springgreen3","red3"), main = "Salary/ Pocket Money by working status",ylab = "Lev",xlab = "Do you work ?")
wilcox.test(survey_data_frame$Salary.Pocket.Money[survey_data_frame$Do.you.work.. == "Yes"],survey_data_frame$Salary.Pocket.Money[survey_data_frame$Do.you.work.. == "No"], paired = F,exact = T,conf.int = T)

#DO YOU WORK-GROCERIES

groceries_by_working_status = boxplot(survey_data_frame$Groceries..[survey_data_frame$Do.you.work.. == "Yes"],survey_data_frame$Groceries..[survey_data_frame$Do.you.work.. == "No"],names = c("Yes","No"),col = c("springgreen3","red3"), main = "Money spent on groceires by working status ?",ylab = "Lev",xlab = "Do you work ?")
t.test(survey_data_frame$Groceries..[survey_data_frame$Do.you.work.. == "Yes"],survey_data_frame$Groceries..[survey_data_frame$Do.you.work.. == "No"], paired = F,exact = T,conf.int = T)

#DO YOU WORK-EATING OUT
eating_out_by_working_status = boxplot(survey_data_frame$Eating.out..[survey_data_frame$Do.you.work.. == "Yes"],survey_data_frame$Eating.out..[survey_data_frame$Do.you.work.. == "No"],names = c("Yes","No"),col = c("springgreen3","red3"), main = "Money spent on eating out by working status",ylab = "Lev",xlab = "Do you work ?")
wilcox.test(survey_data_frame$Eating.out..[survey_data_frame$Do.you.work.. == "Yes"],survey_data_frame$Eating.out..[survey_data_frame$Do.you.work.. == "No"], paired = F,exact = T,conf.int = T)
summary(eating_out_by_working_status$stats)


# Numeric vs numeric

install.packages("ggpubr")
library("ggpubr")

#Salary - Groceries

cor.test(survey_data_frame$Salary.Pocket.Money,survey_data_frame$Groceries..,method = "pearson")
ggscatter(survey_data_frame, x = 'Salary.Pocket.Money', y = 'Groceries..', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = c("pearson"),
          xlab = "Salary/Pocket Money", ylab = "Money spent on groceries")

summary(lm(survey_data$Groceries..~survey_data$Salary.Pocket.Money))
#Salary - Eating out

cor.test(survey_data_frame$Salary.Pocket.Money,survey_data_frame$Eating.out..,method = "spearman")
ggscatter(survey_data_frame, x = 'Salary.Pocket.Money', y = 'Eating.out..', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = c("spearman"),
          xlab = "Salary/Pocket Money", ylab = "Money spent on eating out")


#Groceries- Eating out

cor.test(survey_data_frame$Groceries..,survey_data_frame$Eating.out..,method = "spearman") # rho is 1 -> positive corelativity
ggscatter(survey_data_frame, x = 'Groceries..', y = 'Eating.out..', 
          add = 'reg.line', conf.int = TRUE, 
          cor.coef = TRUE, cor.method = c("spearman"),
          xlab = "Money spent on groceries", ylab = "Money spent on eating out")


