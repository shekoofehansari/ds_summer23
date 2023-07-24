## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
knit_hooks$set(purl = hook_purl)

## ----1, include=FALSE---------------------------------------------------------
setwd("C:/Users/ansar/Desktop/final/")
library("wordcountaddin")

## ---- echo=F, message=FALSE---------------------------------------------------
#Wordcount:
wordcount <- wordcountaddin::word_count( )

## -----------------------------------------------------------------------------
df <- read.csv("C:/Users/ansar/Desktop/final/diabetes.csv")
head(df)

## -----------------------------------------------------------------------------
df <- read.csv("C:/Users/ansar/Desktop/final/diabetes.csv")
missing_data <- df[,setdiff(names(df), c('Outcome'))]
features_miss_num <- apply(missing_data, 2, function(x) sum(x <= 0))
features_miss <- names(missing_data)[ features_miss_num > 0]

rows_miss <- apply(missing_data, 1, function(x) sum(x <= 0) >= 1) 
summary(rows_miss)

## -----------------------------------------------------------------------------
summary(missing_data)

## -----------------------------------------------------------------------------
cat("Number of missing value:", sum(is.na(df)), "\n")

## -----------------------------------------------------------------------------
df <- read.csv("C:/Users/ansar/Desktop/final/diabetes.csv")

row_sub = apply(df, 1, function(row) all(row !=0 ))

## -----------------------------------------------------------------------------
ID <- 1:20

library(dplyr)

df_grp <-  df %>% group_by(Pregnancies)  %>%
  summarise(Frequencies = length(Pregnancies),
            .groups = 'drop')

library(ggplot2)
ggplot(df_grp, aes(x=Pregnancies, y=Frequencies)) + 
  geom_bar(stat = "identity")  +
  scale_x_continuous("Pregnancies", labels = as.character(ID), breaks = ID)+
  geom_text(aes(label = Frequencies), vjust = -0.2)
  

## -----------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
df_bmi <-  missing_data %>% group_by(BMI)  %>%
  summarise(Frequencies = length(BMI),
            .groups = 'drop')
P15 <- filter(df_bmi, BMI >= 18 & BMI <= 70)
ggplot(data = P15 ,mapping = aes(x = BMI, y = Frequencies ))  +
  geom_point()

## -----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
df_rt <-  df %>% group_by(Age)  %>%
  summarise(Frequencies = length(Age),
            .groups = 'drop')
df_rt %>% 
    group_by(Age) %>% 
    mutate(mean_age = mean(Age)) %>% 
    ggplot(aes(x = Age, y = Frequencies)) +
    geom_point(color = "purple", shape = 18) +
  geom_text(aes(label = Frequencies), vjust = -0.2)

## -----------------------------------------------------------------------------
library(corrgram)
corrgram(missing_data, order=TRUE, lower.panel= panel.shade ,
  upper.panel= panel.pie, text.panel=panel.txt,
  main="Correlation Matrix")

## -----------------------------------------------------------------------------
df$BloodPressure <- NULL
df$SkinThickness <- NULL
train <- df[1:540,]
test <- df[541:768,]
model <-glm(Outcome ~.,family=binomial(link='logit'),data=train)
summary(model)

## -----------------------------------------------------------------------------
diabetes <- read.csv("C:/Users/ansar/Desktop/final/diabetes.csv")
attach(diabetes)
par(mfrow=c(2,4))
boxplot(Pregnancies~Outcome, main="No. of Pregnancies vs. Diabetes", 
        xlab="Outcome", ylab="Pregnancies",col="red")
boxplot(Glucose~Outcome, main="Glucose vs. Diabetes", 
        xlab="Outcome", ylab="Glucose",col="pink")
boxplot(BloodPressure~Outcome, main="Blood Pressure vs. Diabetes", 
        xlab="Outcome", ylab="Blood Pressure",col="green")
boxplot(SkinThickness~Outcome, main="Skin Thickness vs. Diabetes", 
        xlab="Outcome", ylab="Skin Thickness",col="orange")
boxplot(Insulin~Outcome, main="Insulin vs. Diabetes", 
        xlab="Outcome", ylab="Insulin",col="yellow")
boxplot(BMI~Outcome, main="BMI vs. Diabetes", 
        xlab="Outcome", ylab="BMI",col="purple")
boxplot(DiabetesPedigreeFunction~Outcome, main="Diabetes Pedigree Function vs. Diabetes", xlab="Outcome", ylab="DiabetesPedigreeFunction",col="lightgreen")
boxplot(Age~Outcome, main="Age vs. Diabetes", 
        xlab="Outcome", ylab="Age",col="lightblue")
box(which = "outer", lty = "solid")

## -----------------------------------------------------------------------------
attach(missing_data)
par(mfrow=c(2,4))
boxplot(Pregnancies~Outcome, main="No. of Pregnancies vs. Diabetes", 
        xlab="Outcome", ylab="Pregnancies")
boxplot(Glucose~Outcome, main="Glucose vs. Diabetes", 
        xlab="Outcome", ylab="Glucose")
boxplot(BloodPressure~Outcome, main="Blood Pressure vs. Diabetes", 
        xlab="Outcome", ylab="Blood Pressure")
boxplot(SkinThickness~Outcome, main="Skin Thickness vs. Diabetes", 
        xlab="Outcome", ylab="Skin Thickness")
boxplot(Insulin~Outcome, main="Insulin vs. Diabetes", 
        xlab="Outcome", ylab="Insulin")
boxplot(BMI~Outcome, main="BMI vs. Diabetes", 
        xlab="Outcome", ylab="BMI")
boxplot(DiabetesPedigreeFunction~Outcome, main="Diabetes Pedigree Function vs. Diabetes", xlab="Outcome", ylab="DiabetesPedigreeFunction")
boxplot(Age~Outcome, main="Age vs. Diabetes", 
        xlab="Outcome", ylab="Age")

## -----------------------------------------------------------------------------
df$BloodPressure <- NULL
df$SkinThickness <- NULL
train <- df[1:540,]
test <- df[541:768,]
model <-glm(Outcome ~.,family=binomial(link='logit'),data=train)
summary(model)

## -----------------------------------------------------------------------------
anova(model, test="Chisq")

## -----------------------------------------------------------------------------
fitted.results <- predict(model,newdata=test,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Outcome)
print(paste('Accuracy',1-misClasificError))

## -----------------------------------------------------------------------------
df$pregnancy.history[df$Pregnancies == 0] = 0
df$pregnancy.history[df$Pregnancies > 0] = 1

table(df$pregnancy.history)

## -----------------------------------------------------------------------------
library(ggplot2) 
library(grid) 
library(gridExtra) 
library(corrplot) 
library(caret) 
library(e1071)

p1 <- ggplot(missing_data, aes(x=Pregnancies)) + ggtitle("Number of times pregnant") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 1, colour="Black", fill="orange") + ylab("Percentage")
p2 <- ggplot(df, aes(x=Glucose)) + ggtitle("Glucose") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 5, colour="black", fill="purple") + ylab("Percentage")
p3 <- ggplot(df, aes(x=BloodPressure)) + ggtitle("Blood Pressure") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 2, colour="black", fill="brown") + ylab("Percentage")
p4 <- ggplot(df, aes(x=SkinThickness)) + ggtitle("Skin Thickness") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 2, colour="black", fill="Red") + ylab("Percentage")
p5 <- ggplot(df, aes(x=Insulin)) + ggtitle("Insulin") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 20, colour="black", fill="blue") + ylab("Percentage")
p6 <- ggplot(df, aes(x=BMI)) + ggtitle("Body Mass Index") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 1, colour="black", fill="pink") + ylab("Percentage")
p7 <- ggplot(df, aes(x=DiabetesPedigreeFunction)) + ggtitle("Diabetes Pedigree Function") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), colour="black", fill="yellow") + ylab("Percentage")
p8 <- ggplot(df, aes(x=Age)) + ggtitle("Age") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth=1, colour="black", fill="green") + ylab("Percentage")
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=2)
grid.rect(width = 1, height = 1, gp = gpar(lwd = 1, col = "black", fill = NA))

## -----------------------------------------------------------------------------
library(ggplot2) 
library(grid) 
library(gridExtra) 
library(corrplot) 
library(caret) 
library(e1071)

df <- read.csv("C:/Users/ansar/Desktop/final/diabetes.csv")
p1 <- ggplot(df, aes(x=Pregnancies)) + ggtitle("Number of times pregnant") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 1, colour="black", fill="gray") + ylab("Percentage")
p2 <- ggplot(df, aes(x=Glucose)) + ggtitle("Glucose") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 5, colour="black", fill="gray") + ylab("Percentage")
p3 <- ggplot(df, aes(x=BloodPressure)) + ggtitle("Blood Pressure") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 2, colour="black", fill="gray") + ylab("Percentage")
p4 <- ggplot(df, aes(x=SkinThickness)) + ggtitle("Skin Thickness") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 2, colour="black", fill="gray") + ylab("Percentage")
p5 <- ggplot(df, aes(x=Insulin)) + ggtitle("Insulin") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 20, colour="black", fill="gray") + ylab("Percentage")
p6 <- ggplot(df, aes(x=BMI)) + ggtitle("Body Mass Index") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth = 1, colour="black", fill="gray") + ylab("Percentage")
p7 <- ggplot(df, aes(x=DiabetesPedigreeFunction)) + ggtitle("Diabetes Pedigree Function") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), colour="black", fill="gray") + ylab("Percentage")
p8 <- ggplot(df, aes(x=Age)) + ggtitle("Age") +
  geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth=1, colour="black", fill="gray") + ylab("Percentage")
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, ncol=2)
grid.rect(width = 1, height = 1, gp = gpar(lwd = 1, col = "black", fill = NA))

## -----------------------------------------------------------------------------
anova(model, test="Chisq")

## -----------------------------------------------------------------------------
library(mlbench)
library(rpart) 
library(rpart.plot)
library(caret)
library(Metrics)
data(diabetes)
dfn <- na.omit(df)
dplyr::glimpse(dfn)
set.seed(123)
index <- sample(2, nrow(df), prob = c(0.8, 0.2), replace = TRUE)
dfn_train <- df[index==1, ] 
dfn_test <- df[index == 2, ] 
dfn_model <- rpart(formula = dfn , 
                        method = "class")
rpart.plot(x = dfn_model, yesno = 2, type = 3, extra = 0,shadow.col = "gray", box.palette="Grays")

