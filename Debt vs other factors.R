# STAT 371 Group Project
install.packages("car")
library(car)

data1 <- read.csv("/Users/hodacdan/Downloads/STAT371Datav2.csv")


summary(data1)

colnames(data1)

colnames(data1)[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)] <- c("countries", "govt_debt", "population", "gdp", "inflation_rate", "billionaires_amt", "democracy_index", "size", "gdp_per_capita", "hdi")

data1

model_1 <- lm(govt_debt ~ population + gdp  + inflation_rate + 
               billionaires_amt + democracy_index + size + gdp_per_capita + 
               hdi, data = data1)

summary(model_1)



## i) QQPlot

resid_1 <- resid(model_1)
qqnorm(resid_1)
qqline(resid_1, col=3)


## ii) Studentized residuals

sdntres <- rstudent(model_1)

plot(sdntres ~ data1$index_number , xlab= "Index", ylab ="Studentized Residuals",
     main = "Studentized Residuals vs Index", col=3,bg=3,pch=21)

abline(h = 0, col = "red", lty = 1)

## iii) Residuals vs Fitted Values

yhat<-fitted(model_1)

plot(resid_1 ~ yhat, xlab ="Fitted Values", ylab="Residuals", main = 
       "Residual vs Fitted Values", col=3, bg=3,pch=21)


## iv) Leverage vs Index

leverage_1 <- hatvalues(model_1)

plot(leverage_1 ~ data1$index_number, xlab = 'Index', ylab = 'Leverage', 
     main = "Leverage vs Index", col=3, bg=3,pch=21)



# Model 2 (Log of govt_debt)

model_2 <- lm(log(govt_debt) ~ population + gdp  + inflation_rate + 
                billionaires_amt + democracy_index + size + gdp_per_capita + 
                hdi, data = data1) 

summary(model_2)


### i) QQ Plot

resid_2 <- resid(model_2)

qqnorm(resid_2)

qqline(resid_2, col=2)


### ii) Studentized Residuals

sdntres2 <- rstudent(model_2)

plot(sdntres2 ~ data1$index_number, xlab= "Index", ylab ="Studentized Residuals",
     main = "Studentized Residuals vs Index", col=2,bg=2,pch=21)

abline(h = 0, col = "red", lty = 1)


### iii) Residual vs Fitted Values

yhat2 <- fitted(model_2)

plot(resid_2 ~ yhat2, xlab ="Fitted Values", ylab="Residuals", 
     main = "Residual vs Fitted values", col=2,bg=2,pch=21)


### iv) Leverage vs Index

leverage_2 <- hatvalues(model_2)
plot(leverage_2 ~ data1$index_number, xlab = 'Index', ylab = 'Leverage', 
     main = "Leverage vs Index", col=2,bg=2,pch=21)

fit1 <- lm(leverage_2 ~ data1$index_number)

abline(fit1, col = "blue", lty = 1)


# Find the index number of the observation with the largest leverage
max_leverage_index <- which.max(leverage_2)
max_leverage_index

index_studentized_res <- sdntres2[85]
index_studentized_res

max_studentized_res <- which.max(sdntres2)
max_studentized_res

sort(leverage_2)

# Model 3 (Removing Points of high levergage from log(govt_debt)

data2 <- read.csv("~/Desktop/Desktop - Aryaman’s MacBook Pro/University/Classes/Term 3B/STAT 371/Group Project/STAT 371 Group Project Data v3.csv", header = TRUE)

model_3 <- lm(log(govt_debt) ~ population + gdp + inflation_rate + 
                billionaires_amt + democracy_index + size + gdp_per_capita + 
                hdi, data = data2) 

summary(model_3)


### i) QQ Plot

resid_3 <- resid(model_3)

qqnorm(resid_3)

qqline(resid_3, col=2)


### ii) Studentized Residuals

sdntres3 <- rstudent(model_3)

plot(sdntres3 ~ data2$index_number, xlab= "Index", ylab ="Studentized Residuals",
     main = "Studentized Residuals vs Index", col=2,bg=2,pch=21)

abline(h = 0, col = "blue", lty = 1)


### iii) Residual vs Fitted Values

yhat3 <- fitted(model_3)

plot(resid_3 ~ yhat3, xlab ="Fitted Values", ylab="Residuals", 
     main = "Residual vs Fitted values", col=2,bg=2,pch=21)


### iv) Leverage vs Index

leverage_3 <- hatvalues(model_3)
plot(leverage_3 ~ data2$index_number, xlab = 'Index', ylab = 'Leverage', 
     main = "Leverage vs Index", col=2,bg=2,pch=21)

fit2 <- lm(leverage_3 ~ data2$index_number)

abline(fit2, col = "blue", lty = 1)

project<- read.csv("/Users/hodacdan/Downloads/statproject.csv")
project1<- read.csv("/Users/hodacdan/Downloads/statv2.csv")
library(MASS)




#Model selection 
#backward selection
independentvars <- c("population" , "gdp", "inflation_rate", "billionaires_amt",
                     "democracy_index", "size", "gdp_per_capita","hdi")
colnames(project)


elections1 <-project[1:87,c('govt_debt', independentvars)]
model.elections.basic<-lm(log(govt_debt)~1,data=project)
model.elections.full<-lm(log(govt_debt)~.,data=project)
select.backward.AIC<-stepAIC(model.elections.full,scope=formula(model.elections.full),direction="backward")
select.backward.AIC$anova

#forward

independentvars <- c("population" , "gdp", "inflation_rate", "billionaires_amt",
                     "democracy_index", "size", "gdp_per_capita","hdi")

#election<-project[1:87,c("Debt",independentvars)]
model.elections.basic<-lm(log(govt_debt)~1,data=project)
model.elections.full<-lm(log(govt_debt)~.,data=project)
select.forward.AIC<-stepAIC(model.elections.basic,scope=formula(model.elections.full),direction="forward")
select.forward.AIC$anova


#summary of the model to interpret the coefficent 

Fmodel <- lm(log(govt_debt) ~ democracy_index + gdp + billionaires_amt + gdp_per_capita,data=project)
summary(Fmodel)



# VIF

Fmodel <- lm(log(govt_debt) ~ democracy_index + gdp + billionaires_amt + gdp_per_capita,data=project)
vif(Fmodel)
Fmodel1 <-lm(log(govt_debt) ~ democracy_index + gdp + billionaires_amt + gdp_per_capita,data=project1)
summary(Fmodel)
summary(Fmodel1)


Model2<- lm(log(govt_debt) ~ democracy_index + gdp + gdp_per_capita ,data=project)
vif(Model2)
summary(Debt1)
summary(Fmodel)

print(cor(project))

# solution
model_1 <- lm(log(govt_debt) ~ population + gdp  + inflation_rate + 
                billionaires_amt + democracy_index + size + gdp_per_capita + 
                hdi, data = project)

model_2 <- lm(log(govt_debt) ~ population + gdp  + inflation_rate + 
                billionaires_amt + democracy_index + size + gdp_per_capita + 
                hdi, data = project1)
summary(model_1)
summary(model_2)







