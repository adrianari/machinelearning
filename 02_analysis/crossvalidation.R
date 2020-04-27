
------------------------------------------------------
  #RANDOM SPLITTING
------------------------------------------------------

#models
  
lm.1 <- lm(data=insurance, charges~.)

lm.2 <- lm(data=insurance, charges~children+smoker)

lm.3 <- lm(data=insurance, charges~(poly(bmi, degree=3))
           +(poly(age, degree=2))+children+smoker)

#in sample performance

summary(lm.1)$r.squared
summary(lm.2)$r.squared
summary(lm.3)$r.squared

#out of sample performance
  
set.seed(5)

r.squared.lm.1 <- c()
r.squared.lm.2 <- c()
r.squared.lm.3 <- c()

for(i in 1:100){
  
  # prepare data
  
  train.YES <- sample(x=c(TRUE,FALSE), 
                      size=nrow(insurance), 
                      replace = TRUE)
  
  table(train.YES)
  
  insurance.train <- insurance[train.YES, ]
  insurance.test <- insurance[!train.YES, ]
  
  # fit model with train data 
  
  lm.1.train <- lm(formula = formula(lm.1), 
                   data = insurance.train)
  
  lm.2.train <- lm(formula = formula(lm.2), 
                   data = insurance.train)
  
  lm.3.train <- lm(formula = formula(lm.3), 
                   data = insurance.train)
  
  # make prediction on test data
  lm.1.predict <- predict(lm.1.train, 
                          newdata = insurance.test)
  
  lm.2.predict <- predict(lm.2.train, 
                          newdata = insurance.test)
  
  lm.3.predict <- predict(lm.3.train, 
                          newdata = insurance.test)
  
  # compute r.squared and save in list
  
  r.squared.lm.1[i] <- cor(lm.1.predict, 
                           insurance.test$charges)^2
  
  r.squared.lm.2[i] <- cor(lm.2.predict, 
                           insurance.test$charges)^2
  
  r.squared.lm.3[i] <- cor(lm.3.predict, 
                           insurance.test$charges)^2
}

# mean of r.squared
  
mean(r.squared.lm.1)
mean(r.squared.lm.2)
mean(r.squared.lm.3)















