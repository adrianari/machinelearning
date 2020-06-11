# cv for classification tree (smoker)

# models used
glm.smoker.2$call #glm
bestmod$call #svm
nn3$call #neural network


set.seed(5)


MC.1 <- c()
MC.2 <- c()
MC.3 <- c()


for(i in 1:10){
  
  train.YES <- sample(x = c(TRUE, FALSE),
                      size = nrow(insurance),
                      replace = TRUE)
  
  insurance.train <- insurance[train.YES, ]
  
  insurance.test <- insurance[!train.YES, ]
  
  insurance.train.normalized <-dplyr::select(insurance.train, smoker, charges, age, bmi)
  insurance.test.normalized <-dplyr::select(insurance.test, smoker, charges, age, bmi)
  
  # Min-Max Normalization
  insurance.train.normalized$charges <- (insurance.train.normalized$charges - min(insurance.train.normalized$charges))/(max(insurance.train.normalized$charges) - min(insurance.train.normalized$charges)) 
  insurance.train.normalized$age <- (insurance.train.normalized$age - min(insurance.train.normalized$age))/(max(insurance.train.normalized$age) - min(insurance.train.normalized$age)) 
  insurance.train.normalized$bmi <- (insurance.train.normalized$bmi - min(insurance.train.normalized$bmi))/(max(insurance.train.normalized$bmi)-min(insurance.train.normalized$bmi)) 
  
  
  insurance.test.normalized$charges <- (insurance.test.normalized$charges - min(insurance.test.normalized$charges))/(max(insurance.test.normalized$charges) - min(insurance.test.normalized$charges)) 
  insurance.test.normalized$age <- (insurance.test.normalized$age - min(insurance.test.normalized$age))/(max(insurance.test.normalized$age) - min(insurance.test.normalized$age)) 
  insurance.test.normalized$bmi <- (insurance.test.normalized$bmi - min(insurance.test.normalized$bmi))/(max(insurance.test.normalized$bmi)-min(insurance.test.normalized$bmi))
  
  
  
  # fit model with train data 
  
  mod.1 <- glm(formula = smoker ~ age + bmi + charges, family = "binomial", 
               data = insurance.train)
  
  mod.2 <- svm(smoker~charges+bmi, data = insurance.train, cost =5)
  
  nn3 <- neuralnet(smoker~charges + age + bmi,
                   data = insurance.train.normalized,
                   hidden = c(3,2,1),
                   threshold = 0.0025,
                   lifesign = 'full', 
                   lifesign.step = 500,
                   linear.output = FALSE)
  
  
  # make prediction on test data
  
  #mod1
  mod.1.pred <- predict(mod.1, newdata = insurance.test, type="response")
  mod.1.pred.binomial <- ifelse(mod.1.pred > 0.5, yes = 1, no = 0)
  df.mod.1.pred <- data.frame(obs = insurance.test$smoker, fitted = mod.1.pred.binomial)
  mc.table.1 <- table(obs = df.mod.1.pred$obs,fit = df.mod.1.pred$fitted)
  MC.1[i] <- (mc.table.1[2]+mc.table.1[3])/(mc.table.1[2]+mc.table.1[3]+mc.table.1[1]+mc.table.1[4])
 
  #mod2 
  mod.2.pred <- predict(mod.2, newdata = insurance.test)
  df.mod.2.pred <- data.frame(obs = insurance.test$smoker, fitted = mod.2.pred)
  mc.table.2 <- table(obs = df.mod.2.pred$obs,fit = df.mod.2.pred$fitted)
  MC.2[i] <- (mc.table.2[2]+mc.table.2[3])/(mc.table.2[2]+mc.table.2[3]+mc.table.2[1]+mc.table.2[4])
  
  #mod3
  
  prediction_nn3 <- predict(nn3,insurance.test.normalized) 
  class_nn <- apply(prediction_nn3, 1, which.max)
  df.mod.3.pred <- data.frame(obs = insurance.test$smoker, fitted = class_nn)
  mc.table.3 <- table(obs = df.mod.3.pred$obs,fit = df.mod.3.pred$fitted)
  MC.3[i] <- (mc.table.3[2]+mc.table.3[3])/(mc.table.3[2]+mc.table.3[3]+mc.table.3[1]+mc.table.3[4])
  
}

#misclassification rates of the models
mean(MC.1)
mean(MC.2)
mean(MC.3)


