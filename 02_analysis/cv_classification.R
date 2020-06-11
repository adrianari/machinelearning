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
  
  
  # fit model with train data 
  
  mod.1 <- glm(formula = smoker ~ age + bmi + charges, family = "binomial", 
               data = insurance.train)
  
  mod.2 <- svm(smoker~charges+bmi, data = insurance.train, cost =5)
  
  mod.3 <- neuralnet(formula = smoker ~ charges + age + bmi, data = insurance.train, 
                     hidden = c(3, 2, 1), threshold = 0.0025, lifesign = "full", 
                     lifesign.step = 500, linear.output = FALSE)
  
  
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
  mod.3.pred <- predict(mod.3, newdata = insurance.test)
  mod.3.pred.binomial <- apply(mod.3.pred, 1, which.max)
  df.mod.3.pred <- data.frame(obs = insurance.test$smoker, fitted = mod.3.pred.binomial)
  mc.table.3 <- table(obs = df.mod.3.pred$obs,fit = df.mod.3.pred$fitted)
  MC.3[i] <- (mc.table.3[2]+mc.table.3[3])/(mc.table.3[2]+mc.table.3[3]+mc.table.3[1]+mc.table.3[4])
  
}

mean(MC.1)
mean(MC.2)
mean(MC.3)

head(mod.3.pred)

