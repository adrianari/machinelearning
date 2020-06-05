library(tidyverse) # for nice plots, include automatically ggplot2 and other utilities
library(e1071)     # for the SVM funtion()

set.seed(1)

# visual exploration
ggplot(data = insurance) +
  geom_point(aes(x = charges, y = bmi, color=smoker))

# guess of hyperplane
slope <- 0.001
intercept <- 10

ggplot(data = insurance) +
  geom_point(aes(x = charges, y = bmi, color=smoker))+
  geom_abline(slope = slope, intercept = intercept)

#fit a SVM with a linear kernel (Support Vector Classifier) 
svmfit <-
  svm(
    smoker ~ charges+bmi,
    data = insurance.train,
    kernel = "linear",
    cost = 10,
    scale = FALSE
  )


plot(svmfit, insurance.train, bmi~charges)


#get optimal cost 
cost_range <-
  c(1e-10,
    1e-7,
    1e-5,
    0.001,
    0.0025,
    0.005,
    0.0075,
    0.01,
    0.1,
    1,
    5,
    10,
    100)
tune.out <- tune(
  svm,
  smoker ~ charges+bmi,
  data = insurance.train,
  kernel = "linear",
  ranges = list(cost = cost_range)
)
summary(tune.out)

#best model 
tune.out$best.parameters
bestmod <- tune.out$best.model
summary(bestmod)
plot(bestmod, insurance.train, bmi~charges)


#get optimal cost and degree for polynomial

cost_range <-
  c(1e-10,
    1e-7,
    1e-5,
    0.001,
    0.0025,
    0.005,
    0.0075,
    0.01,
    0.1,
    1,
    5,
    10,
    100)

degree_range <- 2:4

tune.out.poly <- tune(
  svm,
  smoker ~ charges+bmi,
  data = insurance.train,
  kernel = "polynomial",
  ranges = list(cost = cost_range, degree = degree_range))
summary(tune.out.poly)

#best model poly
tune.out.poly$best.parameters
bestmod.poly <- tune.out.poly$best.model
summary(bestmod.poly)
plot(bestmod.poly, insurance.train, bmi~charges)

# confusion matrix linear
confusion.lin.train <- table(predict = predict(bestmod, insurance.train),
      truth = insurance.train$smoker)

confusion.lin.test <- table(predict = predict(bestmod, insurance.test),
      truth = insurance.test$smoker)

# confusion matrix linear
confusion.poly.train <- table(predict = predict(bestmod.poly, insurance.train),
      truth = insurance.train$smoker)

confusion.poly.test <- table(predict = predict(bestmod.poly, insurance.test),
      truth = insurance.test$smoker)

#classification error rate 

#linear
(confusion.lin.test[1,2]+confusion.lin.test[2,1])/sum(confusion.lin.test[1:2,1:2])

#poly
(confusion.poly.train[1,2]+confusion.poly.train[2,1])/sum(confusion.poly.train[1:2,1:2])

#poly
(confusion.poly.test[1,2]+confusion.poly.test[2,1])/sum(confusion.poly.test[1:2,1:2])


    