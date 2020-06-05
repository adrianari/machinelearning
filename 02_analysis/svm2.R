library(tidyverse) # for nice plots, include automatically ggplot2 and other utilities
library(e1071)     # for the SVM funtion()

set.seed(1)

# visual exploration
ggplot(data = insurance) +
  geom_point(aes(x = charges, y = bmi, color=region))

ggplot(data = insurance) +
  geom_point(aes(x = charges, y = age, color=region))

ggplot(data = insurance) +
  geom_point(aes(x = bmi, y = age, color=region))


#tuning polynomial svm for 4 classes -> not enough computing power 
cost_range <-
  c(
    10,
    100)

degree_range <- 2:4

tune.out.regions <- tune(
  svm,
  region ~ charges+bmi,
  data = insurance.train,
  kernel = "polynomial",
  ranges = list(cost = cost_range, degree = degree_range),
  scale = FALSE
)

svmfit_regions <- tune.out.regions$best.model

plot(svmfit_regions, insurance.train)


#polynomial svm for 4 classes without tuning -> not working 
svmfit <-
  svm(
    region ~ charges+bmi,
    data = insurance.train,
    kernel = "polynomial",
    cost = 0.01,
    scale = FALSE
  )


plot(svmfit, insurance.train, bmi~charges)

svmfit.2 <-
  svm(
    region ~ charges+bmi,
    data = insurance.train,
    kernel = "polynomial",
    cost = 1,
    scale = FALSE
  )


plot(svmfit.2, insurance.train, bmi~charges)

table(predict = predict(svmfit.2, insurance.train),
      truth = insurance.train$region)



# try with age group

insurance$age_group <- as.factor(substr(insurance$age,1,1))
insurance.train$age_group <- as.factor(substr(insurance.train$age,1,1))
insurance.test$age_group <-as.factor( substr(insurance.test$age,1,1))

ggplot(data = insurance) +
  geom_point(aes(x = charges, y = bmi, color=age_group))

svmfit.age <-
  svm(
    age_group ~ charges+bmi,
    data = insurance.train,
    kernel = "polynomial",
    cost = 10,
    scale = FALSE
  )


plot(svmfit.age, insurance.train, bmi~charges)

table(predict = predict(svmfit.age, insurance.train),
      truth = insurance.train$age_group)


#make groups as example

slope <- 0.003
intercept1 <- -5
intercept2 <- -60

ggplot(data = insurance) +
  geom_point(aes(x = charges, y = age))+
  geom_abline(slope = slope1, intercept = intercept1, color="purple")+
  geom_abline(slope = slope2, intercept = intercept2, color="orange")

insurance_3_classes <- data.frame(x = insurance,
                                t = as.factor(
                                as.numeric(insurance$charges * slope + intercept1 <= insurance$age)+
                                as.numeric(insurance$charges * slope + intercept2 <= insurance$age)))



#plot with classes
ggplot(data = insurance_3_classes) +
  geom_point(aes(x = x.charges, y = x.age, color=t))+
  geom_abline(slope = slope1, intercept = intercept1, color="purple")+
  geom_abline(slope = slope2, intercept = intercept2, color="orange")

#make train/test data
train.YES <- sample(x=c(TRUE,FALSE), 
                    size=nrow(insurance), 
                    replace = TRUE)


insurance_3_classes.train <- insurance_3_classes[train.YES, ]
insurance_3_classes.test <- insurance_3_classes[!train.YES, ]


#tuning polynomial svm for 3 classes
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

tune.out.classes <- tune(
  svm,
  t ~ x.charges+x.age,
  data = insurance_3_classes.train,
  kernel = "polynomial",
  ranges = list(cost = cost_range, degree = degree_range),
  scale = FALSE
)

tune.out.classes$best.model

svmfit_classes <- tune.out.classes$best.model

#train data

plot(svmfit_classes, insurance_3_classes.train, x.age~x.charges)

table(predict = predict(svmfit_classes, insurance_3_classes.train),
      truth = insurance_3_classes.train$t)


#test data

plot(svmfit_classes, insurance_3_classes.test, x.age~x.charges)

table(predict = predict(svmfit_classes, insurance_3_classes.test),
      truth = insurance_3_classes.test$t)



     