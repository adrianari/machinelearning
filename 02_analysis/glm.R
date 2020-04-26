#Libraries
library(ggplot2)
library(gridExtra)


# Generalised Linear models 

----------------------------------------------------------------
  # Count data
----------------------------------------------------------------

# Model predicting the number of children (count data)

head(insurance)

summary(insurance$children)

# Linear model (not suited for count data)

lm.children <- lm(children ~ , 
                  data=insurance)
summary(lm.children)
round(coef(lm.children), digits = 5)

# Poisson model with all predictors

glm.children.1 <- glm(children ~ ., 
                   data=insurance, 
                   family = "poisson")

summary(glm.children.1)
exp(coef(glm.children.1)) #-> coefficients were log transformed


#Poisson model with the significant predictors smoker and charges
glm.children.2 <- glm(children ~ charges+smoker, 
                    data=insurance, 
                    family = "poisson")

summary(glm.children.2)
exp(coef(glm.children.2)) #-> coefficients were log transformed



#Simulate data for glm
set.seed(5)
sim.data.glm.children <- simulate(glm.children.1)
summary(sim.data.glm.children)

#Comparison to actual data 
df <- data.frame(insurance)
summary(df$children)

#Plots for comparison
plot.original <- ggplot(df, 
                        aes(x=children))+ 
  geom_bar(fill="darkgreen")+
  labs(title = "Original data", x="number of children")

plot.sim <- ggplot(sim.data.glm.children, 
                   aes(x=sim_1))+
                     geom_bar(fill="darkblue")+
  labs(title = "Simulated data", x="number of children")

grid.arrange(plot.original, plot.sim, nrow=1)

--------------------------------------------------
# Binary data
--------------------------------------------------
  
# Binomial Model with all predictors
  
glm.smoker.1 <- glm(smoker ~ ., 
                        data=insurance, 
                        family = "binomial")

summary(glm.smoker.1)

### only siginificant:

glm.smoker.2 <- glm(smoker ~ age+bmi+charges, 
                    data=insurance, 
                    family = "binomial")

summary(glm.smoker.2)

exp(coef(glm.smoker.2)) 

#Age and BMI has a negative effect on smoker. 
#This means the higher a persons BMI and age, the lower the probability that the person 
#is a smoker. 
# Charges has a positive effect. This means the higher a persons charges, the higher is
# the possibility that the person smokes. 


#can be proven graphically, especially for charges
library(ggplot2)
library(gridExtra)

plot1 <- ggplot(data = insurance,
       mapping = aes(y = smoker,
                     x = age)) +
  geom_point(alpha = 0.05)

plot2 <- ggplot(data = insurance,
       mapping = aes(y = smoker,
                     x = bmi)) +
  geom_point(alpha = 0.05)

plot3 <- ggplot(data = insurance,
       mapping = aes(y = smoker,
                     x = charges)) +
  geom_point(alpha = 0.05)

grid.arrange(plot1, plot2, plot3, ncol=3)

# Estimating the performance 



fitted.smoker.disc <- ifelse(fitted(glm.smoker.2) < 0.5,
                           yes = 0, no = 1)
head(fitted.smoker.disc)

d.obs.fit.smoker <- data.frame(obs = insurance$smoker,
                             fitted = fitted.smoker.disc)
head(d.obs.fit.smoker)
tail(d.obs.fit.smoker)

# we can observe that the model reached the follwing fit:

table(obs = d.obs.fit.smoker$obs,
      fit = d.obs.fit.smoker$fitted)




--------------------------------------------------
# Binomial data
--------------------------------------------------

# our dataset does not contain binomial data. 











