#Libraries
library(ggplot2)
library(gridExtra)


# Generalised Linear models 

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




  











