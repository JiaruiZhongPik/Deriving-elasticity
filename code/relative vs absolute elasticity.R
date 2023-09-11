#-----------------------------experiment with randomly generated data---------------
set.seed(42)  # For reproducibility
# Generate random data from a log-normal distribution
# this is assumed to be pre damage income 
y_pre <- rlnorm(2000, meanlog = 7, sdlog = 0.8)  # Adjust meanlog and sdlog as needed
summary(y_pre)

# Create a density plot
ggplot(data.frame(x = y_pre), aes(x)) +
  geom_density(fill = "blue", color = "black") +
  labs(title = "Density Plot of Log-Normal Distribution", x = "Value", y = "Density")

#Assume the relative income elasticity equal to 0.7
ela.G=0.7

#Assume average damage ratio (of income) is 0.1
D_mean=0.1

#Total damage
Damage=sum(y_pre)*D_mean

#Compute the constant
C=1/sum( (y_pre / sum(y_pre))^ela.G )

#compute households' damage share in total damage
Dhh = C * (y_pre / sum(y_pre)) ^ ela.G

#compute households' absolute damage
Damage.hh = Damage * Dhh

ggplot(data.frame(x = Damage.hh), aes(x)) +
  geom_density(fill = "blue", color = "black") +
  labs(title = "Density Plot of Log-Normal Distribution", x = "Value", y = "Density")

data=data.frame(y_pre,Dhh,Damage.hh)

#compute Gilli's elasticity, the coefficient should equal to 0.7
Model.G=felm(log(Dhh)  ~ log(y_pre/sum(y_pre)) ,data=data)
Model.G

#compute Remind elasticity
Model.B= felm(log(Damage.hh / y_pre) ~ poly(log(y_pre), 1, raw = TRUE),data = data)
Model.B



#--------------------Experiment with pdata--------------------------------------------


data <- pdata %>%
  select(ends_with(persistence),all_of(c("Year", "state","panelid", "temp","cpercentile","IG_hetero"))) %>%
  dplyr::filter(Year > 1987) %>%
  rename_with(~ gsub(paste("_",persistence,"$",sep=''), "", .), ends_with(persistence)) %>%
  mutate(dep1 = Damage/Counterfactual_income,
         cov1 = Counterfactual_income)%>%
         group_by(state, Year) %>%
           mutate(State_Damage = sum(Damage, na.rm=F),
                  State_preDMG_Income = sum(Counterfactual_income,na.rm=F),
                  dep2 = Damage / State_Damage,
                  cov2 = Counterfactual_income/State_preDMG_Income)

#remove observations with missing value
data <- na.omit(data)

#only keep observations with positive damage
data <- data %>%
  dplyr::filter(Damage>=0,State_Damage>=0)

summary(data)
#compute Gilli's elasticity, the coefficient should equal to 0.7

felm( log(dep2) ~ log(cov2) | Year + state ,data=data)

#compute Remind elasticity
felm(log(dep1) ~ poly(log(cov1), 1, raw = TRUE)| Year + state,data = data)
