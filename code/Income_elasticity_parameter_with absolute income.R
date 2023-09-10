


# Calculating the difference in growth rate between the observed temperature and the counterfactual temperature
pdata$delta_growth = pdata$yhat2 - pdata$yhat1

# Calculating the counterfactual growth rate
pdata$growth_counterfactual = pdata$Growth_income + pdata$delta_growth


# Calculate absolute income values 
pdata$Lag_incomeA=exp(pdata$Lag_income)
pdata$income_ppp05=exp(pdata$Ln_income_ppp05)

# Calculating current-year counterfactual pre-tax national income
pdata$Counterfactual_income = pdata$Lag_incomeA + (pdata$Lag_incomeA * pdata$growth_counterfactual)

# Calculating damages
pdata$Damage = pdata$income_ppp05 - pdata$Counterfactual_income

# Calculating sum of country-level damages
pdata <- pdata %>%
  group_by(state, Year) %>%
  mutate(State_Damage = sum(Damage, na.rm=T))

# Calculating sum of country-level income
pdata <- pdata %>%
  group_by(state, Year) %>%
  mutate(State_iNCOME = sum(income_ppp05,na.rm=T))

# Estimating income groups share of country-level damages
pdata <- pdata %>%
  mutate(dep = log(Damage / State_Damage),
         dep1 = Damage / State_Damage)  # Share of damages without logs to identify damage outliers

# Estimating income groups share of country-level income
pdata <- pdata %>%
  mutate(cov = log(income_ppp05 / State_iNCOME))  # Two sets of country level income available

# Generate variable for estimating elasticity according to Bjoern's approach
pdata$dmgS=pdata$Damage/pdata$income_ppp05

#---------------------------------------------regression------------------------------------------------------
# ESTIMATING THE INCOME ELASTICITY PARAMETER


# Pooled OLS regression to estimate income elasticity parameter
ols_model <- lm(dep ~ cov, data = pdata)
summary(ols_model)

# Pooled OLS regression with year fixed effects
ols_fe_model <- lm(dep ~ cov + factor(Year), data = pdata)
summary(ols_fe_model)

# Pooled OLS regression with year and country fixed effects
ols_fe2_model <- lm(dep ~ cov + factor(Year) + factor(state), data = pdata)
summary(ols_fe2_model)

# Pooled OLS regression with year, country fixed effects, and country-specific time trends
ols_fe3_model <- lm(dep ~ cov + factor(Year) + factor(state) +as.numeric(Year) : state +I(as.numeric(Year)^2):factor(state), data = pdata)
summary(ols_fe3_model)

# Dropping damage outliers - damages "greater than or equal to +1" or "equal to or greater than -1"
data_outlier <- pdata %>%
  filter(dep1 <= 1 & dep1 >= -1)

# ESTIMATING THE INCOME ELASTICITY PARAMETER - RESULTS EXCLUDING OUTLIERS

# Pooled OLS regression to estimate income elasticity parameter
ols_model_outliers <- lm(dep ~ cov, data = data_outlier)
summary(ols_model_outliers)

# Pooled OLS regression with year fixed effects
ols_fe_model_outliers <- lm(dep ~ cov + factor(Year), data = data_outlier)
summary(ols_fe_model_outliers)

# Pooled OLS regression with year and country fixed effects
ols_fe2_model_outliers <- lm(dep ~ cov + factor(Year) + factor(state), data =data_outlier)
summary(ols_fe2_model_outliers)

# Pooled OLS regression with year, country fixed effects, and country-specific time trends
ols_fe3_model_outliers <- lm(dep ~ cov + factor(Year) + factor(state) ++as.numeric(Year) : state +I(as.numeric(Year)^2):factor(state), data =data_outlier)
summary(ols_fe3_model_outliers)


save.image("regression.RData")


#estimate Bjoern's model

# Pooled OLS regression with country and year fixed effects
ols_Bmodel_outliers <-felm(
  log(dmgS) ~ poly(log(income_ppp05), 2, raw = TRUE) | state,
  data = data_outlier
)
print(paste("mean of country fixed effects:", mean(getfe(ols_Bmodel_outliers)$effect)))

elasticity_B=1+coef(ols_Bmodel_outliers)[1]+2*coef(ols_Bmodel_outliers)[2]*log(mean(data_outlier$Ln_income_ppp05))

print(paste("Mean Elasticity:",elasticity_B))
