# Installing packages -----------------------------------------------------

packages <- c("gitcreds",
              "tidyverse",
              "ggplot2",
              "readxl",
              "zoo",
              "car",
              "mgcv")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

library(gitcreds)
library(tidyverse)
library(ggplot2)
library(readxl)
library(zoo)
library(car)
library(mgcv)


# ??Git_command?? ---------------------------------------------------------

#gitcreds_set() #Only needed in the first setup when git sync is not working with password only

# Data preparation --------------------------------------------------------

# Load data
df <- read_excel("data/autos.xlsx")

# Create columns yearmon and age

df["yearmon"] = as.Date(as.yearmon(paste(df$yearOfRegistration,
                                         df$monthOfRegistration,
                                         sep = "-")))
df["age"] = as.numeric(as.Date(df$dateCreated) - df$yearmon)

# Clean data step 1
# The following filters were applied based on the investigations in the section "Investigate
# variables" The aim was to reduce the data set to less than 10^5 observations, to simplify
# it in terms of factor levels, to come up with reasonable ranges where enough data points
# were available and to get an approximative normal distribution for the variable values 
# whenever possible.

df <- df %>% 
  filter(seller == "privat" &
           offerType == "Angebot" &
           price >= 500 &
           price <= 40000 &
           vehicleType != "andere" &
           !is.na(vehicleType) &
           !is.na(gearbox) &
           powerPS >= 50 &
           powerPS <= 325 &
           kilometer >= 10000 &
           kilometer <= 150000 &
           fuelType %in% c("benzin", "diesel") &
           !is.na(fuelType) &
           !is.na(notRepairedDamage) &
           !is.na(yearOfRegistration) & 
           !is.na(age) &
           age >= 180 &
           age <= 10000 &
           model != "andere" &
           lastSeen <= as.POSIXct("2016-04-04 23:59:59")) %>%
  select(-c("seller",
            "offerType",
            "abtest",
            "name",
            "nrOfPictures"))

# Keep models and brands with more than x observations

brand_table <- df %>%
  count(brand) %>%
  filter(n >= 1000)

brand_vector <- as.character(brand_table$brand)

model_table <- df %>%
  count(model) %>%
  filter(n >= 0)

model_vector <- as.character(model_table$model)

# Clean data step 2

df <- df %>% 
  filter(brand %in% brand_vector &
           model %in% model_vector)

# Change data types

str(df)
df <- df %>%
  mutate_if(sapply(df, is.character), as.factor) %>%
  mutate(kilometer = as.factor(kilometer))

# Investigate variables ---------------------------------------------------

# As mentioned earlier, the following functions were used to get a better understanding
# of each variable and to filter them accordingly. The histogram provides information
# on the distribution of the data points within a variable. The quantile and ecdf function
# were used to try out different values and to spot anomalies and mistakes in the data set.
# They were also used to come up with the final ranges. 
# TO DO: Is there a systematic process how to filter and decide ranges?

# price

summary(df$price)
hist(df$price, xlab = "price", main = "Histogram response variable price")
quantile(df$price, 0.95)
ecdf(df$price)(17000)

# Our response variable is extremely right-skewed as the histogram shows. Prices
# range from 500 to 40'000. 95% of all observations show a price which is 17'000
# or less. As our response variable is considered as amount, we will log-transform
# it in the modelling part. This helps also to reduce right-skewedness.

# age

summary(df$age)
hist(df$age, xlab = "age in days", main = "Histogram age of cars")
quantile(df$age, 0.50)
ecdf(df$age)(4374)

# Half of the cars offered are older than 4374 / 365 ~ 12 years. The data seems to approximately
# follow a normal distribution.

# powerPS

summary(df$powerPS)
hist(df$powerPS, xlab = "PS", main = "Histogram PS of cars")
quantile(df$powerPS, 0.75)
ecdf(df$powerPS)(120) - ecdf(df$powerPS)(100)

# The distribution of the PS values is also right-skewed. About 20 % of all observations show values 
# between 100 and 120 PS. Prices fitted from PS values should better fit in this range.

# kilometer

summary(df$kilometer)
barplot(table(df$kilometer), xlab = "number of kilometers", main = "Mileage of cars")

# The mileage variable form the data set is most likely a categorical variable where the 
# advertiser could set a predefined value. The distribution is extremely left-skewed.
# The share of cars with 125'000 or more kilometers is around 75%.

# vehicleType

summary(df$vehicleType)
barplot(table(df$vehicleType))

# Limousine, Kombi and Kleinwagen are the three dominant vehicle types.

# gearbox

summary(df$gearbox)
barplot(table(df$gearbox))

# The majority of cars advertised come with manual gearbox.

# brand & model

summary(df$brand)
nlevels(df$brand)
summary(df$model)
nlevels(df$model)

barplot(table(df$brand))

ggplot(data=df, aes(x=brand, fill = model)) +
  geom_bar(stat = "count") +
  #scale_fill_brewer(palette = "Set3") + 
  ggtitle("Brands and models") +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12))
  
# Volkswagen is the dominant car brand with more than 20'000 cars advertised.
# The VW Golf is the dominant model advertised, followed by the BMW 3er.

# fuelType

summary(df$fuelType)
barplot(table(df$fuelType), main = "Fuel type of cars")
range(df$yearOfRegistration)

# We focus on "Benzin" and "Diesel". There were no electric vehicles advertised in the data set
# which is not much of a surprise as the car registration values for the advertised cars
# range from 1988 to 2015.

#notRepairedDamage

summary(df$notRepairedDamage)
barplot(table(df$notRepairedDamage), main = "Repaired damage")

# Around 10% of the advertised cars had a damage and were repaired before advertised.


# Graphical analysis ------------------------------------------------------

# Analysis of continuous variables:

# Create template

template.scatterplot.df <- ggplot(data = df,
                                  mapping = aes(y = log(price))) +
  geom_point(alpha = 0.1) +
  geom_smooth()

# age

template.scatterplot.df + aes(x = age)

# The relationship between age and price looks linear until an age of around 7000 days. Also
# the variance is pretty stable until then. Afterwards the variability increases significantly
# and the relationship changes from a negative effect to positive effect. Whether this can be
# modeled by a quadratic term will be checked later. There are some observations with a very low
# price in the age range 5000 to 10000. We may need to check for that in the modelling part.

# powerPS

template.scatterplot.df + aes(x = powerPS)

# There is a clear positive effect of PS on price. whether the relationship is best modeled by
# a linear or higher order term needs to be evaluated. There is some bumpiness from around
# 220 PS on. Is this random noise due to decreasing observations or does that to be modeled?

# kilometer as continuous variable

template.scatterplot.df + aes(x = as.numeric(kilometer))

# The mileage of the cars does have a negative effect on the price which is logical from a business
# point of view. It looks mostly like a linear relationship except for the low and high segments of
# the mileage. But be careful, the last two mileage levels show a distance of 25'000 kilometers in
# between. Not visible here are advertised cars with a mileage of 5000km. In the range of 5000
# to 10000 kilometers the observations actually showed a positive on the price variable which is
# difficult to understand.

# Analysis of categorical variables:

# Create template

template.boxplot.df <- ggplot(data = df,
                              mapping = aes(y = log(price))) +
  geom_boxplot()

# kilometer

template.boxplot.df + aes(x = kilometer)

# There is a negative effect of kilometers on price. The effect is larger for the ladder levels.
# Also the variability increases for the ladder levels. The effects seem to be more or less
# normally distributed.

template.boxplot.df + aes(x = vehicleType)

# There are some differences in prices for the different vehicle types. SUV's are the most
# expensive cars whereas kleinwagen seem to be lowest in price. This is not a surprise.
# The variability for coupe is larger than for cabrio, kleinwagen, kombi, limousine and bus.
# The variability for suv semms to be the smallest with some outliers in the low price segment.
# If all the different vehicle types have a significant influence on price will be checked in
# the modelling part.

template.boxplot.df + aes(x = gearbox)

# It seems to be that automatic gearbox are advertised with higher prices. From a business
# perspective, this makes perfect sense.

template.boxplot.df + aes(x = fuelType)

# Cars with fuel type Diesel are advertised with higher prices. This is also in line with
# our business understanding.

template.boxplot.df + aes(x = notRepairedDamage)

# Cars with a previous damage are advertised with lower prices. Also common sense. Whether
# the effect is significant will be checked in the modelling part.

template.boxplot.df + aes(x = brand)

# Cars form the brand mini show the highest median price whereas Renault cars show the lowest.
# The brands do show different variability in price. 

template.boxplot.df + aes(x = model)

# There are considerable differences between the different models. We will check the boxplots
# for the brand again but this time we distinguish between the models.

boxplot.audi <- df %>% 
  filter(brand == "audi") %>%
  ggplot(mapping = aes(y = log(price), x = model)) +
  geom_boxplot()

boxplot.audi

# No clear differences in prices visible for the different models.

boxplot.bmw <- df %>% 
  filter(brand == "bmw") %>%
  ggplot(mapping = aes(y = log(price), x = model)) +
  geom_boxplot()

boxplot.bmw

# Different median but the boxes are very much overlapping.

boxplot.merc <- df %>% 
  filter(brand == "mercedes_benz") %>%
  ggplot(mapping = aes(y = log(price), x = model)) +
  geom_boxplot()

boxplot.merc

# e_klasse seems to be slightly more expensive but is this significant?

boxplot.opel <- df %>% 
  filter(brand == "opel") %>%
  ggplot(mapping = aes(y = log(price), x = model)) +
  geom_boxplot()

boxplot.opel

# Corsa cheaper than Astra but significantly?

boxplot.vw <- df %>% 
  filter(brand == "volkswagen") %>%
  ggplot(mapping = aes(y = log(price),x = model)) +
  geom_boxplot()

boxplot.vw

# Polo cheaper than Passat and Golf.

# Do we have other variables to control for where an effect is different among the levels of
# another factor? Or in other words, is there any interaction between the predictor variables?
# For example is the kilometer depreciation in price different among the brands?
# Is the age depreciation in price different among the brands?
# Or is the depreciation in price from kilometers and age different among the vehicle types?
# We will focus for now on the graphical analysis and test these hypotheses later in the
# modelling part.

ggplot(data = df,
       mapping = aes(y = log(price), x = age)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  facet_wrap(. ~ brand)

# We can see that the price depreciation from age behaves similarly among the different brands.
# From a certain age on the we can observe a positive effect on price. For some brands we do 
# do not have many data points for older vehicles.

ggplot(data = df,
       mapping = aes(y = log(price), x = as.numeric(kilometer))) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  facet_wrap(. ~ brand)

# We changed the kilometer variable to numeric in order to plot a smoother to compare the
# different brands. Again, the behavior looks similar. BMW cars tend to have higher depreciation
# in the beginning and the end. For kilometers between 20 and 40k BMW's tend to raise in value.
# This is really interesting. The same behavior can be observed for the brands Mercedes-Benz
# and Opel, which have periods where the effect of depreciation is not linearly negative until
# about 50'000 km driven. There are brands were we do not have many data points for a short mileage.

ggplot(data = df,
       mapping = aes(y = log(price), x = age)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  facet_wrap(. ~ vehicleType)

# Again the curves look similar in terms of age depreciation among the different vehicle types. 
# There is a change of the effect on price from negative to positive from a certain age on. 
# For the vehicle type suv's there are not many observations for an age older than 5000 days.
# This could indicate that SUV's became popular only recently. For cabrios the turning point is
# around 7000 days. From there on, cabrios gain in value faster than other vehicle types. This
# may be due to the attraction of vintage cabrios.

ggplot(data = df,
       mapping = aes(y = log(price), x = as.numeric(kilometer))) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  facet_wrap(. ~ vehicleType)

# The graphics look similar except for coupes and kombis. Coupes are gaining in value until about
# 50'000 kilometers driven. This seems counter-intuitive and may be due to the fact that there are
# not many data points until then.


# Fitting a linear model ----------------------------------------------------------

# We start by including all predictors without any interactions:

lm.autos_1 <- lm(log(price) ~ age + powerPS + kilometer + gearbox + brand + model + fuelType +
                   notRepairedDamage + vehicleType,
                 data = df)
summary(lm.autos_1)

# The model seems to be quite good with 86% of the variability explained. However, there are 4
# coefficients with an NA value. After googling, that tells us some of our variables are perfectly
# dependent on one or more other variables. We can check that with the alias function.

alias(lm.autos_1)

# Let's take the example of the model Citroen c5. Indeed, it makes sense that whenever the brand is
# Citroen and the model is not berlingo, c1, c2, c3 or c4 it must be a c5. So including the brand and
# the model as two separate variables makes no sense as the brand is already defined through the model.
# We can simply solve that issue by only including the model type.

lm.autos_2 <- lm(log(price) ~ age + powerPS + kilometer + gearbox + model + fuelType +
                   notRepairedDamage + vehicleType,
                 data = df)
summary(lm.autos_2)


# If we compare the summary statistics of lm.autos_1 with lm.autos_2 we state that the model got
# simpler as there are less variables but the variability explained remains unchanged. We now check
# our model for collinearity.

vif(lm.autos_2)

# We can see that the GVIF value for the variable "model" is way too large. This indicates that the model
# variable is correlated to a linear combination of other variables. In our case, that could mean that the
# "model" is highly correlated with other variables just as powerPS, gearbox, vehicleType and so on.
# We therefore exclude the "model" variable and replace it with "brand".

lm.autos_3 <- lm(log(price) ~ age + powerPS + kilometer + gearbox + brand + fuelType +
                   notRepairedDamage + vehicleType,
                 data = df)
summary(lm.autos_3)

# Our model does now have fewer parameters to fit and therefore became simpler. We lost some of the explained
# variability which is logical as we have less parameters. We check again for collinearity.

vif(lm.autos_3)

# Now there seems to no problem anymore. From here on We test if all predictors do have a significant effect
# on the response variable with the drop1 command:

drop1(lm.autos_3, test = "F")

# We can clearly state that all predictors do have a strong effect on the response variable. We now include
# the relevant two-fold interactions as figured out in the graphical analysis.

lm.autos_4 <- lm(log(price) ~ age + powerPS + kilometer + gearbox + brand + fuelType + 
                   notRepairedDamage + vehicleType + age:brand + age:vehicleType +
                   kilometer:brand + kilometer:vehicleType, data = df)
summary(lm.autos_4)
alias(lm.autos_4)


# Including the relevant two-fold-interactions, the variability explained increases slightly. Again we check if all
# predictors including the two-fold-interactions have a significant effect on the response variable.

drop1(lm.autos_4, test = "F")

# All of our predictors and interactions do have a strong effect on our response variable.
# The variability explained is around 85%.

# We now check if we need to model non-linear effects. As previously seen in the graphical analysis.
# The assumption is that the age variable has a quadratic effect on the response variable. Therefore
# we try to model that with a poly function.

lm.autos_5 <- lm(log(price) ~ (poly(age, degree = 2) + powerPS + kilometer + gearbox + brand + fuelType +
                                 notRepairedDamage + vehicleType + poly(age, degree = 2):brand + 
                                 poly(age, degree = 2):vehicleType + kilometer:brand + kilometer:vehicleType),
                 data = df)

anova(lm.autos_4, lm.autos_5)
summary(lm.autos_5)
drop1(lm.autos_5, test = "F")

# There is strong evidence that age needs a quadratic term. So we leave that.

# We now have our final linear model with explained variability of around 85%. All predictors do have a strong
# effect on the response variable. The variables brand and vehicle type have a mixed effect.
# The coefficients for the parameters powerPS, fuelTypediesel and notRepairedDamagenein are positive and therefore
# have a positive effect on the response variable price. The coefficient for the parameter gearboxmanuell is negative
# and therefore has a negative impact on price. Interestingly, the kilometer and age parameters show a different
# influence on price. Whereas for up to 90'000 kilometers they have a mixed influence, the effect from 100'000
# kilometers on is clearly negative. This is a bit counter-intuitive and it would be interesting to check
# that with business people. An interpretation of all interaction terms is omitted.

# Predicted values

# Calculating in-sample RMSE

pred.lm.autos_5.in_sample <- predict(lm.autos_5, df)
error <- exp(pred.lm.autos_5.in_sample) - df$price
sqrt(mean(error^2))

# Our model is off by 2390 Euros for each car. This seems not to be satisfactory result.

# Calculating in-sample MAPE

residuals <- exp(pred.lm.autos_5.in_sample) - df$price
ape <- abs(residuals) / df$price
mean(ape)

# This means that our price prediction deviates on average 30% from the actual price.

# We want a model that does not overfit an generalize well. To answer the question if our model performs
# well on new data, we split up our data set randomly into a test and training data set, apply our model
# on the training data set and test it with the test data set.

set.seed(42)
rows <- sample(nrow(df))
df.sample <- df[rows, ]

split <- round(nrow(df) * 0.80)
df.train <- df.sample[1:split, ]
df.test <- df.sample[(split + 1):nrow(df.sample), ]

lm.autos_5_sample <- lm(log(price) ~ (poly(age, degree = 2) + powerPS + kilometer + gearbox + brand + fuelType +
                                 notRepairedDamage + vehicleType + poly(age, degree = 2):brand + 
                                 poly(age, degree = 2):vehicleType + kilometer:brand + kilometer:vehicleType),
                        data = df.train)

# Calculating out-of-sample RMSE

pred.lm.autos_5.out_sample <- predict(lm.autos_5_sample, df.test)
error <- exp(pred.lm.autos_5.out_sample) - df.test$price
sqrt(mean(error^2))

# Calculating out-of-sample MAPE

residuals <- exp(pred.lm.autos_5.out_sample) - df.test$price
ape <- abs(residuals) / df.test$price
mean(ape)

# There are no major deviations from the in-sample testing, so at least our model generalizes well.
# We leave it like that for the moment and hope for better results with different models.

# TODO: Cross-validation and residual analysis.


# Fitting a gam model -----------------------------------------------------

# In our data set we do have age and powerPS, two continuous variables with a possible non-linear effect.
# We allow them have a non-linear effect by specifying the smooth terms s(age) and s(powerPS) in the model.
# As a basis we use our linear model from above without any interactions.

gam.autos_1 <- gam(log(price) ~ s(age) + s(powerPS) + kilometer + gearbox + brand + fuelType +
                                 notRepairedDamage + vehicleType,
                 data = df)
summary(gam.autos_1)

# The summary output indicates that there there is strong evidence that age and powerPS have a non-linear
# effect on the response variable. The estimated degrees of freedom quantify the complexity of the smooth
# functions. Let’s visualise the effects of the variable age and powerPS.

plot(gam.autos_1, residuals = TRUE, select = 1, col = alpha("black", 0.1), shade = TRUE)
plot(gam.autos_1, residuals = TRUE, select = 2, col = alpha("black", 0.1), shade = TRUE)

# From a visual perspective, it is hard to interpret the estimated degrees of freedom for the smooth terms
# age and powerPS.

# Let's now fit our gam model with the interactions from the previously defined linear model.

gam.autos_2 <- gam(log(price) ~ s(age) + s(powerPS) + kilometer + gearbox + brand + fuelType +
                     notRepairedDamage + vehicleType + s(age, by = brand) + s(age, by = vehicleType) +
                     kilometer:brand + kilometer:vehicleType,
                   data = df)
summary(gam.autos_2)
anova(gam.autos_1, gam.autos_2, test = "F")

# There is strong evidence that the model with the interactions better explains the effect on the response
# variable. We will trust our gam model by now and make some predictions in the same way we already did for
# the linear model.

# Calculating in-sample RMSE

pred.gam.autos_2.in_sample <- predict(gam.autos_2, df)
error <- exp(pred.gam.autos_2.in_sample) - df$price
sqrt(mean(error^2))

# Our model is off by 2130 Euros for each car. This is slightly better than with the linear model.

# Calculating in-sample MAPE

residuals <- exp(pred.gam.autos_2.in_sample) - df$price
ape <- abs(residuals) / df$price
mean(ape)

# This means that our price prediction deviates on average 28% from the actual price.

# Calculating out-of-sample RMSE

gam.autos_2_sample <- gam(log(price) ~ s(age) + s(powerPS) + kilometer + gearbox + brand + fuelType +
                     notRepairedDamage + vehicleType + s(age, by = brand) + s(age, by = vehicleType) +
                     kilometer:brand + kilometer:vehicleType,
                     data = df.train)

pred.gam.autos_2.out_sample <- predict(gam.autos_2_sample, df.test)
error <- exp(pred.gam.autos_2.out_sample) - df.test$price
sqrt(mean(error^2))

# Calculating out-of-sample MAPE

residuals <- exp(pred.gam.autos_2.out_sample) - df.test$price
ape <- abs(residuals) / df.test$price
mean(ape)

# There are no major deviations from the in-sample testing, so the model generalizes well.
# Also for the gam model the results are not convincing .

# TODO: Cross-validation and residual analysis.
