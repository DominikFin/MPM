# Installing packages -----------------------------------------------------

packages <- c("gitcreds",
              "tidyverse",
              "ggplot2",
              "readxl",
              "zoo",
              "car")
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
           age <= 10000) %>%
  select(-c("seller",
            "offerType",
            "abtest",
            "name",
            "nrOfPictures"))

# Keep models and brands with more than x observations

brand_table <- df %>%
  count(brand) %>%
  filter(n >= 4000)

brand_vector <- as.character(brand_table$brand)

model_table <- df %>%
  count(model) %>%
  filter(n >= 4000)

model_vector <- as.character(model_table$model)

# Clean data step 2

df <- df %>% 
  filter(brand %in% brand_vector &
           model %in% model_vector &
           model != "andere")

# Change data types

str(df)
df <- df %>%
  mutate_if(sapply(df, is.character), as.factor) %>%
  mutate(kilometer = as.factor(kilometer))

# Investigate variables ---------------------------------------------------

# price

summary(df$price)
hist(df$price, xlab = "price", main = "Histogram response variable price")
quantile(df$price, 0.95)

# Our response variable is extremely right-skewed as the histogram shows. Prices
# range from 500 to 40'000. 95% of all observations show a price which is 20'000
# or less. As our response variable is considered as amount, we will log-transform
# it in the modelling part. This helps also to reduce right-skewness.

# age

summary(df$age)
hist(df$age, xlab = "age in days", main = "Histogram age of cars")

# Half of the cars offered are older than 4513 / 365 = 12 years. The data seems to approximately
# follow a normal distribution.

# powerPS

summary(df$powerPS)
hist(df$powerPS, xlab = "PS", main = "Histogram PS of cars")
ecdf(df$powerPS)(120) - ecdf(df$powerPS)(100)

# The distribution of the PS values is also right-skewed. 20 % of all observations show values 
# between 100 and 120 PS. Prices fitted from PS values should better fit in this range.

# kilometer

summary(df$kilometer)
barplot(table(df$kilometer), xlab = "number of kilometers", main = "Mileage of cars")
df %>%
  count(kilometer)

# The mileage variable form the data set is most likely a categorical variable where the 
# advertiser could set a predefined value. The distribution is extremely left-skewed.
# The share of cars with 125'000 or more kilometers is around 80%.

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
summary(df$model)
ggplot(data=df, aes(x=brand, fill = model)) +
  geom_bar(stat = "count") +
  scale_fill_brewer(palette = "Set3") + 
  ggtitle("Brands and models") +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.text.y = element_text(size = 12))
  
# Volkswagen is the dominant car brand with more than 30'000 cars advertised.
# The VW Golf is the dominant model advertised, followed by the BMW 3er.

# fuelType

summary(df$fuelType)
barplot(table(df$fuelType), main = "Fuel type of cars")
range(df$yearOfRegistration)

# We focus on "Benzin" and "Diesel". There are no electric vehicles advertised in the data set
# which is not much of a surprise as the car registration values for the advertised cars
# range from 1988 to 2015.

#notRepairedDamage

summary(df$notRepairedDamage)
barplot(table(df$notRepairedDamage), main = "Repaired damage")

# 8% of the advertised cars had a damage and were repaired whereas 82% had no damage.


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
# 220 PS on. Is this random noised due to decreasing observations or does that to be modeled?

# kilometer as continuous variable

template.scatterplot.df + aes(x = as.numeric(kilometer))

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

# There are some differences in prices for the different vehicle types. Buses are the most
# expensive cars whereas kleinwagen seem to be lowest in price. This is not a surprise.
# The variability for cabrio, coupe, kleinwagen, kombi and limousine is larger then for
# bus and suv. If all the different vehicle types have a significant influence on price
# will be checked in the modelling part.

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

# Cars form the brand Audi will be advertised the most expensive whereas Opel cars will be
# advertised with lower prices. Variability looks pretty constant over all brands.

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
# another factor? Or in other words, is there any interaction? For example is the kilometer
# depreciation in price different among the brands. Is the age depreciation in price
# different among the brands? Or is the depreciation in price from kilometers and age different
# among the vehicle types? We will test that in the modelling part.

ggplot(data = df,
       mapping = aes(y = log(price), x = age)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  facet_wrap(. ~ brand)

# We can see that the price depreciation from age behaves similarly for the different brands.
# For the brands audi an opel we do not have many data points for an age above 7500 days.
# Especially for the audi brand this abrupt missing of values is surprising.

ggplot(data = df,
       mapping = aes(y = log(price), x = as.numeric(kilometer))) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  facet_wrap(. ~ brand)

# We changed the kilometer variable to numeric in order to plot some smoothers to compare the
# different brands. Again, the behavior looks similar. BMW cars tend to have higher depreciation
# in the beginning and the end. For kilometers between 20 and 40k BMW's tend to raise in value.
# This is really interesting. The same behavior can be observed for the brands Mercedes-Benz
# and Opel, which have periods where the effect of depreciation is not linearly negative until
# about 50'000 km driven.

ggplot(data = df,
       mapping = aes(y = log(price), x = age)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  facet_wrap(. ~ vehicleType)

# Again the curves look similar in terms of age depreciation among the different vehicle types 
# except for buses and suv's. There is an abrupt change in observations for buses older than
# 4000 days. For the vehicle type suv's it becomes clear that there are simply too less
# observations. SUV's became popular only recently. For cabrios the turning point is around
# 7000 days. From there on, cabrios gain in value faster than other vehicle types. This may be
# due to the attraction of vintage cabrios.

ggplot(data = df,
       mapping = aes(y = log(price), x = as.numeric(kilometer))) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  facet_wrap(. ~ vehicleType)

# The graphics looks similar except for coupes. Coupes are gaining in value until about
# 50'000 kilometers driven. This seems counter-intuitive and may be due to the fact that
# there are not many data points until then. Also the graph for suv's is useless respectively
# not even produced. Therefore we strap the suv's from our dataset.

df <- df %>% 
  filter(vehicleType != "suv")


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

# Let's take the example of the model Polo. Indeed, it is logical that whenever the brand is
# Volkswagen and the model is not Golf or Passat it must be a VW Polo. How could we solve
# that issue? Create one variable out of model and brand?

df["brandModel"] = paste(df$brand,df$model,sep = "_")

# Fit model again with new predictor brandModel

lm.autos_2 <- lm(log(price) ~ age + powerPS + kilometer + gearbox + brandModel + fuelType +
                   notRepairedDamage + vehicleType,
                 data = df)
summary(lm.autos_2)

# If we compare the summary statistics of lm.autos_1 with lm.autos_2 we state that the model got
# simpler as there are less variables but the variability explained remains unchanged.

# Do we lose some variability if we only focus on brands instead of brands and models?
# Let's check it once only including the brands and once only including the models.

lm.autos_3 <- lm(log(price) ~ age + powerPS + kilometer + gearbox + brand + fuelType +
                   notRepairedDamage + vehicleType,
                 data = df)
summary(lm.autos_3)

# Only a tiny little bit not including the models.

lm.autos_4 <- lm(log(price) ~ age + powerPS + kilometer + gearbox + model + fuelType +
                   notRepairedDamage + vehicleType,
                 data = df)
summary(lm.autos_4)

# Of course this is the same again with the variable brandmodel as there is actually no difference
# between the two as models specify the brands more precisely.

# We now check our model with the "brandModel" predictor in terms of collinearity again

vif(lm.autos_2)

# As we can see there are some GVIFs values above 10 so we have some collinearity issues. We check
# lm.model_3 only including the "brand" predictor as well for collinearity.

vif(lm.autos_3)

# Here the GVIFs values are just fine so that we decide to strap the models form our model. 

# We test as well if all predictors do have a significant effect on the response variable with
# the drop1 command:

drop1(lm.autos_3, test = "F")

# We can clearly state that all predictors do have a strong effect on the response variable.

# We now include all the two-fold interactions between the variables.

lm.autos_5 <- lm(log(price) ~ (age + powerPS + kilometer + gearbox + brand + fuelType +
                                 notRepairedDamage + vehicleType)^2,
                 data = df)
summary(lm.autos_5)

alias(lm.autos_5)

# Including all two-fold-interactions, the variability explained only changes slightly. Again
# we check if all predictors including the two-fold-interactions between them have a significant
# effect on the response variable.

drop1(lm.autos_5, test = "F")

# We update our model now only including the relevant interactions.

lm.autos_6 <- lm(log(price) ~ (age + powerPS + kilometer + gearbox + brand + fuelType +
                                 notRepairedDamage + vehicleType + age:powerPS + age:kilometer +
                                 age:gearbox + age:brand + age:fuelType + age:notRepairedDamage +
                                 age:vehicleType + powerPS:kilometer + powerPS:gearbox + powerPS:brand +
                                 powerPS:fuelType + powerPS:notRepairedDamage + powerPS:vehicleType +
                                 kilometer:gearbox + kilometer:brand + kilometer:fuelType +
                                 kilometer:notRepairedDamage + kilometer:vehicleType + gearbox:brand +
                                 gearbox:fuelType + gearbox:notRepairedDamage + gearbox:vehicleType +
                                 brand:fuelType + brand:vehicleType + fuelType:notRepairedDamage +
                                 fuelType:vehicleType + notRepairedDamage:vehicleType),
                               data = df)
summary(lm.autos_6)
drop1(lm.autos_6, test = "F")

# All of our predictors and interactions do have a moderate to strong effect on our response variable.
# The variability explained is around 87%.

# We now check if we need to model non-linear effects. As previously seen in the graphical analysis.
# The assumption is that the age variable has a quadratic effect on the response variable. Therefore
# we try to model that with a poly function.

lm.autos_7 <- lm(log(price) ~ (poly(age, degree = 2) + powerPS + kilometer + gearbox + brand + fuelType +
                                 notRepairedDamage + vehicleType + poly(age, degree = 2):powerPS +
                                 poly(age, degree = 2):kilometer + poly(age, degree = 2):gearbox +
                                 poly(age, degree = 2):brand + poly(age, degree = 2):fuelType +
                                 poly(age, degree = 2):notRepairedDamage + poly(age, degree = 2):vehicleType +
                                 powerPS:kilometer + powerPS:gearbox + powerPS:brand +
                                 powerPS:fuelType + powerPS:notRepairedDamage + powerPS:vehicleType +
                                 kilometer:gearbox + kilometer:brand + kilometer:fuelType +
                                 kilometer:notRepairedDamage + kilometer:vehicleType + gearbox:brand +
                                 gearbox:fuelType + gearbox:notRepairedDamage + gearbox:vehicleType +
                                 brand:fuelType + brand:vehicleType + fuelType:notRepairedDamage +
                                 fuelType:vehicleType + notRepairedDamage:vehicleType),
                 data = df)
anova(lm.autos_6, lm.autos_7)
summary(lm.autos_7)
drop1(lm.autos_7, test = "F")

# There is pretty strong evidence that age needs a quadratic term. So we leave that.
# We now have out final linear model with explained variability of around 88%. All predictors
# do have an weak (gearbox:notRepairedDamage) to strong (various examples) effect on the
# response variables. The coefficients for the parameters age, gearboxmauell, brandopel,
# vehicleTypecoupe, # vehicleTypekleinwagen, vehicleTypekombi and vehicleTypelimousine are 
# negative and have therefore a negative effect on the advertised price. The parameters 
# powerPS, brandbmw, brandmercedes_benz, brandvolkswagen, fuelTypediesel, notRepairedDamagenein,
# vehicleTypecabrio have a positive effect on the price. Interestingly, the kilometer parameters
# show a different influence on price. Whereas for up to 30'000 kilometers they do have a
# negative effect on prices, they have a positive effect in the range from 40'000 kilometers
# to 100'000 kilometers. This is a bit counter-intuitive and it would be interesting to check
# that with business people. An interpretation of all interaction terms is omitted.

# Residual analysis and validation of model








