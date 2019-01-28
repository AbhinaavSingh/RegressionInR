

# car_sales

car = read.csv("car_sales.csv")
head(car)
nrow(car)	# 157

car.lm = lm(mpg~price+engine_s+horsepow+curb_wgt, data=car)
summary(car.lm)

# horsepow and price do not appear to be significant predictors
car.lm = step(car.lm)
# gives error because of missing data

# so need to remove rows with missing data
car = na.omit(car)
nrow(car)	# 117

car.lm = lm(mpg~price+engine_s+horsepow+curb_wgt, data=car)
car.lm = step(car.lm)

summary(car.lm)

# so, only engine_s and curb_wgt are significant predictors

# make the prediction required - can ignore the given values of price 
# and horsepow since they do not appear in the model

predict(car.lm, data.frame(engine_s=3, curb_wgt=3))

# answer is 25.667

###############################################################


# Enquirer.csv

enq = read.csv("Enquirer.csv")
head(enq)
nrow(enq)
# remove missing data if any
enq = na.omit(enq)
nrow(enq)     # there were no misssing data!

enq.lm = lm(pass~adc+freelunch+medincome, data=enq)
summary(enq.lm)

# all the predictors are good predictors, so no need to run step

# make the prediction
predict(enq.lm, data.frame(adc=2, freelunch=3, medincome=30000))

# answer is 79.72017

##############################################################

# Credit2.csv

cr = read.csv("credit2.csv")
nrow(cr)
head(cr)
# remove missing data if any:
cr = na.omit(cr)

cr.lm = lm(Limit~Income+Rating+Cards+Age+Balance, data=cr)
cr.lm = step(cr.lm)

summary(cr.lm)

# only Rating and Cards are good predictors
# make the prediction

predict(cr.lm, data.frame(Rating=300, Cards=2))

# answer is 3985.924

#############################################################
#############################################################

# Logistic Regression

# Bank.csv

bank = read.csv("Bank.csv")
head(bank)

# The variable "Direct" indicates whether a person will use 
# direct banking or not (1 indicates Yes, 0 indicates No)

bank.glm = glm(Direct~Balance, data=bank, family=binomial)
summary(bank.glm)

# Balance is a good predictor

# make the prediction:

predict(bank.glm, data.frame(Balance=3), type="response")

# so the probability of doing direct banking is 0.122.
# This will round down to zero, so a person with a balance of 3 will 
# NOT use direct banking

#############################################################

# Titanic.csv
# This is a famous (and real) dataset!!
# This is about the Titanic disaster and contains information about 
# who actually survived and who did not.
# This dataset has been analyzed a countless number of times...
 

titan = read.csv("Titanic.csv")
head(titan)

titan.glm = glm(Survived~Class+Sex+Age, data=titan, family=binomial)
summary(titan.glm)

# all three predictors are good predictors!

# categorical data must be enclosed in quotes
predict(titan.glm, data.frame(Class="1st", Sex="Male", Age="Adult"), type="response")

# the probaility of a Male Adult traveling 1st class who survived is 0.407
# rounding will give 0, so he would not have survived...

# That's actually true - most of the survivors were women - they were 
# allowed to get into the lifeboats first (did you see the movie?)

