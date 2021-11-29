library(forecast)
library(fpp2)
library(seasonal)

## 1 ##
head(pigs)
sesPig <- ses(pigs, h = 4)
round(accuracy(sesPig),2)
autoplot(sesPig) +
  autolayer(fitted(sesPig), series="Fitted") +
  ylab("Pigs Slaghtered") + xlab("Year")

## 2 ##

sespigPig2 <- ses(pigs,h = 4, initial = c("optimal", "simple")
autoplot(sesPig) +
      autolayer(fitted(sespigPig2), series="Fitted") +
      ylab("Pigs Slaghtered") + xlab("Year")
                  
## 5 ##

plot(books)
                  
paperback <- ses(books[, "Paperback"], h = 4)
hardcover <- ses(books[, "Hardcover"], h = 4)

autoplot(books[, "Paperback"], series = "Paperback") +
  autolayer(paperback, series = "Paperback") +
  autolayer(books[, "Hardcover"], series = "Hardcover") +
  autolayer(hardcover, series = "Hardcover") +
  ylab("Sales") +
  ggtitle("Sales of paperback and hardcover books")

sqrt(mean(paperback$residuals^2))
sqrt(mean(hardcover$residuals^2))

## 6 ##

H_paperback <- holt(books[, "Paperback"], h = 4)
H_hardcover <- holt(books[, "Hardcover"], h = 4)

autoplot(books[, "Paperback"]) +
  autolayer(H_paperback)
autoplot(books[, "Hardcover"]) +
  autolayer(H_hardcover)

RSME_paper <-sqrt(mean(H_paperback$residuals^2))
RSME_hard <- sqrt(mean(H_hardcover$residuals^2))


H_paperback$upper[1, "95%"]
H_paperback$lower[1, "95%"]

H_paperback$mean[1] + 1.96*RSME_paper
H_paperback$mean[1] - 1.96*RSME_paper

H_hardcover$upper[1, "95%"]
H_hardcover$lower[1, "95%"]

H_hardcover$mean[1] + 1.96*RSME_hard
H_hardcover$mean[1] - 1.96*RSME_hard

## 7 ##

Holt_eggs <- holt(eggs, h= 100)
autoplot(Holt_eggs) +
  autolayer(Holt_eggs$fitted)

Holt_eggs2 <- holt(eggs, h = 100, damped= TRUE)
autoplot(Holt_eggs2) +
  autolayer(Holt_eggs2$fitted)

ses_eggs <- ses(eggs, h = 100)
autoplot(ses_eggs) +
  autolayer(ses_eggs$fitted)

sqrt(mean(Holt_eggs$residuals^2))
sqrt(mean(Holt_eggs2$residuals^2))
sqrt(mean(ses_eggs$residuals^2))

## 10 ##

plot(ukcars)
sns_cars <- ukcars %>% stl(s.window = 4, robust = TRUE) %>% seasadj() 
autoplot(sns_cars)

cars_add<- sns_cars %>% seasadj() %>%
  stlf(etsmodel="AAN", damped=TRUE)

autoplot(carS_add)

model_cars <- ets(ukcars, model = "ZZZ", damped = NULL)
summary(model_cars)
autoplot(model_cars)
checkresiduals(model_cars)

## 11 ##
plot(visitors)

train <- window(visitors, end = c(2003,12))
visitors_mult <- hw(train, seasonal = "multiplicative")
autoplot(visitors_mult)

ETS <- ets(train, model = "ZZZ")
ETS_for <- forecast(ETS, h = 24)
autoplot(ETS_for)

vis_box <- ets(train, model = "ZZZ", lambda = BoxCox.lambda(train), additive.only = TRUE)

box_for<- forecast(vis_box, h=24)
autoplot(box_for)

for_naive <- snaive(train, h=24)
autoplot(for_naive)

box_sns <- stlf(train, t.window = 13, s.window = "periodic", h = 24, robust = TRUE, method = "ets", lambda = BoxCox.lambda(train))

autoplot(box_sns)


checkresiduals(ETS)
checkresiduals(vis_box)
checkresiduals(for_naive)
checkresiduals(box_sns)

accuracy(ETS)
accuracy(vis_box)
accuracy(for_naive)
accuracy(box_sns)

## 13 ##

##ausbeer ##
beer_ets <- ets(ausbeer, model = "ZZZ")
beer_for1 <- forecast(beer_ets, h = 20)
autoplot(beer_for1)

beer_naive <- snaive(ausbeer, h = 20)
autoplot(beer_naive)

beer_box <- stlf(ausbeer, t.window = 13, s.window = "periodic", h = 24, robust = TRUE, method = "ets", lambda = BoxCox.lambda(ausbeer))
autoplot(beer_box)

## bricksq  ##

brick_ets <- ets(bricksq, model = "ZZZ")
brick_for1 <- forecast(brick_ets, h = 20)
autoplot(brick_for1)

brick_naive <- snaive(bricksq, h = 20)
autoplot(brick_naive)

brick_box <- stlf(bricksq, t.window = 13, s.window = "periodic", h = 24, robust = TRUE, method = "ets", lambda = BoxCox.lambda(bricksq))
autoplot(brick_box)

## dole ##

dole_ets <- ets(dole, model = "ZZZ")
dole_for1 <- forecast(dole_ets, h = 20)
autoplot(dole_for1)

dole_naive <- snaive(dole, h = 20)
autoplot(dole_naive)

dole_box <- stlf(dole, t.window = 13, s.window = "periodic", h = 24, robust = TRUE, method = "ets", lambda = BoxCox.lambda(dole))
autoplot(dole_box)

## a10 ##

a10_ets <- ets(a10, model = "ZZZ")
a10_for1 <- forecast(a10_ets, h = 20)
autoplot(a10_for1)

a10_naive <- snaive(a10, h = 20)
autoplot(a10_naive)

a10_box <- stlf(a10, t.window = 13, s.window = "periodic", h = 24, robust = TRUE, method = "ets", lambda = BoxCox.lambda(a10))
autoplot(a10_box)

## h02 ##

h02_ets <- ets(h02, model = "ZZZ")
h02_for1 <- forecast(h02_ets, h = 20)
autoplot(h02_for1)

h02_naive <- snaive(h02, h = 20)
autoplot(h02_naive)

h02_box <- stlf(h02, t.window = 13, s.window = "periodic", h = 24, robust = TRUE, method = "ets", lambda = BoxCox.lambda(h02))
autoplot(h02_box)

## usmelec ##

usme_ets <- ets(usmelec, model = "ZZZ")
usme_for1 <- forecast(usme_ets, h = 20)
autoplot(usme_for1)

usme_naive <- snaive(usmelec, h = 20)
autoplot(usme_naive)

usme_box <- stlf(usmelec, t.window = 13, s.window = "periodic", h = 24, robust = TRUE, method = "ets", lambda = BoxCox.lambda(usmelec))
autoplot(usme_box)


checkresiduals(beer_ets)
checkresiduals(beer_naive)
checkresiduals(beer_box)
checkresiduals(brick_ets)
checkresiduals(brick_naive)
checkresiduals(brick_box)
checkresiduals(dole_ets)
checkresiduals(dole_naive)
checkresiduals(dole_box)
checkresiduals(a10_ets)
checkresiduals(a10_naive)
checkresiduals(a10_box)
checkresiduals(h02_ets)
checkresiduals(h02_naive)
checkresiduals(h02_box)
checkresiduals(usme_ets)
checkresiduals(usme_naive)
checkresiduals(usme_box)

## 14 ##

coal_ets <- ets(bicoal, model = "ZZZ")
coal_for1 <- forecast(coal_ets, h = 20)
autoplot(coal_for1)

chkn_ets <- ets(chicken, model = "ZZZ")
chkn_for1 <- forecast(chkn_ets, h = 20)
autoplot(chkn_for1)

dole_ets <- ets(dole, model = "ZZZ")
dole_for1 <- forecast(dole_ets, h = 20)
autoplot(dole_for1)

us_ets <- ets(usdeaths, model = "ZZZ")
us_for1 <- forecast(us_ets, h = 20)
autoplot(us_for1)

lynx_ets <- ets(lynx, model = "ZZZ")
lynx_for1 <- forecast(lynx_ets, h = 20)
autoplot(lynx_for1)

ibm_ets <- ets(ibmclose, model = "ZZZ")
ibm_for1 <- forecast(ibm_ets, h = 20)
autoplot(ibm_for1)

eggs_ets <- ets(eggs, model = "ZZZ")
eggs_for1 <- forecast(eggs_ets, h = 20)
autoplot(eggs_for1)


