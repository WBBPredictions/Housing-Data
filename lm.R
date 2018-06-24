dat <- Cross_val_maker(train, .1)
train2 <- dat$Train
test2 <- dat$Test
fit2 <- lm(Age~Pclass+Sex, data = train2)
pred2 <- predict(fit2, test2[,c(3, 5)])
summary(fit2)
plot()