

# script parte 03 - lab 02

# ------------------------------------------------------------------------------
# CARET

library(caret)

set.seed(107)

in.train <- createDataPartition(y = mtcars$mpg,
                                p = 0.75)

mtcars.train <- mtcars[in.train[[1]],]
mtcars.test <- mtcars[-in.train[[1]],]


model.mtcars_lm <- train(mpg ~ wt
                         ,mtcars.train
                         ,method = "lm"
)

mtcars.pred <- predict(model.mtcars_lm, mtcars.test)
sqrt(mean(mtcars.pred-mtcars.test$mpg)^2)



# ------------------------------------------------------------------------------
# Ridge e Lasso

library(ISLR)
library(caret)

Hitters <- na.omit(Hitters)

split <- createDataPartition(y=Hitters$Salary, p = 0.5, list = FALSE)
train <- Hitters[split,]
test <- Hitters[-split,]

ridge.pred <- predict(ridge, test)

sqrt(mean(ridge.pred - test$Salary)^2)


set.seed(825)
fitControl <- trainControl(method = "cv",
                           number = 10)
# Set seq of lambda to test
lambdaGrid <- expand.grid(lambda = 10^seq(10, -2, length=100))

ridge <- train(Salary~., data = train,
               method='ridge',
               trControl = fitControl,
               tuneGrid = lambdaGrid,
               preProcess=c('center', 'scale')
)

ridge


predict(ridge$finalModel, type='coef', mode='norm')$coefficients[19,]

ridge.pred <- predict(ridge, test)
sqrt(mean(ridge.pred - test$Salary)^2)

lmfit <- train(Salary ~., data = train,
               method='lm',
               trControl = fitControl,
               preProc=c('scale', 'center'))
lmfit

coef(lmfit$finalModel)


lmfit.pred <- predict(lmfit, test)
sqrt(mean(lmfit.pred - test$Salary)^2)



lasso <- train(Salary ~., train,
               method='lasso',
               preProc=c('scale','center'),
               trControl=fitControl)
lasso

predict.enet(lasso$finalModel, type='coefficients', s=lasso$bestTune$fraction, mode='fraction')

lasso.pred <- predict(lasso, test)
sqrt(mean(lasso.pred - test$Salary)^2)

plot(varImp(ridge, scale = FALSE))

plot(varImp(lasso, scale = FALSE))

plot(varImp(lmfit, scale = FALSE))









