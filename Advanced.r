set.seed(1234)
train <-read.csv("C:/Users/anishjoshi500/Downloads/KaggleSantander/train.csv")
test<-read.csv("C:/Users/anishjoshi500/Downloads/KaggleSantander/test.csv")


train$ID <- NULL
test.id <- test$ID
test$ID <- NULL


train.y <- train$TARGET
train$TARGET <- NULL


count0 <- function(x) {
  return( sum(x == 0) )
}
train$n0 <- apply(train, 1, FUN=count0)
test$n0 <- apply(test, 1, FUN=count0)


cat("\n## Removing the constants features.\n")
for (f in names(train)) {
  if (length(unique(train[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    train[[f]] <- NULL
    test[[f]] <- NULL
  }
}


features_pair <- combn(names(train), 2, simplify = F)
toRemove <- c()
for(pair in features_pair) {
  f1 <- pair[1]
  f2 <- pair[2]
  
  if (!(f1 %in% toRemove) & !(f2 %in% toRemove)) {
    if (all(train[[f1]] == train[[f2]])) {
      cat(f1, "and", f2, "are equals.\n")
      toRemove <- c(toRemove, f2)
    }
  }
}

feature.names <- setdiff(names(train), toRemove)

train <- train[, feature.names]
test <- test[, feature.names]

train$TARGET <- train.y
mylogit <- glm(TARGET ~ saldo_medio_var33_hace3+saldo_medio_var33_ult1+saldo_medio_var33_ult3+saldo_medio_var44_hace2, data = train, family = "binomial")
summary(mylogit)
confint(mylogit)
test$TargetValue <- predict(mylogit, newdata = test, type = "response")
print(test$TargetValue)
