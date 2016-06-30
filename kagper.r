# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(boot)

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.
train <- read.csv("C:/Users/anishjoshi500/Downloads/KaggleSantander/train.csv")
test <- read.csv("C:/Users/anishjoshi500/Downloads/KaggleSantander/test.csv")
#View(train$ID)
logit <- glm(TARGET~ num_meses_var5_ult3 + var15 + saldo_var30 + var38 + num_var22_ult3 + ind_var30_0 + num_var30 + num_op_var39_efect_ult3 + saldo_medio_var8_hace2 + ind_var8_0 
            + num_var5 + saldo_medio_var5_ult1 + num_aport_var13_hace3, data = train,family = binomial())
summary(logit)
confint(logit)
logit.pred <- predict(logit,test,type="response")
mycost <- function(r, pi = 0) {
mean(abs(r-pi) > 0.5)
}

unsatisfied<-which(logit.pred>0.5)
print(length(unsatisfied))
print(logit.pred[unsatisfied])
plot(logit.pred)
cv.errork10<- cv.glm(train, logit, cost=mycost, K=10)
cv.errork10$delta
output = data.frame(test$ID,logit.pred)
print(head(output))
names(output) = c("ID","TARGET")
write.table(output,file = "lr.csv", sep=",",col.names = T, row.names = F)
