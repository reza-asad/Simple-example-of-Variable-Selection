# Author: Reza Asad

library(randomForest)
# Get the data
x.tr <- read.table("train.dat", header = TRUE);
x.te <- read.table("test.dat", header = TRUE);
tru.class <- read.table("tru.class.dat", header = FALSE);

# Visualize the Data! 
# This could Help finding significant variables.
# Replace x1 and x2 to see which varaibles show
# a pattern in the data.
plot(x1 ~ x2, data = x.tr, pch = c(1, 2)[y+1], 
   cex = 1.25, col = c("blue", "red")[y+1])

# After you found siginificant variables, this tells
# u which of the other varaibles are strongly correlated
# with the significant varaibles, i.e they are not adding 
# anything new to the model.
correlation <- cor(x.tr[,-11])

# Training with Random Forest
set.seed(123)
# Here I'm removing the insignificant variables
x.train <- x.tr[,-c(11,9,10,3,4,7,8)];
y = factor(x.tr$y)
dat.train <- data.frame(x.train,y)
rf <- randomForest(y~., data = dat.train, ntree = 100)

# Testing
x.test <- x.te[,-c(11,9,10,3,4,7,8)];
y = factor(y[1:nrow(x.test)])
dat.test <- data.frame(x.test,y)
test.pr <- predict(rf, newdata = dat.test, type = 'class')

# Misclassification Error
mean(test.pr!=tru.class[,1])

# This results a misclassification error of 0.055
