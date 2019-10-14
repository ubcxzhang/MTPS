## ----library, echo = T, results = 'hide', warning=FALSE,message=FALSE----
library(MTPS)

## ----data----------------------------------------------------------------
data("HIV")
head(YY)
XX[8:9, 7:10]
dim(YY)
dim(XX)

## ----ctn-split-data, echo = T, results = 'hide'--------------------------
set.seed(12345)
xmat <- as.matrix(XX)
ymat <- as.matrix(YY)
nobs <- nrow(xmat)
id <- createFolds(rowMeans(XX), k=5, list=F)
training.id <- sample(seq_len(nobs), size = 0.8 * nobs)
y.train <- ymat[training.id, ]
y.test  <- ymat[-training.id, ]
x.train <- xmat[training.id, ]
x.test  <- xmat[-training.id, ]

## ----ctn-noss, echo = T, results = 'hide'--------------------------------
# no stacking
fit.mult <- multiFit(xmat = x.train, ymat = y.train, method = glmnet1, family = "gaussian")

