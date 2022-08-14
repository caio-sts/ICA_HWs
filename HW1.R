d1=read.csv("Boston.csv",sep=",",header=TRUE)

cols = names(d1)

for (i in cols) {
  hist(d1[i])
}

df <- data.frame( LSTAT = d1$LSTAT, RM= d1$RM,PTRATIO = d1$PTRATIO, INDUS = d1$INDUS, MEDV = d1$MEDV)

media <- sapply(Filter(is.numeric, df[, 2:5]), mean)

hist(d1$LSTAT)
desvpad <- sapply(Filter(is.numeric, df[, 2:5]), sd)

#install.packages("timeSeries")
library(timeSeries)

obliq <- colSkewness(df[, 2:5],pvalue=true)

resumo <- data.frame(media=media, desvioPadrao=desvpad, skewness = obliq)
resumo









library("ggplot2")
library("GGally")  

ggpairs(df[, 1:4]) 

\end{lstlisting}

\subsection*{Matriz de correlação visual}
\begin{lstlisting}[language = R]
#install.packages("corrplot")

library(corrplot)

res <- cor(d1) 
round(res, 2)

corrplot(res, method="circle")












install.packages("ggfortify")
library(ggfortify)
df <- data.frame( LSTAT = d1$LSTAT, RM= d1$RM,PTRATIO = d1$PTRATIO, INDUS = d1$INDUS, MEDV = d1$MEDV)

PCbiplot <- function(PC, x="PC1", y="PC2", colors=c('black', 'black', 'red', 'red')) {
    # PC being a prcomp object
    #data <- data.frame(obsnames=row.names(PC$x), PC$x)
  data<-data.frame(obsnames= 1:506, PC$x)
    plot <- ggplot(data, aes_string(x=x, y=y)) + geom_text(alpha=.4, size=3, aes(label=obsnames), color=colors[1])
  
    plot <- plot + geom_hline(aes(0), size=.2, yintercept=1, xintercept=1) + geom_vline(aes(0), size=.2, color=colors[2], xintercept=1)
    datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
    mult <- min(
        (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
        (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
        )
    datapc <- transform(datapc,
            v1 = .7 * mult * (get(x)),
            v2 = .7 * mult * (get(y))
            )
    plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color=colors[3])
    plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color=colors[4])
    plot
}

pca.eg <- prcomp(df, scale=T)
PCbiplot(pca.eg, colors=c("black", "black", "red", "yellow"))
















install.packages("glmnet")
library(glmnet)
library(caret)



cols = c('LSTAT', 'RM', 'PTRATIO', 'INDUS')

pre_proc_val <- preProcess(train.data[,cols], method = c("center", "scale"))

train.data[,cols] = predict(pre_proc_val, train.data[,cols])
test.data[,cols] = predict(pre_proc_val, test.data[,cols])


lr <- lm(MEDV ~., data = train.data)



eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE = sqrt(SSE/nrow(df))

  
  # Model performance metrics
data.frame(
  RMSE = RMSE,
  Rsquare = R_square
)
  
}

# Prediction and evaluation on train data
pred_lr <- predict(lr, newx = train.data)
eval_results(train.data$MEDV, pred_lr, train.data)


# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
lr_10fold <- train(MEDV ~., data = train.data, method = "lm",
               trControl = train.control)
# Summarize the results
print(lr_10fold)













cols_reg = c('LSTAT', 'RM', 'PTRATIO','INDUS', 'MEDV')
 
library("caret")

lett <- dummyVars(MEDV ~ ., data = df[,cols_reg])

train_lett = predict(lett, newdata = train.data[,cols_reg])

test_lett = predict(lett, newdata = test.data[,cols_reg])

print( dim(train_lett) ); print( dim(test_lett) )


x = as.matrix(train_lett)
y_train = train.data$MEDV


x_test = as.matrix(test_lett)
y_test = test.data$MEDV



##Ridge normal
ridge_reg = glmnet(x, y_train, nlambda = 25, alpha = 0, family = 'gaussian')

lambda_ord = min(ridge_reg$lambda)

##Ridge com cross validation
set.seed(34)
cv_ridge <- cv.glmnet(x, y_train, alpha = 0, lambda = lambdas)
plot(cv_ridge)
optimal_lambda <- cv_ridge$lambda.min
optimal_lambda















# Predicao e avaliacao nos dados de treino para ridge sem validacao cruzada
predictions_train <- predict(ridge_reg, s = lambda_ord, newx = x)
eval_results(y_train, predictions_train, train.data)

# Predicao e avaliacao nos dados de teste para ridge sem validacao cruzada
predictions_test <- predict(ridge_reg, s = lambda_ord, newx = x_test)
eval_results(y_test, predictions_test, test.data)


# Predicao e avaliacao nos dados de treino para ridge com validacao cruzada
predic_train <- predict(cv_ridge, s = optimal_lambda, newx = x)
eval_results(y_train, predic_train, train.data)

# Predicao e avaliacao nos dados de teste para ridge com validacao cruzada
predic_test <- predict(cv_ridge, s = optimal_lambda, newx = x_test)
eval_results(y_test, predic_test, test.data)













set.seed(2)

pcr.fit1=pcr(MEDV~., data=test.data,scale=TRUE, validation="CV")
summary(pcr.fit1)
validationplot(pcr.fit1,val.type="MSEP")


predic_test_pcr <- predict(pcr.fit1, newx = x_test)
eval_results(y_test, predic_test_pcr, test.data)


set.seed(2)

pcr.fit2=pcr(MEDV~., data=train.data, scale=TRUE, validation="CV")
validationplot(pcr.fit,val.type="MSEP")

predic_train_pcr <- predict(pcr.fit2, newx = x)
eval_results(y_train, predic_train_pcr, train.data)

summary(pcr.fit2)
