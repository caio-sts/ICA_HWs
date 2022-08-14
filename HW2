d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d3=merge(d1,d2,all = TRUE)










library(dplyr)
d3$romantic=case_when(d3$romantic=="yes" ~ 1L, d3$romantic=="no" ~ 0L  );

d3$internet=case_when(d3$internet=="yes" ~ 1L, d3$internet=="no" ~ 0L  );

d3$nursery=case_when(d3$nursery=="yes" ~ 1L, d3$nursery=="no" ~ 0L  );
d3$schoolsup=case_when(d3$schoolsup=="yes" ~ 1L, d3$schoolsup=="no" ~ 0L  );
d3$famsup=case_when(d3$famsup=="yes" ~ 1L, d3$famsup=="no" ~ 0L  );
d3$higher=case_when(d3$higher=="yes" ~ 1L, d3$higher=="no" ~ 0L  );
d3$activities=case_when(d3$activities=="yes" ~ 1L, d3$activities=="no" ~ 0L  );
d3$paid=case_when(d3$paid=="yes" ~ 1L, d3$paid=="no" ~ 0L  );
d3$Fjob=case_when( d3$Fjob=="teacher" ~ 4L,d3$Fjob=="health" ~ 3L,d3$Fjob=="services" ~ 2L, d3$Fjob=="at_home" ~ 1L,d3$Fjob=="other" ~ 0L);
d3$Mjob=case_when( d3$Mjob=="teacher" ~ 4L, d3$Mjob=="health" ~ 3L, d3$Mjob=="services" ~ 2L, d3$Mjob=="at_home" ~ 1L, d3$Mjob=="other" ~ 0L);
d3$reason=case_when( d3$reason=="other" ~ 3L, d3$reason=="course" ~ 2L, d3$reason=="reputation" ~ 1L, d3$reason=="home" ~ 0L);
d3$guardian=case_when( d3$guardian=="other" ~ 2L, d3$guardian=="father" ~ 1L, d3$guardian=="mother" ~ 0L);
d3$famsize=case_when( d3$famsize=="GT3" ~ 1L, d3$famsize=="LE3" ~ 0L);
d3$Pstatus=case_when(d3$Pstatus=="A" ~ 1L, d3$Pstatus=="T" ~ 0L);
d3$sex=case_when(d3$sex=="M" ~ 1L, d3$sex=="F" ~ 0L);
d3$address=case_when(d3$address=="U" ~ 1L, d3$address=="R" ~ 0L);
d3$school=case_when(d3$school=="GP" ~ 1L, d3$school=="MS" ~ 0L);









media_stu <- sapply(Filter(is.numeric, d3), mean)

#hist(d1$LSTAT)
desvpad_stu <- sapply(Filter(is.numeric, d3), sd)

#install.packages("timeSeries")
library(timeSeries)

obliq_stu <- colSkewness(d3,pvalue=true)

resume_stu <- data.frame(media=media_stu, desvioPadrao=desvpad_stu, skewness = obliq_stu)
resume_stu

for (i in 1:length(names(d3))){
  boxplot(d3[i], xlab=colnames(d3)[i], ylab="values")
}


## Após analisar a relação entre os preditores e a saída

#install.packages("corrplot")
library(dplyr)
library(corrplot)
res <- cor(d3) 
round(res, 2)

corrplot(res, method = "circle")

df <- data.frame(Medu=d3$Medu, studytime=d3$studytime, failures=d3$failures, higher=d3$higher, G3 =d3$G3)

df$G3=case_when(d3$G3 < 10 ~ 0L, d3$G3>=10 ~ 1L);

colnames(df)[5] <- "Approved"

corrplot(cor(df), method="circle")

for (i in 1:length(names(df))){
  boxplot(df[i], xlab=colnames(df)[i], ylab="values")
}

# Histogramas dos preditores escolhidos

for (i in 1:length(names(df))){
  hist(df[,i], main=("histograma"), xlab=colnames(df)[i], ylab="Frequency" )
}












library(ggplot2)
PCbiplot <- function(PC, df, x="PC1", y="PC2", colors=c('black', 'black', 'red', 'red')) {
    # PC being a prcomp object
    #data <- data.frame(obsnames=row.names(PC$x), PC$x)
  
  data<-data.frame(obsnames= 1:1044, PC$x)
    
  plot <- ggplot(data, aes_string(x=x, y=y)) + geom_text(alpha=.4, size=3, aes(label=obsnames),color=ifelse(df$Approved == 1, 'green', 'red'))#Dados
  
    plot <- plot + geom_hline(aes(0), size=0.2, yintercept=1, xintercept=1) + geom_vline(aes(0), size=.2, color=colors[2], xintercept=1) #Nomes
    datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
    
    mult <- min(
        (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
        (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
        )
    
    datapc <- transform(datapc,
            v1 = .7 * mult * (get(x)),
            v2 = .7 * mult * (get(y))
            )
    
    plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color=colors[1])
    
    plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), size=1.1, arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color=colors[4])
    
    plot
}

pca.eg<-prcomp(df, scale=T) 

summary(pca.eg)

PCbiplot(pca.eg, df, colors=c("black", "black", "red", "blue"))













library(caret)
library(glmnet)

training.samples <- createDataPartition(df$Approved,p = 0.7, list = FALSE)
train.data  <- df[training.samples, ]
test.data <- df[-training.samples, ]











regLog <- glm(Approved ~ ., data=train.data)
summary(regLog)

probab_regLog <- predict(regLog,test.data, type = "response")
predicted.classes <- ifelse(probab_regLog > 0.5, "1", "0")


print("Taxa de acerto:")
mean(predicted.classes == test.data$Approved)

print("Matriz de confusão:")
table(test.data$Approved, predicted.classes>0.5)

#install.packages("ROCR")
library(ROCR)

rocplot <- function(pred , truth) 
  { predob <- prediction(pred , truth) 
  perf <- performance(predob , "tpr", "fpr")
  plot(perf)
  } 

rocplot(test.data$Approved, predicted.classes)












install.packages("class")
library(class)

perc.erro = seq(1,20)

for(i in 1:20){
    set.seed(1)
    previsoes = knn(train = train.data, test= test.data,cl= train.data$Approved,k=i)
    perc.erro[i] =mean(train.data$Approved != previsoes)
}

library(ggplot2)

k.values <- 1:20
error.df <- data.frame(perc.erro,k.values)


ggplot(error.df,aes(x=k.values,y=perc.erro)) + geom_point()+ geom_line(lty="dotted",color='red')

index_min = which(perc.erro==min(perc.erro))
  
paste("K para o menor erro:",error.df$k.values[index_min[1]], collapse = " ")


preds_knn = knn(train = train.data, test= test.data,cl= train.data$Approved,k=18)

accur_knn = mean(preds_knn == test.data$Approved)
paste("Acurácia do knn:",accur_knn, collapse = " ")

preds_knn.classes <- ifelse(preds_knn == 1, "1", "0")

table(test.data$Approved, preds_knn.classes > 0.5)


rocplot(test.data$Approved, preds_knn)












library(MASS)
library(klaR)
library(ROCR)
set.seed(101)

qda_stu <- qda(Approved ~ ., data=train.data)


preds_qda = predict(qda_stu,test.data)


accur_qda = mean(preds_qda$class == test.data$Approved)
paste("Acurácia da QDA:",accur_qda,collapse = " ")



preds_qda.classes <- ifelse(preds_qda$class == 1, "1", "0")

print("Matriz de confusão:")
table(test.data$Approved, preds_qda.classes > 0.5)


rocplot(test.data$Approved, preds_qda$class)












install.packages('e1071')
library(e1071)
 
classifier = svm(formula = Approved ~ .,
                 data = train.data,
                 type = 'C-classification',
                 kernel = 'linear')


preds_svm = predict(classifier, newdata=test.data )


accur_svm = mean(preds_svm == test.data$Approved)
paste("Acurácia do SVM:",accur_svm,collapse = " ")


print("Matriz de confusão")
preds_svm.classes <- ifelse(preds_svm == 1, "1", "0")

table(test.data$Approved, preds_svm.classes > 0.5)


rocplot(test.data$Approved, preds_svm)
