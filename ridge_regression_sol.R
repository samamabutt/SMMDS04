train = as.matrix(read.table("E:/MS DS/SMMDS/assignment 4/train2_5.txt", quote="\"", comment.char=""))
train_labels = as.matrix(read.table("E:/MS DS/SMMDS/assignment 4/train2_5Labels.txt", quote="\"", comment.char=""))
test = as.matrix(read.table("E:/MS DS/SMMDS/assignment 4/test2_5.txt", quote="\"", comment.char=""))
test_labels = as.matrix(read.table("E:/MS DS/SMMDS/assignment 4/test2_5Labels.txt", quote="\"", comment.char=""))

train_matrix<-cbind(c(1),train)
test_matrix<-cbind(c(1),test) 

#train$newcol <- rep(1,nrow(train))
#test_matrix

r = train[151,]       #first digit image, i.e., image in row 1 
im = matrix(r,nrow=16,byrow=TRUE)   #convert vector to image 
image(im[,ncol(im):1])#view image
   

regressionCoefficients <- function(X,Y,lambda) {
  #X <- cbind(X, c(10, 11, 12))  
  
  
  negClass = Y==5			#this will give you all indices of class = 1
  posClass = Y==2			#this will give you all indices of class = 0
  Y[negClass,] = -1
  Y[posClass,] = 1
  
  #(XT*X + lambda*I)-1 XT*Y
  a = t(X) %*% X
  b = lambda * diag(257)
  c = a+b
  d = t(X) %*% Y
  e = solve(c) %*% d
  
  cat("For lambda:",lambda,"\n")
  return (e)
  
  
}

predictions <- function(testX,regressionCoefficients){
  return (testX %*% regressionCoefficients) 
} 

Matrix <- function(prediction,actualLabels){
  
  fiveClass = prediction < 0			#this will give you all indices of class = 1
  twoClass = prediction > 0			#this will give you all indices of class = 0
  prediction[fiveClass,] = 5
  prediction[twoClass,] = 2
  
  TP = (prediction == 2 & actualLabels == 2)
  FN = (prediction == 5 & actualLabels == 2)
  TN = (prediction == 5 & actualLabels == 5)
  FP = (prediction == 2 & actualLabels == 5)
  
  count_tp = length(actualLabels[TP,])
  
  count_fn = length(actualLabels[FN,])
  
  count_tn = length(actualLabels[TN,])
  
  count_fp = length(actualLabels[FP,])
  
  cat("TP",count_tp,"\n","FN",count_fn,"\n","TN",count_tn,"\n","FP",count_fp,"\n")
  return(prediction)
}

Matrix(predictions(train_matrix,regressionCoefficients(train_matrix, train_labels,0.001)),train_labels)
Matrix(predictions(test_matrix,regressionCoefficients(train_matrix, train_labels,0.001)),test_labels)

Matrix(predictions(train_matrix,regressionCoefficients(train_matrix, train_labels,1)),train_labels)
Matrix(predictions(test_matrix,regressionCoefficients(train_matrix, train_labels,1)),test_labels)

Matrix(predictions(train_matrix,regressionCoefficients(train_matrix, train_labels,1000)),train_labels)
Matrix(predictions(test_matrix,regressionCoefficients(train_matrix, train_labels,1000)),test_labels)

Matrix(predictions(train_matrix,regressionCoefficients(train_matrix, train_labels,10)),train_labels)
Matrix(predictions(test_matrix,regressionCoefficients(train_matrix, train_labels,10)),test_labels)


# a = count_tp + count_tn
# b = count_tp + count_tn + count_fn + count_fp
# accuracy = a/b
# accuracy