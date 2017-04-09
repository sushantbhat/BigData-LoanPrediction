library(lattice)

setwd("D:\\study\\IT sem 6\\Bda")
cust_inp = as.data.frame(read.csv("trainUnmodified.csv"))

#preprocessing

preMatrix = matrix(0,nrow = dim(cust_inp[1]),ncol = 13)

preMatrix[,7] = cust_inp[,7]
preMatrix[,8] = cust_inp[,8]
preMatrix[,9] = cust_inp[,9]
preMatrix[,10] = cust_inp[,10]
preMatrix[,11] = cust_inp[,11]
#preMatrix[,13] = cust_inp[,13]

#Loan_Status

for(i in 1:dim(cust_inp)[1]){
  if(cust_inp[i,13] == 'Y'){
    preMatrix[i,13] = 1
    
  }
  else if(cust_inp[i,13] == 'N'){
    preMatrix[i,13] = 0
    
  }
  else{
    preMatrix[i,13] = 0.5
  }
}



#Gender
for(i in 1:dim(cust_inp)[1]){
  if(cust_inp[i,2] == 'Male'){
    preMatrix[i,2] = 1
    
  }
  else if(cust_inp[i,2] == 'Female'){
    preMatrix[i,2] = 0
    
  }
  else{
    preMatrix[i,2] = 0.5
  }
}

#Married
for(i in 1:dim(cust_inp)[1]){
  if(cust_inp[i,3] == 'Yes'){
    preMatrix[i,3] = 1
    
  }
  else if(cust_inp[i,3] == 'No'){
    preMatrix[i,3] = 0
    
  }
  else{
    preMatrix[i,3] = 0.5
  }
}

#Education
for(i in 1:dim(cust_inp)[1]){
  if(cust_inp[i,5] == 'Graduate'){
    preMatrix[i,5] = 1
    
  }
  else if(cust_inp[i,5] == 'Not Graduate'){
    preMatrix[i,5] = 0
    
  }
  else{
    preMatrix[i,5] = 0.5
  }
}

#Self Employment
for(i in 1:dim(cust_inp)[1]){
  if(cust_inp[i,6] == 'Yes'){
    preMatrix[i,6] = 1
    
  }
  else if(cust_inp[i,6] == 'No'){
    preMatrix[i,6] = 0
    
  }
  else{
    preMatrix[i,6] = 0.5
  }
}

#Area
for(i in 1:dim(cust_inp)[1]){
  if(cust_inp[i,12] == 'Urban'){
    preMatrix[i,12] = 1
    
  }
  else if(cust_inp[i,12] == 'Semiurban'){
    preMatrix[i,12] = 2
    
  }
  else if(cust_inp[i,12] == 'Rural'){
    preMatrix[i,12] = 3
  }
  else{
    preMatrix[i,12] = 2
  }
}

#Credit History
for(i in 1:dim(cust_inp)[1]){
  if(is.na(cust_inp[i,11]) ==  TRUE){
    preMatrix[i,11] = 0.5
    
  }
  
}

#Loan Amount Term
for(i in 1:dim(cust_inp)[1]){
  if(is.na(cust_inp[i,10]) ==  TRUE){
    preMatrix[i,10] = 0
    
  }
  
}
mn = mean(preMatrix[,10])
for(i in 1:dim(cust_inp)[1]){
  if(preMatrix[i,10]==0){
    preMatrix[i,10] = mn
    
  }
  
}

#Loan Amount 
for(i in 1:dim(cust_inp)[1]){
  if(is.na(cust_inp[i,9]) ==  TRUE){
    preMatrix[i,9] = 0
    
  }
  
}
mn = mean(preMatrix[,9])
for(i in 1:dim(cust_inp)[1]){
  if(preMatrix[i,9]==0){
    preMatrix[i,9] = mn
    
  }
  
}




#Depender
for(i in 1:dim(cust_inp)[1]){
  if(cust_inp[i,4] == '0'){
    preMatrix[i,4] = 0
    
  }
  else if(cust_inp[i,4] == '1'){
    preMatrix[i,4] = 1
    
  }
  else if(cust_inp[i,4] == '2'){
    preMatrix[i,4] = 2
    
  }
  else if(cust_inp[i,4] == '3+'){
    preMatrix[i,4] = 3
    
  }
  else{
    preMatrix[i,4] = 1.5
  }
}

colnames(preMatrix)= c(colnames(cust_inp))

preMatrix=as.data.frame(preMatrix)
results2 <- glm(Loan_Status ~  Married  +   Credit_History , preMatrix,family=binomial(link="logit"))
summary(results2)

#ApplicantIncome=5720
#Dependents=0
#new_pt <- data.frame(ApplicantIncome,Dependents)
#conf_int_pt <- predict(results2,new_pt,level=.95,interval="confidence")
#conf_int_pt

#for(i in 1:dim(cust_inp)[1]){
 # Gender = preMatrix[i,2]
  #Married = preMatrix[i,3]
  #Dependents = preMatrix[i,4]
  #Education = preMatrix[i,5]
  #Self_Employed = preMatrix[i,6]
  #ApplicantIncome = preMatrix[i,7]
  #CoapplicantIncome = preMatrix[i,8]
  #Loan_Amount_Term = preMatrix[i,10]
  #Credit_History = preMatrix[i,11]
  #Property_Area = preMatrix[i,12]
  #new_pt <- data.frame(Gender,Married,Dependents,Education,Self_Employed,ApplicantIncome,CoapplicantIncome,Loan_Amount_Term,Credit_History,Property_Area)
  #conf_int_pt <- predict(results2,new_pt,level=.95,interval="confidence")
  #print(conf_int_pt)
  #result = t.test(conf_int_pt)
  #lower_conf = result$conf.int[1]
#  
 # upper_conf = result$conf.int[2]
  
#  if(preMatrix[i,9] >lower_conf && preMatrix[i,9] < upper_conf){
 #   preMatrix[i,13] = 'Y'
  #}
  #else preMatrix[i,13] = 'N'
#}

#write.csv(preMatrix, file = "output.csv",row.names=FALSE)
