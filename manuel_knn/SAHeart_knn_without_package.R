
#convert data types

dat<- as.data.frame( SAheart)
dat$Outcome <- as.factor(dat$chd)

#normalize

normalize <- function (i) {
  (i - min(i))/(max(i) - min(i))
}

norm_dat <- dat %>% 
  select(-Outcome) %>% 
  lapply(., normalize) %>% 
  as.data.frame()


dat_samp <- sample(2, nrow(dat), replace=TRUE, prob=c(0.70, 0.30))
dat_training <- norm_dat[dat_samp==1, 1:9]
dat_test <- norm_dat[dat_samp==2, 1:9]

dat_target_group <- dat[dat_samp==1, 10]
dat_test_group <- dat[dat_samp==2, 10]

#modelling

my_knn <- function(dat_training,dat_test,dat_target_group,k     ){
  M <- nrow(dat_training)
  N <- nrow(dat_test)
  distmatrix <- matrix(0,nrow = M,ncol = N)
  for(i in 1:M){
    for(j in 1:N){
      distmatrix[i,j]<- sum((dat_training[i,]-dat_test[j,])^2) %>% sqrt()
    }
  }
  sortedDistIndexes <- apply(distmatrix,2,order)
  
  ans<- matrix(dat_target_group[sortedDistIndexes[1:k,1:nrow(dat_test)]],ncol = k, byrow = T)
  
  findmajority<-function(x) {
    names( which.max(table(x))     )
  } 
  finalanswer<- ans %>% apply( . ,1,findmajority   )
  finalanswer <- as.factor(finalanswer)  
  return(finalanswer)
}


dat_pred<- my_knn( dat_training, dat_test, dat_target_group, k=2)
summary(dat_pred)
summary(dat_test_group)

tab <- table(dat_pred, dat_test_group)
tab

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
