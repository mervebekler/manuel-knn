library(tidyverse)
library(RNHANES)
library(class)

DEMO_F = nhanes_load_data("DEMO_F", "2009-2010") %>%
  select(SEQN, RIDAGEYR)
BMX_F = nhanes_load_data("BMX_F", "2009-2010") %>% 
  select(SEQN, BMXBMI, BMXWT)
HDL_F =  nhanes_load_data("HDL_F", "2009-2010") %>% 
  select(SEQN, LBDHDD)
GLU_F = nhanes_load_data("GLU_F", "2009-2010") %>% 
  select(SEQN, LBXGLU, LBXIN)
DIQ_F = nhanes_load_data("DIQ_F", "2009-2010") %>% 
  select(SEQN, DIQ010)


dtx = left_join(DEMO_F, HDL_F) %>% 
  left_join(GLU_F) %>% 
  left_join(BMX_F) %>% 
  left_join(DIQ_F)

dat = dtx %>% 
  filter(!is.na(BMXBMI), !is.na(LBDHDD), !is.na(LBXGLU), !is.na(LBXIN),RIDAGEYR >= 40, DIQ010 %in% c(1, 2)) %>% 
  transmute(SEQN, Age = RIDAGEYR, BMI = BMXBMI, Cholest = LBDHDD, Glucose = LBXGLU, Insuline = LBXIN, Weight = BMXWT, Diabetes = DIQ010) %>% 
  mutate(Diabetes = recode_factor(Diabetes, 
                                  `1` = "Yes", 
                                  `2` = "No"))
ggplot(dat, aes(Glucose, Weight, color = Diabetes)) +
  geom_point(alpha = 0.7, size = 2) 


normalize <- function (i) {
  (i - min(i))/(max(i) - min(i))
}

norm_dat <- dat %>% 
  select(Age, BMI, Cholest, Glucose, Insuline, Weight) %>% 
  lapply(., normalize) %>% 
  as.data.frame()


dat_samp <- sample(2, nrow(dat), replace=TRUE, prob=c(0.67, 0.33))
dat_training <- norm_dat[dat_samp==1, 1:6]
dat_test <- norm_dat[dat_samp==2, 1:6]

dat_target_group <- dat[dat_samp==1, 8]
dat_test_group <- dat[dat_samp==2, 8]

dat_pred <- knn(train = dat_training, test = dat_test, cl = dat_target_group, k=3)
dat_pred


summary(dat_pred)
summary(dat_test_group)

tab <- table(dat_pred, dat_test_group)
tab


accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
