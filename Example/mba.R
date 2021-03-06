#install.packages("lawstat")
library(car)

#DATA
mba <- read.csv('mba.csv')

#PARAMS
param.1 <- rbinom(p=0.5,size=1,n=1)
param.2 <- c(1,3,5,7)


#SCRIPT
#Q1-i
q1_i = "a"; q1_i
q1_i_options <- 4

#Q1-ii
q1_ii = "a"; q1_ii
q1_ii_options <- 4

#Q1-iii
## Levene's test
q1_iii = ifelse( leveneTest(y=mba$salary, group=mba$gender, center="mean")$Pr[1] < 0.005, "a","b")
q1_iii
q1_iii_options <- 2

#Q1-iv
q1_iv = ifelse(q1_iii=="a","c","b")
q1_iv
q1_iv_options <- 4

#Q1-v
# t-test
q1_v = ifelse(q1_iv == "c",
         t.test(mba$salary[ mba$gender == "male" ], mba$salary[ mba$gender == "female" ])$statistic,
         t.test(mba$salary[ mba$gender == "male" ], mba$salary[ mba$gender == "female" ], var.equal=TRUE)$statistic)
q1_v


#Q1-vi
q1_vi = ifelse(q1_iv == "c",
          ifelse( 
           t.test(mba$salary[ mba$gender == "male" ], mba$salary[ mba$gender == "female" ])$p.value < 0.1
           , "b","c"),
          ifelse( 
           t.test(mba$salary[ mba$gender == "male" ], mba$salary[ mba$gender == "female" ], var.equal=TRUE)$p.value < 0.1
           , "b","c"))
q1_vi
q1_vi_options <- 4

#Q2-i
q2_i = param.1 + param.2; q2_i

#Q2-ii
q2_ii = param.1 * param.2; q2_ii

#Q2-iii
q2_iii = 'This is a text answer.'

#END