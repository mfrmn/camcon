data(growth)

#DATA
growth.dat <- growth # [CS,0.2]

#PARAMS

#SCRIPT

#Q1-i [10,NUM]
q1_i <- mean(growth.dat$empgrow)

#Q1-ii [15,SC,2]
q1_ii <- ifelse(sd(growth.dat$GDPgrow)>0,"A","B")

#Q1-iii [20,NUM]
q1_iii <- cor(growth.dat$empgrow, growth.dat$GDPgrow)

#Q2-i [30,NUM]
lm1 <- lm(GDPgrow ~ empgrow, growth.dat)
q2_i <- coef(lm1)[2]

#Q2-ii [20,NUM]
q2_ii <- summary(lm1)$r.squared

#Q2-iii [3]
q2_iii <- "Hi"
# This is the solution

#END