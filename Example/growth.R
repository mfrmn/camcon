data(growth)

#DATA
growth <- growth; growth.opts <- c('CS')

#PARAMS

#SCRIPT

#Q1-i
q1_i <- mean(growth$empgrow)

#Q1-ii
q1_ii <- sd(growth$GDPgrow)

#Q1-iii
q1_iii <- cor(growth$empgrow, growth$GDPgrow)

#Q2-i
lm1 <- lm(GDPgrow ~ empgrow, growth)
q2_i <- coef(lm1)[2]

#Q2-ii
q2_ii <- summary(lm1)$r.squared
# This is the solution

#END