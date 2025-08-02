x <- seq(0, 200, by = 1)

y <- dnorm(x, 100, 20)

plot(x, y, type = "l", 
     xlab="[conc]", 
     ylab = "densità di probabilità", 
     col="red")

# QUALE VALORE INCLUDE IL 99% DELLA DISTRIBUZIONE?

q99 <- qnorm(0.99, 100, 20)
abline(v = q99, col = "purple") 

#################################################

x <- seq(0, 80, by = 1)
plot(dlnorm(x, meanlog=0.2, sdlog=5), type = "l",
     xlab="[conc]", 
     ylab = "densità di probabilità", 
     col="red")

# QUALE VALORE INCLUDE IL 99% DELLA DISTRIBUZIONE?

ql99<-qlnorm(0.99, meanlog=0.2, sdlog=1.5)

ql99

abline(v = ql99, col = "purple") 

##################################################

set.seed(123)  # set seed for reproducibility
n <- 1000      # n. osservazioni
meanlog <- 0   # media oraria nota
sdlog <- 0.5   # devst ricavata da (CV * media oraria nota)

# generate log-normal data
log_normal_data <- rlnorm(n, meanlog = meanlog, sdlog = sdlog)

# histogram
hist(log_normal_data,
     breaks = 30,
     probability = TRUE,
     main = "Log-Normal Distribution",
     xlab = "Value",
     col = "skyblue",
     border = "white")

# density curve
lines(density(log_normal_data), col = "red", lwd = 2)
# pct99
ql99<-qlnorm(0.99, meanlog=meanlog, sdlog=sdlog)
# lines
abline(v = ql99, col = "purple", lty="dashed") 
# text
text(ql99+0.3, 0.4, "Pct99", col = "purple")

###############

curve(dweibull(x, shape = 1.5, scale = 2), 
      from=0, to=10, 
      col='green')

qw99 <- qweibull(0.99, shape=1.5, scale = 2) #<<

abline(v = qw99, col = "purple", lty="dashed") 
text(qw99+1, 0.2, "Pct99", col = "purple")


####

set.seed(123)  
n <- 1000      
shape <- 1.5 #<<
scale <- 2   #<<

# generate weibull data
weibull_data <- rweibull(n, shape = shape, scale = scale)

# histogram
hist(weibull_data,
     breaks = 50,
     probability = TRUE,
     main = NULL,
     xlab = NULL, ylab = NULL,
     col = "skyblue")

# density curve
lines(density(weibull_data), col = "red", lwd =1.5)
# pct99
qw99 <- qweibull(0.99,      #<<
                 shape=1.5, #<< 
                 scale = 2) #<<
# lines
abline(v = qw99, col = "purple", lty="dashed", lwd =1.5) 
# text
text(qw99+1, 0.2, "Pct99", col = "purple")







####

shape <- 2   #<<
scale <- 15  #<<
q <- 0.99     #<<
from <- 0
to <- 200

curve(dgamma(x, shape = shape, scale = scale),
      from = from, to = to, 
      col='blue')

qg99 <- qgamma(q, shape = shape, scale = scale) #<<

abline(v = qg99, col = "purple", lty="dashed") 
text(qg99+15, 0.01, "Pct99", col = "purple")
