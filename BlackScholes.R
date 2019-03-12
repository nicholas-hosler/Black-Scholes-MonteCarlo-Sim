# Quick black scholes computation in R 

# Assumptions 
# exec is paid call options as a poriton of his bonus 
# at exercise price(x) = 105
# current stock price(S) is $100
# volatility or sigma is .2 
# rf is .03
# the option will expire in 1 year T = 1 

s <- 100
x <- 105
sigma <- 0.2 
rf <- 0.03
t <- 1 


d1 <- ((log(s/x)+rf*t)/(sigma*sqrt(t))+(sigma*sqrt(t))/2)

d2 <- d1 - (sigma*sqrt(t))


c <- s*pnorm(d1) - exp(-rf*t)*x*pnorm(d2)

list$optionPrice[i] <- c 

cat("The value of the $",x, " call option is: ", c)


############ looping it in the case that you want to price many different price points ##########

# create a table of various strike prices 
strike <- 95:150

list <- data.frame(strike)

#create a space to put the strikes
list$optionPrice <- ""



# loop to fill in the data 
for (i in 1:dim(list)[1]) {
  
  x <- list$strike[i]
  
  d1 <- ((log(s/x)+rf*t)/(sigma*sqrt(t))+(sigma*sqrt(t))/2)
  
  d2 <- d1 - (sigma*sqrt(t))
  
  
  c <- s*pnorm(d1) - exp(-rf*t)*x*pnorm(d2)
  
  list$optionPrice[i] <- c 
  
  cat("The value of the $",x, " call option is: ", c)
  
  
}





