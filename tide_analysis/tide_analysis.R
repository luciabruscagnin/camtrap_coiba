library(rethinking)
str(agoutisequence)
dens(agoutisequence$n)
#monkeys only
d <- agoutisequence[agoutisequence$capuchin==1,]
d <- d[d$dep_length_hours<5000,] # get rid of deployments shorter than 208 days
d$location_index <- as.integer(as.factor(d$location_name))
dens(d$tidedif)
dens(d$dep_length_hours)

hist(d$tidedif) 
dens(d$tidedif)
plot(d$tidedif, d$n , pch=19 , col=col.alpha("slateblue" , alpha = 0.05))
d$num_cap <- d$n
#checking distributions of hours
h.lub <- hour(d$seq_start)
hist(h.lub)

##intercept only model glm
dlist <- list(
  num_capuchins=d$n, 
  tidedif=d$tidedif,
  dep_length_hours=log(d$dep_length_hours),
  location_index=d$location_index,
  tool_site = d$tool_site
) 

curve( dlnorm( x , 1 , 0.7 ) , from=0 , to=100 , n=2000 )
m1 <- ulam(
  alist(
    num_capuchins ~ dpois( lambda ),
    log(lambda) <- a,
    a ~ dnorm( 1 , 0.7 )
  ), data=dlist , chains=4 ,cores=4 , log_lik=TRUE )

precis(m1)
exp(0.6)
mean(dlist$num_capuchins)
post <- extract.samples(m1)
dens(post$a , xlim=c(0,3))
dens(rnorm(10000, mean = 1, sd = 0.7) , col="red" , add=TRUE)

m2 <- ulam(
  alist(
    num_capuchins ~ dpois( lambda ),
    log(lambda) <- a + bt*tidedif,
    a ~ dnorm( 1 , 0.7 ),
    bt ~ dnorm( 0 , 2 )
    
  ), data=dlist , chains=4 ,cores=4 , log_lik=TRUE )

plot(precis(m2))


m3 <- ulam(
  alist(
    num_capuchins ~ dpois( lambda ),
    log(lambda) <- a + bt*tidedif + log(dep_length_hours),
    a ~ dnorm( 1 , 0.7 ),
    bt ~ dnorm( 0 , 2 )
    
  ), data=dlist , chains=4 ,cores=4 , log_lik=TRUE )

plot(d$num_cap~d$tidedif)
plot(precis(m3))
precis(m3)

##add location varying intercepts
m4 <- ulam(
  alist(
    num_capuchins ~ dpois( lambda ),
    log(lambda) <- a_location[location_index] + bt*tidedif + log(dep_length_hours),
    a_location[location_index] ~ dnorm( a , sigma ),
    a ~ dnorm( 1 , 0.7 ),
    bt ~ dnorm( 0 , 2 ),
    sigma ~ dexp( 1 )
    
  ), data=dlist , chains=4 ,cores=4 , log_lik=TRUE )

precis(m4 , depth=2)

m4 <- ulam(
  alist(
    num_capuchins ~ dpois( lambda ),
    log(lambda) <- a_location[location_index] + bt*tidedif + log(dep_length_hours),
    a_location[location_index] ~ dnorm( a , sigma ),
    a ~ dnorm( 1 , 0.7 ),
    bt ~ dnorm( 0 , 2 ),
    sigma ~ dexp( 1 )
    
  ), data=dlist , chains=4 ,cores=4 , log_lik=TRUE )

precis(m4 , depth=2)

agoutisequence$tool_site

m5 <- ulam(
  alist(
    num_capuchins ~ dpois( lambda ),
    log(lambda) <- A + BT*tidedif + log(dep_length_hours),
    A <- a + v[location_index,1],
    BT <- bt + v[location_index,2],
    # adaptive prior
    matrix[location_index,2]: v <- compose_noncentered( sigma , L_Rho , z ),
    matrix[2,location_index]: z ~ normal( 0 , 1 ),
    #fixed priors
    a ~ dnorm( 1 , 0.7 ),
    bt ~ dnorm( 0 , 2 ),
    vector[2]: sigma ~ exponential(1),
    cholesky_factor_corr[2]: L_Rho ~ lkj_corr_cholesky( 4 )
  ), data=dlist , chains=4 ,cores=4 , log_lik=TRUE )

precis(m5 , depth=3)
plot(precis(m5 , depth=3))
post <- extract.samples((m5))
str(post)

m6 <- ulam(
  alist(
    num_capuchins ~ dpois( lambda ),
    log(lambda) <- A + BT*tidedif + btool*tool_site + log(dep_length_hours),
    A <- a + v[location_index,1],
    BT <- bt + v[location_index,2],
    # adaptive prior
    matrix[location_index,2]: v <- compose_noncentered( sigma , L_Rho , z ),
    matrix[2,location_index]: z ~ normal( 0 , 1 ),
    #fixed priors
    a ~ dnorm( 1 , 0.7 ),
    bt ~ dnorm( 0 , 2 ),
    btool ~ dnorm(0, 1),
    vector[2]: sigma ~ exponential(1),
    cholesky_factor_corr[2]: L_Rho ~ lkj_corr_cholesky( 4 )
  ), data=dlist , chains=4 ,cores=4 , log_lik=TRUE )

precis(m6 , depth=3)
plot(precis(m6 , depth=3))

# interaction tool use and tide dif
m7 <- ulam(
  alist(
    num_capuchins ~ dpois( lambda ),
    log(lambda) <- A + BT*tidedif + (btXtool*tidedif + btool)*tool_site + log(dep_length_hours),
    A <- a + v[location_index,1],
    BT <- bt + v[location_index,2],
    # adaptive prior
    matrix[location_index,2]: v <- compose_noncentered( sigma , L_Rho , z ),
    matrix[2,location_index]: z ~ normal( 0 , 1 ),
    #fixed priors
    a ~ dnorm( 1 , 0.7 ),
    bt ~ dnorm( 0 , 2 ),
    c(btool,btXtool) ~ dnorm(0, 1),
    vector[2]: sigma ~ exponential(1),
    cholesky_factor_corr[2]: L_Rho ~ lkj_corr_cholesky( 4 )
  ), data=dlist , chains=4 ,cores=4 , log_lik=TRUE )

precis(m7 , depth=3)
plot(precis(m7 , depth=3))
