library(rethinking)
source("R/fct_analysis.R")

ITTdata <- load_data("brexit")
ITTdata <- lickert_to_numeric(ITTdata, "stan")
ITTdata <- dichot_read_discuss(ITTdata)

# Analysis script for brexit topic 

# The below analyses are included in the preregistration of the paper: https://osf.io/uz9ge

# Due to an 8 year old laptop, R version 3.6.0 (2019-04-26) was used, meaning Rethinking version 1.95 was used. Apologies this means outdated/deprecated functions such as map2stan are in use in my models. However all model objects are saved in the repo so you can play with those instead of re-running if you like.   

##### MODEL 1: Is passing ITT absolute criteria predicted by arguer position after accounting for rater id, arguer id, argument id? #####

model1_passedITT <- map2stan(
  alist(
    passedITT ~ dbinom(1, p),
    logit(p) <- a + bArg_Pos*Arguer_Position_numeric + rat_id[Rater_ID]*sigma_g + arg_id[ARGUER_ID]*sigma_a + args_ind[argument_index]*sigma_i,
    a ~ dnorm(0,4),
    bArg_Pos ~ dnorm(0,1),
    rat_id[Rater_ID] ~ dnorm(0,1),
    arg_id[ARGUER_ID] ~ dnorm(0,1),
    args_ind[argument_index] ~ dnorm(0,1),
    sigma_g ~ dcauchy(0,1),
    sigma_a ~ dcauchy(0,1),
    sigma_i ~ dcauchy(0,1)
  ),
  data=ITTdata,constraints=list(sigma_g="lower=0", sigma_a="lower=0", sigma_i="lower=0"),
  warmup=1000, iter=4000, chains=3, cores=3)

precis(model1_passedITT)

#           mean   sd  5.5% 94.5% n_eff Rhat
# a        -0.56 0.20 -0.89 -0.24  1841    1
# bArg_Pos  0.11 0.27 -0.33  0.55  2048    1
# sigma_g   1.79 0.12  1.61  1.99  2646    1
# sigma_a   0.32 0.13  0.08  0.51   575    1
# sigma_i   1.26 0.07  1.16  1.37  2625    1

# no difference between arguer positions

# Same but for relative criteria

model1_relativeITT <- map2stan(
  alist(
    passed_relative_ITT ~ dbinom(1, p),
    logit(p) <- a + bArg_Pos*Arguer_Position_numeric + rat_id[Rater_ID]*sigma_g + arg_id[ARGUER_ID]*sigma_a + args_ind[argument_index]*sigma_i,
    a ~ dnorm(0,4),
    bArg_Pos ~ dnorm(0,1),
    rat_id[Rater_ID] ~ dnorm(0,1),
    arg_id[ARGUER_ID] ~ dnorm(0,1),
    args_ind[argument_index] ~ dnorm(0,1),
    sigma_g ~ dcauchy(0,1),
    sigma_a ~ dcauchy(0,1),
    sigma_i ~ dcauchy(0,1)
  ),
  data=ITTdata,constraints=list(sigma_g="lower=0", sigma_a="lower=0", sigma_i="lower=0"),
  warmup=1000, iter=4000, chains=3, cores=3)
 
# precis(model1_relativeITT)

#           mean   sd  5.5% 94.5% n_eff Rhat
# a         0.99 0.17  0.73  1.26  1665    1
# bArg_Pos -0.14 0.23 -0.51  0.21  1635    1
# sigma_g   1.30 0.08  1.17  1.44  2514    1
# sigma_a   0.40 0.13  0.16  0.60   440    1
# sigma_i   1.41 0.07  1.29  1.52  2171    1

# no difference between arguer positions again 

##### MODEL 2: Is passing predicted by how often they discuss the topic with those who disagree with them?  #####


model2_passedITT <- map2stan(
  alist(
    passedITT ~ dbinom(1, p),
    logit(p) <- a + bdis*discuss + rat_id[Rater_ID]*sigma_g + arg_id[ARGUER_ID]*sigma_a + args_ind[argument_index]*sigma_i,
    a ~ dnorm(0,4),
    bdis ~ dnorm(0,1),
    rat_id[Rater_ID] ~ dnorm(0,1),
    arg_id[ARGUER_ID] ~ dnorm(0,1),
    args_ind[argument_index] ~ dnorm(0,1),
    sigma_g ~ dcauchy(0,1),
    sigma_a ~ dcauchy(0,1),
    sigma_i ~ dcauchy(0,1)
  ),
  data=ITTdata,constraints=list(sigma_g="lower=0", sigma_a="lower=0", sigma_i="lower=0"),
  warmup=1000, iter=4000, chains=3, cores=3)

precis(model2_passedITT)
# mean   sd  5.5% 94.5% n_eff Rhat
# a       -0.49 0.15 -0.74 -0.25  1176 1.00
# bdis    -0.31 0.27 -0.75  0.13  3935 1.00
# sigma_g  1.78 0.12  1.60  1.98  1662 1.00
# sigma_a  0.34 0.13  0.11  0.53   510 1.01
# sigma_i  1.26 0.07  1.15  1.37  2140 1.00

# no effect of discussion time

# same but for relative criteria 

model2_relativeITT <- map2stan(
  alist(
    passed_relative_ITT ~ dbinom(1, p),
    logit(p) <- a + bdis*discuss + rat_id[Rater_ID]*sigma_g + arg_id[ARGUER_ID]*sigma_a + args_ind[argument_index]*sigma_i,
    a ~ dnorm(0,4),
    bdis ~ dnorm(0,1),
    rat_id[Rater_ID] ~ dnorm(0,1),
    arg_id[ARGUER_ID] ~ dnorm(0,1),
    args_ind[argument_index] ~ dnorm(0,1),
    sigma_g ~ dcauchy(0,1),
    sigma_a ~ dcauchy(0,1),
    sigma_i ~ dcauchy(0,1)
  ),
  data=ITTdata,constraints=list(sigma_g="lower=0", sigma_a="lower=0", sigma_i="lower=0"),
  warmup=1000, iter=4000, chains=3, cores=3)

precis(model2_relativeITT)
# mean   sd  5.5% 94.5% n_eff Rhat
# a        0.93 0.13  0.73  1.14  1371 1.01
# bdis    -0.19 0.29 -0.66  0.27  2655 1.00
# sigma_g  1.30 0.09  1.17  1.44  2008 1.00
# sigma_a  0.39 0.14  0.13  0.59   363 1.01
# sigma_i  1.41 0.07  1.29  1.53  1648 1.00


##### MODEL 3: Does how often they research the topic predict passing ITT? #####

model3_passedITT <- map2stan(
  alist(
    passedITT ~ dbinom(1, p),
    logit(p) <- a + b_read*read + rat_id[Rater_ID]*sigma_g + arg_id[ARGUER_ID]*sigma_a + args_ind[argument_index]*sigma_i,
    a ~ dnorm(0,4),
    b_read ~ dnorm(0,1),
    rat_id[Rater_ID] ~ dnorm(0,1),
    arg_id[ARGUER_ID] ~ dnorm(0,1),
    args_ind[argument_index] ~ dnorm(0,1),
    sigma_g ~ dcauchy(0,1),
    sigma_a ~ dcauchy(0,1),
    sigma_i ~ dcauchy(0,1)
  ),
  data=ITTdata,constraints=list(sigma_g="lower=0", sigma_a="lower=0", sigma_i="lower=0"),
  warmup=1000, iter=4000, chains=3, cores=3)

precis(model3_passedITT)
# mean   sd  5.5% 94.5% n_eff Rhat
# a       -0.44 0.15 -0.68 -0.21   860 1.01
# b_read  -0.25 0.16 -0.50  0.01  2633 1.00
# sigma_g  1.79 0.12  1.61  1.99  1613 1.00
# sigma_a  0.30 0.14  0.06  0.50   416 1.00
# sigma_i  1.26 0.07  1.16  1.38  1908 1.00

# no effect of reading (although again almost negative)

# same for relative criteria 

model3_relativeITT <- map2stan(
  alist(
    passed_relative_ITT ~ dbinom(1, p),
    logit(p) <- a + b_read*read + rat_id[Rater_ID]*sigma_g + arg_id[ARGUER_ID]*sigma_a + args_ind[argument_index]*sigma_i,
    a ~ dnorm(0,4),
    b_read ~ dnorm(0,1),
    rat_id[Rater_ID] ~ dnorm(0,1),
    arg_id[ARGUER_ID] ~ dnorm(0,1),
    args_ind[argument_index] ~ dnorm(0,1),
    sigma_g ~ dcauchy(0,1),
    sigma_a ~ dcauchy(0,1),
    sigma_i ~ dcauchy(0,1)
  ),
  data=ITTdata,constraints=list(sigma_g="lower=0", sigma_a="lower=0", sigma_i="lower=0"),
  warmup=1000, iter=4000, chains=3, cores=3)

precis(model3_relativeITT)
 
#         mean   sd  5.5% 94.5% n_eff Rhat
# a        1.69 0.62  0.70  2.69  5232    1
# b_read  -0.40 0.63 -1.41  0.61  5507    1
# sigma_g  0.69 0.21  0.38  1.05  3013    1
# sigma_a  0.93 0.47  0.17  1.68  1122    1
# sigma_i  1.78 0.33  1.30  2.34  2043    1

##### STANLEY MODELS: Are stanley measures of open mindedness predicted by passing the ITT? #####

ITTdata <- mutate(ITTdata, stanley_ratings = reasoning_stan)

model_stanley1 <- map2stan(
  alist(
    stanley_ratings ~ dordlogit(phi, cutpoints),
    phi <-  bITT*passedITT +
      arg_id[ARGUER_ID]*sigma_a,
    bITT ~ dnorm(0,1),
    arg_id[ARGUER_ID] ~ dnorm(0,1),
    sigma_a ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=ITTdata,
  constraints = list(sigma_a = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(model_stanley1)
# mean   sd 5.5% 94.5% n_eff Rhat
# bITT    0.08 0.05 0.01  0.16  3164    1
# sigma_a 0.88 0.04 0.81  0.94   522    1


###### MORALITY MODELS: Are your ratings of opponents' morality higher/lower if you pass the ITT? ##### 

morality_frame <- wide_to_long(ITTdata, "morality", "moral_Q", "morality_ratings")

morality1 <- map2stan(
   alist(
     morality_ratings ~ dordlogit(phi, cutpoints),
     phi <-  bITT*passedITT +
       arg_id[ARGUER_ID]*sigma_a + mor_id[moral_Q]*sigma_q,
     bITT ~ dnorm(0,1),
     arg_id[ARGUER_ID] ~ dnorm(0,1),
     mor_id[moral_Q] ~ dnorm(0,1),
     sigma_a ~ normal(0,0.1),
     sigma_q ~ normal(0,0.1),
     cutpoints ~ dnorm(0,10)
   ),
   data=morality_frame, 
   constraints = list(sigma_a = "lower=0", sigma_q = "lower=0"),
   start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
   control=list(adapt_delta=0.99, max_treedepth=13),
   chains = 3, cores = 3, iter=1200)
 
precis(morality1)
# mean   sd  5.5% 94.5% n_eff Rhat
# bITT    -0.16 0.03 -0.20 -0.11  1901 1.00
# sigma_a  1.15 0.05  1.08  1.22   131 1.02
# sigma_q  0.11 0.05  0.05  0.19   888 1.00

##### INTELLECT MODELS: Are your ratings of opponents' intellect higher/lower if you pass the ITT? #####

intellect_frame <- wide_to_long(ITTdata, "intellect", "intellect_Q", "intellect_ratings")

intellect1 <- map2stan(
  alist(
    intellect_ratings ~ dordlogit(phi, cutpoints),
    phi <-  bITT*passedITT +
       int_id[intellect_Q]*sigma_q,
    bITT ~ dnorm(0,1),
    int_id[intellect_Q] ~ dnorm(0,1),
    sigma_q ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=intellect_frame,
  constraints = list(sigma_q = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(intellect1)
# mean      sd  5.5% 94.5% n_eff Rhat
# bITT    -0.2 0.03 -0.24 -0.16  1805    1
# sigma_q  0.2 0.05  0.14  0.29  1117    1

intellect1.1 <- map2stan(
  alist(
    intellect_ratings ~ dordlogit(phi, cutpoints),
    phi <-  bITT*passedITT + arg_id[ARGUER_ID]*sigma_a +
      int_id[intellect_Q]*sigma_q,
    arg_id[ARGUER_ID] ~ dnorm(0,1),
    bITT ~ dnorm(0,1),
    int_id[intellect_Q] ~ dnorm(0,1),
    sigma_a ~ normal(0,0.1),
    sigma_q ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=intellect_frame,
  constraints = list(sigma_q = "lower=0", sigma_a = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(intellect1.1)
# mean            sd  5.5% 94.5% n_eff Rhat4
# bITT    -0.11 0.03 -0.16 -0.07  1458  1.00
# sigma_a  1.02 0.04  0.95  1.09   109  1.01
# sigma_q  0.22 0.05  0.15  0.31  1030  1.00
