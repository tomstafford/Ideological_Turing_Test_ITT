library(rethinking)
source("R/fct_analysis.R")

ITTdata <- load_data("vaccine")
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

# mean   sd  5.5% 94.5% n_eff Rhat
# a         0.21 0.19 -0.09  0.51  2271    1
# bArg_Pos -0.36 0.25 -0.76  0.04  2642    1
# sigma_g   1.45 0.10  1.30  1.61  3073    1
# sigma_a   0.49 0.17  0.17  0.73   450    1
# sigma_i   1.78 0.09  1.64  1.93  2043    1

# same for the relative criteria: 

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

precis(model1_relativeITT)
# mean   sd  5.5% 94.5% n_eff Rhat
# a         0.21 0.19 -0.10  0.51  2544    1
# bArg_Pos -0.35 0.25 -0.76  0.05  2219    1
# sigma_g   1.45 0.10  1.31  1.62  2679    1
# sigma_a   0.50 0.16  0.21  0.73   474    1
# sigma_i   1.78 0.09  1.65  1.93  1745    1


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

#           mean   sd  5.5% 94.5% n_eff Rhat
# a       -0.01 0.16 -0.27  0.25  2738 1.00
# bdis     0.09 0.19 -0.21  0.39  3579 1.00
# sigma_g  1.46 0.09  1.31  1.61  2746 1.00
# sigma_a  0.51 0.15  0.25  0.74   538 1.01
# sigma_i  1.78 0.09  1.65  1.92  2246 1.00

# same with relative criteria: 

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
# a        0.21 0.17 -0.07  0.49  2478 1.00
# b_read  -0.33 0.18 -0.62 -0.03  2825 1.00
# sigma_g  1.46 0.10  1.31  1.62  2459 1.00
# sigma_a  0.49 0.16  0.21  0.72   394 1.02
# sigma_i  1.77 0.09  1.64  1.92  1840 1.00
 
