library(rethinking)
source("R/fct_analysis.R")

ITTdata <- load_data("veganism")
ITTdata <- lickert_to_numeric(ITTdata, "stan")
ITTdata <- dichot_read_discuss(ITTdata)

# Analysis script for vegan topic 

# The below analyses are included in the preregistration of the paper: https://osf.io/uz9ge

# Due to an 8 year old laptop, R version 3.6.0 (2019-04-26) was used, meaning Rethinking version 1.95 was used. Apologies this means outdated/deprecated functions such as map2stan are in use in my models.  


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
# a         1.32 0.17  1.05  1.58  2294    1
# bArg_Pos -2.45 0.24 -2.84 -2.08  2184    1
# sigma_g   1.39 0.09  1.25  1.55  2143    1
# sigma_a   0.14 0.09  0.01  0.31   967    1
# sigma_i   1.36 0.07  1.26  1.48  3137    1

# shows what we thought, non-vegans passing more in the absolute (neg col means anti vegan (0) more likely pass ITT)

# Same as previous but for relative criteria 

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
# 
#           mean   sd  5.5% 94.5% n_eff Rhat
# a         1.41 0.16  1.15  1.67  2049    1
# bArg_Pos -0.01 0.22 -0.37  0.35  2033    1
# sigma_g   1.24 0.08  1.11  1.37  2546    1
# sigma_a   0.15 0.10  0.01  0.34   771    1
# sigma_i   1.49 0.08  1.37  1.61  2618    1

# no diff when looking at relative- as suspected


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

# precis(model2_passedITT)
#           mean   sd  5.5% 94.5% n_eff Rhat
# a        0.15 0.15 -0.10  0.39   852    1
# bdis    -0.43 0.20 -0.74 -0.12  1912    1
# sigma_g  1.78 0.12  1.59  1.98  1371    1
# sigma_a  0.14 0.10  0.01  0.32   773    1
# sigma_i  1.40 0.07  1.29  1.52  2608    1

# more likely to pass if you discuss less often for vegans, interesting

# Model 2.1 same for relative below 

# model2_relativeITT <- map2stan(
#   alist(
#     passed_relative_ITT ~ dbinom(1, p),
#     logit(p) <- a + bdis*discuss + rat_id[Rater_ID]*sigma_g + arg_id[ARGUER_ID]*sigma_a + args_ind[argument_index]*sigma_i,
#     a ~ dnorm(0,4),
#     bdis ~ dnorm(0,1),
#     rat_id[Rater_ID] ~ dnorm(0,1),
#     arg_id[ARGUER_ID] ~ dnorm(0,1),
#     args_ind[argument_index] ~ dnorm(0,1),
#     sigma_g ~ dcauchy(0,1),
#     sigma_a ~ dcauchy(0,1),
#     sigma_i ~ dcauchy(0,1)
#   ),
#   data=ITTdata,constraints=list(sigma_g="lower=0", sigma_a="lower=0", sigma_i="lower=0"),
#   warmup=1000, iter=4000, chains=3, cores=3)
# 
# precis(model2_relativeITT)
# # 
# # mean   sd  5.5% 94.5% n_eff Rhat
# # a        1.41 0.12  1.22  1.61  1678    1
# # bdis    -0.08 0.20 -0.39  0.25  2281    1
# # sigma_g  1.23 0.08  1.11  1.37  2363    1
# # sigma_a  0.16 0.11  0.02  0.35   633    1
# # sigma_i  1.49 0.07  1.37  1.61  2596    1
# # but not for relative - makes sense probs see same for research below

# 
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
# # mean   sd  5.5% 94.5% n_eff Rhat
# # a        0.32 0.16  0.07  0.57  1018    1
# # b_read  -0.61 0.16 -0.87 -0.36  2270    1
# # sigma_g  1.72 0.12  1.54  1.91  1494    1
# # sigma_a  0.11 0.09  0.01  0.27   912    1
# # sigma_i  1.40 0.07  1.29  1.52  3034    1

# same pattern as for discussing, shows in absolute but not relative below

# model3_relativeITT <- map2stan(
#   alist(
#     passed_relative_ITT ~ dbinom(1, p),
#     logit(p) <- a + b_read*read + rat_id[Rater_ID]*sigma_g + arg_id[ARGUER_ID]*sigma_a + args_ind[argument_index]*sigma_i,
#     a ~ dnorm(0,4),
#     b_read ~ dnorm(0,1),
#     rat_id[Rater_ID] ~ dnorm(0,1),
#     arg_id[ARGUER_ID] ~ dnorm(0,1),
#     args_ind[argument_index] ~ dnorm(0,1),
#     sigma_g ~ dcauchy(0,1),
#     sigma_a ~ dcauchy(0,1),
#     sigma_i ~ dcauchy(0,1)
#   ),
#   data=ITTdata,constraints=list(sigma_g="lower=0", sigma_a="lower=0", sigma_i="lower=0"),
#   warmup=1000, iter=4000, chains=3, cores=3)
# 
# precis(model3_relativeITT)
# 
# # mean   sd  5.5% 94.5% n_eff Rhat
# # a        1.46 0.13  1.25  1.68  2231    1
# # b_read  -0.16 0.16 -0.42  0.09  2923    1
# # sigma_g  1.23 0.08  1.10  1.36  2829    1
# # sigma_a  0.14 0.10  0.01  0.32   797    1
# # sigma_i  1.49 0.07  1.38  1.61  3053    1
# 

# 
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

# precis(model_stanley1)
# # 
# #         mean   sd 5.5% 94.5% n_eff Rhat
# # bITT    1.14 0.05 1.07  1.22  2918 1.00
# # sigma_a 0.89 0.04 0.82  0.96   432 1.01
# yes but again check for relative below 

# Doing same for relative passing 
model_stanley2 <- map2stan(
  alist(
    stanley_ratings ~ dordlogit(phi, cutpoints),
    phi <-  b_r_ITT*passed_relative_ITT +
      arg_id[ARGUER_ID]*sigma_a,
    b_r_ITT ~ dnorm(0,1),
    arg_id[ARGUER_ID] ~ dnorm(0,1),
    sigma_a ~ normal(0,0.1),
    cutpoints ~ dnorm(0,10)
  ),
  data=ITTdata,
  constraints = list(sigma_a = "lower=0"),
  start = list(cutpoints=c(-2,-1,0,1,2,2.5)),
  control=list(adapt_delta=0.99, max_treedepth=13),
  chains = 3, cores = 3, iter=1200)

precis(model_stanley2)

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

# # mean   sd  5.5% 94.5% n_eff Rhat
# # bITT    -1.12 0.03 -1.16 -1.07  2258 1.00
# # sigma_a  1.01 0.05  0.94  1.08   266 1.01
# # sigma_q  0.18 0.05  0.11  0.27  1359 1.00

# rerunning now with relative passing criteria instead

morality2 <- map2stan(
  alist(
    morality_ratings ~ dordlogit(phi, cutpoints),
    phi <-  b_r_ITT*passed_relative_ITT +
      arg_id[ARGUER_ID]*sigma_a + mor_id[moral_Q]*sigma_q,
    b_r_ITT ~ dnorm(0,1),
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

precis(morality2)
#mean   sd  5.5% 94.5% n_eff Rhat4
#b_r_ITT -0.05 0.03 -0.10  0.00  1564  1.00
#sigma_a  0.99 0.05  0.92  1.06   234  1.01
#sigma_q  0.17 0.05  0.11  0.26   884  1.01
saveRDS(morality2, "vegan_morality_relativeITT.RDS")
# 

##### INTELLECT MODELS: Are your ratings of opponents' intellect higher/lower if you pass the ITT? #####

intellect_frame <- wide_to_long(ITTdata, "intellect", "intellect_Q", "intellect_ratings")

intellect1 <- map2stan(
  alist(
    intellect_ratings ~ dordlogit(phi, cutpoints),
    phi <-  bITT*passedITT +
      arg_id[ARGUER_ID]*sigma_a + int_id[intellect_Q]*sigma_q,
    bITT ~ dnorm(0,1),
    arg_id[ARGUER_ID] ~ dnorm(0,1),
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

precis(intellect1)
# # mean   sd  5.5% 94.5% n_eff Rhat
# # bITT    -0.56 0.03 -0.60 -0.51  2380 1.00
# # sigma_a  1.20 0.05  1.12  1.27   265 1.01
# # sigma_q  0.29 0.05  0.22  0.37  1636 1.00

# doing this for relative passing too
intellect2 <- map2stan(
  alist(
    intellect_ratings ~ dordlogit(phi, cutpoints),
    phi <-  b_r_ITT*passed_relative_ITT +
      arg_id[ARGUER_ID]*sigma_a + int_id[intellect_Q]*sigma_q,
    b_r_ITT ~ dnorm(0,1),
    arg_id[ARGUER_ID] ~ dnorm(0,1),
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

precis(intellect2)

