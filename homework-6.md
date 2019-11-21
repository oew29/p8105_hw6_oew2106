Homework 6
================
Olivia Wagner
11/21/2019

``` r
birthweight_data = janitor::clean_names(read_csv('./birthweight.csv'))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
print(birthweight_data)
```

    ## # A tibble: 4,342 x 20
    ##    babysex bhead blength   bwt delwt fincome frace gaweeks malform menarche
    ##      <dbl> <dbl>   <dbl> <dbl> <dbl>   <dbl> <dbl>   <dbl>   <dbl>    <dbl>
    ##  1       2    34      51  3629   177      35     1    39.9       0       13
    ##  2       1    34      48  3062   156      65     2    25.9       0       14
    ##  3       2    36      50  3345   148      85     1    39.9       0       12
    ##  4       1    34      52  3062   157      55     1    40         0       14
    ##  5       2    34      52  3374   156       5     1    41.6       0       13
    ##  6       1    33      52  3374   129      55     1    40.7       0       12
    ##  7       2    33      46  2523   126      96     2    40.3       0       14
    ##  8       2    33      49  2778   140       5     1    37.4       0       12
    ##  9       1    36      52  3515   146      85     1    40.3       0       11
    ## 10       1    33      50  3459   169      75     2    40.7       0       12
    ## # … with 4,332 more rows, and 10 more variables: mheight <dbl>,
    ## #   momage <dbl>, mrace <dbl>, parity <dbl>, pnumlbw <dbl>, pnumsga <dbl>,
    ## #   ppbmi <dbl>, ppwt <dbl>, smoken <dbl>, wtgain <dbl>

``` r
# Data Clean #

missing  = apply(birthweight_data, 2, function(x) any(is.infinite(x) | is.na(x)))
print(missing)
```

    ##  babysex    bhead  blength      bwt    delwt  fincome    frace  gaweeks 
    ##    FALSE    FALSE    FALSE    FALSE    FALSE    FALSE    FALSE    FALSE 
    ##  malform menarche  mheight   momage    mrace   parity  pnumlbw  pnumsga 
    ##    FALSE    FALSE    FALSE    FALSE    FALSE    FALSE    FALSE    FALSE 
    ##    ppbmi     ppwt   smoken   wtgain 
    ##    FALSE    FALSE    FALSE    FALSE

``` r
birthweight_data = birthweight_data %>%
  mutate(mrace = as.factor(mrace), malform = as.factor(malform), frace = as.factor(frace), babysex = as.factor(babysex)) 

# Hypothesized Model #

initial_model = lm(bwt ~ ., data = birthweight_data)
summary(initial_model)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ ., data = birthweight_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1097.68  -184.86    -3.33   173.09  2344.15 
    ## 
    ## Coefficients: (3 not defined because of singularities)
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -6265.3914   660.4011  -9.487  < 2e-16 ***
    ## babysex2       28.7073     8.4652   3.391 0.000702 ***
    ## bhead         130.7781     3.4523  37.881  < 2e-16 ***
    ## blength        74.9536     2.0217  37.075  < 2e-16 ***
    ## delwt           4.1007     0.3948  10.386  < 2e-16 ***
    ## fincome         0.2898     0.1795   1.614 0.106551    
    ## frace2         14.3313    46.1501   0.311 0.756168    
    ## frace3         21.2361    69.2960   0.306 0.759273    
    ## frace4        -46.9962    44.6782  -1.052 0.292912    
    ## frace8          4.2969    74.0741   0.058 0.953745    
    ## gaweeks        11.5494     1.4654   7.882 4.06e-15 ***
    ## malform1        9.7650    70.6259   0.138 0.890039    
    ## menarche       -3.5508     2.8951  -1.226 0.220083    
    ## mheight         9.7874    10.3116   0.949 0.342588    
    ## momage          0.7593     1.2221   0.621 0.534418    
    ## mrace2       -151.4354    46.0453  -3.289 0.001014 ** 
    ## mrace3        -91.3866    71.9190  -1.271 0.203908    
    ## mrace4        -56.4787    45.1369  -1.251 0.210901    
    ## parity         95.5411    40.4793   2.360 0.018307 *  
    ## pnumlbw             NA         NA      NA       NA    
    ## pnumsga             NA         NA      NA       NA    
    ## ppbmi           4.3538    14.8913   0.292 0.770017    
    ## ppwt           -3.4716     2.6121  -1.329 0.183913    
    ## smoken         -4.8544     0.5871  -8.269  < 2e-16 ***
    ## wtgain              NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 272.5 on 4320 degrees of freedom
    ## Multiple R-squared:  0.7183, Adjusted R-squared:  0.717 
    ## F-statistic: 524.6 on 21 and 4320 DF,  p-value: < 2.2e-16

``` r
hyp_model = lm(bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + mrace + parity + ppwt + blength*bhead + delwt*ppwt, data = birthweight_data)
summary(hyp_model)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ babysex + bhead + blength + delwt + fincome + 
    ##     gaweeks + mrace + parity + ppwt + blength * bhead + delwt * 
    ##     ppwt, data = birthweight_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1077.70  -184.04    -4.75   180.22  2529.43 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -3.444e+03  7.887e+02  -4.367 1.29e-05 ***
    ## babysex2       3.548e+01  8.531e+00   4.159 3.26e-05 ***
    ## bhead          5.110e+01  2.406e+01   2.123 0.033771 *  
    ## blength        2.074e+01  1.651e+01   1.256 0.209110    
    ## delwt          5.460e+00  7.267e-01   7.513 6.96e-14 ***
    ## fincome        3.992e-01  1.755e-01   2.274 0.022990 *  
    ## gaweeks        1.171e+01  1.485e+00   7.884 3.99e-15 ***
    ## mrace2        -1.167e+02  9.643e+00 -12.106  < 2e-16 ***
    ## mrace3        -4.193e+01  4.268e+01  -0.982 0.326022    
    ## mrace4        -9.651e+01  1.901e+01  -5.076 4.01e-07 ***
    ## parity         9.757e+01  4.063e+01   2.402 0.016365 *  
    ## ppwt          -4.930e-01  8.966e-01  -0.550 0.582471    
    ## bhead:blength  1.693e+00  4.960e-01   3.414 0.000646 ***
    ## delwt:ppwt    -1.123e-02  4.701e-03  -2.389 0.016955 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 274.3 on 4328 degrees of freedom
    ## Multiple R-squared:  0.714,  Adjusted R-squared:  0.7131 
    ## F-statistic: 831.2 on 13 and 4328 DF,  p-value: < 2.2e-16

``` r
# residual plot #

hyp_plot = birthweight_data %>% 
  add_residuals(hyp_model) %>%
  add_predictions(hyp_model) %>%
  ggplot(aes(x = pred, y = resid)) +
  geom_point(aes(alpha = 0.30))

print(hyp_plot)
```

![](homework-6_files/figure-gfm/problem%201%20models-1.png)<!-- -->

``` r
# model 2 #

birth_age_model = lm(bwt ~ blength + gaweeks, data = birthweight_data)
summary(birth_age_model)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ blength + gaweeks, data = birthweight_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1709.6  -215.4   -11.4   208.2  4188.8 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -4347.667     97.958  -44.38   <2e-16 ***
    ## blength       128.556      1.990   64.60   <2e-16 ***
    ## gaweeks        27.047      1.718   15.74   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 333.2 on 4339 degrees of freedom
    ## Multiple R-squared:  0.5769, Adjusted R-squared:  0.5767 
    ## F-statistic:  2958 on 2 and 4339 DF,  p-value: < 2.2e-16

``` r
# model 3 #

length_gender_model = lm(bwt ~ bhead + babysex + blength + bhead*blength + bhead*babysex + babysex*blength + babysex*blength*bhead, data = birthweight_data)
summary(length_gender_model)
```

    ## 
    ## Call:
    ## lm(formula = bwt ~ bhead + babysex + blength + bhead * blength + 
    ##     bhead * babysex + babysex * blength + babysex * blength * 
    ##     bhead, data = birthweight_data)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1132.99  -190.42   -10.33   178.63  2617.96 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)            -7176.8170  1264.8397  -5.674 1.49e-08 ***
    ## bhead                    181.7956    38.0542   4.777 1.84e-06 ***
    ## babysex2                6374.8684  1677.7669   3.800 0.000147 ***
    ## blength                  102.1269    26.2118   3.896 9.92e-05 ***
    ## bhead:blength             -0.5536     0.7802  -0.710 0.478012    
    ## bhead:babysex2          -198.3932    51.0917  -3.883 0.000105 ***
    ## babysex2:blength        -123.7729    35.1185  -3.524 0.000429 ***
    ## bhead:babysex2:blength     3.8781     1.0566   3.670 0.000245 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 287.7 on 4334 degrees of freedom
    ## Multiple R-squared:  0.6849, Adjusted R-squared:  0.6844 
    ## F-statistic:  1346 on 7 and 4334 DF,  p-value: < 2.2e-16

To create my hypothesized model, I initially fit a saturated model then
pared down the number of variables to those that had a p-value of 0.20
or less, and created interactions for varaibles whose measurements
seemed to be correlated. In plotting the residuals vs. the predicted
values above we see most of the variation seems to be scattered fairly
evenly across resid = 0, with the exception of a few values in the left
tail of the data.

``` r
# cross validation #
cv_df = crossv_mc(birthweight_data, 100)
cv_df = cv_df %>% mutate(train = map(train, as_tibble, test = map(as_tibble)))

cv_df = cv_df %>% 
  mutate(hyp_mod = map(train, ~lm(bwt ~ babysex + bhead + blength + delwt + fincome + gaweeks + mrace + parity + ppwt + blength*bhead + delwt*ppwt, data = .x)), 
         age_mod = map(train, ~lm(bwt ~ blength + gaweeks, data = .x)), 
         length_mod = map(train, ~lm(bwt ~ bhead + babysex + blength + bhead*blength + bhead*babysex + babysex*blength + babysex*blength*bhead, data = .x)))%>%
  mutate(rmse_hyp = map2_dbl(hyp_mod, test, ~rmse(model = .x, data = .y)), 
         rmse_age = map2_dbl(age_mod, test, ~rmse(model = .x, data = .y)), 
         rmse_length = map2_dbl(length_mod, test, ~rmse(model = .x, data = .y)))
```

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit
    ## may be misleading
    
    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit
    ## may be misleading

``` r
# prediction error plot #
prediction_error = cv_df%>%
  select(starts_with('rmse')) %>%
  pivot_longer(everything(), names_to = 'model', values_to = 'rmse', names_prefix = 'rmse_') %>%
  mutate(model = fct_inorder(model)) %>%
  mutate(model = fct_inorder(model)) %>%
  ggplot(aes(x = model, y = rmse)) +
  geom_violin() %>% 
  print()
```

    ## geom_violin: draw_quantiles = NULL, na.rm = FALSE
    ## stat_ydensity: trim = TRUE, scale = area, na.rm = FALSE
    ## position_dodge

The best performing model was my hypothesized model. The amount of cross
validation prediction error is far lower than the other two models
(birth/gestational age and head/length measurement models). The adjusted
p-value is also greater then the other two models.

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'crul':
    ##   method                 from
    ##   as.character.form_file httr

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## file path:          /Users/oliviaewagner/Library/Caches/rnoaa/ghcnd/USW00094728.dly

    ## file last updated:  2019-11-21 12:56:53

    ## file min/max dates: 1869-01-01 / 2019-11-30

``` r
# bootstrap #

boot_straps = weather_df %>%
  modelr::bootstrap(n = 5000)

# work on log(beta0 * beta1) #
boot_strap_df = boot_straps %>% mutate(models = map(strap, ~lm(tmax ~ tmin, data = .x)), results = map(models, broom::tidy)) %>%
  select(-strap, -models) %>%
  unnest(results) %>%
  group_by(.id) %>%
  mutate(min_est = ifelse(term == 'tmin', estimate, 0), int_est = ifelse(term == '(Intercept)', estimate, 0)) 
  
log_param = boot_strap_df %>%
  group_by(.id) %>%
  summarize(min_est = sum(min_est), int_est = sum(int_est)) %>%
  mutate(log_min_max = log(min_est*int_est)) %>%
  print()
```

    ## # A tibble: 5,000 x 4
    ##    .id   min_est int_est log_min_max
    ##    <chr>   <dbl>   <dbl>       <dbl>
    ##  1 0001     1.02    7.48        2.03
    ##  2 0002     1.03    7.24        2.01
    ##  3 0003     1.04    7.14        2.00
    ##  4 0004     1.05    7.21        2.03
    ##  5 0005     1.05    7.16        2.02
    ##  6 0006     1.04    7.08        2.00
    ##  7 0007     1.06    6.89        1.99
    ##  8 0008     1.05    7.20        2.02
    ##  9 0009     1.04    7.22        2.02
    ## 10 0010     1.06    6.90        1.99
    ## # … with 4,990 more rows

``` r
# work on r^2 #

boot_strap_df_corr = boot_straps %>% mutate(models = map(strap, ~lm(tmax ~ tmin, data = .x)), results = map(models, broom::glance)) %>%
  select(-strap, -models) %>%
  unnest(results) %>% 
  select(.id, r.squared)

# plot distributions #

ggplot(data = boot_strap_df_corr, aes(x = r.squared)) +
  geom_histogram(aes(y = ..density..), color = 'black', fill = 'white')+
  geom_density(alpha = 0.25, fill = '#FF6666')+
  ggtitle('Distribution of R squared')
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](homework-6_files/figure-gfm/problem%202-1.png)<!-- -->

``` r
ggplot(data = log_param, aes(x = log_min_max)) +
  geom_histogram(aes(y = ..density..), color = 'black', fill = 'white')+
  geom_density(alpha = 0.25, fill = '#FF6666')+
  ggtitle('Distribution of log(beta_0 * beta_1)')
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](homework-6_files/figure-gfm/problem%202-2.png)<!-- -->

``` r
# Confidence Intervals #
    # R squared upper bound #
    quantile(pull(boot_strap_df_corr, r.squared), 0.975)
```

    ##     97.5% 
    ## 0.9271234

``` r
    # R squared lower bound #
    quantile(pull(boot_strap_df_corr, r.squared), 0.025)
```

    ##      2.5% 
    ## 0.8939398

``` r
    # log estimate upper bound #
    quantile(pull(log_param, log_min_max), 0.975)
```

    ##    97.5% 
    ## 2.058739

``` r
    #log estimate lower bound #
    quantile(pull(log_param, log_min_max), 0.025)
```

    ##     2.5% 
    ## 1.965828

The distribution of both of these estimates appear to be approximately
normal, with the distribution of r squared values slightly skewed to the
left.

R squared 95% CI : (0.8946, 0.9272)

log(\(\hat{\beta_{0}}*\hat{\beta_{1}}\)) 95% CI: (1.964, 2.058)
