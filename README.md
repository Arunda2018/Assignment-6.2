# Assignment-6.2
library(C50)
data(churn)

head(churnTrain)
head(churnTest)

#churnTrain = churnTrain[1:500,]
#churnTest = churnTest[1:500,]

# logistic regression model:


fit <- glm(churn~.,data = churnTrain,family = binomial(link='logit'))
summary(fit)

library(MASS)
step_fit <- stepAIC(fit,method='backward')
summary(step_fit)
confint(step_fit)

#ANOVA on base model
anova(fit,test = 'Chisq')

#ANOVA from reduced model after applying the Step AIC
anova(step_fit,test = 'Chisq')

#plot the fitted model
plot(fit$fitted.values)


library(C50)
> data(churn)
> head(churnTrain)
  state account_length     area_code international_plan voice_mail_plan number_vmail_messages
1    KS            128 area_code_415                 no             yes                    25
2    OH            107 area_code_415                 no             yes                    26
3    NJ            137 area_code_415                 no              no                     0
4    OH             84 area_code_408                yes              no                     0
5    OK             75 area_code_415                yes              no                     0
6    AL            118 area_code_510                yes              no                     0
  total_day_minutes total_day_calls total_day_charge total_eve_minutes total_eve_calls
1             265.1             110            45.07             197.4              99
2             161.6             123            27.47             195.5             103
3             243.4             114            41.38             121.2             110
4             299.4              71            50.90              61.9              88
5             166.7             113            28.34             148.3             122
6             223.4              98            37.98             220.6             101
  total_eve_charge total_night_minutes total_night_calls total_night_charge total_intl_minutes
1            16.78               244.7                91              11.01               10.0
2            16.62               254.4               103              11.45               13.7
3            10.30               162.6               104               7.32               12.2
4             5.26               196.9                89               8.86                6.6
5            12.61               186.9               121               8.41               10.1
6            18.75               203.9               118               9.18                6.3
  total_intl_calls total_intl_charge number_customer_service_calls churn
1                3              2.70                             1    no
2                3              3.70                             1    no
3                5              3.29                             0    no
4                7              1.78                             2    no
5                3              2.73                             3    no
6                6              1.70                             0    no
> head(churnTest)
  state account_length     area_code international_plan voice_mail_plan number_vmail_messages
1    HI            101 area_code_510                 no              no                     0
2    MT            137 area_code_510                 no              no                     0
3    OH            103 area_code_408                 no             yes                    29
4    NM             99 area_code_415                 no              no                     0
5    SC            108 area_code_415                 no              no                     0
6    IA            117 area_code_415                 no              no                     0
  total_day_minutes total_day_calls total_day_charge total_eve_minutes total_eve_calls
1              70.9             123            12.05             211.9              73
2             223.6              86            38.01             244.8             139
3             294.7              95            50.10             237.3             105
4             216.8             123            36.86             126.4              88
5             197.4              78            33.56             124.0             101
6             226.5              85            38.51             141.6              68
  total_eve_charge total_night_minutes total_night_calls total_night_charge total_intl_minutes
1            18.01               236.0                73              10.62               10.6
2            20.81                94.2                81               4.24                9.5
3            20.17               300.3               127              13.51               13.7
4            10.74               220.6                82               9.93               15.7
5            10.54               204.5               107               9.20                7.7
6            12.04               223.0                90              10.04                6.9
  total_intl_calls total_intl_charge number_customer_service_calls churn
1                3              2.86                             3    no
2                7              2.57                             0    no
3                6              3.70                             1    no
4                2              4.24                             1    no
5                4              2.08                             2    no
6                5              1.86                             1    no
> fit <- glm(churn~.,data = churnTrain,family = binomial(link='logit'))
> summary(fit)

Call:
glm(formula = churn ~ ., family = binomial(link = "logit"), data = churnTrain)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-3.0431   0.1661   0.3123   0.4995   1.9487  

Coefficients:
                                Estimate Std. Error z value Pr(>|z|)    
(Intercept)                    9.686e+00  9.798e-01   9.885  < 2e-16 ***
stateAL                       -3.385e-01  7.629e-01  -0.444 0.657272    
stateAR                       -9.106e-01  7.519e-01  -1.211 0.225884    
stateAZ                       -8.973e-02  8.452e-01  -0.106 0.915453    
stateCA                       -1.816e+00  7.822e-01  -2.322 0.020238 *  
stateCO                       -6.445e-01  7.631e-01  -0.845 0.398339    
stateCT                       -1.021e+00  7.252e-01  -1.408 0.159167    
stateDC                       -6.880e-01  8.081e-01  -0.851 0.394577    
stateDE                       -7.460e-01  7.490e-01  -0.996 0.319234    
stateFL                       -5.916e-01  7.610e-01  -0.777 0.436956    
stateGA                       -6.601e-01  7.778e-01  -0.849 0.396075    
stateHI                        2.300e-01  8.963e-01   0.257 0.797469    
stateIA                       -2.083e-01  9.024e-01  -0.231 0.817410    
stateID                       -8.705e-01  7.474e-01  -1.165 0.244100    
stateIL                        2.382e-01  8.340e-01   0.286 0.775165    
stateIN                       -4.410e-01  7.526e-01  -0.586 0.557924    
stateKS                       -1.062e+00  7.296e-01  -1.455 0.145659    
stateKY                       -7.889e-01  7.658e-01  -1.030 0.302931    
stateLA                       -5.546e-01  8.352e-01  -0.664 0.506716    
stateMA                       -1.161e+00  7.430e-01  -1.562 0.118261    
stateMD                       -1.144e+00  7.168e-01  -1.596 0.110430    
stateME                       -1.327e+00  7.281e-01  -1.823 0.068321 .  
stateMI                       -1.390e+00  7.137e-01  -1.948 0.051400 .  
stateMN                       -1.160e+00  7.149e-01  -1.622 0.104709    
stateMO                       -5.979e-01  7.741e-01  -0.772 0.439914    
stateMS                       -1.355e+00  7.278e-01  -1.862 0.062601 .  
stateMT                       -1.865e+00  7.166e-01  -2.603 0.009245 ** 
stateNC                       -5.765e-01  7.545e-01  -0.764 0.444822    
stateND                       -1.274e-01  7.969e-01  -0.160 0.872995    
stateNE                       -2.952e-01  8.055e-01  -0.367 0.713984    
stateNH                       -1.160e+00  7.689e-01  -1.509 0.131367    
stateNJ                       -1.572e+00  7.098e-01  -2.215 0.026757 *  
stateNM                       -4.590e-01  7.867e-01  -0.583 0.559596    
stateNV                       -1.251e+00  7.245e-01  -1.727 0.084198 .  
stateNY                       -1.161e+00  7.191e-01  -1.614 0.106496    
stateOH                       -6.726e-01  7.464e-01  -0.901 0.367508    
stateOK                       -8.660e-01  7.557e-01  -1.146 0.251811    
stateOR                       -7.684e-01  7.354e-01  -1.045 0.296126    
statePA                       -1.141e+00  7.791e-01  -1.464 0.143121    
stateRI                        1.099e-01  8.198e-01   0.134 0.893337    
stateSC                       -1.747e+00  7.371e-01  -2.370 0.017782 *  
stateSD                       -8.227e-01  7.607e-01  -1.081 0.279510    
stateTN                       -2.604e-01  8.207e-01  -0.317 0.751071    
stateTX                       -1.637e+00  7.079e-01  -2.313 0.020745 *  
stateUT                       -1.047e+00  7.435e-01  -1.408 0.159056    
stateVA                        4.425e-01  8.220e-01   0.538 0.590344    
stateVT                       -8.390e-02  7.799e-01  -0.108 0.914330    
stateWA                       -1.400e+00  7.237e-01  -1.934 0.053081 .  
stateWI                       -2.836e-01  7.798e-01  -0.364 0.716109    
stateWV                       -5.732e-01  7.329e-01  -0.782 0.434139    
stateWY                       -2.952e-01  7.541e-01  -0.391 0.695449    
account_length                -9.646e-04  1.434e-03  -0.673 0.501212    
area_codearea_code_415         7.876e-02  1.418e-01   0.555 0.578569    
area_codearea_code_510         1.016e-01  1.632e-01   0.622 0.533622    
international_planyes         -2.192e+00  1.534e-01 -14.294  < 2e-16 ***
voice_mail_planyes             2.131e+00  5.944e-01   3.585 0.000337 ***
number_vmail_messages         -3.832e-02  1.865e-02  -2.055 0.039866 *  
total_day_minutes              3.823e-01  3.380e+00   0.113 0.909942    
total_day_calls               -4.045e-03  2.862e-03  -1.414 0.157477    
total_day_charge              -2.326e+00  1.988e+01  -0.117 0.906870    
total_eve_minutes             -8.927e-01  1.700e+00  -0.525 0.599510    
total_eve_calls               -1.018e-03  2.890e-03  -0.352 0.724642    
total_eve_charge               1.041e+01  2.000e+01   0.521 0.602695    
total_night_minutes            2.228e-01  9.044e-01   0.246 0.805401    
total_night_calls             -1.810e-04  2.928e-03  -0.062 0.950718    
total_night_charge            -5.039e+00  2.010e+01  -0.251 0.802042    
total_intl_minutes             4.149e+00  5.494e+00   0.755 0.450194    
total_intl_calls               9.055e-02  2.575e-02   3.516 0.000438 ***
total_intl_charge             -1.567e+01  2.035e+01  -0.770 0.441115    
number_customer_service_calls -5.366e-01  4.100e-02 -13.089  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 2758.3  on 3332  degrees of freedom
Residual deviance: 2070.8  on 3263  degrees of freedom
AIC: 2210.8

Number of Fisher Scoring iterations: 6

> library(MASS)
> step_fit <- stepAIC(fit,method='backward')
Start:  AIC=2210.8
churn ~ state + account_length + area_code + international_plan + 
    voice_mail_plan + number_vmail_messages + total_day_minutes + 
    total_day_calls + total_day_charge + total_eve_minutes + 
    total_eve_calls + total_eve_charge + total_night_minutes + 
    total_night_calls + total_night_charge + total_intl_minutes + 
    total_intl_calls + total_intl_charge + number_customer_service_calls

                                Df Deviance    AIC
- state                         50   2158.3 2198.3
- area_code                      2   2071.2 2207.2
- total_night_calls              1   2070.8 2208.8
- total_day_minutes              1   2070.8 2208.8
- total_day_charge               1   2070.8 2208.8
- total_night_minutes            1   2070.9 2208.9
- total_night_charge             1   2070.9 2208.9
- total_eve_calls                1   2070.9 2208.9
- total_eve_charge               1   2071.1 2209.1
- total_eve_minutes              1   2071.1 2209.1
- account_length                 1   2071.3 2209.3
- total_intl_minutes             1   2071.4 2209.4
- total_intl_charge              1   2071.4 2209.4
<none>                               2070.8 2210.8
- total_day_calls                1   2072.8 2210.8
- number_vmail_messages          1   2075.1 2213.1
- total_intl_calls               1   2083.9 2221.9
- voice_mail_plan                1   2084.9 2222.9
- number_customer_service_calls  1   2250.7 2388.7
- international_plan             1   2272.4 2410.4

Step:  AIC=2198.28
churn ~ account_length + area_code + international_plan + voice_mail_plan + 
    number_vmail_messages + total_day_minutes + total_day_calls + 
    total_day_charge + total_eve_minutes + total_eve_calls + 
    total_eve_charge + total_night_minutes + total_night_calls + 
    total_night_charge + total_intl_minutes + total_intl_calls + 
    total_intl_charge + number_customer_service_calls

                                Df Deviance    AIC
- area_code                      2   2158.7 2194.7
- total_day_minutes              1   2158.3 2196.3
- total_day_charge               1   2158.3 2196.3
- total_night_minutes            1   2158.3 2196.3
- total_night_charge             1   2158.3 2196.3
- total_night_calls              1   2158.3 2196.3
- total_eve_calls                1   2158.4 2196.4
- total_eve_charge               1   2158.5 2196.5
- total_eve_minutes              1   2158.5 2196.5
- account_length                 1   2158.6 2196.6
- total_intl_minutes             1   2158.9 2196.9
- total_intl_charge              1   2158.9 2196.9
- total_day_calls                1   2159.6 2197.6
<none>                               2158.3 2198.3
- number_vmail_messages          1   2162.3 2200.3
- voice_mail_plan                1   2171.8 2209.8
- total_intl_calls               1   2172.7 2210.7
- number_customer_service_calls  1   2335.5 2373.5
- international_plan             1   2347.8 2385.8

Step:  AIC=2194.72
churn ~ account_length + international_plan + voice_mail_plan + 
    number_vmail_messages + total_day_minutes + total_day_calls + 
    total_day_charge + total_eve_minutes + total_eve_calls + 
    total_eve_charge + total_night_minutes + total_night_calls + 
    total_night_charge + total_intl_minutes + total_intl_calls + 
    total_intl_charge + number_customer_service_calls

                                Df Deviance    AIC
- total_day_minutes              1   2158.7 2192.7
- total_day_charge               1   2158.7 2192.7
- total_night_minutes            1   2158.7 2192.7
- total_night_charge             1   2158.8 2192.8
- total_night_calls              1   2158.8 2192.8
- total_eve_calls                1   2158.9 2192.9
- total_eve_charge               1   2159.0 2193.0
- total_eve_minutes              1   2159.0 2193.0
- account_length                 1   2159.1 2193.1
- total_intl_minutes             1   2159.4 2193.4
- total_intl_charge              1   2159.4 2193.4
- total_day_calls                1   2160.1 2194.1
<none>                               2158.7 2194.7
- number_vmail_messages          1   2162.7 2196.7
- voice_mail_plan                1   2172.3 2206.3
- total_intl_calls               1   2173.3 2207.3
- number_customer_service_calls  1   2335.7 2369.7
- international_plan             1   2348.3 2382.3

Step:  AIC=2192.73
churn ~ account_length + international_plan + voice_mail_plan + 
    number_vmail_messages + total_day_calls + total_day_charge + 
    total_eve_minutes + total_eve_calls + total_eve_charge + 
    total_night_minutes + total_night_calls + total_night_charge + 
    total_intl_minutes + total_intl_calls + total_intl_charge + 
    number_customer_service_calls

                                Df Deviance    AIC
- total_night_minutes            1   2158.8 2190.8
- total_night_charge             1   2158.8 2190.8
- total_night_calls              1   2158.8 2190.8
- total_eve_calls                1   2158.9 2190.9
- total_eve_charge               1   2159.0 2191.0
- total_eve_minutes              1   2159.0 2191.0
- account_length                 1   2159.1 2191.1
- total_intl_minutes             1   2159.4 2191.4
- total_intl_charge              1   2159.4 2191.4
- total_day_calls                1   2160.1 2192.1
<none>                               2158.7 2192.7
- number_vmail_messages          1   2162.7 2194.7
- voice_mail_plan                1   2172.3 2204.3
- total_intl_calls               1   2173.3 2205.3
- total_day_charge               1   2316.9 2348.9
- number_customer_service_calls  1   2335.7 2367.7
- international_plan             1   2348.3 2380.3

Step:  AIC=2190.75
churn ~ account_length + international_plan + voice_mail_plan + 
    number_vmail_messages + total_day_calls + total_day_charge + 
    total_eve_minutes + total_eve_calls + total_eve_charge + 
    total_night_calls + total_night_charge + total_intl_minutes + 
    total_intl_calls + total_intl_charge + number_customer_service_calls

                                Df Deviance    AIC
- total_night_calls              1   2158.8 2188.8
- total_eve_calls                1   2158.9 2188.9
- total_eve_charge               1   2159.0 2189.0
- total_eve_minutes              1   2159.0 2189.0
- account_length                 1   2159.1 2189.1
- total_intl_minutes             1   2159.4 2189.4
- total_intl_charge              1   2159.4 2189.4
- total_day_calls                1   2160.1 2190.1
<none>                               2158.8 2190.8
- number_vmail_messages          1   2162.8 2192.8
- total_night_charge             1   2169.9 2199.9
- voice_mail_plan                1   2172.3 2202.3
- total_intl_calls               1   2173.4 2203.4
- total_day_charge               1   2316.9 2346.9
- number_customer_service_calls  1   2335.9 2365.9
- international_plan             1   2348.3 2378.3

Step:  AIC=2188.81
churn ~ account_length + international_plan + voice_mail_plan + 
    number_vmail_messages + total_day_calls + total_day_charge + 
    total_eve_minutes + total_eve_calls + total_eve_charge + 
    total_night_charge + total_intl_minutes + total_intl_calls + 
    total_intl_charge + number_customer_service_calls

                                Df Deviance    AIC
- total_eve_calls                1   2159.0 2187.0
- total_eve_charge               1   2159.1 2187.1
- total_eve_minutes              1   2159.1 2187.1
- account_length                 1   2159.2 2187.2
- total_intl_minutes             1   2159.5 2187.5
- total_intl_charge              1   2159.5 2187.5
- total_day_calls                1   2160.1 2188.1
<none>                               2158.8 2188.8
- number_vmail_messages          1   2162.8 2190.8
- total_night_charge             1   2170.0 2198.0
- voice_mail_plan                1   2172.3 2200.3
- total_intl_calls               1   2173.4 2201.4
- total_day_charge               1   2317.3 2345.3
- number_customer_service_calls  1   2335.9 2363.9
- international_plan             1   2348.6 2376.6

Step:  AIC=2186.96
churn ~ account_length + international_plan + voice_mail_plan + 
    number_vmail_messages + total_day_calls + total_day_charge + 
    total_eve_minutes + total_eve_charge + total_night_charge + 
    total_intl_minutes + total_intl_calls + total_intl_charge + 
    number_customer_service_calls

                                Df Deviance    AIC
- total_eve_charge               1   2159.2 2185.2
- total_eve_minutes              1   2159.2 2185.2
- account_length                 1   2159.3 2185.3
- total_intl_minutes             1   2159.6 2185.6
- total_intl_charge              1   2159.7 2185.7
- total_day_calls                1   2160.3 2186.3
<none>                               2159.0 2187.0
- number_vmail_messages          1   2162.9 2188.9
- total_night_charge             1   2170.1 2196.1
- voice_mail_plan                1   2172.4 2198.4
- total_intl_calls               1   2173.5 2199.5
- total_day_charge               1   2317.7 2343.7
- number_customer_service_calls  1   2336.0 2362.0
- international_plan             1   2348.7 2374.7

Step:  AIC=2185.2
churn ~ account_length + international_plan + voice_mail_plan + 
    number_vmail_messages + total_day_calls + total_day_charge + 
    total_eve_minutes + total_night_charge + total_intl_minutes + 
    total_intl_calls + total_intl_charge + number_customer_service_calls

                                Df Deviance    AIC
- account_length                 1   2159.6 2183.6
- total_intl_minutes             1   2159.9 2183.9
- total_intl_charge              1   2159.9 2183.9
- total_day_calls                1   2160.5 2184.5
<none>                               2159.2 2185.2
- number_vmail_messages          1   2163.1 2187.1
- total_night_charge             1   2170.4 2194.4
- voice_mail_plan                1   2172.6 2196.6
- total_intl_calls               1   2173.7 2197.7
- total_eve_minutes              1   2200.5 2224.5
- total_day_charge               1   2318.3 2342.3
- number_customer_service_calls  1   2336.4 2360.4
- international_plan             1   2348.8 2372.8

Step:  AIC=2183.56
churn ~ international_plan + voice_mail_plan + number_vmail_messages + 
    total_day_calls + total_day_charge + total_eve_minutes + 
    total_night_charge + total_intl_minutes + total_intl_calls + 
    total_intl_charge + number_customer_service_calls

                                Df Deviance    AIC
- total_intl_minutes             1   2160.2 2182.2
- total_intl_charge              1   2160.3 2182.3
- total_day_calls                1   2160.9 2182.9
<none>                               2159.6 2183.6
- number_vmail_messages          1   2163.5 2185.5
- total_night_charge             1   2170.7 2192.7
- voice_mail_plan                1   2172.9 2194.9
- total_intl_calls               1   2174.0 2196.0
- total_eve_minutes              1   2200.7 2222.7
- total_day_charge               1   2318.8 2340.8
- number_customer_service_calls  1   2337.1 2359.1
- international_plan             1   2349.9 2371.9

Step:  AIC=2182.24
churn ~ international_plan + voice_mail_plan + number_vmail_messages + 
    total_day_calls + total_day_charge + total_eve_minutes + 
    total_night_charge + total_intl_calls + total_intl_charge + 
    number_customer_service_calls

                                Df Deviance    AIC
- total_day_calls                1   2161.6 2181.6
<none>                               2160.2 2182.2
- number_vmail_messages          1   2164.1 2184.1
- total_night_charge             1   2171.4 2191.4
- voice_mail_plan                1   2173.5 2193.5
- total_intl_calls               1   2174.5 2194.5
- total_intl_charge              1   2179.2 2199.2
- total_eve_minutes              1   2201.3 2221.3
- total_day_charge               1   2319.8 2339.8
- number_customer_service_calls  1   2337.7 2357.7
- international_plan             1   2350.0 2370.0

Step:  AIC=2181.62
churn ~ international_plan + voice_mail_plan + number_vmail_messages + 
    total_day_charge + total_eve_minutes + total_night_charge + 
    total_intl_calls + total_intl_charge + number_customer_service_calls

                                Df Deviance    AIC
<none>                               2161.6 2181.6
- number_vmail_messages          1   2165.5 2183.5
- total_night_charge             1   2172.9 2190.9
- voice_mail_plan                1   2175.0 2193.0
- total_intl_calls               1   2176.1 2194.1
- total_intl_charge              1   2180.7 2198.7
- total_eve_minutes              1   2202.3 2220.3
- total_day_charge               1   2321.6 2339.6
- number_customer_service_calls  1   2338.5 2356.5
- international_plan             1   2351.2 2369.2
> summary(step_fit)

Call:
glm(formula = churn ~ international_plan + voice_mail_plan + 
    number_vmail_messages + total_day_charge + total_eve_minutes + 
    total_night_charge + total_intl_calls + total_intl_charge + 
    number_customer_service_calls, family = binomial(link = "logit"), 
    data = churnTrain)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-3.2421   0.1969   0.3375   0.5133   2.1204  

Coefficients:
                               Estimate Std. Error z value Pr(>|z|)    
(Intercept)                    8.067161   0.515870  15.638  < 2e-16 ***
international_planyes         -2.040338   0.145243 -14.048  < 2e-16 ***
voice_mail_planyes             2.003234   0.572352   3.500 0.000465 ***
number_vmail_messages         -0.035262   0.017964  -1.963 0.049654 *  
total_day_charge              -0.076589   0.006371 -12.022  < 2e-16 ***
total_eve_minutes             -0.007182   0.001142  -6.290 3.17e-10 ***
total_night_charge            -0.082547   0.024653  -3.348 0.000813 ***
total_intl_calls               0.092176   0.024988   3.689 0.000225 ***
total_intl_charge             -0.326138   0.075453  -4.322 1.54e-05 ***
number_customer_service_calls -0.512256   0.039141 -13.087  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 2758.3  on 3332  degrees of freedom
Residual deviance: 2161.6  on 3323  degrees of freedom
AIC: 2181.6

Number of Fisher Scoring iterations: 6

> confint(step_fit)
Waiting for profiling to be done...
                                     2.5 %        97.5 %
(Intercept)                    7.070889530  9.0938386274
international_planyes         -2.326003857 -1.7562369114
voice_mail_planyes             0.907323062  3.1529598406
number_vmail_messages         -0.070725900 -0.0002376739
total_day_charge              -0.089209954 -0.0642271566
total_eve_minutes             -0.009433875 -0.0049559927
total_night_charge            -0.131007646 -0.0343300642
total_intl_calls               0.043965614  0.1419403277
total_intl_charge             -0.474987860 -0.1790890316
number_customer_service_calls -0.589558964 -0.4360233938
> #ANOVA on base model
> anova(fit,test = 'Chisq')
Analysis of Deviance Table

Model: binomial, link: logit

Response: churn

Terms added sequentially (first to last)


                              Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
NULL                                           3332     2758.3              
state                         50   83.184      3282     2675.1 0.0022252 ** 
account_length                 1    1.221      3281     2673.9 0.2691078    
area_code                      2    0.081      3279     2673.8 0.9602791    
international_plan             1  175.984      3278     2497.8 < 2.2e-16 ***
voice_mail_plan                1   43.206      3277     2454.6 4.928e-11 ***
number_vmail_messages          1    3.785      3276     2450.8 0.0517159 .  
total_day_minutes              1  130.345      3275     2320.5 < 2.2e-16 ***
total_day_calls                1    1.314      3274     2319.2 0.2516785    
total_day_charge               1    0.020      3273     2319.2 0.8879159    
total_eve_minutes              1   31.752      3272     2287.4 1.752e-08 ***
total_eve_calls                1    0.177      3271     2287.2 0.6738425    
total_eve_charge               1    0.610      3270     2286.6 0.4348650    
total_night_minutes            1    8.859      3269     2277.8 0.0029160 ** 
total_night_calls              1    0.023      3268     2277.7 0.8793045    
total_night_charge             1    0.385      3267     2277.3 0.5348351    
total_intl_minutes             1   12.482      3266     2264.9 0.0004108 ***
total_intl_calls               1   13.740      3265     2251.1 0.0002099 ***
total_intl_charge              1    0.472      3264     2250.7 0.4919867    
number_customer_service_calls  1  179.849      3263     2070.8 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> #ANOVA from reduced model after applying the Step AIC
> anova(step_fit,test = 'Chisq')
Analysis of Deviance Table

Model: binomial, link: logit

Response: churn

Terms added sequentially (first to last)


                              Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
NULL                                           3332     2758.3              
international_plan             1  170.400      3331     2587.9 < 2.2e-16 ***
voice_mail_plan                1   41.868      3330     2546.0 9.765e-11 ***
number_vmail_messages          1    3.638      3329     2542.4 0.0564756 .  
total_day_charge               1  135.452      3328     2406.9 < 2.2e-16 ***
total_eve_minutes              1   30.874      3327     2376.1 2.753e-08 ***
total_night_charge             1    8.500      3326     2367.6 0.0035509 ** 
total_intl_calls               1   12.887      3325     2354.7 0.0003309 ***
total_intl_charge              1   16.210      3324     2338.5 5.668e-05 ***
number_customer_service_calls  1  176.839      3323     2161.6 < 2.2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> #plot the fitted model
> plot(fit$fitted.values)
