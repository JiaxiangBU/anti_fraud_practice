
主要参考 Baesens and Höppner (2018) 的讲解。

主要内容

1.  periodic time features
2.  use networks to fight fraud <!-- 老柴 -->
3.  the imbalance or skewness of the data and
4.  the various costs for different types of misclassification
5.  digit analysis

<!-- end list -->

``` r
knitr::opts_chunk$set(warning = FALSE, message = FALSE)
```

    ## [1] 7309.576 3048.104 4479.268 7170.568 2318.984 7674.572

    ## # A tibble: 6 x 2
    ##   ORIG_BALANCE_BEFORE AMOUNT
    ## *               <dbl>  <dbl>
    ## 1                1902     25
    ## 2                1766     35
    ## 3                1252     25
    ## 4                2112     39
    ## 5                1247    277
    ## 6                 945     NA

  - read
    `.Rdata`  
    <https://stackoverflow.com/questions/7270544/how-to-see-data-from-rdata-file>

<!-- end list -->

``` r
transfers <- 
    plyr::join_all(
        list(
            fread(here::here('data','transfer01.csv'))
            ,fread(here::here('data','transfer02.csv'))
            ,fread(here::here('data','transfer03.csv'))
        )
        ,by ='id'
        ,type = 'left'
    )
```

``` r
theme_nothing <- 
function (base_size = 12, legend = FALSE) 
{
    if (legend) {
        return(theme(axis.text = element_blank(), axis.title = element_blank(), 
            panel.background = element_blank(), panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), axis.ticks.length = unit(0, 
                "cm"), panel.margin = unit(0, "lines"), plot.margin = unit(c(0, 
                0, 0, 0), "lines"), complete = TRUE))
    }
    else {
        return(theme(line = element_blank(), rect = element_blank(), 
            text = element_blank(), axis.ticks.length = unit(0, 
                "cm"), legend.position = "none", panel.margin = unit(0, 
                "lines"), plot.margin = unit(c(0, 0, 0, 0), "lines"), 
            complete = TRUE))
    }
}
```

# Imbalance

``` r
# Print the first 6 rows of the dataset
head(transfers)
```

    ## # A tibble: 6 x 13
    ##      id fraud_flag transfer_id timestamp orig_account_id benef_account_id
    ##   <int>      <int> <chr>           <int> <chr>           <chr>           
    ## 1   226          0 xtr215694    21966103 X27769025       X86111129       
    ## 2   141          0 xtr671675    40885557 X15452684       X63932196       
    ## 3   493          0 xtr977348    19945191 X96278924       X56011266       
    ## 4   240          0 xtr655123    27404301 X27769025       X95653232       
    ## 5   445          0 xtr785302     6566236 X96278924       X85352318       
    ## 6    88          0 xtr187306    17576922 X15452684       X18544316       
    ## # ... with 7 more variables: benef_country <chr>, channel_cd <chr>,
    ## #   authentication_cd <chr>, communication_cd <chr>,
    ## #   empty_communication_flag <int>, orig_balance_before <int>,
    ## #   amount <int>

``` r
# Display the structure of the dataset
str(transfers)
```

    ## Classes 'data.table' and 'data.frame':   628 obs. of  13 variables:
    ##  $ id                      : int  226 141 493 240 445 88 714 475 97 132 ...
    ##  $ fraud_flag              : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ transfer_id             : chr  "xtr215694" "xtr671675" "xtr977348" "xtr655123" ...
    ##  $ timestamp               : int  21966103 40885557 19945191 27404301 6566236 17576922 29583007 14857126 22221450 38214048 ...
    ##  $ orig_account_id         : chr  "X27769025" "X15452684" "X96278924" "X27769025" ...
    ##  $ benef_account_id        : chr  "X86111129" "X63932196" "X56011266" "X95653232" ...
    ##  $ benef_country           : chr  "ISO03" "ISO03" "ISO03" "ISO03" ...
    ##  $ channel_cd              : chr  "CH01" "CH03" "CH04" "CH01" ...
    ##  $ authentication_cd       : chr  "AU02" "AU02" "AU05" "AU04" ...
    ##  $ communication_cd        : chr  "COM02" "COM02" "COM02" "COM02" ...
    ##  $ empty_communication_flag: int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ orig_balance_before     : int  5412 7268 1971 10603 6228 4933 1779 1866 4582 6218 ...
    ##  $ amount                  : int  33 40 227 20 5176 54 71 27 28 59 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
# Determine fraction of legitimate and fraudulent cases
class_distribution <- prop.table(table(transfers$fraud_flag))
print(class_distribution)
```

    ## 
    ##          0          1 
    ## 0.97770701 0.02229299

``` r
# Make pie chart of column fraud_flag
df <- data.frame(class = c("no fraud", "fraud"), 
                 pct = as.numeric(class_distribution)) %>%
  mutate(class = factor(class, levels = c("no fraud", "fraud")),
         cumulative = cumsum(pct), midpoint = cumulative - pct / 2,
         label = paste0(class, " ", round(pct*100, 2), "%"))
# df
# with
# name pct cum_pct and label
ggplot(df, aes(x = 1, weight = pct, fill = class)) +
    # for polar 
    scale_fill_manual(values = c("dodgerblue", "red")) +
    # change default col
    geom_bar(width = 1, position = "stack") +
    coord_polar(theta = "y") +
    geom_text(aes(x = 1.3, y = midpoint, label = label)) +
    # the label pos is set by x.
    theme_nothing()
```

![](datacamp_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Here is the imnbalance of data.

Set confusion matrix with loss cost.

``` r
# Create vector predictions containing 0 for every transfer
predictions <- factor(rep.int(0, nrow(transfers)), levels = c(0, 1))

# Compute confusion matrix
library(caret)
levels(predictions)
```

    ## [1] "0" "1"

``` r
levels(as.factor(transfers$fraud_flag))
```

    ## [1] "0" "1"

``` r
# 错误: package e1071 is required
confusionMatrix(data = predictions, reference = as.factor(transfers$fraud_flag))
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 614  14
    ##          1   0   0
    ##                                           
    ##                Accuracy : 0.9777          
    ##                  95% CI : (0.9629, 0.9878)
    ##     No Information Rate : 0.9777          
    ##     P-Value [Acc > NIR] : 0.570441        
    ##                                           
    ##                   Kappa : 0               
    ##  Mcnemar's Test P-Value : 0.000512        
    ##                                           
    ##             Sensitivity : 1.0000          
    ##             Specificity : 0.0000          
    ##          Pos Pred Value : 0.9777          
    ##          Neg Pred Value :    NaN          
    ##              Prevalence : 0.9777          
    ##          Detection Rate : 0.9777          
    ##    Detection Prevalence : 1.0000          
    ##       Balanced Accuracy : 0.5000          
    ##                                           
    ##        'Positive' Class : 0               
    ## 

``` r
# Compute cost of not detecting fraud
cost <- sum(transfers$amount[transfers$fraud_flag == 1])
print(cost)
```

    ## [1] 64410

`amount`是借款本金，在不考虑逾期后回款的情况(这是欺诈用户的特征)，那么都算损失。

# Time feature

> Do not use arithmetic mean to compute an average timestamp\!

Use periodic mean.

\(\Box\) 参考PPT 把特例举例出来。

因为24小时制，0点是跟11点和1点都非常近似。 以下展示图。 这个特征好。

\(\Box\) xyjl 注册时间的分布，执行区间，短信消息的区间，这些可以做起来！

> The circular histogram is a visual representation of the timestamps of
> events.

解决方案是使用循环直方图。

\(\Box\) 下载PPT。

``` r
timestamps <- 
c(
    "08:43:48","09:17:52","12:56:22","12:27:32","10:59:23","07:22:45"
    ,"11:13:59","10:13:26","10:07:01","06:09:56","12:43:17","07:07:35"
    ,"09:36:44","10:45:00","08:27:36","07:55:35","11:32:56","13:18:35"
    ,"11:09:51","09:46:33","06:59:12","10:19:36","09:39:47","09:39:46"
    ,"18:23:54"
)
```

\(\Box\) 也可以出考题。

Use Von Mises distribution.

``` r
# Convert the plain text to hours
library(lubridate)
ts <- as.numeric(hms(timestamps)) / 3600

# Convert the data to class circular
library(circular)
ts <- circular(ts, units = 'hours', template = "clock24")
# input is decimal timestamp

# Create the von Mises distribution estimates
estimates <- mle.vonmises(ts)
p_mean <- estimates$mu %% 24
p_mean
```

    ## Circular Data: 
    ## Type = angles 
    ## Units = hours 
    ## Template = clock24 
    ## Modulo = asis 
    ## Zero = 1.570796 
    ## Rotation = clock 
    ## [1] 10.09938

``` r
# In the plot, 10 AM is the peroidic mean.

# Plot a circular histogram
clock <- ggplot(data.frame(ts), aes(x = ts)) +
  geom_histogram(breaks = seq(0, 24), colour = "blue", fill = "lightblue") +
  coord_polar() + scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24)) +
  geom_vline(xintercept = as.numeric(p_mean), color = "red", linetype = 2, size = 1.5)
plot(clock)
```

![](datacamp_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

因此发现有一个出现在晚上6点半左右，那么就算异常。

预测置信区间。

``` r
# Estimate the periodic mean and concentration on the first 24 timestamps
p_mean <- estimates$mu %% 24
concentration <- estimates$kappa

# Estimate densities of all 25 timestamps
densities <- dvonmises(ts, mu = p_mean, kappa = concentration)

# Check if the densities are larger than the cutoff of 95%-CI
cutoff <- dvonmises(qvonmises((1 - .95)/2, mu = p_mean, kappa = concentration), mu = p_mean, kappa = concentration)

# Define the variable time_feature
time_feature <- densities >= cutoff
print(cbind.data.frame(ts, time_feature))
```

    ##           ts time_feature
    ## 1   8.730000         TRUE
    ## 2   9.297778         TRUE
    ## 3  12.939444         TRUE
    ## 4  12.458889         TRUE
    ## 5  10.989722         TRUE
    ## 6   7.379167         TRUE
    ## 7  11.233056         TRUE
    ## 8  10.223889         TRUE
    ## 9  10.116944         TRUE
    ## 10  6.165556         TRUE
    ## 11 12.721389         TRUE
    ## 12  7.126389         TRUE
    ## 13  9.612222         TRUE
    ## 14 10.750000         TRUE
    ## 15  8.460000         TRUE
    ## 16  7.926389         TRUE
    ## 17 11.548889         TRUE
    ## 18 13.309722         TRUE
    ## 19 11.164167         TRUE
    ## 20  9.775833         TRUE
    ## 21  6.986667         TRUE
    ## 22 10.326667         TRUE
    ## 23  9.663056         TRUE
    ## 24  9.662778         TRUE
    ## 25 18.398333        FALSE

``` r
# time_feature == FALSE => outlier.
```

\(\Box\) 这个人可以follow

von Mises probability distribution

# Frequency feature

查询一个用户不同渠道重复的频率。

``` r
trans_Bob <- 
    plyr::join_all(
        list(
            fread(here::here('data','trans_Bob01.csv'))
            ,fread(here::here('data','trans_Bob02.csv'))
            ,fread(here::here('data','trans_Bob03.csv'))
        )
        ,by ='id'
        ,type = 'left'
    )
```

``` r
# Frequency feature based on channel_cd
frequency_fun <- function(steps, channel) {
  n <- length(steps)
  frequency <- sum(channel[1:n] == channel[n + 1])
  # The value of the current rows is equal to the previous rows.
  # Count 1.
  return(frequency)
}

# Create freq_channel feature
freq_channel <- 
    zoo::rollapply(
        trans_Bob$transfer_id
        ,width = list(-1:-length(trans_Bob$transfer_id))
        ,partial = TRUE
        ,FUN = frequency_fun
        ,trans_Bob$channel_cd
        )

length(freq_channel)
```

    ## [1] 16

``` r
# Print the features channel_cd, freq_channel and fraud_flag next to each other
freq_channel <- c(0, freq_channel)
freq_channel_tbl01 <- 
    cbind.data.frame(trans_Bob$channel_cd, freq_channel, trans_Bob$fraud_flag) %>% 
    set_names('channel_cd','freq_channel','fraud_flag')
```

Another way.

``` r
freq_channel_tbl02 <- 
    trans_Bob %>% 
    mutate(channel_cd = factor(channel_cd)) %>% 
    group_by(account_name,channel_cd) %>% 
    arrange(timestamp) %>% 
    mutate(freq_channel = row_number()-1) %>% 
    ungroup() %>% 
    select(-account_name) %>% 
    select('channel_cd','freq_channel','fraud_flag')
```

``` r
setequal(freq_channel_tbl01,freq_channel_tbl02)
```

    ## TRUE

``` r
freq_channel_tbl02 %>% 
    tail
```

    ## # A tibble: 6 x 3
    ##   channel_cd freq_channel fraud_flag
    ##   <fct>             <dbl>      <int>
    ## 1 CH07                  6          0
    ## 2 CH06                  3          0
    ## 3 CH02                  2          0
    ## 4 CH07                  7          0
    ## 5 CH06                  4          0
    ## 6 CH05                  0          1

注意欺诈发生于`freq_channel=0`的时候，这是freq feature的作用。

# Recency features

\(\Box\) how to add bracket in
ggplot

``` r
knitr::include_graphics(here::here('pic','recencyfeature.png'))
```

![](/Users/vija/Downloads/180805_folder_01/tmp_jli/trans/projIN/anti_fraud_practice/pic/recencyfeature.png)<!-- -->

\[\text{recency} = e^{-\gamma t}\]

1.  \(e^n|n<0 \in (0,1)\)
2.  \(t\) is time interval between two consecutive events of the same
    type
3.  \(\gamma\) close to 0 (e.g. 0.01, 0.02, 0.05), control \(t\) effect

<!-- end list -->

``` r
expand.grid(
    time_interval = 0:350
    ,gamma = c(0.01,0.02,0.05,0.10,0.20)
) %>% 
    mutate(recency = exp(-time_interval*gamma)
           ,text = glue::glue('gamma is {gamma}')
           ) %>% 
    ggplot(aes(x = time_interval,y = recency, col = text)) +
    geom_line()
```

![](datacamp_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

1.  recency descreases by time interval.
2.  recency desceases more by gamma increasing.

<!-- end list -->

``` r
recency_fun <- function(t, gamma, auth_cd, freq_auth) {
    n_t <- length(t)
    if (freq_auth[n_t] == 0) {
        recency <- 0 # recency = 0 when frequency = 0
    } else {
        time_diff <- t[1] - max(t[2:n_t][auth_cd[(n_t-1):1] == auth_cd[n_t]]) # time-interval = current timestamp
        # - timestamp of previous transfer with same auth_cd
        recency <- exp(-gamma * time_diff) 
    }
    return(recency) 
    }
```

``` r
trans <- 
    plyr::join_all(
        list(
            fread(here::here('data','trans01.csv'))
            ,fread(here::here('data','trans02.csv'))
            ,fread(here::here('data','trans03.csv'))
        )
        ,by ='id'
        ,type = 'left'
    )
```

``` r
freq_channel_data <- 
    trans %>% 
    arrange(timestamp) %>% 
    group_by(account_name,channel_cd) %>% 
    mutate(
        time_diff = timestamp-lag(timestamp)
        ,gamma = 0.05116856
        ,rec_channel = 
            ifelse(freq_channel == 0,0,exp(-time_diff*gamma))
            # if (freq_channel == 0) {
            #   print(0)
            # } else {
            #   print(exp(-time_diff*gamma))
            # }
    ) %>% 
    select(account_name, channel_cd, timestamp,freq_channel, rec_channel, fraud_flag)
```

注意`rec_channel=0`产生了欺诈行为。

``` r
transfers %>% 
    
    mutate(channel_cd = factor(channel_cd)) %>% 
    
    # Freq feature
    group_by(orig_account_id,channel_cd) %>% 
    arrange(timestamp) %>% 
    mutate(freq_channel = row_number()-1) %>% 
    ungroup() %>% 
    
    # Rec feature

    group_by(orig_account_id,channel_cd) %>% 
    mutate(
        time_diff = timestamp-lag(timestamp)
        ,gamma = 0.05116856
        ,rec_channel = 
            ifelse(freq_channel == 0,0,exp(-time_diff*gamma))
    ) %>% 
    ungroup() %>% 
    
    # summary
    group_by(fraud_flag) %>% 
    select(freq_channel, rec_channel) %>% 
    nest() %>% 
    transmute(desc = map(data,psych::describe)) %>% 
    unnest()
```

    ## # A tibble: 4 x 13
    ##    vars     n    mean      sd median trimmed   mad   min     max   range
    ##   <int> <dbl>   <dbl>   <dbl>  <dbl>   <dbl> <dbl> <dbl>   <dbl>   <dbl>
    ## 1     1   614 3.50e+1 3.30e+1   25   3.02e+1  29.7     0 1.37e+2 1.37e+2
    ## 2     2   614 6.07e-2 2.28e-1    0   1.45e-3   0       0 1.00e+0 1.00e+0
    ## 3     1    14 3.09e+1 3.49e+1   12.5 2.94e+1  18.5     0 8.00e+1 8.00e+1
    ## 4     2    14 1.39e-4 5.20e-4    0   5.22e-7   0       0 1.94e-3 1.94e-3
    ## # ... with 3 more variables: skew <dbl>, kurtosis <dbl>, se <dbl>

目前欺诈用户的统计指标在这两种变量中差异很大。

# Social network

chai: 小样本进行分析 wj: 数据含有时间吗？

书签
<https://campus.datacamp.com/courses/fraud-detection-in-r/social-network-analytics?ex=2>

# 参考

<div id="refs" class="references">

<div id="ref-BaesensFraud">

Baesens, Bart, and Sebastiaan Höppner. 2018. “Fraud Detection in R.”
2018. <https://www.datacamp.com/courses/fraud-detection-in-r>.

</div>

</div>
