数据可视化模板
================
李家翔
2018-11-28

以这个
[Kaggle](https://www.kaggle.com/arjunjoshua/predicting-fraud-in-financial-payment-services/data)
的数据为例。

> There is a lack of public available datasets on financial services and
> specially in the emerging mobile money transactions domain. Financial
> datasets are important to many researchers and in particular to us
> performing research in the domain of fraud detection. Part of the
> problem is the intrinsically private nature of financial transactions,
> that leads to no publicly available datasets.

专门做网贷的数据。

``` r
library(data.table)
raw_data <- fread('PS_20174392719_1491204439457_log.csv')
# 数据切分，便于分享。
# 这段代码不需要跑。
sep_index <- rep_len(0:19,nrow(raw_data))
raw_data %>% 
    mutate(sep_index = sep_index) %>% 
    group_by(sep_index) %>% 
    nest() %>% 
    mutate(
        data1 = map2(
            data,sep_index
            ,~write_excel_csv(.x
                              ,file.path('sep_data',
                                         paste0('joshua_data_',.y,'.csv'))
                              )
        )
    )
```

同时把之前的数据切分下。 控制每个数据集20MB左右。

``` r
library(data.table)
library(tidyverse)
```

    ## -- Attaching packages -------------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## √ ggplot2 3.1.0     √ purrr   0.2.5
    ## √ tibble  1.4.2     √ dplyr   0.7.8
    ## √ tidyr   0.8.2     √ stringr 1.3.1
    ## √ readr   1.1.1     √ forcats 0.3.0

    ## -- Conflicts ----------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::between()   masks data.table::between()
    ## x dplyr::filter()    masks stats::filter()
    ## x dplyr::first()     masks data.table::first()
    ## x dplyr::lag()       masks stats::lag()
    ## x dplyr::last()      masks data.table::last()
    ## x purrr::transpose() masks data.table::transpose()

``` r
raw_data <- 
    list.files('sep_data'
    ,full.names=T
    ) %>% 
    map(fread) %>% 
    bind_rows
```

``` r
# 这个函数跑起来很慢
library(DataExplorer)
create_report(raw_data)
```
