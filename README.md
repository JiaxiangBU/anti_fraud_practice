
<!-- README.md is generated from README.Rmd. Please edit that file -->

``` r
library(tidyverse)
#> ─ Attaching packages ────────────────────────────────────── tidyverse 1.2.1 ─
#> ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
#> ✔ tibble  1.4.2     ✔ dplyr   0.7.6
#> ✔ tidyr   0.8.1     ✔ stringr 1.3.1
#> ✔ readr   1.1.1     ✔ forcats 0.3.0
#> ─ Conflicts ──────────────────────────────────────── tidyverse_conflicts() ─
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following objects are masked from 'package:dplyr':
#> 
#>     between, first, last
#> The following object is masked from 'package:purrr':
#> 
#>     transpose
fread('LoanStats3a.csv',nrows = 5)
#>    id member_id loan_amnt funded_amnt funded_amnt_inv       term int_rate
#> 1: NA        NA      5000        5000            4975  36 months   10.65%
#> 2: NA        NA      2500        2500            2500  60 months   15.27%
#> 3: NA        NA      2400        2400            2400  36 months   15.96%
#> 4: NA        NA     10000       10000           10000  36 months   13.49%
#> 5: NA        NA      3000        3000            3000  60 months   12.69%
#>    installment grade sub_grade                emp_title emp_length
#> 1:      162.87     B        B2                           10+ years
#> 2:       59.83     C        C4                    Ryder   < 1 year
#> 3:       84.33     C        C5                           10+ years
#> 4:      339.31     C        C1      AIR RESOURCES BOARD  10+ years
#> 5:       67.79     B        B5 University Medical Group     1 year
#>    home_ownership annual_inc verification_status  issue_d loan_status
#> 1:           RENT      24000            Verified Dec-2011  Fully Paid
#> 2:           RENT      30000     Source Verified Dec-2011 Charged Off
#> 3:           RENT      12252        Not Verified Dec-2011  Fully Paid
#> 4:           RENT      49200     Source Verified Dec-2011  Fully Paid
#> 5:           RENT      80000     Source Verified Dec-2011  Fully Paid
#>    pymnt_plan url
#> 1:          n  NA
#> 2:          n  NA
#> 3:          n  NA
#> 4:          n  NA
#> 5:          n  NA
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              desc
#> 1:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   Borrower added on 12/22/11 > I need to upgrade my business technologies.<br>
#> 2:   Borrower added on 12/22/11 > I plan to use this money to finance the motorcycle i am looking at. I plan to have it paid off as soon as possible/when i sell my old bike. I only need this money because the deal im looking at is to good to pass up.<br><br>  Borrower added on 12/22/11 > I plan to use this money to finance the motorcycle i am looking at. I plan to have it paid off as soon as possible/when i sell my old bike.I only need this money because the deal im looking at is to good to pass up. I have finished college with an associates degree in business and its takingmeplaces<br>
#> 3:                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               
#> 4:                                                                                                                                                                                                                                                                                                                                                                                                                             Borrower added on 12/21/11 > to pay for property tax (borrow from friend, need to pay back) & central A/C need to be replace. I'm very sorry to let my loan expired last time.<br>
#> 5:                                                                                                                                                                                                                                                                                               Borrower added on 12/21/11 > I plan on combining three large interest bills together and freeing up some extra each month to pay toward other bills.  I've always been a good payor but have found myself needing to make adjustments to my budget due to a medical scare. My job is very stable, I love it.<br>
#>           purpose                title zip_code addr_state   dti
#> 1:    credit_card             Computer    860xx         AZ 27.65
#> 2:            car                 bike    309xx         GA  1.00
#> 3: small_business real estate business    606xx         IL  8.72
#> 4:          other             personel    917xx         CA 20.00
#> 5:          other             Personal    972xx         OR 17.94
#>    delinq_2yrs earliest_cr_line inq_last_6mths mths_since_last_delinq
#> 1:           0         Jan-1985              1                     NA
#> 2:           0         Apr-1999              5                     NA
#> 3:           0         Nov-2001              2                     NA
#> 4:           0         Feb-1996              1                     35
#> 5:           0         Jan-1996              0                     38
#>    mths_since_last_record open_acc pub_rec revol_bal revol_util total_acc
#> 1:                     NA        3       0     13648      83.7%         9
#> 2:                     NA        3       0      1687       9.4%         4
#> 3:                     NA        2       0      2956      98.5%        10
#> 4:                     NA       10       0      5598        21%        37
#> 5:                     NA       15       0     27783      53.9%        38
#>    initial_list_status out_prncp out_prncp_inv total_pymnt total_pymnt_inv
#> 1:                   f         0             0    5863.155         5833.84
#> 2:                   f         0             0    1014.530         1014.53
#> 3:                   f         0             0    3005.667         3005.67
#> 4:                   f         0             0   12231.890        12231.89
#> 5:                   f         0             0    4066.908         4066.91
#>    total_rec_prncp total_rec_int total_rec_late_fee recoveries
#> 1:         5000.00        863.16               0.00        0.0
#> 2:          456.46        435.17               0.00      122.9
#> 3:         2400.00        605.67               0.00        0.0
#> 4:        10000.00       2214.92              16.97        0.0
#> 5:         3000.00       1066.91               0.00        0.0
#>    collection_recovery_fee last_pymnt_d last_pymnt_amnt next_pymnt_d
#> 1:                    0.00     Jan-2015          171.62           NA
#> 2:                    1.11     Apr-2013          119.66           NA
#> 3:                    0.00     Jun-2014          649.91           NA
#> 4:                    0.00     Jan-2015          357.48           NA
#> 5:                    0.00     Jan-2017           67.30           NA
#>    last_credit_pull_d collections_12_mths_ex_med
#> 1:           Oct-2018                          0
#> 2:           Oct-2016                          0
#> 3:           Jun-2017                          0
#> 4:           Apr-2016                          0
#> 5:           Apr-2018                          0
#>    mths_since_last_major_derog policy_code application_type
#> 1:                          NA           1       Individual
#> 2:                          NA           1       Individual
#> 3:                          NA           1       Individual
#> 4:                          NA           1       Individual
#> 5:                          NA           1       Individual
#>    annual_inc_joint dti_joint verification_status_joint acc_now_delinq
#> 1:               NA        NA                        NA              0
#> 2:               NA        NA                        NA              0
#> 3:               NA        NA                        NA              0
#> 4:               NA        NA                        NA              0
#> 5:               NA        NA                        NA              0
#>    tot_coll_amt tot_cur_bal open_acc_6m open_act_il open_il_12m
#> 1:           NA          NA          NA          NA          NA
#> 2:           NA          NA          NA          NA          NA
#> 3:           NA          NA          NA          NA          NA
#> 4:           NA          NA          NA          NA          NA
#> 5:           NA          NA          NA          NA          NA
#>    open_il_24m mths_since_rcnt_il total_bal_il il_util open_rv_12m
#> 1:          NA                 NA           NA      NA          NA
#> 2:          NA                 NA           NA      NA          NA
#> 3:          NA                 NA           NA      NA          NA
#> 4:          NA                 NA           NA      NA          NA
#> 5:          NA                 NA           NA      NA          NA
#>    open_rv_24m max_bal_bc all_util total_rev_hi_lim inq_fi total_cu_tl
#> 1:          NA         NA       NA               NA     NA          NA
#> 2:          NA         NA       NA               NA     NA          NA
#> 3:          NA         NA       NA               NA     NA          NA
#> 4:          NA         NA       NA               NA     NA          NA
#> 5:          NA         NA       NA               NA     NA          NA
#>    inq_last_12m acc_open_past_24mths avg_cur_bal bc_open_to_buy bc_util
#> 1:           NA                   NA          NA             NA      NA
#> 2:           NA                   NA          NA             NA      NA
#> 3:           NA                   NA          NA             NA      NA
#> 4:           NA                   NA          NA             NA      NA
#> 5:           NA                   NA          NA             NA      NA
#>    chargeoff_within_12_mths delinq_amnt mo_sin_old_il_acct
#> 1:                        0           0                 NA
#> 2:                        0           0                 NA
#> 3:                        0           0                 NA
#> 4:                        0           0                 NA
#> 5:                        0           0                 NA
#>    mo_sin_old_rev_tl_op mo_sin_rcnt_rev_tl_op mo_sin_rcnt_tl mort_acc
#> 1:                   NA                    NA             NA       NA
#> 2:                   NA                    NA             NA       NA
#> 3:                   NA                    NA             NA       NA
#> 4:                   NA                    NA             NA       NA
#> 5:                   NA                    NA             NA       NA
#>    mths_since_recent_bc mths_since_recent_bc_dlq mths_since_recent_inq
#> 1:                   NA                       NA                    NA
#> 2:                   NA                       NA                    NA
#> 3:                   NA                       NA                    NA
#> 4:                   NA                       NA                    NA
#> 5:                   NA                       NA                    NA
#>    mths_since_recent_revol_delinq num_accts_ever_120_pd num_actv_bc_tl
#> 1:                             NA                    NA             NA
#> 2:                             NA                    NA             NA
#> 3:                             NA                    NA             NA
#> 4:                             NA                    NA             NA
#> 5:                             NA                    NA             NA
#>    num_actv_rev_tl num_bc_sats num_bc_tl num_il_tl num_op_rev_tl
#> 1:              NA          NA        NA        NA            NA
#> 2:              NA          NA        NA        NA            NA
#> 3:              NA          NA        NA        NA            NA
#> 4:              NA          NA        NA        NA            NA
#> 5:              NA          NA        NA        NA            NA
#>    num_rev_accts num_rev_tl_bal_gt_0 num_sats num_tl_120dpd_2m
#> 1:            NA                  NA       NA               NA
#> 2:            NA                  NA       NA               NA
#> 3:            NA                  NA       NA               NA
#> 4:            NA                  NA       NA               NA
#> 5:            NA                  NA       NA               NA
#>    num_tl_30dpd num_tl_90g_dpd_24m num_tl_op_past_12m pct_tl_nvr_dlq
#> 1:           NA                 NA                 NA             NA
#> 2:           NA                 NA                 NA             NA
#> 3:           NA                 NA                 NA             NA
#> 4:           NA                 NA                 NA             NA
#> 5:           NA                 NA                 NA             NA
#>    percent_bc_gt_75 pub_rec_bankruptcies tax_liens tot_hi_cred_lim
#> 1:               NA                    0         0              NA
#> 2:               NA                    0         0              NA
#> 3:               NA                    0         0              NA
#> 4:               NA                    0         0              NA
#> 5:               NA                    0         0              NA
#>    total_bal_ex_mort total_bc_limit total_il_high_credit_limit
#> 1:                NA             NA                         NA
#> 2:                NA             NA                         NA
#> 3:                NA             NA                         NA
#> 4:                NA             NA                         NA
#> 5:                NA             NA                         NA
#>    revol_bal_joint sec_app_earliest_cr_line sec_app_inq_last_6mths
#> 1:              NA                       NA                     NA
#> 2:              NA                       NA                     NA
#> 3:              NA                       NA                     NA
#> 4:              NA                       NA                     NA
#> 5:              NA                       NA                     NA
#>    sec_app_mort_acc sec_app_open_acc sec_app_revol_util
#> 1:               NA               NA                 NA
#> 2:               NA               NA                 NA
#> 3:               NA               NA                 NA
#> 4:               NA               NA                 NA
#> 5:               NA               NA                 NA
#>    sec_app_open_act_il sec_app_num_rev_accts
#> 1:                  NA                    NA
#> 2:                  NA                    NA
#> 3:                  NA                    NA
#> 4:                  NA                    NA
#> 5:                  NA                    NA
#>    sec_app_chargeoff_within_12_mths sec_app_collections_12_mths_ex_med
#> 1:                               NA                                 NA
#> 2:                               NA                                 NA
#> 3:                               NA                                 NA
#> 4:                               NA                                 NA
#> 5:                               NA                                 NA
#>    sec_app_mths_since_last_major_derog hardship_flag hardship_type
#> 1:                                  NA             N            NA
#> 2:                                  NA             N            NA
#> 3:                                  NA             N            NA
#> 4:                                  NA             N            NA
#> 5:                                  NA             N            NA
#>    hardship_reason hardship_status deferral_term hardship_amount
#> 1:              NA              NA            NA              NA
#> 2:              NA              NA            NA              NA
#> 3:              NA              NA            NA              NA
#> 4:              NA              NA            NA              NA
#> 5:              NA              NA            NA              NA
#>    hardship_start_date hardship_end_date payment_plan_start_date
#> 1:                  NA                NA                      NA
#> 2:                  NA                NA                      NA
#> 3:                  NA                NA                      NA
#> 4:                  NA                NA                      NA
#> 5:                  NA                NA                      NA
#>    hardship_length hardship_dpd hardship_loan_status
#> 1:              NA           NA                   NA
#> 2:              NA           NA                   NA
#> 3:              NA           NA                   NA
#> 4:              NA           NA                   NA
#> 5:              NA           NA                   NA
#>    orig_projected_additional_accrued_interest
#> 1:                                         NA
#> 2:                                         NA
#> 3:                                         NA
#> 4:                                         NA
#> 5:                                         NA
#>    hardship_payoff_balance_amount hardship_last_payment_amount
#> 1:                             NA                           NA
#> 2:                             NA                           NA
#> 3:                             NA                           NA
#> 4:                             NA                           NA
#> 5:                             NA                           NA
#>    disbursement_method debt_settlement_flag debt_settlement_flag_date
#> 1:                Cash                    N                        NA
#> 2:                Cash                    N                        NA
#> 3:                Cash                    N                        NA
#> 4:                Cash                    N                        NA
#> 5:                Cash                    N                        NA
#>    settlement_status settlement_date settlement_amount
#> 1:                NA              NA                NA
#> 2:                NA              NA                NA
#> 3:                NA              NA                NA
#> 4:                NA              NA                NA
#> 5:                NA              NA                NA
#>    settlement_percentage settlement_term
#> 1:                    NA              NA
#> 2:                    NA              NA
#> 3:                    NA              NA
#> 4:                    NA              NA
#> 5:                    NA              NA
```
