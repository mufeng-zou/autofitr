
#' @export
mztest <- function() {
  print('mz test')
  args <- c('COMMPRS')
  invisible(autofitr(args))
}

#' @export
mztest2 <- function() {
  print('mz test 2')
  args <- c('COMMPRS2')
  invisible(autofitr(args))
}

#' @export
mztest3 <- function() {
  print('mz test 3')
  invisible(autofitr(scorecard='VSA',lookup_table='[MAWORK_CR_DSOL].[CORP\\Mufeng.Zou].MAH_autofitr_lookup'))
}

#' @export
mztest4 <- function() {
  print('mz test 4')
  invisible(autofitr(scorecard='VS12_YX_BEH'))
}

#' @export
mztest5 <- function() {
  print('mz test 5')
  autofitr(scorecard = 'EVO',
           server = 'NZAKLGL-DB601\\STG',
           db = 'MAWORK',
           lookup_table = 'MAWORK.[CORP\\Mufeng.Zou].monitoring_autofitr_lookup')
}

#' example for creating lookup
#' @export
example_lookup <- function() {
  print('example_lookup')
  c_str <- 'x_df3_lc	x_df36	x_dft	x_dft_fraud	x_dft_merc_dc	x_dft_p	x_dft_sts_0	x_dft_time_0	x_dft_time_1	x_pc12	x_pct	x_sa1_dist_acc	x_sa12_dist_sub	x_sa12_lc	x_sa12_nafc	x_sa24_at_pl	x_sa24_dist_sub	x_sa24_nafc	x_sa24_sic_g	x_sa3_lc	x_sa36_dist_ind	x_sa36_dist_sub	x_sa36_nafc	x_sa36_sic_g	x_sa6_comm	x_sa6_dist_acc	x_sa6_it_co	x_sa6_sic_k	x_sat	x_sat_acc_0	x_sat_amt_0	x_sat_amt5_gt_25k	x_sat_appl	x_sat_ass_0	x_sat_at_rm	x_sat_dc	x_sat_ind_0	x_sat_indnz_ff5	x_sat_lc	x_sat_rm	x_sat_ua	xy_age_min	xy_agefle_max	xy_brt	xy_brt_nap_disch	xy_brt_nap_notdisch	xy_brt_non_sionap_disch	xy_brt_non_sionap_notdisch	xy_brt_sio_disch	xy_brt_sio_notdisch	xy_djt	x_dft_p2dft_ratio	x_sat_lc2enq_ratio	xy_brt_status	x_sa6_rm	x_sa12_cc	x_sa360d_rm	x_sa7d	x_sat_nCC30LCRMCM	x_df3_unpaid	x_df6_unpaid	x_df12_unpaid	x_dft_unpaid	x_df3	x_df6	x_df12	x_dft_amt_0	x_sa3	x_sa6	x_sa12	xy_djt_amt'
  p_str_adv <- "case
    when 1=1 then 0.0899
  else 0.0
  end as vs_adv_consumer_constant
  ,case
  when x_df3_lc = 0 or x_df3_lc is null then  0.0466
  when x_df3_lc > 0 and x_df3_lc <= 1 then  -0.4622
  when x_df3_lc > 1 then  -0.8023
  else 0.0
  end as vs_adv_x_df3_lc
  ,case
  when x_df36 = 0 or x_df36 is null then  0.0851
  when x_df36 > 0 and x_df36 <= 1 then  0.0361
  when x_df36 > 1 and x_df36 <= 2 then  -0.0101
  when x_df36 > 2 and x_df36 <= 3 then  -0.0483
  when x_df36 > 3 and x_df36 <= 4 then  -0.066
  when x_df36 > 4 and x_df36 <= 5 then  -0.0807
  when x_df36 > 5 and x_df36 <= 7 then  -0.1059
  when x_df36 > 7 and x_df36 <= 9 then  -0.1276
  when x_df36 > 9 and x_df36 <= 11 then  -0.1684
  when x_df36 > 11 then  -0.2325
  else 0.0
  end as vs_adv_x_df36
  ,case
  when (x_dft_merc_dc>=0 and x_dft_merc_dc <= 1) or (x_dft_merc_dc is null) then  0.0705
  when x_dft_merc_dc > 1 and x_dft_merc_dc <= 2 then  -0.0184
  when x_dft_merc_dc > 2 and x_dft_merc_dc <= 3 then  -0.0687
  when x_dft_merc_dc > 3 and x_dft_merc_dc <= 5 then  -0.131
  when x_dft_merc_dc > 5 and x_dft_merc_dc <= 7 then  -0.2007
  when x_dft_merc_dc > 7 and x_dft_merc_dc <= 10 then  -0.2527
  when x_dft_merc_dc > 10 then  -0.4103
  else 0.0
  end as vs_adv_x_dft_merc_dc
  ,case
  when (x_dft_p2dft_ratio= 0) and (x_dft >= 10) then  -0.797
  when (x_dft_p2dft_ratio > 0 and x_dft_p2dft_ratio <= 0.30) and (x_dft >= 10) then  -0.7245
  when (x_dft_p2dft_ratio = 0) and (x_dft between 8 and 9) then  -0.5285
  when ((x_dft_p2dft_ratio = 0) and (x_dft between 6 and 7))
  or
  ((x_dft_p2dft_ratio > 0.1 and x_dft_p2dft_ratio <=0.30) and (x_dft between 6 and 9)) then -0.4574
  when (x_dft_p2dft_ratio = 0) and (x_dft=5 ) then  -0.4055
  when ((x_dft_p2dft_ratio > 0 and x_dft_p2dft_ratio <= 0.2) and  (x_dft = 5))
  or
  ((x_dft_p2dft_ratio > 0 and x_dft_p2dft_ratio <= 0.25) and  (x_dft = 4))
  or
  ((x_dft_p2dft_ratio = 0 ) and  (x_dft = 4)) then -0.3096
  when ((x_dft_p2dft_ratio = 0) and (x_dft=3)) then  -0.2517
  when (x_dft_p2dft_ratio = 0) and (x_dft=2)
  or
  (x_dft_p2dft_ratio  > 0.3 and x_dft_p2dft_ratio <= 0.6 ) then -0.1246
  when x_dft_p2dft_ratio > 0.6 and x_dft_p2dft_ratio <= 0.9 then  -0.0725
  when x_dft_p2dft_ratio=0 and x_dft=1 then  0.1287
  when x_dft_p2dft_ratio > 0.9 and x_dft_p2dft_ratio <= 1 then  0.3282
  when isnull(x_dft,0)  = 0 then  0.3306
  else 0.0
  end as vs_adv_x_dft_p2dft_ratio
  ,case
  when isnull(x_dft,0)  > 0 and isnull(x_dft_sts_0, '') = '' then -0.6411
  else 0.0
  end as vs_adv_x_dft_sts_0
  ,case
  when x_dft_time_0 >= 0 and x_dft_time_0 <= 1 then  -0.3898
  when x_dft_time_0 > 1 and x_dft_time_0 <= 3 then  -0.2744
  when x_dft_time_0 > 3 and x_dft_time_0 <= 6 then  -0.12
  when x_dft_time_0 > 6 and x_dft_time_0 <= 9 then  -0.0147
  when x_dft_time_0 > 9 and x_dft_time_0 <= 12 then  0.057
  when x_dft_time_0 > 12 and x_dft_time_0 <= 15 then  0.1097
  when x_dft_time_0 > 15 and x_dft_time_0 <= 21 then  0.1728
  when x_dft_time_0 > 21 and x_dft_time_0 <= 30 then  0.2538
  when x_dft_time_0 > 30 and x_dft_time_0 <= 36 then  0.3605
  when x_dft_time_0 > 36 and x_dft_time_0 <= 39 then  0.4176
  when x_dft_time_0 > 39 and x_dft_time_0 <= 48 then  0.5157
  when (x_dft_time_0 > 48) or (x_dft_time_0 is null) then  0.621
  else 0.0
  end as vs_adv_x_dft_time_0
  ,case
  when (x_dft_time_1 > 48) or (x_dft_time_1 is null) then 0.1666
  when x_dft_time_1 >= 0 and x_dft_time_1 <= 1 then  -0.4073
  when x_dft_time_1 > 1 and x_dft_time_1 <= 3 then  -0.365
  when x_dft_time_1 > 3 and x_dft_time_1 <= 6 then  -0.2612
  when x_dft_time_1 > 6 and x_dft_time_1 <= 9 then  -0.202
  when x_dft_time_1 > 9 and x_dft_time_1 <= 21 then  -0.1172
  when x_dft_time_1 > 21 and x_dft_time_1 <= 24 then  -0.0515
  when x_dft_time_1 > 24 and x_dft_time_1 <= 33 then  0.0071
  when x_dft_time_1 > 33 and x_dft_time_1 <= 39 then  0.0308
  when x_dft_time_1 > 39 and x_dft_time_1 <= 48 then  0.0949
  else 0.0
  end as vs_adv_x_dft_time_1
  ,case
  when x_sa12_nafc is null or x_sa12_nafc = 0 then  0.3818
  when x_sa12_nafc > 0 and x_sa12_nafc <= 1 then  0.2152
  when x_sa12_nafc > 1 and x_sa12_nafc <= 2 then  0.0789
  when x_sa12_nafc > 2 and x_sa12_nafc <= 3 then  -0.0322
  when x_sa12_nafc > 3 and x_sa12_nafc <= 4 then  -0.1824
  when x_sa12_nafc > 4 and x_sa12_nafc <= 6 then  -0.2503
  when x_sa12_nafc > 6 and x_sa12_nafc <= 7 then  -0.3962
  when x_sa12_nafc > 7 and x_sa12_nafc <= 8 then  -0.4743
  when x_sa12_nafc > 8 and x_sa12_nafc <= 10 then  -0.5716
  when x_sa12_nafc > 10 and x_sa12_nafc <= 12 then  -0.6787
  when x_sa12_nafc > 12 then  -0.8898
  else 0.0
  end as vs_adv_x_sa12_nafc
  ,case
  when isnull(x_sa24_dist_sub,0) <= 1 then  0.7047
  when x_sa24_dist_sub > 1 and x_sa24_dist_sub <= 2 then  0.4434
  when x_sa24_dist_sub > 2 and x_sa24_dist_sub <= 3 then  0.3383
  when x_sa24_dist_sub > 3 and x_sa24_dist_sub <= 4 then  0.2051
  when x_sa24_dist_sub > 4 and x_sa24_dist_sub <= 5 then  0.1169
  when x_sa24_dist_sub > 5 and x_sa24_dist_sub <= 6 then  -0.034
  when x_sa24_dist_sub > 6 and x_sa24_dist_sub <= 7 then  -0.1253
  when x_sa24_dist_sub > 7 and x_sa24_dist_sub <= 8 then  -0.2128
  when x_sa24_dist_sub > 8 and x_sa24_dist_sub <= 10 then  -0.2949
  when x_sa24_dist_sub > 10 and x_sa24_dist_sub <= 11 then  -0.3882
  when x_sa24_dist_sub > 11 and x_sa24_dist_sub <= 14 then  -0.523
  when x_sa24_dist_sub > 14 and x_sa24_dist_sub <= 15 then  -0.6618
  when x_sa24_dist_sub > 15 and x_sa24_dist_sub <= 16 then  -0.7318
  when x_sa24_dist_sub > 16 and x_sa24_dist_sub <= 21 then  -0.8564
  when x_sa24_dist_sub > 21 and x_sa24_dist_sub <= 22 then  -1.1871
  when x_sa24_dist_sub > 22 then  -1.5101
  else 0.0
  end as vs_adv_x_sa24_dist_sub
  ,case
  when isnull(x_sa36_dist_ind,0) <= 1 then  0.4102
  when x_sa36_dist_ind > 1 and x_sa36_dist_ind <= 2 then  0.2483
  when x_sa36_dist_ind > 2 and x_sa36_dist_ind <= 3 then  0.1182
  when x_sa36_dist_ind > 3 and x_sa36_dist_ind <= 4 then  -0.0143
  when x_sa36_dist_ind > 4 and x_sa36_dist_ind <= 5 then  -0.1261
  when x_sa36_dist_ind > 5 and x_sa36_dist_ind <= 6 then  -0.2409
  when x_sa36_dist_ind > 6 and x_sa36_dist_ind <= 10 then  -0.3465
  when x_sa36_dist_ind > 10 and x_sa36_dist_ind <= 11 then  -0.6989
  when x_sa36_dist_ind > 11 then  -0.8626
  else 0.0
  end as vs_adv_x_sa36_dist_ind
  ,case
  when x_sa6_it_co = 0  or x_sa6_it_co is null then  0.0963
  when x_sa6_it_co > 0 and x_sa6_it_co <= 1 then  -0.357
  when x_sa6_it_co > 1 and x_sa6_it_co <= 2 then  -0.6509
  when x_sa6_it_co > 2 and x_sa6_it_co <= 3 then  -0.8544
  when x_sa6_it_co > 3 then  -1.5864
  else 0.0
  end as vs_adv_x_sa6_it_co
  ,case
  when isnull(x_sat_acc_0,'') in ('cc') then  0.2241
  else 0.0
  end as vs_adv_x_sat_acc_0
  ,case
  when x_sat_amt5_gt_25k = 0 then  -0.0388
  when x_sat_amt5_gt_25k > 0 and x_sat_amt5_gt_25k <= 1 then  0.2735
  when x_sat_amt5_gt_25k > 1 and x_sat_amt5_gt_25k <= 2 then  0.3676
  when x_sat_amt5_gt_25k > 2 and x_sat_amt5_gt_25k <= 3 then  0.4554
  when x_sat_amt5_gt_25k > 3 and x_sat_amt5_gt_25k <= 4 then  0.6343
  when x_sat_amt5_gt_25k > 4 then  0.7882
  else 0.0
  end as vs_adv_x_sat_amt5_gt_25k
  ,case
  when x_sat_at_rm = 0 or x_sat_at_rm is null then  -0.0674
  when x_sat_at_rm > 0 and x_sat_at_rm <= 2 then  0.2992
  when x_sat_at_rm > 2 and x_sat_at_rm <= 6 then  0.3883
  when x_sat_at_rm > 6 and x_sat_at_rm <= 9 then  0.1999
  when x_sat_at_rm > 9 then  -0.1463
  else 0.0
  end as vs_adv_x_sat_at_rm
  ,case
  when x_sat_dc = 0 or x_sat_dc is null then  0.1129
  when x_sat_dc > 0 and x_sat_dc <= 1 then  0.0562
  when x_sat_dc > 1 and x_sat_dc <= 2 then  0.0096
  when x_sat_dc > 2 and x_sat_dc <= 3 then  -0.0293
  when x_sat_dc > 3 and x_sat_dc <= 4 then  -0.052
  when x_sat_dc > 4 and x_sat_dc <= 5 then  -0.0756
  when x_sat_dc > 5 and x_sat_dc <= 7 then  -0.1059
  when x_sat_dc > 7 and x_sat_dc <= 9 then  -0.1388
  when x_sat_dc > 9 and x_sat_dc <= 14 then  -0.1827
  when x_sat_dc > 14 then  -0.2274
  else 0.0
  end as vs_adv_x_sat_dc
  ,case
  when isnull(x_sat,0) < 2 then 0.8435
  when x_sat_lc2enq_ratio is null or x_sat_lc2enq_ratio = 0.0 then  0.5557
  when x_sat_lc2enq_ratio  > 0.0  and x_sat_lc2enq_ratio  <= 0.1 then  0.3173
  when x_sat_lc2enq_ratio  > 0.1  and x_sat_lc2enq_ratio  <= 0.2 then  0.2231
  when x_sat_lc2enq_ratio  > 0.2  and x_sat_lc2enq_ratio  <= 0.3 then  0.0772
  when x_sat_lc2enq_ratio  > 0.3  and x_sat_lc2enq_ratio  <= 0.4 then  -0.0104
  when x_sat_lc2enq_ratio  > 0.4  and x_sat_lc2enq_ratio  <= 0.5 then  -0.0736
  when x_sat_lc2enq_ratio  > 0.5  then  -0.1161
  else 0.0
  end as vs_adv_x_sat_lc2enq_ratio
  ,case
  when xy_agefle_max <= 36 or xy_agefle_max is null then  -0.3448
  when xy_agefle_max > 36 and xy_agefle_max <= 60 then  -0.1709
  when xy_agefle_max > 60 then  0.0743
  else 0.0
  end as vs_adv_xy_agefle_max
  ,case
  when isnull(xy_djt,0) = 0 then  0.0077
  when xy_djt > 0 and xy_djt <= 1 then  -0.0896
  when xy_djt > 1 and xy_djt <= 2 then  -0.1275
  when xy_djt > 2 then  -0.259
  else 0.0
  end as vs_adv_xy_djt"
  p_str_1ap <- ",case
    when xy_agefle_max < 60 AND X_PCT >= 2 then -0.4495
  when xy_agefle_max >= 60 AND X_PCT <= 1 then 1.4117
  when xy_agefle_max >= 60 AND X_PCT = 2 then 1.2141
  when xy_agefle_max >= 60 AND X_PCT = 3 then  0.6358
  when xy_agefle_max >= 60 AND X_PCT > 5 then -1.0143
  else 0.0
  end as vs_ap1_agefle_max_and_x_pct
  ,case
  when 1=1 then 4.1823
  else 0.0
  end as vs_ap1_consumer_constant
  ,case
  when x_sat_acc_0 IN ('CC','NC','T') then 0.1117
  when x_sat_acc_0 IN ('CM','DC','LC','OD','R') then -0.7225
  when x_sat_acc_0 IN ('RM','TR','UA') then 0.6562
  when x_sat_acc_0 IN ('TC') then -0.5045
  else 0.0
  end as vs_ap1_x_sat_acc_0
  ,case
  when x_sat_amt_0 > 15000 and x_sat_amt_0 <= 25000 then 0.8441
  when x_sat_amt_0 > 25000 then 1.3886
  else 0.0
  end as vs_ap1_x_sat_amt_0
  ,case
  when x_sat_ass_0 IN (3,4) then 0.7467
  else 0.0
  end as vs_ap1_x_sat_ass_0
  ,case
  when x_sat_ind_0 IN ('AGRI','ACCT','CONS','ELEC','FOOD','GOVE','MANU','RETA','WHOL')  then 0.611
  when x_sat_ind_0 IN ('APPL','MERC','INSU','NAFC','FINA','MOTO') then -0.3118
  else 0.0
  end as vs_ap1_x_sat_ind_0
  ,case
  when xy_age_min >= 180 and xy_age_min <= 228 then -2.316
  when xy_age_min > 228 and xy_age_min <= 240 then -1.4879
  when xy_age_min > 240 and xy_age_min <= 300 then -0.5597
  when xy_age_min > 300 and xy_age_min <= 480 then 0.1224
  when xy_age_min > 480 and xy_age_min <= 600 then 0.642
  when xy_age_min > 600 and xy_age_min <= 720 then 1.0394
  when xy_age_min > 720 and xy_age_min <= 1188 then 1.7229
  when isnull(xy_age_min,0) = 0 or xy_age_min < 180 or xy_age_min > 1188 then 0.0157
  else 0.0
  end as vs_ap1_xy_age_min
  ,case
  when xy_agefle_max > 144 then 0.6153
  else 0.0
  end as vs_ap1_xy_agefle_max"
  p_str_cd <- ",case
    when 1=1 then 2.6375
  else 0.0
  end as vs_cd_consumer_constant
  ,case
  when isnull(x_sa12_dist_sub,0) <= 1 then  0.3895
  when x_sa12_dist_sub > 1 and x_sa12_dist_sub <= 2 then  0.2593
  when x_sa12_dist_sub > 2 and x_sa12_dist_sub <= 3 then  0.1196
  when x_sa12_dist_sub > 3 and x_sa12_dist_sub <= 4 then  -0.0205
  when x_sa12_dist_sub > 4 and x_sa12_dist_sub <= 5 then  -0.1569
  when x_sa12_dist_sub > 5 and x_sa12_dist_sub <= 6 then  -0.2653
  when x_sa12_dist_sub > 6 and x_sa12_dist_sub <= 7 then  -0.3800
  when x_sa12_dist_sub > 7 and x_sa12_dist_sub <= 9 then  -0.4815
  when x_sa12_dist_sub > 9 and x_sa12_dist_sub <= 10 then  -0.5241
  when x_sa12_dist_sub > 10 and x_sa12_dist_sub <= 14 then  -0.7157
  when x_sa12_dist_sub > 14 then  -1.0462
  else 0.0
  end as vs_cd_x_sa12_dist_sub
  ,case
  when x_sa24_at_pl is null or x_sa24_at_pl = 0 then  0.1356
  when x_sa24_at_pl > 0 and x_sa24_at_pl <= 1 then  -0.3001
  when x_sa24_at_pl > 1 and x_sa24_at_pl <= 2 then  -0.3851
  when x_sa24_at_pl > 2 and x_sa24_at_pl <= 3 then  -0.4844
  when x_sa24_at_pl > 3 and x_sa24_at_pl <= 5 then  -0.5400
  when x_sa24_at_pl > 5 and x_sa24_at_pl <= 8 then  -0.6437
  when x_sa24_at_pl > 8 then  -0.8367
  else 0.0
  end as vs_cd_x_sa24_at_pl
  ,case
  when x_sa24_nafc is null or x_sa24_nafc = 0 then  0.1835
  when x_sa24_nafc > 0 and x_sa24_nafc <= 1 then  0.1146
  when x_sa24_nafc > 1 and x_sa24_nafc <= 2 then  0.0555
  when x_sa24_nafc > 2 and x_sa24_nafc <= 3 then  0.0109
  when x_sa24_nafc > 3 and x_sa24_nafc <= 4 then  -0.0460
  when x_sa24_nafc > 4 and x_sa24_nafc <= 5 then  -0.0729
  when x_sa24_nafc > 5 and x_sa24_nafc <= 7 then  -0.1201
  when x_sa24_nafc > 7 and x_sa24_nafc <= 9 then  -0.1684
  when x_sa24_nafc > 9 and x_sa24_nafc <= 11 then  -0.2131
  when x_sa24_nafc > 11 and x_sa24_nafc <= 12 then  -0.2361
  when x_sa24_nafc > 12 then  -0.2776
  else 0.0
  end as vs_cd_x_sa24_nafc
  ,case
  when x_sa24_sic_g is null or x_sa24_sic_g = 0 then  0.0865
  when x_sa24_sic_g > 0 and x_sa24_sic_g <= 1 then  -0.1648
  when x_sa24_sic_g > 1 and x_sa24_sic_g <= 2 then  -0.3779
  when x_sa24_sic_g > 2 and x_sa24_sic_g <= 4 then  -0.5308
  when x_sa24_sic_g > 4 then  -0.7519
  else 0.0
  end as vs_cd_x_sa24_sic_g
  ,case
  when isnull(x_sa36_dist_sub,0) <= 1 then  0.7471
  when x_sa36_dist_sub > 1 and x_sa36_dist_sub <= 2 then  0.5857
  when x_sa36_dist_sub > 2 and x_sa36_dist_sub <= 3 then  0.4656
  when x_sa36_dist_sub > 3 and x_sa36_dist_sub <= 4 then  0.3197
  when x_sa36_dist_sub > 4 and x_sa36_dist_sub <= 5 then  0.1700
  when x_sa36_dist_sub > 5 and x_sa36_dist_sub <= 6 then  0.0285
  when x_sa36_dist_sub > 6 and x_sa36_dist_sub <= 7 then  -0.0608
  when x_sa36_dist_sub > 7 and x_sa36_dist_sub <= 8 then  -0.1492
  when x_sa36_dist_sub > 8 and x_sa36_dist_sub <= 9 then  -0.2079
  when x_sa36_dist_sub > 9 and x_sa36_dist_sub <= 11 then  -0.2854
  when x_sa36_dist_sub > 11 and x_sa36_dist_sub <= 15 then  -0.4623
  when x_sa36_dist_sub > 15 and x_sa36_dist_sub <= 16 then  -0.5854
  when x_sa36_dist_sub > 16 and x_sa36_dist_sub <= 19 then  -0.7073
  when x_sa36_dist_sub > 19 then  -0.9029
  else 0.0
  end as vs_cd_x_sa36_dist_sub
  ,case
  when x_sa6_it_co is null or x_sa6_it_co = 0 then  0.1133
  when x_sa6_it_co > 0 and x_sa6_it_co <= 1 then  -0.4192
  when x_sa6_it_co > 1 then  -0.8335
  else 0.0
  end as vs_cd_x_sa6_it_co
  ,case
  when x_sa6_sic_k is null or x_sa6_sic_k <= 1 then  0.1724
  when x_sa6_sic_k > 1 and x_sa6_sic_k <= 2 then  0.0635
  when x_sa6_sic_k > 2 and x_sa6_sic_k <= 3 then  -0.0184
  when x_sa6_sic_k > 3 and x_sa6_sic_k <= 4 then  -0.1125
  when x_sa6_sic_k > 4 and x_sa6_sic_k <= 5 then  -0.2011
  when x_sa6_sic_k > 5 and x_sa6_sic_k <= 8 then  -0.3128
  when x_sa6_sic_k > 8 and x_sa6_sic_k <= 9 then  -0.4260
  when x_sa6_sic_k > 9 then  -0.5670
  else 0.0
  end as vs_cd_x_sa6_sic_k
  ,case
  when x_sat_amt5_gt_25k = 0 then  -0.1309
  when x_sat_amt5_gt_25k > 0 and x_sat_amt5_gt_25k <= 1 then  0.2660
  when x_sat_amt5_gt_25k > 1 and x_sat_amt5_gt_25k <= 2 then  0.4815
  when x_sat_amt5_gt_25k > 2 and x_sat_amt5_gt_25k <= 3 then  0.5600
  when x_sat_amt5_gt_25k > 3 then  0.7436
  else 0.0
  end as vs_cd_x_sat_amt5_gt_25k
  ,case
  when x_sat_dc is null or x_sat_dc = 0 then  0.1691
  when x_sat_dc > 0 and x_sat_dc <= 1 then  -0.1953
  when x_sat_dc > 1 and x_sat_dc <= 2 then  -0.5412
  when x_sat_dc > 2 and x_sat_dc <= 3 then  -0.7767
  when x_sat_dc > 3 then  -0.9165
  else 0.0
  end as vs_cd_x_sat_dc
  ,case
  when x_sat_ind_0 = 'MERC' then  -1.5005
  else 0.0
  end as vs_cd_x_sat_ind_0
  ,case
  when x_sat_indnz_ff5 = 0 then  0.2758
  when x_sat_indnz_ff5 > 0 and x_sat_indnz_ff5 <= 1 then  0.0454
  when x_sat_indnz_ff5 > 1 and x_sat_indnz_ff5 <= 2 then  -0.1113
  when x_sat_indnz_ff5 > 2 and x_sat_indnz_ff5 <= 3 then  -0.2310
  when x_sat_indnz_ff5 > 3 then  -0.2987
  else 0.0
  end as vs_cd_x_sat_indnz_ff5
  ,case
  when x_sat_lc2enq_ratio = 0 then  0.3378
  when x_sat_lc2enq_ratio > 0 and x_sat_lc2enq_ratio <= 0.1 then  0.2288
  when x_sat_lc2enq_ratio > 0.1 and x_sat_lc2enq_ratio <= 0.2 then  0.1757
  when x_sat_lc2enq_ratio > 0.2 and x_sat_lc2enq_ratio <= 0.3 then  0.0845
  when x_sat_lc2enq_ratio > 0.3 and x_sat_lc2enq_ratio <= 0.4 then  -0.0209
  when x_sat_lc2enq_ratio > 0.4 and x_sat_lc2enq_ratio <= 0.5 then  -0.0765
  when x_sat_lc2enq_ratio > 0.5 and x_sat_lc2enq_ratio <= 0.6 then  -0.1240
  when x_sat_lc2enq_ratio > 0.6 then  -0.1635
  else 0.0
  end as vs_cd_x_sat_lc2enq_ratio
  ,case
  when x_sat_rm is null or x_sat_rm = 0 then  -0.2306
  when x_sat_rm > 0 then  0.3201
  else 0.0
  end as vs_cd_x_sat_rm
  ,case
  when (xy_age_min > 300 and xy_age_min <= 420) or (isnull(xy_age_min,0) < 180 or xy_age_min > 1188) then  0.0100
  when xy_age_min >= 180 and xy_age_min <= 228 then  -0.8854
  when xy_age_min > 228 and xy_age_min <= 240 then  -0.6658
  when xy_age_min > 240 and xy_age_min <= 252 then  -0.4264
  when xy_age_min > 252 and xy_age_min <= 264 then  -0.3309
  when xy_age_min > 264 and xy_age_min <= 276 then  -0.1993
  when xy_age_min > 276 and xy_age_min <= 300 then  -0.0952
  when xy_age_min > 420 and xy_age_min <= 540 then  0.0658
  when xy_age_min > 540 and xy_age_min <= 720 then  0.1466
  when xy_age_min > 720 and xy_age_min <= 1188 then  0.2481
  else 0.0
  end as vs_cd_xy_age_min
  ,case
  when xy_agefle_max  is null or xy_agefle_max <= 3 then  -1.7786
  when xy_agefle_max > 3 and xy_agefle_max <= 6 then  -1.4736
  when xy_agefle_max > 6 and xy_agefle_max <= 12 then  -1.0917
  when xy_agefle_max > 12 and xy_agefle_max <= 24 then  -0.7895
  when xy_agefle_max > 24 and xy_agefle_max <= 36 then  -0.4494
  when xy_agefle_max > 36 and xy_agefle_max <= 42 then  -0.2144
  when xy_agefle_max > 42 and xy_agefle_max <= 54 then  -0.0917
  when xy_agefle_max > 54 then  0.1802
  else 0.0
  end as vs_cd_xy_agefle_max"
  p_str_cs <- ",case
    when xy_agefle_max >= 0 and xy_agefle_max < 60  then  0.1241
  when xy_agefle_max >= 60 and x_pct = 1  then  0.6178
  when xy_agefle_max >= 60 and x_pct = 2  then  0.4642
  when xy_agefle_max >= 60 and (x_pct >= 3 and x_pct <= 5) then  0.2108
  when xy_agefle_max >= 60 and (x_pct >= 8 and x_pct <= 9 ) then  -0.3272
  when xy_agefle_max >= 60 and x_pct >= 10  then  -0.6113
  else 0.0
  end as vs_cs_agefle_max_and_x_pct
  ,case
  when 1=1 then 3.7290
  else 0.0
  end as vs_cs_consumer_constant
  ,case
  when x_pc12 = 1  then  -0.0171
  when x_pc12  between 2 and 999  then  -0.0443
  when  x_pc12 > 999 then  -0.0187
  else 0.0
  end as vs_cs_x_pc12
  ,case
  when x_sa1_dist_acc = 2  then  -0.3689
  when x_sa1_dist_acc >= 3 and x_sa1_dist_acc <= 6  then  -0.4583
  when isnull(x_sa1_dist_acc,0) < 1  then  -0.0592
  else 0.0
  end as vs_cs_x_sa1_dist_acc
  ,case
  when isnull(x_sa12_lc,0) = 0 then 0.4252
  when x_sa12_lc = 2 then -0.3876
  when x_sa12_lc in (3,4,5,6) then -0.7139
  when x_sa12_lc is null or x_sa12_lc < 0 then 0.1961
  else 0.0
  end as vs_cs_x_sa12_lc
  ,case
  when  x_sa24_dist_sub = 1 then 0.6802
  when x_sa24_dist_sub = 4 then -0.3060
  when x_sa24_dist_sub >= 5 and x_sa24_dist_sub <= 6 then -0.5739
  when x_sa24_dist_sub is null or x_sa24_dist_sub < 1 then 0.1736
  else 0.0
  end as vs_cs_x_sa24_dist_sub
  ,case
  when x_sa3_lc = 2  then  -0.3429
  when x_sa3_lc >= 3 and x_sa3_lc <= 6  then  -0.6568
  when x_sa3_lc < 0 then  -0.0301
  else 0.0
  end as vs_cs_x_sa3_lc
  ,case
  when isnull(x_sa36_nafc , 0) = 0 then 0.1675
  when x_sa36_nafc = 3 then -0.1373
  when x_sa36_nafc >= 4 and x_sa36_nafc <= 6  then  -0.5072
  when  x_sa36_nafc is null or x_sa36_nafc < 0  then  0.0693
  else 0.0
  end as vs_cs_x_sa36_nafc
  ,case
  when isnull(x_sa36_sic_g,0) = 0 then 0.4311
  when x_sa36_sic_g >= 2 and x_sa36_sic_g <= 6 then -0.7227
  when x_sa36_sic_g is null or x_sa36_sic_g < 0 then 0.3857
  else 0.0
  end as vs_cs_x_sa36_sic_g
  ,case
  when isnull(x_sa6_comm,0) = 0  then  0.6268
  when x_sa6_comm in (2,3,4,5,6)  then  -0.6844
  when x_sa6_comm is null or x_sa6_comm < 0  then  0.4782
  else 0.0
  end as vs_cs_x_sa6_comm
  ,case
  when x_sa6_dist_acc = 3  then  -0.4487
  when x_sa6_dist_acc in (4,5,6)  then  -0.5226
  when isnull(x_sa6_dist_acc,0) < 1  then  -0.0352
  else 0.0
  end as vs_cs_x_sa6_dist_acc
  ,case
  when x_sat_appl >= 1 and x_sat_appl <= 6  then  -0.8440
  when x_sat_appl < 0 then  -0.0080
  else 0.0
  end as vs_cs_x_sat_appl
  ,case
  when isnull(x_sat_dc,0) = 0 then 0.3480
  when x_sat_dc >= 2 and x_sat_dc <= 6 then -0.8463
  when x_sat_dc is null or x_sat_dc < 0 then 0.3090
  else 0.0
  end as vs_cs_x_sat_dc
  ,case
  when isnull(x_sat_rm,0) = 0 then -1.4942
  when x_sat_rm = 2 then 0.1960
  when x_sat_rm >= 3 and x_sat_rm <= 6 then 0.3762
  when x_sat_rm is null or x_sat_rm < 0 then -1.1314
  else 0.0
  end as vs_cs_x_sat_rm
  ,case
  when x_sat_ua >= 1 and x_sat_ua <= 6  then  0.8084
  when x_sat_ua < 0 then  0.0411
  else 0.0
  end as vs_cs_x_sat_ua
  ,case
  when xy_age_min >= 180 and xy_age_min <= 227  then  -1.9406
  when xy_age_min > 227 and xy_age_min <= 239  then  -1.4781
  when xy_age_min > 239 and xy_age_min <= 251  then  -1.0169
  when xy_age_min > 251 and xy_age_min <= 263  then  -0.7712
  when xy_age_min > 263 and xy_age_min <= 299  then  -0.4588
  when xy_age_min > 299 and xy_age_min <= 335  then  -0.2223
  when xy_age_min > 383 and xy_age_min <= 479  then  0.0807
  when xy_age_min > 479 and xy_age_min <= 623  then  0.3728
  when xy_age_min > 623 and xy_age_min <= 731  then  0.7512
  when xy_age_min > 731 and xy_age_min <= 1188  then  1.2474
  else 0.0
  end as vs_cs_xy_age_min"
  vcr_str <- 'vcr044	vcr047	vcr123	vcr160	vcr172	vcr174	vcr182	vcr257	vcr258	vcr298	vcr302	vcr304 ratio_vcr258_2_vcr257 vcr076 vcr078 vcr086R1'
  p_str_vcr <- ",case
    when ratio_vcr258_2_vcr257>=0  and  ratio_vcr258_2_vcr257<=   0.14 then -1.1312
  when ratio_vcr258_2_vcr257>0.14   and  ratio_vcr258_2_vcr257<=  0.4 then -1.0197
  when ratio_vcr258_2_vcr257> 0.4   and  ratio_vcr258_2_vcr257<=  0.47 then -0.8841
  when ratio_vcr258_2_vcr257> 0.47   and  ratio_vcr258_2_vcr257<=  0.62 then -0.6536
  when ratio_vcr258_2_vcr257>0.62   and ratio_vcr258_2_vcr257<=  0.73 then -0.4675
  when ratio_vcr258_2_vcr257> 0.73   and ratio_vcr258_2_vcr257 <=  0.8 then -0.3186
  when ratio_vcr258_2_vcr257> 0.8   and  ratio_vcr258_2_vcr257<=  0.92 then -0.0884
  when ratio_vcr258_2_vcr257> 0.92 then 0.2822
  else 0.0
  end as poa_all_ratio_vcr258_2_vcr257
  ,case
  when vcr044>=0  and vcr044 <= 2500 then -0.0795
  when vcr044>2500  and  vcr044<=  5000 then 0.0157
  when vcr044>5000  and  vcr044<=  7500 then 0.0765
  when vcr044>7500  and  vcr044<=  10000 then 0.1525
  when vcr044>10000  and  vcr044<=  15000 then 0.197
  when vcr044> 15000 then 0.2305
  else 0.0
  end as poa_all_vcr044
  ,case
  when vcr076>=0   and  vcr076<=  0.08 then -0.0097
  when vcr076> 0.08   and  vcr076<=  0.5 then -0.0107
  when vcr076>0.5  and  vcr076<=  0.86 then -0.0119
  when vcr076> 0.86 then -0.0129
  else 0.0
  end as poa_all_vcr076
  ,case
  when vcr078>0 then -0.0517
  else 0.0
  end as poa_all_vcr078
  ,case
  when vcr123>0 then -1.0821
  else 0.0
  end as poa_all_vcr123
  ,case
  when vcr160=0 then -0.3232
  when vcr160=1 then -0.2253
  when vcr160=2 then -0.2067
  when vcr160=3 then -0.1877
  when vcr160>3  and   vcr160<=  6 then -0.1717
  when vcr160>6  and  vcr160<=  9 then -0.149
  when vcr160>9 then -0.128
  else 0.0
  end as poa_all_vcr160
  ,case
  when vcr172>=0   and  vcr172<=  0.3 then 0.1014
  when vcr172>0.3  and   vcr172<=  1 then -0.3632
  when vcr172>1  and vcr172<=  2 then -0.813
  when vcr172>2 then -1.0733
  else 0.0
  end as poa_all_vcr172
  ,case
  when vcr174>=0  and   vcr174<=  0.3 then 0.297
  when vcr174>0.3   and  vcr174<=  1 then -0.2409
  when vcr174=2 then -1.0233
  when vcr174>2 then -1.3078
  else 0.0
  end as poa_all_vcr174
  ,case
  when vcr182>=0  and vcr182<=   0.4 then 0.0543
  when vcr182=1 then -0.0238
  when vcr182=2 then -0.1209
  when vcr182>2 then -0.1616
  else 0.0
  end as poa_all_vcr182"

  devvar_adv <- extract_devvar(p_str_adv,c_str)
  devvar_1ap <- extract_devvar(p_str_1ap,c_str)
  devvar_cd <- extract_devvar(p_str_cd,c_str)
  devvar_cs <- extract_devvar(p_str_cs,c_str)

  devvar_vcrs <- extract_devvar(p_str_vcr,vcr_str)

  devvar_adv2 <- paste0(devvar_adv,',',devvar_vcrs)
  devvar_1ap2 <- paste0(devvar_1ap,',',devvar_vcrs)
  devvar_cd2 <- paste0(devvar_cd,',',devvar_vcrs)
  devvar_cs2 <- paste0(devvar_cs,',',devvar_vcrs)
}
