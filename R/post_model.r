#' merge fitted values from all segments
#'
#' @export
merge_segments <- function(list_metrics, scorecard) {
  if (length(list_metrics)>1) {
    list_metrics[[1]][[2]]
    df_fitted_all <- suppressWarnings(bind_rows(lapply(list_metrics, function(x){return(x[[1]])})))
    any_newvar <- any(df_fitted_all$model=='gbm_devnewvar')
    df_gini_all <- df_fitted_all %>%
      mutate(segment='ALL') %>%
      group_by(segment, model) %>%
      summarise(gini = pROC::auc(gbf,fitted_value)*2-1) %>%
      ungroup() %>%
      arrange(desc(model))
    message('all-segments gini:')
    print(df_gini_all)

    df_gini_merged <- rbind(df_gini_all,  suppressWarnings(bind_rows(lapply(list_metrics, function(x){return(x[[2]])}))))
    df_ca_merged <- suppressWarnings(bind_rows(lapply(list_metrics, function(x){return(x[[3]])})))
    df_corr_merged <- suppressWarnings(bind_rows(lapply(list_metrics, function(x){return(x[[4]])})))

    metrics_merged <- list(df_fitted_all, df_gini_merged, df_ca_merged, df_corr_merged)
    metrics_merged <- lapply(metrics_merged, append_scorecard_time, run_date = Sys.Date(), scorecard = scorecard)

  } else {
    metrics_merged <- list_metrics[[1]]
    metrics_merged <- lapply(metrics_merged, append_scorecard_time, run_date = Sys.Date(), scorecard = scorecard)
  }
  return(metrics_merged)
}

#' append scorecard and time column at first
#' @export
append_scorecard_time <- function(df, run_date, scorecard){
  if (nrow(df)>0) {
    dfout <- data.frame(run_date = run_date, scorecard=scorecard, df)
  } else {
    dfout <- df[c(1,1,1:ncol(df))]
    colnames(dfout) <- c('run_date','scorecard',colnames(df))
  }
  return(dfout)
}
