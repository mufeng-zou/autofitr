#' 1. read inputs from sqlserver
#' assume all relevant tables are in same server
#' returns a list of dataframe with attr(,"segment") of score segments
#' and each dataframe has columns:
#' user_ref, gbf, devvar, newvar
#'
#' @export
read_from_sql <- function(scorecard, lookup_server, lookup_db, lookup_table){
  ODBC_string <- paste0("Driver={SQL Server};Server={",lookup_server,"};Database=",lookup_db,";Trusted_Connection=true")
  conn <- RODBC::odbcDriverConnect(ODBC_string, readOnlyOptimize = T)
  q <- paste0("select * from ",lookup_table," where scorecard='",scorecard,"'")
  df_lookup <- RODBC::sqlQuery(conn,q, stringsAsFactors = FALSE)

  l_out <- list()
  for (i in 1:nrow(df_lookup)) {
    df_lookupi <- df_lookup[i,]
    #gbf
    if (!is.na(df_lookupi$score) & df_lookupi$score!='') {
      score_string = df_lookupi$score
    } else {
      score_string = 'null'
    }
    q <- paste('select user_ref,',df_lookupi$gbf,'as gbf,',score_string,'as score from',df_lookupi$out_table,df_lookupi$condition)
    df_out <- RODBC::sqlQuery(conn,q) %>% filter((gbf %in% c('G','B')) | (gbf %in% c(1,0)))
    if (max(df_out$score, na.rm = T)>100) {df_out$score <- (df_out$score-200)/100*log(2)}
    #devvar
    q <- paste('select user_ref,',df_lookupi$devvar,'from',df_lookupi$devvar_table,df_lookupi$condition)
    df_devvar <- RODBC::sqlQuery(conn,q) %>% filter(user_ref %in% df_out$user_ref)
    colnames(df_devvar)[-1] <- paste0('devvar_',colnames(df_devvar)[-1])
    #newvar
    if (!is.na(df_lookupi$newvar) & df_lookupi$newvar!='' &
        !is.na(df_lookupi$newvar_table) & df_lookupi$newvar_table!='') {
      q <- paste('select user_ref,',df_lookupi$newvar,'from',df_lookupi$newvar_table,df_lookupi$condition)
      df_newvar <- RODBC::sqlQuery(conn,q) %>% filter(user_ref %in% df_out$user_ref)
      colnames(df_newvar)[-1] <- paste0('newvar_',colnames(df_newvar)[-1])
    } else {
      df_newvar <- data.frame(user_ref=df_out$user_ref)
    }

    df_merged <- df_out %>%
      #mutate(segment = df_lookupi$segment) %>%
      select(user_ref, gbf, score) %>%
      left_join(df_devvar,by='user_ref') %>%
      left_join(df_newvar,by='user_ref')
    l_out[[i]] <- df_merged
    attr(l_out,'segment')[i] <- df_lookupi$segment
  }

  RODBC::odbcClose(conn)
  return(l_out)
}



#' 5. output 3 tables:
#' myautofitr_value: fitted value from different models
#' myautofitr_gini: devvar gini and dev+newvar gini
#' myautofitr_ca: CA and variable importance
#' myautofitr_corr: correlation matrix newvar to devvar
#'
#' @export
write_to <- function(sqlserver_name, db_name, metrics){

  #to sql
  temp_fn <-'temp_monitoring_refit.txt'

  message('Writing to server ',sqlserver_name,', database ',db_name,'...\n')
  message('Writing table myautofitr_value...')
  suppressMessages(suppressWarnings(todb(metrics[[1]],'myautofitr_value',db_name,sqlserver_name,TRUE,temp_fn)))
  message('Writing table myautofitr_gini...')
  suppressMessages(suppressWarnings(todb(metrics[[2]],'myautofitr_gini',db_name,sqlserver_name,TRUE,temp_fn)))
  message('Writing table myautofitr_ca...')
  suppressMessages(suppressWarnings(todb(metrics[[3]],'myautofitr_ca',db_name,sqlserver_name,TRUE,temp_fn)))
  message('Writing table myautofitr_corr...')
  suppressMessages(suppressWarnings(todb(metrics[[4]],'myautofitr_corr',db_name,sqlserver_name,TRUE,temp_fn)))

  if (file.exists(temp_fn)) file.remove(temp_fn)

  return(TRUE)
}
