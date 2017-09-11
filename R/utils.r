#' 0. parse arguments from cmd line, or from a vector of length 3
#' args <- c('apprwidb006', 'MAWORK_DSOL', 'monitoring_rebuild_testin2')
#'
parse_args <- function(args) {
  if (is.null(args)) {
    #try read from cmdline
    args <- commandArgs(trailingOnly=TRUE)
  }
  if(length(args)!=3)  {
    stop('args should be (a vector of) length 3: c(sqlserver_name, db_name, scorecard)', call. = FALSE)
  }
  return(args)
}

#' cowsay something
#' @export
saysth <- function(sth=NULL) {
  if (is.element('cowsay',installed.packages()[,1]) & is.element('fortunes',installed.packages()[,1])) {
    if (is.null(sth)) {
      sth <- paste(fortunes::fortune() %>% .[!is.na(.)], collapse = '\n')
    }
    cowsay::say(sth, 'cow')
  }
  invisible(NULL)
}

#' extract devvar string from
#' long string for points calculations
#' long string for all chars available
#'
#' @examples
#' \dontrun{
#' #this can be a huge block of codes from points table creation
#' points_str <- "case  when defaults_y_12m>=1 then -1.3344   else 0.0	end as thvs11cs_defaults_y_12m"
#'
#' #this is list of columns from char table
#' char_str <- "insolvencies	th_ind_dischargeFlag	th_ind_bankruptcyStatus_type defaults_y_12m"
#'
#' extract_devvar(points_str, char_str)
#' }
#'
#' @export
extract_devvar <- function(points_str,char_str) {
  points_strspl <- strsplit(points_str,'[^[:alnum:]_]')[[1]] %>% .[.!=''] %>% unique()
  char_strspl <- strsplit(char_str,'[^[:alnum:]_]')[[1]] %>% .[.!=''] %>% unique()
  points_strspl2 <- points_strspl[points_strspl %in% char_strspl]
  out <- paste(points_strspl2,collapse=',')
  return(out)
}

#' calculate gini from sql server table
#' return gini value, or data frame if "by" field is provided
#'
#' @examples
#' \dontrun{
#' gini('mawork_stg','myout','c090_master_scale','x_gbf_c090')
#'
#' gini('mawork_stg','myout','c360_master_scale','x_gbf_c360')
#'
#' gini(odbc='mawork_stg',tbl='nztemp_temp',score='fitted_value',gbf='gbf_temp',by='segment')
#' }
#' @export
gini <- function(odbc, tbl, score, gbf, by, filter='') {

  if (filter!='') {
    filter_string <- paste0('and ',filter)
  } else {
    filter_string <- ''
  }

  if (missing(by)) {

    conn <- RODBC::odbcConnect(odbc)

    q <- paste0('select ',score,', ',gbf,' from ',tbl,' where ',gbf," in ('G','B') and ",score,' > -900 ', filter_string)

    df <- RODBC::sqlQuery(conn, q)

    RODBC::odbcClose(conn)

    gini <- pROC::auc(df[[gbf]], df[[score]], levels = c('G', 'B')) * 2 - 1
    return(gini)

  } else {

    conn <- RODBC::odbcConnect(odbc)

    q <- paste0('select ',score,', ',gbf,', ',by,' from ',tbl,' where ',gbf," in ('G','B') and ",score,' > -900 ', filter_string)

    df <- RODBC::sqlQuery(conn, q)

    RODBC::odbcClose(conn)

    segments <- unique(df[[by]])
    ginis <- numeric()
    i <- 0
    for (segment in segments) {
      i <- i + 1
      ginis[i] <- pROC::auc(df[df[[by]]==segment,gbf], df[df[[by]]==segment,score], levels = c('G', 'B')) * 2 - 1
    }
    gini_all <- pROC::auc(df[[gbf]], df[[score]], levels = c('G', 'B')) * 2 - 1

    df_auc <- data.frame(by=segments,gini=ginis)
    df_auc <- rbind(data.frame(by='ALL',gini=gini_all),df_auc)
    return(df_auc)

  }
}
