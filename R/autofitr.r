#' @title
#' autofitr Main Interface
#'
#' @description
#' Automatically fit GBM models while applying credit scoring methodology and restrictions.
#'
#' @author Mufeng Zou, \email{mufeng.zou@equifax.com}
#'
#' @param scorecard scorecard name, should match to the [scorecard] column in lookup table
#' @param server sql server name for the lookup table and output tables, default as \code{apprwidb006}
#' @param db database for the lookup table and output tables, default as \code{MAWORK_DSOL}
#' @param lookup_table table name for the lookup table and output tables, default as \code{[CORP\\Mufeng.Zou].autofitr_lookup}
#'
#' @details
#' \code{autofitr} uses characteristics data from table specified by parameters,
#' applies pre-processing rules similarly to standard scorecard development (bucketing with enough bads, monotonic trends),
#' fits 3 GBM models with different variables/interaction-depths (devvar 1 depth, dev+new var 1 depth, dev+new 3 depths),
#' then write 4 metrics tables \code{myautofitr_value}, \code{myautofitr_gini}, \code{myautofitr_ca}, \code{myautofitr_corr} into server \code{server}, database \code{db}.
#'
#' The output sql tables / list of metrics are:
#'
#' \code{myautofitr_value}: fitted value from all 3 GBM models
#'
#' \code{myautofitr_gini}: fitted Gini values from all 3 GBM models
#'
#' \code{myautofitr_ca}: CAs for "devvar" model and "dev+new var" model.
#' (CA not available for "deeper" model, which has depths of 3)
#'
#' \code{myautofitr_corr}: Correlation matrix from the "dev+new var" model,
#' including new variables newly joined the model, and all dev variables.
#'
#'
#' @return List of metrics with length 4, where contents are identical to the output sql tables.
#'
#' @examples
#' \dontrun{
#'
#' #to make results reproducible
#' set.seed(25252)
#'
#' #using default lookup in apprwidb006 (e.g. for scorecard monitoring purposes)
#' autofitr("COMMPRS")
#'
#' #if you want to use your own lookup, or on other server
#' #(to see how to create a lookup table, have a look at the default lookup table in apprwidb006)
#' autofitr("myscorecard", "myserver", "mydb", "mylookup")
#' }
#'
#' @import dplyr
#' @import gbm
#' @import RODBC
#' @import retror
#' @import tidyr
#' @export
autofitr <- function(scorecard,
                     server='apprwidb006',
                     db='MAWORK_DSOL',
                     lookup_table='DSOL_Monitoring.dbo.monitoring_autofitr_lookup'){

  message('Running autofitr for scorecard ',scorecard)

  message('\n1. Reading from SQL...')
  list_orig <- read_from_sql(scorecard=scorecard,
                             lookup_server=server,
                             lookup_db=db,
                             lookup_table=lookup_table)
  segments <- attr(list_orig,'segment')

  message('\n2. Preprocessing dataframe...')
  list_preprocessed <- lapply(list_orig, preprocess_vars, maxlevel = 100)

  message('\n3. Bucketing variables...')
  list_buckets <- lapply(list_preprocessed, buckets_ca, minbads=20, maxbuckets=20)
  
  message('\n4. Fitting models...')
  list_metrics <- mapply(fit_model, list_orig, list_buckets, segments, SIMPLIFY = FALSE,
                         MoreArgs = list(monotonic_threshold = 0.3))

  message('\n5. Merge all segments...')
  metrics_merged <- merge_segments(list_metrics, scorecard)

  message('\n6. Writting to SQL...')
  write_to(server, db, metrics_merged)
  
  saysth()

  message('\nAll done!')

  return(invisible(metrics_merged))
}
