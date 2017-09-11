#' fit 3 gbm models
#'
#' @export
fit_model <- function(df_orig, df_buckets, segment, monotonic_threshold = 0.3){
  
  message('\nFitting models for segment "',segment,'"...')
  
  if (suppressWarnings(any(!is.na(df_orig$score) & df_orig$score>0))) {
    production_fitted <- df_orig$score
    production_gini <- pROC::auc(df_buckets$gbf,df_orig$score) * 2 - 1
    
    message('production gini:')
    message(round(production_gini*100,1))
  } else {
    production_fitted <- NA
    production_gini <- NA
  }
  
  #devvar
  gbfcol <- which(colnames(df_buckets) == 'gbf')
  devcols <- which(startsWith(colnames(df_buckets),'devvar_'))
  newcols <- which(startsWith(colnames(df_buckets),'newvar_') & sapply(df_buckets,function(x){length(unique(x))})>1)

  #monotonic trend
  mono <- rep(0, ncol(df_buckets))
  corr <- sapply(df_buckets, function(var,gbf){
    if (!is.numeric(var)) return(NA)
    a <- data.frame(var=var,gbf=gbf) %>%
      group_by(var) %>%
      summarise(logodds = log(sum(gbf==1)/sum(gbf==0))) %>%
      ungroup()
    corr <- cor(a$var,a$logodds,use='na.or.complete')
    return(corr)
  },gbf=df_buckets$gbf)
  mono[devcols] <- ifelse(corr[devcols]>0,1,ifelse(corr[devcols]<0,-1,0))
  mono[newcols] <- ifelse(corr[newcols]>monotonic_threshold,1,ifelse(corr[newcols]< -monotonic_threshold,-1,0))
  mono[is.na(mono)] <- 0

  #devvar
  message('Fitting devvar models...')

  devvar_model <- gbm(gbf~.,
                      distribution = 'bernoulli',
                      data = df_buckets[c(gbfcol,devcols)],
                      var.monotone = mono[devcols],
                      n.trees = 2000,
                      interaction.depth = 1,
                      shrinkage = 0.01,
                      bag.fraction = 0.5,
                      cv.folds = 4,
                      keep.data = F
  )
  
  best.iter <- gbm.perf(devvar_model, method="cv", plot.it = F)
  oddsp <- predict.gbm(devvar_model,df_buckets[c(gbfcol,devcols)],n.trees = best.iter)
  yp <- 1/(1/exp(oddsp)+1)
  devvar_gini <- pROC::auc(df_buckets$gbf,yp) * 2 - 1
  message('devvar gini:')
  message(round(devvar_gini*100,1))

  devvar_fitted <- oddsp
  df_ca_devvar <- gbm_ca(df_orig, df_buckets[c(gbfcol,devcols)], devvar_model)

  #newvar
  if (length(newcols)>0) {
    message('Fitting devar+newvar models...')

    devnewvar_model <- gbm(gbf~.,
                           distribution = 'bernoulli',
                           data = df_buckets[c(gbfcol,devcols,newcols)],
                           var.monotone = mono[c(devcols,newcols)],
                           n.trees = 2000,
                           interaction.depth = 1,
                           shrinkage = 0.01,
                           bag.fraction = 0.5,
                           cv.folds = 4,
                           keep.data = F
    )

    best.iter <- gbm.perf(devnewvar_model, method="cv", plot.it = F)
    oddsp <- predict.gbm(devnewvar_model,df_buckets[c(gbfcol,devcols,newcols)],n.trees = best.iter)
    yp <- 1/(1/exp(oddsp)+1)
    devnewvar_gini <- pROC::auc(df_buckets$gbf,yp) * 2 - 1
    message('devvar+newvar gini:')
    message(round(devnewvar_gini*100,1))

    devnewvar_fitted <- oddsp
    df_ca_devnewvar <- gbm_ca(df_orig, df_buckets[c(gbfcol,devcols,newcols)], devnewvar_model)
  } else {
    devnewvar_fitted <- rep(NA,nrow(df_buckets))
    devnewvar_gini <- NA
    df_ca_devnewvar <- data.frame(NULL)
  }

  #allow deeper interaction
  message('Fitting deeper models...')

  deeper_model <- gbm(gbf~.,
                      distribution = 'bernoulli',
                      data = df_buckets[c(gbfcol,devcols,newcols)],
                      var.monotone = mono[c(devcols,newcols)],
                      n.trees = 1000,
                      interaction.depth = 3,
                      shrinkage = 0.01,
                      bag.fraction = 0.5,
                      cv.folds = 4,
                      keep.data = F
  )

  best.iter <- gbm.perf(deeper_model, method="cv", plot.it = F)
  oddsp <- predict.gbm(deeper_model,df_buckets[c(gbfcol,devcols,newcols)],n.trees = best.iter)
  yp <- 1/(1/exp(oddsp)+1)
  deeper_model_gini <- pROC::auc(df_buckets$gbf,yp) * 2 - 1
  message('deeper_model gini:')
  message(round(deeper_model_gini*100,1))

  deeper_fitted <- oddsp

  #merge all results

  df_fitted <- data.frame(segment=segment,
                          user_ref=df_buckets$user_ref,
                          gbf=df_buckets$gbf,
                          gbm_devvar=devvar_fitted,
                          gbm_devnewvar=devnewvar_fitted,
                          gbm_deeper=deeper_fitted,
                          production=production_fitted) %>%
    tidyr::gather('model','fitted_value',4:7) %>%
    filter(!is.na(fitted_value))

  df_gini <- data.frame(segment=segment,
                        gbm_devvar=devvar_gini,
                        gbm_devnewvar=devnewvar_gini,
                        gbm_deeper=deeper_model_gini,
                        production=production_gini) %>%
    tidyr::gather('model','gini',2:5) %>%
    filter(!is.na(gini))

  if (length(newcols)>0) {
    df_ca_all <- rbind(data.frame(segment=segment, model='gbm_devvar',df_ca_devvar),
                       data.frame(segment=segment, model='gbm_devvar_newvar',df_ca_devnewvar))
  } else {
    df_ca_all <- data.frame(segment=segment, model='gbm_devvar',df_ca_devvar)
  }

  #correlation matrix, between new added newvar and all devvars
  #mark if devvar are presented in new model
  if (length(newcols)>0) {
    df_corr_devvar <- df_buckets[devcols]
    df_corr_devvar <- df_corr_devvar[sapply(df_corr_devvar,is.numeric)]
    df_corr_newvar <- df_buckets[newcols]
    df_corr_newvar <- df_corr_newvar[(colnames(df_corr_newvar) %in% df_ca_devnewvar$field) & sapply(df_corr_newvar,is.numeric)]
    df_corr <- expand.grid(segment=segment, newvar=colnames(df_corr_newvar),devvar=colnames(df_corr_devvar), corr=NA,stringsAsFactors = F)
    if (nrow(df_corr)>0){
      for (i in 1:nrow(df_corr)){
        suppressWarnings(df_corr$corr[i] <- cor(df_corr_newvar[df_corr$newvar[i]],df_corr_devvar[df_corr$devvar[i]],use='na.or.complete')[1])
      }
    }
    df_corr <- df_corr[order(abs(df_corr$corr),decreasing = T),] %>%
      filter(!is.na(corr)) %>%
      mutate(devvar_in_refit=1*(devvar %in% df_ca_devnewvar$field))
  } else {
    df_corr <- data.frame(segment=segment,
                          newvar=NA_character_,
                          devvar=NA_character_,
                          corr=NA_real_,
                          devvar_in_refit=NA_integer_) %>% head(0)
  }

  metrics <- list(df_fitted, df_gini, df_ca_all, df_corr)

  return(metrics)
}

#' create ca from 1 depth level gbm model
#' @export
gbm_ca <- function(df_orig, df_input, model) {
  #calculate all df_points
  best.iter <- gbm.perf(model,method="cv", plot.it = F)
  df_points <- data.frame(matrix(0,nrow(df_input),length(model$var.names)))
  colnames(df_points) <- model$var.names
  for (treei in 1:best.iter) {
    tree <- pretty.gbm.tree(model,treei)
    vari <- tree$SplitVar[1] + 1       #adjust as this is 0 based index
    varname <- model$var.names[vari]
    if(model$var.type[vari]==0) {
      #continuous
      #df_points[,vari] <- df_points[,vari] + tree$Prediction[1]
      df_points[!is.na(df_input[[varname]]) & (df_input[[varname]]<=tree$SplitCodePred[1]),vari] <- df_points[!is.na(df_input[[varname]]) & (df_input[[varname]]<=tree$SplitCodePred[1]),vari] + tree$Prediction[2]
      df_points[!is.na(df_input[[varname]]) & (df_input[[varname]]>tree$SplitCodePred[1]),vari] <- df_points[!is.na(df_input[[varname]]) & (df_input[[varname]]>tree$SplitCodePred[1]),vari] + tree$Prediction[3]
      df_points[is.na(df_input[[varname]]),vari] <- df_points[is.na(df_input[varname]),vari] + tree$Prediction[4]
    } else if (model$var.type[vari]>=2) {
      #categorical
      varlev <- match(df_input[[varname]],model$var.levels[[vari]])
      splits <- model$c.split[[tree$SplitCodePred[1] + 1]]  #adjust as this is 0 based index
      direction <- splits[varlev]
      df_points[,vari] <- df_points[,vari] +
        #tree$Prediction[1] +
        ifelse(direction==-1,tree$Prediction[2],ifelse(direction==1,tree$Prediction[3],tree$Prediction[4]))
    }
  }

  varimp <- summary(model,n.trees = best.iter, plotit = F) %>% filter(rel.inf>0)
  varimp$var <- as.character(varimp$var)
  #df_points <- df_points[colnames(df_points) %in% varimp$var]
  df_points <- round(df_points,4)
  df_orig2 <- df_orig[match(colnames(df_points),tolower(colnames(df_orig)))]
  colnames(df_orig2) <- tolower(colnames(df_orig2))

  list_ca <- mapply(create_ca,val=df_orig2,points=df_points,field=colnames(df_points),MoreArgs=list(gbf=df_input$gbf),SIMPLIFY = F)
  df_ca <- bind_rows(list_ca) %>%
    left_join(varimp,by = c('field'='var')) %>%
    mutate(relinf = round(rel.inf,3)) %>%
    arrange(-relinf,field,points) %>%
    mutate(charorder = match(field,unique(field))) %>%
    select(charorder,field,points,bucket,totalGB,goods,bads,relinf)

  return(df_ca)
}

#'create ca for 1 variable
#' @export
create_ca <- function(val,points,field,gbf) {
  #create ca for 1 variable
  df <- data.frame(val=val,points=points,gbf=gbf)
  if (is.numeric(val)) {
    ca <- df %>%
      group_by(points) %>%
      summarise(minval=round(min(val,na.rm=T),3),
                maxval=round(max(val,na.rm=T),3),
                totalGB=n(),
                goods=sum(gbf),
                bads=n()-sum(gbf)
      ) %>%
      ungroup() %>%
      mutate(field=field,
             bucket = ifelse(is.na(minval),'NA',ifelse(minval==maxval,as.character(minval),paste(minval,'to',maxval)))) %>%
      arrange(points) %>%
      select(field,points,bucket,totalGB,goods,bads)
  } else {
    ca <- df %>%
      group_by(points) %>%
      summarise(bucket=paste0(paste0('"',sort(unique(val)),'"'), collapse = " | "),
                totalGB=n(),
                goods=sum(gbf),
                bads=n()-sum(gbf)
      ) %>%
      ungroup() %>%
      mutate(field=field,
             bucket = ifelse(bucket=='','NA',bucket)) %>%
      arrange(points) %>%
      select(field,points,bucket,totalGB,goods,bads)
  }
  return(ca)
}
