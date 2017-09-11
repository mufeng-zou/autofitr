
#' 2. preprocess dataframe
#'
#' Assuming input dataframe has gbf column with value 'G'/'B',
#' this function will:
#' 1. change 'G'/'B' to 1/0, while 1/0 stays the same
#' 2. remove all missing and constant columns
#' 3. convert numeric columns with only 1 valid value and NA to a factor (required for GBM)
#'
#' @export
preprocess_vars <- function(df, maxlevel = 100){

  df <- df %>% select(-score)
  colnames(df) <- tolower(colnames(df))

  df2 <- df %>%
    #    filter(gbf %in% c('G','B')) %>%
    mutate(gbf=ifelse(gbf %in% c(1,0), gbf, ifelse(gbf=='G',1,0)))

  #remove all na cols
  df3 <- df2[colSums(is.na(df2))<nrow(df2)]

  #remove constant cols
  nlevel <- sapply(df3, function(x){length(unique(x))})
  validfactor <- sapply(df3, function(x){!is.factor(x) | length(levels(x)) < maxlevel})
  df4 <- df3[nlevel>1 & validfactor]

  #as with gbm function in R, it ignores variable which has 1 value and NA (which is actually 2 different value)
  #change the vars to categorical for these case
  df5 <- data.frame(lapply(df4, function(x){
    ux <- unique(x)
    if (length(ux)==2 & any(is.na(ux))) {
      x <- as.character(x)
      x[is.na(x)] <- 'NA'
      x <- as.factor(x)
      return(x)
    } else {
      return(x)
    }
  }))

  return(df5)
}


#' 3. ca buckets for dataframe
#'
#'
#' @export
buckets_ca <- function(df, minbads=20, maxbuckets=20){
  cols <- (startsWith(colnames(df),'devvar_') | startsWith(colnames(df),'newvar_')) &
    (sapply(df, function(x){length(unique(x))})>2)
  df[cols] <- data.frame(lapply(df[cols], bucket_func, gbf=df[['gbf']], minbads=minbads, maxbuckets=maxbuckets))
  return(df)
}

#' ca buckets
#' 
#' @export
bucket_func <- function(x, ...) {
  UseMethod('bucket_func')
}


#' ca buckets for 1 numeric vector
#'
#' @export
bucket_func.numeric <- function(x, gbf, minbads=20, maxbuckets=20){
  
  dx <- data.frame(x,bad=1-gbf) %>%
    group_by(x) %>%
    summarise(bad = sum(bad)) %>%
    ungroup() %>%
    arrange(x) %>%
    mutate(bucket = x) %>%
    group_by(bucket) %>%
    summarise(minx = min(x), maxx = max(x), bad = sum(bad)) %>%
    ungroup()
  
  #merge groups below minbads
  mergei <- which(dx$bad<minbads & !is.na(dx$minx))
  while ((length(mergei)>0) & (nrow(dx)>2)) {
    mergei2 <- mergei[!((mergei %% 2==0) & (((mergei+1) %in% mergei) | ((mergei-1) %in% mergei)))]
    lefti <- ifelse(mergei2-1<=0,NA,mergei2-1)
    righti <- ifelse(mergei2+1>nrow(dx),NA,mergei2+1)
    direction <- ifelse(mergei2==1,1,ifelse(((mergei2==nrow(dx)) | dx[lefti,]$bad<=dx[righti,]$bad),-1,1))
    left <- mergei2[(((mergei2<nrow(dx)) & is.na(dx[mergei2+1,]$minx)) | (direction==-1))]
    right <- mergei2[!(mergei2 %in% left)]
    if (length(right)>0) {
      dx[right+1,'minx'] <- dx[right,'minx']
      dx[right+1,'bad'] <- dx[right+1,'bad']+dx[right,'bad']
    }
    if (length(left)>0) {
      dx[left-1,'maxx'] <- dx[left,'maxx']
      dx[left-1,'bad'] <- dx[left-1,'bad']+dx[left,'bad']
    }
    dx <- dx[-c(left,right),]
    
    mergei <- which(dx$bad<minbads & !is.na(dx$minx))
  }
  
  #continue merge until nbands<maxbuckets
  while (nrow(dx)>maxbuckets) {
    nmerge <- nrow(dx)-maxbuckets
    tomerge <- dx %>% mutate(rownum=1:nrow(dx)) %>% filter(!is.na(minx)) %>% select(rownum,bad) %>% arrange(bad) %>% head(nmerge)
    mergei <- tomerge$rownum
    
    mergei2 <- mergei[!((mergei %% 2==0) & (((mergei+1) %in% mergei) | ((mergei-1) %in% mergei)))]
    lefti <- ifelse(mergei2-1<=0,NA,mergei2-1)
    righti <- ifelse(mergei2+1>nrow(dx),NA,mergei2+1)
    direction <- ifelse(mergei2==1,1,ifelse(((mergei2==nrow(dx)) | dx[lefti,]$bad<=dx[righti,]$bad),-1,1))
    left <- mergei2[(((mergei2<nrow(dx)) & is.na(dx[mergei2+1,]$minx)) | (direction==-1))]
    right <- mergei2[!(mergei2 %in% left)]
    if (length(right)>0) {
      dx[right+1,'minx'] <- dx[right,'minx']
      dx[right+1,'bad'] <- dx[right+1,'bad']+dx[right,'bad']
    }
    if (length(left)>0) {
      dx[left-1,'maxx'] <- dx[left,'maxx']
      dx[left-1,'bad'] <- dx[left-1,'bad']+dx[left,'bad']
    }
    dx <- dx[-c(left,right),]
  }
  
  newx <- x
  for (i in 1:nrow(dx)) {
    thisbucket <- (x>=dx[i,]$minx & x<=dx[i,]$maxx)
    newx[thisbucket] <- median(x[thisbucket], na.rm = T)
  }
  #convert to factor when there's only 1 valid number and NA
  ux <- unique(newx)
  if (length(ux)==2 & any(is.na(ux))) {
    newx <- as.character(newx)
    newx[is.na(newx)] <- 'NA'
    newx <- as.factor(newx)
  }
  return(newx)
}

#' ca buckets for 1 factor vector
#'
#' @export
bucket_func.factor <- function(x, gbf, minbads=20, maxbuckets=20){
  
  dx <- data.frame(x,gbf) %>%
    group_by(x) %>%
    summarise(good = sum(gbf),
              bad = sum(1-gbf),
              logodds = log(good/bad)) %>%
    ungroup() %>%
    arrange(logodds, bad) %>%
    mutate(x=as.character(x))
  
  #merge groups below minbads
  mergei <- which(dx$bad<minbads)
  while ((length(mergei)>0) & (nrow(dx)>2)) {
    mergei2 <- mergei[!((mergei %% 2==0) & (((mergei+1) %in% mergei) | ((mergei-1) %in% mergei)))]
    lefti <- ifelse(mergei2-1<=0,NA,mergei2-1)
    righti <- ifelse(mergei2+1>nrow(dx),NA,mergei2+1)
    direction <- ifelse(mergei2==1,1,ifelse(((mergei2==nrow(dx)) | dx[lefti,]$bad<=dx[righti,]$bad),-1,1))
    left <- mergei2[direction==-1]
    right <- mergei2[direction==1]
    if (length(right)>0) {
      dx[right+1,]$x <- paste0(dx[right+1,]$x,'|',dx[right,]$x)
      dx[right+1,'good'] <- dx[right+1,'good']+dx[right,'good']
      dx[right+1,'bad'] <- dx[right+1,'bad']+dx[right,'bad']
    }
    if (length(left)>0) {
      dx[left-1,]$x <- paste0(dx[left-1,]$x,'|',dx[left,]$x)
      dx[left-1,'good'] <- dx[left-1,'good']+dx[left,'good']
      dx[left-1,'bad'] <- dx[left-1,'bad']+dx[left,'bad']
    }
    dx <- dx[-c(left,right),]
    
    mergei <- which(dx$bad<minbads)
  }
  dx$logodds=log(dx$good/dx$bad)
  
  #continue merge until nbands<maxbuckets
  while (nrow(dx)>maxbuckets) {
    nmerge <- nrow(dx)-maxbuckets
    tomerge <- dx %>% mutate(rownum=1:nrow(dx)) %>% select(rownum,bad) %>% arrange(bad) %>% head(nmerge)
    mergei <- tomerge$rownum
    
    mergei2 <- mergei[!((mergei %% 2==0) & (((mergei+1) %in% mergei) | ((mergei-1) %in% mergei)))]
    lefti <- ifelse(mergei2-1<=0,NA,mergei2-1)
    righti <- ifelse(mergei2+1>nrow(dx),NA,mergei2+1)
    direction <- ifelse(mergei2==1,1,ifelse(((mergei2==nrow(dx)) | dx[lefti,]$bad<=dx[righti,]$bad),-1,1))
    left <- mergei2[direction==-1]
    right <- mergei2[direction==1]
    if (length(right)>0) {
      dx[right+1,]$x <- paste0(dx[right+1,]$x,'|',dx[right,]$x)
      dx[right+1,'good'] <- dx[right+1,'good']+dx[right,'good']
      dx[right+1,'bad'] <- dx[right+1,'bad']+dx[right,'bad']
    }
    if (length(left)>0) {
      dx[left-1,]$x <- paste0(dx[left-1,]$x,'|',dx[left,]$x)
      dx[left-1,'good'] <- dx[left-1,'good']+dx[left,'good']
      dx[left-1,'bad'] <- dx[left-1,'bad']+dx[left,'bad']
    }
    dx <- dx[-c(left,right),]
  }
  
  newlabel <- as.factor(dx$x)
  newx <- factor(rep(NA,length(x)),levels = levels(newlabel))
  for (i in 1:nrow(dx)) {
    old <- strsplit(dx[i,]$x,'|',fixed = T)[[1]]
    old <- match(old,as.character(levels(x)))
    new <- newlabel[i]
    newx[as.numeric(x) %in% old] <- newlabel[i]
  }
  return(newx)
}