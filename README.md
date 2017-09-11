# autofitr

autofitr uses characteristics data from table specified by parameters,
applies pre-processing rules similarly to standard scorecard development (bucketing with enough bads, monotonic trends),
fits 3 GBM models with different variables/interaction-depths (devvar 1 depth, dev+new var 1 depth, dev+new 3 depths),
then write 4 metrics tables to server

See more in package help 
```r
?autofitr::autofitr
```

todo:
1. add validation split and validation gini
2. add early stopping in gbm fit using loop and gbm.more
3. add support for Hadoop