
source(file.path('data-raw', 'trick-functions.R'))

data(gb_load)
data(clust_train)
data(clust_test)

clr_load <- clrdata(x = gb_load$ENGLAND_WALES_DEMAND,
                    order_by = gb_load$TIMESTAMP,
                    support_grid = 1:48)

# data cleaning: replace zeros with NA
clr_load[rowSums((clr_load == 0) * 1) > 0, ] <- NA

Y <- clr_load[2:nrow(clr_load), ]
X <- clr_load[1:(nrow(clr_load) - 1), ]

begin_pred <- which(substr(rownames(Y), 1, 4) == '2016')[1]
Y_train <- Y[1:(begin_pred - 1), ]
X_train <- X[1:(begin_pred - 1), ]
Y_test <- Y[begin_pred:nrow(Y), ]
X_test <- X[begin_pred:nrow(X), ]

## Example with only load and without any cluster
model <- clr(Y = Y_train, X = X_train)
pred_on_train <- predict(model, simplify = TRUE)
raw_res <- Y_train[as.numeric(rownames(pred_on_train)), ] - pred_on_train
norm_res <- raw_res / Y_train[as.numeric(rownames(pred_on_train)), ]
rmse(raw_res, 0) # 1334
mape(norm_res, 2) # 3.06

pred_on_test <- predict(model, newX = X_test, simplify = TRUE)
n <- nrow(pred_on_test) - sum(is.na(rowSums(pred_on_test)))
# 362
raw_res <- Y_test[as.numeric(rownames(pred_on_test)), ] - pred_on_test
norm_res <- raw_res / Y_test[as.numeric(rownames(pred_on_test)), ]
rmse(raw_res, 0) # 1626
mape(norm_res, 2) # 4.07


## Example with only load and with clusters
model <- clr(Y = Y_train, X = X_train, clust = clust_train)

pred_on_train <- predict(model, simplify = TRUE)
n <- nrow(pred_on_train) - sum(is.na(rowSums(pred_on_train)))
# 1724
raw_res <- Y_train[as.numeric(rownames(pred_on_train)), ] - pred_on_train
norm_res <- raw_res / Y_train[as.numeric(rownames(pred_on_train)), ]
rmse(raw_res, 0) # 717
mape(norm_res, 2) # 1.55

pred_on_test <- predict(model, newX = X_test, newclust = clust_test,
                        simplify = TRUE)
n <- nrow(pred_on_test) - sum(is.na(rowSums(pred_on_test)))
# 346
raw_res <- Y_test[as.numeric(rownames(pred_on_test)), ] - pred_on_test
norm_res <- raw_res / Y_test[as.numeric(rownames(pred_on_test)), ]
rmse(raw_res, 0) # 1152
mape(norm_res, 2) # 2.74


## EXAMPLE WITH TEMPERATURE ----------------------------------------------------
clr_temp <- clrdata(x = gb_load$TEMPERATURE,
                    order_by = gb_load$TIMESTAMP,
                    support_grid = 1:48)

# data cleaning: NA if too missing values in TEMPERATURE
MV <- gb_load[gb_load$SETTLEMENT_PERIOD == 1, 'MV']
clr_temp[MV >= 0.6, ] <- NA

X <- cbind(clr_load[1:(nrow(clr_load) - 1), ],
           clr_temp[row.names(Y), ])
X_train <- X[1:(begin_pred - 1), ]
X_test <- X[begin_pred:nrow(X), ]

# No clusters
model <- clr(Y = Y_train, X = X_train)
pred_on_test <- predict(model, newX = X_test, simplify = TRUE)
n <- nrow(pred_on_test) - sum(is.na(rowSums(pred_on_test)))
# 352
raw_res <- Y_test[as.numeric(rownames(pred_on_test)), ] - pred_on_test
norm_res <- raw_res / Y_test[as.numeric(rownames(pred_on_test)), ]
rmse(raw_res, 0) # 1591
mape(norm_res, 2) # 3.94

# With clusters
model <- clr(Y = Y_train, X = X_train, clust = clust_train)
pred_on_test <- predict(model, newX = X_test, newclust = clust_test,
                        simplify = TRUE)
raw_res <- Y_test[as.numeric(rownames(pred_on_test)), ] - pred_on_test
norm_res <- raw_res / Y_test[as.numeric(rownames(pred_on_test)), ]
rmse(raw_res, 0) # 1070
mape(norm_res, 2) # 2.51
n <- nrow(raw_res) - sum(is.na(rowSums(raw_res)))
# 335


## EXAMPLE OF CORRECTING MEAN
pred_list <- predict(model, newX = X_test, newclust = clust_test)

# FF = 1
new_pred <- vector('list', length(pred_list))
for (c in 1:length(pred_list)){
  new_pred[[c]] <- pred_list[[c]] - matrix(model[[c]]$Y_mean,
                                           nrow = nrow(pred_list[[c]]),
                                           ncol = ncol(pred_list[[c]]),
                                           byrow = TRUE)
  newY <- rbind(Y_train[model[[c]]$idx, ],
                Y_test[clust_test[[c]], ])
  n <- nrow(Y_train[model[[c]]$idx, ])

  for (i in 1:nrow(new_pred[[c]])) {
    new_mean <- colMeans(newY[1:(n + i - 1), ], na.rm = TRUE)
    new_pred[[c]][i, ] <- new_pred[[c]][i, ] + new_mean
  }
  print(c)
}

new_pred <- do.call(rbind, new_pred)
new_pred <- new_pred[order(as.numeric(row.names(new_pred))), ]

raw_res <- Y_test[as.numeric(rownames(new_pred)), ] - new_pred
norm_res <- raw_res / Y_test[as.numeric(rownames(new_pred)), ]
rmse(raw_res, 0) # 1061
mape(norm_res, 2) # 2.55

# ...


## EXAMPLE OF REFITTING BEFORE EACH FORECAST
# clust computed in prepare-data.R
online_pred <- matrix(NA,
                      nrow = length(begin_pred:nrow(Y)),
                      ncol = ncol(Y),
                      dimnames = list(begin_pred:nrow(Y),
                                      colnames(Y)))

for (i in begin_pred:nrow(Y)) {

  c <- which(unlist(lapply(clust, function(x) i %in% x)))

  if (sum(c) > 0) {
    temp_clust <- clust[[c]][clust[[c]] < i]
    model <- clr(Y = Y[1:(i-1), ], X = X[1:(i-1), ],
                 clust = list(temp_clust))
    pred <- predict(model, newX = X, newclust = list(clust[[c]]),
                    simplify = TRUE)
    online_pred[paste(i), ] <- pred[paste(i), ]
  }

}

raw_res <- Y_test - online_pred
norm_res <- raw_res / Y_test
rmse(raw_res, 0) # 1051
mape(norm_res, 2) # 2.47
# fitting window could be optimized


