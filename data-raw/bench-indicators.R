
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
n <- nrow(pred_on_test) - sum(is.na(rowSums(pred_on_test)))
# 337
raw_res <- Y_test[as.numeric(rownames(pred_on_test)), ] - pred_on_test
norm_res <- raw_res / Y_test[as.numeric(rownames(pred_on_test)), ]
rmse(raw_res, 0) # 1070
mape(norm_res, 2) # 2.51

