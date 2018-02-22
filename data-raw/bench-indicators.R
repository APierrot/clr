
data(gb_load)

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

## Example without any cluster
model <- clr(Y = Y_train, X = X_train)
pred_on_test <- predict(model, newX = X_test, simplify = TRUE)

## Example with clusters
model <- clr(Y = Y_train, X = X_train, clust = clust_train)
pred_on_test <- predict(model, newX = X_test, newclust = clust_test,
                        simplify = TRUE)
ow.names(pred_on_test) <- row.names(Y_test)[as.numeric(row.names(pred_on_test))]
