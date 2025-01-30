### OpenML Data representation in plots
### load r file openml_permutation_test.R until row 112

data_openml_filte_no_cuts <- data_openml_filter

# now load row 114 to 121
data_openml_filter

saveRDS(data_openml_filte_no_cuts, "data_openml_filte_no_cuts.rds")
saveRDS(data_openml_filter, "data_openml_filter.rds")
data_openml_filte_no_cuts
data_openml_filter


### Scatterplots

cor(data_openml_filte_no_cuts$predictive.accuracy,
    data_openml_filte_no_cuts$usercpu.time.millis.training)

cor(data_openml_filte_no_cuts$predictive.accuracy,
    data_openml_filte_no_cuts$usercpu.time.millis.testing)

## accuracy vs training time
# note high accuracy is good, but high training time is bad
ggplot(data_openml_filte_no_cuts,
       aes(x = usercpu.time.millis.training, y = predictive.accuracy)) +
  geom_point(size=1)  +
  xlab("computation time on the training data") +
  ylab("predictive accuracy") +
  ggtitle("Performance of all classifiers")


ggplot(data_openml_filte_no_cuts[data_openml_filte_no_cuts$usercpu.time.millis.training < 20, ],
       aes(x = usercpu.time.millis.training, y = predictive.accuracy)) +
  geom_point(size=1)  +
  xlab("computation time on the training data (restricted to lower 20)") +
  ylab("predictive accuracy") +
  ggtitle("Performance of all classifiers")

ggplot(data_openml_filte_no_cuts[which(data_openml_filte_no_cuts$learner.name == "classif.ranger"), ],
       aes(x = usercpu.time.millis.training, y = predictive.accuracy)) +
  geom_point(size=1) +
  xlab("computation time on the training data") +
  ylab("predictive accuracy") +
  ggtitle("Performance of Random Forest")

ggplot(data_openml_filte_no_cuts[which(data_openml_filte_no_cuts$learner.name == "classif.ranger" &
                                   data_openml_filte_no_cuts$usercpu.time.millis.training < 10), ],
       aes(x = usercpu.time.millis.training, y = predictive.accuracy)) +
  geom_point(size=1) +
  xlab("computation time on the training data (restricted to lower 10)") +
  ylab("predictive accuracy")+
  ggtitle("Performance of Random Forest")

ggplot(data_openml_filte_no_cuts[which(data_openml_filte_no_cuts$learner.name == "classif.xgboost"), ],
       aes(x = usercpu.time.millis.training, y = predictive.accuracy)) +
  geom_point(size=1) +
  xlab("computation time on the training data") +
  ylab("predictive accuracy") +
  ggtitle("Performance of xGBoost")

ggplot(data_openml_filte_no_cuts[which(data_openml_filte_no_cuts$learner.name == "classif.xgboost" &
                                   data_openml_filte_no_cuts$usercpu.time.millis.training < 10), ],
       aes(x = usercpu.time.millis.training, y = predictive.accuracy)) +
  geom_point(size=1) +
  xlab("computation time on the training data (restricted to lower 10") +
  ylab("predictive accuracy") +
ggtitle("Performance of xGBoost")


## accuracy vs testing time
# note high accuracy is good, but high testing time is bad
ggplot(data_openml_filte_no_cuts,
       aes(x = usercpu.time.millis.testing, y = predictive.accuracy)) +
  geom_point(size=1) +
  xlab("computation time on the test data") +
  ylab("predictive accuracy") +
  ggtitle("Performance of all classifiers")

ggplot(data_openml_filte_no_cuts[data_openml_filte_no_cuts$usercpu.time.millis.testing < 0.5, ],
       aes(x = usercpu.time.millis.testing, y = predictive.accuracy)) +
  geom_point(size=1) +
  xlab("computation time on the test data (restricted to lower 0.5)") +
  ylab("predictive accuracy") +
  ggtitle("Performance of all classifiers")


## training time vs testing time
ggplot(data_openml_filte_no_cuts,
       aes(x = usercpu.time.millis.training, y = usercpu.time.millis.testing)) +
  geom_point(size=1)  +
  xlab("computation time on the training data") +
  ylab("computation time on test data") +
  ggtitle("Performance of all classifiers")

ggplot(data_openml_filte_no_cuts[data_openml_filte_no_cuts$usercpu.time.millis.testing < 0.5
                                 & data_openml_filte_no_cuts$usercpu.time.millis.training < 2.5, ],
       aes(x = usercpu.time.millis.training, y = usercpu.time.millis.testing)) +
  geom_point(size=1) +
  xlab("computation time on the test data (restricted to lower 0.5)") +
  ylab("computation time on test data (restricted to lower 2.5)") +
  ggtitle("Performance of all classifiers")







### Comparison of CART and xGBoost
data_cart_xgboost <- data_openml_filter[which(data_openml_filter$learner.name %in% c("classif.rpart", "classif.xgboost")), ]

comp_values <- list(rpart_all_above = 0, xgboost_all_above = 0,
                    incomparable = 0, equal_all = 0,
                    rpart_acc_above = 0, xgboost_acc_above = 0)
rpart_all_above_list <- list()
xgboost_all_above_list <- list()
incomparable_list <- list()
equal_all_list <- list()
rpart_acc_above <- list()
xgboost_acc_above <- list()
i <- 1
while (i < dim(data_cart_xgboost)[1]) {
  if (all(data_cart_xgboost[i, c(2,3,4)] > data_cart_xgboost[i + 1, c(2,3,4)])) {
    comp_values$rpart_all_above <- comp_values$rpart_all_above + 1
    rpart_all_above_list <- append(rpart_all_above_list, i)
  }
  if (all(data_cart_xgboost[i, c(2,3,4)] < data_cart_xgboost[i + 1, c(2,3,4)])) {
    comp_values$xgboost_all_above <- comp_values$xgboost_all_above + 1
    xgboost_all_above_list <- append(xgboost_all_above_list, i)
  }
  if (all(data_cart_xgboost[i, c(2,3,4)] == data_cart_xgboost[i + 1, c(2,3,4)])) {
    comp_values$equal_all <- comp_values$equal_all + 1
    equal_all_list <- append(equal_all_list, i)
  }
  if (any(data_cart_xgboost[i, c(2,3,4)] < data_cart_xgboost[i + 1, c(2,3,4)]) &&
      any(data_cart_xgboost[i, c(2,3,4)] > data_cart_xgboost[i + 1, c(2,3,4)])) {
    comp_values$incomparable <- comp_values$incomparable + 1
    incomparable_list <- append(incomparable_list, i)
  }
  if (data_cart_xgboost[i, 2] < data_cart_xgboost[i + 1, 2]) {
    comp_values$xgboost_acc_above <- comp_values$xgboost_acc_above + 1
    xgboost_acc_above <- append(xgboost_acc_above, i)
  }
  if (data_cart_xgboost[i, 2] > data_cart_xgboost[i + 1, 2]) {
    comp_values$rpart_acc_above <- comp_values$rpart_acc_above + 1
    rpart_acc_above <- append(rpart_acc_above, i)
  }
  i <- i + 2
}
comp_values
# not that at in total only 74 comparison instead of 80, see below and then it makes sense
data_cart_xgboost[c(11, 12, 15, 16, 33, 34, 45, 46, 91, 92, 135, 136), ]


## acc of xgboost and rpart when rpart is better in all
data_frame_plot <- data.frame(rpart = data_cart_xgboost[unlist(rpart_all_above_list), 2],
                              xgboost = data_cart_xgboost[unlist(rpart_all_above_list) + 1, 2])

ggplot(data_frame_plot, aes(x = predictive.accuracy.1, y = predictive.accuracy)) +
  geom_point(size=1) +
  xlab("acc of xgboost") +
  ylab("acc of rpart")

## acc of xgboost and rpart when are incomparable
data_frame_plot <- data.frame(rpart = data_cart_xgboost[unlist(incomparable_list), 2],
                              xgboost = data_cart_xgboost[unlist(incomparable_list) + 1, 2])

ggplot(data_frame_plot, aes(x = predictive.accuracy.1, y = predictive.accuracy)) +
  geom_point(size=1) +
  xlab("acc of xgboost") +
  ylab("acc of rpart")


# Comparison of CART and RF
### Comparison of CART and xGBoost
data_cart_ranger <- data_openml_filter[which(data_openml_filter$learner.name %in% c("classif.rpart", "classif.ranger")), ]

comp_values <- list(rpart_all_above = 0, ranger_all_above = 0,
                    incomparable = 0, equal_all = 0,
                    rpart_acc_above = 0, ranger_acc_above = 0)
rpart_all_above_list <- list()
ranger_all_above_list <- list()
incomparable_list <- list()
equal_all_list <- list()
rpart_acc_above <- list()
ranger_acc_above <- list()
i <- 1
while (i < dim(data_cart_ranger)[1]) {
  if (all(data_cart_ranger[i, c(2,3,4)] < data_cart_ranger[i + 1, c(2,3,4)])) {
    comp_values$rpart_all_above <- comp_values$rpart_all_above + 1
    rpart_all_above_list <- append(rpart_all_above_list, i)
  }
  if (all(data_cart_ranger[i, c(2,3,4)] > data_cart_ranger[i + 1, c(2,3,4)])) {
    comp_values$ranger_all_above <- comp_values$ranger_all_above + 1
    ranger_all_above_list <- append(ranger_all_above_list, i)
  }
  if (all(data_cart_ranger[i, c(2,3,4)] == data_cart_ranger[i + 1, c(2,3,4)])) {
    comp_values$equal_all <- comp_values$equal_all + 1
    equal_all_list <- append(equal_all_list, i)
  }
  if (any(data_cart_ranger[i, c(2,3,4)] < data_cart_ranger[i + 1, c(2,3,4)]) &&
      any(data_cart_ranger[i, c(2,3,4)] > data_cart_ranger[i + 1, c(2,3,4)])) {
    comp_values$incomparable <- comp_values$incomparable + 1
    incomparable_list <- append(incomparable_list, i)
  }
  if (data_cart_ranger[i, 2] > data_cart_ranger[i + 1, 2]) {
    comp_values$ranger_acc_above <- comp_values$ranger_acc_above + 1
    ranger_acc_above <- append(ranger_acc_above, i)
  }
  if (data_cart_ranger[i, 2] < data_cart_ranger[i + 1, 2]) {
    comp_values$rpart_acc_above <- comp_values$rpart_acc_above + 1
    rpart_acc_above <- append(rpart_acc_above, i)
  }
  i <- i + 2
}
comp_values
# sort(c(unlist(rpart_all_above_list), unlist(incomparable_list)))
# not that at in total only 77 comparison instead of 80, see below and then it makes sense
data_cart_ranger[c(5,6, 11, 12, 15, 16), ]


## acc of xgboost and rpart when rpart is better in all
data_frame_plot <- data.frame(rpart = data_cart_ranger[unlist(rpart_all_above_list), 2],
                              xgboost = data_cart_ranger[unlist(rpart_all_above_list) + 1, 2])

ggplot(data_frame_plot, aes(x = predictive.accuracy.1, y = predictive.accuracy)) +
  geom_point(size=1) +
  xlab("acc of xgboost") +
  ylab("acc of rpart")

## acc of xgboost and rpart when are incomparable
data_frame_plot <- data.frame(rpart = data_cart_ranger[unlist(incomparable_list), 2],
                              xgboost = data_cart_ranger[unlist(incomparable_list) + 1, 2])

ggplot(data_frame_plot, aes(x = predictive.accuracy.1, y = predictive.accuracy)) +
  geom_point(size=1) +
  xlab("acc of xgboost") +
  ylab("acc of rpart")



### Comparison of CART and RF
data_cart_ranger <- data_openml_filter[which(data_openml_filter$learner.name %in% c("classif.rpart", "classif.ranger")), ]

comp_values <- list(rpart_all_above = 0, ranger_all_above = 0,
                    incomparable = 0, equal_all = 0,
                    rpart_acc_above = 0, ranger_acc_above = 0)
rpart_all_above_list <- list()
ranger_all_above_list <- list()
incomparable_list <- list()
equal_all_list <- list()
rpart_acc_above <- list()
ranger_acc_above <- list()
i <- 1
while (i < dim(data_cart_ranger)[1]) {
  if (all(data_cart_ranger[i, c(2,3,4)] < data_cart_ranger[i + 1, c(2,3,4)])) {
    comp_values$rpart_all_above <- comp_values$rpart_all_above + 1
    rpart_all_above_list <- append(rpart_all_above_list, i)
  }
  if (all(data_cart_ranger[i, c(2,3,4)] > data_cart_ranger[i + 1, c(2,3,4)])) {
    comp_values$ranger_all_above <- comp_values$ranger_all_above + 1
    ranger_all_above_list <- append(ranger_all_above_list, i)
  }
  if (all(data_cart_ranger[i, c(2,3,4)] == data_cart_ranger[i + 1, c(2,3,4)])) {
    comp_values$equal_all <- comp_values$equal_all + 1
    equal_all_list <- append(equal_all_list, i)
  }
  if (any(data_cart_ranger[i, c(2,3,4)] < data_cart_ranger[i + 1, c(2,3,4)]) &&
      any(data_cart_ranger[i, c(2,3,4)] > data_cart_ranger[i + 1, c(2,3,4)])) {
    comp_values$incomparable <- comp_values$incomparable + 1
    incomparable_list <- append(incomparable_list, i)
  }
  if (data_cart_ranger[i, 2] > data_cart_ranger[i + 1, 2]) {
    comp_values$ranger_acc_above <- comp_values$ranger_acc_above + 1
    ranger_acc_above <- append(ranger_acc_above, i)
  }
  if (data_cart_ranger[i, 2] < data_cart_ranger[i + 1, 2]) {
    comp_values$rpart_acc_above <- comp_values$rpart_acc_above + 1
    rpart_acc_above <- append(rpart_acc_above, i)
  }
  i <- i + 2
}
comp_values
# sort(c(unlist(rpart_all_above_list), unlist(incomparable_list)))
# not that at in total only 77 comparison instead of 80, see below and then it makes sense
data_cart_ranger[c(5,6, 11, 12, 15, 16), ]


## acc of xgboost and rpart when rpart is better in all
data_frame_plot <- data.frame(rpart = data_cart_ranger[unlist(rpart_all_above_list), 2],
                              xgboost = data_cart_ranger[unlist(rpart_all_above_list) + 1, 2])

ggplot(data_frame_plot, aes(x = predictive.accuracy.1, y = predictive.accuracy)) +
  geom_point(size=1) +
  xlab("acc of xgboost") +
  ylab("acc of rpart")

## acc of xgboost and rpart when are incomparable
data_frame_plot <- data.frame(rpart = data_cart_ranger[unlist(incomparable_list), 2],
                              xgboost = data_cart_ranger[unlist(incomparable_list) + 1, 2])

ggplot(data_frame_plot, aes(x = predictive.accuracy.1, y = predictive.accuracy)) +
  geom_point(size=1) +
  xlab("acc of xgboost") +
  ylab("acc of rpart")


### Comparison of CART and SVM
data_cart_svm <- data_openml_filter[which(data_openml_filter$learner.name %in% c("classif.rpart", "classif.svm")), ]

comp_values <- list(rpart_all_above = 0, svm_all_above = 0,
                    incomparable = 0, equal_all = 0,
                    rpart_acc_above = 0, svm_acc_above = 0)
rpart_all_above_list <- list()
svm_all_above_list <- list()
incomparable_list <- list()
equal_all_list <- list()
rpart_acc_above <- list()
svm_acc_above <- list()
i <- 1
while (i < dim(data_cart_svm)[1]) {
  if (all(data_cart_svm[i, c(2,3,4)] > data_cart_svm[i + 1, c(2,3,4)])) {
    comp_values$rpart_all_above <- comp_values$rpart_all_above + 1
    rpart_all_above_list <- append(rpart_all_above_list, i)
  }
  if (all(data_cart_svm[i, c(2,3,4)] < data_cart_svm[i + 1, c(2,3,4)])) {
    comp_values$svm_all_above <- comp_values$svm_all_above + 1
    svm_all_above_list <- append(svm_all_above_list, i)
  }
  if (all(data_cart_svm[i, c(2,3,4)] == data_cart_svm[i + 1, c(2,3,4)])) {
    comp_values$equal_all <- comp_values$equal_all + 1
    equal_all_list <- append(equal_all_list, i)
  }
  if (any(data_cart_svm[i, c(2,3,4)] < data_cart_svm[i + 1, c(2,3,4)]) &&
      any(data_cart_svm[i, c(2,3,4)] > data_cart_svm[i + 1, c(2,3,4)])) {
    comp_values$incomparable <- comp_values$incomparable + 1
    incomparable_list <- append(incomparable_list, i)
  }
  if (data_cart_svm[i, 2] < data_cart_svm[i + 1, 2]) {
    comp_values$svm_acc_above <- comp_values$svm_acc_above + 1
    svm_acc_above <- append(svm_acc_above, i)
  }
  if (data_cart_svm[i, 2] > data_cart_svm[i + 1, 2]) {
    comp_values$rpart_acc_above <- comp_values$rpart_acc_above + 1
    rpart_acc_above <- append(rpart_acc_above, i)
  }
  i <- i + 2
}
comp_values
# not that at in total only 74 comparison instead of 80, see below and then it makes sense
sort(c(unlist(rpart_all_above_list), unlist(incomparable_list)))
data_cart_svm[c(3,4, 11, 12, 13, 14, 25, 26, 27, 28, 69, 70, 81, 82, 109, 110,
                131, 132 ), ]


## acc of xgboost and rpart when rpart is better in all
data_frame_plot <- data.frame(rpart = data_cart_svm[unlist(rpart_all_above_list), 2],
                              xgboost = data_cart_svm[unlist(rpart_all_above_list) + 1, 2])

ggplot(data_frame_plot, aes(x = predictive.accuracy.1, y = predictive.accuracy)) +
  geom_point(size=1) +
  xlab("acc of svm") +
  ylab("acc of rpart")

## acc of xgboost and rpart when are incomparable
data_frame_plot <- data.frame(rpart = data_cart_svm[unlist(incomparable_list), 2],
                              xgboost = data_cart_svm[unlist(incomparable_list) + 1, 2])

ggplot(data_frame_plot, aes(x = predictive.accuracy.1, y = predictive.accuracy)) +
  geom_point(size=1) +
  xlab("acc of svm") +
  ylab("acc of rpart")


# Comparison of CART and RF
### Comparison of CART and xGBoost
data_cart_ranger <- data_openml_filter[which(data_openml_filter$learner.name %in% c("classif.rpart", "classif.ranger")), ]

comp_values <- list(rpart_all_above = 0, ranger_all_above = 0,
                    incomparable = 0, equal_all = 0,
                    rpart_acc_above = 0, ranger_acc_above = 0)
rpart_all_above_list <- list()
ranger_all_above_list <- list()
incomparable_list <- list()
equal_all_list <- list()
rpart_acc_above <- list()
ranger_acc_above <- list()
i <- 1
while (i < dim(data_cart_ranger)[1]) {
  if (all(data_cart_ranger[i, c(2,3,4)] < data_cart_ranger[i + 1, c(2,3,4)])) {
    comp_values$rpart_all_above <- comp_values$rpart_all_above + 1
    rpart_all_above_list <- append(rpart_all_above_list, i)
  }
  if (all(data_cart_ranger[i, c(2,3,4)] > data_cart_ranger[i + 1, c(2,3,4)])) {
    comp_values$ranger_all_above <- comp_values$ranger_all_above + 1
    ranger_all_above_list <- append(ranger_all_above_list, i)
  }
  if (all(data_cart_ranger[i, c(2,3,4)] == data_cart_ranger[i + 1, c(2,3,4)])) {
    comp_values$equal_all <- comp_values$equal_all + 1
    equal_all_list <- append(equal_all_list, i)
  }
  if (any(data_cart_ranger[i, c(2,3,4)] < data_cart_ranger[i + 1, c(2,3,4)]) &&
      any(data_cart_ranger[i, c(2,3,4)] > data_cart_ranger[i + 1, c(2,3,4)])) {
    comp_values$incomparable <- comp_values$incomparable + 1
    incomparable_list <- append(incomparable_list, i)
  }
  if (data_cart_ranger[i, 2] > data_cart_ranger[i + 1, 2]) {
    comp_values$ranger_acc_above <- comp_values$ranger_acc_above + 1
    ranger_acc_above <- append(ranger_acc_above, i)
  }
  if (data_cart_ranger[i, 2] < data_cart_ranger[i + 1, 2]) {
    comp_values$rpart_acc_above <- comp_values$rpart_acc_above + 1
    rpart_acc_above <- append(rpart_acc_above, i)
  }
  i <- i + 2
}
comp_values
# sort(c(unlist(rpart_all_above_list), unlist(incomparable_list)))
# not that at in total only 77 comparison instead of 80, see below and then it makes sense
data_cart_ranger[c(5,6, 11, 12, 15, 16), ]


## acc of xgboost and rpart when rpart is better in all
data_frame_plot <- data.frame(rpart = data_cart_ranger[unlist(rpart_all_above_list), 2],
                              xgboost = data_cart_ranger[unlist(rpart_all_above_list) + 1, 2])

ggplot(data_frame_plot, aes(x = predictive.accuracy.1, y = predictive.accuracy)) +
  geom_point(size=1) +
  xlab("acc of xgboost") +
  ylab("acc of rpart")

## acc of xgboost and rpart when are incomparable
data_frame_plot <- data.frame(rpart = data_cart_ranger[unlist(incomparable_list), 2],
                              xgboost = data_cart_ranger[unlist(incomparable_list) + 1, 2])

ggplot(data_frame_plot, aes(x = predictive.accuracy.1, y = predictive.accuracy)) +
  geom_point(size=1) +
  xlab("acc of xgboost") +
  ylab("acc of rpart")
