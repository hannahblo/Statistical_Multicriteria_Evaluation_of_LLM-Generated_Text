# This file corresponds to the openml analysis





################################################################################
# Session Settings
################################################################################
library(purrr)# for detect_index
library(dplyr) # for group_by
library(slam) # simple triplet matrix
library(gurobi) # solving LP
library(readr)  # data preparation
library(forcats) # data preparation
library(ggplot2) # visualization
library(reshape2) # visualization
library(tidyverse) # data wrangling
library(ggridges) # visualization
library(latex2exp) # for gamma (and epsilon) symbols
library(RColorBrewer) # color palettes
library(rcartocolor) # color gradients
library(ggthemes)#
library(readxl)

source("R/constraints_r1_r2.R") # contains the functions compute_constraints...
source("R/sample_permutation_test.R") # permutation test, sample etc
source("R/plotting_permutationtest.R") # plot function
source("R/test_two_items.R") # main function summarizing the computation

################################################################################
# Select the performance measures to compare
# Select comparison between what kind of strategies
################################################################################
# one cardinal
cardinal_1 <- "Q_Text"

# two ordinal
ordinal_1 <- "Evaluation_Julian"
ordinal_2 <- "Evaluation_Christoph"

# comparison
strategy_interest <- "Human_Human"
strategy_comparison <- list("Qwen 2_topk (50)",
                            "Qwen 2_CS (('0.6', '10'))",
                            "Qwen 2_beam (5)",
                            "Qwen 2_temp (0.9)",
                            "Qwen 2_topp (0.95)")



################################################################################
# Prepare Data Set
################################################################################

# Load the data set
data_all <- read_excel("datasets/evaluations_wikitext_wikinews.xlsx")
data_all <- data_all[, c(2,3,4,5,11)]
colnames(data_all) <- c( "Model", "Method", "ordinal_1", "ordinal_2", "cardinal_1")
data_all[["strategy"]] <- with(data_all, paste(Model, Method, sep = "_"))

data_prepared <- data_all %>%
  dplyr::select(c(cardinal_1, ordinal_1, ordinal_2, "strategy"))

################################################################################
# Conducting the permutation test and plotting the results
################################################################################

# strategy <- strategy_comparison[[1]]

for (strategy in strategy_comparison) {

  data_inner <- data_prepared

  # Now we select two classifiers and compare them
  # here we choose classif.rpart and classif.svm
  data_inner <- data_inner[data_inner$strategy %in%
                             c(strategy_interest, strategy), ]


  # now, we need to convert the data_final_selected into a dataframe with columns:
  # ordinal_1, ordinal_2, numeric, count_group_a, count_group_b, count_all, ID

  # Step 1: Converting the variables of interest into numeric and order modes
  data_inner[["cardinal_1"]] <- as.numeric(as.character(data_inner[["cardinal_1"]]))
  data_inner[["ordinal_1"]] <- as.ordered(as.character(data_inner[["ordinal_1"]]))
  data_inner[["ordinal_2"]] <- as.ordered(as.character(data_inner[["ordinal_2"]]))


  # Step 2: duplication handling
  data_count <- data_inner %>% group_by_all() %>% count()

  data_group_1 <- data_count[which(data_count$strategy == strategy_interest),
                             c(1, 2, 3, 5)]
  data_group_1 <- matrix(as.numeric(as.matrix(data_group_1)), ncol = 4)
  colnames(data_group_1) <- c("numeric", "ordinal_1", "ordinal_2",
                              "count_group_a")


  data_group_2 <- data_count[which(data_count$strategy == strategy),
                                 c(1, 2, 3, 5)]
  data_group_2 <- matrix(as.numeric(as.matrix(data_group_2)), ncol = 4)
  colnames(data_group_2) <-  c("numeric", "ordinal_1", "ordinal_2",
                               "count_group_b")


  dat_final <- merge(x = data_group_1, y = data_group_2,
                     by = c("ordinal_1", "ordinal_2", "numeric"),
                     all.x = TRUE, all.y = TRUE)
  dat_final[is.na(dat_final)] <- 0
  dat_final$count_all <- dat_final$count_group_a + dat_final$count_group_b
  dat_final$ID <- seq(1:dim(dat_final)[1])


  # View(dat_final)
  # dim(dat_final)
  # min(dat_final$numeric)
  # max(dat_final$numeric)

  index_max <- which(dat_final$numeric == max(dat_final$numeric))
  # dat_final[index_max, ]
  index_min <- which(dat_final$numeric == min(dat_final$numeric))
  # dat_final[index_min, ]

  # Add minimal and maximal at the bottom of the matrix

  # ATTENTION: It is very important for the following analysis that the
  # input at the second largest row is the minimal value and the largest row
  # represents the maximal value
  dat_final[dim(dat_final)[1] + 1, ] <- c(min(dat_final$ordinal_1),
                                          min(dat_final$ordinal_2),
                                          dat_final[index_min[1], 3],
                                          0, 0, 0,
                                          max(dat_final$ID) + 1)
  dat_final[dim(dat_final)[1] + 1, ] <- c(max(dat_final$ordinal_1),
                                          max(dat_final$ordinal_2),
                                          dat_final[index_max[1], 3],
                                          0, 0, 0,
                                          max(dat_final$ID) + 1)


  # Note that we can have now the problem that these two added elements are not
  # allowed to occur already in the data, thus we check this and eventually delete
  # this row
  for (i in seq(1, length(index_min))) {
    if (all((dat_final[dim(dat_final)[1] - 1, ] == dat_final[index_min[i], ])[c(1,2,3)])) {
      dat_final[dim(dat_final)[1] - 1, ] <- dat_final[index_min[i], ]
      dat_final <- dat_final[-c(index_min[i]), ]
      dat_final$ID <- seq(1:dim(dat_final)[1])

      # We have to update index_max as now the data frame changed
      # note that we want to compare to the last row and therefore we have
      # to delete this one in index_max
      index_max <- which(dat_final$numeric == max(dat_final$numeric))
      index_max <- index_max[-length(index_max)]
    }
  }
  for (i in seq(1, length(index_max))) {
    if (all((dat_final[dim(dat_final)[1], ] == dat_final[index_max[i], ])[c(1,2,3)])) {
      dat_final[dim(dat_final)[1], ] <- dat_final[c(index_max[i]), ]
      dat_final <- dat_final[-c(index_max[i]), ]
      dat_final$ID <- seq(1:dim(dat_final)[1])
    }
  }


  dat_set <- dat_final
  saveRDS(dat_set, paste0(strategy, "_dat_set.rds"))
  start_time <- Sys.time()
  result_inner <- test_two_items(dat_set,
                                 iteration_number = 1000,
                                 seed_number = 875,
                                 eps_0 = 0,
                                 eps_1 = 0.25,
                                 eps_2 = 0.5,
                                 eps_3 = 0.75,
                                 eps_4 = 1,
                                 mc.cores = 1,
                                 reverse = FALSE)
  total_time <- Sys.time() - start_time

  saveRDS(result_inner, paste0(strategy, "_result.rds"))
  saveRDS(total_time, paste0(strategy, "_computation_time.rds"))
}




# computing the test statistic values
all_eps_values <- c("result_eps_0", "result_eps_1", "result_eps_2",
                    "result_eps_3", "result_eps_4")

proportion_below_df <- as.data.frame(matrix(rep(0, 5 * 5), nrow = 5, ncol = 5),
                                     row.names = all_eps_values)
colnames(proportion_below_df) <- unlist(strategy_comparison)
for (strategy in strategy_comparison) {
  result_classifier <- readRDS(paste0(strategy, "_result.rds"))

  for (eps_value in all_eps_values) {
    base_value <- result_classifier$d_observed[[eps_value]]
    proportion_below_df[eps_value, strategy] <-
      sum(result_classifier$permutation_test[eps_value, ] < base_value)
  }
}

saveRDS(proportion_below_df, "proportion_below_df.rds")


################################################################################
# Result and Plotting
################################################################################
# plotting the test results (of the pairwise comparisons) in the paper

results_plots = list()

strategy_name <-  c("Qwen2_topk_50", "Qwen2_CS_0.6_10", "Qwen2_beam_5", "Qwen2_temp_0.9", "Qwen2_topp_0.95")
i <- 1
for (strategy in strategy_comparison) {
  # if(classifier == "classif.xgboost")
  #   debugonce(plotting_permutationtest)s
  inner_result <- readRDS(paste0(strategy, "_result.rds"))
  results_plots[[strategy_name[i]]] = inner_result
  i <- i + 1
}

plotting_permutationtest_wiki(results_plots)










################################################################################
# Constructing the graph representing all pairwise comparisons at once
# therfore we have to compute the reverse
# (switching classifier and classifier_of_interest
# position) observed minimal difference
################################################################################
strategy_all <- list("Human_Human",
                        "Qwen 2_topk (50)",
                        "Qwen 2_CS (('0.6', '10'))",
                        "Qwen 2_beam (5)",
                        "Qwen 2_temp (0.9)",
                        "Qwen 2_topp (0.95)")

for (k in seq(1, length(strategy_all))) {
  for (m in seq(1, length(strategy_all))[-k]) {

    strategy <- strategy_all[m]
    strategy_of_interest <- strategy_all[k]


    data_inner <- data_prepared

    # Now we select two classifiers and compare them
    # here we choose classif.rpart and classif.svm
    data_inner <- data_inner[data_inner$strategy %in%
                               c(strategy_interest, strategy), ]


    # now, we need to convert the data_final_selected into a dataframe with columns:
    # ordinal_1, ordinal_2, numeric, count_group_a, count_group_b, count_all, ID

    # Step 1: Converting the variables of interest into numeric and order modes
    data_inner[["cardinal_1"]] <- as.numeric(as.character(data_inner[["cardinal_1"]]))
    data_inner[["ordinal_1"]] <- as.ordered(as.character(data_inner[["ordinal_1"]]))
    data_inner[["ordinal_2"]] <- as.ordered(as.character(data_inner[["ordinal_2"]]))


    # Step 2: duplication handling
    data_count <- data_inner %>% group_by_all() %>% count()


    data_group_1 <- data_count[which(data_count$strategy == strategy),
                                   c(1, 2, 3, 5)]
    data_group_1 <- matrix(as.numeric(as.matrix(data_group_1)), ncol = 4)
    colnames(data_group_1) <-  c("numeric", "ordinal_1", "ordinal_2", "count_group_a")


    data_group_2 <- data_count[which(data_count$strategy == strategy_of_interest),
                                   c(1, 2, 3, 5)]
    data_group_2 <- matrix(as.numeric(as.matrix(data_group_2)), ncol = 4)
    colnames(data_group_2) <-  c("numeric", "ordinal_1", "ordinal_2", "count_group_b")

    dat_final <- merge(x = data_group_1, y = data_group_2,
                       by = c("ordinal_1", "ordinal_2", "numeric"),
                       all.x = TRUE, all.y = TRUE)
    dat_final[is.na(dat_final)] <- 0
    dat_final$count_all <- dat_final$count_group_a + dat_final$count_group_b
    dat_final$ID <- seq(1:dim(dat_final)[1])


    # View(dat_final)
    # dim(dat_final)
    # min(dat_final$numeric)
    # max(dat_final$numeric)

    index_max <- which(dat_final$numeric == max(dat_final$numeric))
    # dat_final[index_max, ]
    index_min <- which(dat_final$numeric == min(dat_final$numeric))
    # dat_final[index_min, ]

    # Add minimal and maximal at the bottom of the matrix

    # ATTENTION: It is very important for the following analysis that the
    # the input at the second largest row is the minimal value and the largest row
    # represents the maximal value
    dat_final[dim(dat_final)[1] + 1, ] <- c(min(dat_final$ordinal_1),
                                            min(dat_final$ordinal_2),
                                            dat_final[index_min[1], 3],
                                            0, 0, 0,
                                            max(dat_final$ID) + 1)
    dat_final[dim(dat_final)[1] + 1, ] <- c(max(dat_final$ordinal_1),
                                            max(dat_final$ordinal_2),
                                            dat_final[index_max[1], 3],
                                            0, 0, 0,
                                            max(dat_final$ID) + 1)


    # Note that we can have now the problem that these two added elements are not
    # allowed to occur already in the data, thus we check this and eventually delete
    # this row
    for (i in seq(1, length(index_min))) {
      if (all((dat_final[dim(dat_final)[1] - 1, ] == dat_final[index_min[i], ])[c(1,2,3)])) {
        dat_final[dim(dat_final)[1] - 1, ] <- dat_final[index_min[i], ]
        dat_final <- dat_final[-c(index_min[i]), ]
        dat_final$ID <- seq(1:dim(dat_final)[1])

        # We have to update index_max as now the data frame changed
        # note that we want to compare to the last row and therefore we have
        # to delete this one in index_max
        index_max <- which(dat_final$numeric == max(dat_final$numeric))
        index_max <- index_max[-length(index_max)]
      }
    }
    for (i in seq(1, length(index_max))) {
      if (all((dat_final[dim(dat_final)[1], ] == dat_final[index_max[i], ])[c(1,2,3)])) {
        dat_final[dim(dat_final)[1], ] <- dat_final[c(index_max[i]), ]
        dat_final <- dat_final[-c(index_max[i]), ]
        dat_final$ID <- seq(1:dim(dat_final)[1])
      }
    }


    dat_set <- dat_final
    start_time <- Sys.time()
    result_inner <- test_two_items(dat_set, iteration_number = 1)

    total_time <- Sys.time() - start_time

    saveRDS(result_inner, paste0(strategy, "-", strategy_of_interest, "_result_all.rds"))
    saveRDS(total_time, paste0(strategy, "-", strategy_of_interest, "_computation_time_all.rds"))
  }
}



df_eps_0 <- as.data.frame(matrix(rep(NA, length(strategy_all) * length(strategy_all)),
                                 nrow = length(strategy_all)))
colnames(df_eps_0) <- rownames(df_eps_0) <- strategy_all

for (k in seq(1, length(strategy_all))) {
  for (m in seq(1, length(strategy_all))[-k]) {
    strategy <- strategy_all[k]
    strategy_of_interest <- strategy_all[m]

    result_inner <- readRDS(paste0(strategy, "-", strategy_of_interest, "_result_all.rds"))
    df_eps_0[unlist(strategy), unlist(strategy_of_interest)] <- result_inner$d_observed$result_eps_0

  }
}

# df_eps_p explanation:
# an entry at with row x (correpsonding to algorithm x) and column y (corresponding to algorithm y)
# gives us an objectiv where algorithm x is included with minus in objective
# and algorihtm y is included with plus in objectiv
# all in all row gives - ; and column gives +
# With this we get that here the first row corresponds to the already computed
# comparisons needed for evaluating the test



# The Hasse graph of the empirical GSD relation for the OpenML datasets was created
# by first computing the value d_{80}(C,C') for all distinct pairs (C,C')  from the
# set {SVM, RF, CART, LR, GLMNet, xGBoost, kNN } and then drawing a top-down edge
# from C to C', whenever d_{80}(C,C') >= 0. The resulting figure was created manually.


