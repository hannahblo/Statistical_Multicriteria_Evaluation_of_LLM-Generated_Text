# Statistical Multicriteria Evaluation of LLM-Generated Text

## Introduction
This anonymous repository contains R-code and data sets corresponding to the "Statistical Multicriteria Evaluation of LLM-Generated Text" article. We apply the introduced tests on a selection of WikiText and WikiNews prompts. 
The structure of the repository is as follows:
- File _setup_session.R installs all needed R-packages.
- File evaluation_wikitext_wikinews.R evaluates a slection of the WikiText and WikiNews prompts, see Section 4 of the article, Li et al. (2023) and Merity et al. (2016).
- Folder datasets contains the file evaluations_wikitext_wikinews.xlsx.
- Folder R contains all necessary functions to run evaluation_wikitext_wikinews.R.
- Folder results_wikitext_wikinews stores all the results that are produced by running evaluation_wikitext_wikinews.R


## Setup
First, install all necessary R packages by downloading and sourcing the file _setup_session.R

Second, download the evaluation_wikitext_wikinews.R file and the two folders datasets and R.
  
Finally, run evaluation_wikitext_wikinews.R (approximate 2 days). The automatically generated files are stored in the same folder as evaluation_wikitext_wikinews.R is stored.

## Explanation of all the produced plots
Running the code results in some automatically generated rds/pdf files.

The resulting files are as follows:
- #_result.rds stores the result of the empirical as well as the permutated test statistic where we compute the expectation of # minus the human completion.
- #_computation_time.rds stores the computation time to obtain #_result.rds
- #_dat_set.rds stores the used sorted data set (here group_a corresponds to the human completion and # to group_b)
- *_°_result_all.rds stores the result of the empirical as well as the permutated test statistic where we compute the expectation of * minus °
- *_°_computation_time.rds stores the time to compute *_°_result_all.rds
- proportion_below_df.rds stores the number of permutation results that are below the observed one
- fig_2.pdf and fig_3.pdf are the plots from Figure~2 in the graphik

## References:
- Xiang Lisa Li, Ari Holtzman, Daniel Fried, Percy Liang, Jason Eisner, Tatsunori Hashimoto, Luke Zettlemoyer, and Mike Lewis. Contrastive decoding: Open-ended text generation as optimization, 2023.
- Stephen Merity, Caiming Xiong, James Bradbury, and Richard Socher. Pointer sentinel mixture models, 2016.
