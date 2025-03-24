# Statistical Multicriteria Evaluation of LLM-Generated Text

## Introduction
This anonymous repository contains R-code and data sets corresponding to the "Statistical Multicriteria Evaluation of LLM-Generated Text" article. We apply the introduced tests on three examples: BBOB suite, DeepOPS suite, and a benchmarking suite on multi-objective evolutionary algorithms.

The structure of the repository is as follows:
- File _setup_session.R installs all needed R-packages.
- File evaluation_wikitext_wikinews.R evaluates a slection of the WikiText and WikiNews prompts, see TODO.
- Folder datasets contains the file evaluations_wikitext_wikinews.xlsx.
- Folder R contains all necessary functions to run evaluation_wikitext_wikinews.R.
- Folder results_wikitext_wikinews stores all the results that are produced by running evaluation_wikitext_wikinews.R


## Setup
First, install all necessary R packages by downloading and sourcing the file _setup_session.R

Second, download the evaluation_wikitext_wikinews.R file and the two folders datasets and R.
  
Finally, run evaluation_wikitext_wikinews.R (approximate 2 days). The automatically generated files are stored in the same folder as evaluation_wikitext_wikinews.R is stored.

## Explanation of all the produced plots
Running the code results in some automatically generated rds/pdf files
The resulting files are as follows
