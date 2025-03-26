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
First, please install all necessary R-packages (can be found in _setup_session.R):
- For the computation of the linear programs, we used the R interface of gurobi optimizer, see [here](https://www.gurobi.com/) (accessed: 26.03.2025). This is a commercial
solver that offers a free academic licenses which can be found [here](https://www.gurobi.com/features/academic-named-user-license/) (accessed: 26.03.2025). To install this package, please follow the instructions there. A documentation can be found [here](https://www.gurobi.com/wp-content/plugins/hd_documentations/documentation/9.0/refman.pdf) (page 643ff) (accessed: 26.03.2025).
- Afterwards, please install all dependencies by sourcing the file _setup_session.R and source the files 

Second, download the evaluation_wikitext_wikinews.R file and the two folders datasets and R.
  
Finally, run evaluation_wikitext_wikinews.R (approximate 3 days). The automatically generated files are stored in the same folder as evaluation_wikitext_wikinews.R is stored.


## Further computation details
This code is designed to evaluate the null hypothesis described in the corresponding article. The set of algorithms or decoding strategies to be compared can be arbitrarily large, though computation time increases accordingly. The evaluation requires one cardinal and two ordinal quality metrics.  

The implementation is based on Jansen et al. (2023) and Jansen et al. (2024). For further details on the computational process, see, for example, Section 8.1 of Jansen et al. (2023).  

The worst-case complexity of the code is $\mathcal{O}(n)$. However, in practical applications, the worst-case scenario occurs infrequently. The code takes advantage of this fact to reduce computation time in typical real-world analyses.  

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
- Christoph Jansen, Georg Schollmeyer, Hannah Blocher, Julian Rodemann, Thomas Augustin. Robust statistical comparison of random variables with locally varying scale of measurement. Proceedings of the Thirty-Ninth Conference on Uncertainty in Artificial Intelligence, PMLR 216:941-952, 2023.
- Christoph Jansen, Georg Schollmeyer, Julian Rodemann, Hannah Blocher, Thomas Augustin. Statistical Multicriteria Benchmarking via the GSD-Front. Advances in Neural Information Processing Systems 37 (NeurIPS 2024), Openreview, 2024
- Xiang Lisa Li, Ari Holtzman, Daniel Fried, Percy Liang, Jason Eisner, Tatsunori Hashimoto, Luke Zettlemoyer, and Mike Lewis. Contrastive decoding: Open-ended text generation as optimization, 2023.
- Stephen Merity, Caiming Xiong, James Bradbury, and Richard Socher. Pointer sentinel mixture models, 2016.
