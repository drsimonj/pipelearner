pipelearner
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
pipelearner makes it easy to create machine learning pipelines in R.

Installation and background
---------------------------

pipelearner is currently available from github as a development package only. It can be installed by running:

``` r
# install.packages("devtools")
devtools::install_github("drsimonj/pipelearner")
```

pipelearner is built on top of [tidyverse](https://github.com/tidyverse/tidyverse) packages like [modelr](https://github.com/hadley/modelr). To harness the full power of pipelearner, you should be familiar with tidyverse tools and principles. In particular, it will help to understand the following:

-   `%>%`, the pipe operator from magrittr package.
-   tibbles and how to iterate through them with the likes of `purrr`.
-   It would also help to be familiar with `resample` objects from the [modelr](https://github.com/hadley/modelr) package.

The basics
----------

Similar to the way ggplot2 elements are layered with `+`, pipelearner involves building a pipelearner object, which is a list, with functions that can be piped into eachother with `%>%`:

-   `pipelearner()` to setup the pipelearner object
-   `learn_cvpairs()` to customize the cross-validation pairs
-   `learn_curves()` to customize the learning curves using incremental proportions of training data
-   `learn_models()` to add new learning models.

Once the pipelearner object is setup, call `learn()` to fit everything and `summary()` to extract the results as a tibble.

An important thing to note is that "fit everything" means that pieplearner fits all combinations of the specified models, sets of training data in the cross-validation pairs, with proportions defined by the learning curves.

Simple case
-----------

The following trains a single regression model for predicting `Sepal.Length` with all other variables in the iris data set.

``` r
library(pipelearner)

pl <- pipelearner(iris) %>% learn_models(lm, Sepal.Length ~ .)

pl %>% learn() %>% summary()
#> # A tibble: 1 × 10
#>   models.id cv_pairs.id train_p      fit       target model     params
#>       <chr>       <chr>   <dbl>   <list>        <chr> <chr>     <list>
#> 1         1           1       1 <S3: lm> Sepal.Length    lm <list [1]>
#> # ... with 3 more variables: train <list>, test <list>, .id <chr>
```

-   A pipelearner object is created and setup to use the iris data set with `pipelearner(iris)`. By default, this creates a single cross-validation pair by randomly selecting 80% of the data for training, and separating 20% to be used as a test set.
-   A linear regression model, predicting Sepal.Length with all other variables, is specified by piping the object to `learn_models(lm, Sepal.Length ~ .)`.
-   The model is fit to the training data by piping into `learn()`.
-   Results are extracted as a tibble with `summary()`. The tibble contains our fitted regression model under `fit`, the training data it was fit on under `train`, and a `test` data set to predict on.

Cross-validation pairs
----------------------

Cross-validation pairs can be customized with `learn_cvpairs`. The following implements k-fold cross-validation, creating five folds:

``` r
pl %>%
  learn_cvpairs(k = 5) %>% 
  learn() %>% 
  summary()
#> # A tibble: 5 × 10
#>   models.id cv_pairs.id train_p      fit       target model     params
#>       <chr>       <chr>   <dbl>   <list>        <chr> <chr>     <list>
#> 1         1           1       1 <S3: lm> Sepal.Length    lm <list [1]>
#> 2         1           2       1 <S3: lm> Sepal.Length    lm <list [1]>
#> 3         1           3       1 <S3: lm> Sepal.Length    lm <list [1]>
#> 4         1           4       1 <S3: lm> Sepal.Length    lm <list [1]>
#> 5         1           5       1 <S3: lm> Sepal.Length    lm <list [1]>
#> # ... with 3 more variables: train <list>, test <list>, .id <chr>
```

Notice the five rows where the model has been fitted to training data for each fold, represented by `cv_pairs.id`.

Learning curves
---------------

Learning curves can be customized wth `learn_curves`. The following will fit the model to three proportions of the training data - .5, .75, and 1:

``` r
pl %>% 
  learn_curves(.5, .75, 1) %>% 
  learn() %>% 
  summary()
#> # A tibble: 3 × 10
#>   models.id cv_pairs.id train_p      fit       target model     params
#>       <chr>       <chr>   <dbl>   <list>        <chr> <chr>     <list>
#> 1         1           1    0.50 <S3: lm> Sepal.Length    lm <list [1]>
#> 2         1           1    0.75 <S3: lm> Sepal.Length    lm <list [1]>
#> 3         1           1    1.00 <S3: lm> Sepal.Length    lm <list [1]>
#> # ... with 3 more variables: train <list>, test <list>, .id <chr>
```

Notice the three rows where the model has been fitted to the three proportions of the training data, represented by `train_p`.

More models
-----------

We can add many models with `learn_models()`, which, unlike the previous functions, we can use many times. For example, the following adds a decision tree to be fitted:

``` r
pl %>% 
  learn_models(c(rpart::rpart), Sepal.Length ~ .) %>% 
  learn() %>% 
  summary()
#> # A tibble: 2 × 10
#>   models.id cv_pairs.id train_p         fit       target        model
#>       <chr>       <chr>   <dbl>      <list>        <chr>        <chr>
#> 1         1           1       1    <S3: lm> Sepal.Length           lm
#> 2         2           1       1 <S3: rpart> Sepal.Length rpart::rpart
#> # ... with 4 more variables: params <list>, train <list>, test <list>,
#> #   .id <chr>
```

Notice two rows where the regression and decision tree models have been fit to the training data, represented by `models.id`.

`learn_models()` is very flexible. It will take a pipelearner object followed by a vector of model functions, a vector of formulas, and vector of any additional hyperparameters. When vectors are provided, all combinations are expanded, making it easy to do things like compare many models with the same formulas, compare many different formulas, or do grid-search.

For example, the following compares linear regression and a decision tree over different formulas:

``` r
pipelearner(iris) %>%
  learn_models(c(lm, rpart::rpart),
               c(Sepal.Length ~ Sepal.Width,
                 Sepal.Length ~ Sepal.Width + Petal.Length,
                 Sepal.Length ~ Sepal.Width + Petal.Length + Species)) %>% 
  learn() %>% 
  summary()
#> # A tibble: 6 × 10
#>   models.id cv_pairs.id train_p         fit       target        model
#>       <chr>       <chr>   <dbl>      <list>        <chr>        <chr>
#> 1         1           1       1    <S3: lm> Sepal.Length           lm
#> 2         2           1       1 <S3: rpart> Sepal.Length rpart::rpart
#> 3         3           1       1    <S3: lm> Sepal.Length           lm
#> 4         4           1       1 <S3: rpart> Sepal.Length rpart::rpart
#> 5         5           1       1    <S3: lm> Sepal.Length           lm
#> 6         6           1       1 <S3: rpart> Sepal.Length rpart::rpart
#> # ... with 4 more variables: params <list>, train <list>, test <list>,
#> #   .id <chr>
```

The following tests a regression model and grid-searches hyperparameters of a decision tree:

``` r
pipelearner(iris) %>%
  learn_models(c(lm), Sepal.Length ~ .) %>% 
  learn_models(c(rpart::rpart), Sepal.Length ~ .,
               minsplit = c(2, 20), cp = c(0.01, 0.1)) %>% 
  learn() %>% 
  summary()
#> # A tibble: 5 × 10
#>   models.id cv_pairs.id train_p         fit       target        model
#>       <chr>       <chr>   <dbl>      <list>        <chr>        <chr>
#> 1         1           1       1    <S3: lm> Sepal.Length           lm
#> 2         2           1       1 <S3: rpart> Sepal.Length rpart::rpart
#> 3         3           1       1 <S3: rpart> Sepal.Length rpart::rpart
#> 4         4           1       1 <S3: rpart> Sepal.Length rpart::rpart
#> 5         5           1       1 <S3: rpart> Sepal.Length rpart::rpart
#> # ... with 4 more variables: params <list>, train <list>, test <list>,
#> #   .id <chr>
```

Brining it all together
-----------------------

All of the functions can be combined. For example, the following will:

-   Create 100 cross-validation pairs
-   To each be fitted in proportions of .5 to 1 in increments of .1.
-   With a regression modelling all interactions, and a decision tree modelling all features.

``` r
pipelearner(iris) %>% 
  learn_cvpairs(n = 100) %>% 
  learn_curves(seq(.5, 1, by = .1)) %>% 
  learn_models(c(lm), Sepal.Width ~ .*.) %>% 
  learn_models(c(rpart::rpart), Sepal.Width ~ .) %>% 
  learn() %>% 
  summary()
#> # A tibble: 1,200 × 10
#>    models.id cv_pairs.id train_p      fit      target model     params
#>        <chr>       <chr>   <dbl>   <list>       <chr> <chr>     <list>
#> 1          1         001     0.5 <S3: lm> Sepal.Width    lm <list [1]>
#> 2          1         001     0.6 <S3: lm> Sepal.Width    lm <list [1]>
#> 3          1         001     0.7 <S3: lm> Sepal.Width    lm <list [1]>
#> 4          1         001     0.8 <S3: lm> Sepal.Width    lm <list [1]>
#> 5          1         001     0.9 <S3: lm> Sepal.Width    lm <list [1]>
#> 6          1         001     1.0 <S3: lm> Sepal.Width    lm <list [1]>
#> 7          1         002     0.5 <S3: lm> Sepal.Width    lm <list [1]>
#> 8          1         002     0.6 <S3: lm> Sepal.Width    lm <list [1]>
#> 9          1         002     0.7 <S3: lm> Sepal.Width    lm <list [1]>
#> 10         1         002     0.8 <S3: lm> Sepal.Width    lm <list [1]>
#> # ... with 1,190 more rows, and 3 more variables: train <list>,
#> #   test <list>, .id <chr>
```
