# Decision Boundaries

Plots decision boundaries for some common machine learning algorithms.  Also 
includes a dataset binomial.csv of admit data from 
http://www.ats.ucla.edu/stat/data/binary.csv.  

Provided data functions:
*  get.admit.data
*  get.car.data
*  get.iris.data
*  get.state.data

Required libraries:
*  ggplot2  (plotting)
*  rpart  (decision trees)
*  randomForest  (random forest)
*  RWeka  (k nearest neighbors)
*  klaR  (naive Bayes)

Example usage to create .png files in a plots subfolder:
```R
> source('decision_boundaries.R')
Loading required package: ggplot2
Loading required package: rpart
Loading required package: randomForest
randomForest 4.6-7
Type rfNews() to see new features/changes/bug fixes.
Loading required package: RWeka
Loading required package: klaR
Loading required package: MASS
> test.all(get.car.data)
null device 
          1 
```
