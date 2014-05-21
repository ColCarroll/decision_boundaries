require(ggplot2)
require(rpart)
require(randomForest)
require(RWeka)
require(klaR)

dir.create('test', showWarnings=F)

get.admit.data <- function() {
  df <- read.csv("binomial.csv")
  df$admit <- factor(df$admit)
  df$rank <- factor(df$rank)
  return(list(data = df, formula=admit ~ gre + gpa, predicted.var="admit"))
}

get.state.data <- function() {
  df <- cbind(as.data.frame(state.x77), state.region)
  df <- df[df$state.region %in% c("South", "Northeast"),]
  df$state.region <- factor(df$state.region)
  return(list(data=df, formula = state.region ~ Frost + Income, predicted.var="state.region"))
}

get.iris.data <- function() {
  df <- iris
  df <- df[df$Species %in% c("versicolor", "setosa"),]
  df$Species <- factor(df$Species)
  return(list(data=df, formula = Species ~ Sepal.Width + Sepal.Length, predicted.var="Species" ))
}

get.car.data <- function() {
  df <- mtcars
  df$am <- factor(df$am)
  return(list(data=df, formula = am ~ qsec + mpg, predicted.var="am"))
}

test <- function(fn, filename, data.fn, nbins=100) {
  dset <- data.fn()
  plot.decision.boundary(fn(dset$formula, dset$data), dset$data, dset$predicted.var, attr(terms(dset$formula), 'term.labels'), filename, nbins)
}

logistic <- function(formula, data) {
  return(glm(formula, data=data, family="binomial"))
}

test.all <- function(data.fn, nbins=100) {
  dset <- data.fn()
  plot.data(dset$data, dset$predicted.var, attr(terms(dset$formula), 'term.labels'), 'plots/no_classification.png')
  test(rpart, "plots/decision_tree.png", data.fn, nbins)
  test(logistic, "plots/logistic_regression.png", data.fn, nbins)
  test(randomForest, "plots/random_forest.png", data.fn, nbins)
  test(IBk, "plots/knn.png", data.fn, nbins)
  test(NaiveBayes, "plots/naive_bayes.png", data.fn, nbins)
}

toseq <- function(some.lims, nbins=100) {
  return(seq(some.lims[1], some.lims[2], (some.lims[2] - some.lims[1]) / nbins))

}

get.grid <- function(xlims, ylims, nbins=100) {
  return(expand.grid(toseq(xlims, nbins), toseq(ylims, nbins)))
}

plot.data <- function(the.data, predicted.var, predictor.vars, filename) {
  x.lims <- extendrange(the.data[,c(predictor.vars[1])])
  y.lims <- extendrange(the.data[,c(predictor.vars[2])])
  p <- ggplot(the.data, aes_string(x=predictor.vars[1], y=predictor.vars[2], shape=predicted.var)) 
  p <- p + geom_point(size=5)
  p <- p + scale_x_continuous(lim=x.lims)
  p <- p + scale_y_continuous(lim=y.lims)
  png(filename, width=1200, height=800)
  print(p)
  dev.off()
}

plot.decision.boundary <- function(the.model, the.data, predicted.var, predictor.vars, filename, nbins=100) {
  predicted.var <- predicted.var

  x.lims <- extendrange(the.data[,c(predictor.vars[1])])
  y.lims <- extendrange(the.data[,c(predictor.vars[2])])

  df <- get.grid(x.lims, y.lims, nbins)
  names(df)[c(1,2)] <- predictor.vars[c(1,2)]

  if(identical(class(the.model), c("glm", "lm"))) {
    prob.predicts <- predict(the.model, df, type="response")
    prob.predicts <- data.frame(1 - prob.predicts, prob.predicts)
  } else{
    prob.predicts <- predict(the.model, df, type="prob")
    if(length(prob.predicts) == 2) {
      prob.predicts <- prob.predicts[[2]]
      }
  }

  class.predicts <- round(prob.predicts[,2])
  prob.predicts <- pmax(prob.predicts[,1], prob.predicts[,2])

  df <- cbind(df, class.predicts, prob.predicts) 

  names(df)[c(3,4)] <- c("class", "prob")
  df$class <- factor(df$class)

  p <- ggplot(df, aes_string(x=predictor.vars[1], y=predictor.vars[2]))
  p <- p + geom_tile(aes(fill=class, alpha=prob))
  p <- p + geom_point(data=the.data, size=5, aes_string(shape=predicted.var))

  png(filename, width=1200, height=800)
  print(p)
  dev.off()
}

