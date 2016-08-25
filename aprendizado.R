# devtools::install_github("topepo/caret/pkg/caret") 
# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")

# library(devtools)
# devtools::install_github("imbs-hl/ranger", subdir="ranger-r-package/ranger")
# remove.packages("ranger")
# install.packages("ranger")

# install.packages("GGally")
# install.packages("dummies")

library(GGally)
library(plyr)
library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(monomvn)
library(Amelia)
library(L1pack)
library(combinat)
library(knitr)
library(corrplot)
library(RRF)
library(ranger)
library(car)
library(xgboost)
library(dummies)

setwd("/Users/ema/Documents/empregos/vivareal")
train_all <- read.csv("train.csv")
target_id <- function () {
  which(colnames(train_set) == "target")
}

# Remove id column and possibly defective features (having only one unique value or too many unique values)
remove_id <- c("id", "feature_1", "feature_2", "feature_3", "feature_13", "feature_17")
train_all[, remove_id] <- NULL
set.seed(1)
sample_size <- round(0.2 * nrow(train_all))
valid_ind <- sample(1:nrow(train_all), sample_size)
valid_set <- train_all[valid_ind, ]
train_set <- train_all[-valid_ind, ]

# Get factor and numeric variables
factor_variables  <- which(mapply(is.factor, train_set)  == TRUE)
numeric_variables <- which(mapply(is.numeric, train_set) == TRUE)







lad <- getModelInfo("lm", regex=FALSE)[[1]]
lad$label <- "Least absolute deviations regression"
lad$fit <- function (x, y, wts, param, lev, last, classProbs, ...) {
  dat <- if (is.data.frame(x)) {
    x
  } else {
    as.data.frame(x)
  }
  dat$.outcome <- y
  if (!is.null(wts)) {
    if (param$intercept) {
      out <- lad(.outcome ~ ., data=dat, weights=wts, ...)
    } else {
      out <- lad(.outcome ~ 0 + ., data=dat, weights=wts, ...)
    }
  } else {
    if (param$intercept) {
      out <- lad(.outcome ~ ., data=dat, ...)
    } else {
      out <- lad(.outcome ~ 0 + ., data=dat, ...)
    }
  }
  out
}
lad$predict <- function (modelFit, newdata, submodels=NULL) {
  if (!is.data.frame(newdata)) {
    newdata <- as.data.frame(newdata)
  }
  # predict(modelFit, newdata)
  coeff <- modelFit$coefficients
  num_coeff <- length(coeff)
  rowSums(data.frame(mapply(`*`, newdata, coeff[2:num_coeff], SIMPLIFY=FALSE))) + coeff[1]
}



#################################################################
#################################################################
#################################################################
# FROM ?preProcess
    # If PCA is requested but centering and scaling are not, the values
    #  will still be centered and scaled. Similarly, when ICA is
    #  requested, the data are automatically centered and scaled.
#################################################################
#################################################################
#################################################################



methods <- list(
  # lad,     ### não funciona com o YeoJohnson
  # getModelInfo("RRF", regex=FALSE)[[1]],     ### muuuuuuito lento!!!
  # getModelInfo("rf", regex=FALSE)[[1]]     ### muuuuuuito lento!!!
  getModelInfo("ranger", regex=FALSE)[[1]],     ###  lento!!!
  # getModelInfo("gcvEarth", regex=FALSE)[[1]],
  # getModelInfo("lm", regex=FALSE)[[1]],
  getModelInfo("glm", regex=FALSE)[[1]],
  # getModelInfo("glmnet", regex=FALSE)[[1]],
  # getModelInfo("svmRadial", regex=FALSE)[[1]],
  # getModelInfo("rpart", regex=FALSE)[[1]],
  # getModelInfo("knn", regex=FALSE)[[1]]
  # getModelInfo("earth", regex=FALSE)[[1]]
  # getModelInfo("bagEarth", regex=FALSE)[[1]],
  getModelInfo("xgbTree", regex=FALSE)[[1]]
)

method_name_set <- c(
  # "lad",     ### não funciona com o YeoJohnson
  # "RRF",     ### muuuuuuito lento!!!
  # "rf"     ### muuuuuuito lento!!!
  "ranger",     ###  lento!!!
  # "gcvEarth",
  # "lm",
  "glm",
  # "glmnet",
  # "svmRadial",
  # "rpart",
  # "knn"
  # "earth"
  # "bagEarth",
  "xgbTree"
)

# grid <- expand.grid(.cp=c(0, 0.05, 0.1))



# preproc_default <- c("")
# preproc_default <- c("zv")
preproc_default <- c("nzv")

preproc_scale <- list(
  c("")
  # c("center"),
  # c("scale"),
  # c("center", "scale" )
)
preproc_power <- c(
  "",
  # "BoxCox",
  "YeoJohnson"
  # "expoTrans"
)
preproc_impute  <- c(
  # ""
  "medianImpute"
  # "knnImpute"
  # "bagImpute",
)
preproc_project <- c(
  ""
  # "pca",
  # "ica"
  # "spatialSign"
)

# The operations are applied in this order: 
# zero-variance filter, near-zero variance filter, 
# Box-Cox/Yeo-Johnson/exponential transformation, 
# centering, scaling, range, 
# imputation, 
# PCA, ICA then spatial sign. This is a departure from versions of 'caret'
# prior to version 4.76 (where imputation was done first) and is not
# backwards compatible if bagging was used for imputation.
preprocess_name_set <- list()
x <- 1
for (i in 1:length(preproc_scale)) {
  for (j in 1:length(preproc_power)) {
    for (k in 1:length(preproc_impute)) {
      for (l in 1:length(preproc_project)) {
        # i=1
        # j=1
        # k=1
        # l=1
        aux <- preproc_scale[i]
        aux <- mapply(c, aux, preproc_impute[k], SIMPLIFY=FALSE)
        aux <- mapply(c, aux, preproc_power[j], SIMPLIFY=FALSE)
        aux <- mapply(c, aux, preproc_project[l], SIMPLIFY=FALSE)
        aux <- aux[[1]]
        if (!all(aux == "")) {
          aux <- aux[aux != ""]
          preprocess_name_set[[x]] <- aux
          x <- x + 1
        } else if(preproc_default != "") {
          aux <- preproc_default
          preprocess_name_set[[x]] <- aux
          x <- x + 1
        }
      }
    }
  }
}
preprocess_name_set <- unique(preprocess_name_set)
if (length(preprocess_name_set) == 0) {
  preprocess_name_set = list(preproc_default)
}



# # Remove existing ones
# length(preprocess_name_set)
# preprocess_name_set <- preprocess_name_set[c(-1, -7, -10, -16)]
# length(preprocess_name_set)

medianSummary <- function (data, lev=NULL, model=NULL) {
  out <- median(abs(data$obs - data$pred) / data$obs)
  names(out) <- "MED"
  out
}

trainControl <- trainControl(
  summaryFunction=medianSummary,
  method="repeatedcv",
  # number=10,
  number=5,
  repeats=3,
  # number=2,
  # repeats=1,
  search="random"
  # index=createFolds(iris$Species)
)


# preProc <- "zv"

# BoxCox, YeoJohnson, expoTrans, center, scale, range, knnImpute, bagImpute, medianImpute, pca, ica, spatialSign, ignore, keep, remove, zv, nzv, conditionalX








preProc <- ""
method <- "ranger"
do_training <- function(dataset, preProc, method) {
  set.seed(1)
  if (preProc != "") {
    fit <- train(
      # target ~ feature_0 + feature_7 + feature_8 + feature_9 + feature_10 + feature_11 + feature_12 + feature_14 + feature_15 + feature_16, 
      # target ~ feature_0 + feature_7 + feature_8 + feature_9 + feature_10 + feature_11 + feature_12 + feature_14 + feature_15 + feature_16 + feature_5, 
      # target ~ feature_14 + feature_15 + feature_9 + feature_7 + feature_8,
      target ~ ., 
      data=dataset,
      preProc=preProc,
      method=method,
      metric="MED",
      maximize=FALSE,
      na.action=na.pass,
      # na.action=na.roughfix,
      # na.action=na.fail,
      trControl=trainControl
      # importance=TRUE
    )
  } else {
    fit <- train(
      # target ~ feature_0 + feature_7 + feature_8 + feature_9 + feature_10 + feature_11 + feature_12 + feature_14 + feature_15 + feature_16, 
      # target ~ feature_0 + feature_7 + feature_8 + feature_9 + feature_10 + feature_11 + feature_12 + feature_14 + feature_15 + feature_16 + feature_5, 
      # target ~ feature_14 + feature_15 + feature_9 + feature_7 + feature_8,
      target ~ ., 
      data=dataset,
      method=method,
      metric="MED",
      maximize=FALSE,
      na.action=na.pass,
      # na.action=na.roughfix,
      # na.action=na.fail,
      trControl=trainControl
      # importance=TRUE
    )
  }
  # Return fit
  fit
}





dataset <- train_all[-valid_ind, ]


fit_file <- "fit.RData"
fit <- list()
method_ids <- list()
x <- 1
num_preProcs <- length(preprocess_name_set)
num_methods  <- length(methods)
for (i in 1:num_preProcs) {
  for (j in 1:num_methods) {
    # Start the clock!
    start_time <- Sys.time()

    # Prepare the id of the current method (preprocess and method)
    prep_names <- paste(preprocess_name_set[[i]], collapse="_", sep="_")
    method_id  <- method_name_set[j]
    if (prep_names != "") {
      method_id <- paste(method_id, prep_names, sep="_")
    }

    # Logging activities
    print("------------------------------")
    print(paste("Preprocess ", i, "/", num_preProcs, " -- Method ", j, "/", num_methods, sep=""))
    print(paste("  Preprocess:", prep_names))
    print(paste("  Method:", method_name_set[j]))
    print(paste("  Start: ", start_time))

    # Start cross-validation
    fit_aux <- try(
      do_training(
        dataset=dataset,
        preProc=preprocess_name_set[[i]],
        method=methods[[j]]
      ),
      TRUE
    )

    # Stop the clock
    end_time   <- Sys.time()
    total_time <- as.numeric(end_time - start_time, units="mins")

    # Deal with errors and continue
    if (inherits(fit_aux, "try-error")) {
      print("    Oops! An error occurred.")
      print("    Here's the original error message:")
      print(paste("    ", geterrmessage(), sep=""))
      fit[[method_id]] <- list("ema.error"=geterrmessage())
      fit[[method_id]]$ema.has_error <- TRUE
    } else {
      fit[[method_id]] <- fit_aux
      fit[[method_id]]$ema.has_error <- FALSE
    }

    # Additional info
    fit[[method_id]]$ema.method_id  <- method_ids[x]
    fit[[method_id]]$ema.total_time <- total_time
    fit[[method_id]]$ema.preprocess_name <- preprocess_name_set[[i]]
    fit[[method_id]]$ema.method_name     <- method_name_set[[j]]

    print(paste("  End: ",  end_time))
    print(paste("  Took: ", round(total_time), "minutes"))

    save(fit, preprocess_name_set, method_name_set, method_ids, file=fit_file)
    x <- x + 1
  }
}













# Compare algorithms
fit_file <- "fit.RData"
load(fit_file)
results <- list()
res <- NULL
for (i in 1:length(fit)) {
  if (!fit[[i]]$ema.has_error) {
    method_id <- toupper(fit[[i]]$ema.method_id)
    results[[method_id]] <- fit[[i]]
  } else {
    print(paste("Found error in position", i, "!"))
    print(fit$ema.error)
  }
}
results <- resamples(results)
dotplot(results)
bwplot(results)


fit

# save(results, file="fit_results.RData")
# load("fit_results.RData")

model_names_aux  <- names(results$methods)
model_names_aux <- strsplit(model_names_aux, "_")
method_names <- sapply(model_names_aux, "[", 1)
preproc_names <- sapply(model_names_aux, "[", 3)
preproc_names[is.na(preproc_names)] <- "NONE"
error <- mapply(mean, results$values[, -1], MoreArgs=list(na.rm=TRUE))
timings <- round(results$timings/60)$Everything
cost <- data.frame(preproc_names, method_names, error, timings)
ggplot(data=cost) +
  geom_point(aes(x=timings, y=error, col=method_names, shape=preproc_names)) +
  xlab("Custo (minutos)") + 
  ylab("Erro mediano absoluto") +
  guides(color=guide_legend("Métodos"), shape=guide_legend("Transformação"))
cost[order(cost[, 3]), ]


fit[["ranger_medianImpute"]]

mtry <- 24

train_set <- train_all[-valid_ind, ]
dummies <- dummyVars(~ ., data=train_set, levelsOnly=FALSE)
train_set <-  as.data.frame(predict(dummies, newdata=train_set))
preprocessParams <- preProcess(train_set, method="medianImpute")
train_set <- predict(preprocessParams, train_set)

valid_set <- train_all[valid_ind, ]
valid_set <-  as.data.frame(predict(dummies, newdata=valid_set))
valid_set <-  predict(preprocessParams, newdata=valid_set)


t <- Sys.time()
fit <- ranger(target ~ ., data=train_set, mtry=mtry, write.forest=TRUE, importance="impurity")
Sys.time() - t

save(fit, file="fit_final.RData")
load("fit_final.RData")

imp <- importance(fit)
imp <- sort(imp, decreasing=FALSE)
imp <- imp / sum(imp)
threshold <- 0.005
imp <- c("Demais"=sum(imp[imp < threshold]), imp)
imp <- imp[imp > threshold]
save(imp, file="fit_final_importance.RData")
par(las=2) # make label text perpendicular to axis
par(mar=c(5, 15, 4, 2)) # increase y-axis margin.
barplot(imp * 100, main="Importância das Variáveis (%)", horiz=TRUE, cex.names=0.8)


predictions <- predict(fit, valid_set)
# save(predictions, file="fit_final_predictions.RData")
# load("fit_final_predictions.RData")
predictions <- predictions$predictions
errors <- (valid_set$target - predictions) / valid_set$target
med <- median(abs(errors))
print(med)

test_set <- read.csv("test.csv")
test_set_id <- test_set$id
test_set$target <- NaN
test_set <- as.data.frame(predict(dummies, newdata=test_set))
test_set <- predict(preprocessParams, newdata=test_set)
pred <- predict(fit, test_set)
target <- data.frame("id"=test_set_id, "target"=pred$predictions)
write.csv(target, "test_predictions.csv", row.names=F, quote=FALSE)


