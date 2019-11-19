MTPS <- function(xmat, ymat, family ,
                            cv = FALSE, residual = TRUE,
                            nfold=5, method.step1, method.step2,
                            resid.type=c("deviance", "pearson", "raw"),
                            resid.std=FALSE) {

  resid.type <- match.arg(resid.type)

  ny <- ncol(ymat)

  # check family input
  if (length(family) == 1) {
    if (!family %in% c("gaussian", "binomial")) {
      stop("family must be gaussian or binomial")
    }
    if (family == "gaussian") {
      family = rep("gaussian", ny)
    } else if (family == "binomial") {
      family = rep("binomial", ny)
    }
  }
  if (length(family) != ny) {
    stop("length of family must be consistent with response")
  }
  if (sum(family %in% c("gaussian", "binomial")) != ny) {
    stop("family must be gaussian or binomial or their combination")
  }

  # check family method consistency
  if (length(method.step1) == 1) {
    method.step1 <- rep(list(method.step1),ny)
  }
  if (length(method.step2) == 1) {
    method.step2 <- rep(list(method.step2),ny)
  }
  if (length(method.step1) != ny) {
    stop("length of method.step1 must be 1 or the same as response column")
  }
  if (length(method.step2) != ny) {
    stop("length of method.step2 must be 1 or the same as response column")
  }
  for (ii in 1:ny) {
    if (!check.match(family[ii], FUN=method.step1[[ii]])) {
      stop("method.step1 must be consistent with response category")
    }
  }
  if (!residual) {
    for (ii in 1:ny) {
      if (!check.match(family[ii], FUN=method.step2[[ii]])) {
        stop("method.step2 must be consistent with response category")
      }
    }
  } else {
    for (ii in 1:ny) {
      if (!check.match("gaussian", FUN=method.step2[[ii]])) {
        stop("residual stacking does not allow binary outcome model in second step")
      }
    }
  }

  # step 1
  if (cv) {
    fit1 <- cv.multiFit(xmat=xmat, ymat=ymat, nfold=nfold,
                        method=method.step1,
                        family=family)
  } else {
    fit1 <- multiFit(xmat=xmat, ymat=ymat,
                     method=method.step1,
                     family=family)
  }
  pred1 <- fit1$y.fitted

  # step 2
  if (residual) {
    fit2 <- rs.multiFit(yhat=pred1,ymat=ymat,xmat=xmat,
                        family=family,
                        resid.type=resid.type,
                        resid.std= resid.std,
                        method=method.step2)
  } else {
    fit2 <- multiFit(xmat=pred1, ymat=ymat,
                     method=method.step2,
                     family=family)
  }

  fit <- list(fit1 = fit1, fit2 = fit2,
              cv=cv, residual=residual)
  class(fit) <- "MTPS"
  return(fit)
}

predict.MTPS <- function(object, newdata, ...) {

  if (object$cv) {
    pred1 <- predict(object$fit1, newdata)
  } else {
    pred1 <- predict(object$fit1, newdata)
  }

  if (object$residual) {
    pred2 <- predict(object$fit2, pred1)
  } else {
    pred2 <- predict(object$fit2, pred1)
  }

  return(pred2)

}




