################################################
## Functions for derivatives of GAM(M) models ##
################################################
# Taken from https://gist.githubusercontent.com/gavinsimpson/e73f011fdaaab4bb5a30/raw/82118ee30c9ef1254795d2ec6d356a664cc138ab/Deriv.R
# See https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/

plotGAMSignificantSlopes = function(gam.model, Term, Term.label, eps = 1e-7, aoaLab="AoAscale"){
  # This is a custom function for the loanwords project
  # It essentially uses the same code as in the original source, 
  # with some tricks to handle categorical data
  
  t.labs <- attr(gam.model$terms, "term.labels")
  
  # Plot the curve using standard library
  # this will be drawn over
  smooth.plot = plot(gam.model,select=which(t.labs==Term))
  names.smooth.plot = sapply(smooth.plot,function(X){X$xlab})
  curve.x = smooth.plot[[which(names.smooth.plot==Term)]]$x
  curve.y = smooth.plot[[which(names.smooth.plot==Term)]]$fit
  curve.y.upper = curve.y+smooth.plot[[which(names.smooth.plot==Term)]]$se
  curve.y.lower = curve.y-smooth.plot[[which(names.smooth.plot==Term)]]$se
  
  newD = data.frame(fit=curve.y)
  smooth.plot.VarNames = as.character(sapply(smooth.plot,function(X){X$main},simplify = F))
  for(var in t.labs){
    if(!var %in% names.smooth.plot){
      # categorical variables
      newD[,var] = smooth.plot[[
        which(grepl(var,smooth.plot.VarNames))[1]
        #which(names.smooth.plot=="Gaussian quantiles")[1]
        ]]$raw[1]
    } else{
      newD[,var] = smooth.plot[[which(names.smooth.plot==var)]]$x
    }
  }
  
  # Predict values
  X0 = predict(gam.model,newdata = newD,type="lpmatrix")
  # Increment numeric values by small amount
  for(i in 1:ncol(newD)){
    if(is.numeric(newD[,i])){
      newD[,i] = newD[,i] + eps
    }
  }
  X1 <- predict(gam.model,newdata = newD,type = "lpmatrix")
  # Work out derivatives
  Xp <- (X1 - X0) / eps
  Xp.r <- NROW(Xp)
  Xp.c <- NCOL(Xp)
  
  nt <- length(t.labs)
  ## list to hold the derivatives
  lD <- vector(mode = "list", length = nt)
  names(lD) <- t.labs
  # Move through each basis function for each term
  # and work out the overall standard error for the spline
  for(i in seq_len(nt)) {
    Xi <- Xp * 0
    want <- grep(t.labs[i], colnames(X1))
    Xi[, want] <- Xp[, want]
    df <- Xi %*% coef(gam.model)
    df.sd <- rowSums(Xi %*% gam.model$Vp * Xi)^.5
    lD[[i]] <- list(deriv = df, se.deriv = df.sd)
  }
  
  class(lD) <- "Deriv"
  lD$gamModel <- gam.model
  lD$eps <- eps
  
  # subtract the eps again to return the data to original
  for(i in 1:ncol(newD)){
    if(is.numeric(newD[,i])){
      newD[,i] = newD[,i] - eps
    }
  }
  lD$eval <- newD 
  
  # Calculate confidence intervals
  # essentially the same as confint.Deriv(lD, term = Term)
  alpha = 0.05
  m2.dci <- vector(mode = "list", length = length(Term))
  names(m2.dci) <- Term
  residual.df <- df.residual(gam.model)
  tVal <- qt(1 - (alpha/2), residual.df)
  ##for(i in term.labs[term]) {
  for(i in Term) {
    upr <- lD[[i]]$deriv + tVal * lD[[i]]$se.deriv
    lwr <- lD[[i]]$deriv - tVal * lD[[i]]$se.deriv
    m2.dci[[i]] <- list(upper = drop(upr), lower = drop(lwr))
  }
  m2.dci$alpha = alpha
  
  # If slope is significant (upper and lower bounds of derivative don't cross 0), 
  # then return the position in the slope.
  # If not, return NA (this is used for plotting)
  m2.dsig <- signifD(curve.y, 
                     d = lD[[Term]]$deriv, 
                     m2.dci[[Term]]$upper, 
                     m2.dci[[Term]]$lower)
  
  # Plot significant increases
  lines(curve.x, unlist(m2.dsig$incr), col = "blue", lwd = 3)
  # Plot significant decreases
  lines(curve.x, unlist(m2.dsig$decr), col = "red", lwd = 3)
  
}


####
# Original code:


Deriv <- function(mod, n = 200, eps = 1e-7, newdata, term) {
  if(inherits(mod, "gamm"))
    mod <- mod$gam
  m.terms <- attr(terms(mod), "term.labels")
  if(missing(newdata)) {
    newD <- sapply(model.frame(mod)[, m.terms, drop = FALSE],
                   function(x) seq(min(x), max(x), length = n))
    names(newD) <- m.terms
  } else {
    newD <- newdata
  }
  X0 <- predict(mod, data.frame(newD), type = "lpmatrix")
  for(i in 1:ncol(newD)){
    if(is.numeric(newD[,i])){
      newD[,i] = newD[,i] + eps
    }
  }
  #newD <- newD + eps
  X1 <- predict(mod, data.frame(newD), type = "lpmatrix")
  Xp <- (X1 - X0) / eps
  Xp.r <- NROW(Xp)
  Xp.c <- NCOL(Xp)
  ## dims of bs
  bs.dims <- sapply(mod$smooth, "[[", "bs.dim") - 1
  ## number of smooth terms
  t.labs <- attr(mod$terms, "term.labels")
  ## match the term with the the terms in the model
  if(!missing(term)) {
    want <- grep(term, t.labs)
    if(!identical(length(want), length(term)))
      stop("One or more 'term's not found in model!")
    t.labs <- t.labs[want]
  }
  nt <- length(t.labs)
  ## list to hold the derivatives
  lD <- vector(mode = "list", length = nt)
  names(lD) <- t.labs
  for(i in seq_len(nt)) {
    Xi <- Xp * 0
    want <- grep(t.labs[i], colnames(X1))
    Xi[, want] <- Xp[, want]
    df <- Xi %*% coef(mod)
    df.sd <- rowSums(Xi %*% mod$Vp * Xi)^.5
    lD[[i]] <- list(deriv = df, se.deriv = df.sd)
  }
  class(lD) <- "Deriv"
  lD$gamModel <- mod
  lD$eps <- eps
  for(i in 1:ncol(newD)){
    if(is.numeric(newD[,i])){
      newD[,i] = newD[,i] - eps
    }
  }
  lD$eval <- newD 
  lD ##return
}

confint.Deriv <- function(object, term, alpha = 0.05, ...) {
  l <- length(object) - 3
  term.labs <- names(object[seq_len(l)])
  if(missing(term)) {
    term <- term.labs
  } else { ## how many attempts to get this right!?!?
    ##term <- match(term, term.labs)
    ##term <- term[match(term, term.labs)]
    term <- term.labs[match(term, term.labs)]
  }
  if(any(miss <- is.na(term)))
    stop(paste("'term'", term[miss], "not a valid model term."))
  res <- vector(mode = "list", length = length(term))
  names(res) <- term
  residual.df <- df.residual(object$gamModel)
  tVal <- qt(1 - (alpha/2), residual.df)
  ##for(i in term.labs[term]) {
  for(i in term) {
    upr <- object[[i]]$deriv + tVal * object[[i]]$se.deriv
    lwr <- object[[i]]$deriv - tVal * object[[i]]$se.deriv
    res[[i]] <- list(upper = drop(upr), lower = drop(lwr))
  }
  res$alpha = alpha
  res
}

signifD <- function(x, d, upper, lower, eval = 0) {
  miss <- upper > eval & lower < eval
  incr <- decr <- x
  want <- d > eval
  incr[!want | miss] <- NA
  want <- d < eval
  decr[!want | miss] <- NA
  list(incr = incr, decr = decr)
}

plot.Deriv <- function(x, alpha = 0.05, polygon = TRUE,
                       sizer = FALSE, term,
                       eval = 0, lwd = 3,
                       col = "lightgrey", border = col,
                       ylab, xlab, main, ...) {
  l <- length(x) - 3
  ## get terms and check specified (if any) are in model
  term.labs <- names(x[seq_len(l)])
  if(missing(term)) {
    term <- term.labs
  } else {
    term <- term.labs[match(term, term.labs)]
  }
  if(any(miss <- is.na(term)))
    stop(paste("'term'", term[miss], "not a valid model term."))
  if(all(miss))
    stop("All terms in 'term' not found in model.")
  l <- sum(!miss)
  nplt <- n2mfrow(l)
  tVal <- qt(1 - (alpha/2), df.residual(x$gamModel))
  if(missing(ylab))
    ylab <- expression(italic(hat(f)*"'"*(x)))
  if(missing(xlab)) {
    xlab <- attr(terms(x$gamModel), "term.labels")
    names(xlab) <- xlab
  }
  if (missing(main)) {
    main <- term
    names(main) <- term
  }
  ## compute confidence interval
  CI <- confint(x, term = term)
  ## plots
  layout(matrix(seq_len(l), nrow = nplt[1], ncol = nplt[2]))
  for(i in term) {
    upr <- CI[[i]]$upper
    lwr <- CI[[i]]$lower
    ylim <- range(upr, lwr)
    plot(x$eval[,i], x[[i]]$deriv, type = "n",
         ylim = ylim, ylab = ylab, xlab = xlab[i], main = main[i], ...)
    if(isTRUE(polygon)) {
      polygon(c(x$eval[,i], rev(x$eval[,i])),
              c(upr, rev(lwr)), col = col, border = border)
    } else {
      lines(x$eval[,i], upr, lty = "dashed")
      lines(x$eval[,i], lwr, lty = "dashed")
    }
    abline(h = 0, ...)
    if(isTRUE(sizer)) {
      lines(x$eval[,i], x[[i]]$deriv, lwd = 1)
      S <- signifD(x[[i]]$deriv, x[[i]]$deriv, upr, lwr,
                   eval = eval)
      lines(x$eval[,i], S$incr, lwd = lwd, col = "blue")
      lines(x$eval[,i], S$decr, lwd = lwd, col = "red")
    } else {
      lines(x$eval[,i], x[[i]]$deriv, lwd = 2)
    }
  }
  layout(1)
  invisible(x)
}




