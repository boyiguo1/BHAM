calculate_EDF <- function(object, vars = 1:length(object$coefficients)){
  x <- model.matrix(object)[, vars, drop = FALSE]

  # TODO: matching dimension
  # S_lambda <- diag(object$prior.scale[vars-1])

  S_lambda <- diag(1/object$prior.scale[vars])

  A <- x%*%solve(crossprod(x) + S_lambda)%*%t(x)

  tau <- sum(diag(2*A))-sum(diag(A%*%A))

  return(tau)
}
