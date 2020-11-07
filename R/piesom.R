

#' piesom_setup
#'
#' @param data
#' @param formula
#'
#' @return
#' @export
#'
#' @examples
piesom_setup <- function(data, formula = ~.){
  res <- list()
  res$data <- data
  res$formula <- formula
  res$model.matrix <- model.matrix(formula, data)[,-1]
  res$pc <- princomp(res$model.matrix)
  res$sd.ratio.pc <- res$pc$sdev[1] / res$pc$sdev[2]
  res
}



#' piesom_som
#'
#' @param object
#' @param n
#' @param ...
#'
#' @return
#' @export
#' @importFrom kohonen somgrid
#' @importFrom kohonen som
#' @examples
piesom_som <- function(object, n = 6, ...){
  res <- object
  if(is.null(object$pc)) stop("piesom has not been initialized.")

  m <- ceiling(n * res$sd.ratio.pc)

  # To avoid unused argument error.
  res$grid <- R.utils::doCall("somgrid", xdim = m , ydim = n, ...)

  res$init.score <- res$grid$pts
  res$init.score[,1] <- rescale(res$grid$pts[,1], range(res$pc$scores[,1]), range(res$grid$pts[,1]))
  res$init.score[,2] <- rescale(res$grid$pts[,2], range(res$pc$scores[,2]), range(res$grid$pts[,2]))

  res$init.code <- list(res$init.score %*% t(res$pc$loadings[, 1:2]) + res$pc$center)

  res$som <- som(res$model.matrix, grid = res$grid, init = res$init.code, ...)

  res$som.score <- predict(res$pc, res$som$codes[[1]])

  res
}
