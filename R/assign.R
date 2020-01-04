#' @name assign
#' @title Typed assign
#' @description Drop-in replacement for standard assignment to parse input
#' @examples
#' `at<-` <- function(a, b, c){
#' a[b[[1]], b[[2]]] <- c
#' a
#' }
#' mat <- matrix(0, 3, 3)
#' mat[2, 2] <- 7
#' at(mat, c(1,2)) <- 5
#' @export
`<-` <- function(x, value){

  # Split symbol into function and call
  .Primitive("<-")('ref.env', parent.frame())
  .Primitive("<-")('passed.call', rlang::enexpr(x))
  .Primitive("<-")(fun.name, NULL)
  .Primitive("<-")(called.fun, NULL)

  if (is.call(passed.call)) {
    .Primitive("<-")(fun.name, paste0(rlang::call_name(passed.call), "<-"))
    .Primitive("<-")(fun.args, rlang::call_args(passed.call))
    .Primitive("<-")(var.name, deparse(fun.args[[1]]))
    .Primitive("<-")(called.fun, get(fun.name, ref.env))

    assign('value', do.call(fun.name, c(fun.args, list(value)),
                            envir = ref.env))
  } else {
    .Primitive("<-")(var.name, deparse(substitute(x)))
  }

  if (!is.null(attr(called.fun, 'typed.pre.hook')))
    attr(called.fun, 'typed.pre.hook')(var.name, fun.args, value, env = ref.env)

  # Assignment
  do.call(.Primitive("<-"), list(var.name, value), envir = ref.env)

  # Post hook
  if (!is.null(attr(called.fun, 'typed.post.hook')))
    attr(called.fun, 'typed.post.hook')(var.name, fun.args, value, env = ref.env)
}

# rm(`<-`)

