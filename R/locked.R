#' @name assign
#' @title Typed assign
#' @description Drop-in replacement for standard assignment to parse input
#'
`<-` <- function(x, value){
  # Split symbol into function and call
  assign('ref.env', parent.frame())
  assign('passed.call', rlang::enexpr(x))

  .Primitive("<-")(fun.name, NULL)
  .Primitive("<-")(var.name, NULL)

  if (is.call(passed.call)) {
    .Primitive("<-")(fun.name, rlang::call_name(passed.call))
    .Primitive("<-")(fun.args, rlang::call_args(passed.call))
    .Primitive("<-")(var.name, deparse(fun.args[[1]]))
    .Primitive("<-")(rest.args, fun.args[-1])

    # browser()

    if (exists(var.name, envir = ref.env)){
      .Primitive("<-")(variable, get(var.name, envir = ref.env))
      assign('value', do.call(paste0(fun.name, '<-'),
                              c(list(variable), rest.args, list(value)),
                              envir = ref.env))
    } else {
      assign('value', do.call(paste0(fun.name, '<-'),
                              c(list(var.name), rest.args, list(value)),
                              envir = ref.env))
    }

  } else {
    .Primitive("<-")(var.name, deparse(substitute(x)))
  }

  # Assignment
  do.call(.Primitive("<-"), list(var.name, value), envir = ref.env)

  if (!is.null(fun.name))
    if (any(fun.name %in% c('locked', 'immutable')))
      lockBinding(var.name, env = ref.env)
}

z <- 10
locked(q) <- 20
q <- 10

123 -> locked(ert)
ert <- 25


`at<-` <- function(a, b, c){
  a[b[[1]], b[[2]]] <- c
  a
}

mat <- matrix(0, 3, 3)

mat[2, 2] <- 7

at(mat, c(1,2)) <- 5


immutable(super.variable) <- 789

# # rm(`<-`)
#
# c(locked, integer) : x <- 10
# casted(integer)
#
#
#
# typed_function <- function(...){
#   raw.exprs <- rlang::exprs(...)
#   function(function.body, output){
#
#   }
# }
#
# qwer <- typed_function(integer: x, char: y = "trolo")({
#
# }, integer)
#
#
# qwer <- function(x = typed(integer), y = typed(char), z = typed(data.frame)){
#
# }


`locked<-` <- function(x, value){
  # browser()
  #assign(substitute(x), value = value, envir = parent.frame())
  #lockBinding(x, parent.frame())
  value
}

locked(x) <- 5

# x <- locked(5)
#
# colnames(z) <- c(1,2,3)
#
# assign(substitute(x))
# ?base::`<-`
#
# qwer <- list()

