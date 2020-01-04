#' Define variable type
`typed<-` <- function(x, type, value){
  value
}

# Pre hook
attr(`typed<-`, 'typed.pre.hook') <- function(var.name, fun.args, value, ..., env){
  browser()
  type <- deparse(fun.args[[2]])

  if (exists(var.name, envir = env) & length(fun.args) == 2)



  if (!inherits(value, type))
    stop(sprintf("You are trying to assign %s value to %s variable",
                 class(value), type))

}

# typed(z, integer) <- 12
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
