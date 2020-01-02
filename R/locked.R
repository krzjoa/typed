#' @name assign
#' @title Typed assign
#' @description Drop-in replacement for standard assignment to parse input
#'
`<-` <- function(x, value){
  # Split symbol into function and call
  assign('ref.env', parent.frame())
  assign('passed.call', rlang::enexpr(x))
  if (is.call(passed.call)) {
    # print("trolol")
    # browser()
    .Primitive("<-")(fun.args, rlang::call_args(passed.call))
    .Primitive("<-")(fun.name, rlang::call_name(passed.call))
    assign('value', do.call(paste0(fun.name, '<-'), list('x', value), envir = ref.env))
  } else {
    assign('z', eval(x))
  }

  # Assignment
  do.call(.Primitive("<-"), list('x', value), envir = ref.env)

  lockBinding('x', env = ref.env)
}

z <- 10
# rm(`<-`)

c(locked, integer) : x <- 10
casted(integer)



typed_function <- function(...){
  raw.exprs <- rlang::exprs(...)
  function(function.body, output){

  }
}

qwer <- typed_function(integer: x, char: y = "trolo")({

}, integer)


qwer <- function(x = typed(integer), y = typed(char), z = typed(data.frame)){

}


`locked<-` <- function(x, value){
  # browser()
  #assign(substitute(x), value = value, envir = parent.frame())
  #lockBinding(x, parent.frame())
  value
}

locked(x) <- 5

x <- locked(5)

colnames(z) <- c(1,2,3)

assign(substitute(x))
?base::`<-`

qwer <- list()

