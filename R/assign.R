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
