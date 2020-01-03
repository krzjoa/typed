#' @name assign
#' @title Typed assign
#' @description Drop-in replacement for standard assignment to parse input
#' TODO: limit function to executing pre.hook and post.hook only
`<-` <- function(x, value){
  
  # Split symbol into function and call
  .Primitive("<-")('ref.env', parent.frame())
  .Primitive("<-")('passed.call', rlang::enexpr(x))

  .Primitive("<-")(fun.name, NULL)
  # .Primitive("<-")(var.name, NULL)

  if (is.call(passed.call)) {
    .Primitive("<-")(fun.name, rlang::call_name(passed.call))
    .Primitive("<-")(fun.args, rlang::call_args(passed.call))
    .Primitive("<-")(var.name, deparse(fun.args[[1]]))
    .Primitive("<-")(rest.args, fun.args[-1])

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
  
  # Pre hook 
  # TODO

  # Assignment
  do.call(.Primitive("<-"), list(var.name, value), envir = ref.env)
  
  # Post hook
  if (!is.null(attr(fun.name, 'post.hook')))
    attr(fun.name, 'pre.hook')(var.name, ..., env = ref.name)

}

`at<-` <- function(a, b, c){
  a[b[[1]], b[[2]]] <- c
  a
}

# colnames(z) <- c(1,2,3)
mat <- matrix(0, 3, 3)
mat[2, 2] <- 7
at(mat, c(1,2)) <- 5

# # rm(`<-`)

#' @name locked
#' @title Lock variable
#' @examples
#' x <- 10
#' x <- 2.5
#' x
#' locked(q) <- 20
#' q <- 10
#' 123 -> locked(y)
#' y <- 25
`locked<-` <- function(x, value){
  value
}

# Post hook
attr(`locked<-`, 'typed.post.hook') <- function(x, ..., env){
   lockBinding(var.name, env = env)
}

#' @rdname locked
#' @export
`immutable<-` <- `locked<-`
