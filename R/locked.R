#' @name locked
#' @title Lock variable binding
#' @description Using `locked` (or its alias, `immutable`) together with `typed` package
#' implementation of assignment operator allows you to lock variable binding in simplier, more
#' straighforward way (in one function call, without additional `lockBinding` function calls).
#' @examples
#' x <- 10
#' x <- 2.5
#' x
#' locked(q) <- 20
#' q <- 10
#' 123 -> locked(y)
#' y <- 25
#' @export
`locked<-` <- function(x, value){
  value
}

# Post hook
attr(`locked<-`, 'typed.post.hook') <- function(x, ..., env){
   lockBinding(x, env = env)
}

#' @rdname locked
#' @export
`immutable<-` <- `locked<-`
