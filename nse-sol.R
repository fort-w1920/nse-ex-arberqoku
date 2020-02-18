get2 <- function(x, env = parent.frame()) {
  eval(as.name(x), env)
}

assign2 <- function(x, value, env = parent.frame()) {
  stopifnot(is.character(x) && length(x) == 1)
  x_sym <- as.name(x)
  eval(substitute(x_sym <- value), env)
  # `<-`(x, value)
}

g <- function(x = 20, y) {
  x + y
}

g(y = 5)

# TODO: very ugly...must be some other way
formals(g)[[1]] <- formals(function(x) NULL)[[1]]
formals(g)[[2]] <- 10


########## Finding terms in interactions #######################################
# Disclaimer: it will not cover ALL possible combinations in a formula because
# you can create pretty messy formulas for which this function will fail.

# self explanatory type identifying functions
is_symbol <- function(x, ...) rlang::is_symbol(x, ...)
is_constant <- function(x, ...) rlang::is_syntactic_literal(x)
is_call <- function(x, ...) rlang::is_call(x, ...)

is_mult <- function(x) is_call(x, "*")
is_pow <- function(x) is_call(x, "^")
is_colon <- function(x) is_call(x, ":")
is_bracket <- function(x) is_call(x, "(")

# convert expression type to string, in order to use switch operation
expr_type <- function(x) {
  if (is_constant(x)) {
    "constant"
  } else if (is_symbol(x)) {
    "symbol"
  } else if (is_call(x)) {
    "call"
  } else {
    typeof(x)
  }
}

# wrapper on switch operation
switch_expr <- function(x, ...) {
  switch(expr_type(x),
    ...,
    stop("Don't know how to handle type ", typeof(x), call. = FALSE)
  )
}

# collection iteration followed by flattening
flat_map_chr <- function(.x, .f, ...) {
  purrr::flatten_chr(purrr::map(.x, .f, ...))
}

# wrapper on find_terms_in_interactions_rec
find_terms_in_interactions <- function(formula) find_terms_in_interactions_rec(formula)

# recursively find interactions in a formula by traversing the syntax tree
find_terms_in_interactions_rec <- function(x) {
  switch_expr(x,
    # Base cases
    constant = ,
    symbol = character(),

    # Recursive cases
    call = find_terms_in_call(x)
  )
}

# different calls/operations need to be handled differently
find_terms_in_call <- function(x) {
  res <- character()
  
  # multiplications and colons indicate the presence of an interaction
  if (is_mult(x) || is_colon(x)) {
    
    is_first_symbol <- is_symbol(x[[2]])
    is_second_symbol <- is_symbol(x[[3]])
    
    if (is_second_symbol) {
      res <- c(res, as.character(x[[3]]))
    }
    
    # if both are symbols, simply go up one layer
    if (is_first_symbol) {
      res <- c(res, as.character(x[[2]]))
      children <- as.list(x)[-1]
      
    # handle consecutive calls of `*` or `:`
    } else {
      children <- as.list(x)
    }
  }
  
  # handle power (with brackets)
  else if (is_pow(x)) {
    if (is_bracket(x[[2]]) && is_constant(x[[3]]) && x[[3]] > 1) {
      res <- c(res, as.character(x[[2]][[2]][-1]))
      children <- as.list(x)[-1]
    }
    children <- as.list(x)
  } 
  # nothing found, keep going
  else {
    res <- character()
    children <- as.list(x)
  }
  c(res, flat_map_chr(children, find_terms_in_interactions_rec))
}

find_terms_in_interactions(a ~ 1 + a + b * c)

find_terms_in_interactions(a ~ 1 + a + b * c + poly(d, 3) + (g + h)^2 + e:f:b:fnargl)
