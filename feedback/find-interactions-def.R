find_terms_in_interactions <- function(formula) {
  checkmate::assert_class(formula, "formula")

  # s. "Walking the AST with recursive functions" in Advanced R Programming Book
  # if called on a call to `^`, `:` or `*`, we save all variable names, else not.
  find_interaction <- function(formula) {
    if (is.atomic(formula) | is.name(formula)) {
      return(NULL)
    }
    if (!is.call(formula)) {
      stop("Don't know how to handle type ", typeof(formula),
           call. = FALSE)
    }
    if (formula[[1]] == quote(`^`) | formula[[1]] == quote(`:`) |
          formula[[1]] == quote(`*`)) {
        return(all.names(formula, functions = FALSE))
    }
    lapply(formula[-1], find_interaction)
  }
  in_interactions <- unique(unlist(find_interaction(formula)))
  sort(as.character(in_interactions))
}
