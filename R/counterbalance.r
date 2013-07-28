#' Create a unique set of permutations
#' 
#' @param conds A list of values you want the unique permutations of
#' @param N The number of unique permutations you want
#' @return (matrix) It will return a matrix of the conds with each row representing a unique permutation
#' @importFrom e1071 permutations
#' @export
uniquePermutations <- function(conds,N) {
  howManyPossible <- factorial(length(conds))
  if (howManyPossible < N) {stop("In russmisc::uniquePermutations: more unique permutations requested than are possible!")}
  if (howManyPossible == N) {e1071::permutations(length(conds))}
  clean <- NULL
  requested <- N
  repeat{
    res <- replicate(requested,sample(conds))
    clean <- unique(cbind(res,clean),MARGIN=2)
    requested <- N - ncol(clean)
    if (requested == 0) break
  }
  return(t(clean))
}
NULL

#' Create a cyclic latin square
#'
#' This function creates a cyclic latin square 
#'
#' @param conds A vector of condition IDs or condition names
#' @return (list) Digram balanced latin squares come in pairs when there are an odd number of conditions, but only one square when there are an even number of conditions. This function returns a list with a square (in a matrix) for each item.
#' @author Russell S. Pierce \email{Russell.S.Pierce@@gmail.com}
#' @export
#' @examples
#' latinSquareCyclic(1:3)
#' latinSquareCyclic(LETTERS[1:4])
latinSquareCyclic <- function(conds) {
  length.conds <- length(conds)
  res <- matrix(NA,nrow=length.conds,ncol=length.conds)
  dblconds <- c(conds,conds)
  for (i in seq_along(conds)) {
    res[i,] <- dblconds[i:(i+length.conds-1)]
  }
  return(res)
}
NULL

#' Create a digram-balanced latin square
#'
#' This function creates a digram-balanced latin square as described in Keppel & Wickens, 2004 pg 384-386. The condition orders for subjects are listed by rows, such that row one is the order of conditions for subject 1, and so on. This method of counter-balancing is preferred to so-called cyclic counter-balancing because it balances the order of presentation such that conditions occur in each position equally and they also balance the order of presentation such that each condition occurs before and after each other presentation an equal number of times. See Keppel & Wickens, 2004 for details. 
#'
#' @export latinSquareDigram
#' @param conds A vector of condition IDs or condition names
#' @return (list) Digram balanced latin squares come in pairs when there are an odd number of conditions, but only one square when there are an even number of conditions. This function returns a list with a square (in a matrix) for each item.
#' @references
#' Keppel & Wickens, 2004 pp 384-386
#' @author Russell S. Pierce \email{Russell.S.Pierce@@gmail.com}
#' @examples
#' latinSquareDigram(1:3)
#' latinSquareDigram(LETTERS[1:4])
latinSquareDigram <- function(conds) {
    if (!is.vector(conds)) {
        stop("latinSquareDigram requires an input in the form of a vector")
    }
    nconditions <- length(conds)
    if (nconditions < 2) {
        stop("latinSquareDigram requires an vector of a length of 2 or more items")
    }
    interleave <- function(source1, source2) {
        result <- NULL
        for (i in 1:length(source1)) {
            result <- c(result, source1[i], source2[i])
        }
        return(result)
    }
    cyclic <- matrix(nrow = nconditions, ncol = nconditions)
    for (i in 1:nconditions) {
        cyclic[i, ] <- c(seq(from = i, to = nconditions, length.out = nconditions - 
            (i - 1)), seq(from = 1, to = (i - 1), length.out = i - 
            1))
    }
    reversed <- cyclic[, ncol(cyclic):1]
    interleaved <- matrix(nrow = nconditions, ncol = nconditions * 
        2)
    for (i in 1:nconditions) {
        interleaved[i, ] <- interleave(cyclic[i, ], reversed[i, 
            ])
    }
    square1 <- interleaved[, 1:nconditions]
    square1 <- matrix(conds[square1], nrow = nrow(square1))
    square2 <- interleaved[, (nconditions + 1):(nconditions * 
        2)]
    square2 <- matrix(conds[square2], nrow = nrow(square2))
    #with an even number of conditions, the squares produced are functionally identical
    if (ncol(square1)%%2 == 0) {
        return(list(square1))  
    }
    else {
        return(list(square1, square2))
    }
}
NULL

#' Get a set of minimially replicated sequences
#'
#' This function (attempts to) provide a maximally counterbalanced 
#' set of sequences from the full possible set of sequences. 
#' 
#' @section Details:
#' When the number of requested sequences allows for a (multiple)
#' set(s) of full permtations (a.k.a. t x t! design or full rectangular array)
#' to be used, it is provided.
#' 
#' If the number of sequences requested does not allow for a(nother)
#' full set of permutations, then the remaining sequences are
#' selected from full digram balanced latin squares.  The assignment
#' of conditions to values in those latin squares are randomized 
#' between squares with the constraint that no two squares can use 
#' the same set of assignments.  In addition, if iter is greater 
#' than 0, an attempt is made to meet the constraint that no 
#' condition is assigned to any given value in the latin square
#' \code{toomany} (or more) times more than any other.
#' 
#' If the number of sequences requested does not allow for a(nother)
#' full (set of) digram balanced latin square(s), the
#' remaining requested sequences are randomly selected from 
#' a latin square with an as yet used assignment of condtions to 
#' values selected according to the constraints used in the selection
#' of full latin squares. In addition, if \code{iter} is greater 
#' than 0, an attempt is made to meet the constraint that no 
#' condition is assigned to any position \code{toomany} (or more) 
#' times more than any other.  A warning message is generated
#' if this constraint is not met before the limit set in \code{iter}
#' is reached.
#'
#' @section Warning: The function will not work for large numbers of conditions because the ability to fit all possible permutations into memory is compromised!
#' @export
#' @importFrom e1071 permutations
#' @param conds (vector) A of condition IDs or condition names
#' @param N (numeric) The number of sequences desired
#' @param toomany (numeric) If there are this many (or more) extra occurances of a given condition at a given order position by the time the function returns, then a warning is generated.
#' @param iter (integer) The maximum number of iterations to search for a set of permutations that does not fail the \code{toomany} criterion.  If you don't want a search to take place, set iter to -1.  Note that if iter is == -1, then pb == FALSE by default.  The default value of 500 seems sufficicent for at least 95\% of common cases
#' @param latinSquareFunction (function) The function you want to use to produce latin squares.  The arguments and returns for such a function should be like \code{\link{latinSquareDigram}}.
#' @param pb (boolean) Indicate whether you want a progress bar.  Having a progress bar may make solution finding slower.
#' @return N x conds (matrix) of sequences (by rows)
#' @author Russell S. Pierce \email{Russell.S.Pierce@@gmail.com}
#' @examples
#' #counterbalance(LETTERS[1:4],12) #not run, time consuming
#' counterbalance(LETTERS[1:4],25)
#' 
#' #Sample code to check the maximum difference in the number of times a condition appears in a given position
#' checkmaxdiff <- function(mat) {
#'  mat.levels <- as.character(sort(unique(as.vector(mat))))
#'  mat.factor <- as.data.frame(apply(mat,2,as.character))
#'  mat.factor <- lapply(mat.factor,factor,levels=mat.levels)  
#'  return(max(unlist(lapply(lapply(lapply(mat.factor,table),range),diff))))
#' }
#' 
#' ## Example code not run
#' #Checking number of iter required
#' #combinations <- expand.grid(Ncond=3:10,N=1:300)
#' #combinations$howmany <- NA
#' #for (i in 1:nrow(combinations)){
#' # res <- counterbalance(1:combinations[i,"Ncond"],combinations[i,"N"],toomany=1,iter=1000)
#' # combinations[i,"howmany"] <- checkmaxdiff(res)
#' #}
#' 
counterbalance <- function(conds, N, toomany = 2, iter=500, pb=TRUE, latinSquareFunction=latinSquareDigram) {
  if (iter <= 0) {pb <- FALSE}
  warn.latin <- warn.individual <- FALSE
  res <- NULL #Yes, I am committing the sin of 'growing' the matrix
  counterbalanceN.remaining <- N
  #A function to see how well we've met our preferences of having equal N for each cond in each position
  checkmaxdiff <- function(mat) {
    mat.levels <- as.character(sort(unique(as.vector(mat))))
    mat.factor <- as.data.frame(apply(mat,2,as.character))
    mat.factor <- lapply(mat.factor,factor,levels=mat.levels)  
    return(max(unlist(lapply(lapply(lapply(mat.factor,table),range),diff))))
  }
  possiblePerms <- tryCatch({
      e1071::permutations(length(conds))
    }, error=function(e) {
    warning("In russmisc::counterbalance: It was not possible to calculate all possible permutations.  N unique permutations substituted.  This process is non-deterministic and may take quite some time.  Moreover it may lead to unbalanced designs!")
    uniquePermutations(1:length(conds),N)
    }
  )
  possiblePerms <- matrix(conds[possiblePerms],ncol=ncol(possiblePerms),nrow=nrow(possiblePerms))
  possiblePerms.nrow <- nrow(possiblePerms)
  possiblePerms.ncol <- ncol(possiblePerms)
  #We make lsDigram just to see how many rows it will be
  lsDigram <- do.call("rbind",latinSquareFunction(1:length(conds)))
  lsDigram.nrow <- nrow(lsDigram)
  fullsets <- trunc(N/possiblePerms.nrow)
  if (fullsets > 0) {    
    res <- do.call(rbind,replicate(fullsets,possiblePerms,simplify=FALSE))
    counterbalanceN.remaining <- N - nrow(res)
  } else {
    res <- NULL
  }
  fullsquares <- trunc(counterbalanceN.remaining/lsDigram.nrow)  
  if (fullsquares > possiblePerms.nrow) {stop("In russmisc::counterbalance: A programming error has occured, tried to generate more full latin squares than there are possible permutations of the conditions!")}
  i <- 0
  found <- FALSE
  best.checkmaxdiff <- .Machine$integer.max
  saved.attempt <- matrix(possiblePerms[sample(1:possiblePerms.nrow,fullsquares+1),],ncol=length(conds))
  if (pb) pb.obj <- txtProgressBar(max=iter,title="Selecting Latin Square Sequences")
  while ((i <= iter) & (found == FALSE)) {
    attempt <- matrix(possiblePerms[sample(1:possiblePerms.nrow,fullsquares+1),],ncol=length(conds))
    attempt.checkmaxdiff <- checkmaxdiff(attempt)
    if (attempt.checkmaxdiff < best.checkmaxdiff) {
      saved.attempt <- attempt
      best.checkmaxdiff <- attempt.checkmaxdiff
      if (best.checkmaxdiff < toomany) {found <- TRUE}
    }
    i <- i + 1
    if (pb) setTxtProgressBar(pb.obj,i)
  }
  if (pb) close(pb.obj)
  if (best.checkmaxdiff >= toomany) {warn.latin <- TRUE} 
  squarePerms <- saved.attempt
  remainder.square.perm <- squarePerms[nrow(squarePerms),]
  squarePerms <- matrix(squarePerms[-nrow(squarePerms),],ncol=length(conds))
  squares.selected <- apply(squarePerms,1,latinSquareFunction)
  #code would otherwise fail if there was just an empty list
  if (length(squares.selected)) {
    res <- rbind(res,do.call("rbind",unlist(squares.selected,recursive=FALSE)))
  }
  counterbalanceN.remaining <- counterbalanceN.remaining - fullsquares*lsDigram.nrow
  if (counterbalanceN.remaining) {
    remainder.square <- latinSquareFunction(remainder.square.perm)
    remainder.square <- do.call("rbind",remainder.square)
    i <- 0
    found <- FALSE
    best.checkmaxdiff <- .Machine$integer.max
    saved.attempt <- remainder.square[sample(1:nrow(remainder.square),counterbalanceN.remaining),]
    if (pb) pb.obj <- txtProgressBar(max=iter,title="Selecting Latin Square Sequences")
    while ((i <= iter) & (found == FALSE)) {
      attempt <- matrix(remainder.square[sample(1:nrow(remainder.square),counterbalanceN.remaining),],ncol=length(conds))
      attempt.checkmaxdiff <- if (warn.latin) {checkmaxdiff(rbind(res,attempt))} else {checkmaxdiff(attempt)}
      if (attempt.checkmaxdiff < best.checkmaxdiff) {
        saved.attempt <- attempt
        best.checkmaxdiff <- attempt.checkmaxdiff
        if (best.checkmaxdiff < toomany) {found <- TRUE}
      }
      i <- i + 1
      if (pb) setTxtProgressBar(pb.obj,i)
    }
    res <- rbind(res,saved.attempt)
  } #end if (counterbalanceN.remaining)
  if (pb) close(pb.obj)
  if (best.checkmaxdiff >= toomany) {warn.individual <- TRUE}
  rownames(res) <- NULL
  howmany <- checkmaxdiff(res)
  message("In russmisc::counterbalance: Including ",fullsets," full sets of permutations.")
  message("In russmisc::counterbalance: Including ",fullsquares," full (unique) digram balanced latin squares.")
  message("In russmisc::counterbalance: Including ",counterbalanceN.remaining," sequences from a partial (unique) digram balanced latin square.")
  if (howmany >= toomany) {
    msg <- paste("In russmisc::counterbalance: Final solution exceeds toomany criterion. Specifically, one condition occurs at a given time", howmany, "times more than the least frequent condition at that time. Increase iter to improve the chances of a more balanced set of sequences.")
    if (warn.latin)      {msg <- paste(msg,"Insufficient solution to the orders assigned to latin squares found.")}
    if (warn.individual) {msg <- paste(msg,"Insufficient solution to the orders assigned to from the partial (unique) digram balanced latin squares found.")}
    warning(msg)
  }
  #cat("WL:",warn.latin,"WI:",warn.individual,"\n")
  if (nrow(res) != N) {stop("In russmisc::counterbalance: Programming error. The number of returned rows did not match the number of requested rows!")}
  return(res)
}
NULL

## Sample code for checking sets of orders
# res <- NULL
# data.ncol <- ncol(data)
# n.order <- vector("list",data.ncol-2)
# names(n.order) <- paste("Sets of ",2:(data.ncol-1),sep="")
# data.zoo <- zoo(t(data))
# for (o in 1:(data.ncol-2)){
#   for (i in 1:(data.ncol-o)){
#     zoo.res <- window(data.zoo,start=i,end=(i+o))
#     n.order[[o]] <- c(n.order[[o]],apply(zoo.res,2,paste,collapse=""))
#   }  
# }
# lapply(lapply(lapply(n.order,table),range),diff)
