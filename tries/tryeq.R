
#' Data
#' =====
#' 

data(mtcars)
head(mtcars)
mtcars$amf <- factor(ifelse(mtcars$am == 1, "a", "m"))





#'
#'Equations
#' ========================================================================

#+ def
regeq <- function(mod, digits = 3, 
                  math = FALSE, displaymath = FALSE, sep = TRUE) {
        co <- coef(mod)
        response <- attr(mod$terms, "variables")[[2]]
        predictors <- names(co)
        # displaymath supersedes math
        if(!math & displaymath) {math <- displaymath}
        # define separator, prefix, suffix
        if (math) {
                prefix <- ifelse(displaymath, "$$", "$")
                suffix <- prefix
                sepcoeff <- ifelse(sep, "\\cdot ", "")
        }else{
                prefix <- ""
                suffix <- ""
                sepcoeff <- ifelse(sep, " ", "") 
        }
        
        # function for constructing the body
        partialeq <- function(x, predname) {
                op <- ifelse(sign(x) >=0, "+", "-")
                paste(paste(op, format(abs(x), digits = digits)),
                      predname,
                      sep = sepcoeff)
        }
        
        bodyeq <- paste( mapply(partialeq, co[-1], predictors[-1]), collapse = " ")
        # complete eq
        equa <- paste(response,
                      "=",
                      format(co[1], digits = digits),
                      bodyeq
        )
        # with prefix and suffix
        paste0(prefix, equa, suffix)
}

#' tests
#' ------

#+ models
mod <- lm(mpg ~ wt + cyl + amf*wt, data = mtcars)
mod2 <- lm(mpg ~ wt + cyl + amf, data = mtcars)

#' And at last the equations:
#' 
#' * for mod1, default = text (`regeq(mod)`): `r regeq(mod)`
#' * for mod1, text , no sep (`regeq(mod, sep = FALSE)`) : `r regeq(mod, sep = FALSE)`
#' * for mod1, math, display = FALSE  
#' (`regeq(mod, math = TRUE)`) : `r regeq(mod, math = TRUE)`
#' * for mod1, math, display = FALSE, no sep (`regeq(mod, math = TRUE, sep = FALSE)`) : `r regeq(mod, math = TRUE, sep = FALSE)`
#' * for mod1, math, display = TRUE (`regeq(mod, math = TRUE, displaymath = TRUE)`) : `r regeq(mod, math = TRUE, displaymath = TRUE)`
#' * for mod1, math, display = TRUE, no sep (`regeq(mod, math = TRUE, displaymath = TRUE, sep = FALSE)` ) : `r regeq(mod, math = TRUE, displaymath = TRUE, sep = FALSE)`  
#' * for mod1, display = TRUE, no sep (`regeq(mod, displaymath = TRUE, sep = FALSE)` ) : `r regeq(mod, displaymath = TRUE, sep = FALSE)`  
#' * for mod2, default = text (`regeq(mod2)`): `r regeq(mod2)`
#' * for mod2, text , no sep (`regeq(mod2, sep = FALSE)`) : `r regeq(mod2, sep = FALSE)`
#' * for mod2, math, display = FALSE  (`regeq(mod2, math = TRUE)`) : `r regeq(mod2, math = TRUE)`
#' * for mod2, math, display = FALSE, no sep (`regeq(mod2)`) : `r regeq(mod2, math = TRUE, sep = FALSE)`
#' * for mod2, math, display = TRUE (`regeq(mod2, math = TRUE, displaymath = TRUE)`) : `r regeq(mod2, math = TRUE, displaymath = TRUE)`
#' * for mod2, math, display = TRUE, no sep (`regeq(mod2, math = TRUE, displaymath = TRUE, sep = FALSE)` ) : `r regeq(mod2, math = TRUE, displaymath = TRUE, sep = FALSE)`  


