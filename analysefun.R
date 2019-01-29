
# Faire une analyse de certaines variables d'une dataframe, dans l'ordre indiqué

analyse <- function(df, vars, type ) {
        # vars = vecteur de noms de variables à analyser en séquence 
        # type = vecteur de types de variables, type = "cat", "numd", "numc"
        
        #verification
        print(vars %in% colnames(df))
}



################# tests 
size <- 30
set.seed(18)
datf <- data.frame(x = 1:size, 
                 y = factor(sample(x = LETTERS, replace = TRUE, size = size )),
                 z = factor(sample(x = letters, replace = TRUE, size = size )),
                 t = rnorm(size),
                 g = factor(sample(x=c("g", "f"), replace = TRUE, size = size)))
