
#  Générer des données de test==================================================


# test data for rankings--------------------------------------------------------

df <- local({
        seed <- 154
        set.seed(seed)
        # parameters
        datalength <- 200
        mranksize <- 9 #("width" of the row of ranks)
        maxrank <- 5 # maximal rank of an item
        prefix <- "pf_"
        shift <- sample(1:5, size = 1) # notimportant , just for forming the 
                                        # names of the items that were ranked
        
        
        # function making sets of rankings (rows of dataset)
        mkranks <- function(size, maxrank){ 
                if (missing(maxrank)) {maxrank <- size}
                lastrank  <-  sample(0:maxrank,size = 1) # pick maximal rank
                rankvect <- 1:lastrank # vector of ranks
                indices <- sample(1:size, # vector of positions in the row
                                  size = lastrank, 
                                  replace = FALSE)
                result <- rep(x = NA, times = size) #make a row full of NA's
                for (j in 1:lastrank){result[indices[j]] <- rankvect[j]} # put the ranks in place
                return(result) #return the row
        }
        
        # building the dataset
        
        # make column of NA's ---------------------------------
        nacol <- rep(NA, times = datalength)
        # column names
        col_names <- paste0(prefix, LETTERS[1:mranksize], letters[(1:mranksize) + shift])
        
        # core dataframe (1 column)
        dfr <- data.frame(nacol)
        # add desired number of similar columns
        for (j in 2:mranksize) {dfr[[j]] <- nacol}
        colnames(dfr) <- col_names
        
        
        # remplacer les lignes de données (NA) 
        # par des vecteurs de rangs---------------------------------
        set.seed(seed)
        for (i in 1:datalength) {
                dfr[i, ] <- mkranks(size = mranksize, maxrank = maxrank)
        }
        # compléter par des colonnes inutiles ---------------------------------
        df <- cbind(data.frame(x = 1:datalength,
                               irrelv1 = letters[sample(1:datalength, 
                                                        datalength, 
                                                        replace = TRUE)]), 
                    dfr, 
                    data.frame( 
                            irrelv2 = LETTERS[sample(1:datalength, 
                                                     datalength, 
                                                     replace = TRUE)],
                            irrelv3 = letters[sample(1:datalength, 
                                                     datalength, 
                                                     replace = TRUE)]
                    )
        )
        
        # sauvegarder
        write.table(x=df, sep = ";", file = file.path("data", "testdf.csv"))
        
        # retourner la dataframe
        df
})









# rdf <-read.table( sep = ";", file = file.path("data", "testdf.csv")) 
# rdf
