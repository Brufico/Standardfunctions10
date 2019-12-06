
#' fonction pour enregistrer les résultats dans une liste
#'=======================================================


#' fonction pour enregistrer les résultats dans une liste
# initialisation: Initialise la liste des résultats et crée les fonctions setresult/getresult
initresults <- function(){
        # initialiser la variable locale liste de résultats
        allresults <- list(Whatsit="List of Results")
        # stocker un résultat, attribué à setresult dans globalenv
        setresult <<- function(rres, rname) {
                if (missing(rname)) {
                        rname <- rres[["name"]]
                }
                allresults[[rname]] <<- rres
        }
        # retrouver une résultat,  attribué à getresult dans globalenv
        getresult <<- function(rname){
                allresults[[rname]]
        }
}
# 



# # tests
# initresults()
# getresult("Whatsit")
# getresult("whatsit")
# 
# setresult(list(name="hoho", thing = "thisisit"))
# getresult("hoho")

