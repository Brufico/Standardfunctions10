
# make storage list of objects
# 
# x <-  listobj(startlistname = `start@@list`) 
#       ou listobj(firstname = firstvalue) ??? non
# 
#       à l'intérieur : 
#       startlistname <- `start@@list`
#       default = "mydefault"
#       lobj <-  list(`Start@@list` = "start")
# 
# x$set(name, value) ==> set or add one named value
# x$get(name) -> value of name `name`, or NULL (works also with char vector of names)
# x$get() -> $get(mydefault)
# 
# x$getall() -> the complete stored list (less the beginning)
# x$init() -> re-initializes tje list
# x$ls() -> vector of all names
# 
# 
# x$getdefault() -> the default name
# x$setdefault() -> the default name
# 
# 

# make object
listobj <- function(default = "mydefault", 
                    legalnames = NULL, #either NULL or character vector of 
                                        #legal names (set at creation only)
                    objname = NULL, # the storing object id name 
                                        # (set at creation only)
                    lobj = list()){
        # if needed verify 1 name
        
        if (is.null(legalnames)){
                verifyname <- function(name){
                        if (name == "") {stop("name cannot be empty string")}
                }
        }else{
                if(!(default %in% legalnames)){
                        legalnames <- c(default, legalnames)  
                }
                verifyname <- function(name){
                        if (name == "") {stop("name cannot be empty string")}
                        if (!(name %in% legalnames)){
                                warning(paste(objname, "Illegal name: ", name))
                        }
                }
        }
        getlegalnames <- function() legalnames  
        getobjectname <- function() objname
        init <- function() {lobj <- list()}
        set <- function(name = default, value) { 
                verifyname(name)
                lobj[[name]] <<- value 
        }
        setlist <- function(...){ #syntax : setlist(a = truc, b = chose) ** exporter
                listargs <- list(...)
                llist <- length(listargs)
                nl <- names(listargs)
                if (llist == 0) {stop("liste d'arguments vide")}
                if (llist == 1) {
                        set(name = names(listargs)[1], value = listargs[1])
                }else if (llist > 1) {
                        mapply(FUN = set,
                               nl,
                               listargs)
                }
        }
        get <- function(name = default) { lobj[[name]] }
        getlist <-function(name = default){
                if (length(name) <= 1) {
                        get(name)
                }else{
                        res <- lapply(name,
                               FUN = function(nm){
                                       get(nm) # getlist(nm) 
                               }
                        )
                        names(res) <- name
                        res
                }
        } 
        getall <- function() { lobj }
        remove <- function(name) { lobj[[name]] <<- NULL }
        ls <- function() { names(lobj) }
        getdefault <- function() default
        setdefault <- function(value) default <<- value
        listlength <- function() length(lobj)
        
        helpme <- function(name = "rephrase") {
                message(switch(EXPR = name,
                               init = "init() => empty storage",
                               set = "set(name, value) => store value under name",
                               get = "get(name) => gets stored value",
                               getall = "getall() => gets list of all stored values",
                               legalnames = 'returns vector of legal names',
                               
                               length = "length() => number of stored values",
                               remove = "remove(name) => removes stored value (same as rm())",
                               rm = "rm(name) => removes stored value  (same as remove())",
                               ls = "ls() => lists all names of stored values",
                               rephrase = {paste0 (
                                       "Functions: ",
                                       paste(names(funlist), collapse = "; " ),
                                       " -- ",
                                       "Try again $helpme(myfun)")
                               },
                               "No help here yet"
                               ) )
        }
        
        funlist <- list(
                init = init,
                legalnames = getlegalnames,
                set = set,
                setlist = setlist,
                get = get,
                getlist = getlist,
                getall = getall,
                
                length = listlength,
                remove = remove,
                rm = remove,
                ls = ls,
                
                setdefault = setdefault,
                getdefault = getdefault,
                helpme = helpme
        )
        
        return(funlist)
}



# tries
uu <- listobj(objname = "Tryit")

names(uu)


uu$set("onoma", value = "zoot")

uu$setlist(nomba = 10000)
uu$setlist(boulba = c(1,2), 
           googla = list(one = 1, two = 1:2))


uu$get(name = "googla")
uu$get("nomba")
str(uu$get("nomba"))
uu$get()
res = uu$getlist(name = c("boulba", "nomba"))
res

str(res$nomba)

uu$ls()
uu$getall()

# get by position ?? unhealthy
uu$get(1)
uu$get(3)
uu$get(4)

uu$get(c("onoma", "nomba"))
uu$get(c("onoma", "googla"))
uu$setdefault("nomba")
uu$getdefault()
uu$get()

uu$helpme("ls")
uu$helpme()


vv<- listobj(objname = "VV", 
               legalnames = c("aa", "bb", "xx"),
               default ="xx" 
               )
vv$set("aa", 1)
vv$set("bb", 1:2)
vv$set("cc", 10:30)
vv$set("xx", 20:10)

vv$ls()
vv$rm("cc")
vv$ls()
