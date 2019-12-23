
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
# x$get(name) -> value of name `name`, or NULL
# x$get() -> $get(mydefault)
# 
# x$getall() -> the complete stored list (less the beginning)
# x$init() -> re-initializes tje list
# x$names() -> vector of all names
# x$$ls ==> same as names
# 
# x$getdefault() -> the default name
# x$setdefault() -> the default name
# 
# 

# make object
listobj <- function(default = "mydefault", lobj = list()){
        
        init <- function() {lobj <- list()}
        set <- function(name = default, value) { lobj[[name]] <<- value }
        get <- function(name = default) { lobj[[name]] }
        getvect <-function(name = default){
                if (length(name) <= 1) {
                        get(name)
                }else{
                        lapply(name,
                               FUN = function(nm){ getvect(nm) }
                                       )
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
                set = set,
                get = getvect,
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
uu <- listobj()

names(uu)


uu$set("onoma", value = "zoot")
uu$set("nomba", value = 10000)
uu$set("boulba", value = c(1,2))
uu$set(name = "googla", value = list(one =1, two = 1:2))

uu$get(name = "googla")
uu$get()
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
