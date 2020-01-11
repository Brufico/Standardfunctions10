
df <- res$table1
colnames(df)[1] <- "diffname"
df

library(reshape2)
df1 <- melt(df)

h3 <- function(df) head(df, 3)
h3(df1)

df1$rang <- sapply(as.character(df1$variable),
       FUN= function(x) substr(x,nchar(x),nchar(x))
               )
df1$variable <- sapply(as.character(df1$variable),
                       FUN= function(x) substr(x,1,nchar(x)-1)
)
# names(x) <- NULL
# x

df2 <- dcast(df1,diffname + rang ~ variable )
# reorder by citations
df2$diffname <- orderfact(df2, "diffname", ordervar = "nbcit")

# get short names


# facetnames
facetlabs  <-  c("ensemble", "Rang = 1", "Rang = 2", "Rang = 3+")
names(facetlabs) <- 0:3
        
ggplot(df2, aes(diffname , nbcit)) + 
        geom_col()+ 
        facet_grid(rang ~ ., labeller = labeller(rang = facetlabs)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))

#========================================================================================


library(reshape2)
library(dplyr)
library(grid)
library(gridExtra)
# library(cowplot)

source("standardfunctionsV30.R")


#' 
#' Data
#' ====



#' 
#' Data
#' ====
#' 
#' the data is stored in df

# we can use `oldata` (from enquête jeunes diplômés)

choicedata <- "testdata" # "oldata" or "testdata"

# olddata----------------------------------------------------------------------
if (choicedata == "oldata"){
        
        datadir <- file.path(getwd(),"data")
        datapath <- file.path(datadir, "oldata.csv")
        
        file.exists(datapath)
        
        df <- read.csv(file = datapath, sep = ";")
        
        # nettoyage
        # cleaning cols
        remvar <- grep(paste0("^" , "Date_creation_"), colnames(df), value = FALSE)
        df <- df[ , -remvar]
        remvar <- grep(paste0("^" , "Date_maj_"), colnames(df), value = FALSE)
        df <- df[ , -remvar]
        
}


# or testdata ----------------------------------------------------------------

if (choicedata == "testdata"){
        source(file = file.path("tries", "testdata.R"))
}



# start trying mods for the graphs ===========================================
dataf <- df
prefix <- "pf_"            #"situation_difficultes_"
ranksok <-  c(1, 5)
maxrankgraph <- 3



variables <- grep(paste0("^" , prefix), 
                  colnames(dataf), 
                  value = TRUE) # => get the relevant cols names
varshort <- c("correspondance projet pro", "Mobilite géographique", 
              "méconnaissance debouches",
              "Manque d'experience", "mise en valeur competences", "Formation pas reconnue", 
              "formation_inadaptée", "salaire insuffisant","autres difficultes")

varshort <- substring(variables, nchar(prefix) + 1, 100)
corrtable <- data.frame(variable = variables,
                        valvect = variables,
                        valshort = varshort )

dataf <- dataf[ , variables]

# remove incorrect ranks (outside ranksok)
for (i in 1: nrow(dataf)) {
        for(j in 1:ncol(dataf)) {
                if (!is.na(dataf[i, j])) {
                        if( dataf[i, j] < min(ranksok) | dataf[i, j] > max(ranksok)) {
                                dataf[i, j] <- NA
                        }
                }
        }
}

# keep only useful rows
isuseful <- rep(TRUE, nrow(dataf)) #initialisation
for(i in 1:nrow(dataf))
{isuseful[i] <- !all(is.na(dataf[i, ]))}
dataf <- dataf[isuseful, ]



ncases <- nrow(dataf) # nombre de cas (citations)

## long format for the ranks dfrm, for tables and graphs: 
lresdf <- melt(dataf)
lresdf <- nonadf(lresdf,"value") # get rid of NA's



# barchart flipped + violin----------------------------------------------------
# data changes




bpbcount <- ggplot(lresdf, aes(variable, fill = rang)) + 
        geom_bar()







# barchart faceted by rank  ----------------------------------------------------


# modif rangs
# 
lresdf$rang <-factor(sapply(lresdf$value, 
                            function(x) {
                                    ifelse(x < maxrankgraph, 
                                           yes = as.character(x), 
                                           no = paste0(as.character(maxrankgraph),"+" 
                                                  ))}))

lresdf [["variable"]] <- orderfact(lresdf, "variable")

levels(lresdf [["variable"]])

graphlabs <- as.character(vlookup(levels(lresdf [["variable"]]), searchtable = corrtable,
                                  searchcol = "variable", returncol = "valshort")) 

# short names, in the same order as lims !! as character !
# 

# facetnames
rankvalues <- c(as.character(1:(maxrankgraph - 1)), 
                paste0(as.character(maxrankgraph),"+")) 
facetlabs  <-  c("Ensemble", paste("Rang =", rankvalues ))
names(facetlabs) <- c("(all)", rankvalues)

# counts = y
bpbcount <- ggplot(lresdf, aes(variable, fill = rang)) + 
        geom_bar()
# proportion = y
bpbprop <- ggplot(lresdf, aes(variable, fill = rang, y = ..prop..,group = rang )) + 
        geom_bar()

prop_y <- TRUE
if(prop_y) {bpb <- bpbprop
}else{bpb <- bpbcount}


bpb +
        facet_grid(rang ~ ., margins = TRUE, switch = "y", labeller = labeller(rang = facetlabs)) +
        labs(title = "Citations")+
        scale_x_discrete("source de difficultés",
                         limits = levels(lresdf [["variable"]]),
                         labels = graphlabs) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
              legend.position = "none", plot.title = element_text(hjust = 0.5))

