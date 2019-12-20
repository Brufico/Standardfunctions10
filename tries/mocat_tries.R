#' ---
#' title : tries for improvement of function mocat
#' subtitle: exploration
#' author: BFC
#' date: "`r format(Sys.Date(), '%d %B %Y')`"
#' ---
#' 


#' 
#' External code 
#' ====
#' 

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

# we can use `oldata` (from enquêt jeunes diplômés)

choicedata <- "oldata" # "oldata" or "testdata"

# olddata----------------------------------------------------------------------
if (choicedata == "oldata"){

datadir <- file.path(getwd(),"data")
datapath <- file.path(datadir, "oldata.csv")

file.exists(datapath)

df <- read.csv(file = datapath, sep = ";")


# nettoyage
# 
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




#' 
#' utilities
#' ========
#' 

displaygraph <- function(one.graph,
                         windevice = defdevice,
                         width = nfigwidth,
                         height = nfigheight ) {
        if (missing(windevice)) {windevice <- NULL}
        if (is.null(windevice)) {
                grid.newpage()
                grid.draw(one.graph)
        } else {
                windows(width = width, height = height)
                grid.newpage()
                grid.draw(one.graph)
                Sys.sleep(defsleep)
                dev.off()
        }
}



#' 
#' travail
#' ====
#' 

mocat1 <- function(dataf, prefix, valvect = NULL, valshort = NULL, valname = NULL,
                   ranksok= c(1, 5) ) {
        variables <- grep(paste0("^" , prefix), colnames(dataf), value = TRUE) # => the relevant cols names
        if (is.null(valvect)) {valvect <- variables}
        if (is.null(valshort)) {valshort <- valvect}
        if (is.null(valname)) {valname <- prefix}
        # verify
        if (length(variables) != length(valvect) |
            length(variables) != length(valshort)){
                error(" mocat1 : argument lengths mismatch")
        }
        corrtable <- data.frame(variable = variables,
                                valvect,
                                valshort) # correspondance table for use in graphs and tables
        
        # Data: keep only useful cols
        dataf <- dataf[ , variables]

        # keep only useful rows
        isuseful <- rep(TRUE, nrow(dataf)) #initialisation
        for(i in 1:nrow(dataf))
        {isuseful[i] <- !all(is.na(dataf[i, ]))}
        dataf <- dataf[isuseful, ]
        # remove incorrect ranks ???
        for (i in 1: nrow(dataf)) {
                for(j in 1:ncol(dataf)) {
                        if (!is.na(dataf[i, j])) {
                                if( dataf[i, j] < min(ranksok) | dataf[i, j] > max(ranksok)) {
                                        dataf[i, j] <- NA
                                }
                        }
                }
        }
             
        
        ncases <- nrow(dataf) # nombre de cas
        
        ## make the graph(s): long format for the ranks dfrm
        lresdf <- melt(dataf)
        lresdf <- nonadf(lresdf,"value") # get rid of NA's
        
        # order the factor in reverse because of coord_flip. useful ?
        #lresdf$variable <- orderfact(lresdf , "variable", orderdesc = FALSE) ## ? useful? NO
        
        # compute % of individuals and citations explicitly, record the variable values in lims
        restable <- group_by(lresdf, variable) %>%
                summarise(nbcit = n(),
                          rangmed = median(value)) %>%
                arrange(desc(nbcit)) %>%
                mutate(percases = 100 * nbcit / ncases,
                       percit = 100 * nbcit / sum(nbcit))
        
        restable$valnames <- vlookup(restable$variable, searchtable = corrtable,
                                     searchcol = "variable", returncol = "valvect")
        restable$shortname <- vlookup(restable$variable, searchtable = corrtable,
                                      searchcol = "variable", returncol = "valshort")
        
        # printable table
        # ptable <- select(restable, valnames, nbcit, percases, percit, rangmed)
        utable <- select(restable, variable, nbcit, percases, percit, rangmed) #useful

        
        lims <- restable$variable # to ensure both plots have the same category order
        graphlabels <- as.character(restable$shortname) # short names, in the same order as lims !! as character !
        names(graphlabels) <- lims # (to be sure and not to depend on order later)
        
        ptable <- select(restable, variable, nbcit, percases, percit, rangmed) #printable
        colnames(ptable) <- c(valname, "citations", "% individus", "% citations", "rang median")
        ptable[[1]] <- graphlabels
        
        
        
        p1 <- ggplot(restable, aes(variable, percases)) +
                geom_bar(stat="identity") +
                scale_x_discrete(limits = rev(lims), labels = graphlabels) + #labels = graphlabels
                labs(y = "% individus", x = valname) +
                coord_flip()
        
        p2 <- ggplot(lresdf, aes(variable, value)) +
                geom_violin() +
                geom_jitter(height = 0.1, width = 0.5,
                            alpha = 0.4, color = "steelblue") +
                geom_point(data = utable, aes(variable,rangmed),
                           color = "red", size = 2) +
                geom_line(data = utable, aes(variable,rangmed, group = 1),
                          color = "red") +
                scale_x_discrete(labels = NULL,
                                 limits=rev(lims)) +
                # scale_y_continuous(limits = c(0.5, 5.5)) +
                labs(x = NULL, y = 'Rang citation') +
                coord_flip()
        
        p3 <- ggplot(lresdf, aes(variable, I(1 / value))) +
                geom_violin() +
                geom_jitter(height = 0.02, width = 0.3,
                            alpha = 0.4, color = "steelblue") +
                geom_point(data = utable, 
                           aes(variable, I(1 / rangmed)),
                           color = "red", size = 4, alpha = 0.5) +
                geom_line(data = utable, 
                          aes(variable, I(1 / rangmed), group = 1),
                          color = "red") +
                scale_y_reverse(breaks = c(1, 0.5, 0.33, 0.25, 0.20),
                                labels = 1:5) +
                # ylim(c(1.1, 0)) +
                scale_x_discrete(labels = NULL,
                                 limits=rev(lims)) +
                labs(x = NULL, y = 'Rang de citation (inversé)') +
                coord_flip()
        
        make.result( name = makeresname(prefix, "mocat"), #modifié
                     funname = "mocat",
                     varnames = c(prefix = prefix),
                     numcases = ncases,
                     ptable = ptable,
                     # plot = quote(multiplot(plot1, plot2, cols = 2)), # autre code possible
                     # plot = quote(multiplot(plot1, plot2,
                     #                        layout = matrix(c(1, 1, 2), nrow = 1, byrow = TRUE))), # code
                     #better
                     plot = cowplot::plot_grid(p1, p2, 
                                               nrow = 1, rel_widths = c(2,1),
                                               align = 'h', axis = 'b'),
                     plot1 = cowplot::plot_grid(p1, p3, 
                                                nrow = 1, rel_widths = c(2,1),
                                                align = 'h', axis = 'b'),
                     # plot1 = p1,
                     plot2 = p2,
                     plot3 = p3
                     )
}



#' 
#' tries
#' =====
#' 

variables <- grep(paste0("^" , "situation_difficultes_"), colnames(df), value = TRUE)
varshort <- c("correspondance projet pro", "Mobilite géographique", 
              "méconnaissance debouches",
              "Manque d'experience", "mise en valeur competences", "Formation pas reconnue", 
              "formation_inadaptée", "salaire insuffisant","autres difficultes")

res <- mocat1(df, prefix = "situation_difficultes_", valvect = variables, valshort = varshort)
# res <- mocat1(df, prefix = "situation_difficultes_")
res$name
res$ptable

res$plot
res$plot1

# ====================================================================


barp <- res$plot1
violinp <- res$plot2




arp <- arrangeGrob(barp,
            violinp,
            layout_matrix =matrix(c(1, 1, 2), nrow = 1, byrow = TRUE)
            )
displaygraph(arp)

cp <- cowplot::plot_grid(barp, violinp, 
                          nrow = 1, rel_widths = c(2,1),
                          align = 'h', axis = 'b')

displaygraph(cp)



# 
# for (i in seq_along(variables)) {
#         table(df[ , variables[2]], useNA = "no")
#         # i <- 1
#         table(df[ , variables[i]], useNA = "no")
#         df[ , variables[i]] <- ifelse(df[ , variables[i]] <= 0, 
#                                       yes = - df[ , variables[i]],
#                                       no = df[ , variables[i]])
#         table(df[ , variables[i]], useNA = "no")    
# }
# 
# 
# for (i in seq_along(variables)) {
#         table(df[ , variables[2]], useNA = "no")
#         # i <- 1
#         table(df[ , variables[i]], useNA = "no")
#         df[ , variables[i]] <- ifelse(df[ , variables[i]] <= 0, 
#                                       yes = - df[ , variables[i]],
#                                       no = df[ , variables[i]])
#         table(df[ , variables[i]], useNA = "no")    
# }



