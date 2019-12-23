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
        
        variables <- grep(paste0("^" , prefix), 
                          colnames(dataf), 
                          value = TRUE) # => get the relevant cols names
        if (is.null(valvect)) {valvect <- variables}
        if (is.null(valshort)) {valshort <- valvect}
        if (is.null(valname)) {valname <- prefix}
        
        # verify input---------------------------------------------------------
        if (length(variables) != length(valvect) |
            length(variables) != length(valshort)){
                error(" mocat1 : argument lengths mismatch")
        }
        # utility def ----------------------------------------------------------
        # correspondance table for use in graphs and tables 
        corrtable <- data.frame(variable = variables,
                                valvect,
                                valshort) 
        
        # data manipulation ----------------------------------------------------
        # Data: keep only useful cols
        dataf <- dataf[ , variables]
        
        # remove incorrect ranks
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
        
             
        
        ncases <- nrow(dataf) # nombre de cas
        
        ## long format for the ranks dfrm, for tables and graphs: 
        lresdf <- melt(dataf)
        lresdf <- nonadf(lresdf,"value") # get rid of NA's
        
        # make tables: -------------------------------------------------------
        # 
        # a) individuals and ranks ...........................................
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
        
        # a) citations by ranks ...........................................
        restable0 <-  lresdf  %>%
                group_by(variable) %>%
                summarise(nbcit0 = n()) %>%
                mutate(percit0 = 100 * nbcit0 / sum(nbcit0))
        restable1 <-  filter(lresdf, value == 1) %>%
                group_by(variable) %>%
                summarise(nbcit1 = n()) %>%
                mutate(percit1 = 100 * nbcit1 / sum(nbcit1))
        restable2 <-  filter(lresdf, value == 2) %>%
                group_by(variable) %>%
                summarise(nbcit2 = n()) %>%
                mutate(percit2 = 100 * nbcit2 / sum(nbcit2))
        restable3 <-  filter(lresdf, value >= 3) %>%
                group_by(variable) %>%
                summarise(nbcit3 = n()) %>%
                mutate(percit3 = 100 * nbcit3 / sum(nbcit3))
        
        citetable <- merge(restable0, restable1, by = "variable")
        citetable <- merge(citetable, restable2, by = "variable")
        citetable <- merge(citetable, restable3, by = "variable")
        citetable <- arrange(citetable, desc(nbcit0), desc(nbcit1))
        
        # printable table and graphs preparation ...............................

        utable <- select(restable, variable, nbcit, percases, percit, rangmed) #useful

        
        lims <- restable$variable # to ensure both plots have the same category order
        graphlabels <- as.character(restable$shortname) # short names, in the same order as lims !! as character !
        names(graphlabels) <- lims # (to be sure and not to depend on order later)
        
        ptable <- select(restable, variable, nbcit, percases, percit, rangmed) #printable
        colnames(ptable) <- c(valname, "citations", "% individus", "% citations", "rang median")
        ptable[[1]] <- graphlabels
        
        
        # make graphs ----------------------------------------------------------- 
        # 
        # %individuals + ranks .................................................
        
        barp <- ggplot(restable, aes(variable, percases)) +
                geom_bar(stat="identity") +
                scale_x_discrete(limits = rev(lims), labels = graphlabels) + #labels = graphlabels
                labs(y = "% individus", x = valname) +
                coord_flip()
        
        violinp_lin <- ggplot(lresdf, aes(variable, value)) +
                geom_violin() +
                geom_jitter(height = 0.1, width = 0.5,
                            alpha = 0.4, color = "steelblue") +
                geom_point(data = utable, aes(variable,rangmed),
                           color = "red", size = 4, alpha = 0.4) +
                geom_line(data = utable, aes(variable,rangmed, group = 1),
                          color = "red") +
                scale_x_discrete(labels = NULL,
                                 limits=rev(lims)) +
                # scale_y_continuous(limits = c(0.5, 5.5)) +
                labs(x = NULL, y = 'Rang citation') +
                coord_flip()
        
        violinp_inverse <- ggplot(lresdf, aes(variable, I(1 / value))) +
                geom_violin() +
                geom_jitter(height = 0.02, width = 0.3,
                            alpha = 0.4, color = "steelblue") +
                geom_point(data = utable, 
                           aes(variable, I(1 / rangmed)),
                           color = "red", size = 4, alpha = 0.4) +
                geom_line(data = utable, 
                          aes(variable, I(1 / rangmed), group = 1),
                          color = "red") +
                scale_y_reverse(breaks = 1 / 1:5,  #c(1, 0.5, 0.33, 0.25, 0.20),
                                labels = 1:5) +
                # ylim(c(1.1, 0)) +
                scale_x_discrete(labels = NULL,
                                 limits=rev(lims)) +
                labs(x = NULL, y = 'Rang de citation (inversé)') +
                coord_flip()
        
        # combined plot
        indivrank_cp <- cowplot::plot_grid(barp, violinp_lin, 
                                           nrow = 1, rel_widths = c(2,1),
                                           align = 'h', axis = 'b')
        
        
        # %citations, by ranks .................................................
        
        lims2 <- citetable$variable # to ensure both plots have the same category order
        graphlabels2 <- as.character(restable$shortname) # short names, in the same order as lims !! as character !
        names(graphlabels2) <- lims # (to be sure and not to depend on order later)
        
        barp0 <- ggplot(citetable, aes(variable, percit0)) +
                geom_bar(stat="identity") +
                scale_x_discrete(limits = rev(lims2), 
                                 labels = graphlabels2) + #labels = graphlabels2
                labs(y = "% cit. global", x = valname) +
                coord_flip()
        
        barp1 <- ggplot(citetable, aes(variable, percit1)) +
                geom_bar(stat="identity") +
                scale_x_discrete(NULL,
                        limits = rev(lims2), labels = NULL) + # no labels
                labs(y = "% cit. Rng = 1") +
                coord_flip() 
        
        barp2 <- ggplot(citetable, aes(variable, percit2)) +
                geom_bar(stat="identity") +
                scale_x_discrete(NULL,
                                 limits = rev(lims2), labels = NULL) + # no labels
                labs(y = "% cit. Rng = 2") +
                coord_flip() 
        
        barp3 <- ggplot(citetable, aes(variable, percit2)) +
                geom_bar(stat="identity") +
                scale_x_discrete(NULL,
                                 limits = rev(lims2), labels = NULL) + # no labels
                labs(y = "% cit. Rng >= 3") +
                coord_flip() 
        
        cite_cp <- cowplot::plot_grid(barp0, barp1, barp2, barp3,
                                      nrow = 1, rel_widths = c(2, 1, 1, 1),
                                      align = 'h', axis = 'b')
        
        
        
        # retour résultat------------------------------------------------------
        make.result( name = makeresname(prefix, "mocat"), #modifié
                     funname = "mocat",
                     varnames = c(prefix = prefix),
                     numcases = ncases,
                     ptable = ptable,
                     table1 = citetable,

                     plot = indivrank_cp,
                     plot1 = cite_cp
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

res <- mocat1(df, prefix = "situation_difficultes_", 
              valvect = variables, valshort = varshort)
# res <- mocat1(df, prefix = "situation_difficultes_")
res$name
res$ptable
res$table1
res$plot
res$plot1

# ====================================================================

# 
# barp <- res$plot1
# violinp <- res$plot2
# 
# 
# 
# 
# arp <- arrangeGrob(barp,
#             violinp,
#             layout_matrix =matrix(c(1, 1, 2), nrow = 1, byrow = TRUE)
#             )
# displaygraph(arp)
# 
# cp <- cowplot::plot_grid(barp, violinp, 
#                           nrow = 1, rel_widths = c(2,1),
#                           align = 'h', axis = 'b')
# 
# displaygraph(cp)
# 
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



