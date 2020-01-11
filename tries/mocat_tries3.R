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

# we can use `oldata` (from enquête jeunes diplômés)

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
                   fill = sfdefault("filldefault"), colorannots = sfdefault("colorannots1"),
                   rankscale = "log", # "normal", "inverse", "log"
                   ranksok= c(1, 5), # elimination des valeurs aberrantes 
                   maxrankgraph = 3 ) {
        
        variables <- grep(paste0("^" , prefix), 
                          colnames(dataf), 
                          value = TRUE) # => get the relevant cols names
        if (is.null(valvect)) {valvect <- variables}
        if (is.null(valshort)) {
                valshort <- substring(variables, nchar(prefix) + 1, 100)
                }
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
        

        ncases <- nrow(dataf) # nombre de cas (individus répondants)
        
        ## long format for the ranks dfrm, for tables and graphs: 
        lresdf <- reshape2::melt(dataf)
        lresdf <- nonadf(lresdf,"value") # get rid of NA's
        
        # make tables: -------------------------------------------------------
        # 
        # a) individuals and ranks ...........................................
        # compute % of individuals and citations explicitly, 
        # record the variable values in lims
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
        
        # b) citations by ranks ...........................................
        # Rem : NOT ideal, to be improved with tidy eval
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
# 
#         # citetable <- merge(restable0, restable1, by = "variable")
#         # citetable <- merge(citetable, restable2, by = "variable")
#         # citetable <- merge(citetable, restable3, by = "variable")
#         # citetable <- arrange(citetable, desc(nbcit0), desc(nbcit1))

        # better
        citetable <- plyr::join_all(list(restable0, restable1, restable2, restable3),
                                    by = "variable")
        
        # 
        # # new summarising function (the whole thing is NOT WORKING)
        # tbrank <- function(i, maxrank){
        #         nbcitname <- paste0("nbcit", as.character(i))
        #         percitname <- paste0("percit", as.character(i))
        #         # filter data for cerain ranks
        #         if (i == 0){
        #                 message(0)
        #                 fildf <- lresdf
        #         }else if (i >= maxrank) {
        #                 message("maxrank")
        #                 fildf <- filter(lresdf, value >= !!maxrank)
        #         }else{
        #                 message( "NORMAL")
        #                 fildf <- filter(lresdf, value == i)
        #         }
        #         ~# compute table
        #         fildf %>%
        #                 group_by(variable) %>%
        #                 summarise(!!nbcitname := n()) %>% 
        #                 mutate(!!percitname := 100 * !!as.name(nbcitname) / sum(!!as.name(nbcitname)))
        # }
        # 
        # 
        # list_table <- local(
        #         {
        #             lapply(0:maxrankgraph,
        #                    tbrank,
        #                    maxrankgraph)    
        #         }
        # )
        # 
        # 
        # citetable <- plyr::join_all(list_table, by = "variable") # FAILS HERE*************
        # # citetable <- plyr::join_all(list(restable0, restable1, restable2, restable3))
        
        
        
        
        # printable table and graphs preparation ...............................

        utable <- select(restable, variable, nbcit, percases, percit, rangmed) #useful

        
        lims <- restable$variable # to ensure both plots have the same category order
        graphlabels <- as.character(restable$shortname) # short names, in the same order as lims !! as character !
        names(graphlabels) <- lims # (to be sure and not to depend on order later)
        
        ptable <- select(restable, variable, nbcit, percases, percit, rangmed) #printable
        colnames(ptable) <- c(valname, "citations", "%individus", "%citations", "rang median")
        ptable[[1]] <- graphlabels
        
        
        # make graphs ----------------------------------------------------------- 
        # 
        # %individuals + ranks .................................................
        
        barp <- ggplot(restable, aes(variable, percases)) +
                geom_bar(stat="identity", fill = fill) +
                scale_x_discrete(limits = rev(lims), labels = graphlabels) + 
                labs(y = "% individus", x = valname) +
                coord_flip()
        
        violinp_lin <- ggplot(lresdf, aes(variable, value)) +
                geom_violin() +
                geom_jitter(height = 0.03, width = 0.3,
                            alpha = 0.4, color = "steelblue") +
                geom_point(data = utable, aes(variable,rangmed),
                           color = "red", size = 4, alpha = 0.4) +
                geom_line(data = utable, aes(variable,rangmed, group = 1),
                          color = colorannots, alpha = 0.4) +
                scale_x_discrete(labels = NULL,
                                 limits=rev(lims)) +
                # scale_y_continuous(limits = c(0.5, 5.5)) +
                labs(x = NULL, y = 'Rang citation') +
                coord_flip()
        
        # violinp_inverse <- ggplot(lresdf, aes(variable, I(1 / value))) +
        #         geom_violin() +
        #         geom_jitter(height = 0.02, width = 0.3,
        #                     alpha = 0.4, color = "steelblue") +
        #         geom_point(data = utable, 
        #                    aes(variable, I(1 / rangmed)),
        #                    color = "red", size = 4, alpha = 0.4) +
        #         geom_line(data = utable, 
        #                   aes(variable, I(1 / rangmed), group = 1),
        #                   color = "red") +
        #         scale_y_reverse(breaks = 1 / 1:5,  #c(1, 0.5, 0.33, 0.25, 0.20),
        #                         labels = 1:5) +
        #         # ylim(c(1.1, 0)) +
        #         scale_x_discrete(labels = NULL,
        #                          limits=rev(lims)) +
        #         labs(x = NULL, y = 'Rang de citation (inversé)') +
        #         coord_flip()
        violinp_inverse <- violinp_lin +
                scale_y_continuous( trans = 'reciprocal',
                        breaks = c(1:5),  #c(1, 0.5, 0.33, 0.25, 0.20),
                                labels = 1:5) +
                labs(y = 'Rang de citation (inversé)')
        violinp <- switch(rankscale,
                          normal = violinp_lin,
                          inverse = violinp_inverse, # incorrect for the moment
                          log = violinp_lin + 
                                  scale_y_log10(breaks = 1:5 ,
                                                     labels = 1:5),
                          violinp_lin
        )
        
        # violinp <- violinp_lin
        # violinp <- violinp_inverse
        # 
        # combined plot
        indivrank_cp <- cowplot::plot_grid(barp, violinp, 
                                           nrow = 1, rel_widths = c(2,1),
                                           align = 'h', axis = 'b')
        
        
        # %citations, by ranks .................................................
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
        
        # facetnames
        rankvalues <- c(as.character(1:(maxrankgraph - 1)), 
                        paste0(as.character(maxrankgraph),"+")) 
        facetlabs  <-  c("Ensemble", paste("Rang =", rankvalues ))
        names(facetlabs) <- c("(all)", rankvalues)
        
        # =================================================================================**************
        # barplots with y = counts // proportions (generate both plots)
        
        bpbcount <- ggplot(lresdf, aes(variable, fill = rang))
        
        # proportion = y
        bpbprop <- ggplot(lresdf, aes(variable, fill = rang, y = ..prop..,group = rang )) 
        
        morebar <- function(p, titre = "citations", xscalename = NULL) {
                p +
                        geom_bar() +
                        facet_grid(rang ~ ., margins = TRUE, switch = "y", labeller = labeller(rang = facetlabs)) +
                        labs(title = titre) + 
                        scale_x_discrete(xscalename,
                                         limits = levels(lresdf [["variable"]]),
                                         labels = graphlabs) +
                        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3),
                              legend.position = "none", plot.title = element_text(hjust = 0.5))
        }
                
        barfacetprop <- morebar(p = bpbprop)
        barfacetcount <- morebar(p = bpbcount)
        
        
        
        
        
        # retour résultat------------------------------------------------------
        make.result( name = makeresname(prefix, "mocat"), #modifié
                     funname = "mocat",
                     varnames = c(prefix = prefix),
                     numcases = ncases,
                     ptable = ptable,
                     table1 = citetable,

                     plot = indivrank_cp,
                     plot1 = barfacetcount,
                     plot2 = barfacetprop
                     )
}


# tests ===============================================================
#' 
#' tries, 
#' =====
#' 

variables <- grep(paste0("^" , "situation_difficultes_"), colnames(df), value = TRUE)
varshort <- c("correspondance projet pro", "Mobilite géographique", 
              "méconnaissance debouches",
              "Manque d'experience", "mise en valeur competences", "Formation pas reconnue", 
              "formation_inadaptée", "salaire insuffisant","autres difficultes")

res <- mocat1(df, prefix = "situation_difficultes_" #,
              # valvect = variables , 
              # valshort = varshort,
              # rankscale = "normal"
              )
# res <- mocat1(df, prefix = "situation_difficultes_")
res$name
res$ptable
res$table1
res$plot
res$plot1
res$plot2
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



