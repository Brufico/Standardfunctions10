# 
# Data ========================================================================
# 
# Read in---------------------------------------------------------------------
# df <- read.table(file.path("data", "DCcomics.csv"), 
#                sep = ";",# stringsAsFactors = FALSE, 
#                header = TRUE, na.strings = "")
#                # Does not work

df <- read.delim(file.path("data", "DCcomics.csv"), 
                 sep = ";",# stringsAsFactors = FALSE, 
                 header = TRUE, na.strings = "")

# Verification ---------------------------------------------------------------
colnames(df)
nrow(df)
# 
# tail(df)
# df <- (df[1:(nrow(df) - 1),]) # eliminer la derniere ligne (NA only)
# tail(df)


# main function ===============================================================
library(dplyr)

recode_order <- function(dtable, namefact, namenewfact, listrecode, 
                         lastlevel = FALSE) {
        if (!missing(listrecode)) {
                if (length(listrecode) >= 0) {
                        # recoding
                        recode <- dplyr::recode
                        dtable[[namenewfact]] <- 
                                do.call( "recode",
                                         c(list(.x = dtable[[namefact]]), 
                                           listrecode) )    
                }
                
        }
        
        #ordering by relative frequency
        dtable[[namenewfact]] <- orderfact(dataf = dtable, nomfact = namenewfact)
        # put the "lastlevel" (i.e. "other" catagory) last in the levels
        if (lastlevel != FALSE) {
                dtable[[namenewfact]] <- 
                        factor(dtable[[namenewfact]],
                               levels = c(
                                       setdiff(levels(dtable[[namenewfact]]),
                                               lastlevel),
                                       lastlevel
                               )
                        )
        }
        
        return(dtable)
}

# Helpers ---------------------------------------------------------------------

# generate + print tentative recode function call + verification code
recode_prep <- function(dataf, # must be the name (unquoted) of the dataframe
                        fname, newfname, 
                        min_percent = 0.03, min_label = "Other") {
        dataname <- enexpr(dataf)
        
        ptbl <- prop.table(table(dataf[[fname]] ))
        ptbl <- sort(ptbl, decreasing = TRUE)
        cat(pander::pander(ptbl)) # show initial table
        
        make_recodelist <- function(ptbl, fname, min_percent, min_label) {
                # generate + print tentative list definition
                lv = names(ptbl) # current levels
                labs <- ifelse(ptbl >= min_percent, 
                               names(ptbl), 
                               min_label) # new levels
                # vector of lines i list definition (one for each level)
                vlines <- 
                        paste0('`', 
                               lv, 
                               '`', 
                               ' = "', labs, '"' )
                # '\n')
                
                # add commas + newline at the end of each line save the last
                nbln <- length(vlines)
                eol <- c(rep(',\n', times = nbln-1), '\n')
                vlines <-paste0(vlines, eol) 
                # add begin and end of list
                vlines <- c('list( \n' , vlines, ' ), \n')
                return(vlines)
        }
        # beginning of call
        make_begincode <- function(tablename, namefact, namenewfact){
                c(
                        '# recodage\n',
                        sprintf('%s <- recode_order(\n', 
                                as.character(tablename)),
                        sprintf('dtable = %s ,"%s", "%s",\n listrecode = ',
                                as.character(tablename), 
                                namefact, namenewfact)      
                )

        }
        # end of call + verification
        make_endcode <- function(dataname, newfname, min_label){
                c(sprintf('lastlevel = "%s"', min_label),
                  '\n)\n',
                  '# verification',
                  sprintf('\nrecode_verif(dataf = %s, fname = "%s")\n',
                          as.character(dataname), newfname)
                )
        }
        
        # putting the bits together
        all_lines <- c(
                make_begincode(dataname, fname, newfname),
                make_recodelist(ptbl = ptbl, fname, min_percent, min_label),
                make_endcode(dataname, newfname, min_label)
        )
        
        
        cat(all_lines)
}



recode_verif <- function(dataf, fname) {
        cat("levels : ")
        print(levels(dataf[[fname]]))
        cat("tables : ")
        print(table(dataf[[fname]]))
}

# Applications =================================================================

# HAIR -------------------------------------------------------------------------

recode_prep(dataf = df, fname = "HAIR", min_label = "Other Hair")

df <- recode_order(dtable = df ,"HAIR", "HAIR_1", 
                   listrecode = 
                           list( 
                                   #`Black Hair` = "Black Hair",
                                   #`Brown Hair` = "Brown Hair",
                                   #`Blond Hair` = "Blond Hair",
                                   #`Red Hair` = "Red Hair",
                                   #`White Hair` = "White Hair",
                                   #`Grey Hair` = "Grey Hair",
                                   `Green Hair` = "Other Hair",
                                   `Blue Hair` = "Other Hair",
                                   `Purple Hair` = "Red Hair",
                                   `Strawberry Blond Hair` = "Other Hair",
                                   `Orange Hair` = "Other Hair",
                                   `Pink Hair` = "Other Hair",
                                   `Gold Hair` = "Red Hair",
                                   `Violet Hair` = "Other Hair",
                                   `Reddish Brown Hair` = "Red Hair",
                                   `Silver Hair` = "Grey Hair",
                                   `Platinum Blond Hair` = "Blond Hair"
                           ),
                   lastlevel = "Other Hair"
)

recode_verif(dataf = df, fname = "HAIR_1")


# EYE -------------------------------------------------------------------------

recode_prep(dataf = df, 'EYE', min_percent = 0.03,min_label = "Other Eyes")

df <- recode_order(dtable = df ,"EYE", "EYE_1", 
                   listrecode = 
                           list( 
                                   # `Blue Eyes` = "Blue Eyes",
                                   # `Brown Eyes` = "Brown Eyes",
                                   # `Black Eyes` = "Black Eyes",
                                   # `Green Eyes` = "Green Eyes",
                                   # `Red Eyes` = "Red Eyes",
                                   # `White Eyes` = "White Eyes",
                                   `Yellow Eyes` = "Other Eyes",
                                   `Photocellular Eyes` = "Other Eyes",
                                   `Grey Eyes` = "Other Eyes",
                                   `Hazel Eyes` = "Brown Eyes",
                                   `Purple Eyes` = "Red Eyes",
                                   `Violet Eyes` = "Other Eyes",
                                   `Orange Eyes` = "Other Eyes",
                                   `Gold Eyes` = "Yellow Eyes",
                                   `Auburn Hair` = "Brown Eyes",
                                   `Pink Eyes` = "Other Eyes",
                                   `Amber Eyes` = "Brown Eyes"
                           ) ,
                   lastlevel = "Other Eyes"
)

recode_verif(dataf = df, "EYE_1")

# End -------------------------------------------------------------------------
 