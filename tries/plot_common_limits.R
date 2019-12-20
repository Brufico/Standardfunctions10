

# Find common limits (histogram + Boxplot ) ------------------------------------
comlimshistbox <- function(histp, boxp, safemargin = 0.05){
        # 1. make limits of both plots
        tb <- ggplot_build(histp)$data[[1]][ , 1:8] # get freq table from histogram
        hminmax <- c(min(tb[ ,"xmin"]),max(tb[ ,"xmax"]) ) #min-max of histogram bins
        build_boxp <- ggplot_build(boxp) #get min-max of boxplot data
        bminmax <- c(build_boxp$data[[1]][1,"ymin_final"], 
                     build_boxp$data[[1]][1,"ymax_final"])
        
        lmtp <- c(bminmax, hminmax) # mixed limits of plots
        halfrng <-  (max(lmtp) - min(lmtp)) / 2 # half range
        crng <- (max(lmtp) + min(lmtp)) / 2 # center of range
        lmtp <- crng + c(-1, 1) * halfrng * (1 + safemargin) # new limits of both plots
        
        # dbg
        # message(paste(round(lmtp[1], 0), round(lmtp[2], 0) ) )
        
        return(lmtp) 
}


# Find common limits (barplot + Boxplot )
comlimsbarbox <- function(barp, boxp, safemargin = 0.05){
        # 1. make limits of both plots
        tbfull <- ggplot_build(barp)$data[[1]]
        # tb <- tbfull[ , 1:8] # get freq table from barplot
        
        hminmax <- c(min(tbfull[ ,"xmin"]), max(tbfull[ ,"xmax"]) ) #min-max of barplot bars
        
        build_boxp <- ggplot_build(boxp) #get min-max of boxplot data
        bminmax <- c(build_boxp$data[[1]][1,"ymin_final"], 
                     build_boxp$data[[1]][1,"ymax_final"])
        
        lmtp <- c(bminmax, hminmax) # mixed limits of plots
        halfrng <-  (max(lmtp) - min(lmtp)) / 2 # half range
        crng <- (max(lmtp) + min(lmtp)) / 2 # center of range
        lmtp <- crng + c(-1, 1) * halfrng * (1 + safemargin) # new limits of both plots
        
        # dbg
        # message(paste(round(lmtp[1], 0), round(lmtp[2], 0) ) )
        
        return(lmtp) 
}






# num1c(new)
num1c <- function(dataf, nomvar, usedensity = FALSE, plot_density = FALSE,
                  fillhist = sfdefault("filldefault"), color_density = "red", digits = 2, # ? modifier
                  bins = "nclass.FD", closed = "left",
                  rel_heights = c(2, 1), safemargin = 0.05,
                  ...) {  # ... = addtl arguments for geom_hist
        if (plot_density) {usedensity <- TRUE} # plot_density overrides usedensity
        # bins = Null, integer, or a function name : "nclass.Sturges", "nclass.FD" , "nclass.scott"
        # get or compute bins (as integer)
        if (!is.null(bins)) {
                if ("character" %in% class(bins) ) {
                        bins <-  do.call(bins, list(nonavect(dataf[[nomvar]])))
                } else {bins <- NULL
                warning("bins is not a function", call. = TRUE)}
        }
        # make histogram (version 1)
        hp <- ggplot(dataf, aes_(as.name(nomvar))) +
                if (usedensity) {geom_histogram(aes(y=..density..),
                                                bins = bins, fill = fillhist,...)
                } else {geom_histogram(bins = bins, fill = fillhist, ...)}
        
        if (plot_density) {hp <- hp + geom_density(color=color_density) }
        
        # make boxplot (version 1)
        boxp <- ggplot(data = dataf, aes_(1, as.name(nomvar))) + 
                geom_boxplot() + coord_flip()
        
        # make summaries vector + get number of cases ------------------------
        s = sumvector(dataf[[nomvar]])
        num = s["n"] # number of cases
        
        # get the frequency table from ggplot ---------------------------------
        tb <- ggplot_build(hp)$data[[1]][ , 1:8]
        # add  columns to it
        tb$rfreq <- tb$count/num
        tb$numlabs <-  paste0("n=", tb$count)
        tb$perclabs <- paste0(100* round(tb$rfreq, digits), "%")
        tb$index <- ave(1:nrow(tb),  FUN = function(x) 1:length(x)) # rank
        # done, compute more info
        cbinw <- unique(round(tb$xmax-tb$xmin,digits)) # get binwidth
        cbreaks <- with(tb, c(xmin[1],xmax)) # get breaks vector from table
        clabs <- mkclabs(cbreaks, closed = closed) # make class lablels
        # make a printable table
        ptb <- data.frame(
                class = clabs,
                center = tb$x,
                freq = tb$count,
                rfreq = tb$rfreq * 100
        )
        
        # Uniform Chi2 test
        uchisq <- try.chisq.test(tb$count)
        # warn if different class widths
        if (length(cbinw) >= 2) {
                warning(paste0("Unif chi2 test ",
                               nomvar,
                               " called with different class widths!",
                               call. = TRUE)) }
        
        # new plots with common limits --------------------------------------
        
        # compute common limits for x scale
        comlims <- comlimshistbox(hp, boxp, safemargin = safemargin) 
        
        # new plots
        newhp <- ggplot(dataf, aes_(as.name(nomvar)))
        if (usedensity) {
                newhp <- newhp + 
                        geom_histogram(aes(y=..density..),
                                       breaks = cbreaks, 
                                       fill = fillhist,
                                       ...)
        } else {
                newhp <- newhp +
                        geom_histogram(breaks = cbreaks, 
                                       fill = fillhist,
                                       ...)
        }
        
        newhp <- newhp + 
                scale_x_continuous(name = NULL,
                                   labels  = NULL,
                                   limits = comlims )
        
        if (plot_density) {newhp <- newhp + geom_density(color=color_density) }
        
        newboxp <- ggplot(data = dataf, aes_(1, as.name(nomvar))) + 
                geom_boxplot(outlier.alpha = 0.5, outlier.colour = "red") + 
                scale_y_continuous(limits = comlims) + 
                scale_x_continuous(name = "", breaks = NULL) +
                coord_flip()
        
        # return values -----------------------------------------------------
        make.result( name = makeresname(nomvar, "num1c"), #modifié
                     funname = num1c,
                     varnames = c(nomvar = nomvar),
                     numcases = num,
                     summaries = s,
                     table = tb,
                     ptable = ptb,
                     details =list(binwidths = cbinw,
                                   breaks = cbreaks,
                                   closed = closed,
                                   limits = comlims),
                     chi2 = uchisq,
                     plot = cowplot::plot_grid(newhp, newboxp, 
                                               ncol = 1, rel_heights = rel_heights,
                                               align = 'v', axis = 'b'),
                     plot1 = hp,
                     plot2 = newhp, # temporaire
                     plot3 = boxp,
                     plot4 = newboxp)# temporaire
}


