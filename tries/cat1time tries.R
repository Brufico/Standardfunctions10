data("economics")

head(economics)
head (economics_long)

# geom_line() is suitable for time series
ggplot(economics, aes(date, unemploy)) + geom_line()
ggplot(economics_long, aes(date, value01, colour = variable)) +
        geom_line()

# geom_step() is useful when you want to highlight exactly when
# the y value changes
recent <- economics[economics$date > as.Date("2013-01-01"), ]
ggplot(recent, aes(date, unemploy)) + geom_line()
ggplot(recent, aes(date, unemploy)) + geom_step()

# geom_path lets you explore how two variables are related over time,
# e.g. unemployment and personal savings rate
m <- ggplot(economics, aes(unemploy/pop, psavert))
m + geom_path()
m + geom_path(aes(colour = as.numeric(date)))

# Changing parameters ----------------------------------------------
ggplot(economics, aes(date, unemploy)) +
        geom_line(colour = "red")

# Use the arrow parameter to add an arrow to the line
# See ?arrow for more details
c <- ggplot(economics, aes(x = date, y = pop))
c + geom_line(arrow = arrow())
c + geom_line(
        arrow = arrow(angle = 15, ends = "both", type = "closed")
)

# Control line join parameters
df <- data.frame(x = 1:3, y = c(4, 1, 9))
base <- ggplot(df, aes(x, y))
base + geom_path(size = 10)
base + geom_path(size = 10, lineend = "round")
base + geom_path(size = 10, linejoin = "mitre", lineend = "butt")

# You can use NAs to break the line.
df <- data.frame(x = 1:5, y = c(1, 2, NA, 4, 5))
ggplot(df, aes(x, y)) + geom_point() + geom_line()


# Setting line type vs colour/size
# Line type needs to be applied to a line as a whole, so it can
# not be used with colour or size that vary across a line
x <- seq(0.01, .99, length.out = 100)
df <- data.frame(
        x = rep(x, 2),
        y = c(qlogis(x), 2 * qlogis(x)),
        group = rep(c("a","b"),
                    each = 100)
)
p <- ggplot(df, aes(x=x, y=y, group=group))
# These work
p + geom_line(linetype = 2)
p + geom_line(aes(colour = group), linetype = 2)
p + geom_line(aes(colour = x))
# But this doesn't
should_stop(p + geom_line(aes(colour = x), linetype=2))


# tries-------------------------------------------------------------------------


dftest <- 
        local({
                df_wide <- df2
                varname <- "HAIR"
                timevarname <- "YEAR"
                useNA <-  "yes"
                
                #value
                dfl <- df_wide %>%
                        dplyr::select(c(.data[[timevarname]], 
                                        .data[[varname]])) %>%
                nonadf(varname, useNA=useNA )
                
        })

sum(is.na(dftest[["HAIR"]]))

# TODO : remplacer useNA, par na_level = FALSE ou "unknown"


# NEW VERSION =================================================================

cat1_time <- function(df_wide, varname, timevarname, cumulative, # required
                      yvar = "prop", # "prop" ou "count" ou "count_total"
                      na_level = FALSE,  # or "Unknown" level
                      useNA = "no", # (default; or "yes")
                      line_size = FALSE, # size of main geom_line (no incidence if false) 
                      smoother = "loess", # ou autre methode, ou FALSE
                      smoothersize = 2,
                      se = TRUE,
                      se_alpha = 0.1,
                      smooth_alpha = 0.4,
                      ...) { # other smoother arguments: span, ...
        
        # helper: add factor level to replace NA's----------
        explicit_NA <- function(dfrm, varname, na_level = FALSE) {
                if(na_level != FALSE){
                        dfrm[[varname]] <- 
                                forcats::fct_explicit_na(f = dfrm[[varname]]
                                                         , na_level)
                }
                return(dfrm)
        }
        
                
        # work on data: make it long -----------------------------
        make_long <- function(df_wide, varname, timevarname, na_level) {
                
                # Allow or filter out NA's 
                if (na_level == FALSE) {
                        # don't change useNA: yes = yes, no = no
                } else {
                        useNA <- "no"
                }
                
                return( 
                        df_wide %>%
                                dplyr::select(c(.data[[timevarname]], 
                                                .data[[varname]])) %>%
                                explicit_NA(varname, na_level) %>%
                                nonadf(varname, useNA=useNA) %>%
                                arrange(.data[[timevarname]], 
                                        .data[[varname]]) %>%
                                group_by(.data[[timevarname]], 
                                         .data[[varname]]) %>%
                                dplyr::summarize(n = n()) %>%
                                ungroup() %>%
                                # add "missing" rows
                                tidyr::complete(.data[[timevarname]],
                                                .data[[varname]], 
                                                fill = list(n = 0)) %>% 
                ###########     # nonadf(varname, useNA=useNA) %>% 
                                group_by(.data[[timevarname]]) %>%
                                dplyr::mutate(total = sum(n)) %>%
                                mutate(proportion = n / total) %>%
                                ungroup() %>%                # inutile?
                                group_by(.data[[varname]]) %>%
                                mutate(cum_n = cumsum(n)) %>% #cum.nb indiv in group
                                ungroup() %>%
                                group_by(.data[[timevarname]]) %>%
                                # cum. nb indiv. /all  levels
                                mutate(cum_total = sum(cum_n)) %>% 
                                # proportion cum.nb indiv in level / total
                                mutate(cum_proportion = cum_n / cum_total) 
                        
                )
        }
        
        # plotting
        make_time_plot <- function(df_long, varname, timevarname, cumulative, 
                                   # required
                                   yvar = "prop", #"prop"/"count"/"count_total"
                                   useNA ,
                                   line_size = FALSE,
                                   smoother = "loess", #ou autre methd, ou FALSE
                                   smoothersize = 2,
                                   se = TRUE,
                                   se_alpha = 0.1,
                                   smooth_alpha = 0.4,
                                   ...) { # other smoother arguments: span, ...)
                
                # message(sprintf("smoother = %s", as.character(smoother) ))
                
                # select output variables
                if (cumulative) {
                        count <-  "cum_n"
                        totalcount <- "cum_total"
                        proportion <- "cum_proportion"
                } else {
                        count <-  "n"
                        totalcount <- "total"
                        proportion <- "proportion"
                }
                # baseplot helper:
                geom_myline <- function(line_size)
                        if (line_size == FALSE) {
                                geom_line()
                        } else {
                                geom_line(size = line_size)
                        }
                
                # make_baseplot
                if ( yvar == "prop") {
                        timeplot <- ggplot(data = df_long, 
                                           aes(x = !!as.name(timevarname),
                                               y =  !!as.name(proportion) ,
                                               color = !!as.name(varname) ) ) + 
                                geom_myline(line_size = line_size)
                } else if (yvar == "count") {
                        timeplot <- ggplot(data = df_long, 
                                           aes(x = !!as.name(timevarname),
                                               y =  !!as.name(count) ,
                                               color = !!as.name(varname) ) ) + 
                                geom_line()
                } else if (yvar == "count_total"){
                        timeplot <- ggplot(data = df_long, 
                                           aes(x = !!as.name(timevarname),
                                               y =  !!as.name(count) ,
                                               color = !!as.name(varname) ) ) + 
                                geom_line() + 
                                geom_line(aes(y = !!as.name(totalcount), 
                                              color = "Total"))
                } else {
                        warning(sprintf(paste0("cat1_time: mauvaise",
                                               " spécification de yvar: %s"), 
                                        yvar))
                        timeplot <- ggplot(data = df_long, 
                                           aes(x = !!as.name(timevarname),
                                               y =  Proportion ,
                                               color = !!as.name(varname) ) ) + 
                                geom_line()
                }
                
                # smoother and se
                if (!is.null(smoother)) {
                        if(!is.na(smoother) && smoother != FALSE) {
                                if (se != FALSE) {
                                        # add se
                                        timeplot <- timeplot +
                                                geom_ribbon(
                                                        stat='smooth', 
                                                        method = smoother, 
                                                        se=TRUE, 
                                                        alpha = se_alpha, 
                                                        aes(color = NULL,
                                                            fill = !!as.name(varname),
                                                            group = !!as.name(varname)),
                                                        show.legend = FALSE,
                                                        ... )
                                }                    
                                # add smoother line
                                timeplot <- timeplot +
                                        geom_line(stat='smooth', 
                                                  method = smoother,
                                                  size = smoothersize, 
                                                  alpha =smooth_alpha,
                                                  show.legend = FALSE,
                                                  ...)
                        }
                }
        
        return(timeplot + 
                       guides(color = guide_legend(override.aes = 
                                                           list(alpha = 1, 
                                                                size = smoothersize)))
        )
        }
        
        return(make_time_plot(make_long(df_wide, varname, timevarname, 
                                        na_level),
                              varname = varname, 
                              timevarname = timevarname, 
                              cumulative = cumulative, 
                              yvar = yvar,
                              line_size = line_size,
                              smoother = smoother,
                              smoothersize = smoothersize,
                              se = se,
                              se_alpha =se_alpha,
                              smooth_alpha = smooth_alpha
                              )
               )
}


# test ========================================================================
cat1_time(df2, varname = "ALIGN", timevarname = "YEAR", cumulative = FALSE)
cat1_time(df2, varname = "ALIGN", timevarname = "YEAR", cumulative = FALSE, 
          na_level = "(Unknown Orientation)")
cat1_time(df2, varname = "ALIGN", timevarname = "YEAR", cumulative = TRUE)
cat1_time(df2, varname = "ALIGN", timevarname = "YEAR", cumulative = TRUE, 
          na_level = "(Unknown Orientation)")
cat1_time(df2, varname = "ALIGN", timevarname = "YEAR", cumulative = TRUE, 
          na_level = "(Unknown Orientation)", smoother = FALSE)
cat1_time(df2, varname = "ALIGN", timevarname = "YEAR", cumulative = TRUE, 
          useNA = "no", smoother = FALSE) # removed missing
cat1_time(df2, varname = "ALIGN", timevarname = "YEAR", cumulative = TRUE, 
          useNA = "no", line_size = 1, smoother = FALSE) # removed missing

cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", yvar = "count", cumulative = FALSE)
cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", yvar = "count", cumulative = TRUE)
cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", 
           yvar = "count_total", smoothersize = 1, cumulative = FALSE)
cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", 
           yvar = "count_total", smoothersize = 1, cumulative = TRUE)

cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", 
           yvar = "count_total", smoother = FALSE, smoothersize = 1, cumulative = TRUE)

cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", 
           yvar = "totalcount_")
cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", alpha = 0.2, cumulative = TRUE)
cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", smoother = NULL)
cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", 
           smoother = "lm", 
           formula = y ~ splines::bs(x, 3),
           cumulative = )

cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", cumulative = FALSE, span = 0.4)

cat1_time (df2, varname = "SEX", timevarname = "YEAR", cumulative = FALSE) 
cat1_time (df2, varname = "SEX", timevarname = "YEAR", cumulative = TRUE) 
cat1_time (df2, varname = "SEX", timevarname = "YEAR",
           smoother = FALSE, cumulative = TRUE) 

cat1_time (df2, varname = "HAIR", timevarname = "YEAR", cumulative = FALSE,
           na_level = "(Missing)") ## too many categories
cat1_time (df2, varname = "HAIR", timevarname = "YEAR", cumulative = TRUE)

cat1_time (df2, varname = "EYE", timevarname = "YEAR", 
           na_level = "Unknown Eyes",cumulative = FALSE) ## too many categories
cat1_time (df2, varname = "EYE", timevarname = "YEAR", cumulative = TRUE)


cat1_time(df2, varname = "GSM", timevarname = "YEAR",  cumulative = FALSE,
           na_level = "Unknown",
           smoother = FALSE, smoothersize=.5, se = FALSE)

cat1_time(df2, varname = "GSM", timevarname = "YEAR",  cumulative = FALSE,
           na_level = "Straight characters",
           smoother = FALSE, smoothersize=.5, se = FALSE)

cat1_time(df2, varname = "GSM", timevarname = "YEAR", cumulative = TRUE,
           na_level = "Straight characters",
           smoother = FALSE, #smoothersize=.5, 
           se = FALSE)


# REM    =====================================================================


# forcats::fct_explicit_na

table(df[["ALIGN"]], useNA = "ifany")


# avant modif ==================================================================


cat1_time <- function(df_wide, varname, timevarname, cumulative, # required
                      yvar = "prop", # "prop" ou "count" ou "count_total"
                      useNA = "no",
                      smoother = "loess", # ou autre methode, ou FALSE
                      smoothersize = 2,
                      se = TRUE,
                      se_alpha = 0.1,
                      line_alpha = 0.4,
                      ...) { # other smoother arguments: span, ...
        
        # work on data: make it long
        make_long <- function(df_wide, varname, timevarname, useNA) {
                return( 
                        df_wide %>%
                                dplyr::select(c(.data[[timevarname]], 
                                                .data[[varname]])) %>%
                                nonadf(varname, useNA=useNA) %>%
                                arrange(.data[[timevarname]], 
                                        .data[[varname]]) %>%
                                group_by(.data[[timevarname]], 
                                         .data[[varname]]) %>%
                                dplyr::summarize(n = n()) %>%
                                ungroup() %>%
                                # add "missing" rows
                                tidyr::complete(.data[[timevarname]],
                                                .data[[varname]], 
                                                fill = list(n = 0)) %>% 
                                group_by(.data[[timevarname]]) %>%
                                dplyr::mutate(total = sum(n)) %>%
                                mutate(proportion = n / total) %>%
                                ungroup() %>%                # inutile?
                                group_by(.data[[varname]]) %>%
                                mutate(cum_n = cumsum(n)) %>% #cum.nb indiv in group
                                ungroup() %>%
                                group_by(.data[[timevarname]]) %>%
                                # cum. nb indiv. /all  levels
                                mutate(cum_total = sum(cum_n)) %>% 
                                # proportion cum.nb indiv in level / total
                                mutate(cum_proportion = cum_n / cum_total) 
                        
                )
        }
        
        # plotting
        make_time_plot <- function(df_long, varname, timevarname, cumulative, 
                                   # required
                                   yvar = "prop", #"prop"/"count"/"count_total"
                                   useNA ,
                                   smoother = "loess", #ou autre methd, ou FALSE
                                   smoothersize = 2,
                                   se = TRUE,
                                   se_alpha = 0.1,
                                   line_alpha = 0.4,
                                   ...) { # other smoother arguments: span, ...)
                # select output variables
                if (cumulative) {
                        count <-  "cum_n"
                        totalcount <- "cum_total"
                        proportion <- "cum_proportion"
                } else {
                        count <-  "n"
                        totalcount <- "total"
                        proportion <- "proportion"
                }
                
                # make_baseplot
                if ( yvar == "prop") {
                        timeplot <- ggplot(data = df_long, 
                                           aes(x = !!as.name(timevarname),
                                               y =  !!as.name(proportion) ,
                                               color = !!as.name(varname) ) ) + 
                                geom_line()
                } else if (yvar == "count") {
                        timeplot <- ggplot(data = df2_y, 
                                           aes(x = !!as.name(timevarname),
                                               y =  !!as.name(count) ,
                                               color = !!as.name(varname) ) ) + 
                                geom_line()
                } else if (yvar == "count_total"){
                        timeplot <- ggplot(data = df2_y, 
                                           aes(x = !!as.name(timevarname),
                                               y =  !!as.name(count) ,
                                               color = !!as.name(varname) ) ) + 
                                geom_line() + 
                                geom_line(aes(y = !!as.name(totalcount), 
                                              color = "Total"))
                } else {
                        warning(sprintf(paste0("cat1_time: mauvaise",
                                               " spécification de yvar: %s"), 
                                        yvar))
                        timeplot <- ggplot(data = df2_y, 
                                           aes(x = !!as.name(timevarname),
                                               y =  Proportion ,
                                               color = !!as.name(varname) ) ) + 
                                geom_line()
                }
                
                # smoother and se
                if (!is.null(smoother)) {
                        if(!is.na(smoother)) {
                                if (smoother != FALSE) {
                                        if (se != FALSE) {
                                                # add se
                                                timeplot <- timeplot +
                                                        geom_ribbon(
                                                                stat='smooth', 
                                                                method = smoother, 
                                                                se=TRUE, 
                                                                alpha = se_alpha, 
                                                                aes(color = NULL,
                                                                    fill = !!as.name(varname),
                                                                    group = !!as.name(varname)),
                                                                show.legend = FALSE,
                                                                ... )
                                        }                    
                                        # add smoother line
                                        timeplot <- timeplot +
                                                geom_line(stat='smooth', 
                                                          method = smoother,
                                                          size = smoothersize, 
                                                          alpha=line_alpha,
                                                          show.legend = FALSE,
                                                          ...)
                                }
                        }
                }  
                
                return(timeplot + 
                               guides(color = guide_legend(override.aes = 
                                                                   list(alpha = 1, 
                                                                        size = smoothersize)))
                )
        }
        
        return(make_time_plot(make_long(df_wide, varname, timevarname, useNA),
                              varname, timevarname, cumulative, 
                              # required
                              yvar = "prop", #"prop"/"count"/"count_total"
                              smoother = "loess", #ou autre methd, ou FALSE
                              smoothersize = 2,
                              se = TRUE,
                              se_alpha = 0.1,
                              line_alpha = 0.4
        )
        )
}


# test
cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", cumulative = FALSE)
cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", cumulative = TRUE)
cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", cumulative = FALSE, na_level = "UNKNOWN")
cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", yvar = "count", cumulative = FALSE)
cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", yvar = "count", cumulative = TRUE)
cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", 
           yvar = "count_total", smoothersize = 1, cumulative = FALSE)
cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", 
           yvar = "count_total", smoothersize = 1, cumulative = TRUE)

cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", 
           yvar = "count_total", smoother = FALSE, smoothersize = 1, cumulative = TRUE)

cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", 
           yvar = "totalcount_")
cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", alpha = 0.2)
cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", smoother = NULL)
cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", 
           smoother = "lm", 
           formula = y ~ splines::bs(x, 3))

cat1_time (df2, varname = "ALIGN", timevarname = "YEAR", cumulative = FALSE, span = 0.4)

cat1_time (df2, varname = "SEX", timevarname = "YEAR", cumulative = FALSE) 
cat1_time (df2, varname = "SEX", timevarname = "YEAR", cumulative = TRUE) 
cat1_time (df2, varname = "SEX", timevarname = "YEAR",
           smoother = FALSE, cumulative = TRUE) 

cat1_time (df2, varname = "HAIR", timevarname = "YEAR", cumulative = FALSE,
           # useNA = "no"
           na_level = "UNKNOWN"
           ) ## too many categories
cat1_time (df2, varname = "HAIR", timevarname = "YEAR", cumulative = TRUE)

cat1_time (df2, varname = "EYE", timevarname = "YEAR", cumulative = FALSE) ## too many categories
cat1_time (df2, varname = "EYE", timevarname = "YEAR", cumulative = TRUE)


cat1_time (df2, varname = "GSM", timevarname = "YEAR",  cumulative = FALSE,
           useNA = "yes",
           smoother = FALSE, smoothersize=.5, se = FALSE)

cat1_time (df2, varname = "GSM", timevarname = "YEAR",  cumulative = FALSE,
           #useNA = "no",
           na_level = "UNKNOWN",
           smoother = FALSE, smoothersize=.5, se = FALSE)

cat1_time (df2, varname = "GSM", timevarname = "YEAR", cumulative = TRUE, useNA = "yes",
           smoother = FALSE, smoothersize=.5, se = FALSE)

# END =========================================================================

