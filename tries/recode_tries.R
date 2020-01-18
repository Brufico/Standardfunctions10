
# get data
getwd()
dir(file.path("data"))

df <- read.table(file.path("data", "DCcomics.csv"), 
               sep = ";",# stringsAsFactors = FALSE, 
               header = TRUE, na.strings = "")



tail(df[-(c(1, 2, 3))])
df[6718,"SEX"]
df[6718,"GSM"]

local({
        cn <- "EYE" 
        print(levels(df[[cn]]))
        # unique(df[[cn]])
        tb <- table(df[[cn]])
        print(tb)
        prop.table(tb)
})

# recode_table
recode_eyes <- data.frame(
        old = c("Amber Eyes", "Auburn Hair", "Black Eyes", 
                "Blue Eyes", "Brown Eyes", "Gold Eyes",
                "Green Eyes", "Grey Eyes", "Hazel Eyes",
                "Orange Eyes", "Photocellular Eyes", "Pink Eyes",
                "Purple Eyes", "Red Eyes", "Violet Eyes",
                "White Eyes", "Yellow Eyes"
        ),
        new = c("Brownish Eyes", "Brownish Eyes", "Black Eyes",
                "Blue Eyes", "Brownish Eyes", "Other Eyes",
                "Green Eyes", "Grey Eyes", "Brownish Eyes",
                "Other Eyes", "Photocellular Eyes", "Reddish Eyes",
                "Reddish Eyes", "Reddish Eyes", "Reddish Eyes",
                "White Eyes", "Yellow Eyes"
                ),
        stringsAsFactors = FALSE
)

unique(recode_eyes$new)
