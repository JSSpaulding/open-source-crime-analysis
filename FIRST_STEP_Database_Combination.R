## Set the Working Directory ----
maindir <- "*DIRECTORY PATH HERE*" #enter directory folder location here 
setwd(maindir) 

## Load Chicago Database (All three parts) ----
load("chicago_crimes_database_p1.rData") #loads Spaulding Chicago Crime Data Part 1
load("chicago_crimes_database_p2.rData") #loads Spaulding Chicago Crime Data Part 2
load("chicago_crimes_database_p3.rData") #loads Spaulding Chicago Crime Data Part 3

# Combine all Database Parts into Single Dataframe
full <- rbind(part1,part2,part3)

# Clear Up System Memory - Leave Solely Full DF
rm(part1,part2,part3)

# Save Full Database to Local Directory
## NOTE: Combined Database is too Large for GitHub - exceeds 100 MB
##       Save in Local Directory instead
save.image("chicago_crimes_database.RData")