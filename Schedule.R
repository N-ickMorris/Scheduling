# -----------------------------------------------------------------------------------
# ---- Input Information ------------------------------------------------------------
# -----------------------------------------------------------------------------------

# set the path of where the input files are
mywd = "C:/Users/Nick Morris/Downloads/Scheduling"

# open a graphics window
windows()

# -----------------------------------------------------------------------------------
# ---- Packages ---------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# data handling
require(data.table)
require(chron)
require(zoo)

# plotting
require(ggplot2)
require(gridExtra)
require(scales)

}

# -----------------------------------------------------------------------------------
# ---- Functions --------------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# ---- prints the data types of each column in a data frame -------------------------

types = function(dat)
{
  require(data.table)
  
  # make dat into a data.table
  dat = data.table(dat)
  
  # get the column names
  column = names(dat)
  
  # get the class of the columns
  dataType = sapply(1:ncol(dat), function(i) class(unlist(dat[, i, with = FALSE])))
  
  # compute the number of levels for each column
  levels = sapply(1:ncol(dat), function(i) ifelse(dataType[i] == "factor", length(levels(droplevels(unlist(dat[, i, with = FALSE])))), 0))
  
  # compute the number of unique values for each column
  uniqueValues = sapply(1:ncol(dat), function(i) length(unique(unname(unlist(dat[, i, with = FALSE])))))
  
  # compute the portion of missing data
  missing = sapply(1:ncol(dat), function(i) nrow(na.omit(dat[, i, with = FALSE], invert = TRUE)) / nrow(dat))
  
  # build the output table 
  output = data.table(column, id = 1:length(column), dataType, levels, uniqueValues, missing)
  
  # order output by dataType
  output = output[order(dataType)]
  
  return(output)
}

# ---- converts all columns to a character data type --------------------------------

tochar = function(dat)
{
  require(data.table)
  
  # make dat into a data.frame
  dat = data.table(dat)
  
  # get the column names
  column = names(dat)
  
  # get the values in the columns and convert them to character data types
  values = lapply(1:ncol(dat), function(i) as.character(unname(unlist(dat[, i, with = FALSE]))))
  
  # combine the values back into a data.frame
  dat = data.table(do.call("cbind", values), stringsAsFactors = FALSE)
  
  # give dat its column names
  setnames(dat, column)
  
  return(dat)
}

# ---- a qualitative color scheme ---------------------------------------------------

qcolor = function(n, a = 1)
{
  require(grDevices)
  require(scales)
  return(alpha(colorRampPalette(c("#e41a1c", "#0099ff", "#4daf4a", "#984ea3", "#ff7f00", "#ff96ca", "#a65628"))(n), 
               a))
}

# ---- the ggplot2 color scheme -----------------------------------------------------

ggcolor = function(n, a = 1)
{
  require(grDevices)
  require(scales)
  return(alpha(hcl(h = seq(15, 375, length = n + 1), 
                   l = 65, c = 100)[1:n], 
               a))
}

}

# -----------------------------------------------------------------------------------
# ---- Prepare the Data -------------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# set the work directory
setwd(mywd)

# import the tasks and vacation data
tasks = na.omit(data.table(read.csv("Tasks.csv", stringsAsFactors = FALSE)))
vaca = na.omit(data.table(read.csv("Days Off.csv", stringsAsFactors = FALSE)))

# convert Start, End, and Date to date data types
tasks[, Start := as.Date(chron(Start))]
tasks[, End := as.Date(chron(End))]
vaca[, Date := as.Date(chron(Date))]

# rank tasks by End
tasks = tasks[, Value := rank(End)]

# rescale tasks of End to the min/max of Importance
tasks[, Value := rescale(-Value, to = range(Importance))]

# multiply Value with Importance
tasks[, Value := Value * Importance]

# order tasks by Value
tasks = tasks[order(-Value)]

# update each task to contain no more than 22 characters
tasks[, Task := substr(Task, start = 0, stop = 22)]

}

# -----------------------------------------------------------------------------------
# ---- Build the Schedule -----------------------------------------------------------
# -----------------------------------------------------------------------------------

{

# pick a schedule length
months = 2

# set up a schedule table based on tasks and months
schedule = data.table(Date = seq.Date(from = min(tasks$Start), length.out = 31 * months, by = "days"),
                      Hard = rep_len(unname(unlist(tasks[Class == "Hard", .(Task)])), length.out = 31 * months),
                      Hard.Start = as.Date(rep_len(unname(unlist(tasks[Class == "Hard", .(Start2 = as.character(Start))])), length.out = 31 * months)),
                      Hard.End = as.Date(rep_len(unname(unlist(tasks[Class == "Hard", .(End2 = as.character(End))])), length.out = 31 * months)),
                      Easy = rep_len(unname(unlist(tasks[Class == "Easy", .(Task)])), length.out = 31 * months),
                      Easy.Start = as.Date(rep_len(unname(unlist(tasks[Class == "Easy", .(Start2 = as.character(Start))])), length.out = 31 * months)),
                      Easy.End = as.Date(rep_len(unname(unlist(tasks[Class == "Easy", .(End2 = as.character(End))])), length.out = 31 * months)))

# update Hard to be NA if the Date is passed the End date or before the Start date
schedule[, Hard := ifelse(Date < Hard.Start, NA, Hard)]
schedule[, Hard := ifelse(Date > Hard.End, NA, Hard)]
schedule[, c("Hard.Start", "Hard.End") := NULL]

# update Easy to be NA if the Date is passed the End date or before the Start date
schedule[, Easy := ifelse(Date < Easy.Start, NA, Easy)]
schedule[, Easy := ifelse(Date > Easy.End, NA, Easy)]
schedule[, c("Easy.Start", "Easy.End") := NULL]

# replace NA's with last known value
schedule[, Hard := na.locf(Hard, fromLast = TRUE, na.rm = FALSE)]
schedule[, Easy := na.locf(Easy, fromLast = TRUE, na.rm = FALSE)]

# combine Hard, Easy, and Mind into one Task column
schedule[, Task := paste(paste0("Hard:\n", Hard), 
                         paste0("\nEasy:\n", Easy), 
                         sep = "\n")]

# get the day of week, week, month, and year from Date
schedule[, Day := format(Date, "%a")]
schedule[, Week := format(Date, "%U")] 
schedule[, Month := format(Date, "%B")] 
schedule[, Year := format(Date, "%Y")] 

# update week to be the first date of the week
schedule[, Week.Date := min(Date), by = .(Week)]

# split up schedule into two monthly segments
m = unique(schedule$Month)
y = unique(schedule$Year)
month1 = m[1]
year1 = y[1]
month2 = m[2]
year2 = y[length(y)]
schedule1 = schedule[Month %in% month1]
schedule2 = schedule[Month %in% month2]

# convert Task into a factor
schedule1[, Task := factor(Task, levels = unique(Task))]
schedule2[, Task := factor(Task, levels = unique(Task))]

# convert Day into a factor
schedule1[, Day := factor(Day, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))]
schedule2[, Day := factor(Day, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))]

# convert Week.Date into a factor
schedule1[, Week.Date := as.character(format(Week.Date, "%B %d"))]
schedule1[, Week.Date := factor(Week.Date, levels = rev(unique(Week.Date)))]
schedule2[, Week.Date := as.character(format(Week.Date, "%B %d"))]
schedule2[, Week.Date := factor(Week.Date, levels = rev(unique(Week.Date)))]

# plot schedule1
p1 = ggplot(schedule1, aes(x = Day, y = Week.Date)) +
  geom_tile(colour = "black", fill = "white") +
  geom_text(label = schedule1$Task, size = 4) + 
  scale_x_discrete(expand = c(0, 0)) +
  ggtitle(paste(month1, year1, sep = "-")) + 
  theme_gray(base_size = 20) + 
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(panel.background = element_rect(fill = "transparent"))+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p1

# set up a pdf file to capture the next graphic
pdf(paste0("To-Do-", month1, "-", year1, ".pdf"), width = 16, height = 10, paper = "special") 

# call the plot
print(p1)

# close off the connection
dev.off()

# plot schedule2
p2 = ggplot(schedule2, aes(x = Day, y = Week.Date)) +
  geom_tile(colour = "black", fill = "white") +
  geom_text(label = schedule2$Task, size = 4) + 
  scale_x_discrete(expand = c(0, 0)) +
  ggtitle(paste(month2, year2, sep = "-")) + 
  theme_gray(base_size = 20) + 
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(panel.background = element_rect(fill = "transparent"))+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

p2

# set up a pdf file to capture the next graphic
pdf(paste0("To-Do-", month2[1], "-", year2[1], ".pdf"), width = 16, height = 10, paper = "special") 

# call the plot
print(p2)

# close off the connection
dev.off()

}



