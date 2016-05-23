moving.stationary.segments.df <- function(data.dir="analysis/data/"){
  # Creates unified, cleaned data file for all of the segments coded moving or stationary
  
  # Import segment files and merge
  india.segs <- read.csv(paste(data.dir, "Indiams.csv", sep=""), header=TRUE)
  india.segs$country <- "India"
  us.segs <- read.csv(paste(data.dir, "USms.csv", sep=""), header=TRUE)
  us.segs$country <- "U.S."
  
  all.segs <- merge(india.segs, us.segs, all = TRUE)
  
  # Clean-up
  drops <- c("X", "moments", "clip", "ordinal")
  drop.cols <- (names(all.segs) %in% drops)
  all.segs <- all.segs[,!drop.cols]
  
  all.segs$age.wks <- as.numeric(substr(all.segs$sub,1,3))
  
  all.segs$secs <- all.segs$dur * (1/1000)
  
  levels(all.segs$code) <- c("moving", "stationary")
  
  all.segs$country <- as.factor(all.segs$country)
  c <- relevel( all.segs$country, "U.S.") # make U.S. first for consistency
  all.segs$country <- c
  
  names(all.segs) <- c("onset", "offset", "motion.status", "ms", "subj", "country", "age.wks", "secs")
  
  return(all.segs)
}