make.motion.static.df.study.1 <- function(data.url="https://raw.githubusercontent.com/gilmore-lab/ICDL-EpiRob-2015/master/data/coded-segments.csv"){
  library(RCurl)
  
  df.in <- read.csv(text=getURL(data.url), header=T)
  
  keep <- c("Moving", "Stationary")
  df.out <- subset(df.in, Motion.Static %in% keep)
  df.out$Motion.Static <- factor(df.out$Motion.Static)
  
  # Clean-up
  drops <- c("AgeMatchGroup", "Clip.Index", "Segment.Index")
  drop.cols <- (names(df.out) %in% drops)
  df.out <- df.out[,!drop.cols]
  
  c <- relevel( df.out$Country, "U.S.") # make U.S. first for consistency
  df.out$Country <- c
  
  levels(df.out$Motion.Static) <- c("moving", "stationary")
  
  df.out$secs <- df.out$Segment.ms * (1/1000)
  
  names(df.out) <- c("subj", "country", "age.wks", "motion.status", "onset", "offset", "ms", "secs")

  return(df.out)
}