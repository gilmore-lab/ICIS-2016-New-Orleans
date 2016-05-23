compute.p.moving <- function(df){
  # Computes proportion of time in motion
  library(ggplot2)
  library(dplyr)
  
  total.df <- df %>%
    group_by(subj) %>%
    summarize(total.secs = sum(secs),
              n.segs = n())

  p.motion.df <- df %>%
    filter(motion.status=="moving") %>%
    group_by(subj) %>%
    summarize(moving.secs = sum(secs), n.moving.segs = n())
  
  p.motion.df <- merge(p.motion.df, total.df, by=c("subj"))
  
  p.motion.df <- p.motion.df %>%
    mutate(p.motion = moving.secs/total.secs, 
           p.motion.segs = n.moving.segs/n.segs)
  
  country.wks.df <- df %>%
    select(subj, country, age.wks) %>%
    unique()
  
  p.motion.df <- merge(p.motion.df, country.wks.df, by=c("subj"))
  
  return(p.motion.df)
}