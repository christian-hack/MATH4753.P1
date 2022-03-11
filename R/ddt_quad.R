#' @title Plot given SPECIES
#'
#' @param df A data set to be later subsetted according to given species name
#' @param cond A subset condition (species' name)
#'
#' @importFrom dplyr '%>%' filter
#' @importFrom utils write.csv
#'
#' @return A sophisticated plot with quadratic curve
#' @export
#'
#' @examples
#' myddt(df = ddt, cond = "CCATFISH")
library(dplyr)
library(ggplot2)
library(Intro2R)
myddt <- function(df, cond){
  RIVER <-  WEIGHT <- LENGTH <- NULL
  print(df)
  df1 <- df %>% filter(SPECIES == {{cond}}) # Note the use of {{}}
  g <- ggplot(df1, aes_string(x=df1$WEIGHT,y=df1$LENGTH)) + # Note the use of aes_string
    geom_point(aes_string(color = df1$RIVER)) +
    geom_smooth(formula = y~x +I(x^2), method = "lm") +
    ggtitle("Christian Hackelman") + xlab("WEIGHT") + ylab("LENGTH") + labs(color = "RIVER")
  write.csv(df1, paste("LvsWfor", cond, ".csv", sep = ""), row.names = FALSE)
  print(g)
  print(df1)
  t <- table(df$RIVER)/length(df$RIVER)
  print(t)
}
