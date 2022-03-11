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
  # taking care of global variable problem
  RIVER <-  WEIGHT <- LENGTH <- NULL
  # print original data set
  print(df)
  # subset the data set based on given species
  df1 <- df %>% filter(SPECIES == {{cond}}) # Note the use of {{}}
  # create plot using ggplot2 and tack on a quadratic curve as well as coloring points to distinguish River
  g <- ggplot(df1, aes_string(x=df1$WEIGHT,y=df1$LENGTH)) + # Note the use of aes_string
    geom_point(aes_string(color = df1$RIVER)) +
    geom_smooth(formula = y~x +I(x^2), method = "lm") +
    ggtitle("Christian Hackelman") + xlab("WEIGHT") + ylab("LENGTH") + labs(color = "RIVER")
  # write out new csv file containing subsetted data set
  write.csv(df1, paste("LvsWfor", cond, ".csv", sep = ""), row.names = FALSE)
  # print the created plot
  print(g)
  # print the new data set
  print(df1)
  # create relative frequency table based upon River
  t <- table(df$RIVER)/length(df$RIVER)
  # print relative frequency table
  print(t)
}
