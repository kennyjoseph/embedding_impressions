library(GGally)
library(Hmisc)
library(readstata13)
library(scales)
library(stringi)
library(data.table)
library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(agrmt)
library(ggridges)
library(sandwich)
library(ggrepel)
library(ggpubr)
library(mgcv)
source("https://raw.githubusercontent.com/dgrtwo/drlib/master/R/reorder_within.R")

align_measures <- function(colv, align_to){
  z <- cor(colv, align_to)
  if(is.na(z)){
    return(colv)
  }
  if(z < 0){
    return(colv*-1)
  }
  return(colv)
}

read_personality <- function(f){
  k <- fread(f)
  k$dim <- sub("[.]csv","",basename(f))
  return(k)
}



confint_robust <- function(object, parm, level = 0.95, 
                           HC_type="HC3", t_distribution = FALSE,...){
  cf <- coef(object); pnames <- names(cf)
  if(missing(parm))
    parm <- pnames
  else if (is.numeric(parm))
    parm <- pnames[parm]
  
  a <- (1-level)/2; a <- c(a, 1-a)
  pct <- paste(format(100 * a, 
                      trim = TRUE, 
                      scientific = FALSE, 
                      digits = 3), 
               "%")
  if (t_distribution)
    fac <- qt(a, object$df.residual)
  else
    fac <- qnorm(a)
  ci <- array(NA, 
              dim = c(length(parm), 2L), 
              dimnames = list(parm, pct))
  ses <- sqrt(diag(vcovHC(object, type=HC_type, ...)))[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}


create_ci_dat <- function(mod, do_exp=T){
  cf <-  confint_robust(mod)
  if(do_exp){
    r2 <- data.frame(
      row.names(cf),
      exp(coef(mod)),
      exp(cf[,1]),
      exp(cf[,2]))
  } else{
    r2 <- data.frame(
      row.names(cf),
      coef(mod),
      cf[,1],
      cf[,2])
  }
  
  setnames(r2, c("name","m","l","u"))
  r2 <- r2[r2$name != "(Intercept)",]
  r2 <- data.table(r2)
  r2$name <- str_to_title(sub("_"," ",r2$name))
  return(r2)
}


fill_datatable_na_with_zero <- function(DT){
  for(j in seq_along(DT)){
    set(DT, i = which(is.na(DT[[j]]) & is.numeric(DT[[j]])), j = j, value = 0)
  }
}


rescale_survey_data <- function(data_df){
  mean_scores <- data_df[, mean(value), by=.(identity,qtype,dimension)]
  spread_means <- spread(mean_scores[,-"qtype",with=F], dimension,V1)
  
  ### First, fill all NAs with zero
  fill_datatable_na_with_zero(spread_means)
  
  # Add in denotative race info
  spread_means[identity == "asian"]$Asian <- 100
  spread_means[identity == "white"]$White <- 100
  spread_means[identity == "black"]$Black <- 100
  spread_means[identity == "arab"]$`Middle Eastern` <- 100
  spread_means[identity == "hispanic"]$Latino <- 100
  
  # Add in denotative gender info
  spread_means[identity == "guy"]$Gender <- -2
  spread_means[identity == "boy"]$Gender <- -2
  spread_means[identity == "girl"]$Gender <- 2
  spread_means[identity == "lady"]$Gender <- 2
  spread_means[identity == "man"]$Gender <- -2
  spread_means[identity == "woman"]$Gender <- 2
  
  # make the names match the survey data
  cols <- names(spread_means)[! names(spread_means)%in% c("identity")]
  spread_means[, (cols) := lapply(.SD, function(l){scale(l)[,]}), .SDcols=cols]
  return(spread_means)
}


rq1_name_map <- c("(Intercept)"="(Intercept)",
        "variablegarg"="Word Position\nMeasure: Garg et al. (2018)",
        "variablebolukbasi fun"="Word Position\nMeasure: Bolukbasi et al. (2016)",
        "variablekozlowski fun"="Word Position\nMeasure: Kozlowski et al. (2019)",
        "variableripa"="Word Position\nMeasure: Ethayarajh et al. (2019) + Garg et al.",
        "variableripa bolukbasi_fun"="Word Position\nMeasure: Ethayarajh et al. (2019)",
        "variableripa kozlowski_fun"="Word Position\nMeasure: Ethayarajh et al. (2019) + Kozlowski et al.",
        "variableswinger"="Word Position\nMeasure: Swinger et al. (2018)",
        "dimensionevaluation"="Dimension: Evaluation",
        "dimensionpotency"="Dimension: Potency",
        "dimensionactivity"="Dimension: Activity",
        "dimensionage"="Dimension: Age",
        "dimensionfamily"="Dimension: Family",
        "dimensionpolitics"="Dimension: Politics",
        "dimensionjustice"="Dimension: Justice",
        "dimensionmedicine"="Dimension: Medicine",
        "dimensionbusiness"="Dimension: Business",
        "dimensioneducation"="Dimension: Education",
        "dimensionreligion"="Dimension: Religion",
        "dimensiongender"="Dimension: Gender",
        "dimensionwhite"="Dimension: White",
        "dimensionlatino"="Dimension: Latino",
        "dimensionasian"="Dimension: Asian",
        "dimensionmiddle eastern"="Dimension: Middle Eastern",
        "dimensionblack"="Dimension: Black",
        "dimensionopenness"="Dimension: Openness",
        "dimensionconscientiousness"="Dimension: Conscientiousness",
        "dimensionextroversion"="Dimension: Extroversion",
        "dimensionagreeableness"="Dimension: Agreeableness",
        "dimensionneuroticism"="Dimension: Neuroticism",
        "mid0"="Dimension-inducing\nWordset: Survey-matched Evaluation",
        "mid1"="Dimension-inducing\nWordset: Survey-augmented Evaluation",
        "mid2"="Dimension-inducing\nWordset: Survey-matched Potency",
        "mid3"="Dimension-inducing\nWordset: Survey-augmented Potency",
        "mid4"="Dimension-inducing\nWordset: Survey-matched Activity",
        "mid5"="Dimension-inducing\nWordset: Survey-augmented Activity",
        "mid6"="Dimension-inducing\nWordset: Survey-matched Age",
        "mid7"="Dimension-inducing\nWordset: Survey-augmented Age",
        "mid8"="Dimension-inducing\nWordset: Survey-matched Gender",
        "mid9"="Dimension-inducing\nWordset: Gonen & Goldberg Gender",
        "mid11"="Dimension-inducing\nWordset: Bolukbasi Names Gender",
        "mid12"="Dimension-inducing\nWordset: Kozlowski Gender",
        "mid13"="Dimension-inducing\nWordset: Survey-matched Institution",
        "mid14"="Dimension-inducing\nWordset: Survey-augmented Institution",
        "mid15"="Dimension-inducing\nWordset: Survey-matched Race/Ethnicity",
        "mid16"="Dimension-inducing\nWordset: Kozlowski Race/Ethnicity",
        "mid17"="Dimension-inducing\nWordset: Kozlowski Lowercase Race/Ethnicity",
        "mid18"="Dimension-inducing\nWordset: Survey-augmented Race/Ethnicity",
        "mid19"="Dimension-inducing\nWordset: Agarwal Openness",
        "mid20"="Dimension-inducing\nWordset: Agarwal Conscientiousness",
        "mid21"="Dimension-inducing\nWordset: Agarwal Extroversion",
        "mid22"="Dimension-inducing\nWordset: Agarwal Agreeableness",
        "mid23"="Dimension-inducing\nWordset: Agarwal Neuroticism",
        "mid30"="Dimension-inducing\nWordset: Garg Gender",
        "embeddingglove.6b.100d.txt"="Embedding: GloVe (100D; 6B Tokens, Wiki+GigaWord)",
        "embeddingglove.6b.200d.txt"="Embedding: GloVe (200D; 6B Tokens, Wiki+GigaWord)",
        "embeddingglove.6b.300d.txt"="Embedding: GloVe (300D; 6B Tokens, Wiki+GigaWord)",
        "embeddingglove.6b.50d.txt"="Embedding: GloVe (50D; 6B Tokens, Wiki+GigaWord)",
        "embeddingglove.840b.300d.txt"="Embedding: GloVe (300D; 840B Tokens, Common Crawl)",
        "embeddingglove.twitter.27b.100d.txt"="Embedding: GloVe (100D; 27B Tokens, Twitter)",
        "embeddingglove.twitter.27b.200d.txt"="Embedding: GloVe (200D; 27B Tokens, Twitter)",
        "embeddingglove.twitter.27b.50d.txt"="Embedding: GloVe (50D; 27B Tokens, Twitter)",
        "embeddingnumberbatch-en-19.08.txt"="Embedding: Number Batch v19.08 (300D; ConceptNet)",
        "embeddingw2v.txt"="Embedding: Word2Vec (300D; Google News)",
        "embeddingwiki-news-300d-1m-subword.vec"="Embedding: FastText (300D; Wiki+Gigaword)")




