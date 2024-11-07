## ---------------------------
## Script name: idiom_finder.R
## Purpose of script: detect the idiom of some text
## Author: David Paiva Fernandes
## Email: david.paiva.fernandes@gmail.com
## Date Created: 2024-11-01
## ---------------------------

# text file read functions
library(readr)

# forward-pipe operator %>%
library(magrittr)

# euclidean and manhatan distance functions
library(abdiv) 

# read tsv text file into data.frame
# using first row as data frame column names
df <- readr::read_tsv("data/letter_frequencies.tsv", col_names = TRUE)

# cleaning column names removing that [xx] footnote references
colnames(df) <- gsub('([a-zA-Z]*)(.*)', '\\1', colnames(df))

# function to "clean" a string removing non numerical characters
# and returning a number
clean_values <- function(data) {
  as.numeric(gsub('([0-9\\.]*)([%~\\*\\(\\)])', '\\1', data))
}

# applying the clean up function to data columns df[2:ncol(df)]
# and rebuilding a new data frame consisting of the letters column
# of the original df and the data columns nice and neatly cleaned
df <- data.frame(df[1], sapply(df[2:ncol(df)], clean_values))

# function that infers the idiom of `text` using an idiom's frequency data frame
# and applying some distance calculation function
idiom_inference <- function(text, frequency_df, distance_fn) {

  # getting a data frame of the absolute frequencies of individual letters
  # present on `text`
  letter_count <- (
    text
    %>% tolower
    %>% strsplit(split="") 
    %>% unlist 
    %>% `[`(!. %in% c("", " ", ".", ",")) 
    %>% table
    %>% data.frame)

  # giving the letters data frame a proper nice structure
  colnames(letter_count) = c("Letter", "Frequency")
  
  # and merging it with the original all-letters columns
  # in order to get a complete vector
  letter_count <- merge(x = frequency_df[1], y = letter_count, by = c("Letter"), all.x = TRUE)

  # letter missing in `text` get a NA value so we transform that in 0
  letter_count[is.na(letter_count)] <- 0
  
  # and calculate the relative frequency
  letter_count$Percentage <- 100 * letter_count$Frequency / sum(letter_count$Frequency)
 
  # finally we apply the distance function between out text relative frequency vector
  # to each of the idioms relative frequency
  distances <- t(data.frame(lapply(frequency_df[2:ncol(frequency_df)], distance_fn, letter_count$Percentage)))
  
  # and return then rowname, which is the idiom name, of the lesser
  rownames(distances)[which(distances == min(distances), arr.ind=TRUE)[,1]]
}

# Testing

# phrases to be tested
phrase_pt <- "as armas e os baroes assinalados que da ocidental praia lusitana"
phrase_en <- "the weapons and barons marked from the western Lusitanian beach"
phrase_tk <- "Batı Lusitanya sahilinden işaretlenmiş silahlar ve baronlar"
phrase_fr <- "les armes et les barons marquaient celui de la plage ouest de la Lusitanienne"
phrase_de <- "Die Waffen und Barone markierten das vom westlichen lusitanischen Strand aus"
phrase_es <- "las armas y los barones marcaron eso desde la playa lusitana occidental"
phrase_it <- "le armi e i baroni lo segnavano dalla spiaggia lusitana occidentale"
phrase_se <- "vapnen och baronerna markerade det från den västra lusitanska stranden"
phrase_pl <- "broń i baronowie oznaczali to z zachodniej plaży Lusitanii"
phrase_nl <- "de wapens en baronnen markeerden dat vanaf het westelijke Lusitaanse strand"
phrase_dk <- "våbnene og baronerne påpegede, at der fra den vestlige lusitanske"
phrase_is <- "vopnin og barónarnir merktu það frá vesturhluta Lusitanian ströndinni"
phrase_fi <- "aseet ja paronit merkitsivät sen Länsi-Lusitanian rannalta"
phrase_ck <- "Zbraně a baroni označili,To ze západní pláže Lusitana"
phrase_hu <- "A fegyverek és a bárók megjelölték, A nyugati Lusitana strandé"

test_df <- data.frame(
  rbind(
    c("English", phrase_en), 
    c("French", phrase_fr), 
    c("German", phrase_de), 
    c("Spanish", phrase_es), 
    c("Portuguese", phrase_pt), 
    c("Italian", phrase_it), 
    c("Turkish", phrase_tk), 
    c("Swedish", phrase_se), 
    c("Polish", phrase_pl), 
    c("Dutch", phrase_nl), 
    c("Danish", phrase_dk), 
    c("Icelandic", phrase_is), 
    c("Finnish", phrase_fi), 
    c("Czech", phrase_ck), 
    c("Hungarian", phrase_hu)))

colnames(test_df) <- c("Idiom", "SampleText")

infered <- lapply(X=test_df$SampleText, FUN=idiom_inference, df, abdiv::manhattan)

# build a human-readable data frame
test_results <- data.frame(cbind(test_df$SampleText, test_df$Idiom, infered, test_df$Idiom == infered))
colnames(test_results) <- c("Text", "KnownIdiom", "InferedIdiom", "Match")

# return the results
test_results

