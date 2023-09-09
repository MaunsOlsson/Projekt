# Webscaping Wikipedia Test med analys ##################

## Kapitel 1:Kommentar ##################################

# För att genomföra analysen används flera paket som
# Undelättar  med analysen används [Nummer] olika paket

# Man kan också använda require(), skillnaden mellan dem
# är mimimal. För att läsa skillnaden se kommentaren under:

## Skillnaden mellan require och library är att, i en kodad funktion,
## library stoppar funktionen om funktionens paket saknas i Environment,
## require fortsätter funktionen utan den.

## Kapitel 2:Paket ######################################

# Varför skrivs kommandon med {}? För att bättre få koll på
# dokumentet används {} som förstoras eller minskas.
if(!require(tidyverse)){
  install.packages("tidyverse")
}
if(!require(XML)){
  install.packages("XML")
}
if(!require(rvest)){
  install.packages("rvest")
}
if(!require(rlist)){
  install.packages("rlist")
}
if(!require(stringi)){
  install.packages("stringi")
}
if(!require(htmltab)){
  install.packages("htmltab")
}

# Tar fram paketen i library

# Samling av mindre datamanipulerings paket
library(tidyverse)
# Kan läsa html och pdf filer
library(XML)
# rvest webscraping verktyg, sammanfattat hanterar objekt som importeras
# med XML
library(rvest)

# För att kolla paketets funktion rekommenderas länken:
# https://rvest.tidyverse.org/

## Kapitel 3: Paket funktion ######################################

# Först behöver man en funktion som läser in html genom funktion. Detta gör det möjligt att
# hämta ut data och annan information från en hemsida
html <- read_html(x = "https://rvest.tidyverse.org/articles/starwars.html")

# Om man skriver ut objektet eller undersöker det i 'Global Environment', så ser man att det har
# samma innehåll som 'inspekt koden på hemsidan'
print(html)
View(html)

# För att se alla delkapitel på hemsidan
html_elements(x = html, css ="section")
# (Man kan även använda sig av tidyverse %>%, vilket är enklare programmerat.
# Detta används inte för att visa vad varje delargument anger).

# I utskriften ser man <section><h2 data-id="1">\nThe Phantom Menace\n</h2> ...
# anta att man vill hämta ut texten på titeln på sidans delkapitel (exempelvis "The Phantom Menace"
# som är första delkapitlets titel/den första nämnda filmen).

# Notera att html_text bara skriver ner själva texten som är skriven i koden
# exempelvis '[1] \nThe Phantom Menace\n'
# html_text2, skriver ner hur koden visas i en webläsare 
test <- html_elements(x = html, css ="section") %>%
  html_element("h2") %>% html_text2()

# För att plocka ut id används html_attr
avsnitt <- html_elements(x = html, css ="section") %>%
  html_elements("h2") %>% html_attr("data-id") %>% as.integer()
avsnitt

# Om det finns färdiga tabeller på hemsidan
html <- read_html("https://en.wikipedia.org/w/index.php?title=The_Lego_Movie&oldid=998422565")

# Titeln heter "Track listing" och är kodat som ett 'tracklist' objekt
# Detta kan ses i '<table class="tracklist">' i inspect

df <- html %>% 
  html_element(".tracklist") %>% 
  html_table()

## Egen användning ######################################
# Anta att man vill kopiera flera tabeller från olika wikipedia artiklar och
# sedan sammanställa de.

# paket för list manipulering
library(rlist)
# paket för string (text) manipulering
library(stringr)

# Läser in filen
# Självmordsstatistik
chopsuie <- read_html(x = "https://en.wikipedia.org/wiki/List_of_countries_by_suicide_rate", encoding = "")
# Frihets index
freedom <- read_html(x = 'https://en.wikipedia.org/wiki/Freedom_in_the_World')
# Utbildning
edu <- read_html(x = 'https://en.wikipedia.org/wiki/Education_Index')

# sökvägen på hemsidan till tabellen
df1 <- chopsuie %>% html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>%
  html_table()
df2 <- freedom %>% html_node(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>%
  html_table()
df3 <- edu %>% html_node(xpath = '//*[@id="mw-content-text"]/div[1]/div[2]/table') %>%
  html_table()

# Onödig info behöver tas bort
df2 <- df2[-c(1, 2), c(1, 6)]
df3 <- df3[, c(1, 31)]

# Döper om variabelnamn
colnames(df1) <- c("Country", "Sui_B", "Sui_M", "Sui_F")
colnames(df2) <- c("Country", "Freedom")
colnames(df3) <- c("Country", "Education")

# Om man kollar namnen så ser man att några har '*' i sitt namn. Detta blir as konstigt om man sätter ihopp.
# Därmed används stringmanipulering

df1$Country <- str_replace_all(string = df1$Country, c("\\*" = '', " " = ''))
# Det finns säkert ett bättre sätt att koda detta än att upprepa tre gånger, men jag har glömt bort och kommer inte kolla upp det

# DF2 skriver '*' som ' *', vilket komplicerar när man vill lägga ihop data
df2$Country <- str_replace_all(string = df2$Country, pattern = "\\*", replacement = '')
df2$Country <- str_trim(string = df2$Country, side = "right")
# Skriver NA som ''. Detta är dumt och måste fixas
df2$Freedom <- replace(x = df2$Freedom, list = df2$Freedom == "", values = NA)
df2$Freedom <- df2$Freedom %>% as.numeric()

df3$Country <- str_replace_all(string = df3$Country, pattern = "\\*", replacement  = '')

# Nu kan dataramen sättas ihop, som tidigare nämnt kan detta göras i en och samma funktion. Det är bara att jag inte har kommit ihåg det.
df <- merge(df1, df2, by = 'Country', all = FALSE)
df <- merge(df, df3, by = 'Country', all = FALSE)


# Detta är ett klumpigt sätt att skapa data på i praktiken, då vi inte undersöker exempelvis hur dessa tabeller skiljer sig i länders namn.
# Exempelvis namnger df3 congo som 'Congo (Democratic Republic of the)', istället för 'DR Congo'. Denna kod är dock bara för att testa webscraping

# Visualisering #########################################################################################################

# Snygga lite diagram
library(ggplot2)
# Standard visualiseringspaket

library(ggthemes)
# Tillåter lite fina diagramteman

library(GGally)
# Tillåter lite mysiga sambandsmatriser

ggpairs(df[, -1]) + theme_bw()
# Slutsats - Det finns ett markant samband mellan utbildning och yttrandefriheten i landet, vilket inte är förvånande.
# Sen kan man fortsätta och göra en multivariate analys, vilket kanske jag gör senare.