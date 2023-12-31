MetaCritic_PageScarpe <- function(website, wait = 2){
  page <- read_html(website)
  message("Scraping URL: ", website)
  
  name <- page %>%
    html_element("div.c-productHero_title.g-inner-spacing-bottom-medium.g-outer-spacing-top-medium") %>%
    html_text2()
  
  release <- page %>%
    html_element("div.g-text-xsmall span.u-text-uppercase") %>%
    html_text2()
  release <- lubridate::mdy(release)
  
  criticscore <- page %>%
    html_element(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[2]/div/div[1]/div[2]/div/div[1]/div/div/a/div/div') %>%
    html_text2() %>% as.numeric()
  
  # Hämtar ut alla N critics
  {
    # amount of reviews
    Ncritic <- page %>%
      html_elements(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[2]/div/div[1]/div[2]/div/div[1]/div/div/div/div/span[2]/a/span') %>%
      html_text2()
    Ncritic <- gsub("[^0-9.-]", "", Ncritic) %>% as.numeric()
    
    # amount of positive
    Ncritic_Pos <- page %>%
      html_elements(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[2]/div/div[1]/div[2]/div/div[2]/div[2]/div[1]/span[2]') %>%
      html_text2()
    Ncritic_Pos <- gsub("[^0-9.-]", "", Ncritic_Pos) %>% as.numeric()
    
    # amount of mixed
    Ncritic_Mixed <- page %>%
      html_elements(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[2]/div/div[1]/div[2]/div/div[2]/div[2]/div[2]/span[2]') %>%
      html_text2()
    Ncritic_Mixed <- gsub("[^0-9.-]", "", Ncritic_Mixed) %>% as.numeric()
    
    # amount of negative
    Ncritic_Neg <- page %>%
      html_elements(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[2]/div/div[1]/div[2]/div/div[2]/div[2]/div[3]/span[2]') %>%
      html_text2()
    Ncritic_Neg <- gsub("[^0-9.-]", "", Ncritic_Neg) %>% as.numeric()
    
    {
      if(length(Ncritic) == 0) Ncritic <- NA
      if(length(Ncritic_Pos) == 0) Ncritic_Pos <- NA
      if(length(Ncritic_Neg) == 0) Ncritic_Neg <- NA
      if(length(Ncritic_Mixed) == 0) Ncritic_Mixed <- NA
    }
    }
  
  userscore <- page %>%
    html_element(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[4]/div/div[1]/div[2]/div/div[1]/div/div/a/div/div') %>%
    html_text2() %>% as.numeric()
  
  # Hämtar ut alla N critics
  {
    # amount of reviews
    Nuser <- page %>%
      html_elements(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[4]/div/div[1]/div[2]/div/div[1]/div/div/div/div/span[2]/a/span') %>%
      html_text2()
    Nuser <- gsub("[^0-9.-]", "", Nuser) %>% as.numeric()
    
    # amount of positive
    Nuser_Pos <- page %>%
      html_elements(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[4]/div/div[1]/div[2]/div/div[2]/div[2]/div[1]/span[2]') %>%
      html_text2()
    Nuser_Pos <- gsub("[^0-9.-]", "", Nuser_Pos) %>% as.numeric()
    
    # amount of mixed
    Nuser_Mixed <- page %>%
      html_elements(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[4]/div/div[1]/div[2]/div/div[2]/div[2]/div[2]/span[2]') %>%
      html_text2()
    Nuser_Mixed <- gsub("[^0-9.-]", "", Nuser_Mixed) %>% as.numeric()
    
    # amount of negative
    Nuser_Neg <- page %>%
      html_elements(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[4]/div/div[4]/div/div[1]/div[2]/div/div[2]/div[2]/div[3]/span[2]') %>%
      html_text2()
    Nuser_Neg <- gsub("[^0-9.-]", "", Nuser_Neg) %>% as.numeric()
    
    Nuser_var <- c(Nuser, Nuser_Mixed, Nuser_Neg, Nuser_Pos)
    
    # Condition if empty
    {
      if(length(Nuser) == 0) Nuser <- NA
      if(length(Nuser_Pos) == 0) Nuser_Pos <- NA
      if(length(Nuser_Neg) == 0) Nuser_Neg <- NA
      if(length(Nuser_Mixed) == 0) Nuser_Mixed <- NA
    }
    }
  
  Dev <- page %>%
    html_element(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[6]/div/div/div[2]/div[2]/div[2]/div[1]/ul/li') %>%
    html_text2()
  
  Publisher <- page %>%
    html_element(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[6]/div/div/div[2]/div[2]/div[2]/div[2]/span[2]') %>%
    html_text2()
  
  Genre <- page %>%
    html_element(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[6]/div/div/div[2]/div[2]/div[3]/ul/li/div/a/span') %>%
    html_text2()
  
  Rate <- page %>%
    html_element(xpath = '//*[@id="__layout"]/div/div[2]/div[1]/div[6]/div/div/div[2]/div[1]/div/div/div[2]/span[1]') %>%
    html_text2()
  
  # Stannar upp funktionen. Detta används så att man inte gör en DDOS på hemsidan
  Sys.sleep(wait)
  
  tibble(Name = name,
         Release = release,
         Developer = Dev,
         Publisher = Publisher,
         Rating = Rate,
         Genre = Genre,
         Critics_Score = criticscore,
         Number_Critics = Ncritic,
         Number_Critics_Pos = Ncritic_Pos,
         Number_Critics_Mixed = Ncritic_Mixed,
         Number_Critics_Negative = Ncritic_Neg,
         User_score = userscore,
         Number_User = Nuser,
         Number_User_Pos = Nuser_Pos,
         Number_User_Mixed = Nuser_Mixed,
         Number_User_Negative = Nuser_Neg,
         url = website)
}