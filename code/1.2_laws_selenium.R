library(RSelenium)
library(tidyverse)

rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "98.0.4758.102",
                             verbose = FALSE,
                             port = 4837L)

# create client
remDr <- rs_driver_object$client

# open a browser
remDr$open()
remDr$setTimeout(type = "implicit", milliseconds = 5000)     
remDr$setTimeout(type = "page load", milliseconds = 5000)  

# for realzies
x <- "https://www.ris.bka.gv.at/eli/bgbl/I/2017/147/20171110?ResultFunctionToken=d9321097-1a07-470d-b41d-2851ef0cc90f&Position=301&Abfrage=BgblAuth&Titel=&Bgblnummer=&SucheNachGesetzen=True&SucheNachKundmachungen=False&SucheNachVerordnungen=False&SucheNachSonstiges=False&SucheNachTeil1=False&SucheNachTeil2=False&SucheNachTeil3=False&Einbringer=&VonDatum=01.01.2004&BisDatum=31.12.2020&ImRisSeitVonDatum=01.01.2004&ImRisSeitBisDatum=31.12.2020&ImRisSeit=Undefined&ResultPageSize=100&Suchworte="

ini_scraping <- function(x){
  
remDr$open()
remDr$navigate(x)
initiative_object <- remDr$findElement(using = 'css selector', '#MainContent_DocumentRepeater_BgblAuthDocumentData_0_InitiativantragContainer_0 a')

#needed to open in same tab
remDr$executeScript("arguments[0].setAttribute('target', arguments[1]);", list(initiative_object, ""));

#switch to parliament website
initiative_object$clickElement()

# accept cookies
cookie_object <- remDr$findElement(using = 'css selector', '.yesSetting')
cookie_object$clickElement()

# save info on who supported bill
tryCatch({
  suppressMessages({
    beschluss_object <- remDr$findElement(using = 'css selector', '.status p')
    beschluss <- beschluss_object$getElementText() %>% unlist(.) %>% ifelse(length(.) == 0, NA, .)
    beschluss
  })
}, 
error = function(e) {
  NA_character_
}
)  

# switch to parliament website of initiator - doesn't work yet
tryCatch({
  suppressMessages({
    remDr$findElement(using = 'css selector', '.c_2 p span')$clickElement()
    party <- remDr$findElement(using = 'css selector', '.tabs-responsive__contentBlock')$getElementText() %>% unlist(.) %>% ifelse(length(.) == 0, NA, .)
    party
  })
}, 
error = function(e) {
  NA_character_
}
) 

# This way works!

#initiator_object <- remDr$findElement(using = 'css selector', '.c_2 p span')
#initiator_object$clickElement()

# define party of first initiator
#party <- remDr$findElement(using = 'css selector', '.tabs-responsive__contentBlock')$getElementText()

remDr$goBack()

# get names of second initiator, if not possible return NA
  
  tryCatch({
    suppressMessages({
      initiator_object2 <- remDr$findElement(using = 'css selector', 'p+ p span')
      party2 <- initiator_object2$clickElement()$getElementText() %>% unlist(.) %>% ifelse(length(.) == 0, NA, .)
      party2
    })
  }, 
  error = function(e) {
    party2 <- NA_character_
  }
  )  

remDr$closeWindow()

ini_laws <- cbind(beschluss, party, party2) %>%
  as.data.frame() 
}

laws_ini <- future_map_dfr(Laws_2004_ini$link, ini_scraping,  .progress = TRUE) 

Laws_2004_ini <- Laws_2004_es %>% filter(initiated == "Initiativantrag") %>% sample_n(10)

