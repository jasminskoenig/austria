Sys.setenv(LANG = "en")
library(tidyverse)
library(stringi)
library(readxl)
library(tidytext)
library(magrittr)
library(lubridate)

#Basic cleaning data2020
data2020 <- readRDS("data/data20202021raw.rds") %>%
  mutate_if(is.character, str_to_lower) %>%
  mutate(geschaeftszahl = str_remove(geschaeftszahl, ".*zahl")) %>%
  mutate_if(is.character, str_squish) %>%
  mutate(spruch = stringi::stri_replace_all_fixed(spruch, c("ü","ä", "ö", "ß"), c("ue", "ae", "oe", "ss"), vectorize_all = FALSE)) %>%
  mutate(spruch = str_replace_all(spruch, "entscheidungsgruende.*$", "")) %>% #deleting the verdict, in cases which are formated wrongly online
  mutate(spruch = str_replace_all(spruch, "(bgb((l|i|I|)(\\.|)(\\s|)((I|i|)(\\s|)(nr|)(\\.|)(\\s|)))*)", "bgb")) %>% #uniforming the spelling of bgb; problem: whitespace is not uniform yet  mutate(spruch = str_replace_all(spruch, c("in der fassung" = "idf", "i\\.d\\.f\\." = "idf"))) 
  mutate(spruch = str_replace_all(spruch, c("in der fassung" = "idf", "i\\.d\\.f\\." = "idf")))

saveRDS(data2020, "data/data2020cleaned.rds")

#shortening the verdict to the request - the easy version("antrag")
data2020 <- data2020 %>%
  mutate_if(is.character, str_squish) %>%
  mutate(antrag = str_replace_all(entscheidung, "ii\\..*$|ueber die antraege wurde erworgen.*$|ueber den antrag wurde erworgen.*$|zur begruendung fuehrte.*$|zur rechtslage.*$", "")) %>%
  mutate(length = stri_length(entscheidung))

#--------------------------------------------------------------------
#filter for federal laws (sort out statelaw)

#spruch > antrag (has more authority)
data2020 <- data2020 %>%
  mutate(index = str_remove_all(index, "index ")) %>%
  mutate(state = case_when(
    str_starts(index, "l") ~ "Land",
    TRUE ~ "Bund"
  )) 

data2020_land <- data2020 %>%
  filter(state == "Land")

saveRDS(data2020_land, "data/data2020_land.rds")

#sorting out provincial laws
data2020 <- data2020 %>%
  filter(state =="Bund")

#----------------------------------------------------------------------

#sorting out vorlagen (based on art267) 
#sorting out renotifications
data2020 <- data2020 %>%
  mutate(sortout = case_when(
    str_detect(spruch, "gerichtshof der europaeischen union") ~ "1",
    str_detect(antrag, "wiederverlautbarung") ~ "2",
    str_detect(spruch, "wiederverlautbarung") ~ "3",
    TRUE ~ "0")) %>%
  filter(sortout == 0) %>%
  distinct(entscheidung, .keep_all = TRUE) 

#------------------------------------------------------------------------

#creating the variable decision
data2020 <- data2020 %>%
  mutate(decision = case_when(
    str_detect(spruch, "kundmachung|kundzumachen") & str_detect(spruch, "abgewiesen|zurueckgewiesen|abgelehnt|nicht (als |)verfassungswidrig|(keine|nicht) folge gegeben") ~ "partly sustained",
    str_detect(spruch, "kundmachung|kundzumachen") & !str_detect(spruch, "abgewiesen|zurueckgewiesen|abgelehnt|nicht (als |)verfassungswidrig|(keine|nicht) folge gegeben") ~ "sustained",
    !str_detect(spruch, "kundmachung|kundzumachen") & str_detect(spruch, "abgewiesen|zurueckgewiesen|abgelehnt|nicht (als |)verfassungswidrig|(keine|nicht) folge gegeben") ~ "overruled",
    !str_detect(spruch, "kundmachung|kundzumachen") & !str_detect(spruch, "abgewiesen|zurueckgewiesen|abgelehnt|nicht (als |)verfassungswidrig|(keine|nicht) folge gegeben") & str_detect(spruch, "eingestellt") ~ "termination",
    TRUE ~ "other"
  )) 

#-----------------------------------------------------------------------
#extracting the norms from spruch
data2020 <- data2020 %>%
  mutate(norms = str_extract_all(spruch, "((bgb(|\\s|\\si\\s)[0-9]{1,3}+(| )\\/[0-9]{4}+|bgb(|\\s|\\si\\s)[0-9]{4}+(| )\\/[0-9]{1,3}+)|(\\d{4}\\,(\\s|)(bgb(|\\s|\\si\\s|i\\s|i\\si\\s))(\\d{1,3})))")) %>%
  separate(norms, c("norm1", "norm2", "norm3", "norm4", "norm5", "norm6", "norm7", "norm8", "norm9", "norm10", "norm11"), sep = "([\\,])") 

data2021 <- data2020 %>%
  mutate(date2 = dmy(date)) %>%
  filter(date2 > "2021-01-01")

data2020 <- data2020 %>%
  mutate(date2 = dmy(date)) %>%
  filter(date2 < "2021-01-01")

data2020 %>%
  write_tsv("manual/data2020.tsv") 

data2020_norms <- read_excel("manual/data2020_norms.xlsx")

data2020 <- data2020 %>%
  select(-contains("norm"), -decision) %>%
  left_join(data2020_norms, by = "link")

data2021 %>%
  write_tsv("manual/data2021.tsv") 

data2021_norms <- read_excel("manual/data2021_norms.xlsx")

data2021 <- data2021 %>%
  select(-contains("norm"), -decision) %>%
  left_join(data2021_norms, by = "link")

# combine 2021 and 2020 in one df again

data2020 <- data2020 %>%
  bind_rows(data2021)


#-------------------------------------------------------------
#defining the decisions for every single norm

#function to check whether norms are contained in spruch
check_partlysustained <- function(spruch, norm){
  map2(spruch, norm, str_detect) %>% unlist 
} 

#applying function to every norm
data2020_partlysustained <- data2020 %>%
  filter(decision == "partly sustained") %>%
  mutate(spruch = str_remove_all(spruch, "[^.]*(nicht als verfassungswidrig|abgewiesen|zurueckgewiesen|nicht folge|eingestellt)[^.]*\\.")) %>%  #sorting out sentences in which norms are declared not unconstitutional 
  mutate(decision1 = check_partlysustained(spruch, norm1)) %>%
  mutate(decision2 = check_partlysustained(spruch, norm2)) %>%
  mutate(decision3 = check_partlysustained(spruch, norm3)) %>%
  mutate(decision4 = check_partlysustained(spruch, norm4)) %>%
  mutate(decision5 = check_partlysustained(spruch, norm5)) %>%
  mutate(decision6 = check_partlysustained(spruch, norm6)) %>%
  mutate(decision7 = check_partlysustained(spruch, norm7)) %>%
  mutate(decision8 = check_partlysustained(spruch, norm8)) %>%
  mutate(decision9 = check_partlysustained(spruch, norm9)) %>%
  mutate(decision10 = check_partlysustained(spruch, norm10)) %>%
  mutate(decision11 = check_partlysustained(spruch, norm11)) %>%
  mutate_at(vars(matches("decision")), ~str_replace_all(., "TRUE", "sustained")) %>% 
  mutate_at(vars(matches("decision")), ~str_replace_all(., "FALSE", "overruled")) 

#Defining the decision for each norm in not partly sustained cases
data2020 <- data2020 %>%
  filter(decision != "partly sustained") %>% 
  mutate(decision1 = case_when(
    !is.na(norm1) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm1) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision2 = case_when(
    !is.na(norm2) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm2) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision3 = case_when(
    !is.na(norm3) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm3) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision4 = case_when(
    !is.na(norm4) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm4) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision5 = case_when(
    !is.na(norm5) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm5) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision6 = case_when(
    !is.na(norm6) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm6) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision7 = case_when(
    !is.na(norm7) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm7) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision8 = case_when(
    !is.na(norm8) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm8) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision9 = case_when(
    !is.na(norm9) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm9) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision10 = case_when(
    !is.na(norm10) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm10) & str_detect(decision, "overruled") ~ "overruled",
  ),
  decision11 = case_when(
    !is.na(norm11) & str_detect(decision, "sustained") ~ "sustained",
    !is.na(norm11) & str_detect(decision, "overruled") ~ "overruled",
  )) 

#combining both into complete df (party sustained, sustained and overruled cases)
data2020 <- bind_rows(data2020, data2020_partlysustained)

rm(data2020_partlysustained)
#--------------------------------------------------------------

#defining a variable on the plaintiff
data2020 <- data2020 %>%
  mutate(antrag = str_replace_all(antrag, "arbeits\\- und sozialgericht", "arbeitsundsozialgericht")) %>%
  mutate(entscheidung = str_remove_all(entscheidung, "\\(im folgenden(|\\:) (\\w+\\s\\w+|\\w+)\\)")) %>%
  mutate(entscheidung = str_replace_all(entscheidung, "\\s\\s", " ")) %>%
  mutate(plaintiff = case_when(
    str_detect(catchwords, "parteiantrag") ~ "parteiantrag",
    str_detect(catchwords, "individualantrag") ~ "individualantrag",
    str_detect(catchwords, "pruefungsbeschluss") | str_detect(entscheidung, "(im verfassungsgerichtshof bedenken|sind beim verfassungsgerichtshof einerseits bedenken|verfassungsgerichtshof von amts wegen|von amts wegen die verfassungsmaessigkeit)") ~ "vfgh",
    str_detect(antrag, "(abgeordnete(|n)|mitglieder) (zum nationalrat|des nationalrates)") ~ "nationalrat",
    str_detect(antrag, "(abgeordnete(|n)|mitglieder) (zum bundesrat|des bundesrates)") ~ "bundesrat",
    str_detect(entscheidung, "((stell(t|te|en)|beantrag(t|te|en)|begehr(t|te|en))\\sdie\\s\\w+(|\\.)\\slandesregierung|die\\s\\w+(|\\.)\\slandesregierung\\s(stell(t|te|en)|beantrag(t|te|en)|begehr(t|te|en)))") ~ "landesregierung",
    str_detect(antrag, "beim (verfassungsgerichtshof|vfgh) (sind|ist)") ~ "vfgh", #not part of the first coding of vfgh since it is not as safe, safer code is used first
    str_detect(entscheidung, "((stell(t|te|en)|beantrag(t|te|en)|begehr(t|te|en)|beschlos(s|sen))\\s(das|der|dieser)(\\s\\w+\\s|\\s)((\\w+|)(gericht|senat)(|shof)|uvs|ogh|vwgh|olg)|(das|der)(\\s\\w+\\s|\\s)((\\w+|)(gericht|senat)(|shof)|uvs|vwgh|ogh|olg)(\\s\\w+\\s|\\s)(stell(t|te|en)|beantrag(t|te|en)|begehr(t|te|en))|antrag des \\w+gericht)") ~ "gericht",
    str_detect(antrag, "((der|die) antragstell|einschreiter)") ~ "privat",
    str_detect(entscheidung, "(beim (verfassungsgerichtshof|vfgh) (sind|ist)|der verfassungsgerichtshof leitete)") ~ "vfgh", #not part of the first coding of vfgh since it is not as safe, safer code is used first
    str_detect(antrag, "anlassverfahren\\, antragsvorbringen und vorverfahren") ~ "gericht",
    str_detect(entscheidung, "(aus anlass (dieses verfahrens|(einer|der)bei ihm anhaengigen)|\\w+gericht(shof|) hat mit beschlu|(einschreitende|antragstellende)(\\s\\w+\\s|\\s)(\\w+|)(gericht|senat)|der unabhaengige verwaltungssenat)") ~ "gericht",
    str_detect(entscheidung, "((der|die) antragstell|einschreiter|beschwerdefuehrer)") ~ "privat",
    TRUE ~ "other"))

data2020 %>%
  dplyr::count(plaintiff)

#defining a variable with the more general category
#of plaintiff
data2020 <- data2020 %>%
  mutate(plaintiff_category = case_when(
    str_detect(plaintiff, "vfgh|gericht") ~ "Court",
    str_detect(plaintiff, "nationalrat|bundesrat|landesregierung") ~ "Politician",
    str_detect(plaintiff, "individualantrag|parteiantrag|privat") ~ "Person",
    TRUE ~ "Other"
  )) 

data2020 %>%
  dplyr::count(plaintiff_category)

data2020 %>%
  filter(decision != "overruled") %>%
  dplyr::count(plaintiff_category)

#------------------------------------------------
#analyzing the policy area through the index number

data2020 <- data2020 %>%
  mutate(index = str_extract(index, "\\d{2}\\/\\d{2}")) %>% 
  mutate(index = str_remove(index, "\\/")) %>% 
  mutate(applied_without_po = str_remove_all(applied, ".*(eo|aussstrg|vwgg|aussstrg|geg|vwgvg|zpo|gog).*")) %>% 
  mutate_if(is.character, str_squish) %>% 
  mutate(policyarea = case_when(
    str_detect(index, "^(1003|1004|1006|1011|1012)") ~ "Political Rights",
    str_detect(index, "^(3105|44|66|82|65|67|68|6903|6905|2003)") | str_detect(applied, "gesundheit|asvg|bsvg|gsvg") & str_detect(index, "^1") |str_detect(catchwords, "versicherung|familienleben|kinderbetreuungsgeld") ~ "Social",
    str_detect(index, "^(2002|61)") | str_detect(catchwords, "familienleben") ~ "Family",
    str_detect(index, "^(4102|6904)") | str_detect(applied, "(asyl|fremd|aufenthaltsg)") & str_detect(index, "^1") | str_detect(catchwords, "auslaender|fremde|asyl|aufenthaltsrecht|staatsbuergerschaft") ~ "Migration",
    str_detect(index, "^(3101|3102|3103|3104|30|37|38|3901|32|35)") | str_detect(applied, "(kwg|kapitalmarkt|estg)") & str_detect(index, "^1") | str_detect(catchwords, "steuer") ~ "Finances",
    str_detect(index, "^(21|23|26|36|34|97|5|2006|2008)") | str_detect(applied, "io|uwg|insolvenz") & str_detect(index, "^1") |str_detect(catchwords, "gewerberecht|gewerbeberechtigung")~ "Economy",
    str_detect(index, "^(24|25|4101|4104|4107|4108|4109|4110|43|1013)") | str_detect(applied, "(stpo|stgb|stvg)") & str_detect(index, "^1") | str_detect(catchwords, "polizei")  ~ "Security",
    str_detect(index, "^(60|62|63|64|6901|6902)") | str_detect(applied, "bdg") & str_detect(index, "^1") |str_detect(catchwords, "arbeitsrecht") ~ "Employment",
    str_detect(index, "^16") | str_detect(applied, "orf-g") & str_detect(index, "^1") |str_detect(catchwords, "rundfunk") ~ "Media",
    str_detect(index, "^(70|71|72|73|75|76)") |str_detect(catchwords, "hochschulen") ~ "Education",
    str_detect(index, "^74") ~ "Religion",
    str_detect(index, "^(81|83|86|89|8902|8907|8908|8909|80|8901)") | str_detect(catchwords, "umweltschutz") ~ "Environment",
    str_detect(index, "^(9901|9902|9903|9905|9906|90|93|94|96|92)") | str_detect(applied, "(kfg)") & str_detect(index, "^1") ~ "Mobility",
    str_detect(index, "^(2005|98|9901|9902|9903|9905|9906|90|93|94|96|92)") | str_detect(applied, "(mietrecht|wohnung|stadterneuerung|kfg)") & str_detect(index, "^1") |str_detect(catchwords, "mietenrecht|kraftfahrrecht") ~ "Infrastructure",
    str_detect(index, "^(22|27|14|40|1007|2011|2012)") | str_detect(applied, "(avg)") & str_detect(index, "^1") | str_detect(applied, "(eo|aussstrg|vwgg|geg|vwgvg|zpo|gog)") & str_detect(index, "^1") & is.na(applied_without_po) ~ "Judicature",
    TRUE ~ "Other"
  ))  %>%
  distinct(link, .keep_all = TRUE) 

#counting the frequency of the policy areas
data2020%>%
  count(policyarea, sort = TRUE) %>% 
  View()

rm(data2020_norms)
rm(data2021_norms)

saveRDS(data2020, "data/data2020final.rds")



