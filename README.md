# climatrade
# ------------------------------------------------------
# =========================================
# DATA WE EXTRACT:
# ----------------
# Assets values, indexes, outcomes:
# -STOCK&ASSETS PRICES [daily | . | 1981 | YahooFinance] 
# -OECD CLI, BCI, CCI leading indicators [monthly | by_country | 1960 | OECD]
#
# Social symptoms/indicators:
# -Twitter POST SENTIMENTS value for given list of concepts [daily | global, by_concept | ?? | Twitter]
# -SEARCHES RELATIVE-VOLUME OVER TIME [daily | global, by_concept | ?? | GoogleTrends]
# -AIR TRAFFIC [daily | by_city | 2016 | openSkies]
# -MUSIC STYLE of steams [daily | per_country | 2017 | Spotify]
# -FOOTBALL RANKING OF COUNTRIES [monthly | by_country&region | 1992 | FIFA]
#
# Earth influence on facts:
# -MOON PHASES, SUNRISE/SUNSET/NIGHTHOURS [daily | NYC | 1960 | suncalc]
# -DAILY WEATHER in key worldwide cities  [daily | by_city | 1989 | NOAA]
#
# Causality hypothesis ("seed")
# !-KAM (Key Asset to Model)
# !-KCH (KAM Causality Hypothesis)
#
#
# FUTURE DEVELOPMENTS:
# --------------------
# -"Multiseed" capability
# -Scheduled extraction (https://www.sqlservercentral.com/articles/how-to-download-stocks-on-schedule-using-r)
# -Weather: use existing code in directory extraction_other/ to extract data by city
# -Air traffic: use existing code in directory extraction_other/ to extract also data by city
# -GoogleTrends: analyze related search terms, not only interest-over-time
# -GoogleTrends: distinguish search results by country ("geo")
# -Twitter-fechas: valorar/PROBAR con Twitter problema de sólo 10 días hacia atrás...COMPROBAR NIVEL ACTUAL EN TWITTER ("Elevated?"): https://developer.twitter.com/en/docs/twitter-api/getting-started/about-twitter-api#Access
# -Separate Google searches per country
# -Separate Twitter posts per country
# -Twitter posts volume (currently using Google searchs for it)
# -Analysis ("tag cloud", etc.) of related term-searches to list given
# -News combined/coexisting terms (in same article) volume and sentiment

