

# Data downloaded 25/July/2021
rankingFIFA <- read.csv("fifa_ranking.csv")
class(rankingFIFA)
head(rankingFIFA)
summary(rankingFIFA)
nrow(rankingFIFA)

# =======================================================
# =======================================================
# Project to scrape (but will need splash package due to page is dynamic)
###Exploring website with R Code

library(rvest)
#jalo la pagina
pageNumber = 1 #son 656 paginas , la ultima tiene 12 jugadores
fifa <- read_html(paste("https://www.fifaindex.com/players/fifa20/",pageNumber,"/",sep = ""))

playerNumber = 3 
playerHTML=(fifa %>% html_nodes(".table") %>% html_nodes("tbody") %>% html_nodes("tr"))[playerNumber]

#jalas la primera informacion 
(playerHTML%>%html_nodes("td")%>%html_text())[3:7] #ovr/pot ; name ; positions ; age ; hits

# That information is extracted from inspecting the website. We are obtaining inside a table, tbody, and extracting tr. Inside tag tr, we are extracting all td tags. Specifically td #’s 3 - 7. In other words, we are extracting data from the following elements. (real view from inspecting website)
# It’s noticeable that the first player starts at tr 
# 3. We can notice, that we just extracted Name, Position, Fifa Overall Rating, age, and Number of Times Player was visited (Hits). We also need the club and Nationality for the player. These can be found inside the attributes for the anchor tags.
playerAttributes= playerHTML%>%html_nodes("td")%>%html_nodes("a")%>%html_attrs()
playerAttributes
#==============================
#==============================

# <span class="d-none d-lg-block">Belgium</span><span class="d-block d-lg-none">BEL</span></td>

dateId <- "id13407"
fifa_teams <- read_html("https://www.fifa.com/fifa-world-ranking/men")



fifa_teams %>% html_nodes(".d-flex .fc-ranking-list-full_selected__1rOMN")
fifa_teams %>% html_nodes(".d-none .d-lg-block")
fifa_teams %>% html_nodes(".d-flex .ff-mr-16") %>% html_children()
fifa_teams %>% html_nodes(".fc-ranking-item-full_rankingTableFullCell__3nRbO")


teamNumber <- 1
teamHTML <- (fifa_teams %>% html_nodes("table") %>% html_nodes("tbody") %>% html_nodes("tr"))[teamNumber]
teamHTML

fifa_teams %>% html_nodes("table") %>% html_nodes("tbody") %>% html_nodes("tr")
fifa %>% html_nodes(".table") %>% html_nodes("tbody")


(teamHTML%>%html_nodes("td")%>%html_text())[3:7] #ovr/pot ; name ; positions ; age ; hits

valuation <- fifa_teams %>% html_nodes(xpath = '//*[@id="__next"]/div/main/section[2]/div/div/div[1]/table/tbody/tr[1]/td[3]/span[1]')

fifa_teams %>% html_nodes(".fc-ranking-list-full_rankingTable__1u4hs") %>% html_table() %>% head()

fifa_teams %>% html_nodes(xpath = '//*[@id="__next"]/div/main/section[2]/div/div/div[1]/table/tbody/tr[1]')

valuation <- (fifa_teams %>% html_nodes(".fc-ranking-item-full_rankingTableFullRow__1nbp7 "))[1]
valuation <- (fifa_teams %>% html_nodes(".fc-ranking-list-full_rankingTable__1u4hs .fc-ranking-item-full_rankingTableFullRow__1nbp7 "))[1]
summary(valuation)
valuation


#==============================
#==============================

# So, player attributes will provide Nationality in position [[2]][2], and club in posiiton [[lastPosition]][2]. Therefore, we can have all the information for a given player, like such.

c((playerHTML%>%html_nodes("td")%>%html_text())[3:7],playerAttributes[[2]][2],playerAttributes[[length(playerAttributes)]][2])

# Now, that we know how to pull the information from one player, let’s do this a few thousand times to get all the players information.

##Building Database

#filling database with mock row. Everything is a string to avoid any data mismatch problems. 
baseDeDatos = data.frame("OVRPOT" = "1234","Name" = "Juan","prefPosition" = "RW","Age" = "30","Hits" = "1",
                         "Pais" = "Nicaragua", "Equipo" = "Managua FC", stringsAsFactors = F)


for(pagina in 1:655){ #website cursor
  
  #save the page. Notice we will iterate over all the pages available.
  fifa <- read_html(paste("https://www.fifaindex.com/players/fifa20/",pagina,"/",sep = ""))
  
  #Note 1:
  #there are 30 players per page. They start in tr# 3, and end in tr # 42 
  #there are 2 spaces every 5 players - thats the reason it ends in tr#42 and not tr # 32
  
  #Note 2: 
  #last page has 22 players
  
  if(pagina != 655){
    for(numJugador in 3:42){ #player cursor
      
      #Getting player 1st part info
      jugador=(fifa %>% html_nodes(".table") %>% html_nodes("tbody") %>% html_nodes("tr"))[numJugador]
      
      #ovr/pot;name;positions;age;hits
      primeraParte = (jugador%>%html_nodes("td")%>%html_text())[3:7] 
      
      #we evaluate if what we obtained is a blank space or a player's info:
      if(sum(is.na(primeraParte))==5){
        next
        
      } else {
        #Getting Player's Nationality and Team
        atributosJugador= jugador%>%html_nodes("td")%>%html_nodes("a")%>%html_attrs()
        
        #Joining both pieces
        finalJug = c(primeraParte,atributosJugador[[2]][2],
                     atributosJugador[[length(atributosJugador)]][2])
        
        #adding to database
        baseDeDatos<- rbind(baseDeDatos,finalJug)
        
      }
      
    }
    
  } else {
    
    for(numJugador in 3:30){ #player cursor
      #Getting player 1st part info
      jugador=(fifa %>% html_nodes(".table") %>% html_nodes("tbody") %>%     html_nodes("tr"))[numJugador]
      
      #ovr/pot;name;positions;age;hits
      primeraParte = (jugador%>%html_nodes("td")%>%html_text())[3:7] 
      
      #we evaluate if what we obtained is a blank space or a player's info:
      if(sum(is.na(primeraParte))==5){
        next
        
      } else {
        #Getting Player's Nationality and Team
        atributosJugador= jugador%>%html_nodes("td")%>%html_nodes("a")%>%html_attrs()
        
        #Joining both pieces
        finalJug = c(primeraParte,atributosJugador[[2]][2],
                     atributosJugador[[length(atributosJugador)]][2])
        
        #adding to database
        baseDeDatos<- rbind(baseDeDatos,finalJug)
        
      }
      
    }
    
  }
  
  
}

baseDeDatos


write.csv(baseDeDatos,"fifaPlayersDB.csv")
