#!/usr/bin/env Rscript

source("~/R/climatrade/project2_main/initialize.R")

freshData <- readRDS("~/R/climatrade/project2_main/dataUptodate.rds")
lastValue <- paste("Last retrieved value:",round(freshData[nrow(freshData),2],2), "on", freshData[nrow(freshData),1])

library(mailR)
send.mail(from = senderMail, 
          to = c("JuanE <prof02@yahoo.com>"), 
          cc = c("JuanE2 <jeloysuarez@gmail.com>"),
          # bcc = c("JuanE3 <Juan-Eloy.Suarez@dupont.com>"),
          subject = paste0("Mensaje de databellum ", "a las ", Sys.time(), ": ",lastValue), 
          body = "<P>Esta es una prueba de envía de email desde la cuenta de <B>databellum</B>, donde se pueden poner varios p&aacute;rrafos e incluir gráficos, etc.</P> <IMG SRC='https://databellum-ai.com/db/img/navbar-logo.png' WIDTH=200 ALIGN='left'> Por ejemplo en este caso.",
          smtp = list(host.name = hostMail, port = 25, 
                      user.name = userMail,            
                      passwd = passwordMail, 
                      ssl = TRUE), 
          authenticate = TRUE, 
          send = TRUE, 
          html=T, 
          attach.files="https://www.atriainnovation.com/wp-content/uploads/2021/02/portada-1080x675.jpg", 
          encoding="utf-8"
          )
print(paste("Email SENT at",Sys.time()))

# ========================================================================
# HOW TO INSTALL PACKAGE "mailR" (REQUIRES INSTALL "rJava" PREVIOUSLY)
#
# 1) Install the Java Runtime Environment (JRE).
# $ sudo apt-get install -y default-jre
# 2) Install the Java Development Kit (JDK).
# $ sudo apt-get install -y default-jdk
# 3)Update where R expects to find various Java files.
# $ sudo R CMD javareconf
# 4) Install the package.
# > install.packages("rJava")
# ========================================================================
