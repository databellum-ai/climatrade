# https://stackoverflow.com/questions/23412265/how-do-you-send-email-from-r

library(mailR)
sender <- "datebellum ...the magicians <JES@databellum-ai.com>"
recipients <- c("Eloy <prof02@yahoo.com>")
send.mail(from = sender, 
          to = recipients, 
          cc = c("Eloy2 <jeloysuarez@gmail.com>"),
          bcc = c("Eloy3 <Juan-Eloy.Suarez@dupont.com>"),
          subject = paste("Prueba de envío automático de email desde databellum", "a las", Sys.time()), 
          body = "<P>Esta es una prueba de envía de email desde la cuenta de <B>databellum</B>, donde se pueden poner varios p&aacute;rrafos e incluir gráficos, etc.</P> <IMG SRC='https://www.atriainnovation.com/wp-content/uploads/2021/02/portada-1080x675.jpg' WIDTH=200 ALIGN='left'> Por ejemplo en este caso.",
          smtp = list(host.name = "mx5.servidormx.es", port = 25, 
                      user.name = "JES@databellum-ai.com",            
                      passwd = "DBellum$2021", 
                      ssl = TRUE), 
          authenticate = TRUE, 
          send = TRUE, 
          html=T, 
          attach.files="https://www.atriainnovation.com/wp-content/uploads/2021/02/portada-1080x675.jpg", 
          encoding="utf-8"
          )

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

