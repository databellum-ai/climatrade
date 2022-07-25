# https://stackoverflow.com/questions/23412265/how-do-you-send-email-from-r
library(mailR)
sender <- "JES@databellum-ai.com"
recipients <- c("prof02@yahoo.com", "jeloysuarez@gmail.com")
send.mail(from = sender, 
          to = recipients, 
          subject = paste("Prueba de envío automático de email desde databellum", "a las", Sys.time()), 
          body = "<P>Esta es una prueba de envía de email desde la cuenta de <B>databellum</B>",
          smtp = list(host.name = "mx5.servidormx.es", port = 25, 
                      user.name = "JES@databellum-ai.com",            
                      passwd = "", ssl = TRUE), 
authenticate = TRUE, send = TRUE)


# ------------
# https://gist.github.com/sandys/7f436e1a4a998aa62b954a5188290fe7

# first install mailR. It is a bit funky to install this because of the dependency on rJava
#First install java
### sudo add-apt-repository ppa:webupd8team/java
### sudo apt-get update
###     sudo apt-get install oracle-java8-installer

#now you can install mailR. The assumption is that java is in /usr/lib/jvm/jdk1.8.0_66/. Check if a newer version has changed the directory
sudo JAVA_HOME=/usr/lib/jvm/jdk1.8.0_66/  R CMD javareconf 
sudo JAVA_HOME=/usr/lib/jvm/jdk1.8.0_66/  Rscript -e 'install.packages(c("mailR"), .Library.site[1], repos="http://cran.us.r-project.org", dependencies=TRUE)'

# now you can run the following script

library(mailR)
smpt_auth <- list(host.name="email-smtp.eu-west-1.amazonaws.com", port=25, user.name="kkkk", passwd="ppp", ssl=TRUE)
send.mail(from="r-noreply@redcarpetmails.com",to="sss@sss.com", subject="Test file attach R", body = "test script", html=T, smtp=smpt_auth, authenticate=T, encoding="utf-8",attach.files="/tmp/Sample_DB.xlsx")

