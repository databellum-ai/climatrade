#!/bin/bash

Rscript ~/R/climatrade/project2_main/extractDataUptodate.R
Rscript ~/R/climatrade/project2_main/sendEmail.R

## OTRAS FORMAS DE LLAMAR DESDE $:
# < a.R --no-save
# R < a.R --vanilla
# R < a.R --save
# R CMD BATCH a.R

# CRONTAB FOR SCHEDULING TASKS:
# =============================
# -Ayuda general sobre CRON: https://help.dreamhost.com/hc/es/articles/215767047-Crear-un-Cron-Job-personalizado
# -View current tasks scheduled:
# $ crontab -l
# 
# -View current tasks scheduled for a user:
# $ crontab -u [user] -l
#   (view other commands: https://phoenixnap.com/kb/how-to-list-display-view-all-cron-jobs-linux)
# 
# -Edit crontab configuration for current user:
# $ crontab -e
#   (this opens editor vim (see commands: https://cambiatealinux.com/comandos-basicos-para-sobrevivir-al-editor-vim)
# 
# -The /etc/crontab file (root user) can be edited using a text editor like nano (easier):
# $ sudo nano /etc/crontab
#   
# -Code used for daily run of script .sh:
# 0 5,17 * * * /scripts/script.sh  
#   */10 * * * * /home/rstudio/R/climatrade/schedule/daily.sh
#   (example command will execute at 5 AM and 5 PM daily. You can specify multiple time stamps by comma-separated)
#   (see other examples: https://tecadmin.net/crontab-in-linux-with-20-examples-of-cron-schedule/)
# (!) It is important to setup EXECUTE permission (chmod) to the .sh file scheduled