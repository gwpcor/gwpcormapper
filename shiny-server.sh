#!/bin/sh

# Make sure the directory for individual app logs exists
mkdir -p /var/log/shiny-server
chown shiny.shiny /var/log/shiny-server

env > /home/shiny/.Renviron
chown shiny.shiny /home/shiny/.Renviron

exec shiny-server 2>&1
