#!/bin/sh

env > /home/rstudio/.Renviron
chown rstudio.rstudio /home/rstudio/.Renviron

exec "$@"