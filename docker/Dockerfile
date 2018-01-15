FROM centos:latest


MAINTAINER "priyanka pandit" priyanka.pandit@atos.net

## Add EPEL repo
RUN yum -y install epel-release
RUN yum -y install wget
# Refresh
RUN yum update -y

RUN yum -y install R
# Install R
RUN yum install -y git wget libcurl-devel libpng-devel libjpeg-devel vim R-core R-core-devel libxml2-devel gtk2-devel v8-devel

RUN which R

#RUN wget -O rstudio-latest-x86_64.rpm https://www.rstudio.org/download/latest/stable/desktop/redhat64/rstudio-latest-x86_64.rpm && yum -y install --nogpgcheck rstudio-latest-x86_64.rpm

# R / Shiny specifics
RUN  R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cran.rstudio.com/')"
#ENV PATH /usr/lib/rstudio-server/bin/:$PATH
#ENV LANG en_US.UTF-8

RUN which R

#RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='http://cran.r-project.org', INSTALL_opts='--no-html')"
RUN R -e "install.packages(c('shiny','shinydashboard','shinyBS','entropy','shinyjs','data.table','DT','rpart','randomForest','sampling','e1071','caTools','sm','pastecs','DMwR','corrplot','caret','xgboost','V8','ggplot2','assertthat','rpart.plot'),repos='http://cran.us.r-project.org')"


RUN R -e "install.packages(c('rattle'), repos='http://cran.us.r-project.org', dependencies = T)"


#RUN R -e "install.packages(pkgs ='https://togaware.com/access/rattle_5.0.14.tar.gz', repos=NULL, type='source')"

RUN yum clean all

RUN mkdir -p /srv/shiny-server 

RUN wget https://download3.rstudio.org/centos5.9/x86_64/shiny-server-1.4.0.756-rh5-x86_64.rpm

RUN yum -y install --nogpgcheck shiny-server-1.4.0.756-rh5-x86_64.rpm \
&& rm -rf shiny-server-1.4.0.756-rh5-x86_64.rpm

RUN chown shiny:shiny /var/log/shiny-server \
&& chown shiny:shiny -R /srv/shiny-server \
&& chmod 777 -R /srv/shiny-server \
&& chown shiny:shiny -R /opt/shiny-server/samples/sample-apps \
&& chmod 777 -R /opt/shiny-server/samples/sample-apps

#RUN chown shiny:shiny -R /srv/shiny-server 
#RUN chmod 777 -R /srv/shiny-server

#RUN chown shiny:shiny -R /usr/bin
#RUN chmod 777 -R /usr/bin
 
WORKDIR /srv/shiny-server

RUN git clone https://github.com/priyanka5693/RAtos.git

RUN ls /srv/shiny-server

RUN cp -R /usr/lib64/R/library/shiny/examples/* /srv/shiny-server/

# Copy shiny-server launcher file
RUN pwd

WORKDIR /srv/shiny-server/RAtos/Atos_Innovatos/docker/

RUN ls

RUN pwd

RUN cp -R rattle /usr/lib64/R/library/

#RUN chmod 754 /usr/bin/shiny-server.sh
RUN cp shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod +x /usr/bin/shiny-server.sh
RUN chmod +x /usr/lib64/R/library/rattle

# Clone git repo into shiny server target directory
#RUN git clone https://bitbucket.fsc.atos-services.net/projects/CFL/repos/codex-shared-fab-lab-repo/browse/Industries/Manufacturing/Atos_Innovatos_Generic_2017
EXPOSE 3838

# Execute Shiny server
CMD ["/usr/bin/shiny-server.sh"]
