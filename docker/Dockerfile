FROM ubuntu:14.04
MAINTAINER kulhos
RUN apt-get update && apt-get install -y binutils wget libicu52 elfutils git
RUN locale-gen en_US.UTF-8
RUN mkdir /opt/pip
RUN ln -s /opt/pip /
RUN git clone -b docker https://github.com/kulhos/pip.git /pip/
RUN wget http://sourceforge.net/projects/fis-gtm/files/GT.M%20Installer/v0.13/gtminstall
RUN sh gtminstall --utf8 5.2 --keep-obj --linkexec /pip
#RUN MDIR=`readlink /pip/gtm` && ln -s `dirname ${MDIR}` /pip/gtm_dist
RUN ln -s $(dirname $(readlink /pip/gtm)) /pip/gtm_dist
RUN sh /pip/scripts/create_db.sh /pip

