FROM debian:7
MAINTAINER kulhos
ENV DEBIAN_FRONTEND noninteractive

ENV LOCALE en_US.UTF-8

#RUN apt-get -y update && apt-get install -y apt-utils locales binutils wget libicu48 elfutils git vim file
RUN apt-get -y update && apt-get install -y apt-utils locales binutils wget libicu48 elfutils unzip vim file

# Install needed default locale
RUN echo 'en_US.UTF-8 UTF-8' >> /etc/locale.gen && \
  locale-gen $LOCALE

# Set default locale for the environment
ENV LC_ALL C.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8

ADD https://github.com/kulhos/pip/archive/master.zip /
RUN unzip /master.zip
RUN mv /pip-master /pip
#RUN git clone https://github.com/kulhos/pip.git /pip/
RUN wget -v http://sourceforge.net/projects/fis-gtm/files/GT.M%20Installer/v0.13/gtminstall
#ADD http://sourceforge.net/projects/fis-gtm/files/GT.M%20Installer/v0.13/gtminstall /pip/
RUN sh gtminstall --utf8 4.8 --keep-obj --ucaseonly-utils --linkexec /pip --overwrite-existing
RUN ln -s $(dirname $(readlink /pip/gtm)) /pip/gtm_dist
RUN sh /pip/scripts/build.sh /pip

VOLUME /pip
