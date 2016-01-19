FROM debian:7
MAINTAINER kulhos
ENV DEBIAN_FRONTEND noninteractive

ENV LOCALE en_US.UTF-8

RUN apt-get -y update && apt-get install -y apt-utils locales binutils wget libicu48 elfutils git vim file

# Install needed default locale
RUN echo 'en_US.UTF-8 UTF-8' >> /etc/locale.gen && \
  locale-gen $LOCALE

# Set default locale for the environment
ENV LC_ALL C.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8

RUN mkdir /pip

RUN git clone -b docker https://github.com/kulhos/pip.git /pip/
RUN wget http://sourceforge.net/projects/fis-gtm/files/GT.M%20Installer/v0.13/gtminstall
#ADD http://sourceforge.net/projects/fis-gtm/files/GT.M%20Installer/v0.13/gtminstall /pip/
RUN sh gtminstall --utf8 4.8 --keep-obj --ucaseonly-utils --linkexec /pip --overwrite-existing
#RUN MDIR=`readlink /pip/gtm` && ln -s `dirname ${MDIR}` /pip/gtm_dist
RUN ln -s $(dirname $(readlink /pip/gtm)) /pip/gtm_dist
RUN sh /pip/scripts/create_db.sh /pip

VOLUME /pip
