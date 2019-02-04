FROM rocker/r-ver:3.4
MAINTAINER Jaehyeon Kim <dottami@gmail.com>


RUN apt-get update
RUN apt-get install -y jq wget supervisor gpg

## Rserve
RUN apt-get install -y libssl-dev
RUN apt-get install -y libprotobuf-dev
RUN apt-get install -y libjq-dev
RUN apt-get install -y curl libcurl4-openssl-dev
RUN apt-get install -y libnetcdf-dev netcdf-bin
RUN apt-get install -y gdal-bin
RUN apt-get install -y libgdal-dev


RUN wget http://www.rforge.net/Rserve/snapshot/Rserve_1.8-5.tar.gz \
    && R CMD INSTALL Rserve_1.8-5.tar.gz

RUN R -e 'install.packages(c("Rook", "rjson", "rgeos", "raster", "ncdf4", "jsonlite", "geojsonio", "rmapshaper", "spdplyr", "rgdal", "curl", "codetools", "jqr", "V8", "httr", "geojsonlint", "gdalUtils"))'

RUN echo '/usr/local/lib/R/lib/' >> /etc/ld.so.conf.d/libR.conf \
    && ldconfig
    
RUN echo '/usr/local/lib/R/site-library/' >> /etc/ld.so.conf.d/libR.conf \
    && ldconfig

RUN mkdir /rserve

COPY ./src/rserve /rserve/

RUN mkdir /rserve/data
RUN mkdir /rserve/data/in
RUN mkdir /rserve/data/out

COPY ./src/api-supervisor.conf /api-supervisor.conf

RUN mkdir -p /media/ramdisk
RUN mount -t tmpfs -o size=2048m tmpfs /media/ramdisk

EXPOSE 8000
CMD ["/usr/bin/supervisord", "-c", "/api-supervisor.conf"]
