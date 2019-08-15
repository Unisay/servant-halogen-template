FROM haskell:8

WORKDIR /opt/server
ADD . ./
RUN stack build

EXPOSE 9000
EXPOSE 9001
ENTRYPOINT ["stack", "run"]
