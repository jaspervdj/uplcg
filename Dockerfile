FROM haskell:8.8
WORKDIR /opt/cafp
COPY server/cafp.cabal server/stack.yaml* /opt/cafp/
RUN stack build --only-dependencies
COPY server /opt/cafp
RUN stack build
ENV CAFP_HOSTNAME=0.0.0.0 CAFP_PORT=8002 CAFP_BASE=/cafp
EXPOSE 8002
CMD stack exec cafp-server
