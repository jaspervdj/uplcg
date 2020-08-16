FROM haskell:8.8
WORKDIR /opt/uplcg
COPY server/uplcg.cabal server/stack.yaml* /opt/uplcg/
RUN stack build --only-dependencies
COPY .git server /opt/uplcg/
RUN stack build
ENV LANG=C.UTF-8 LC_ALL=C.UTF-8
ENV UPLCG_HOSTNAME=0.0.0.0 UPLCG_PORT=8002 UPLCG_BASE=/uplcg
EXPOSE 8002
CMD stack exec uplcg-server
