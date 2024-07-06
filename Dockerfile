FROM haskell:9.6

WORKDIR /Osazone

RUN git clone https://github.com/vbcpascal/Osazone-oopsla24 .

RUN stack setup --install-ghc
RUN stack install
