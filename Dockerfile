FROM haskell:9.6

WORKDIR /Osazone

RUN stack install --resolver lts-22.27 mtl prettyprinter prettyprinter-ansi-terminal
RUN git clone https://github.com/vbcpascal/Osazone-oopsla24 .

RUN stack setup --install-ghc
RUN stack build
