FROM nixos/nix:2.2.1

RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
RUN nix-channel --update
RUN nix-env -iA nixpkgs.git nixpkgs.yarn nixpkgs.python27Packages.supervisor
RUN git clone --depth 1 https://github.com/justinwoo/easy-purescript-nix.git /easy-purescript-nix
RUN nix-env -f easy-purescript-nix/default.nix -iA purs spago

WORKDIR /project
ADD ./supervisord.conf /etc

EXPOSE 1234
EXPOSE 1235
EXPOSE 9003
CMD ["supervisord", "-c", "/etc/supervisord.conf"]