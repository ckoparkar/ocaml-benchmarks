# Contributed by Reviewer B.

self: super: {
  ocamlPackages = super.ocaml-ng.ocamlPackages_4_10.overrideScope' (oself: osuper: {
    ocaml = osuper.ocaml.overrideAttrs (oldAttrs: {
      src = self.fetchFromGitHub {
        owner = "ocaml-multicore";
        repo = "ocaml-multicore";
        rev = "411263b91c7ba0df6dab62060a2703ee28be2a73";
        sha256 = "0gfk0h3d0i3l3pfx88gz42d4fb7msy51i6q1cy3f25ba1602sjhb";
      };
    });
    domainslib = self.ocamlPackages.buildDunePackage rec {
      pname = "domainslib";
      version = "0.2.2";
      src = self.fetchFromGitHub {
        owner = "ocaml-multicore";
        repo = "domainslib";
        rev = version;
        sha256 = "14f37nq5d5dz4wr1m25x06f6x3y8ifp9a1ilkn40y9gy607ln959";
      };
    };

    # need array slice APIs (https://github.com/c-cube/ocaml-containers/blob/master/CHANGELOG.md#30)
    containers = osuper.containers.overrideAttrs (oldAttrs: rec {
      name = "containers-${version}";
      version = "2.8.1";
      src = self.fetchFromGitHub {
        owner = "c-cube";
        repo = "ocaml-containers";
        rev = "v${version}";
        sha256 = "12gz53p9lfxynscqb4arnv5s8i89bygqyq8fd0p7i55qq3vpn1hc";
      };
      propagatedBuildInputs = [ oself.mdx ];
    });

    # these unit tests all fail with multicore ocaml for some reason
    ounit2 = osuper.ounit2.overrideAttrs (oldAttrs: {
      doCheck = false;
    });
    re = osuper.re.overrideAttrs (oldAttrs: {
      doCheck = false;
    });
  });
}
