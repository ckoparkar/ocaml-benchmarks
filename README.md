### ocaml-benchmarks

The easiest way to install dependencies is by using the [Nix](https://nixos.org/download.html)
package manager. We have included a `shell.nix` script here
and running `nix-shell` will get you a shell with the required packages.
Otherwise, you will need to install [opam](https://packages.ubuntu.com/bionic/opam)
and then run the following commands:

```
opam init --auto-setup --bare && \
opam switch create 4.10.0+multicore+no-effect-syntax --packages=ocaml-variants.4.10.0+multicore+no-effect-syntax --repositories=multicore=git+https://github.com/ocaml-multicore/multicore-opam.git,default && \
eval $(opam env) && \
opam install --yes dune.2.7.1 domainslib.0.2.2 sexplib.v0.14.0 containers.2.8.1
```


Some of the benchmarks depend on external data which is not included here to save space;
you can generate it by running `make gen_data` inside this directory.
Then you run the benchmarks one at a time,
e.g. `dune exec ./main.exe -- -p parfib -n 48 -i 9 -j 18` will run the parfib benchmark.
Programs that depend on external data take an additional argument, e.g.
`dune exec ./main.exe -- -p parnearest -n 1 -i 9 -j 18 -a data/plummer_3d_1000000.txt`.
