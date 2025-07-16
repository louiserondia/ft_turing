ft_turing: install
	opam exec dune build && cp _build/install/default/bin/turing ft_turing

install:
	opam install --check . || opam install -y --deps-only .

clean:
	rm -rf _build

fclean: clean
	rm -f ft_turing

re: fclean ft_turing