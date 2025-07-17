ft_turing: install
	opam exec dune build && cp -f _build/install/default/bin/turing ft_turing

run-utm: ft_turing
	./ft_turing programs/utm.json "$$(cat programs/utm_input.txt)"

install:
	opam install --check . || opam install -y --deps-only .

clean:
	rm -rf _build

fclean: clean
	rm -f ft_turing

re: fclean ft_turing