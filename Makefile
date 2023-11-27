init:
	opam switch create . 5.1.0 --yes
	opam update
	eval $(opam env)

clean:
	opam switch remove . --switch=default --yes
	rm -rf _build
	rm -rf _opam