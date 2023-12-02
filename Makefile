init: _switch/
	echo "done"

_switch/:
	opam update
	opam switch create . 5.1.0 --deps-only
	eval $(opam env)

_build/: _switch/
	opam install . --deps-only --yes

clean:
	rm -rf _build
	rm -rf _opam