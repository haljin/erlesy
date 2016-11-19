.PHONY: erlesy
erlesy:
	make -C erlesy

shell:
	erl -pa erlesy/deps/*/ebin -pa erlesy/ebin

http:
	cd erlesy_vis && python -m SimpleHTTPServer 8000
