.PHONY: erlesy
erlesy:
	make -C erlesy

shell:
	erl -sname erlesy -setcookie erlesy -pa erlesy/deps/*/ebin -pa erlesy/ebin -s otp_parser_app

http:
	cd erlesy_vis && python -m SimpleHTTPServer 8000

tests:
	make -C erlesy eunit