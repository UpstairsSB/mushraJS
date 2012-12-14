mushraJS+Server
========

mushraJS is a HTML5 and JavaScript based framework to create MUSHRA listening tests

Because it is made by HTML5, most user want to it working on webserver.

I put it into erlang mochiweb server.

1. git clone this.

2. make

3. ./start-dev.sh

4. put this in erlang shell.
mushradb:reset().

5. access the url. You will see the mushraJS test.
http://localhost:8090/

6. access the url to see the result
http://10.64.85.121:8090/result?t=mobileToPc

enjoy it.
