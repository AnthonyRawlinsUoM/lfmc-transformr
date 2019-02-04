all: build install pull
	
stack:
	@docker build --tag=127.0.0.1:5000/lfmc-transformr .
	@docker push 127.0.0.1:5000/lfmc-transformr
		
build:
	@docker build --tag=anthonyrawlinsuom/lfmc-transformr .

install:
	@docker push anthonyrawlinsuom/lfmc-transformr
	
pull:
	@docker pull anthonyrawlinsuom/lfmc-transformr

release:
	./release.sh

clean:
	@docker rmi --force anthonyrawlinsuom/lfmc-transformr