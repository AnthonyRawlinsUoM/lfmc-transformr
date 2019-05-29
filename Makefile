all: build install pull
	
stack:
	@docker build --tag=anthonyrawlinsuom/lfmc-transformr .
	@docker push anthonyrawlinsuom/lfmc-transformr
		
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