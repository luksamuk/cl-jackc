.PHONY: docker-build docker-run

docker-build:
	docker build --rm -t cl-jackc .

docker-test:
	docker run --rm -t --entrypoint=./run-tests.sh cl-jackc

