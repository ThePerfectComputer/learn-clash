all: run

.PHONY: out/$(MAIN)
out/$(MAIN):
	mkdir -p out
	stack run clash -- --make \
	src/$(MAIN).hs \
	-main-is $(MAIN) \
	-isrc \
	-odir=odir/ \
	-hidir=hidir/ \
	-o out/$(MAIN)

.PHONY: run
run: out/$(MAIN)
	out/$(MAIN)

run-bin-only:
	out/$(MAIN)

.PHONY: clean
clean:
	rm -rf out hidir odir