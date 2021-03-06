.PHONY: elf2hex test clean all

all: src/Decode.hs elf2hex test

src/Decode.hs: src/gen.hs src/Decode_base.hs
	stack setup; stack install split; stack install text; cd src; stack runhaskell gen.hs; cd ..; stack build

elf2hex:
	$(MAKE) -C elf2hex

test:
	$(MAKE) -C test

clean:
	rm -f src/Decode.hs
	rm -rf .stack-work/
	$(MAKE) -C elf2hex clean
	$(MAKE) -C test clean
	rm src/*.o src/*.hi
