all : latc_x86_64 lib/runtime.o

lexer_src = src/Parser/AbsLatte.hs src/Parser/LexLatte.hs src/Parser/ParLatte.hs src/Parser/ErrM.hs
compiler_src = src/Main.hs src/Compiler.hs src/Typecheck.hs src/Env.hs src/Common.hs src/Generation.hs src/Asm.hs src/BackEnv.hs

latc_x86_64: parser $(compiler_src) $(lexer_src)
	ghc --make $(compiler_src) $(lexer_src) -o $@

parser: src/Latte.cf
	cd src && \
	mkdir -p Parser && cd Parser &&\
	bnfc -haskell --functor ../../$< && \
	happy -gca ParLatte.y && \
	alex -g LexLatte.x && \
	rm LexLatte.x ParLatte.y SkelLatte.hs TestLatte.hs PrintLatte.hs DocLatte.txt

lib/runtime.o: lib/runtime.c
	gcc -Wall -c -o $@ $<

clean:
	rm -rf src/Parser/
	find . -type f \( -name "*.hi" -o -name "*.o" \) -delete
	rm -f $(lexer_src)
	rm -f latc_x86_64
	rm -f lib/runtime.o
	rm -f tests/good/*.s tests/good/*.o
