vendors:
	test ! -d $@
	mkdir vendors
	@./source.sh

echo.hvt.target: | vendors
	@echo " BUILD main.exe"
	@dune build --root . --profile=release ./main.exe
	@echo " DESCR main.exe"
	@$(shell dune describe location \
		--context solo5 --no-print-directory --root . --display=quiet \
		./main.exe 1> $@ 2>&1)

echo.hvt: echo.hvt.target
	@echo " COPY echo.hvt"
	@cp $(file < echo.hvt.target) $@
	@chmod +w $@
	@echo " STRIP echo.hvt"
	@strip $@

# echo.install: echo.hvt
# 	@echo " GEN echo.install"
# 	@ocaml install.ml > $@

all: echo.hvt | vendors

.PHONY: clean
clean:
	if [ -d vendors ] ; then rm -fr vendors ; fi
	rm -f echo.hvt.target
	rm -f echo.hvt
	rm -f echo.install

# install: echo.intall
# 	@echo " INSTALL echo"
# 	opam-installer echo.install
