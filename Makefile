# colors
RED=\033[1;31m
GREEN=\033[1;32m
BLUE=\033[1;34m
PURPLE='\033[01;35m'
NC=\033[0m


define INFO
This Makefile is just used for compiling the runtime.

Please see README.md for more information.

endef
export INFO


compile_runtime:
	clang -S -emit-llvm runtime/lib.c -o runtime/lib.ll

all:
	@echo -e "${GREEN}$$INFO${NC}"
	compile_runtime


.PHONY: all
