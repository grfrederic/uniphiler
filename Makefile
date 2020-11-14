# colors
RED=\033[1;31m
GREEN=\033[1;32m
BLUE=\033[1;34m
PURPLE='\033[01;35m'
NC=\033[0m


define INFO
This Makefile is not used for anything, because the
compiler is written in prolog and does not need to
be compiled.

Please see README.md for more information.

endef
export INFO

define STRAJK
 (                              )                      
 )\ )   )               )    ( /(       )           )  
(()/(( /((      ) (  ( /(    )\())   ( /((    (  ( /(  
 /(_))\())(  ( /( )\ )\()) |((_)\ (  )\())\  ))\ )\()) 
(_))(_))(()\ )(_)|(_|(_)\  |_ ((_))\((_)((_)/((_|_))/  
/ __| |_ ((_|(_)_  !| |(_) | |/ /((_) |(_|_|_)) | |_   
\__ \  _| '_/ _` || | / /    ' </ _ \ '_ \ / -_)|  _|  
|___/\__|_| \__,_|/ |_\_\   _|\_\___/_.__/_\___| \__|  
                |__/                                   
endef
export STRAJK


all:
	@echo -e "${GREEN}$$INFO${NC}"
	@echo -e "${RED}$$STRAJK${NC}"


.PHONY: all
