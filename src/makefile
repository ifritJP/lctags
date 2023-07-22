NDIR=$(shell pwd)

INST_DIR=/usr/local/bin

PROXY=

SWIG=swig

INSTALL=/usr/bin/install -c

SO=so
## cygwin
#SO=dll

####### customize Lua environment ######

## cygwin
# LUA_COMMAND=lua
# LUA_INC=/usr/include
# LUA_LDFLAGS=
# LUA_SO=-llua

# LUA_COMMAND=lua5.1
# LUA_INC=/usr/include/lua5.1
# LUA_LDFLAGS=
# LUA_SO=-llua5.1

LUA_COMMAND=lua5.3
LUA_INC=/usr/include/lua5.3
LUA_LDFLAGS=
LUA_SO=-llua5.3

# LUA_COMMAND=luajit
# LUA_INC=/usr/include/luajit-2.0
# LUA_LDFLAGS=
# LUA_SO=-lluajit-5.1

# LUA_COMMAND=./clanglua
# LUA_CFLAGS=-fPIC -DLUA_COMPAT_5_1 -DLUA_COMPAT_5_2 -I$(NDIR)/$(LUA_DIR)
# LUA_DIR=../external/lua/lua-5.3.4/src
# LUA_INC=$(NDIR)/$(LUA_DIR)
# LUA_LIB=$(LUA_DIR)/liblua.a
# LUA_INST_DIR=/usr/local/bin
# LUA_LIB_DIR=/usr/local/lib/lua/5.3
# LUA_LDFLAGS=-lncursesw

TEST_LUA_DIR=../external/lua/lua-5.3.4/src

LUA_URL="https://www.lua.org/ftp/lua-5.3.4.tar.gz"


PYTHON=python3

LUA_COMMAND_FULL:=$(shell $(PYTHON) -c 'import os; print( os.path.abspath( "$(shell which $(LUA_COMMAND))" ) );' )

####### customize libclang environment ######

## cygwin
# LIBCLANG_INC=/usr/include
# LIBCLANG_LIB=/lib
# LIBCLANG_STD_INC=/lib/clang/5.0.1/include


CLANG_VER=15
LIBCLANG_BASE_DIR:=$(wildcard /usr/lib/llvm-$(CLANG_VER))
#LIBCLANG_INC=/usr/lib/llvm-$(CLANG_VER)/include
LIBCLANG_INC=$(LIBCLANG_BASE_DIR)/include
#LIBCLANG_LIB=/usr/lib/llvm-$(CLANG_VER)/lib
LIBCLANG_LIB=$(LIBCLANG_BASE_DIR)/lib
LIBCLANG_STD_INC=$(shell find $(LIBCLANG_LIB)/clang/ -iname 'stddef.h' | head -n 1 )
LIBCLANG_STD_INC:=$(patsubst %/stddef.h,%,$(LIBCLANG_STD_INC))

# LIBCLANG_INC=/usr/lib/llvm-$(CLANG_VER)/include
# LIBCLANG_LIB=/usr/lib/llvm-$(CLANG_VER)/lib
# LIBCLANG_STD_INC=$(shell find /usr/lib/llvm-$(CLANG_VER)/lib/clang/ -maxdepth 1 -mindepth 1 | head -n 1)

# LIBCLANG_DIR=/proj/next/clang/r34-final
# LIBCLANG_INC=../external/clang/r390/llvm/tools/clang/include
# LIBCLANG_STD_INC=../external/clang/r390/build/lib/clang/3.9.0/include
# LIBCLANG_LIB=/usr/lib/llvm-$(CLANG_VER)/lib

# LIBCLANG_SO=-lclang

LIBCLANG_SO=-lclang

####### customize luasqlite3 environment ######
# If you don't have luasqlite3, you must customize this setting.
# http://lua.sqlite.org/index.cgi/index

LUASQLITE3_WORKDIR=../external/luasqlite3
LUASQLITE3_DIR=$(LUASQLITE3_WORKDIR)/lsqlite3_fsl09x
#USE_BDB_SQLITE=y

LUASQLITE3_URL="http://lua.sqlite.org/index.cgi/zip/lsqlite3_fsl09x.zip?uuid=fsl_9x"



####### make rules ######

LUA_LIB_DIR=$(shell echo $(LUA_DIR_LIB) | $(LUA_COMMAND) )
LUA_MOD_DIR=$(shell echo $(LUA_DIR_SCRIPT) | $(LUA_COMMAND) )

LCTAGS_LOG=4
JOBS=3

ifdef LCTAGS_PROF
LCTAGS_PROF_OP = --lctags-prof
endif

LCTAGS_LOCKLOG=y
ifdef LCTAGS_LOCKLOG
LOCKLOG = --lctags-lockLog
endif

ifdef LCTAGS_SERVER_ON
LCTAGS_SERVER=--lctags-srv
endif

LUA_LIB:=$(addprefix $(NDIR)/,$(LUA_LIB))

LCTAGS_LUA=lctags/lctags.lua

#OPT_OP = -O3

IF_LUA = libclanglua/if.lua
IFC_LUA = libclanglua/ifc.lua
COREBASE_SO = libclanglua/coreBase.$(SO)
CORE_SO = libclanglua/core.$(SO)

LUA_DIR_SCRIPT='for path in string.gmatch( package.path, "[^;]+" ) do if path:find( ".*lua.*%?%.lua" ) then print( (path:gsub( "/%?%.lua", "" )) ); break end end'
LUA_DIR_LIB="for path in string.gmatch( package.cpath, '[^;]+' ) do if path:find( \".*lua.*%?%.$(SO)\" ) then print( (path:gsub( \"/%?%.$(SO)\", '' ))); break end end"



ifneq ($(LUA_DIR),)
TARGET_LUA = $(LUA_COMMAND)
endif

all:
	@echo make build
	@echo make build_for_apt [PROXY=http://proxy.hoge:port/]
	@echo make install
	@echo make clean
	@echo make test
	@echo make docker

docker:
	(cd ../docker/; docker-compose build --progress plain)

build: $(IF_LUA) $(TARGET_LUA) $(CORE_SO) sqlite3 lctags-build

clean:
ifneq ($(LUA_DIR),)
	$(MAKE) -C $(LUA_DIR) clean
endif
	rm -f $(COREBASE_SO) $(CORE_SO)
	rm -f $(IF_LUA) $(IFC_LUA)
	rm -f swig/libClangLua_wrap.* swig/libClangLuaBase_wrap.*
	rm -f clanglua
	rm -f lsqlite3.$(SO)
	$(MAKE) -C lctags SO=$(SO) clean

$(LUA_LIB):
	$(MAKE) -C $(LUA_DIR) linux ALL=liblua.a MYCFLAGS="$(LUA_CFLAGS)" MYLDFLAGS="$(LUA_LDFLAGS)"

$(IF_LUA): $(TARGET_LUA) mkwrap.lua _base.lua $(CORE_SO) 
	$(LUA_COMMAND) mkwrap.lua core $(LIBCLANG_STD_INC) $(LIBCLANG_INC) $(LUA_INC) >  mkwrap.log

$(TARGET_LUA): $(LUA_LIB)
ifneq ($(LUA_DIR),)
	$(MAKE) -C $(LUA_DIR) linux ALL=lua.o MYCFLAGS="$(LUA_CFLAGS)" MYLDFLAGS="$(LUA_LDFLAGS)"
	$(MAKE) -C $(LUA_DIR) linux LUA_T=$(NDIR)/clanglua ALL=$(NDIR)/clanglua MYLDFLAGS="$(LUA_LDFLAGS)"
endif


swig/libClangLuaBase_wrap.c: swig/libClangLuaBase.i 
	$(SWIG) -I$(LIBCLANG_INC) -DLLVM_CLANG_C_EXTERN_C_END -DLLVM_CLANG_C_EXTERN_C_BEGIN \
					-o swig/libClangLuaBase_wrap.c -lua swig/libClangLuaBase.i

swig/libClangLuaBase_wrap.o: swig/libClangLuaBase_wrap.c
	gcc $(OPT_OP) -std=c99 -fPIC -c swig/libClangLuaBase_wrap.c -o swig/libClangLuaBase_wrap.o -I$(LIBCLANG_INC) -I$(LUA_INC) -L$(LIBCLANG_LIB) $(LIBCLANG_SO)


swig/libClangLua.i : $(TARGET_LUA) mkwrap.lua _base.lua $(COREBASE_SO) 
	$(LUA_COMMAND) mkwrap.lua base $(LIBCLANG_STD_INC) $(LIBCLANG_INC) $(LUA_INC) >  mkwrap.log

swig/libClangLua_wrap.c: swig/libClangLua.i 
	$(SWIG) -I$(LIBCLANG_INC) -DLLVM_CLANG_C_EXTERN_C_END -DLLVM_CLANG_C_EXTERN_C_BEGIN \
					-o swig/libClangLua_wrap.c -lua swig/libClangLua.i 

swig/libClangLua_wrap.o: swig/libClangLua_wrap.c
	gcc $(OPT_OP) -std=c99 -fPIC -c swig/libClangLua_wrap.c -o swig/libClangLua_wrap.o -I$(LUA_INC) -I$(LIBCLANG_INC) -I$(LUA_INC) -L$(LIBCLANG_LIB) $(LIBCLANG_SO)



$(COREBASE_SO): swig/libClangLuaBase_wrap.o $(LUA_LIB)
	@gcc $(OPT_OP)  -fPIC -shared -Wl,-soname,$(COREBASE_SO) -o $(COREBASE_SO) -lrt -ldl -lpthread -lm swig/libClangLuaBase_wrap.o $(LUA_LIB) -L$(LIBCLANG_LIB) $(LIBCLANG_SO) $(LUA_SO)

$(CORE_SO): swig/libClangLua_wrap.o $(LUA_LIB)
	@gcc $(OPT_OP) -fPIC -shared -Wl,-soname,$(CORE_SO) -o $(CORE_SO) -lrt -ldl -lpthread -lm swig/libClangLua_wrap.o $(LUA_LIB) -L$(LIBCLANG_LIB) $(LIBCLANG_SO) $(LUA_SO)


sqlite3: lsqlite3.$(SO)

ifeq ($(shell $(LUA_COMMAND) -e 'print( require( "lsqlite3" ) )' 2> /dev/null),)
lsqlite3.$(SO): $(LUASQLITE3_DIR)/lsqlite3.c $(LUASQLITE3_DIR)/sqlite3.c
ifeq ($(USE_BDB_SQLITE),y)
	gcc -std=c99 -fPIC -shared $(OPT_OP) -o lsqlite3.$(SO) \
		$(LUASQLITE3_DIR)/lsqlite3.c lctags/lsqlite3.dummy.c \
		-I$(LUA_INC) $(LUA_LIB) $(LUA_SO) -lpthread -ldb_sql-5.3
else
	gcc -std=c99 -fPIC -shared $(OPT_OP) -o lsqlite3.$(SO) \
		$(LUASQLITE3_DIR)/lsqlite3.c $(LUASQLITE3_DIR)/sqlite3.c \
		-I$(LUA_INC) $(LUA_LIB) $(LUA_SO) -lpthread
endif
else
lsqlite3.$(SO):
	@echo lsqlite3 is ready
endif

lctags-build:
	$(MAKE) -C lctags LUA_COMMAND=$(LUA_COMMAND) LUA_INC=$(LUA_INC) LUA_LIB=$(LUA_LIB) LUA_CFLAGS="-I$(LUA_INC)" LUA_LDFLAGS=$(LUA_SO) SO=$(SO)


test: build
ifndef NO_INIT
	$(LUA_COMMAND) $(LCTAGS_LUA) init . $(IT_OP) $(IS_OP) $(IM_OP)
endif
	sync
	rm -f 
	emacs -batch -q -eval "(progn (add-to-list 'load-path \"lisp\") (load \"lctags-test.el\"))"
	cat lisp/lctags_httpd_var.py | sed 's/^#.*//g' | sed 's/^[[:space:]]*//g' > /tmp/lctags_httpd_var.py
	cat servlet-api-info | sed 's/"--lctags-candidateLimit"/\n"--lctags-candidateLimit"/g' | \
		sed 's/))], /))],\n/g' | sed 's/[[:space:]]$$//g' > /tmp/servlet-api-info
	diff -B /tmp/servlet-api-info /tmp/lctags_httpd_var.py

	rm -f test-result test-result2
	rm -rf .lctags/pch
	$(MAKE) test-dbbuild
	LANG= $(MAKE) test-query LUA_COMMAND=$(LUA_COMMAND_FULL) | \
		sed "s@-I/usr/lib/llvm-[\.[:alnum:]_\-\/]\+@-Iclang@g" | \
		sed "s@$(NDIR)@NDIR@g" | \
		grep -v -e 'Leaving directory' -e 'Entering directory' | \
		sed "s@$(LUA_COMMAND)@lua@g" | sed 's@<hash>[0-9a-f]*</hash>@@g' | \
		grep -v 'clang/stdinc' | \
		sed 's@><@>\n<@g' | \
		tee test-result
	LANG= $(MAKE) LUA_COMMAND=$(LUA_COMMAND_FULL) \
		LCTAGS_LUA="$(LCTAGS_LUA) --lctags-form json" test-query | \
		sed "s@-I/usr/lib/llvm-[\.[:alnum:]_\-\/]\+@-Iclang@g" | \
		sed "s@$(NDIR)@NDIR@g" | \
		grep -v -e 'Leaving directory' -e 'Entering directory' | \
		sed "s@$(LUA_COMMAND)@lua@g" | sed 's@"hash": "[0-9a-f]*"@@g' | \
		grep -v 'clang/stdinc' | \
		sed 's@,@,\n@g' | grep -v -i 'id": ' | \
		tee test-result2
	$(MAKE) test-stack
	- diff -c test-result.expect test-result && echo OK
	- diff -c test-result2.expect test-result2 && echo OK
ifdef TEST_DB_DUMP
	rm -f db.dump
	$(call dumpDB, lctags.sqlite3, db.dump, test/*.cpp, test/*.h)
	diff -c db.dump test-db.dump.expect
endif


test-simple:
	$(LUA_COMMAND) $(LCTAGS_LUA) init .
	$(LUA_COMMAND) $(LCTAGS_LUA) addStdInc -I$(LIBCLANG_STD_INC)
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc test/simple.cpp -I$(LIBCLANG_STD_INC)


ifdef BATCH
BATCH_OP=--lctags-only-reg
endif

ifeq ($(INDIV),y)
INDIV_OP=--lctags-indiv
endif


test-dbbuild:
	mkdir -p sql
ifdef LCTAGS_SERVER_ON
	$(LUA_COMMAND) $(LCTAGS_LUA) server start $(LOCKLOG) $(INDIV_OP) --lctags-log 2 &
	sleep 1
endif
	$(LUA_COMMAND) $(LCTAGS_LUA) addStdInc -I$(LIBCLANG_STD_INC)
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc test/hoge.cpp -I$(LIBCLANG_STD_INC) -Itest --lctags-recSql $(LOCKLOG) $(LCTAGS_SERVER) $(BATCH_OP) --lctags-log 5 | tee dump | grep -e ':[[:space:]][12][[:space:]]'
	$(call build-test, -std=c++11,test/sub.cpp, hoge )
	$(call build-test, -std=c++11,test/sub.cpp, hoge1 )
	$(call build-test, -std=c++11,test/sub.cpp, hoge2 )
	$(call build-test, -std=c++11,test/sub2.cpp )
	$(call build-test, -std=c++11,test/comp.cpp )
	$(call build-test, -std=c++11,test/comp-2.cpp )
	$(call build-test, -std=c++11,test/comp2.cpp )
	$(call build-test, -std=c++11,test/comp3.cpp )
	$(call build-test, -std=c++11,test/comp4.cpp )
	$(call build-test, -std=c++11,test/comp5.cpp )
	$(call build-test, -std=c++11,test/comp6.cpp )
	$(call build-test, -std=c++11,test/comp7.cpp )
	$(call build-test, -std=c++11,test/comp8.cpp )
	$(call build-test, -std=c++11,test/comp9.cpp )
	$(call build-test, -std=c++11,test/comp10.cpp )
	$(call build-test, -std=c++11,test/comp11.cpp )
	$(call build-test, -std=c++11,test/comp12.cpp )
	$(call build-test, -std=c++11,test/comp13.cpp )
	$(call build-test, -std=c++11,test/comp14.cpp )
	$(call build-test, -std=c++11,test/comp15.cpp )
	$(call build-test, -std=c++11,test/comp16.cpp )
	$(call build-test, -std=c++11,test/comp17.cpp )
	$(call build-test, -std=c++11,test/comp18.cpp )
	$(call build-test, -std=c++11,test/comp19.cpp )
	$(call build-test, -std=c++11,test/class2.cpp )
	$(call build-test, -std=c++11,test/class3.cpp )
	$(call build-test, -std=c++11,test/class4.cpp )
	$(call build-test, -std=c++11,test/class5.cpp )
	$(call build-test, -std=c++11,test/inc1.cpp )
	$(call build-test, -std=c++11,test/stdinc.cpp )
	$(call build-test, -std=c++11,test/stack.cpp )
#	$(call build-test, -std=c++11,test/class.cpp )
	$(call build-test, ,test/sample.c )
	$(call build-test, -std=c++11,test/simple.cpp )
	$(call build-test, -std=c++11,test/operator.cpp )
	$(call build-test, -std=c++11,test/split.cpp )
	$(call build-test, -std=c++11,test/split2.cpp )
	$(call build-test, -std=c++11,test/enum.cpp )
	$(call build-test, -std=c++11,test/enum2.cpp )
	$(call build-test, -std=c++11,test/enum3.cpp )
	$(call build-test, -std=c++11,test/enum4.cpp )
	$(call build-test, ,test/incFuncTest.c )
	$(call build-test, -std=c++11,test/highlight.cpp )
	$(call build-test, -std=c++11,test/inctest.cpp, , -Itest/inc )
	$(call build-test, -std=c++11,test/indirect.cpp )
	$(call build-test, -std=c++11,test/macro.cpp )
	$(call build-test, -std=c++11,test/module/mod1.cpp, , -Itest/module )
	$(call build-test, -std=c++11,test/module/sub/mod2.cpp, , -Itest/module )


#	$(LUA_COMMAND) $(LCTAGS_LUA) register -i $(LOCKLOG) $(LCTAGS_SERVER) $(BATCH_OP) --lctags-log 3 | tee --append dump | grep -e '^[12][[:space:]]'
#	$(LUA_COMMAND) $(LCTAGS_LUA) update . $(LOCKLOG) $(LCTAGS_SERVER) $(BATCH_OP) --lctags-log 3 | tee --append dump | grep -e '^[12][[:space:]]'
#	$(LUA_COMMAND) $(LCTAGS_LUA) update . $(LOCKLOG) $(LCTAGS_SERVER) $(BATCH_OP) --lctags-log 3 --lctags-target hoge | tee --append dump | grep -e '^[12][[:space:]]'
ifdef BATCH
	($(LUA_COMMAND) $(LCTAGS_LUA) register -i $(LOCKLOG) --lctags-log ${LCTAGS_LOG} )
	$(LUA_COMMAND) $(LCTAGS_LUA) update . --lctags-log 4 $(LOCKLOG) --lctags-recSql
endif
ifdef LCTAGS_SERVER_ON
	$(LUA_COMMAND) $(LCTAGS_LUA) server stop --lctags-log 4
endif
	sleep 1
	echo 'int ccccc;' >> test/field.h
	$(LUA_COMMAND) $(LCTAGS_LUA) update . --lctags-log 5 > dump.update
	grep -v 'int ccccc;' test/field.h > test/field.work && mv test/field.work test/field.h
	$(LUA_COMMAND) $(LCTAGS_LUA) shrink
	$(LUA_COMMAND) $(LCTAGS_LUA) dump target
	$(LUA_COMMAND) $(LCTAGS_LUA) dump file >> dump
	$(LUA_COMMAND) $(LCTAGS_LUA) rm target hoge2
	$(LUA_COMMAND) $(LCTAGS_LUA) dump target


test-query:
	$(LUA_COMMAND) $(LCTAGS_LUA) dump all >> dump
	$(LUA_COMMAND) $(LCTAGS_LUA) -x sub
	$(LUA_COMMAND) $(LCTAGS_LUA) -x ccccc
	$(LUA_COMMAND) $(LCTAGS_LUA) -xr enum_val1
	$(LUA_COMMAND) $(LCTAGS_LUA) -c sub
	$(LUA_COMMAND) $(LCTAGS_LUA) -c sub$$
	$(LUA_COMMAND) $(LCTAGS_LUA) -c %sub
	$(LUA_COMMAND) $(LCTAGS_LUA) -xT sub
	$(LUA_COMMAND) $(LCTAGS_LUA) -xT sub$$
	$(LUA_COMMAND) $(LCTAGS_LUA) -xT %sub
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 14 15
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 44 8
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 236 23
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 237 23 | sort
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 253 18
	$(LUA_COMMAND) $(LCTAGS_LUA) def-at test/hoge.cpp 204 18
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 168 15
	$(LUA_COMMAND) $(LCTAGS_LUA) call-at test/hoge.cpp 267 15
	$(LUA_COMMAND) $(LCTAGS_LUA) def-at test/hoge.cpp 230 18
	$(LUA_COMMAND) $(LCTAGS_LUA) def-at test/hoge.cpp 231 20
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 284 3
	$(LUA_COMMAND) $(LCTAGS_LUA) def-at test/hoge.cpp 299 12
	$(LUA_COMMAND) $(LCTAGS_LUA) expand test/hoge.cpp 316 29
	-rm -f graph.sample/*
	-mkdir -f graph.sample
	$(LUA_COMMAND) $(LCTAGS_LUA) graph caller ::Callback_t -o graph.sample/caller.svg
	$(LUA_COMMAND) $(LCTAGS_LUA) graph callee ::ns1::ns31::sub2 -o graph.sample/callee.svg
	$(LUA_COMMAND) $(LCTAGS_LUA) graph inc test/hoge.cpp -o graph.sample/inc.png -f png
	$(LUA_COMMAND) $(LCTAGS_LUA) graph incSrc 5 -o graph.sample/incSrc.svg
	$(LUA_COMMAND) $(LCTAGS_LUA) graph symbol ::ns1::g_global -o graph.sample/symbol.svg
	$(LUA_COMMAND) $(LCTAGS_LUA) graph-at callee test/hoge.cpp 218 14 -o graph.sample/at-callee.svg
	$(LUA_COMMAND) $(LCTAGS_LUA) graph-at caller test/hoge.cpp 231 21 -o graph.sample/at-caller.svg
	$(LUA_COMMAND) $(LCTAGS_LUA) graph-at symbol test/hoge.cpp 107 18 -o graph.sample/at-symbol.svg
	$(LUA_COMMAND) $(LCTAGS_LUA) list inc test/hoge.cpp -d 2
	$(LUA_COMMAND) $(LCTAGS_LUA) inq-at test/hoge.cpp 237 24
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/hoge.cpp 212 19
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/hoge.cpp 236 15
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/hoge.cpp 235 18
	-$(LUA_COMMAND) $(LCTAGS_LUA) dump def hoge.cpp | grep ::abcddd && echo =========== NG ===============
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp.cpp 26 17
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp-2.cpp 25 16
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp2.cpp 31 10
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp3.cpp 31 25
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp4.cpp 31 17
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp5.cpp 31 18
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp6.cpp 48 6
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp7.cpp 48 17
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp8.cpp 48 13
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp9.cpp 48 8
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp10.cpp 48 6
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp11.cpp 10 2
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp12.cpp 11 5
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp13.cpp 3 8
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp14.cpp 4 12
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp16.cpp 11 9
	cat test/comp17-2.cpp | $(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp17.cpp 7 11 -i
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp18.cpp 9 9
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp19.cpp 9 21
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/class2.cpp 13 10
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/class3.cpp 16 10
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/class4.cpp 11 7
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/class5.cpp 24 6
	$(LUA_COMMAND) $(LCTAGS_LUA) testOpe test/operator.cpp
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/hoge.cpp 340 12
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/hoge.cpp 340 12 -split-param-list ':o:vvv1:val1,:o:vvv2:val2'
	$(LUA_COMMAND) $(LCTAGS_LUA) dump target test/class5.cpp | sed 's/.*class5.cpp//g' | sed "s@[a-z/\-\.\-]\+/stdinc@stdinc@g"
	$(LUA_COMMAND) $(LCTAGS_LUA) chg-proj . test@./test > /dev/null
	$(LUA_COMMAND) $(LCTAGS_LUA) dump target test/class5.cpp | sed 's/.*class5.cpp//g' | sed "s@[a-z/\-\.\-]\+/stdinc@stdinc@g"
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/split.cpp 62 13
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/split.cpp 65 18
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/split.cpp 68 18
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/split.cpp 71 18
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/split.cpp 74 18
	-$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/split.cpp 77 18 && echo NG === split 77
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/split.cpp 91 20
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/split.cpp 118 20
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/split.cpp 129 0
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/split.cpp 131 43
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/split.cpp 142 25
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/split.cpp 151 7
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/split.cpp 157 43
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/split.cpp 166 0
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/split.cpp 183 4
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/split.cpp 211 1 --lctags-directRet
	$(LUA_COMMAND) $(LCTAGS_LUA) expand test/enum.cpp 9 15
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/enum.cpp 11 9
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/enum2.cpp 8 19
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/enum3.cpp 7 11
	$(LUA_COMMAND) $(LCTAGS_LUA) def-at test/inc1.cpp 2 1
	$(LUA_COMMAND) $(LCTAGS_LUA) def-at test/inc2.h 7 1
	$(LUA_COMMAND) $(LCTAGS_LUA) expand-macro test/stdinc.cpp
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at-all test/highlight.cpp 21 7
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at-all test/highlight.cpp 21 18
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at-all test/highlight.cpp 24 12
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at-all test/highlight.cpp 25 10
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at-all test/highlight.cpp 27 13
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at-all test/highlight.cpp 26 19
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at-all test/highlight.cpp 27 19
	$(LUA_COMMAND) $(LCTAGS_LUA) expand-macro test/inc/inctest1.h
	$(LUA_COMMAND) $(LCTAGS_LUA) call-func test/simple.cpp printf
	sed 's@//@@' test/simple.cpp | $(LUA_COMMAND) $(LCTAGS_LUA) inq-at test/simple.cpp 4 4 -i
	$(LUA_COMMAND) $(LCTAGS_LUA) inq callee ::test_indirect_t
	$(LUA_COMMAND) $(LCTAGS_LUA) inq caller ::test_indirect_t
	$(LUA_COMMAND) $(LCTAGS_LUA) inq callee '::@struct::<struct_test_str_t>::func_t'
	$(LUA_COMMAND) $(LCTAGS_LUA) inq refSym ::@struct::test_str_t::func_t
	$(LUA_COMMAND) $(LCTAGS_LUA) inq matchFile $(NDIR)/test
	$(LUA_COMMAND) $(LCTAGS_LUA) inq defBody ::test_indirect_t
	$(LUA_COMMAND) $(LCTAGS_LUA) inq callPair ::test_indirect_t ::test_sub
	$(LUA_COMMAND) $(LCTAGS_LUA) inq refPair ::test_indirect_t ::test_sub
	$(LUA_COMMAND) $(LCTAGS_LUA) inq refSym ::macro_func
	$(LUA_COMMAND) $(LCTAGS_LUA) inq decl ::mod1_func
	$(LUA_COMMAND) $(LCTAGS_LUA) inq dumpDir
	$(MAKE) test-inq FILEID=$(shell $(LUA_COMMAND) $(LCTAGS_LUA) inq matchFile ./test/module/sub/mod2.cpp   --lctags-form json | json_pp | grep fileId | awk '//{print($$3);}' | tr -d , ) 
	$(LUA_COMMAND) $(LCTAGS_LUA) inq refDir ./test/module/sub
	$(LUA_COMMAND) $(LCTAGS_LUA) inq reqDir ./test/module/sub
	$(LUA_COMMAND) $(LCTAGS_LUA) inq searchFile module
	$(LUA_COMMAND) $(LCTAGS_LUA) inq searchDecl mod1
	(cd test; $(LUA_COMMAND_FULL) ../start.lua -xt mod1_func)
	(cd test/module/sub; $(LUA_COMMAND_FULL) ../../../start.lua -xt mod2_func)

	$(LUA_COMMAND) $(LCTAGS_LUA) addIncRef test/incFuncTest.c /usr/include/string.h
	sed 's@//@@' test/incFuncTest.c | $(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/incFuncTest.c 3 8 -i

	$(MAKE) test-chg-proj

test-inq:
	$(LUA_COMMAND) $(LCTAGS_LUA) inq defAtFileId $(FILEID) 
	$(LUA_COMMAND) $(LCTAGS_LUA) inq refFile $(FILEID) "./test/module/sub/mod2.c"
	$(LUA_COMMAND) $(LCTAGS_LUA) inq reqFile $(FILEID) "./test/module/sub/mod2.c"

test-chg-proj:
	$(LUA_COMMAND) $(LCTAGS_LUA) set-projDir . test@test2 > /dev/null
	-$(LUA_COMMAND) $(LCTAGS_LUA) dump file | grep test/ > /dev/null && (echo ==== NG set-projDir ====; exit 1)
	$(LUA_COMMAND) $(LCTAGS_LUA) set-projDir . test2@test > /dev/null

test-stack:
	$(LUA_COMMAND) $(LCTAGS_LUA) stack test/hoge.cpp --lctags-log 4
	$(LUA_COMMAND) $(LCTAGS_LUA) stack test/stack.cpp --lctags-log 4

test-at-lua:
ifeq ($(shell bash -c "which lctags"),)
	@echo please make install 
	@exit 1
endif
	rm -rf $(TEST_LUA_DIR)/.lctags/pch
	rm -rf $(TEST_LUA_DIR)/lctags.sqlite3-journal
	sync
ifndef NO_INIT
	$(LUA_COMMAND) $(LCTAGS_LUA) init $(TEST_LUA_DIR)/.. --lctags-db $(TEST_LUA_DIR)/lctags.sqlite3 $(LOCKLOG) --lctags-log 3 $(IT_OP) $(IS_OP) $(IM_OP)
endif
ifdef WORK_DIR
	mv $(TEST_LUA_DIR)/lctags.sqlite3 $(WORK_DIR)
	ln -s $(WORK_DIR)/lctags.sqlite3 $(TEST_LUA_DIR)
endif
	$(MAKE) -C $(TEST_LUA_DIR) linux ALL=clean
ifdef BATCH
	LUA_DIR_SCRIPT=$(NDIR)/lctags $(MAKE) -C $(TEST_LUA_DIR) linux ALL=liblua.a MYCFLAGS="$(LUA_CFLAGS) -I$(NDIR)/$(LIBCLANG_STD_INC) $(LCTAGS_PROF_OP) --lctags-only-reg $(LOCKLOG) --lctags-log ${LCTAGS_LOG} --lctags-digestRec --lctags-recSql" CC="lctags build gcc" AR=echo RANLIB=echo | tee dump | grep -e ':[[:space:]][12][[:space:]]'
	(cd $(TEST_LUA_DIR); LUA_DIR_SCRIPT=$(NDIR)/lctags lctags register -i $(LOCKLOG) --lctags-log ${LCTAGS_LOG} ) | tee --append dump | grep -e ':[[:space:]][12][[:space:]]'
	(cd $(TEST_LUA_DIR); LUA_DIR_SCRIPT=$(NDIR)/lctags lctags update . $(LOCKLOG) --lctags-log ${LCTAGS_LOG} -j $(JOBS) $(LCTAGS_PROF_OP) $(INDIV_OP) --lctags-recSql) | tee --append dump | grep -e ':[[:space:]][12][[:space:]]'
else
ifdef LCTAGS_SERVER_ON
	(cd $(TEST_LUA_DIR); $(NDIR)/lctags/lctags server start --lctags-log 2 &)
	sleep 1
endif
	LUA_DIR_SCRIPT=$(NDIR)/lctags $(MAKE) -C $(TEST_LUA_DIR) linux ALL=liblua.a MYCFLAGS="$(LUA_CFLAGS) -I$(NDIR)/$(LIBCLANG_STD_INC) $(LCTAGS_PROF_OP) $(LOCKLOG) $(LCTAGS_SERVER) --lctags-log ${LCTAGS_LOG} --lctags-digestRec --lctags-recSql" CC="lctags build gcc" AR=echo RANLIB=echo | tee dump | grep -e ':[[:space:]][12][[:space:]]'
ifdef LCTAGS_SERVER_ON
	(cd $(TEST_LUA_DIR); $(NDIR)/lctags/lctags server stop)
endif
endif
	mkdir -p $(TEST_LUA_DIR)/digest
	-mv $(TEST_LUA_DIR)/digest.* $(TEST_LUA_DIR)/digest
	ls -l $(TEST_LUA_DIR)/lctags.sqlite3
	$(LUA_COMMAND) $(LCTAGS_LUA) shrink --lctags-db $(TEST_LUA_DIR)/lctags.sqlite3 >> dump
ifdef WORK_DIR
	rm $(TEST_LUA_DIR)/lctags.sqlite3
	mv $(WORK_DIR)/lctags.sqlite3 $(TEST_LUA_DIR)
endif



#	sleep 1
#	touch $(TEST_LUA_DIR)/lauxlib.h
#	$(LUA_COMMAND) $(LCTAGS_LUA) update $(TEST_LUA_DIR) --lctags-db $(TEST_LUA_DIR)/lctags.sqlite3
#	ls -l $(TEST_LUA_DIR)/lctags.sqlite3
	$(LUA_COMMAND) $(LCTAGS_LUA) def-ata $(TEST_LUA_DIR)/lvm.c 477 21 --lctags-db $(TEST_LUA_DIR)/lctags.sqlite3
ifdef TEST_DB_DUMP
	$(LUA_COMMAND) $(LCTAGS_LUA) dump all --lctags-db $(TEST_LUA_DIR)/lctags.sqlite3 >> dump
	-rm db.dump
	$(call dumpDB, $(TEST_LUA_DIR)/lctags.sqlite3, db.dump, $(TEST_LUA_DIR)/*.cpp $(TEST_LUA_DIR)/*.h)
	diff db.dump db.dump.expect
endif

install: build
	$(MAKE) install-lib
	$(MAKE) -C lctags install INST_DIR=$(INST_DIR) \
		LUA_COMMAND=$(LUA_COMMAND) LUA_MOD_DIR=$(LUA_MOD_DIR)

uninstall:
	PATH="$(PATH):$(INST_DIR)" $(MAKE) uninstall-lib
	$(MAKE) -C lctags uninstall INST_DIR=$(INST_DIR)

install-lib:
	mkdir -p $(LUA_LIB_DIR)
	mkdir -p $(LUA_LIB_DIR)/libclanglua
	mkdir -p $(LUA_LIB_DIR)/lctags
	mkdir -p $(LUA_MOD_DIR)
	mkdir -p $(LUA_MOD_DIR)/libclanglua
	mkdir -p $(LUA_MOD_DIR)/lctags
	$(INSTALL) $(CORE_SO) $(LUA_LIB_DIR)/libclanglua
	$(INSTALL) $(IF_LUA) $(IFC_LUA) $(LUA_MOD_DIR)/libclanglua
	if [ -f lsqlite3.$(SO)  ]; then $(INSTALL) lsqlite3.$(SO) $(LUA_LIB_DIR); fi
	mkdir -p $(LUA_MOD_DIR)/lctags
	$(INSTALL) lctags/_lctags.conf $(LUA_MOD_DIR)/lctags
	$(INSTALL) lctags/*.lua $(LUA_MOD_DIR)/lctags
ifneq ($(LIBCLANG_STD_INC),)
	sed "s@.*-- replase by install@  self.clangIncPath = \"$(LIBCLANG_STD_INC)\"@g" lctags/config.lua > /tmp/config.lua
	$(INSTALL) /tmp/config.lua $(LUA_MOD_DIR)/lctags
	rm /tmp/config.lua
else
	$(INSTALL) lctags/config.lua $(LUA_MOD_DIR)/lctags
endif
	$(INSTALL) lctags/*.$(SO) $(LUA_LIB_DIR)/lctags
ifneq ($(LUA_INST_DIR),)
	$(INSTALL) $(LUA_COMMAND) $(LUA_INST_DIR)
endif

uninstall-lib:
ifeq ($(LUA_LIB_DIR),$(shell pwd))
	echo LUA_LIB_DIR is this directrory.

else
	-rm -f $(LUA_LIB_DIR)/libclanglua/*
	-rmdir $(LUA_LIB_DIR)/libclanglua
	-rm -f $(LUA_MOD_DIR)/libclanglua/*
	-rmdir $(LUA_MOD_DIR)/libclanglua
	-rm -f $(LUA_LIB_DIR)/lctags/*
	-rmdir $(LUA_LIB_DIR)/lctags
	-rm -f $(LUA_MOD_DIR)/lctags/*
	-rmdir $(LUA_MOD_DIR)/lctags
	-if [ -f lsqlite3.$(SO) ]; then rm -f $(LUA_LIB_DIR)/lsqlite3.$(SO); fi
endif
ifneq ($(LUA_INST_DIR),)
	-rm $(LUA_INST_DIR)/$(LUA_COMMAND)
endif

define dumpDB
	$(LUA_COMMAND) $(LCTAGS_LUA) dump call --lctags-db $1 $3 | awk '//{print( $$7, $$8, $$9 )}' | sed 's/::[0-9]\+//g' | sort -n >> $2
	$(LUA_COMMAND) $(LCTAGS_LUA) dump def --lctags-db $1 $3| awk '//{print( $$8, $$9, $$10, $$11, $$12, $$13, $$14 )}' | sed 's/::[0-9]\+//g' | sort -n >> $2
	$(LUA_COMMAND) $(LCTAGS_LUA) dump ref --lctags-db $1 $3 | awk '//{print( $$6, $$7, $$8, $$9, $$10, $$12, $$13 )}' | sed 's/::[0-9]\+//g' | sort -n >> $2
endef

define build-test
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc $2 $1 $4 -I$(LIBCLANG_STD_INC) -Itest --lctags-recSql  $(LOCKLOG) $(LCTAGS_SERVER) $(BATCH_OP) --lctags-log 3 --lctags-target $3 >> dump


endef

# check command and install command
ifdef APT_TGT_CMD
check-apt:
	@echo check $(APT_TGT_CMD) ...
ifeq ($(shell bash -c "which $(APT_TGT_CMD)"),)
	@echo install $(APT_TGT_CMD)
	sudo apt-get -o Acquire::http::proxy="$(PROXY)" install -y $(APT_TGT_PKG)
else
	@echo ready $(APT_TGT_CMD)
endif
endif

download_luasqlite3:
	@echo check luasqlite3 ...
ifeq ($(wildcard $(LUASQLITE3_DIR).zip),)
	@echo install $(luasqlite3)
	mkdir -p $(LUASQLITE3_WORKDIR)
	curl --proxy "$(PROXY)" $(LUASQLITE3_URL) -o $(LUASQLITE3_DIR).zip
	(cd $(LUASQLITE3_WORKDIR); unzip -o $(shell basename $(LUASQLITE3_DIR).zip))
else
	@echo ready luasqlite3
endif

download_lua:
	@echo check lua ...
	mkdir -p $(TEST_LUA_DIR)
ifeq ($(wildcard $(TEST_LUA_DIR)/../../*.tar.gz),)
	@echo install $(lua)
	curl --proxy "$(PROXY)" $(LUA_URL) -o $(TEST_LUA_DIR)/../../lua.tar.gz
	(cd $(TEST_LUA_DIR)/../..; tar xvf lua.tar.gz)
else
	@echo ready lua
endif



build_for_apt:
#	-sudo apt-get -o Acquire::http::proxy="$(PROXY)" update

	@echo check clang-dev ...
ifeq ($(wildcard /usr/lib/llvm-*/clang-c),)
	sudo apt-get -o Acquire::http::proxy="$(PROXY)" install -y libclang-$(CLANG_VER)-dev
endif

	$(MAKE) APT_TGT_CMD=lua5.3 APT_TGT_PKG=lua5.3 check-apt 

	@echo check lua-dev ...
ifeq ($(wildcard $(LUA_INC)),)
	sudo apt-get -o Acquire::http::proxy="$(PROXY)" install -y liblua5.3-dev
endif

	@echo check openssl-dev ...
ifeq ($(wildcard /usr/include/openssl/),)
	sudo apt-get -o Acquire::http::proxy="$(PROXY)" install -y libssl-dev
endif

	$(MAKE) APT_TGT_CMD=swig APT_TGT_PKG=swig3.0 check-apt 
	$(MAKE) APT_TGT_CMD=gtags APT_TGT_PKG=global check-apt 

	$(MAKE) download_luasqlite3

	$(MAKE) build SWIG=swig3.0

