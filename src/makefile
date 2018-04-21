NDIR=$(shell pwd)

INST_DIR=/usr/local/bin

PROXY=

SWIG=swig

INSTALL=/usr/bin/install -c

####### customize Lua environment ######

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

####### customize libclang environment ######

CLANG_VER=3.8
LIBCLANG_INC=/usr/lib/llvm-$(CLANG_VER)/include
LIBCLANG_LIB=/usr/lib/llvm-$(CLANG_VER)/lib
LIBCLANG_STD_INC=$(shell find /usr/lib/llvm-$(CLANG_VER)/lib/clang/ -maxdepth 1 -mindepth 1 | head -n 1)

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

LUA_LIB_DIR=$(shell $(LUA_COMMAND) -e $(LUA_DIR_LIB))
LUA_MOD_DIR=$(shell $(LUA_COMMAND) -e $(LUA_DIR_SCRIPT))

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
COREBASE_SO = libclanglua/coreBase.so
CORE_SO = libclanglua/core.so

LUA_DIR_SCRIPT='for path in string.gmatch( package.path, "[^;]+" ) do if path:find( ".*lua.*%?%.lua" ) then print( (path:gsub( "/%?%.lua", "" )) ); break end end'
LUA_DIR_LIB='for path in string.gmatch( package.cpath, "[^;]+" ) do if path:find( ".*lua.*%?%.so" ) then print( (path:gsub( "/%?%.so", "" )) ); break end end'


ifneq ($(LUA_DIR),)
TARGET_LUA = $(LUA_COMMAND)
endif

all:
	@echo make build
	@echo make build_for_apt [PROXY=http://proxy.hoge:port/]
	@echo make install
	@echo make clean
	@echo make test

build: $(IF_LUA) $(TARGET_LUA) $(CORE_SO) sqlite3 lctags-build

clean:
ifneq ($(LUA_DIR),)
	$(MAKE) -C $(LUA_DIR) clean
endif
	rm -f $(COREBASE_SO) $(CORE_SO)
	rm -f $(IF_LUA) $(IFC_LUA)
	rm -f swig/libClangLua_wrap.* swig/libClangLuaBase_wrap.*
	rm -f clanglua
	rm -f lsqlite3.so
	$(MAKE) -C lctags clean

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
	$(SWIG) -I$(LIBCLANG_INC) -o swig/libClangLuaBase_wrap.c -lua swig/libClangLuaBase.i

swig/libClangLuaBase_wrap.o: swig/libClangLuaBase_wrap.c
	gcc $(OPT_OP) -std=c99 -fPIC -c swig/libClangLuaBase_wrap.c -o swig/libClangLuaBase_wrap.o -I$(LIBCLANG_INC) -I$(LUA_INC) -L$(LIBCLANG_LIB) $(LIBCLANG_SO)


swig/libClangLua.i : $(TARGET_LUA) mkwrap.lua _base.lua $(COREBASE_SO) 
	$(LUA_COMMAND) mkwrap.lua base $(LIBCLANG_STD_INC) $(LIBCLANG_INC) $(LUA_INC) >  mkwrap.log

swig/libClangLua_wrap.c: swig/libClangLua.i 
	$(SWIG) -I$(LIBCLANG_INC) -o swig/libClangLua_wrap.c -lua swig/libClangLua.i 

swig/libClangLua_wrap.o: swig/libClangLua_wrap.c
	gcc $(OPT_OP) -std=c99 -fPIC -c swig/libClangLua_wrap.c -o swig/libClangLua_wrap.o -I$(LUA_INC) -I$(LIBCLANG_INC) -I$(LUA_INC) -L$(LIBCLANG_LIB) $(LIBCLANG_SO)



$(COREBASE_SO): swig/libClangLuaBase_wrap.o $(LUA_LIB)
	@gcc $(OPT_OP)  -fPIC -shared -Wl,-soname,$(COREBASE_SO) -o $(COREBASE_SO) -lrt -ldl -lpthread -lm swig/libClangLuaBase_wrap.o $(LUA_LIB) -L$(LIBCLANG_LIB) $(LIBCLANG_SO) $(LUA_SO)

$(CORE_SO): swig/libClangLua_wrap.o $(LUA_LIB)
	@gcc $(OPT_OP) -fPIC -shared -Wl,-soname,$(CORE_SO) -o $(CORE_SO) -lrt -ldl -lpthread -lm swig/libClangLua_wrap.o $(LUA_LIB) -L$(LIBCLANG_LIB) $(LIBCLANG_SO) $(LUA_SO)


sqlite3: lsqlite3.so

ifeq ($(shell $(LUA_COMMAND) -e 'print( require( "lsqlite3" ) )' 2> /dev/null),)
lsqlite3.so: $(LUASQLITE3_DIR)/lsqlite3.c $(LUASQLITE3_DIR)/sqlite3.c
ifeq ($(USE_BDB_SQLITE),y)
	gcc -std=c99 -fPIC -shared $(OPT_OP) -o lsqlite3.so \
		$(LUASQLITE3_DIR)/lsqlite3.c lctags/lsqlite3.dummy.c \
		-I$(LUA_INC) $(LUA_LIB) -lpthread -ldb_sql-5.3
else
	gcc -std=c99 -fPIC -shared $(OPT_OP) -o lsqlite3.so \
		$(LUASQLITE3_DIR)/lsqlite3.c $(LUASQLITE3_DIR)/sqlite3.c \
		-I$(LUA_INC) $(LUA_LIB) -lpthread
endif
else
lsqlite3.so:
	@echo lsqlite3 is ready
endif

lctags-build:
	$(MAKE) -C lctags LUA_COMMAND=$(LUA_COMMAND) LUA_INC=$(LUA_INC) LUA_LIB=$(LUA_LIB) LUA_CFLAGS="-I$(LUA_INC)" LUA_LDFLAGS=$(LUA_SO)


test: build
ifndef NO_INIT
	$(LUA_COMMAND) $(LCTAGS_LUA) init . $(IT_OP) $(IS_OP) $(IM_OP)
endif
	sync
	rm -rf .lctags/pch
	$(MAKE) test-dbbuild
	LANG= $(MAKE) test-query | \
		sed "s@-I/usr/lib/llvm-[\.[:alnum:]_\-\/]\+@-Iclang@g" | \
		sed "s@$(NDIR)@NDIR@g" | \
		grep -v -e 'Leaving directory' -e 'Entering directory' | \
		sed "s@$(LUA_COMMAND)@lua@g" | sed 's@<hash>[0-9a-f]*</hash>@@g' | \
		sed 's@><@>\n<@g' | \
		tee test-result
	LANG= $(MAKE) LCTAGS_LUA="$(LCTAGS_LUA) --lctags-form json" test-query | \
		sed "s@-I/usr/lib/llvm-[\.[:alnum:]_\-\/]\+@-Iclang@g" | \
		sed "s@$(NDIR)@NDIR@g" | \
		grep -v -e 'Leaving directory' -e 'Entering directory' | \
		sed "s@$(LUA_COMMAND)@lua@g" | sed 's@"hash": "[0-9a-f]*"@@g' | \
		sed 's@,@,\n@g' | \
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
	$(call build-test, test/sub.cpp, hoge )
	$(call build-test, test/sub.cpp, hoge1 )
	$(call build-test, test/sub.cpp, hoge2 )
	$(call build-test, test/sub2.cpp )
	$(call build-test, test/comp.cpp )
	$(call build-test, test/comp-2.cpp )
	$(call build-test, test/comp2.cpp )
	$(call build-test, test/comp3.cpp )
	$(call build-test, test/comp4.cpp )
	$(call build-test, test/comp5.cpp )
	$(call build-test, test/comp6.cpp )
	$(call build-test, test/comp7.cpp )
	$(call build-test, test/comp8.cpp )
	$(call build-test, test/comp9.cpp )
	$(call build-test, test/comp10.cpp )
	$(call build-test, test/comp11.cpp )
	$(call build-test, test/comp12.cpp )
	$(call build-test, test/comp13.cpp )
	$(call build-test, test/comp14.cpp )
	$(call build-test, test/comp15.cpp )
	$(call build-test, test/comp16.cpp )
	$(call build-test, test/comp17.cpp )
	$(call build-test, test/comp18.cpp )
	$(call build-test, test/comp19.cpp )
	$(call build-test, test/class2.cpp )
	$(call build-test, test/class3.cpp )
	$(call build-test, test/class4.cpp )
	$(call build-test, test/class5.cpp )
	$(call build-test, test/inc1.cpp )
	$(call build-test, test/stdinc.cpp )
	$(call build-test, test/stack.cpp )
#	$(call build-test, test/class.cpp )
	$(call build-test, test/sample.c )
	$(call build-test, test/simple.cpp )
	$(call build-test, test/operator.cpp )
	$(call build-test, test/split.cpp )
	$(call build-test, test/split2.cpp )
	$(call build-test, test/enum.cpp )
	$(call build-test, test/enum2.cpp )
	$(call build-test, test/enum3.cpp )
	$(call build-test, test/enum4.cpp )
	$(call build-test, test/incFuncTest.c )
	$(call build-test, test/highlight.cpp )
	$(call build-test, test/inctest.cpp, , -Itest/inc )

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
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/hoge.cpp 337 8
	$(LUA_COMMAND) $(LCTAGS_LUA) split-at test/hoge.cpp 337 8 -ignore-sym-list val1
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



	$(LUA_COMMAND) $(LCTAGS_LUA) addIncRef test/incFuncTest.c /usr/include/string.h
	sed 's@//@@' test/incFuncTest.c | $(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/incFuncTest.c 3 8 -i

	$(MAKE) test-chg-proj


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
	$(LUA_COMMAND) $(LCTAGS_LUA) init $(TEST_LUA_DIR) --lctags-db $(TEST_LUA_DIR)/lctags.sqlite3 $(LOCKLOG) --lctags-log 3 $(IT_OP) $(IS_OP) $(IM_OP)
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
	PATH=$(PATH):$(INST_DIR) $(MAKE) uninstall-lib
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
	if [ -f lsqlite3.so  ]; then $(INSTALL) lsqlite3.so $(LUA_LIB_DIR); fi
	mkdir -p $(LUA_MOD_DIR)/lctags
	$(INSTALL) lctags/*.lua $(LUA_MOD_DIR)/lctags
	$(INSTALL) lctags/*.so $(LUA_LIB_DIR)/lctags
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
	-if [ -f lsqlite3.so ]; then rm -f $(LUA_LIB_DIR)/lsqlite3.so; fi
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
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc $1 $3 -I$(LIBCLANG_STD_INC) -Itest --lctags-recSql  $(LOCKLOG) $(LCTAGS_SERVER) $(BATCH_OP) -std=c++11 --lctags-log 3 --lctags-target $2 >> dump

endef

# check command and install command
ifdef APT_TGT_CMD
check-apt:
	@echo check $(APT_TGT_CMD) ...
ifeq ($(shell bash -c "which $(APT_TGT_CMD)"),)
	@echo install $(APT_TGT_CMD)
	sudo apt-get -o Acquire::http::proxy="$(PROXY)" install $(APT_TGT_PKG)
else
	@echo ready $(APT_TGT_CMD)
endif
endif

download_luasqlite3:
	@echo check luasqlite3 ...
ifeq ($(wildcard $(LUASQLITE3_DIR).zip),)
	@echo install $(luasqlite3)
	make -p $(LUASQLITE3_WORKDIR)
	curl --proxy "$(PROXY)" $(LUASQLITE3_URL) > $(LUASQLITE3_DIR).zip
	(cd $(LUASQLITE3_WORKDIR); unzip -o $(shell basename $(LUASQLITE3_DIR).zip))
else
	@echo ready luasqlite3
endif



build_for_apt:
#	-sudo apt-get -o Acquire::http::proxy="$(PROXY)" update

	@echo check clang-dev ...
ifeq ($(wildcard /usr/lib/llvm-*/clang-c),)
	sudo apt-get -o Acquire::http::proxy="$(PROXY)" install libclang-$(CLANG_VER)-dev
endif

	$(MAKE) APT_TGT_CMD=lua5.3 APT_TGT_PKG=lua5.3 check-apt 

	@echo check lua-dev ...
ifeq ($(wildcard $(LUA_INC)),)
	sudo apt-get -o Acquire::http::proxy="$(PROXY)" install liblua5.3-dev
endif

	@echo check openssl-dev ...
ifeq ($(wildcard /usr/include/openssl/),)
	sudo apt-get -o Acquire::http::proxy="$(PROXY)" install libssl-dev
endif

	$(MAKE) APT_TGT_CMD=swig APT_TGT_PKG=swig3.0 check-apt 
	$(MAKE) APT_TGT_CMD=gtags APT_TGT_PKG=global check-apt 

	$(MAKE) download_luasqlite3

	$(MAKE) build SWIG=swig3.0

