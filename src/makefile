PWD=$(shell pwd)

INST_DIR=/usr/local/bin

####### customize Lua environment ######

LUA_COMMAND=lua5.3
LUA_INC=/usr/include/lua5.3
LUA_LDFLAGS=-llua5.3

# LUA_COMMAND=luajit
# LUA_INC=/usr/include/luajit-2.0
# LUA_LDFLAGS=-lluajit-5.1

# LUA_COMMAND=./clanglua
# LUA_CFLAGS=-fPIC -DLUA_COMPAT_5_1 -DLUA_COMPAT_5_2 -I$(PWD)/$(LUA_DIR)
# LUA_DIR=../external/lua/lua-5.3.4/src
# LUA_INC=$(PWD)/$(LUA_DIR)
# LUA_LIB=$(LUA_DIR)/liblua.a
# LUA_INST_DIR=/usr/local/bin
# LUA_LIB_DIR=/usr/local/lib/lua/5.3
# LUA_LDFLAGS=-lncursesw

TEST_LUA_DIR=../external/lua/lua-5.3.4/src

####### customize libclang environment ######

LIBCLANG_INC=/usr/lib/llvm-3.8/include
LIBCLANG_LIB=/usr/lib/llvm-3.8/lib
LIBCLANG_STD_INC=/usr/lib/llvm-3.8/lib/clang/3.8.0/include

# LIBCLANG_DIR=/proj/next/clang/r34-final
# LIBCLANG_INC=../external/clang/r390/llvm/tools/clang/include
# LIBCLANG_STD_INC=../external/clang/r390/build/lib/clang/3.9.0/include
# LIBCLANG_LIB=/usr/lib/llvm-3.8/lib

LIBCLANG_SO=-lclang

####### customize luasqlite3 environment ######
# If you don't have luasqlite3, you must customize this setting.
# http://lua.sqlite.org/index.cgi/index

LUASQLITE3_DIR=../external/luasqlite3/lsqlite3_fsl09x


####### make rules ######

LUA_LIB_DIR=$(shell $(LUA_COMMAND) -e $(LUA_DIR_SCRIPT))


LUA_LIB:=$(addprefix $(PWD)/,$(LUA_LIB))

LCTAGS_LUA=lctags/lctags.lua

#OPT_OP = -O3

IF_LUA = libclanglua/if.lua
IFC_LUA = libclanglua/ifc.lua
COREBASE_SO = libclanglua/coreBase.so
CORE_SO = libclanglua/core.so

LUA_DIR_SCRIPT='print( string.gsub( package.cpath, "([^;]+)/%?.so;.*", "%1" ) .. "" )'


ifneq ($(LUA_DIR),)
TARGET_LUA = $(LUA_COMMAND)
endif

all:
	$(MAKE) build

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
	$(MAKE) -C $(LUA_DIR) linux LUA_T=$(PWD)/clanglua ALL=$(PWD)/clanglua MYLDFLAGS="$(LUA_LDFLAGS)"
endif


swig/libClangLuaBase_wrap.c: swig/libClangLuaBase.i 
	swig -I$(LIBCLANG_INC) -o swig/libClangLuaBase_wrap.c -lua swig/libClangLuaBase.i

swig/libClangLuaBase_wrap.o: swig/libClangLuaBase_wrap.c
	gcc $(OPT_OP) -std=c99 -fPIC -c swig/libClangLuaBase_wrap.c -o swig/libClangLuaBase_wrap.o -I$(LIBCLANG_INC) -I$(LUA_INC) -L$(LIBCLANG_LIB) $(LIBCLANG_SO)


swig/libClangLua.i : $(TARGET_LUA) mkwrap.lua _base.lua $(COREBASE_SO) 
	$(LUA_COMMAND) mkwrap.lua base $(LIBCLANG_STD_INC) $(LIBCLANG_INC) $(LUA_INC) >  mkwrap.log

swig/libClangLua_wrap.c: swig/libClangLua.i 
	swig -I$(LIBCLANG_INC) -o swig/libClangLua_wrap.c -lua swig/libClangLua.i 

swig/libClangLua_wrap.o: swig/libClangLua_wrap.c
	gcc $(OPT_OP) -std=c99 -fPIC -c swig/libClangLua_wrap.c -o swig/libClangLua_wrap.o -I$(LUA_INC) -I$(LIBCLANG_INC) -I$(LUA_INC) -L$(LIBCLANG_LIB) $(LIBCLANG_SO)



$(COREBASE_SO): swig/libClangLuaBase_wrap.o $(LUA_LIB)
	@gcc $(OPT_OP)  -fPIC -shared -Wl,-soname,$(COREBASE_SO) -o $(COREBASE_SO) -lrt -ldl -lpthread -lm swig/libClangLuaBase_wrap.o $(LUA_LIB) -L$(LIBCLANG_LIB) $(LIBCLANG_SO)

$(CORE_SO): swig/libClangLua_wrap.o $(LUA_LIB)
	@gcc $(OPT_OP) -fPIC -shared -Wl,-soname,$(CORE_SO) -o $(CORE_SO) -lrt -ldl -lpthread -lm swig/libClangLua_wrap.o $(LUA_LIB) -L$(LIBCLANG_LIB) $(LIBCLANG_SO)


sqlite3: lsqlite3.so

ifeq ($(shell $(LUA_COMMAND) -e 'print( require( "lsqlite3" ) )' 2> /dev/null),)
lsqlite3.so:
	gcc -std=c99 -fPIC -shared $(OPT_OP) -o lsqlite3.so \
		$(LUASQLITE3_DIR)/lsqlite3.c $(LUASQLITE3_DIR)/sqlite3.c \
		-I$(LUA_INC) $(LUA_LIB) -lpthread
else
lsqlite3.so:
	@echo lsqlite3 is ready
endif

lctags-build:
	$(MAKE) -C lctags LUA_COMMAND=$(LUA_COMMAND) LUA_INC=$(LUA_INC) LUA_LIB=$(LUA_LIB) LUA_CFLAGS="-I$(LUA_INC)" LUA_LDFLAGS="$(LUA_LDFLAGS)"


test: build
ifndef NO_INIT
	$(LUA_COMMAND) $(LCTAGS_LUA) init . $(IT_OP) $(IS_OP) $(IM_OP)
endif
	sync
	$(MAKE) test-dbbuild
	env LANG= $(MAKE) test-query | \
		sed "s@-I/usr/lib/llvm-[\.[:alnum:]_\-\/]\+@-Iclang@g" | \
		sed "s@$(PWD)@PWD@g" | \
		tee test-result
	diff test-result.expect test-result && echo OK


test-dbbuild:
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc test/hoge.cpp -I$(LIBCLANG_STD_INC) -Itest --lctags-log 4 | tee dump | grep -e ':[[:space:]][12][[:space:]]'
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc test/sub.cpp -I$(LIBCLANG_STD_INC) -Itest --lctags-log 2  --lctags-target hoge >> dump
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc test/sub.cpp -I$(LIBCLANG_STD_INC) -Itest --lctags-log 2  --lctags-target hoge1 >> dump
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc test/sub.cpp -I$(LIBCLANG_STD_INC) -Itest --lctags-log 2  --lctags-target hoge2 >> dump
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc test/comp.cpp -I$(LIBCLANG_STD_INC) -Itest --lctags-log 3 >> dump
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc test/comp-2.cpp -I$(LIBCLANG_STD_INC) -Itest --lctags-log 3 >> dump
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc test/comp2.cpp -I$(LIBCLANG_STD_INC) -Itest --lctags-log 3 >> dump
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc test/comp3.cpp -I$(LIBCLANG_STD_INC) -Itest --lctags-log 3 >> dump
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc test/comp4.cpp -I$(LIBCLANG_STD_INC) -Itest --lctags-log 3 >> dump
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc test/comp5.cpp -I$(LIBCLANG_STD_INC) -Itest --lctags-log 3 >> dump
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc test/comp6.cpp -I$(LIBCLANG_STD_INC) -Itest --lctags-log 3 >> dump
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc test/comp7.cpp -I$(LIBCLANG_STD_INC) -Itest --lctags-log 3 >> dump
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc test/comp8.cpp -I$(LIBCLANG_STD_INC) -Itest --lctags-log 3 >> dump
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc test/comp9.cpp -I$(LIBCLANG_STD_INC) -Itest --lctags-log 3 >> dump
	sleep 1
	echo 'int ccccc;' >> test/field.h
	$(LUA_COMMAND) $(LCTAGS_LUA) update .
	grep -v 'int ccccc;' test/field.h > test/field.work && mv test/field.work test/field.h

test-query:
	$(LUA_COMMAND) $(LCTAGS_LUA) shrink
	$(LUA_COMMAND) $(LCTAGS_LUA) dump compOp
	$(LUA_COMMAND) $(LCTAGS_LUA) dump dumpFile >> dump
	$(LUA_COMMAND) $(LCTAGS_LUA) rm-tgt hoge2
	$(LUA_COMMAND) $(LCTAGS_LUA) dump compOp
	$(LUA_COMMAND) $(LCTAGS_LUA) dump >> dump
	$(LUA_COMMAND) $(LCTAGS_LUA) -x sub
	$(LUA_COMMAND) $(LCTAGS_LUA) -x ccccc
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 44 8
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 236 23
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 237 23
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 253 18
	$(LUA_COMMAND) $(LCTAGS_LUA) def-at test/hoge.cpp 204 18
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 168 15
	$(LUA_COMMAND) $(LCTAGS_LUA) call-at test/hoge.cpp 267 15
	$(LUA_COMMAND) $(LCTAGS_LUA) def-at test/hoge.cpp 230 18
	$(LUA_COMMAND) $(LCTAGS_LUA) def-at test/hoge.cpp 231 20
	-rm -f graph.sample/*
	$(LUA_COMMAND) $(LCTAGS_LUA) graph caller ::Callback_t -o graph.sample/caller.svg
	$(LUA_COMMAND) $(LCTAGS_LUA) graph callee ::ns1::ns31::sub2 -o graph.sample/callee.svg
	$(LUA_COMMAND) $(LCTAGS_LUA) graph inc test/hoge.cpp -o graph.sample/inc.png -f png
	$(LUA_COMMAND) $(LCTAGS_LUA) graph incSrc 5 -o graph.sample/incSrc.svg
	$(LUA_COMMAND) $(LCTAGS_LUA) graph symbol ::ns1::g_global -o graph.sample/symbol.svg
	$(LUA_COMMAND) $(LCTAGS_LUA) graph-at callee test/hoge.cpp 218 14 -o graph.sample/at-callee.svg
	$(LUA_COMMAND) $(LCTAGS_LUA) graph-at caller test/hoge.cpp 231 21 -o graph.sample/at-caller.svg
	$(LUA_COMMAND) $(LCTAGS_LUA) graph-at symbol test/hoge.cpp 105 18 -o graph.sample/at-symbol.svg
	$(LUA_COMMAND) $(LCTAGS_LUA) list inc test/hoge.cpp -d 2
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/hoge.cpp 212 19
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/hoge.cpp 236 15
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/hoge.cpp 235 18
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp.cpp 26 17
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp-2.cpp 25 16
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp2.cpp 31 10
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp3.cpp 31 25
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp4.cpp 31 17
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp5.cpp 31 18
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp6.cpp 48 5
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp7.cpp 48 17
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp8.cpp 48 13
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/comp9.cpp 48 8

test-at-lua:
ifeq ($(shell which lctags),)
	@echo please make install
	@exit 1
endif
	sync
ifndef NO_INIT
	$(LUA_COMMAND) $(LCTAGS_LUA) init $(TEST_LUA_DIR) --lctags-db $(TEST_LUA_DIR)/lctags.sqlite3 --lctags-log 3 $(IT_OP) $(IS_OP) $(IM_OP)
endif
	$(MAKE) -C $(TEST_LUA_DIR) linux ALL=clean
	-$(MAKE) -C $(TEST_LUA_DIR) linux ALL=liblua.a MYCFLAGS="$(LUA_CFLAGS) --lctags-log 4 --lctags-digestRec --lctags-recSql $$<.sql" CC="lctags build gcc" AR=echo RANLIB=echo | tee dump | grep -e ':[[:space:]][12][[:space:]]'
	mkdir -p $(TEST_LUA_DIR)/digest $(TEST_LUA_DIR)/sql
	-mv $(TEST_LUA_DIR)/digest.* $(TEST_LUA_DIR)/digest
	-mv $(TEST_LUA_DIR)/*.sql $(TEST_LUA_DIR)/sql
	ls -l $(TEST_LUA_DIR)/lctags.sqlite3
	touch $(TEST_LUA_DIR)/lauxlib.h
	$(LUA_COMMAND) $(LCTAGS_LUA) shrink --lctags-db $(TEST_LUA_DIR)/lctags.sqlite3 >> dump
	$(LUA_COMMAND) $(LCTAGS_LUA) update $(TEST_LUA_DIR) --lctags-db $(TEST_LUA_DIR)/lctags.sqlite3
	ls -l $(TEST_LUA_DIR)/lctags.sqlite3
	$(LUA_COMMAND) $(LCTAGS_LUA) def-ata $(TEST_LUA_DIR)/lvm.c 477 21 --lctags-db $(TEST_LUA_DIR)/lctags.sqlite3
	$(LUA_COMMAND) $(LCTAGS_LUA) dump --lctags-db $(TEST_LUA_DIR)/lctags.sqlite3 >> dump

install: build
	$(MAKE) install-lib
	$(MAKE) -C lctags install INST_DIR=$(INST_DIR)

uninstall:
	$(MAKE) uninstall-lib
	$(MAKE) -C lctags uninstall INST_DIR=$(INST_DIR)

install-lib:
	mkdir -p $(LUA_LIB_DIR)
	mkdir -p $(LUA_LIB_DIR)/libclanglua
	cp $(CORE_SO) $(IF_LUA) $(IFC_LUA) $(LUA_LIB_DIR)/libclanglua
	if [ -f lsqlite3.so  ]; then cp lsqlite3.so $(LUA_LIB_DIR); fi
	cp -a lctags $(LUA_LIB_DIR)/
ifneq ($(LUA_INST_DIR),)
	cp $(LUA_COMMAND) $(LUA_INST_DIR)
endif

uninstall-lib:
ifeq ($(LUA_LIB_DIR),$(shell pwd))
	echo LUA_LIB_DIR is this directrory.
else
	-rm -f $(LUA_LIB_DIR)/libclanglua/*
	-rmdir $(LUA_LIB_DIR)/libclanglua
	-rm -f $(LUA_LIB_DIR)/lctags/*
	-rmdir $(LUA_LIB_DIR)/lctags
	-if [ -f lsqlite3.so  ]; then rm -f $(LUA_LIB_DIR)/lsqlite3.so; fi
endif
ifneq ($(LUA_INST_DIR),)
	rm $(LUA_INST_DIR)/$(LUA_COMMAND)
endif
