PWD=$(shell pwd)

INST_DIR=/usr/local/bin

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
#USE_BDB_SQLITE=y



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

LUA_LIB:=$(addprefix $(PWD)/,$(LUA_LIB))

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
	$(call dumpDB, lctags.sqlite3, test-result)
	LANG= $(MAKE) test-query | \
		sed "s@-I/usr/lib/llvm-[\.[:alnum:]_\-\/]\+@-Iclang@g" | \
		sed "s@$(PWD)@PWD@g" | \
		sed "s@$(LUA_COMMAND)@lua@g" | \
		tee -a test-result
	diff -c test-result.expect test-result && echo OK


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
	$(call build-test, test/class2.cpp )
	$(call build-test, test/class3.cpp )
	$(call build-test, test/class4.cpp )
	$(call build-test, test/class5.cpp )
	$(call build-test, test/inc1.cpp )
#	$(call build-test, test/class.cpp )

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
	$(LUA_COMMAND) $(LCTAGS_LUA) update .
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
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 14 15
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 44 8
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 236 23
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 237 23
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 253 18
	$(LUA_COMMAND) $(LCTAGS_LUA) def-at test/hoge.cpp 204 18
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 168 15
	$(LUA_COMMAND) $(LCTAGS_LUA) call-at test/hoge.cpp 267 15
	$(LUA_COMMAND) $(LCTAGS_LUA) def-at test/hoge.cpp 230 18
	$(LUA_COMMAND) $(LCTAGS_LUA) def-at test/hoge.cpp 231 20
	$(LUA_COMMAND) $(LCTAGS_LUA) ref-at test/hoge.cpp 284 3
	$(LUA_COMMAND) $(LCTAGS_LUA) def-at test/hoge.cpp 299 12
	-rm -f graph.sample/*
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
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/class2.cpp 13 10
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/class3.cpp 16 10
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/class4.cpp 11 7
	$(LUA_COMMAND) $(LCTAGS_LUA) comp-at test/class5.cpp 24 6

test-at-lua:
ifeq ($(shell which lctags),)
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
	LUA_DIR_SCRIPT=$(PWD)/lctags $(MAKE) -C $(TEST_LUA_DIR) linux ALL=liblua.a MYCFLAGS="$(LUA_CFLAGS) -I$(PWD)/$(LIBCLANG_STD_INC) $(LCTAGS_PROF_OP) --lctags-only-reg $(LOCKLOG) --lctags-log ${LCTAGS_LOG} --lctags-digestRec --lctags-recSql" CC="lctags build gcc" AR=echo RANLIB=echo | tee dump | grep -e ':[[:space:]][12][[:space:]]'
	(cd $(TEST_LUA_DIR); LUA_DIR_SCRIPT=$(PWD)/lctags lctags register -i $(LOCKLOG) --lctags-log ${LCTAGS_LOG} ) | tee --append dump | grep -e ':[[:space:]][12][[:space:]]'
	(cd $(TEST_LUA_DIR); LUA_DIR_SCRIPT=$(PWD)/lctags lctags update . $(LOCKLOG) --lctags-log ${LCTAGS_LOG} -j $(JOBS) $(LCTAGS_PROF_OP) $(INDIV_OP) --lctags-recSql) | tee --append dump | grep -e ':[[:space:]][12][[:space:]]'
else
ifdef LCTAGS_SERVER_ON
	(cd $(TEST_LUA_DIR); $(PWD)/lctags/lctags server start --lctags-log 2 &)
	sleep 1
endif
	LUA_DIR_SCRIPT=$(PWD)/lctags $(MAKE) -C $(TEST_LUA_DIR) linux ALL=liblua.a MYCFLAGS="$(LUA_CFLAGS) -I$(PWD)/$(LIBCLANG_STD_INC) $(LCTAGS_PROF_OP) $(LOCKLOG) $(LCTAGS_SERVER) --lctags-log ${LCTAGS_LOG} --lctags-digestRec --lctags-recSql" CC="lctags build gcc" AR=echo RANLIB=echo | tee dump | grep -e ':[[:space:]][12][[:space:]]'
ifdef LCTAGS_SERVER_ON
	(cd $(TEST_LUA_DIR); $(PWD)/lctags/lctags server stop)
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
ifndef SKIP_DUMP
	$(LUA_COMMAND) $(LCTAGS_LUA) dump all --lctags-db $(TEST_LUA_DIR)/lctags.sqlite3 >> dump
	$(call dumpDB, $(TEST_LUA_DIR)/lctags.sqlite3, db.dump)
	diff db.dump db.dump.expect
endif

install: build
	$(MAKE) install-lib
	$(MAKE) -C lctags install INST_DIR=$(INST_DIR) \
		LUA_COMMAND=$(LUA_COMMAND) LUA_MOD_DIR=$(LUA_MOD_DIR)

uninstall:
	$(MAKE) uninstall-lib
	$(MAKE) -C lctags uninstall INST_DIR=$(INST_DIR)

install-lib:
	mkdir -p $(LUA_LIB_DIR)
	mkdir -p $(LUA_LIB_DIR)/libclanglua
	mkdir -p $(LUA_LIB_DIR)/lctags
	mkdir -p $(LUA_MOD_DIR)
	mkdir -p $(LUA_MOD_DIR)/libclanglua
	mkdir -p $(LUA_MOD_DIR)/lctags
	cp $(CORE_SO) $(LUA_LIB_DIR)/libclanglua
	cp $(IF_LUA) $(IFC_LUA) $(LUA_MOD_DIR)/libclanglua
	if [ -f lsqlite3.so  ]; then cp lsqlite3.so $(LUA_LIB_DIR); fi
	mkdir -p $(LUA_MOD_DIR)/lctags
	cp -a lctags/*.lua $(LUA_MOD_DIR)/lctags
	cp -a lctags/*.so $(LUA_LIB_DIR)/lctags
ifneq ($(LUA_INST_DIR),)
	cp $(LUA_COMMAND) $(LUA_INST_DIR)
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
	rm $(LUA_INST_DIR)/$(LUA_COMMAND)
endif

define dumpDB
	$(LUA_COMMAND) $(LCTAGS_LUA) dump call --lctags-db $1 | awk '//{print( $$7, $$8, $$9 )}' | sed 's/::[0-9]\+//g' | sort > $2
	$(LUA_COMMAND) $(LCTAGS_LUA) dump def --lctags-db $1  | awk '//{print( $$8, $$9, $$10, $$11, $$12, $$13, $$14 )}' | sed 's/::[0-9]\+//g' | sort >> $2
	$(LUA_COMMAND) $(LCTAGS_LUA) dump ref --lctags-db $1 | awk '//{print( $$6, $$7, $$8, $$9, $$10, $$12, $$13 )}' | sed 's/::[0-9]\+//g' | sort >> $2
endef

define build-test
	$(LUA_COMMAND) $(LCTAGS_LUA) build gcc $1 -I$(LIBCLANG_STD_INC) -Itest --lctags-recSql  $(LOCKLOG) $(LCTAGS_SERVER) $(BATCH_OP) -std=c++11 --lctags-log 3 --lctags-target $2 >> dump

endef
