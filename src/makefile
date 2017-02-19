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
# LUA_LDFLAGS=-lncursesw

TEST_LUA_DIR=../external/lua/lua-5.3.4/src

####### customize libclang environment ######

LIBCLANG_INC=/usr/lib/llvm-3.8/include
LIBCLANG_LIB=/usr/lib/llvm-3.8/lib
LIBCLANG_STD_INC=/usr/lib/llvm-3.8/lib/clang/3.8.0/include

# LIBCLANG_INC=../external/clang/r390/llvm/tools/clang/include
# LIBCLANG_STD_INC=../external/clang/r390/build/lib/clang/3.9.0/include
# LIBCLANG_LIB=/usr/lib/llvm-3.8/lib

LIBCLANG_SO=-lclang

####### customize luasqlite3 environment ######
# If you don't have luasqlite3, you must customize this setting.
# http://lua.sqlite.org/index.cgi/index

LUASQLITE3_DIR=../external/luasqlite3/lsqlite3_fsl09x


####### make rules ######

LUA_LIB:=$(addprefix $(PWD)/,$(LUA_LIB))

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
	$(LUA_COMMAND) lctags/lctags.lua init .
	$(LUA_COMMAND) lctags/lctags.lua build gcc test/hoge.cpp -I$(LIBCLANG_STD_INC) -Itest --lctags-log 2 --lctags-target hoge
	$(LUA_COMMAND) lctags/lctags.lua build gcc test/sub.cpp -I$(LIBCLANG_STD_INC) -Itest --lctags-log 2 
	$(LUA_COMMAND) lctags/lctags.lua dump > dump
	$(LUA_COMMAND) lctags/lctags.lua -xa sub
	$(LUA_COMMAND) lctags/lctags.lua ref-ata test/hoge.cpp 213 23 hoge

test-at-lua:
ifeq ($(shell which lctags),)
	@echo please make install
	@exit 1
endif
	$(LUA_COMMAND) lctags/lctags.lua init $(TEST_LUA_DIR) --lctags-db $(TEST_LUA_DIR)/lctags.sqlite3 --lctags-log 3 
	$(MAKE) -C $(TEST_LUA_DIR) linux ALL=clean
	$(MAKE) -C $(TEST_LUA_DIR) linux ALL=liblua.a MYCFLAGS="$(LUA_CFLAGS) --lctags-log 3 --lctags-digestRec --lctags-recSql $$<.sql" CC="lctags build gcc" AR=echo RANLIB=echo | tee dump | grep -e ':[[:space:]][12][[:space:]]'
	mkdir -p $(TEST_LUA_DIR)/digest $(TEST_LUA_DIR)/sql
	mv $(TEST_LUA_DIR)/digest.* $(TEST_LUA_DIR)/digest
	mv $(TEST_LUA_DIR)/*.sql $(TEST_LUA_DIR)/sql
	ls -l $(TEST_LUA_DIR)/lctags.sqlite3
	$(LUA_COMMAND) lctags/lctags.lua shrink --lctags-db $(TEST_LUA_DIR)/lctags.sqlite3 >> dump
	ls -l $(TEST_LUA_DIR)/lctags.sqlite3
	$(LUA_COMMAND) lctags/lctags.lua dump --lctags-db $(TEST_LUA_DIR)/lctags.sqlite3 >> dump

install: build
	$(MAKE) install-lib LUA_LIB_DIR=$(shell $(LUA_COMMAND) -e $(LUA_DIR_SCRIPT))
	$(MAKE) -C lctags install

uninstall:
	$(MAKE) uninstall-lib LUA_LIB_DIR=$(shell $(LUA_COMMAND) -e $(LUA_DIR_SCRIPT))
	$(MAKE) -C lctags uninstall

install-lib:
ifeq ($(LUA_LIB_DIR),)
	$(error LUA_LIB_DIR is empty)
endif
	mkdir -p $(LUA_LIB_DIR)
	mkdir -p $(LUA_LIB_DIR)/libclanglua
	cp $(CORE_SO) $(IF_LUA) $(IFC_LUA) $(LUA_LIB_DIR)/libclanglua
	if [ -f lsqlite3.so  ]; then cp lsqlite3.so $(LUA_LIB_DIR); fi
	cp -a lctags $(LUA_LIB_DIR)/

uninstall-lib:
ifeq ($(LUA_LIB_DIR),)
	$(error LUA_LIB_DIR is empty)
endif
	rm -f $(LUA_LIB_DIR)/libclanglua/*
	rmdir $(LUA_LIB_DIR)/libclanglua
	rm -f $(LUA_LIB_DIR)/lctags/*
	rmdir $(LUA_LIB_DIR)/lctags
	if [ -f lsqlite3.so  ]; then rm -f $(LUA_LIB_DIR)/lsqlite3.so; fi
