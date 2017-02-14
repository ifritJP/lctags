####### customize Lua environment ######

LUA_COMMAND=lua5.3
LUA_INC=/usr/include/lua5.3
LUA_LIB=-llua5.3

# LUA_COMMAND=luajit
# LUA_INC=/usr/include/luajit-2.0
# LUA_LIB=-lluajit-5.1


LUA_COMMAND:=$(shell which $(LUA_COMMAND))

# LUA_COMMAND=./clanglua
# LUA_CFLAGS="-fPIC -DLUA_COMPAT_5_1 -DLUA_COMPAT_5_2 -I$(PWD)/$(LUA_DIR)"
# LUA_DIR=../external/lua/lua-5.3.4/src
# LUA_INC=$(PWD)/$(LUA_DIR)
# LUA_LIB=$(LUA_DIR)/liblua.a

####### customize libclang environment ######

LIBCLANG_INC=/usr/lib/llvm-3.8/include
LIBCLANG_LIB=/usr/lib/llvm-3.8/lib
LIBCLANG_STD_INC=/usr/lib/llvm-3.8/lib/clang/3.8.0/include

#LIBCLANG_INC=../external/clang/r390/llvm/tools/clang/include
#LIBCLANG_STD_INC=../external/clang/r390/build/lib/clang/3.9.0/include

####### make rules ######

#OPT_OP = -O3

IF_LUA = libclanglua/if.lua
IFC_LUA = libclanglua/ifc.lua
COREBASE_SO = libclanglua/coreBase.so
CORE_SO = libclanglua/core.so

LUA_DIR_SCRIPT='print( string.gsub( package.cpath, "([^;]+)/%?.so;.*", "%1" ) .. "" )'

all:
	$(MAKE) build

build: $(IF_LUA) $(LUA_COMMAND) $(CORE_SO) 

clean:
	-$(MAKE) -C $(LUA_DIR) clean
	rm -f $(COREBASE_SO) $(CORE_SO)
	rm -f $(IF_LUA) $(IFC_LUA)
	rm -f swig/libClangLua_wrap.* swig/libClangLuaBase_wrap.*
	rm -f clanglua

$(LUA_LIB):
	$(MAKE) -C $(LUA_DIR) linux ALL=liblua.a MYCFLAGS=$(LUA_CFLAGS)

$(IF_LUA): $(LUA_COMMAND) mkwrap.lua _base.lua $(CORE_SO) 
	$(LUA_COMMAND) mkwrap.lua core $(LIBCLANG_STD_INC) $(LIBCLANG_INC) $(LUA_INC) >  mkwrap.log

./clanglua: $(LUA_LIB)
	$(MAKE) -C $(LUA_DIR) linux ALL=lua.o
	$(MAKE) -C $(LUA_DIR) linux LUA_T=$(PWD)/clanglua ALL=$(PWD)/clanglua


swig/libClangLuaBase_wrap.c: swig/libClangLuaBase.i 
	swig -I$(LIBCLANG_INC) -o swig/libClangLuaBase_wrap.c -lua swig/libClangLuaBase.i 

swig/libClangLuaBase_wrap.o: swig/libClangLuaBase_wrap.c
	gcc $(OPT_OP) -fPIC -c swig/libClangLuaBase_wrap.c -o swig/libClangLuaBase_wrap.o -I$(LUA_INC) -I$(LIBCLANG_INC)


swig/libClangLua.i : $(LUA_COMMAND) mkwrap.lua _base.lua $(COREBASE_SO) 
	$(LUA_COMMAND) mkwrap.lua base $(LIBCLANG_STD_INC) $(LIBCLANG_INC) $(LUA_INC) >  mkwrap.log

swig/libClangLua_wrap.c: swig/libClangLua.i 
	swig -I$(LIBCLANG_INC) -o swig/libClangLua_wrap.c -lua swig/libClangLua.i 

swig/libClangLua_wrap.o: swig/libClangLua_wrap.c
	gcc $(OPT_OP) -fPIC -c swig/libClangLua_wrap.c -o swig/libClangLua_wrap.o -I$(LUA_INC) -I$(LIBCLANG_INC)



$(COREBASE_SO): swig/libClangLuaBase_wrap.o $(LUA_LIB)
	@gcc $(OPT_OP)  -fPIC -shared -Wl,-soname,$(COREBASE_SO) -o $(COREBASE_SO) -lrt -ldl -lpthread -lm swig/libClangLuaBase_wrap.o $(LUA_LIB) -L$(LIBCLANG_LIB) -lclang 

$(CORE_SO): swig/libClangLua_wrap.o $(LUA_LIB)
	@gcc $(OPT_OP) -fPIC -shared -Wl,-soname,$(CORE_SO) -o $(CORE_SO) -lrt -ldl -lpthread -lm swig/libClangLua_wrap.o $(LUA_LIB) -L$(LIBCLANG_LIB) -lclang 


test: build
	lua5.3 test.lua

install: build
	$(MAKE) install-lib LUA_LIB_DIR=$(shell $(LUA_COMMAND) -e $(LUA_DIR_SCRIPT))

uninstall:
	$(MAKE) uninstall-lib LUA_LIB_DIR=$(shell $(LUA_COMMAND) -e $(LUA_DIR_SCRIPT))

install-lib:
ifeq ($(LUA_LIB_DIR),)
	$(error LUA_LIB_DIR is empty)
endif
	mkdir -p $(LUA_LIB_DIR)
	mkdir -p $(LUA_LIB_DIR)/libclanglua
	cp $(CORE_SO) $(IF_LUA) $(IFC_LUA) $(LUA_LIB_DIR)/libclanglua

uninstall-lib:
ifeq ($(LUA_LIB_DIR),)
	$(error LUA_LIB_DIR is empty)
endif
	rm -rf $(LUA_LIB_DIR)/libclanglua
