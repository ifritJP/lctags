LUA_COMMAND=lua5.3
LUA_INC=../../external/lua/lua-5.3.4/src/
LUA_LIB=-llua5.3

all: Helper.so

Helper.so:
	gcc Helper.c -DHELPER_STAND_ALONE -o Helper -I$(LUA_INC) -lcrypto $(LUA_LIB)
	gcc Helper.c -fPIC -shared -DHELPER_STAND_ALONE -o Helper.so -I$(LUA_INC) -lcrypto $(LUA_LIB)

test: all
	./Helper
	(cd ..; $(LUA_COMMAND) lctags/testHelper.lua)

clean:
	rm -f Helper.so Helper

install:
	chmod +x lctags
	cp lctags /usr/bin/

uninstall:
	rm /usr/bin/lctags
