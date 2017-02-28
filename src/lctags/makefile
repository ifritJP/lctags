LUA_COMMAND=lua5.3
LUA_INC=../../external/lua/lua-5.3.4/src/
LUA_LIB=
LUA_CFLAGS=
LUA_LDFLAGS=-llua5.3

all: Helper.so

Helper.so: Helper.c
	gcc Helper.c -std=c99 -DHELPER_STAND_ALONE -o Helper $(LUA_LIB) $(LUA_CFLAGS) $(LUA_LDFLAGS) -lcrypto -lm 
	gcc Helper.c -std=c99 -fPIC -shared -DHELPER_STAND_ALONE -o Helper.so  $(LUA_LIB) $(LUA_CFLAGS) $(LUA_LDFLAGS) -lcrypto -lm

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
