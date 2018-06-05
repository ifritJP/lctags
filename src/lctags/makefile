LUA_COMMAND=lua5.3
LUA_INC=../../external/lua/lua-5.3.4/src/
LUA_LIB=
LUA_CFLAGS=
LUA_LDFLAGS=-llua5.3
INST_DIR=/usr/local/bin
SO=so

all: Helper.$(SO)

Helper.$(SO): Helper.c
	gcc Helper.c -std=c99 -DHELPER_STAND_ALONE -o Helper $(LUA_LIB) $(LUA_CFLAGS) $(LUA_LDFLAGS) -lcrypto -lm -lpthread -lrt
	gcc Helper.c -std=c99 -fPIC -shared -DHELPER_STAND_ALONE -o Helper.$(SO)  $(LUA_LIB) $(LUA_CFLAGS) $(LUA_LDFLAGS) -lcrypto -lm -lpthread -lrt

test: all
	./Helper
	(cd ..; $(LUA_COMMAND) lctags/testHelper.lua)

clean:
	rm -f Helper.$(SO) Helper

install:
	sed "s@lua5.3@$(LUA_COMMAND)@g" lctags | \
		sed 's@$${DIR}@'"$(LUA_MOD_DIR)/lctags@g" > $(INST_DIR)/lctags
	chmod +x $(INST_DIR)/lctags

uninstall:
	rm $(INST_DIR)/lctags
