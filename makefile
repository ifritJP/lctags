all:
	(cd src; $(MAKE) $(MAKECMDGOALS))

$(MAKECMDGOALS): all
