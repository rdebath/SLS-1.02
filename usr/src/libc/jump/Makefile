DIRS=libc libc.lite libm

clean:
	if [ x"$(DIRS)"x != xx ]; then \
	  for d in $(DIRS); do \
	    echo Cleaning $$d; \
	    (cd $$d; $(MAKE) clean;) \
	  done; \
	else true; fi

