.PHONY: all clean gitrelease specifictarget nexload2

all:
	TARGET=all $(MAKE) specifictarget

clean:
	TARGET=clean $(MAKE) specifictarget

# specific target before commiting to github (to build + keep only important binaries)
gitrelease: clean
	TARGET=gitrelease $(MAKE) specifictarget

###############################################################
# add all sub-projects as prerequisity here
specifictarget: nexload2

###############################################################
# rules for each sub-projects to build specific $(TARGET)

nexload2:
	$(MAKE) --directory=$@ $(TARGET)
