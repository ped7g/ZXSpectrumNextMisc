.PHONY: all clean gitrelease all_projects nexload2 displayedge Layer2BigPic

all:
	TARGET=all $(MAKE) all_projects

clean:
	TARGET=clean $(MAKE) all_projects

# specific target before commiting to github (to build + keep only important binaries)
gitrelease: clean
	TARGET=gitrelease $(MAKE) all_projects

###############################################################
# add all sub-projects as prerequisity here
all_projects: nexload2 displayedge Layer2BigPic

###############################################################
# rules for each sub-projects to build specific $(TARGET)

nexload2 displayedge Layer2BigPic:
	$(MAKE) --directory=$@ $(TARGET)
