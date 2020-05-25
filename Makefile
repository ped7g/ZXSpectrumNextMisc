.PHONY: all clean gitrelease all_projects nexload2 displayedge Layer2BigPic Z80_ISA_tools Layer2FadeOut

all:
	TARGET=all $(MAKE) all_projects

clean:
	TARGET=clean $(MAKE) all_projects

# specific target before commiting to github (to build + keep only important binaries)
gitrelease: clean
	TARGET=gitrelease $(MAKE) all_projects

###############################################################
# add all sub-projects as prerequisity here
all_projects: nexload2 displayedge Layer2BigPic Z80_ISA_tools Layer2FadeOut

###############################################################
# rules for each sub-projects to build specific $(TARGET)

nexload2 displayedge Layer2BigPic Z80_ISA_tools Layer2FadeOut:
	$(MAKE) --directory=$@ $(TARGET)
