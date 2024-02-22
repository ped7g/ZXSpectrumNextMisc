.PHONY: all clean gitrelease all_projects nexload2 displayedge Layer2BigPic Z80_ISA_tools \
 Layer2FadeOut ReadingAtariDrivingController snippets ShowAll512Colors tile8xN dot7gFX \
 FailedHdmiCopperFix

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
all_projects: ReadingAtariDrivingController snippets ShowAll512Colors tile8xN
all_projects: dot7gFX FailedHdmiCopperFix

###############################################################
# rules for each sub-projects to build specific $(TARGET)

nexload2 displayedge Layer2BigPic Z80_ISA_tools Layer2FadeOut ReadingAtariDrivingController snippets ShowAll512Colors tile8xN dot7gFX FailedHdmiCopperFix:
	$(MAKE) --directory=$@ $(TARGET)
