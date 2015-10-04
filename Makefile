
#DIRS := quail
DIRS :=
FILES := misc/ascii.elc

DIRS_EL := $(foreach d,$(DIRS),$(wildcard $(d)/*.el))
ALL_ELC := $(patsubst %.el,%.elc,$(DIRS_EL)) $(FILES)

.PHONY : all
all : $(ALL_ELC)

.PHONY : clean
clean :
	rm -f $(ALL_ELC)

%.elc : %.el
	emacs -batch -f batch-byte-compile $<
