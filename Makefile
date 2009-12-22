# prerequisite: GOROOT and GOARCH must be defined

include $(GOROOT)/src/Make.$(GOARCH)

TARG=lips

MAIN=main

# source files in package
GOFILES=\
	lips.go \

# build executable
$(TARG): package
	$(GC) -Ipkg $(MAIN).go
	$(LD) -Lpkg -o $@ $(MAIN).$O

clean:
	rm -rf *.[$(OS)o] *.a [$(OS)].out pkg $(TARG)

package: pkg/$(TARG).a

# create a Go package file (.a)
pkg/$(TARG).a: $(TARG).$O
	@mkdir -p pkg/$(dir)
	rm -f pkg/$(TARG).a
	gopack grc $@ $(TARG).$O

# compile
$(TARG).$O: $(GOFILES)
	$(GC) -o $@ $(GOFILES)
