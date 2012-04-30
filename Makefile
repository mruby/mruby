# makefile discription.
# basic build file for mruby

# library for iOS
IOSLIB := $(RITEVM)-ios.a
IOSSIMLIB := $(RITEVM)-iossim.a
IOSDEVLIB := $(RITEVM)-iosdev.a
IOSSIMCC := xcrun -sdk iphoneos llvm-gcc-4.2 -arch i386 -isysroot "/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator5.0.sdk/"
IOSDEVCC := xcrun -sdk iphoneos llvm-gcc-4.2 -arch armv7 -isysroot "/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS5.0.sdk/"

# compiler, linker (gcc)
CC = gcc
LL = gcc
DEBUG_MODE = 1
ifeq ($(DEBUG_MODE),1)
CFLAGS = -g -O3
else
CFLAGS = -O3
endif
ALL_CFLAGS = -Wall -Werror-implicit-function-declaration $(CFLAGS)
ifeq ($(OS),Windows_NT)
  MAKE_FLAGS = --no-print-directory CC=$(CC) LL=$(LL) ALL_CFLAGS='$(ALL_CFLAGS)'
else
  MAKE_FLAGS = --no-print-directory CC='$(CC)' LL='$(LL)' ALL_CFLAGS='$(ALL_CFLAGS)'
endif

##############################
# generic build targets, rules

.PHONY : all
all :
	@$(MAKE) -C tools/mruby $(MAKE_FLAGS)

# make library for iOS
.PHONY : ios
ios : $(IOSLIB)

$(IOSLIB) : $(IOSSIMLIB) $(IOSDEVLIB)
	lipo -arch i386 $(IOSSIMLIB) -arch armv7 $(IOSDEVLIB) -create -output $(IOSLIB)

$(IOSSIMLIB) :
	$(MAKE) clean -C src $(MAKE_FLAGS)
	$(MAKE) -C src $(MAKE_FLAGS) CC="$(IOSSIMCC)" LL="$(IOSSIMCC)"
	cp $(LIB) $(IOSSIMLIB)

$(IOSDEVLIB) :
	$(MAKE) clean -C src $(MAKE_FLAGS)
	$(MAKE) -C src $(MAKE_FLAGS) CC="$(IOSDEVCC)" LL="$(IOSDEVCC)"
	cp $(LIB) $(IOSDEVLIB)

# clean up
.PHONY : clean
clean :
	@$(MAKE) clean -C src $(MAKE_FLAGS)
	@$(MAKE) clean -C tools/mruby $(MAKE_FLAGS)
