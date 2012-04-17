##################################################
# About this Makefile
#
# This Makefile depends on srcMakefile.opts
##################################################

EXECUTABLE=test

SRC = test.cpp 

#HEADERS = george.h

#On some compilers, george.cpp takes forever with -O2
EXTRAFLAGS = -O0 -g

LD_LIBS = -l$(PROJECTNAME)

OTHER_DEPENDENCIES = $(TOP)/lib/lib$(PROJECTNAME).$(LIB_SUFFIX)

BASE_DIR = $(TOP)/test

include ../Makefile.local
