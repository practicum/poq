##################################################
# About this Makefile
#
# This Makefile depends on srcMakefile.opts
##################################################

EXECUTABLE=t2

SRC = t2.cpp

EXTRAFLAGS = -O0 -g

LD_LIBS = -l$(PROJECTNAME)

OTHER_DEPENDENCIES = $(TOP)/lib/lib$(PROJECTNAME).$(LIB_SUFFIX)

BASE_DIR = $(TOP)/test

include ../Makefile.local
