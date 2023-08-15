#===============================================
# Compiler and Flags
#===============================================
FC = gfortran
CC = gcc
FFLAGS = -O3 -Wall -Wextra -Wpedantic -std=f2008
CFLAGS = -O -Wall -Wextra

#===============================================
# Directories
#===============================================
SRC_DIR = src
OBJ_DIR = obj
EXEC_DIR = bin

#===============================================
# Source Files and Objects
#===============================================
FSRC = $(wildcard $(SRC_DIR)/**/*.f90)
CSRC = $(wildcard $(SRC_DIR)/**/*.c)
FOBJECTS = $(patsubst $(SRC_DIR)/%.f90, $(OBJ_DIR)/%.o, $(FSRC))
COBJECTS = $(patsubst $(SRC_DIR)/%.c, $(OBJ_DIR)/%.u, $(CSRC))

#===============================================
# General Parameters
#===============================================
EXEC = $(EXEC_DIR)/executable

#===============================================
# Compilation rules
#===============================================
.PHONY: all clean spotless

all: $(EXEC)

clean:
	rm -rf *~ $(OBJECTS) *.pro *.ascii *.nat *.mod *.Gh *.o ./obj ./bin

spotless: clean
	rm -f *~ $(OBJECTS) $(EXEC)

$(EXEC): $(FOBJECTS) $(COBJECTS)
	$(FC) $(FFLAGS) -o $@ $^ -lm

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $< -o $@

$(OBJ_DIR)/%.u: $(SRC_DIR)/%.c $(CHEAD)
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

#===============================================
# Create Directories
#===============================================
$(shell mkdir -p $(OBJ_DIR) $(EXEC_DIR))
