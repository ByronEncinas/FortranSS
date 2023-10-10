#===============================================
# Compiler and Flags
#===============================================
FC = gfortran
FFLAGS = -O3 -Wall -Wextra -Wpedantic -std=f2008

#===============================================
# Directories
#===============================================
SRC_DIR = src
OBJ_DIR = obj
EXEC_DIR = bin

#===============================================
# Source Files and Objects
#===============================================
FSRC = $(wildcard $(SRC_DIR)/*.f90)
FOBJECTS = $(patsubst $(SRC_DIR)/%.f90, $(OBJ_DIR)/%.o, $(FSRC))

#===============================================
# General Parameters
#===============================================
EXEC = $(EXEC_DIR)/fss

#===============================================
# Compilation rules
#===============================================
.PHONY: all clean spotless

all: $(EXEC)

clean:
	rm -rf *~ $(OBJ_DIR)/*.o $(OBJ_DIR)/*.mod *.pro *.ascii *.nat *.Gh *.o ./bin

spotless: clean
	rm -rf *~ $(OBJ_DIR)/*.o $(OBJ_DIR)/*.mod $(OBJ_DIR)/ $(EXEC)

$(EXEC): $(FOBJECTS)
	@mkdir -p $(EXEC_DIR)
	$(FC) $(FFLAGS) -o $@ $^ -lm

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.f90
	@mkdir -p $(dir $@)
	$(FC) $(FFLAGS) -c $< -o $@ -J$(OBJ_DIR)

#===============================================
# Create Directories
#===============================================
$(shell mkdir -p $(OBJ_DIR) $(EXEC_DIR))
