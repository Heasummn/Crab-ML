# Installs Crab to path and creates man page

INSTALL_DIR=/usr/local
EXECUTABLE_DIR=$INSTALL_DIR/bin
LIB_DIR=$INSTALL_DIR/lib
CRAB_LIB_DIR=$LIB_DIR/crab
mkdir -p $CRAB_LIB_DIR

# Make the crab script executable
chmod +x crab

# Move everything to the library directory 
cp --remove-destination crab crab_compiler prelude.c $CRAB_LIB_DIR/

# Create a link to the actual crab script
ln -sf $CRAB_LIB_DIR/crab $EXECUTABLE_DIR/crab
ln -sf $CRAB_LIB_DIR/crab_compiler $EXECUTABLE_DIR/crab_compiler

# Do this right now, as to avoid needing sudo, or rebuilding this later. 
gcc -o $CRAB_LIB_DIR/prelude.o -c $CRAB_LIB_DIR/prelude.c

# Copy man pages
MAN_DIR=$INSTALL_DIR/share/man/man1
mkdir -p $MAN_DIR
cp man/crab.1 $MAN_DIR
