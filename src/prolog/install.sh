#!/bin/bash
# Author: Eyal Dechter
# 
# install.sh
#
# copy PRISM bin directory after build to install location.

BUILDDIR=$1
INSTALLDIR=$2
PRISM_DIR=$INSTALLDIR/prism


if [[ -d "${PRISM_DIR}" ]]; then 
    read -p "Delete directory ${PRISM_DIR}? " -n 1 -r 
    echo
    if [[ "$REPLY" =~ ^[Yy]$ ]]; then 
        rm -fr $INSTALLDIR/prism || $(error: will not overwrite INSTALL_DIR) 
    fi 
fi

mkdir $PRISM_DIR/
CMD="cp -vr ${BUILDDIR} ${PRISM_DIR}/"
echo CMD
$CMD
