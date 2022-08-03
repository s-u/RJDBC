#!/bin/sh

mkdir -p inst/java

for jtarget in 1.4 1.6 1.8; do
    echo " - Attempting to compile for Java $jtarget ..."
    if javac -d inst/java -source $jtarget -target $jtarget java/*.java; then
	(cd inst/java; jar fvc RJDBC.jar info; rm -rf info)
	break
    fi
done

if test ! -e inst/java/RJDBC.jar; then
    echo "*** ERROR: could not compile Java sources! We need working JDK!"
    exit 1
fi
