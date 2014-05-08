JAVA_FILES := $(wildcard *.java)

DEST := ../inst/java

.PHONY: build

build:
	mkdir -p $(DEST)
	javac -d $(DEST) $(JAVA_FILES)
	jar fvc $(DEST)/RJDBC.jar -C $(DEST) info
	rm -rf $(DEST)/info

clean:
	-rm -rf $(DEST)
