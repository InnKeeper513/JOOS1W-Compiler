SBT=/u/cs444/bin/sbt
SCALA=/u/cs444/bin/scala
SOURCES= $(shell find src/ -type f -name '*.scala')
TESTS=""
JARFILE= joos1w.jar
EXEC=joosc
GRAMMAR=src/main/input.cfg
GRAMMAR_TABLE=src/main/resources/output.lr1
SBT_OPTS=-J-Xms1024M -J-Xmx1024M -J-Xss1024M -J-XX:MaxMetaspaceSize=1024M

.PHONY: all clean

all: ${EXEC}

${GRAMMAR_TABLE}: ${GRAMMAR}
	@cd src/main/java; ./gen.sh

${GRAMMAR}: src/main/input.annotated
	@cd src/main; ./genInput.sh input.annotated

${EXEC}: ${JARFILE}
	export SBT_OPTS="-Xms1024M -Xmx1024M -Xss1024M -XX:MaxMetaspaceSize=1024M"; \
	echo "$${SBT_OPTS}"; \
	echo "export SBT_OPTS=\"-Xms1024M -Xmx1024M -Xss1024M -XX:MaxMetaspaceSize=1024M\"" > ${EXEC} ; \
	echo "${SCALA} ${SBT_OPTS} ${JARFILE} \$$@" >> ${EXEC}; \
	chmod u+x ${EXEC}

${JARFILE}: ${SOURCES}
	${SBT} package

grammar: ${GRAMMAR_TABLE}

test:
	${SBT} test

testOnly:
	${SBT} "testOnly ${TESTS}"   # make testOnly TESTS="DFATests ..."

clean:
	${SBT} clean
	@rm ${JARFILE}
	@rm ${EXEC}
