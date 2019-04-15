SBT=/u/cs444/bin/sbt
SCALA=/u/cs444/bin/scala
SOURCES= $(shell find src/ -type f -name '*.scala')
TESTS=""
SBT_OPT=-verbose -debug
JARFILE= joos1w.jar
EXEC=joosc
GRAMMAR=src/main/input.cfg
GRAMMAR_TABLE=src/main/resources/output.lr1

.PHONY: all clean

all: ${EXEC}

${GRAMMAR_TABLE}: ${GRAMMAR}
	@cd src/main/java; ./gen.sh

${GRAMMAR}: src/main/input.annotated
	@cd src/main; ./genInput.sh input.annotated

${EXEC}: ${JARFILE}
	@echo "${SCALA} ${JARFILE} \$$@" > ${EXEC}
	@chmod u+x ${EXEC}

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
