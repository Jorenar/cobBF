BIN   := bf
SRC   := main.cbl interpret.cbl
FLAGS := -O

.PHONY: debug hello clean

$(BIN): $(SRC)
	cobc $(FLAGS) -x -o $(BIN) $(SRC)

hello: $(BIN)
	./$(BIN) hello.b

debug: FLAGS += -d -fdebugging-line
debug: $(BIN)

clean:
	rm -rf $(BIN)
