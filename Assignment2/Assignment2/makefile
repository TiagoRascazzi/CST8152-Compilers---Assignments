CC = gcc
CC_FLAGS = -g -ansi -pedantic -Wall
FILES = buffer.c platy_st.c scanner.c
OUT_EXE = Assignment2

build: $(FILES)
		$(CC) $(CC_FLAGS) -o $(OUT_EXE) $(FILES)

clean:
		rm -f *.o core *.exe *~

rebuild: clean build