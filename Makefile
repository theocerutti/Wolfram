##
## EPITECH PROJECT, 2019
## Makefile
## File description:
##
##

SRC = main.hs \
	get_opt.hs

CC = ghc

FILE = wolfram

all: compi

compi:
	$(CC) -O2 $(SRC) -o $(FILE)

clean:
	rm -f *.hi
	rm -f $(OBJ)

fclean: clean
	rm -f $(FILE)

re : clean fclean all