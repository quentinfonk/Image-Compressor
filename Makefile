##
## EPITECH PROJECT, 2021
## B-FUN-400-PAR-4-1-imageCompressor-alexis.thomas
## File description:
## Makefile
##

all:
	rm -f imageCompressor
	stack build
	cp .stack-work/install/*/*/*/bin/imageCompressor-exe .
	mv imageCompressor-exe imageCompressor

clean:
	stack clean

fclean:
	stack purge

re: fclean all

.PHONY: all clean fclean re
