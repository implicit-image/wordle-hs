##
# Project Title
#
# @file
# @version 0.1

main: src/Main.hs
	stack build && alacritty --working-directory $(pwd) -e stack run

# end
