.PHONY: init
init:
	git config borg.drones-directory || git config --local borg.drones-directory borg

-include elisp/borg/borg/borg.mk

