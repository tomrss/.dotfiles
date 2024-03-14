# Collection of useful commands for using the guix configuration

all: pull check-substitutes home-upgrade system-upgrade

.PHONY: pull
pull:
	guix pull

.PHONY: check-substitutes
check-substitutes:
	guix weather -m utils/manifest-for-weather.scm --substitute-urls="https://substitutes.nonguix.org"

.PHONY: home-upgrade
home-upgrade:
	guix home reconfigure home.scm

.PHONY: system-upgrade
system-upgrade:
	sudo -E guix system reconfigure system.scm

.PHONY: update-channel-definitions
update-channel-definitions:
	guix describe -f channels > channels.scm
