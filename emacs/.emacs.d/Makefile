EMACS := emacs
MANAGE_EL := manage.el
BACKUP_STATE_DIR := emacs-state-backup-$(shell date +'%s')
STATE_FILES := auto-save-list backups eshell transient undo history places projects recentf

EVAL = $(EMACS) --batch --load $(MANAGE_EL) --eval

install:
	$(EVAL) '(manage-install)'

upgrade:
	$(EVAL) '(manage-upgrade)'

build: rebuild

rebuild:
	$(EVAL) '(manage-rebuild)'

uninstall:
	rm -rvf straight/

backup-state:
	mkdir $(BACKUP_STATE_DIR)
	cp -r $(STATE_FILES) $(BACKUP_STATE_DIR)/
	tar -czvf $(BACKUP_STATE_DIR).tgz $(BACKUP_STATE_DIR)
	rm -rvf $(BACKUP_STATE_DIR)
