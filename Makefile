# Gliver Makefile
#
# Targets:
#   make          - compile Guile modules
#   make test     - run test suite
#   make install  - install to PREFIX
#   make clean    - remove build artifacts
#   make gen      - regenerate protocol bindings

PREFIX     ?= /usr/local
BINDIR     ?= $(PREFIX)/bin
GUILEDIR   ?= $(PREFIX)/share/guile/site/3.0
GUILE      ?= guile
GUILD      ?= guild

MODULES =   gliver/core/logs.scm \
			gliver/core/types.scm \
			gliver/core/hooks.scm \
			gliver/core/config.scm \
			gliver/core/keybindings.scm \
			gliver/core/output.scm \
			gliver/core/seat.scm \
			gliver/core/workspace.scm \
			gliver/core/container.scm \
			gliver/core/layout.scm \
			gliver/core/manager.scm \
			gliver/core/window.scm \
			gliver/core.scm \
			gliver/commands.scm \
			gliver/window-rules.scm \
			gliver/render.scm \
			gliver/message-bar.scm \
			gliver/wayland/client.scm \
			gliver/wayland/gen/wayland.scm \
			gliver/wayland/gen/river-input-management-v1.scm \
			gliver/wayland/gen/river-layer-shell-v1.scm \
			gliver/wayland/gen/river-libinput-config-v1.scm \
			gliver/wayland/gen/river-window-management-v1.scm \
			gliver/wayland/gen/river-xkb-bindings-v1.scm \
			gliver/wayland/gen/river-xkb-config-v1.scm \
			gliver/wayland/gen/wlr-layer-shell-unstable-v1.scm \
			gliver/river/connector.scm \
			gliver/river/window-manager.scm \
			gliver/river/wm-seat-manager.scm \
			gliver/river/keybindings-manager.scm \
			gliver/ipc.scm \
			gliver/repl.scm \
			gliver/contrib/gaps.scm \
			gliver/contrib/systemd.scm \
			gliver/contrib/which-key.scm \
			gliver/contrib/keybindings/stumpwm.scm \
			gliver/contrib/keybindings/sway.scm \
			gliver/contrib/layout/alternating.scm

COMPILED = $(patsubst %.scm, build/%.go, $(MODULES))

build/%.go: %.scm
	@echo "Compiling $<..."
	@mkdir -p $(dir $@)
	@guild compile -L . -o $@ $<

TESTS = tests/test-hooks.scm \
		tests/test-keybindings.scm \
		tests/test-core.scm \
		tests/test-config.scm \
		tests/test-ipc.scm \
		tests/test-render.scm \
		tests/test-message-bar.scm \
		tests/test-repl.scm \
		tests/test-window-rules.scm \
		tests/contrib/test-gaps.scm 

.PHONY: all compile test install uninstall clean gen check lint

all: compile

## ---- Compilation ----

compile: $(COMPILED)

%.go: %.scm
	@mkdir -p $(dir $@)
	GUILE_LOAD_PATH=. $(GUILD) compile -L . -o $@ $<

## ---- Testing ----

test: $(TESTS)
	@echo "=== Running test suite ==="
	@failed_count=0; \
	failed_files=""; \
	for t in $(TESTS); do \
		echo "--- $$t ---"; \
		GUILE_LOAD_PATH=. $(GUILE) -L . $$t; \
		if [ $$? -ne 0 ]; then \
			failed_count=$$((failed_count + 1)); \
			failed_files="$$failed_files $$t"; \
		fi; \
	done; \
	echo ""; \
	echo "=== Test Summary ==="; \
	if [ $$failed_count -eq 0 ]; then \
		echo "✅ All $(words $(TESTS)) test file(s) passed."; \
	else \
		echo "❌ $$failed_count test file(s) failed:"; \
		for f in $$failed_files; do \
			echo "   - $$f"; \
		done; \
		exit 1; \
	fi

check: test

gen:
	$(GUILE) -L . tools/generate-bindings.scm \
		protocols/river-window-management-v1.xml \
		protocols/river-xkb-bindings-v1.xml \
		protocols/river-xkb-config-v1.xml \
		protocols/river-input-management-v1.xml \
		protocols/river-libinput-config-v1.xml \
		protocols/river-layer-shell-v1.xml \
        protocols/wayland.xml \
        protocols/wlr-layer-shell-unstable-v1.xml \
		gliver/wayland/gen

lint:
	@echo "Checking module imports..."
	@for m in $(MODULES); do \
		$(GUILE) -L . -c "(use-modules ($(shell echo $$m | sed 's|/| |g;s|\.scm||')))" 2>&1 \
		| grep -v "^$$" && echo "FAIL: $$m" || echo "OK: $$m"; \
	done

install: compile
	@echo "Installing to $(PREFIX)..."
	install -Dm755 bin/gliver    $(DESTDIR)$(BINDIR)/gliver
	install -Dm755 bin/gliver-msg $(DESTDIR)$(BINDIR)/gliver-msg

	# modules
	@for m in $(MODULES); do \
		install -Dm644 $$m $(DESTDIR)$(GUILEDIR)/$$m; \
	done

	# compiled modules
	@for g in $(COMPILED); do \
		if [ -f $$g ]; then \
			install -Dm644 $$g $(DESTDIR)$(GUILEDIR)/$$g; \
		fi; \
	done

	# protocol files
	@for p in protocols/*.xml; do \
		install -Dm644 $$p \
			$(DESTDIR)$(PREFIX)/share/gliver/$$p; \
	done

	# example config
	install -Dm644 doc/examples/init.scm \
		$(DESTDIR)$(PREFIX)/share/gliver/examples/init.scm
	@echo "Done. You may need to add $(GUILEDIR) to GUILE_LOAD_PATH."

uninstall:
	rm -f $(DESTDIR)$(BINDIR)/gliver
	rm -f $(DESTDIR)$(BINDIR)/gliver-msg
	rm -rf $(DESTDIR)$(GUILEDIR)/gliver
	rm -rf $(DESTDIR)$(PREFIX)/share/gliver

clean:
	rm -rf build
	rm -rf "$${XDG_CACHE_HOME:-$$HOME/.cache}/guile/ccache/"*"$(CURDIR)"

repl:
	$(GUILE) -L . -l gliver/core.scm

run:
	GUILE_LOAD_PATH=. $(GUILE) --no-auto-compile -L . --debug bin/gliver

.PHONY: repl run
