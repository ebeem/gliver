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
PKG_CONFIG ?= pkg-config

LIBXKBCOMMON_LIBDIR = $(shell $(PKG_CONFIG) --variable libdir xkbcommon)
LIBWAYLAND_CLIENT_LIBDIR = $(shell $(PKG_CONFIG) --variable libdir wayland-client)

MODULES =   gliver/core/logs.scm \
			gliver/core/types.scm \
			gliver/core/hooks.scm \
			gliver/core/config.scm \
			gliver/core/keybindings.scm \
			gliver/core/output.scm \
			gliver/core/seat.scm \
			gliver/core/workspace.scm \
			gliver/core/container.scm \
			gliver/core/window.scm \
			gliver/core/ffi.scm \
			gliver/core.scm \
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
			gliver/river/layer-shell-manager.scm \
			gliver/contrib/commands.scm \
			gliver/contrib/debug/ipc-server.scm \
			gliver/contrib/debug/repl-server.scm \
			gliver/contrib/utils/systemd.scm \
			gliver/contrib/ui/which-key.scm \
			gliver/contrib/ui/palette.scm \
			gliver/contrib/ui/toast.scm \
			gliver/contrib/ui/integrations/rofi.scm \
			gliver/contrib/windows/window-rules.scm \
			gliver/contrib/keybindings/stumpwm.scm \
			gliver/contrib/keybindings/sway.scm \
			gliver/contrib/keybindings/gliver.scm \
			gliver/contrib/layout/alternating.scm

# phony and default make command
.PHONY: all compile test install uninstall clean gen check lint
all: compile

# compile scheme files into go in the provided directory
build/%.go: %.scm
	@echo "Compiling $<..."
	@mkdir -p $(dir $@)
	@guild compile -L . -o $@ $<

# build ffi.scm and include its dependencies
# gliver/core/keybindings.scm -> xkb
# gliver/wayland/client.scm -> libwayland-client
gliver/core/ffi.scm: gliver/core/ffi.scm.in
	sed -e "s|@LIBXKBCOMMON_LIBDIR@|$(LIBXKBCOMMON_LIBDIR)|" \
		-e "s|@LIBWAYLAND_CLIENT_LIBDIR@|$(LIBWAYLAND_CLIENT_LIBDIR)|" < $< > $@

# build each .scm file specified in modules into .go
# file in the build directory
COMPILED = $(patsubst %.scm, build/%.go, $(MODULES))
$(COMPILED): gliver/core/ffi.scm
compile: $(COMPILED)

# auto-generate the river wayland protocol bindings
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

install: compile
	@echo "Installing to $(PREFIX)..."
	install -Dm755 bin/gliver    $(DESTDIR)$(BINDIR)/gliver
	install -Dm755 bin/gliver-repl $(DESTDIR)$(BINDIR)/gliver-repl

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
	rm -f $(DESTDIR)$(BINDIR)/gliver-repl
	rm -rf $(DESTDIR)$(GUILEDIR)/gliver
	rm -rf $(DESTDIR)$(PREFIX)/share/gliver

clean:
	rm -rf build
	rm -f gliver/core/ffi.scm
	rm -rf "$${XDG_CACHE_HOME:-$$HOME/.cache}/guile/ccache/"*"$(CURDIR)"

run:
	./bin/gliver
