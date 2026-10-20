# sql-grid package guidance

## Scope

- `sql-grid.el` owns the interactive UI and SVG result grid; `sql-grid-db.el` owns SQLite/MySQL/MariaDB connections and execution. The retained ERT suite is `sql-grid-test.el`; usage and backend behavior are in `README.md`.
- This standalone package requires Emacs 29.1 or newer. Follow its existing conventions: lexical-binding and GPL headers in Lisp files, with adjacent ERT tests where appropriate.
- Keep changes scoped to this package and requested behavior. Do not modify generated artifacts.

## Emacs Lisp edits: parentheses first

- Lisp structural edits are especially prone to misplaced or missing parentheses. Make each edit to a complete, balanced form; never guess at an appended closing parenthesis or patch by visual indentation alone.
- **Immediately after every structural Lisp edit, run `check-parens` in `emacs-lisp-mode` before continuing.** This syntax-only check catches unmatched delimiters; it does not prove the form is semantically correct.
- From `sql-grid/`, check the package Lisp files with:

  ```sh
  emacs -Q --batch --eval '(dolist (file (directory-files "." nil "[.]el$")) (with-temp-buffer (insert-file-contents file) (emacs-lisp-mode) (check-parens)))'
  ```

- Indent only the changed complete form after editing; inspect its nesting and surrounding context. Do not reformat unrelated code.
- Before handoff of Lisp changes, byte-compile the changed Lisp files to catch issues `check-parens` cannot detect. Keep generated `.elc` files out of the repository.

## Tests and verification

- Test public, user-observable behavior with deterministic, isolated tests. Prefer assertions on returned results, rendered/copyable values, key behavior, and visible errors; avoid private-helper implementation details, source-shape checks, or mock-echo tests.
- Isolate both clipboard directions in copy tests and smoke scripts: bind `interprogram-cut-function` and `interprogram-paste-function` to nil. Otherwise, `current-kill` can import the user's clipboard and replace the value under test even when clipboard writes are disabled. Do not read or overwrite the user's clipboard during verification.
- From `sql-grid/`, run the retained ERT suite:

  ```sh
  emacs -Q --batch -L . -l sql-grid-test.el -f ert-run-tests-batch-and-exit
  ```

- Batch mode skips graphical result-rendering tests. To include them, use a graphical Emacs session with `noninteractive` dynamically bound to `t`; do not use `--batch`, which skips graphical tests.
- Evil-specific ERT tests load Evil optionally and skip when it is absent. `emacs -Q` does not read the user's configuration, so locate a loadable Evil install (the `evil.el`/`evil.elc` file, typically a Straight `build-*/evil` directory rather than the `repos` source checkout) before loading `sql-grid-test.el`:

  ```sh
  find "${XDG_CONFIG_HOME:-$HOME/.config}/emacs" "$HOME/.emacs.d" \
    -maxdepth 6 -type d \( -path '*/straight/build-*/evil' \
                            -o -path '*/elpa/evil-*' \) 2>/dev/null || true
  ```

  Pass the discovered directory with `-L` or prepend it to `load-path`; do not hardcode a host-specific path. Verify with `(require 'evil nil t)` and confirm those tests report `passed`, not `SKIPPED`.
- GUI ERT output is not reliable on shell stdout. Run ERT from a temporary script, write `*Messages*` to a temporary file afterward, and inspect the `Ran ...` line plus any `FAILED` or `SKIPPED` entries; exit status alone does not prove the tests ran.
- Wrap temporary GUI runners in `condition-case`, save errors and `*Messages*`, and call `kill-emacs` on both success and failure. An unhandled evaluation error can leave graphical Emacs waiting until the tool timeout instead of producing useful failure evidence.
- When writing `x-export-frames` output, bind `coding-system-for-write` to `no-conversion`; ordinary text coding can corrupt PNG bytes. Confirm the captured file decodes before treating it as visual proof.

- Before GUI checks, perform one bounded preflight (about 20 seconds): inspect available display variables and actual sockets under the current user's runtime directory, then try an existing usable Wayland or X11 session before considering Xvfb. A missing display variable does not mean no GUI exists. Use the discovered runtime path and socket, not host-specific values; match GTK backend to the chosen display. Existing sessions may host new test-owned windows, but never manipulate existing user windows.
- Prove the environment with a short Emacs invocation that records `display-graphic-p`, `window-system`, and available frame-export functions to a temporary file. GUI `princ` output may not reach the shell. Inspect recorded results and errors; correct discovered setup problems before considering another display stack, and avoid repeated runtime experiments without new evidence. Do not install packages or change system configuration. Use Xvfb only if installed and needed; its existing X11 instructions require `WAYLAND_DISPLAY` unset.
- Reuse the proven display environment for subsequent GUI checks and workers. Run the focused relevant regression first, then run the retained suite once after edits. Keep actual native-grid surface verification: buffer properties, pane counts, or passing ERT assertions alone do not prove correct rendering. Capture only the test-owned frame (for example, with Emacs `x-export-frames`); never capture the user's desktop.

  For Xvfb, use the graphical test invocation below after provisioning a valid X11 display:

  ```sh
  env -u WAYLAND_DISPLAY GDK_BACKEND=x11 DISPLAY="$DISPLAY" emacs -Q -L . -l sql-grid-test.el --eval '(let ((noninteractive t)) (ert-run-tests-batch-and-exit))'
  ```

  The Xvfb server and valid `DISPLAY` must be provisioned separately; do not assume a particular display number is available. Clean up only test-owned temporary resources.
- Grid or pane changes require actual graphical-surface verification as well as relevant ERT checks. Exercise real keys, not only direct function calls: post-command hooks can move point or reconfigure windows after the command returns. For terminal-compatible changes, also launch a test-owned `emacs -nw -Q` session.
- In scrolling checks, use enough rows to exceed the body height and assert that the viewport actually changes. Pane counts or a row that was already visible cannot prove scrolling. Inspect the captured surface for aligned panes and fixed content rather than relying only on window metadata.
- Find top/bottom test windows by comparing their edges, not by assuming the top coordinate is zero. Terminal menu bars can offset window coordinates; a mistaken test-window lookup can report a rendering failure when the application is working.
- When a pane check fails, record each test window's edges, point, start, horizontal/vertical scroll offsets, and selected status before another attempt. Pair those measurements with a test-frame capture; redisplay and command hooks can change state after an earlier assertion or measurement.
- Do not claim unrun test results. Do not install system packages, alter system configuration, or leave generated/test artifacts behind merely to make a check run.

## sql-grid behavior and invariants

- SQLite uses Emacs's native SQLite interface and retains one connection (and transaction state) until disconnect. MySQL and MariaDB use one external client process per execution; transactions and other session state do not persist between calls.
- Each execution accepts one SQL statement. Compound statements such as `CREATE TRIGGER` with internal semicolons, and client commands such as `DELIMITER`, are rejected. Never change this constraint implicitly or silently split submitted SQL.
- Server-client XML may omit column metadata for zero-row results. Report that limitation; never rerun a query or invent headers to fill it in. Refresh and cell inspection operate on retained results and must not execute SQL again.
- Server credentials are written to a temporary mode-0600 client options file and removed afterward; do not put passwords in command arguments or weaken this handling. Interactive password values are cleared on disconnect/buffer kill.
- Cell truncation and newline normalization are display-only. Preserve full original scalar values for inspection and copying; distinguish SQL `NULL` from an empty string. Truncation counts its single-character Unicode ellipsis within the configured maximum.
- Unicode truncation protects common combining sequences, including Khmer COENG/linkers/joiners, but is not full Unicode grapheme segmentation. Do not claim complete grapheme safety or replace retained values with truncated display text.
- SVG row geometry is shared by the row's cell image slices and interactive cells; keep their geometry aligned. Normalize theme colors to CSS `#RRGGBB` values, and use Emacs face attributes such as `:background`/`:foreground` when reading face colors.
- Khmer rendering depends on a font with Khmer glyph coverage available to Emacs; do not hardcode a host-specific font path.
