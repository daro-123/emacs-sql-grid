# Changelog

## 0.1.0 (unreleased)

Initial SQL Grid release. The package namespace, files, modes, commands, options, and documentation use `sql-grid`.

### Connections and SQL execution

- Connect existing buffers or open dedicated SQL editors for SQLite, MySQL, MariaDB, and PostgreSQL. Configure named profiles or enter connection settings interactively.
- Use `sql-grid-mode`, derived from `sql-mode`, with statement execution (`C-c C-c`), region execution (`C-c C-r`), cancellation (`C-c C-k`), and disconnect (`C-c C-d`). Statements are contiguous nonblank SQL blocks; each execution accepts exactly one statement.
- Execute queries asynchronously, show running elapsed time, and retain previous results on failure or cancellation. Completed headers show elapsed time and affected-row counts when the backend reports them.
- Preserve SQLite transactions, temporary tables, and session state through a persistent native SQLite worker connection. Cancellation closes the worker connection and rolls back uncommitted work; cancelling an in-memory database requires confirmation because its contents are lost.
- Run MySQL/MariaDB and PostgreSQL executions in fresh external client sessions. Transactions, temporary tables, and session state do not persist between calls. PostgreSQL requires `psql` 12 or newer.
- Pass server credentials through temporary mode-0600 client options or libpq password files, removed after use. Profiles may contain an optional plaintext `:password`; omitted or nil passwords prompt interactively. Disconnect and source-buffer cleanup clear connection credentials, not configured profiles.

### Result views and retained data

- Display results in a native-text graphical `grid` view or a terminal-compatible aligned `text` table. Native grid shaping uses Emacs fonts and fontsets, including Khmer when a suitable font is available; SVG support is not required.
- Configure default format, native grid font, result-window focus, cell tooltips, and cell inspection destination. Preserve each result buffer's selected format across refresh and subsequent executions.
- Show alternating theme-aware row backgrounds according to current visible row order. Keep cell geometry aligned during text scaling and font changes, with an inset focus outline.
- Search full original values with native incremental/regexp search and Evil search. Inspect or copy full scalar values, including embedded whitespace, empty strings, and SQL NULL, without rerunning SQL.
- Truncate display values with a single-character Unicode ellipsis counted within the configured character limit. Protect common combining sequences, including Khmer linkers and joiners, without claiming complete grapheme segmentation.
- Select rectangular cell ranges with the native mark or Evil Visual selection and copy tab/newline-separated plain text, without grid borders or display truncation. Evil linewise selection copies complete selected rows.
- Copy individual cells, rows, or columns; reorder columns with keyboard commands; preserve original SQL results and original cell identities.

### Filtering, sorting, pagination, and navigation

- Filter retained rows by a case-insensitive regular expression over full original values. Use `C-c C-f` or `/` outside Evil search bindings; clearing the expression restores all rows.
- Sort retained rows stably by one or multiple columns before pagination. Use `s` or a header action for primary sorting, `S` for secondary keys, and `C-u s` to restore query order. Numbered header indicators show direction and precedence.
- Display up to `sql-grid-max-rows` rows per page, defaulting to 1000, or all rows when nil. Retain every returned row without rewriting SQL. Navigate pages with `[` and `]`.
- Change the current result buffer's page size temporarily with `C-c C-l` (`sql-grid-set-max-rows`). Keep that override across refresh, format changes, and subsequent results without changing configuration or other buffers.
- Remove row and column freezing; result navigation uses a single scrolling view with independent column reordering.
- Export every filtered row across pages as UTF-8 RFC 4180 CSV in current sort and column order. Unquoted empty fields represent SQL NULL; quoted empty fields represent empty strings, although generic readers may collapse that distinction.
- Customize persistent result bindings through `sql-grid-result-mode-map` and `sql-grid-result-mode-hook`. Default native bindings include `C-c C-f` for filtering and `C-c C-v` for result format; user rebindings survive redraws.

### Schema browsing and completion

- Browse cached tables and views with `C-c C-s`, including qualified names, columns, types, nullability, keys, defaults, hidden SQLite columns, indexes, partial-index predicates, and foreign-key targets.
- Complete cached table and column names asynchronously through `M-TAB` or `C-M-i`, including quoted identifiers and qualified names. Completion uses cached schema rather than query aliases or scope; refresh schema manually after DDL.
- Discover attached SQLite schemas, the current MySQL/MariaDB database, and PostgreSQL schemas. Normalize backend metadata for the shared browser and completion interface.

### Internal refactoring

- Reuse prepared grid labels and native pixel measurements for column sizing and cell padding within each render.
- Limit rectangular-selection traversal to the selected rows, preserving displayed row and column order.
- Use an amortized constant-time FIFO for schema discovery, including metadata queries added by discovery callbacks. Cover attached SQLite schema metadata with a browser regression test.
- Share MySQL/MariaDB client credential-file and argument preparation between synchronous and asynchronous execution, preserving private file permissions and lifecycle cleanup.
